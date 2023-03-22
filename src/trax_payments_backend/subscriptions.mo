import Hash       "mo:base/Hash";
import Map        "mo:base/HashMap";
import Principal  "mo:base/Principal";
import Nat        "mo:base/Nat";
import Nat32      "mo:base/Nat32";
import Nat64      "mo:base/Nat64";
import Nat8       "mo:base/Nat64";
import Text       "mo:base/Text";
import Iter       "mo:base/Iter";
import Float      "mo:base/Float";
import T          "./types";
import Ledger     "canister:ledger";
import Account    "./utils/account";
import Time       "mo:base/Time";
import Int        "mo:base/Int";
import Error      "mo:base/Error";
import Debug      "mo:base/Debug";
import Result     "mo:base/Result";
import U          "./utils/utils";
import Hex        "./utils/Hex";
import Blob       "mo:base/Blob";
import Array      "mo:base/Array";
import Buffer     "mo:base/Buffer";
import Trie       "mo:base/Trie";
import TrieMap    "mo:base/TrieMap";
import Cycles     "mo:base/ExperimentalCycles";
import Char       "mo:base/Char";
import Int64      "mo:base/Int64";
import Timer      "mo:base/Timer";
import XRC        "canister:xrc";
import Env        "./utils/env";

// TODO
// * Add functionality for PPV content (royalty sharing) to be able change participants and revenue share after initial posting
// * Adding tiers to subscriptions? (feature)
// * Ability to pause subscriptions (feature)

// * onlyOwner checks throughout contract 
// * Add mag.caller param to payment functions.
// * Change Int.hash to own hashing function or use Stable hashmap 
// * Optimise SC memory and speed, checking types and function logic


// import E "../exchange_rate/main";

actor Subscriptions {
  type ContentID                 = T.ContentID;
  type Content                   = T.Content;
  type ArtistID                  = T.ArtistID;
  type FanID                     = T.FanID;
  type AdminID                   = T.AdminID;
  type AccountIdentifier         = T.AccountIdentifier;
  type ICPTs                     = T.ICPTs;
  type Ticker                    = T.Ticker;
  type Timestamp                 = T.Timestamp;
  type SubPrice                  = T.SubPrice;
  type SubType                   = T.SubscriptionType;
  type SubAccount                = Blob;
  type Percentage                = T.Percentage;
  type TransactionID             = T.TransactionID;
  type Participants              = T.Participants;

  
  private type FanToTime          = Map.HashMap<FanID, (Timestamp, Nat64, Ticker)>;
  
  private type SubInfo            = Map.HashMap<FanID, (Timestamp, SubPrice, SubType, Ticker)>;
  private type SubRevenue         = Map.HashMap<Ticker, Nat64>;

  private type FanToTxData         = Map.HashMap<FanID, TxData>; 
  private type TxData             = Map.HashMap<Timestamp, (Nat64, Ticker)>; 

  let FEE : Nat64 = 10000;
  stable var txNo : Nat64 = 0;


  //SUBSCRIPTIONS
  private stable var _subMap : [(ArtistID, (FanID, (Timestamp, SubPrice, SubType, Ticker)))] = [];
  private stable var _artistTotalSubRevenue : [(ArtistID, (Ticker, Nat64))] = [];
  private stable var _subTxMap : [(ArtistID, (FanID, (Timestamp, (Nat64, Ticker))))] = [];


  var subTxMap = Map.HashMap<ArtistID, FanToTxData>(1, Principal.equal, Principal.hash); 
  var subMap = Map.HashMap<ArtistID, SubInfo>(1, Principal.equal, Principal.hash); // Mapping keeping trax of artist a fan subscribes to and the timestamp of next subscription payment.
  var artistTotalSubRevenue = Map.HashMap<ArtistID, SubRevenue>(1, Principal.equal, Principal.hash); // total revenue earned through subs
  

  var count          =                      0;
  let n              =                    120;
  let nMonth         =                2629800;
  let oneMin: Int    =         60_000_000_000;
  let twoMins: Int   =             oneMin * 2;
  let fourMins: Int  =             oneMin * 4;
  let fiveMins: Int  =             oneMin * 5;
  let oneDay: Int    =     86_400_000_000_000;
  let oneMonth: Int  =  2_629_800_000_000_000;
  let oneYear: Int   =          oneMonth * 12;
  

  system func timer(set : Nat64 -> ()) : async () {
    set(Nat64.fromIntWrap(Time.now()) + Nat64.fromIntWrap(oneMin)); 

    count += 1;
    Debug.print("count " # debug_show count);

    await payArtistsSub();
  };



  // public shared(msg) func updateTraxAccount(account: Text) : async(){
  //   assert(Principal.toText(msg.caller) == TRAX_ACCOUNT);
  //   TRAX_ACCOUNT := account;
  // };





// #region - SUBSCRIPTIONS

  // in the event that a transaction fails and the function 
  public shared({caller}) func _payArtistsSub() : async(){
    if (not U.isAdmin(caller)) {
      throw Error.reject("Unauthorized access. Caller is not an admin. " # Principal.toText(caller));
    };
    await payArtistsSub();
  };

private func checkBalance(fan: FanID, amount: Nat64) : async Bool {
    let bal = await accountBalance(fan);
    if(bal.e8s >= amount){
        return true;
    }else{
        throw Error.reject("Insufficient Balance: " # debug_show bal.e8s); 
        return false;
    }
};

  private func payArtistsSub() : async (){ // balance check

    let priceICP: Float = await getExchangeRate("ICP");

    for(ids in subMap.entries()){
          
          let artistID : ArtistID = ids.0;
          let subInfo: SubInfo =  ids.1;
            for (info in subInfo.entries()){

              let fanID : FanID = info.0;
              let timestamp : Timestamp = info.1.0;
              let priceOfSub : SubPrice = info.1.1;
              let period : SubType = info.1.2;
              let ticker : Ticker = info.1.3;
              
          if(Time.now() > timestamp){
            let formattedAmount: Nat64 = Nat64.fromIntWrap(Float.toInt((priceOfSub / priceICP) * 100000000));
            
            // let check =  await checkBalance(fanID, formattedAmount);
            // if(check == false){
            //   let unsubbed = await unsubscribe(artistID, fanID);
            // };

            Debug.print("period: " # debug_show period);
            var nextPayment : Int = 0;
            let amount : Nat64 = await platformDeduction(fanID, formattedAmount);

            switch(await transfer(fanID, artistID, amount)){
              case(#ok(res)){
                switch(subMap.get(artistID)){
                  case(?innerMap){
                    switch(innerMap.get(fanID)){   
                      case(?currVals){
                        if (period == #monthly){    nextPayment := twoMins;    };
                        if (period == #yearly) {    nextPayment := fourMins;   };
                        
                        var update = innerMap.replace(fanID, ((timestamp + nextPayment), priceOfSub, period, ticker));
                      
                      };
                      case null {   Debug.print("Couldnt find or access FanID in subMap");    };
                    };
                  };
                  case null {   Debug.print("Couldnt find or access ArtistID in subMap");    };
                };

                switch(artistTotalSubRevenue.get(artistID)){
                  case(?innerMap){   
                    switch(innerMap.get(ticker)){
                      case(?currVal){
                        var update = innerMap.replace(ticker, currVal + amount);   
                      }; 
                      case null {
                        innerMap.put(ticker, amount)
                      };
                    };
                  };
                  case null {   
                    var x : SubRevenue = Map.HashMap<Ticker, Nat64>(2, Text.equal, Text.hash);
                    x.put(ticker, amount);
                    artistTotalSubRevenue.put(artistID, x); 
                  };
                };

                await addToSubTxMap(artistID, fanID, timestamp, amount, ticker);
              }; 
              // case (#Err(#InsufficientFunds { balance })) {
              //   let success = await unsubscribe(artistID, fanID);
              //   Debug.print(debug_show success);
              //   throw Error.reject("Insufficient balance of " # debug_show balance # " from account:" # debug_show fanID # " fan has been unsubscribed!")
            
              // };
              case(#err(msg)){  
                Debug.print("ERROR at payArtistSub: " # debug_show msg);
                let success = await unsubscribe(artistID, fanID);
                Debug.print(debug_show success);
                

                // // assert(success);
                // throw Error.reject("Your subscription has been terminated due to: " # debug_show msg);
                // return
              };
            };
          };
        };
      };
  };

  


  public shared(msg) func subscribe(artist: ArtistID, fan: FanID, priceOfSub: Float, ticker: Ticker, period: SubType, freeTrial: Bool, freeDays: Int): async Bool{
    
    let fanSubscribed = await isFanSubscribed(artist, fan);
    if(fanSubscribed == true) {
      throw Error.reject("FAN IS ALREADY A SUBSCRIBER!");
    };
    let priceICP = await getExchangeRate(ticker);
    Debug.print("price ICP: "# debug_show priceICP);

    Debug.print("SubType" # debug_show period);
    

    if(freeTrial){
      await addToSubMap(artist, msg.caller, priceOfSub, period, freeTrial, freeDays, ticker);
      true;
    }else{

      let formattedAmount: Nat64 = Nat64.fromIntWrap(Float.toInt((priceOfSub / priceICP) * 100000000));
      // let check =  await checkBalance(fan, formattedAmount);
      // assert(check == true);

      var amountICP = await platformDeduction(fan, formattedAmount);
      switch(await transfer(fan, artist, amountICP)){
          case(#ok(res)){
            await addToSubMap(artist, fan, priceOfSub, period, freeTrial, freeDays, ticker);
            await addToSubTxMap(artist, fan, Time.now(), amountICP, ticker);
            true;
            }; case(#err(msg)){
              throw Error.reject("Unexpected error: " # debug_show msg);
              false;
            };
      };
    };
  };



  private func addToSubMap(artist: ArtistID, fan: FanID, priceOfSub: Float, period: SubType, freeTrial: Bool, freeDays: Int, ticker: Ticker) : async (){
    let timeNow = Time.now();
    var nextPayment: Int = 0;
    if (period == #monthly){    nextPayment := twoMins;    };
    if (period == #yearly) {    nextPayment := fourMins;   };

    switch(subMap.get(artist)){
      case(?innerMap){
        
        if (freeTrial){
          innerMap.put(fan, (timeNow + (oneDay * freeDays), priceOfSub, period, ticker));
          Debug.print("Fan subscribed with free trial, first payment is at: " # debug_show (timeNow + (oneDay * freeDays)));
        } 
        else {
          innerMap.put(fan, (timeNow + nextPayment, priceOfSub, period, ticker));
          Debug.print("Fan subscribed, next payment is at: " # debug_show (timeNow + nextPayment));
        };
        // Debug.print("Fan subscribed, next payment is at: " # debug_show (Nat64.fromIntWrap(Time.now()) + nextPayment));

      }; case null {
        var x : SubInfo = Map.HashMap<FanID, (Timestamp, Float, SubType, Ticker)>(2, Principal.equal, Principal.hash);

        if(freeTrial){    
          let res = timeNow + (oneDay * freeDays);
          Debug.print(debug_show res);
          x.put(fan, (timeNow + (oneDay * freeDays), priceOfSub, period, ticker));
          Debug.print("Fan subscribed to new artist with free trial, first payment is at: " # debug_show (timeNow + (oneDay * freeDays)));
        }else{
           x.put(fan, (timeNow + nextPayment, priceOfSub, period, ticker));
           Debug.print("Fan subscribed to new artist, next payment is at:" # debug_show (timeNow + nextPayment));
        };

        subMap.put(artist, x);
      };
    };
  };



  private func addToSubTxMap(artist: ArtistID, fan: FanID, timestamp: Timestamp, amount: Nat64, ticker: Ticker) : async (){
    let idFan: Text = Principal.toText(fan);
    let stamp : Text = Int.toText(timestamp);
    let key : Text = stamp # idFan;
    Debug.print("hash text: " # debug_show key);

    switch(subTxMap.get(artist)){
      case(?fanToTxData){
        switch(fanToTxData.get(fan)){
          case(?txData){
            txData.put(timestamp, (amount, ticker))
          };
          case null {
            var y : TxData = Map.HashMap<Timestamp, (Nat64, Ticker)>(2, Int.equal, Int.hash);
            y.put(timestamp, (amount, ticker));
            fanToTxData.put(fan, y);
          };
        }
      }; 
      case null {
        var y : TxData = Map.HashMap<Timestamp, (Nat64, Ticker)>(2, Int.equal, Int.hash);
        var x : FanToTxData = Map.HashMap<FanID, TxData>(2, Principal.equal, Principal.hash);

        y.put(timestamp, (amount, ticker));
        x.put(fan, y);
        subTxMap.put(artist, x);
      };
    };
  };



  public func getSubTxMapArtist(artist: ArtistID) : async [(FanID, Timestamp, Nat64, Ticker)]{
    var res = Buffer.Buffer<(FanID, Timestamp, Nat64, Ticker)>(2);

      switch(subTxMap.get(artist)){
        case(?fanToTxData){
            for(fans in fanToTxData.entries()){
                var fanId : FanID = fans.0;
                switch(fanToTxData.get(fanId)){
                    case(?txData){
                        for(data in txData.entries()){
                            var timestamp: Timestamp = data.0;
                            var amount: Nat64 = data.1.0;
                            var ticker: Ticker = data.1.1;
                            res.add(fanId, timestamp, amount, ticker);
                        }
                    };
                    case null { }
                }
            };          
        };case null { };
      };
      return Buffer.toArray(res);
  };



  public func getSubTxMapFan(fan: ArtistID) : async [(ArtistID, Timestamp, Nat64, Ticker)]{
    var res = Buffer.Buffer<(ArtistID, Timestamp, Nat64, Ticker)>(2);

        for(entries in subTxMap.entries()){
          let artistId : ArtistID = entries.0;
          switch(subTxMap.get(artistId)){
            case(?fanToTxData){
    
              for(fans in fanToTxData.entries()){
                var fanId: FanID = fans.0;
                if(fanId == fan){
                    switch(fanToTxData.get(fan)){
                        case(?txData){
                            for(data in txData.entries()){
                                var timestamp: Timestamp = data.0;
                                var amount: Nat64 = data.1.0;
                                var ticker: Ticker = data.1.1;
                                res.add(artistId, timestamp, amount, ticker);
                            };
                        };
                        case null { }
                    };
                  }
              }
            };case null { };
          };
        };
    return Buffer.toArray(res);
  };



  public func isFanSubscribed(artist: ArtistID, fan: FanID) : async Bool{
    switch(subMap.get(artist)){
      case(?innerMap){
        switch(innerMap.get(fan)){
          case(?exists){
            true
          };
          case null false
        };
      };
      case null false;
    };
  };



  public func unsubscribe(artist: ArtistID, fan: FanID) : async Bool{
    switch(subMap.get(artist)){
      case(?innerMap){
        innerMap.delete(fan);
        Debug.print("Unsubscribed");
        true
      }; case null false;
    };
  };



  public func getArtistTotalSubRevenue(artist: ArtistID, ticker:Ticker) : async ?Nat64{    
    switch(artistTotalSubRevenue.get(artist)){
      case(?innerMap){
        innerMap.get(ticker);
      };
      case null null;
    }   
  };



  public func getNumOfSubs(artist: ArtistID) : async Nat32 {
    var numOfSubs : Nat32 = 0;
    switch(subMap.get(artist)){
      case(?innerMap){
        for(fans in innerMap.entries()){
          numOfSubs := numOfSubs + 1;
        };
        numOfSubs;
      }; case null {
        numOfSubs;
      }
    }
  };

// #endregion




// #region XRC canister call
public func getExchangeRate(symbol : Text) : async Float {

    let request : XRC.GetExchangeRateRequest = {
      base_asset = {
        symbol = symbol;
        class_ = #Cryptocurrency;
      };
      quote_asset = {
        symbol = "USDT";
        class_ = #Cryptocurrency;
      };
      // Get the current rate.
      timestamp = null;
    };

    
    Cycles.add(10_000_000_000); // Every XRC call needs 10B cycles.
    let response = await XRC.get_exchange_rate(request);
    // Print out the response to get a detailed view.
    Debug.print(debug_show(response));
    // Return 0.0 if there is an error for the sake of simplicity.
    switch(response) {
      case (#Ok(rate_response)) {
        let float_rate = Float.fromInt(Nat64.toNat(rate_response.rate));
        let float_divisor = Float.fromInt(Nat32.toNat(10**rate_response.metadata.decimals));
        return float_rate / float_divisor;
      };
      case _ {
        return 0.0;
      };
    }
  };
// #endregion







// #region - HELPER FUNCTIONS
  private func getRemainingAfterDeduction(amount: Nat64, percent: Float) : async(Nat64){
    let priceFloat : Float = Float.fromInt(Nat64.toNat(amount));
    let deduction :  Float = priceFloat * percent;
    return Nat64.fromNat(Int.abs(Float.toInt(priceFloat - deduction)))
  };
  


  private func getDeductedAmount(amount: Nat64, percent: Float) : async(Nat64){
    let priceFloat : Float = Float.fromInt(Nat64.toNat(amount));
    return Nat64.fromNat(Int.abs(Float.toInt(priceFloat * percent)));
  };
// #endregion







// #region - TRANSFER FUNCTIONS  

  private func platformDeduction(fan: FanID, amount : Nat64) : async Nat64 {
    let traxAccount: Principal = Principal.fromText(Env.traxAccount);
    let fee = await getDeductedAmount(amount, 0.10);
    // Debug.print("deducted amount: " # debug_show fee);
    
    switch(await transfer(fan, traxAccount, fee)){
      case(#ok(res)){
        Debug.print("Fee of: " # debug_show fee # "paid to trax account: " # debug_show traxAccount # " in block " # debug_show res);
      };case(#err(msg)){
        throw Error.reject("Unexpected error: " # debug_show msg);
      }
    };

    let amountAfterDeduction = await getRemainingAfterDeduction(amount, 0.10);
    return amountAfterDeduction;
  };


  func transfer(from: Principal, to: Principal, amount: Nat64): async Result.Result<Nat64, Text>{
    // Debug.print(Nat.fromText(Principal.toText(from)));

    let now = Time.now();
    let res = await Ledger.transfer({
          memo = txNo; 
          from_subaccount = ?Account.principalToSubaccount(from);
          to = Account.accountIdentifier(to, Account.defaultSubaccount());
          amount = { e8s = amount };
          fee = { e8s = FEE };
          created_at_time = ?{ timestamp_nanos = Nat64.fromNat(Int.abs(now)) };
        });

        Debug.print("res: "# debug_show res);
        
        switch (res) {
          case (#Ok(blockIndex)) {
            txNo += 1;
            Debug.print("Paid recipient: " # debug_show to # " in block " # debug_show blockIndex);
            return #ok(blockIndex);
          };
          case (#Err(#InsufficientFunds { balance })) {

            return #err("Insufficient balance of " # debug_show balance # " from account:" # debug_show from # "")
            
          };
          // case (#Err(#TxDuplicate {duplicate_of})) {
          //   await transfer(from, to, amount);
          // };
          case (#Err(other)) {
            return #err("Unexpected error: " # debug_show other);
          };
        };
  };
// #endregion








// #region Utils







  public query func canisterAccount() : async Account.AccountIdentifier {
    myAccountId();
  };

  public func accountBalance (account: Principal) : async Ledger.Tokens{
      var specifiedAccount = Account.accountIdentifier(account, Account.defaultSubaccount());
      await Ledger.account_balance({ account = specifiedAccount });
  };


  public func canisterBalance() : async Ledger.Tokens {
    await Ledger.account_balance({ account = myAccountId() });
  };

  private func myAccountId() : Account.AccountIdentifier {
    Account.accountIdentifier(Principal.fromActor(Subscriptions), Account.defaultSubaccount());
  };

  // func principalKey(s : Principal) : Trie.Key<Principal> {
  //       { key = s; hash = Principal.hash(s) };
  // };

  // func textKey(s : Text) : Trie.Key<Text> {
  //       { key = s; hash = Text.hash(s) };
  // };

  let errInvalidToken =
    #err({
       message = ?"This token is not yet supported. Currently, this canister supports ICP.";
       kind = #InvalidToken;
  });

  public query func get_account_identifier (args : T.GetAccountIdentifierArgs) : async T.GetAccountIdentifierResult {
    let token = args.token;
    let principal = args.principal;
    let canisterId = Principal.fromActor(Subscriptions);
    switch (token.symbol) {
      case "ICP" {
        let subaccount = U.getDefaultAccount({principal; canisterId;});
        let hexEncoded = Hex.encode(
          Blob.toArray(subaccount)
        );
        let result : AccountIdentifier = #text(hexEncoded);
        #ok({accountIdentifier = result});
      };
      case _ {
        errInvalidToken;
      };
    };
  };

  public func accountIdentifierToBlob (accountIdentifier : AccountIdentifier) : async T.AccountIdentifierToBlobResult {
    U.accountIdentifierToBlob({
      accountIdentifier;
      canisterId = ?Principal.fromActor(Subscriptions);
    });
  };
// #endregion








// #region - Upgrading state
  system func preupgrade() {
   


    _subMap := [];
        for (subs in subMap.entries()){
            // entry1: (ArtistID, SubInfo)
            let artistID : ArtistID = subs.0;
            let subInfo: SubInfo = subs.1;
            for (details in subInfo.entries()){
                // FanID, (Timestamp, SubPrice, SubscriptionType)
                let fanID : FanID = details.0;
                let timestamp : Timestamp = details.1.0;
                let subPrice : SubPrice = details.1.1;
                let subType : SubType = details.1.2;
                let ticker : Ticker = details.1.3;
  
                _subMap := Array.append(_subMap, [(artistID, (fanID,(timestamp, subPrice, subType, ticker)))]);
            };
        };

  };


  system func postupgrade() {

    for (entry in _subMap.vals()){
        let artistID : ArtistID = entry.0;
        let fanID: FanID = entry.1.0;
        let timestamp : Timestamp = entry.1.1.0;
        let subPrice : SubPrice = entry.1.1.1;
        let subType : SubType = entry.1.1.2;
        let ticker : Ticker = entry.1.1.3;

        
        switch (subMap.get(artistID)){
            case (?subInfo){

                subInfo.put(fanID,(timestamp, subPrice, subType, ticker));
                subMap.put(artistID, subInfo);
            };
            case (_){
                let subInfo: SubInfo = Map.HashMap<FanID, (Timestamp, SubPrice, SubType, Ticker)>(1, Principal.equal, Principal.hash);
                subInfo.put(fanID,(timestamp, subPrice, subType, ticker));
                subMap.put(artistID, subInfo);
            };
        };
    };

    
  };
// #endregion
}
