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

actor PPV {
  type ContentID                 = T.ContentID;
  type Content                   = T.Content;
  type ArtistID                  = T.ArtistID;
  type FanID                     = T.FanID;
  type AdminID                   = T.AdminID;
  type AccountIdentifier         = T.AccountIdentifier;
  type Ticker                    = T.Ticker;
  type Timestamp                 = T.Timestamp;
  type SubAccount                = Blob;
  type Percentage                = T.Percentage;
  type TransactionID             = T.TransactionID;
  type Participants              = T.Participants;

  private type FanToTime         = Map.HashMap<FanID, (Timestamp, Nat64, Ticker)>;
  

  stable var TRAX_ACCOUNT: Text = "2l26f-kcxq2-2rwa7-zy36b-3wive-m3hfd-xrbr4-gocr4-7rklt-gmj4y-nqe";
  let FEE : Nat64 = 10000;
  stable var txNo : Nat64 = 0;


  private stable var _contentMap : [(ContentID, Content)] = [];
  private stable var _contentPaymentMap : [(ContentID, (FanID, (Timestamp, Nat64, Ticker)))] = [];

  var contentMap = Map.HashMap<ContentID, Content>(1, Text.equal, Text.hash); // ContentID -> Content data: publisherID, publisher %age, participantsID, participants %age, price. 
  var contentPaymentMap = Map.HashMap<ContentID, FanToTime>(1, Text.equal, Text.hash); // true false conditional which verifies whether a fan has paid.








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







private func checkBalance(fan: FanID, amount: Nat64) : async Bool {
    let bal = await accountBalance(fan);
    if(bal.e8s >= amount){
        return true;
    }else{
        throw Error.reject("Insufficient Balance: " # debug_show bal.e8s); 
        return false;
    }
};



// #region - PAY-PER-VIEW 
  public func purchaseContent(id: ContentID, fan: Principal, ticker: Text) : async (){
      let hasPaid = await fanHasPaid(id, fan);
      assert(hasPaid == false);
      
      var publisherID : ?ArtistID = null;
      var publisherPercentage : Percentage = 0;
      var participants: [Participants] = [];
      let now = Time.now();
      let priceCrypto = await getExchangeRate(ticker);
      var amountToSend : Nat64 = 0;
      let bal = await accountBalance(fan);
      


      switch(contentMap.get(id)){
        case(?content){
          Debug.print("Price of content: " # debug_show content.price);
          
          let amountICP: Nat64 =  Nat64.fromIntWrap(Float.toInt((content.price / priceCrypto) * 100000000));

        //   let checks =  await checkBalance(fan, amountICP);
        //   if(checks == false){
        //     return;
        //   };
          
          amountToSend := await platformDeduction(fan, amountICP);
          publisherID := ?content.publisher;
          publisherPercentage := content.publisherPercentage;
          participants := content.participants;
        }; 
        case null { throw Error.reject("Could not find content object"); }
      };

        for(collabs in Iter.fromArray(participants)){
          let participantsCut : Nat64 = await getDeductedAmount(amountToSend, collabs.participantPercentage);

          switch(await transfer(fan, collabs.participantID, participantsCut)){
              case(#ok(res)){ 

                Debug.print("Paid artist: " # debug_show collabs.participantID #" amount: "# debug_show participantsCut #  " in block " # debug_show res);
              }; case(#err(msg)){   throw Error.reject("Unexpected error: " # debug_show msg);    };
            };
        };


        switch(publisherID){  
          case (?artist) { 
            let publishersCut :  Nat64 = await getDeductedAmount(amountToSend, publisherPercentage);
  
              switch(await transfer(fan, artist, publishersCut)){
                    case(#ok(res)){ 

                      switch(contentPaymentMap.get(id)){
                        case(?innerMap){
                          
                          innerMap.put(fan, (now, amountToSend, ticker));
                          
                        }; case null {
                          var x = Map.HashMap<FanID, (Timestamp, Nat64, Ticker)>(2, Principal.equal, Principal.hash);
                          x.put(fan, (now, amountToSend, ticker));
                          contentPaymentMap.put(id, x);
                        }
                      };
                      Debug.print("Paid artist: " # debug_show artist # " in block " # debug_show res);
                    }; case(#err(msg)){   throw Error.reject("Unexpected error: " # debug_show msg);    };
                  };
          }; case null { };
        };
  };



  public shared({caller}) func addPPVContent(id: ContentID, content : Content): async () { 

    if (not U.isAdmin(caller)) {
      throw Error.reject("Unauthorized access. Caller is not an admin. " # Principal.toText(caller));
    };

    assert(content.price > 0);
    switch(contentMap.get(id)){
      case(?exists){
        throw Error.reject("This content ID has been taken");
      }; case null {
        contentMap.put(id, content);    
      }
    };
  };



  public shared({caller}) func updatePPVContent (id: ContentID, content: Content) :  async (){
    if (not U.isAdmin(caller)) {
      throw Error.reject("Unauthorized access. Caller is not an admin. " # Principal.toText(caller));
    };

    switch(contentMap.get(id)){
      case(?exists){
        // contentMap.delete(id);
        // await addPPVContent(id, content);
        let update = contentMap.replace(id, content);

      }; case null{

      };
    };
  };

  public shared({caller}) func removeContent(id: ContentID): async () {    
    if (not U.isAdmin(caller)) {
      throw Error.reject("Unauthorized access. Caller is not an admin. " # Principal.toText(caller));
    };
    contentMap.delete(id);   
  };


  public query func fanHasPaid(id: ContentID, fan: FanID) : async Bool{ 

    switch(contentPaymentMap.get(id)){
      case(?nestedMap){
        switch(nestedMap.get(fan)){
          case(?timestamp){
            if (timestamp.0 > 0){   
              true;
            }else{
              false;
            }
          };case null false;
        };
      };case null false;
    };
  };


  public query func showEntriesOfContentMap () : async [(ContentID, Content)] {   Iter.toArray(contentMap.entries());   };



  public query func getAllArtistContentIDs(artist: ArtistID) : async [ContentID] {

    var ids = Buffer.Buffer<ContentID>(2);

    for(entries in contentMap.entries()){
      if(entries.1.publisher == artist){
        var id = entries.0;
        ids.add(id);
      } else {
        for(i in Iter.fromArray(entries.1.participants)){
          if(artist == i.participantID){
            var id = entries.0;
            Debug.print("getAllArtistContentIDs id: " # debug_show id);
            ids.add(id);

          };
          Debug.print("getAllArtistContentIDs ids: " # debug_show Buffer.toArray(ids));
        };
      };
    };
    return Buffer.toArray(ids);
  };



  public func getAllArtistContentPayments(artist: ArtistID) : async [(FanID, Timestamp, Nat64)]{  
    let contentIds =  await getAllArtistContentIDs(artist);

    var res = Buffer.Buffer<(FanID, Timestamp, Nat64)>(2);

    for(entries in contentPaymentMap.entries()){

      for(eachId in Iter.fromArray(contentIds)){

        if(entries.0 == eachId){
          switch(contentPaymentMap.get(entries.0)){
            case(?innerMap){
              for(i in innerMap.entries()){
                var fanId: FanID = i.0;
                var timestamp: Timestamp = i.1.0;
                var amount: Nat64 = i.1.1;
                res.add(fanId, timestamp, amount);
              }
            }; case null { }
          };
          
        };
      };
    };
    return Buffer.toArray(res);

  };

  public func getAllFanContentPayments(fan: FanID) : async [(ContentID, Timestamp, Nat64)]{ 
    var res = Buffer.Buffer<(ContentID, Timestamp, Nat64)>(2);

    for(entries in contentPaymentMap.entries()){ 
      switch(contentPaymentMap.get(entries.0)){
        case(?innerMap){
          for(i in innerMap.entries()){
            if(i.0 == fan){
                var contentId: ContentID = entries.0;
                var timestamp: Timestamp = i.1.0;
                var amount: Nat64 = i.1.1;
                res.add(contentId, timestamp, amount);
            }
          }
        }; case null {};
      }
    };
    return Buffer.toArray(res);
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

















// #region - SHARED FUNCTIONS  

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











// #region UTILS


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
    Account.accountIdentifier(Principal.fromActor(PPV), Account.defaultSubaccount());
  };

//   func principalKey(s : Principal) : Trie.Key<Principal> {
//         { key = s; hash = Principal.hash(s) };
//   };

//   func textKey(s : Text) : Trie.Key<Text> {
//         { key = s; hash = Text.hash(s) };
//   };
let errInvalidToken =
    #err({
       message = ?"This token is not yet supported. Currently, this canister supports ICP.";
       kind = #InvalidToken;
  });

  public query func get_account_identifier (args : T.GetAccountIdentifierArgs) : async T.GetAccountIdentifierResult {
    let token = args.token;
    let principal = args.principal;
    let canisterId = Principal.fromActor(PPV);
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
      canisterId = ?Principal.fromActor(PPV);
    });
  };
// #endregion











// #region - Upgrading state
  system func preupgrade() {
    _contentMap := Iter.toArray(contentMap.entries());
    // _artistTotalSubRevenue := Iter.toArray(artistTotalSubRevenue.entries());
    // _tokensMap := Iter.toArray(tokensMap.entries());

    _contentPaymentMap := [];
        for (fanPayment in contentPaymentMap.entries()){
            // entry1: (FanID, FanPaymentInfo)
            let contentID : ContentID = fanPayment.0;
            let paymentInfo: FanToTime = fanPayment.1;
            for (payment in paymentInfo.entries()){
                // offer : (ContentID, Nat64)
                let fanID : FanID = payment.0;
                let timestamp : Timestamp = payment.1.0;
                let amount : Nat64 = payment.1.1;
                let ticker : Ticker = payment.1.2;
  
                _contentPaymentMap := Array.append(_contentPaymentMap, [(contentID,(fanID, (timestamp, amount, ticker)))])
            };
        };
  };


  system func postupgrade() {
    contentMap := Map.fromIter<ContentID, Content>(_contentMap.vals(), 10, Text.equal, Text.hash);
    _contentMap := [];


    for (entry in _contentPaymentMap.vals()){
        // entry: (FanID, (ContentID, Nat64))
        let fanID : FanID = entry.1.0;
        let id : ContentID =  entry.0;
        let timestamp : Timestamp = entry.1.1.0;
        let amount : Nat64 = entry.1.1.1;
        let ticker : Ticker = entry.1.1.2;
        
        switch (contentPaymentMap.get(id)){
            case (?contentPayment){
                // offer is a hashmap
                contentPayment.put(fanID, (timestamp, amount, ticker));
                contentPaymentMap.put(id, contentPayment);
            };
            case (_){
                let contentPayment: FanToTime = Map.HashMap<FanID, (Timestamp, Nat64, Ticker)>(1, Principal.equal, Principal.hash);
                contentPayment.put(fanID, (timestamp, amount, ticker));
                contentPaymentMap.put(id, contentPayment);
            };
        };
    };
    
  };
// #endregion
}