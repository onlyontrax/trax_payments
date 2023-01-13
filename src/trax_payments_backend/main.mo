import Hash       "mo:base/Hash";
import Map        "mo:base/HashMap";
import Principal  "mo:base/Principal";
import Nat        "mo:base/Nat";
import Nat32      "mo:base/Nat32";
import Nat64      "mo:base/Nat64";
import Text       "mo:base/Text";
import Iter       "mo:base/Iter";
import Float      "mo:base/Float";
import T          "./types";
import Ledger     "canister:ledger";
import Account    "./account";
import Time       "mo:base/Time";
import Int        "mo:base/Int";
import Error      "mo:base/Error";
import Debug      "mo:base/Debug";
import Result     "mo:base/Result";
import U          "./utils";
import Hex        "./Hex";
import Blob       "mo:base/Blob";
import Array      "mo:base/Array";
import Buffer     "mo:base/Buffer";
import Trie       "mo:base/Trie";
import TrieMap    "mo:base/TrieMap";
import Cycles "mo:base/ExperimentalCycles";

// import E "../exchange_rate/main";

actor Payments {
  // minter2 phrase: hundorp

  // TODO
  // * Top up trax token balance with ICP
  //    - Integrate exhange rate SC for use of live price data (ICP -> USD -> trax tokens)
  //    - DS to track and update state of token deposits
  //    - Func should take principal of fan only and send funds to trax account

  type ContentID                 = T.ContentID;
  type Content                   = T.Content;
  type ArtistID                  = T.ArtistID;
  type FanID                     = T.FanID;
  type AdminID                   = T.AdminID;
  type AccountIdentifier         = T.AccountIdentifier;
  type ICPTs                     = T.ICPTs;
  
  public type SubAccount         = Blob;
  type Percentage                = T.Percentage;
  public type Participants       = T.Participants;
  private type ContentToAmount   = Map.HashMap<ContentID, Nat64>;
  private type TippingInfo       = Map.HashMap<FanID, Nat64>;
  private type DateToInfo        = Map.HashMap<Text, (Nat64, Float)>;
  //                                            FanID =>  subPriceUSD
  private type FanToSubPayment   = Map.HashMap<Principal, Float>;


  
  
  // var nextContentId : contentID = 0;
  let TRAX_ACCOUNT = "2l26f-kcxq2-2rwa7-zy36b-3wive-m3hfd-xrbr4-gocr4-7rklt-gmj4y-nqe";
  // user testing account: gmv5g-o74g2-2qqbh-mmjtk-rmegk-yjl3k-ptcpg-agawk-lxmx6-zvlml-7ae
  let FEE : Nat64 = 10000;

  //PPV
  private stable var _contentMap : [(ContentID, Content)] = [];
  private stable var _artistTotalContentMap : [(ArtistID, Nat64)] = [];
  private stable var _fanPaymentMap : [(FanID, (ContentID, Nat64))] = [];
  private stable var _artistTotalPerContentMap : [(ArtistID, (ContentID, Nat64))] = [];

  var contentMap = Map.HashMap<ContentID, Content>(1, Text.equal, Text.hash);
  var artistTotalContentMap = Map.HashMap<ArtistID, Nat64>(1, Principal.equal, Principal.hash); // total amount received from all content purchases
  var artistTotalPerContentMap = Map.HashMap<ArtistID, ContentToAmount>(1, Principal.equal, Principal.hash); // total amount received for each contentID 
  var fanPaymentMap = Map.HashMap<FanID, ContentToAmount>(1, Principal.equal, Principal.hash);

  //TIPPING 
  private stable var _artistTotalMap : [(ArtistID, Nat64)] = [];
  private stable var _tippingMap : [(ArtistID, (FanID, Nat64))] = [];
  
  var tippingMap = Map.HashMap<ArtistID, TippingInfo>(1, Principal.equal, Principal.hash);
  var artistTotalTipsMap = Map.HashMap<ArtistID, Nat64>(1, Principal.equal, Principal.hash);

  // SUBSCRIPTIONS
  private stable var _tokensMap : [(Principal, (Text, (Nat64, Float)))] = [];

  var tokensMap = Map.HashMap<Principal, DateToInfo>(1, Principal.equal, Principal.hash);
  var userTraxWallet = Map.HashMap<Principal, Nat64>(1, Principal.equal, Principal.hash);
  var artistToPaySub = Map.HashMap<Principal, FanToSubPayment>(1, Principal.equal, Principal.hash);











// #region - PPV Changing State 
  public func addContent(id: ContentID, content : Content): async () {   
    switch(contentMap.get(id)){
      case(?exists){
        throw Error.reject("This content ID has been taken");
      }; case null {
        contentMap.put(id, content);    
      }
    };
  };


  public func removeContent(id: ContentID): async () {    contentMap.delete(id);    };


  private func putFanPaymentMap(fan: FanID, status: Map.HashMap<ContentID, Nat64>){   fanPaymentMap.put(fan, status);   };


  private func putArtistTotalContentMap(artist: ArtistID, amount: Nat64){   artistTotalContentMap.put(artist, amount);    };


  private func putArtistTotalPerContentMap(artist: ArtistID, status: Map.HashMap<ContentID, Nat64>){   artistTotalPerContentMap.put(artist, status);   };


  private func updateArtistTotalContentMap(artist: ArtistID, amount: Nat64): async (?Nat64){
    switch(artistTotalContentMap.get(artist)){
      case(?currVal){
        let newAmount = currVal + amount;
        artistTotalContentMap.replace(artist, newAmount)

      };case null null;
    };
  };

  private func updateArtistTotalPerContentMap(artist: ArtistID, id: ContentID, amount: Nat64): async (?Nat64){
    switch(artistTotalPerContentMap.get(artist)){
      case(?innerMap){
        switch(innerMap.get(id)){
          case(?currVal){
            let newAmount = currVal + amount;
            innerMap.replace(id, newAmount)

          };case null null;
        };
      };case null null;
    };
  };
// #endregion










// #region -TIPPING Changing state 
  private func putTippingMap(artist: ArtistID, status: Map.HashMap<FanID, Nat64>){    tippingMap.put(artist, status);   };

  private func putArtistTotal(artist: ArtistID, amount: Nat64){   artistTotalTipsMap.put(artist, amount);   };

  private func updateTippingMap(artist: ArtistID, amount: Nat64, fan: FanID): async (?Nat64){ // if returned == 0 (this function could not find key value pair)
    switch(tippingMap.get(artist)){
        case(?nestedMap){
            switch(nestedMap.get(fan)){
            case(?currVal){
              let newAmount = currVal + amount;
              nestedMap.replace(fan, newAmount)
            };
            case null null;
          };
        };
        case null null;
    };
  };

  private func updateArtistTotal(artist: ArtistID, amount: Nat64): async (?Nat64){ // if returned == 0 (this function could not find key value pair)
    switch(artistTotalTipsMap.get(artist)){
      case(?currVal){
        let newAmount = currVal + amount;
        artistTotalTipsMap.replace(artist, newAmount)

      };case null ?Nat64.fromNat(0);
    };
  };         

// #endregion



//  public query func http_request(req: HttpRequest): async HttpResponse {
//         if (req.url == "/metrics") {
//             let body = Metrics.metrics(stats);
//             {
//                 status_code = 200;
//                 headers = [("Content-Type", "text/plain; version=0.0.4"), ("Content-Length", Nat.toText(body.size()))];
//                 body = body;
//             }
//         } else {
//             {
//                 status_code = 404;
//                 headers = [];
//                 body = Text.encodeUtf8("Not supported");
//             }
//         }
//     };


// Step by step logic
// 1. fan deposits crypto to trax token wallet 
// 2. update token wallet balance for ICP (seperate ICP and USD bals)
// 3. crypto is sent to trax account/address
// 4. every month, per subsciption request, crypto is sent to artist from trax account (automated?)
// 5. If automated create a function that listens to network heartbeats and triggers a payment at the end of the month 
// 6. If NOT, at the end of the month we would need to send crypto to each artists manually 

public func payArtistSubscription() : async (){

};

public func payInitialSub(artist: ArtistID, fan: FanID, priceOfSub: Float, priceICP: Float): async(){
  // let balanceICP = userTraxWallet.get(fan);
  var balanceUSD : Float = 0;
  switch(userTraxWallet.get(fan)){
    case(?balanceICP){
        balanceUSD := (Float.fromInt(Nat64.toNat(balanceICP)) / 100000000) * priceICP;
    }; case null{
      throw Error.reject("Zero balance");
    }
  };
  
  assert(balanceUSD > priceOfSub);
  
  var amountAfter     = balanceUSD - priceOfSub;
  var amountToSendUSD = priceOfSub - priceICP;
  var amountToSendICP = Nat64.fromNat(Int.abs(Float.toInt((amountToSendUSD * priceICP) * 100000000)));



  // e.g. priceOfSub = 2.40 
  // ICP amount = 4.12
  // ICP balance = 12
  // ICP balance in USD = 49.44
  // 47.04 - 

  switch(await transfer(artist, fan, amountToSendICP)){

    case(#ok(res)){

      }; case(#err(msg)){

        throw Error.reject("Unexpected error: " # debug_show msg);
      };
    };
};


public func addFanToArtistSub(artist: ArtistID, fan: FanID, priceOfSub: Float) : async (){
  switch(artistToPaySub.get(artist)){
    case(?innerMap){
      innerMap.put(fan, priceOfSub);
    }; case null {
      var x : FanToSubPayment = Map.HashMap<Principal, Float>(2, Principal.equal, Principal.hash);
      x.put(fan, priceOfSub);
      artistToPaySub.put(artist, x);
    };
  };
    
};

public func getUserWalletBalance(from: Principal) : async ?(Nat64) {    userTraxWallet.get(from);   };

// #region - Transfer  
public func topUpTokenWallet(from: Principal, amount: Nat64, priceUSD: Float) : async (Text, Float){
 
    let now : Text = Int.toText(Time.now());
    var tokens: Float = 0;
    var to : Principal = Principal.fromText(TRAX_ACCOUNT);

    switch(await transfer(from, to, amount)){
      case(#ok(res)){
        // await E.get_rates((Time.now()-100),Time.now());
        tokens := (Float.fromInt(Nat64.toNat(amount)) / 100000000) * priceUSD;
        Debug.print("tokens: " # debug_show tokens);
        // let tokensFormat = Nat32.fromNat(Int.abs(Float.toInt(tokens)));

        switch(userTraxWallet.get(from)){
          case(?currVal){
            let newAmount = currVal + amount;
            let update = userTraxWallet.replace(from, newAmount);
          }; case null {
            userTraxWallet.put(from, amount);
          };
        };

        switch(tokensMap.get(from)){
          case(?innerMap){
                innerMap.put(now,(amount, tokens));
                return (now, tokens);
          };case null{
            // func (a : Nat32) : Nat32 {a;}
                var x = Map.HashMap<Text, (Nat64, Float)>(1, Text.equal, Text.hash);
                x.put(now,(amount, tokens));
                tokensMap.put(from, x);
                return (now, tokens);
          };
        };
         

      }; case(#err(msg)){
        throw Error.reject("Unexpected error: " # debug_show msg);
      };
    };
};



public func sendTip(from: FanID, to: ArtistID, amount: Nat64) : async (){

        switch(await transfer(from, to, amount)){

          case(#ok(res)){

            switch(artistTotalTipsMap.get(to)){
                case(?exists){
                    var updateWorked = await updateArtistTotal(to, amount);
                }; case null {
                    putArtistTotal(to, amount);
                };
            };
            
            switch(tippingMap.get(to)){
                case(?exists){
                    var worked = await updateTippingMap(to, amount, from);
                    if(worked == ?Nat64.fromNat(0)){
                        Debug.print("DID NOT update tipMapping for artist: " # debug_show to # " in block " # debug_show res);
                    }else{
                        Debug.print("UPDATED tipMapping for artist: " # debug_show to # " in block " # debug_show res);
                    };
                };
                case null {
                    var x : TippingInfo = Map.HashMap<FanID, Nat64>(2, Principal.equal, Principal.hash);
                    x.put(from, amount);
                    putTippingMap(to, x);
                };
            };
           

            Debug.print("Paid artist: " # debug_show to # " in block " # debug_show res);
          }; case(#err(msg)){
            throw Error.reject("Unexpected error: " # debug_show msg);
          };
        };
    };
    

public func purchaseContent(id: ContentID, fan: Principal) : async (){
    
    var price : Nat64 = 0;
    var publisherID : ?ArtistID = null;
    var publisherPercentage : Percentage = 0;
    var participants: [Participants] = [];
    let now = Time.now();

    switch(contentMap.get(id)){
      case(?content){
        Debug.print("Price of content: " # debug_show content.price);
        price := await platformDeduction(fan, content.price);
        publisherID := ?content.publisher;
        publisherPercentage := content.publisherPercentage;
        participants := content.participants;
      };
      case null { throw Error.reject("Could not find content object"); }
    };

      for(collabs in Iter.fromArray(participants)){
        let participantsCut : Nat64 = await getDeductedAmount(price, collabs.participantPercentage);

        switch(await transfer(fan, collabs.participantID, participantsCut)){
            case(#ok(res)){ 

              await artistTotalContentMapHelper(collabs.participantID, participantsCut);
              await artistTotalPerContentMapHelper(collabs.participantID, id, participantsCut);
              Debug.print("Paid artist: " # debug_show collabs.participantID #" amount: "# debug_show participantsCut #  " in block " # debug_show res);
            }; case(#err(msg)){   throw Error.reject("Unexpected error: " # debug_show msg);    };
          };
      };

      
      switch(publisherID){  
        case (?artist) { 
          let publishersCut :  Nat64 = await getDeductedAmount(price, publisherPercentage);
 
            switch(await transfer(fan, artist, publishersCut)){
                  case(#ok(res)){ 

                    switch(fanPaymentMap.get(fan)){
                      case(?innerMap){
                        innerMap.put(id, price)
                      }; case null {
                        var x = Map.HashMap<ContentID, Nat64>(2, Text.equal, Text.hash);
                        x.put(id, price);
                        putFanPaymentMap(fan, x);
                      }
                    };
                    

                    await artistTotalContentMapHelper(artist, publishersCut );
                    await artistTotalPerContentMapHelper(artist, id, publishersCut );
                    Debug.print("Paid artist: " # debug_show artist # " in block " # debug_show res);
                  }; case(#err(msg)){   throw Error.reject("Unexpected error: " # debug_show msg);    };
                };
        }; case null { };
      };

};

 

  private func platformDeduction(fan: FanID, amount : Nat64) : async Nat64 {
    let traxAccount: Principal = Principal.fromText(TRAX_ACCOUNT);
    let fee = await getDeductedAmount(amount, 0.10);
    
    switch(await transfer(fan, traxAccount, fee)){
      case(#ok(res)){
        Debug.print("Fee paid to trax account: " # debug_show traxAccount # " in block " # debug_show res);
      };case(#err(msg)){
        throw Error.reject("Unexpected error: " # debug_show msg);
      };
    };

    let amountAfterDeduction = await getRemainingAfterDeduction(amount, 0.10);
    return amountAfterDeduction;
  };


  func transfer(from: Principal, to: Principal, amount: Nat64): async Result.Result<Nat64, Text>{

    let now = Time.now();
    let res = await Ledger.transfer({
          memo = Nat64.fromNat(0); 
          from_subaccount = ?Account.principalToSubaccount(from);
          to = Account.accountIdentifier(to, Account.defaultSubaccount());
          amount = { e8s = amount };
          fee = { e8s = FEE };
          created_at_time = ?{ timestamp_nanos = Nat64.fromNat(Int.abs(now)) };
        });

        Debug.print("res: "# debug_show res);
        
        switch (res) {
          case (#Ok(blockIndex)) {
            Debug.print("Paid recipient: " # debug_show to # " in block " # debug_show blockIndex);
            return #ok(blockIndex);
          };
          case (#Err(#InsufficientFunds { balance })) {
            throw Error.reject("Insufficient balance of " # debug_show balance # " from account:" # debug_show from # "");
          };
          case (#Err(other)) {
            throw Error.reject("Unexpected error: " # debug_show other);
          };
        };
  };
// #endregion





// #region - PPV Query State
  public query func fanHasPaid(id: ContentID, fan: FanID) : async Bool{
    var price : Nat64 = 0;
    switch(contentMap.get(id)){
      case(?content){
        price := content.price;
        Debug.print("@useHasPaid - price before deduction = " # debug_show price);
      };
      case null return false; 
    };
    
    switch(fanPaymentMap.get(fan)){
      case(?nestedMap){
        switch(nestedMap.get(id)){
          case(?amount){
            Debug.print("@useHasPaid - amount = " # debug_show amount);
            let priceFloat : Float = Float.fromInt(Nat64.toNat(price));
            let deduction :  Float = priceFloat * 0.10;
            Debug.print("@useHasPaid - price after deduction = " # debug_show Nat64.fromNat(Int.abs(Float.toInt(priceFloat - deduction))));
            if(Nat64.fromNat(Int.abs(Float.toInt(priceFloat - deduction))) == amount){
              return true;
            }else return false;
          };case null return false;
        };
      };case null return false;
    };
  };

  // public query func getFanPaymentMap(fan: FanID) :

  public query func showSize () : async Nat {   contentMap.size();    };

  public query func getContentMapByID(id : Text) : async ?Content {   contentMap.get(id);   };

  public query func showEntriesOfContentMap () : async [(ContentID, Content)] {   Iter.toArray(contentMap.entries());   };

  public query func getArtistTotalContentMap(artist: ArtistID) : async ?Nat64{    artistTotalContentMap.get(artist);    };

  public query func getArtistTotalPerContentMap(artist: ArtistID, contentID: ContentID) : async ?Nat64{
      switch(artistTotalPerContentMap.get(artist)){
        case(?innerMap){
          innerMap.get(contentID);
        }; case null { null };
      }
  };
// #endregion




//#region - TOKEN WALLET Query state
public func getAllTokenWalletTransfers(user: Principal) : async ?[(Text, (Nat64, Float))]{
  switch(tokensMap.get(user)){
    case(?innerMap){
       return ?Iter.toArray<(Text, (Nat64, Float))>(innerMap.entries());

    }; case null return null;
  };
};
//#endregion




//#region - TIPPING Query state
    public func getTippingMap(artist: ArtistID, fan: FanID) : async ?Nat64 {
            switch(tippingMap.get(artist)){
              case(?nestedMap){
                nestedMap.get(fan)
              };
              case null{
                return ?Nat64.fromNat(0);
              }
            };
    };

    public func getArtistTotalTipsMap(artist: ArtistID) : async ?Nat64 {    artistTotalTipsMap.get(artist);   };
//#endregion








// #region - PPV Helper functions 
  private func artistTotalContentMapHelper(artist: ArtistID, amount: Nat64) : async (){
      switch(artistTotalContentMap.get(artist)){
        case(?exists){
          var update = await updateArtistTotalContentMap(artist, amount);
        }; case null {
          putArtistTotalContentMap(artist, amount);
        };
      };
  };


  private func artistTotalPerContentMapHelper(artist: ArtistID, id: ContentID, amount: Nat64) : async () {

    switch(artistTotalPerContentMap.get(artist)){
      case(?innerMap){
        switch(innerMap.get(id)){
          case(?exists){
              var update = await updateArtistTotalPerContentMap(artist, id, amount);
          };case null {
              // var x : ArtistContentInfo = Map.HashMap<ContentID, Nat64>(1, Text.equal, Text.hash);
              // x.put(id, amount);
              // putArtistTotalPerContentMap(artist, x);
              innerMap.put(id, amount);
          };
        };
        
      }; case null {
        var x : ContentToAmount = Map.HashMap<ContentID, Nat64>(1, Text.equal, Text.hash);
        x.put(id, amount);
        putArtistTotalPerContentMap(artist, x);
      }
    };
  };

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










// #region Utils
  public func accountIdentifierToBlob (accountIdentifier : AccountIdentifier) : async T.AccountIdentifierToBlobResult {
    U.accountIdentifierToBlob({
      accountIdentifier;
      canisterId = ?Principal.fromActor(Payments);
    });
  };

  public func getAccountIdentifier(caller : Principal) : async Blob {
    Account.accountIdentifier(caller, Account.defaultSubaccount());
  };

  public shared func getSubaccount(fan: Principal): async Blob {
        Account.principalToSubaccount(fan);
  };


  let errInvalidToken =
    #err({
       message = ?"This token is not yet supported. Currently, this canister supports ICP.";
       kind = #InvalidToken;
  });

  public query func get_account_identifier (args : T.GetAccountIdentifierArgs) : async T.GetAccountIdentifierResult {
    let token = args.token;
    let principal = args.principal;
    let canisterId = Principal.fromActor(Payments);
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
    Account.accountIdentifier(Principal.fromActor(Payments), Account.defaultSubaccount());
  };

  func principalKey(s : Principal) : Trie.Key<Principal> {
        { key = s; hash = Principal.hash(s) };
  };

  func textKey(s : Text) : Trie.Key<Text> {
        { key = s; hash = Text.hash(s) };
  };
// #endregion





// #region - Upgrading state
  system func preupgrade() {
    _contentMap := Iter.toArray(contentMap.entries());
    _artistTotalMap := Iter.toArray(artistTotalTipsMap.entries());
    _artistTotalContentMap := Iter.toArray(artistTotalContentMap.entries());
    // _tokensMap := Iter.toArray(tokensMap.entries());

    _tokensMap := [];
        for (tokensInfo in tokensMap.entries()){
            // entry1: (FanID, FanPaymentInfo)
            let user : Principal = tokensInfo.0;
            let dateToInfo: DateToInfo = tokensInfo.1;
            for (info in dateToInfo.entries()){
                // offer : (ContentID, Nat64)
                let timestamp : Text = info.0;
                let amount : Nat64 = info.1.0;
                let tokens : Float = info.1.1;
  
                _tokensMap := Array.append(_tokensMap, [(user,(timestamp,(amount, tokens)))]);
            };
        };

    _fanPaymentMap := [];
        for (fanPayment in fanPaymentMap.entries()){
            // entry1: (FanID, FanPaymentInfo)
            let fanID : FanID = fanPayment.0;
            let paymentInfo: ContentToAmount = fanPayment.1;
            for (payment in paymentInfo.entries()){
                // offer : (ContentID, Nat64)
                let id : ContentID = payment.0;
                let price : Nat64 = payment.1;
  
                _fanPaymentMap := Array.append(_fanPaymentMap, [(fanID,(id, price))])
            };
        };
    
    _tippingMap := [];
        for (tipping in tippingMap.entries()){
            // entry1: (ArtistID, (FanID, Nat64))
            let artistID : ArtistID = tipping.0;
            let tippingInfo: TippingInfo = tipping.1;
            for (info in tippingInfo.entries()){
                // offer : (Principal,(Price,Time.Time))
                let fanID : FanID = info.0;
                let amount : Nat64 = info.1;
  
                _tippingMap := Array.append(_tippingMap, [(artistID,(fanID, amount))])
            };
        };
    
    _artistTotalPerContentMap := [];
      for (perContent in artistTotalPerContentMap.entries()){
            // entry1: (ArtistID, (ContentID, Nat64))
            let artistID : ArtistID = perContent.0;
            let contentInfo: ContentToAmount = perContent.1;
            for (content in contentInfo.entries()){
                // offer : (Principal,(Price,Time.Time))
                let contentID : ContentID = content.0;
                let amount : Nat64 = content.1;
  
                _artistTotalPerContentMap := Array.append(_artistTotalPerContentMap, [(artistID,(contentID, amount))])
            };
        };
  };


  system func postupgrade() {
    _contentMap := [];
    _artistTotalMap := [];
    _artistTotalContentMap := [];
    _tokensMap := [];

    for (entry in _fanPaymentMap.vals()){
        // entry: (FanID, (ContentID, Nat64))
        let fanID : FanID = entry.0;
        let id : ContentID =  entry.1.0;
        let price : Nat64 = entry.1.1;
        
        switch (fanPaymentMap.get(fanID)){
            case (?fanPayment){
                // offer is a hashmap
                fanPayment.put(id, price);
                fanPaymentMap.put(fanID, fanPayment);
            };
            case (_){
                let fanPayment: ContentToAmount = Map.HashMap<ContentID, Nat64>(1, Text.equal, Text.hash);
                fanPayment.put(id, price);
                fanPaymentMap.put(fanID, fanPayment);
            };
        };
    };
    
    for (entry in _tippingMap.vals()){
        // entry: (ArtistID, (FanID, Nat64))
        let artistID : ArtistID = entry.0;
        let fanID : FanID =  entry.1.0;
        let amount : Nat64 = entry.1.1;
        
        switch (tippingMap.get(artistID)){
            case (?tipMap){
                // offer is a hashmap
                tipMap.put(fanID, amount);
                tippingMap.put(artistID, tipMap);
            };
            case (_){
                let tipMap: TippingInfo = Map.HashMap<FanID, Nat64>(1, Principal.equal, Principal.hash);
                tipMap.put(fanID, amount);
                tippingMap.put(artistID, tipMap);
            };
        };
    };

    for (entry in _artistTotalPerContentMap.vals()){
        // entry: (ArtistID, (ContentID, Nat64))
        let artistID : ArtistID = entry.0;
        let contentID : ContentID =  entry.1.0;
        let amount : Nat64 = entry.1.1;
        
        switch (artistTotalPerContentMap.get(artistID)){
            case (?artistTotal){
                // offer is a hashmap
                artistTotal.put(contentID, amount);
                artistTotalPerContentMap.put(artistID, artistTotal);
            };
            case (_){
                let artistTotal: ContentToAmount = Map.HashMap<ContentID, Nat64>(1, Text.equal, Text.hash);
                artistTotal.put(contentID, amount);
                artistTotalPerContentMap.put(artistID, artistTotal);
            };
        };
    };

    for (entry in _tokensMap.vals()){
        // entry: (ArtistID, (ContentID, Nat64))
        let user : Principal = entry.0;
        let timestamp : Text =  entry.1.0;
        let amount : Nat64 = entry.1.1.0;
        let tokens : Float = entry.1.1.1;
        
        switch (tokensMap.get(user)){
            case (?innerMap){
                // offer is a hashmap
                innerMap.put(timestamp,(amount, tokens));
                tokensMap.put(user, innerMap);
            };
            case (_){
                let dateToInfo: DateToInfo = Map.HashMap<Text, (Nat64, Float)>(1, Text.equal, Text.hash);
                dateToInfo.put(timestamp,(amount, tokens));
                tokensMap.put(user, dateToInfo);
            };
        };
    };
  };
// #endregion
}




































// backend integration
// * find price fetching logic in api folder for ppv content and add respective sc logic
// * add function in user/src/services/token-transaction.service.ts
// * additional frontend component to handle execution of crypto payment 
// * listen for transaction hash and match user id to unlock content 


    // Debug.print("from subaccount: "# debug_show ?Account.principalToSubaccount(from));
    // Debug.print("from subaccount: "# debug_show ?Account.accountIdentifier(from, Account.defaultSubaccount()));
    // Debug.print("from subaccount: "# debug_show ?Account.accountIdentifier(Principal.fromActor(Payments), Account.principalToSubaccount(from)));
//TODO: add contentID as metadata for memo
          // ?Account.accountIdentifier(from, Account.defaultSubaccount());
          // ?Account.principalToSubaccount(from);