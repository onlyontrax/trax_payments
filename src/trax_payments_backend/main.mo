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
import Buffer      "mo:base/Buffer";

actor Payments {
  // minter2 phrase: hundorp

  // TODO
  // * Top up trax token balance with ICP
  //    - Integrate exhange rate smart contract for live price fetching (to convert ICP into tokens)
  //    - hashmap to track payments and query in future
  //    - Func should take principal of fan only and send funds to trax account
  // * Enable royalty-sharing for PPV content

  type ContentID              = T.ContentID;
  type Content                = T.Content;
  type ArtistID               = T.ArtistID;
  type FanID                  = T.FanID;
  type AdminID                = T.AdminID;
  type AccountIdentifier      = T.AccountIdentifier;
  type ICPTs                  = T.ICPTs;
  public type SubAccount      = Blob;
  type Percentage             = T.Percentage;
  type ParticipantID          = T.ParticipantID;
  public type Participants    = T.Participants;
  private type FanPaymentInfo = Map.HashMap<ContentID, Nat64>;
  private type ArtistContentInfo = Map.HashMap<ContentID, Nat64>;
  private type TippingInfo    = Map.HashMap<FanID, Nat64>;

  
  
  // var nextContentId : contentID = 0;
  let TRAX_ACCOUNT = "2l26f-kcxq2-2rwa7-zy36b-3wive-m3hfd-xrbr4-gocr4-7rklt-gmj4y-nqe";
  // user testing account: gmv5g-o74g2-2qqbh-mmjtk-rmegk-yjl3k-ptcpg-agawk-lxmx6-zvlml-7ae
  let FEE : Nat64 = 10000;

  //PPV
  private stable var _contentMap : [(ContentID, Content)] = [];
  private stable var _artistTotalContentMap : [(ArtistID, Nat64)] = [];
  private stable var _fanPaymentMap : [(FanID, (ContentID, Nat64))] = [];

  var contentMap = Map.HashMap<ContentID, Content>(1, Text.equal, Text.hash);
  var artistTotalContentMap = Map.HashMap<ArtistID, Nat64>(1, Principal.equal, Principal.hash); // total amount received from all content purchases
  var artistTotalPerContentMap = Map.HashMap<FanID, ArtistContentInfo>(1, Principal.equal, Principal.hash); // total amount received for each contentID 
  var fanPaymentMap = Map.HashMap<FanID, FanPaymentInfo>(1, Principal.equal, Principal.hash);

  //TIPPING 
  private stable var _artistTotalMap : [(ArtistID, Nat64)] = [];
  private stable var _tippingMap : [(ArtistID, (FanID, Nat64))] = [];
  
  var tippingMap = Map.HashMap<ArtistID, TippingInfo>(1, Principal.equal, Principal.hash); 
  var artistTotalTipsMap = Map.HashMap<ArtistID, Nat64>(1, Principal.equal, Principal.hash);

  // SUBSCRIPTIONS
  private stable var _tokensMap : [(ArtistID, (Nat64, Nat32))] = [];

  var tokensMap = Map.HashMap<ArtistID, (Nat64, Nat32)>(1, Principal.equal, Principal.hash);










// #region - Upgrading state
  system func preupgrade() {
    _contentMap := Iter.toArray(contentMap.entries());
    _artistTotalMap := Iter.toArray(artistTotalTipsMap.entries());
    _artistTotalContentMap := Iter.toArray(artistTotalContentMap.entries());
    _tokensMap := Iter.toArray(tokensMap.entries());

    _fanPaymentMap := [];
        for (fanPayment in fanPaymentMap.entries()){
            // entry1: (FanID, FanPaymentInfo)
            let fanID : FanID = fanPayment.0;
            let paymentInfo: FanPaymentInfo = fanPayment.1;
            for (payment in paymentInfo.entries()){
                // offer : (ContentID, Nat64)
                let id : ContentID = payment.0;
                let price : Nat64 = payment.1;
  
                _fanPaymentMap := Array.append(_fanPaymentMap, [(fanID,(id, price))])
            };
        };
    
    _tippingMap := [];
        for (tipping in tippingMap.entries()){
            // entry1: (Nat, OfferInfo)
            let artistID : FanID = tipping.0;
            let tippingInfo: TippingInfo = tipping.1;
            for (info in tippingInfo.entries()){
                // offer : (Principal,(Price,Time.Time))
                let fanID : FanID = info.0;
                let amount : Nat64 = info.1;
  
                _tippingMap := Array.append(_tippingMap, [(artistID,(fanID, amount))])
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
                let fanPayment: FanPaymentInfo = Map.HashMap<ContentID, Nat64>(1, Text.equal, Text.hash);
                fanPayment.put(id, price);
                fanPaymentMap.put(fanID, fanPayment);
            };
        };
    };
    
    for (entry in _tippingMap.vals()){
        // entry: (ArtistID, (FanID, Nat64))
        let artistID : FanID = entry.0;
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
  };
// #endregion










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

      };case null ?Nat64.fromNat(0);
    };
  };

  private func updateArtistTotalPerContentMap(artist: ArtistID, id: ContentID, amount: Nat64): async (?Nat64){
    switch(artistTotalPerContentMap.get(artist)){
      case(?innerMap){
        switch(innerMap.get(id)){
          case(?currVal){
            let newAmount = currVal + amount;
            innerMap.replace(id, newAmount)

          };case null ?Nat64.fromNat(0);
        };
      };case null ?Nat64.fromNat(0);
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
            case null ?Nat64.fromNat(0);
          };
        };
        case null ?Nat64.fromNat(0);
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










// #region - Transfer  
public func topUpTokenWallet(amount: Nat64, from: Principal) : async (){
    // TODO:
    // * Transfer amount to trax account
    // * fetch exchange rate of ICP / USD pair
    // * Convert USD value to tokens ($1 to 1 token) 
    // * Update state 

};


public func sendTip(from: FanID, to: ArtistID, amount: Nat64) : async (){

    var amountToSend = await platformDeduction(from, amount);
    

        switch(await transfer(from, to, amountToSend)){

          case(#ok(res)){

            switch(artistTotalTipsMap.get(to)){
                case(?exists){
                    var updateWorked = await updateArtistTotal(to, amountToSend);
                }; case null {
                    putArtistTotal(to, amountToSend);
                };
            };
            
            switch(tippingMap.get(to)){
                case(?exists){
                    var worked = await updateTippingMap(to, amountToSend, from);
                    if(worked == ?Nat64.fromNat(0)){
                        Debug.print("DID NOT update tipMapping for artist: " # debug_show to # " in block " # debug_show res);
                    }else{
                        Debug.print("UPDATED tipMapping for artist: " # debug_show to # " in block " # debug_show res);
                    };
                };
                case null {
                    var x = Map.HashMap<Principal, Nat64>(2, Principal.equal, Principal.hash);
                    x.put(from, amountToSend);
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
        
        let amountFloat : Float = Float.fromInt(Nat64.toNat(price));
        let participantsCut : Nat64 =  Nat64.fromNat(Int.abs(Float.toInt(amountFloat * collabs.participantPercentage)));

        switch(await transfer(fan, collabs.participantID, participantsCut)){
            case(#ok(res)){ 

              await artistTotalContentMapHelper(collabs.participantID, participantsCut);
              await artistTotalPerContentMapHelper(collabs.participantID, id, participantsCut);
              Debug.print("Paid artist: " # debug_show collabs.participantID # " in block " # debug_show res);
            }; case(#err(msg)){   throw Error.reject("Unexpected error: " # debug_show msg);    };
          };
      };

      
      switch(publisherID){  
        case (?artist) { 
          let pubAmountFloat : Float = Float.fromInt(Nat64.toNat(price));
          let publishersCut :  Nat64 = Nat64.fromNat(Int.abs(Float.toInt(pubAmountFloat * publisherPercentage)));
 
            switch(await transfer(fan, artist, publishersCut)){
                  case(#ok(res)){ 

                    var x = Map.HashMap<ContentID, Nat64>(2, Text.equal, Text.hash);
                    x.put(id, price);
                    putFanPaymentMap(fan, x);

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

    let amountFloat : Float = Float.fromInt(Nat64.toNat(amount));
    let deduction :  Float = amountFloat * 0.10;
    
    switch(await transfer(fan, traxAccount, Nat64.fromNat(Int.abs(Float.toInt(deduction))))){
      case(#ok(res)){
        Debug.print("Fee paid to trax account: " # debug_show traxAccount # " in block " # debug_show res);
      };case(#err(msg)){
        throw Error.reject("Unexpected error: " # debug_show msg);
      };
    };

    return Nat64.fromNat(Int.abs(Float.toInt(amountFloat - deduction)));
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

  public query func showSize () : async Nat {   contentMap.size();    };

  public query func getContentMapByID(id : Text) : async ?Content {   contentMap.get(id);   };

  public query func showEntriesOfContentMap () : async [(ContentID, Content)] {   Iter.toArray(contentMap.entries());   };

  public query func getArtistTotalContentMap(artist: ArtistID) : async ?Nat64{    artistTotalContentMap.get(artist);    };
// #endregion











//#region - TIPPING Query state
    public func getTippingMap(artist: ArtistID, fan: FanID) : async ?Nat64 {
            switch(tippingMap.get(artist)){
              case(?nestedMap){
                nestedMap.get(fan)
              };
              case null{
                return ?Nat64.fromNat(0)
              }
            };
    };

    public func getArtistTotalMap(artist: ArtistID) : async ?Nat64 {    artistTotalTipsMap.get(artist);   };
//#endregion










// #region - PPV Helper function 
private func artistTotalContentMapHelper(artist: ArtistID, amount: Nat64) : async (){
    switch(artistTotalContentMap.get(artist)){
      case(?exists){
        var update = await updateArtistTotalContentMap(artist, amount);
      }; case null {
        putArtistTotalContentMap(artist, amount);
      }
    };
 };

 private func artistTotalPerContentMapHelper(artist: ArtistID, id: ContentID, amount: Nat64) : async () {
    switch(artistTotalPerContentMap.get(artist)){
      case(?exists){
        var update = await updateArtistTotalPerContentMap(artist, id, amount);
      }; case null {
        var x = Map.HashMap<ContentID, Nat64>(2, Text.equal, Text.hash);
        x.put(id, amount);
        putArtistTotalPerContentMap(artist, x);
      }
    };
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
    myAccountId()
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