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

actor Payments {
  // minter phrase: hundorp

  type contentID = T.contentID;
  type Content = T.Content;
  type ArtistID = T.ArtistID;
  type UserID = T.UserID;
  type AdminID = T.AdminID;
  type AccountIdentifier = T.AccountIdentifier;
  type ICPTs = T.ICPTs;
  public type SubAccount = Blob;
  let errInvalidToken =
    #err({
       message = ?"This token is not yet supported. Currently, this canister supports ICP.";
       kind = #InvalidToken;
    });
  
  var nextContentId : contentID = 0;
  let TRAX_ACCOUNT = "2l26f-kcxq2-2rwa7-zy36b-3wive-m3hfd-xrbr4-gocr4-7rklt-gmj4y-nqe";
  // user testing account: gmv5g-o74g2-2qqbh-mmjtk-rmegk-yjl3k-ptcpg-agawk-lxmx6-zvlml-7ae
  let FEE : Nat64 = 10000;

  //PPV
  var contentMap = Map.HashMap<contentID, Content>(1, Nat32.equal, func (a : Nat32) : Nat32 {a});
  var userContentMap = Map.HashMap<UserID, Map.HashMap<contentID, Nat64>>(1, Principal.equal, Principal.hash);

  //TIPPING
  // artist address -> user address -> amount 
  var tippingMap = Map.HashMap<ArtistID, Map.HashMap<UserID, Nat64>>(1, Principal.equal, Principal.hash);
  // total amount recieved from tips 
  var artistTotalMap = Map.HashMap<ArtistID, Nat64>(1, Principal.equal, Principal.hash);







// #region - PPV Changing State 
  public func addContent(content : Content): async () {
      contentMap.put(nextContentId, content);
      nextContentId += 1
  };


  public func removeContent(id: contentID): async () {  contentMap.delete(id);  };


  private func getUserContentMap(user: UserID) : ?Map.HashMap<contentID, Nat64> {
            userContentMap.get(user);
  };


  private func putUserContentMap(user: UserID, status: Map.HashMap<contentID, Nat64>){
            userContentMap.put(user, status);
  };
// #endregion






// #region -TIPPING Changing state 
  private func putTippingMap(artist: ArtistID, status: Map.HashMap<UserID, Nat64>){
            tippingMap.put(artist, status);
  };
  private func putArtistTotal(artist: ArtistID, amount: Nat64){
            artistTotalMap.put(artist, amount);
  };

  private func replaceTippingMap(artist: ArtistID, amount: Nat64, user: UserID): async (?Nat64){ // if returned == 0 (this function could not find key value pair)
    switch(tippingMap.get(artist)){
        case(?nestedMap){
            switch(nestedMap.get(user)){
            case(?currVal){
              let newAmount = currVal + amount;
                nestedMap.replace(user, newAmount)
            };
            case null ?Nat64.fromNat(0);
            };
        };
        case null ?Nat64.fromNat(0);
    };
  };

  private func replaceArtistTotal(artist: ArtistID, amount: Nat64): async (?Nat64){ // if returned == 0 (this function could not find key value pair)
    switch(artistTotalMap.get(artist)){
      case(?currVal){
        let newAmount = currVal + amount;
        artistTotalMap.replace(artist, newAmount)

      };case null ?Nat64.fromNat(0);
    };
  };         

// #endregion






// #region - Transfer  
public func sendTip(from: UserID, to: ArtistID, amount: Nat64) : async (){

    var amountToSend = await platformDeduction(from, amount);
    

        switch(await transfer(from, to, amountToSend)){

          case(#ok(res)){

            switch(artistTotalMap.get(to)){
                case(?exists){
                    var replaceWorked = await replaceArtistTotal(to, amountToSend);
                }; case null {
                    putArtistTotal(to, amountToSend);
                };
            };
            
            switch(tippingMap.get(to)){
                case(?exists){
                    var worked = await replaceTippingMap(to, amountToSend, from);
                    if(worked == ?Nat64.fromNat(0)){
                        Debug.print("DID NOT update tipMapping for artist: " # debug_show to # " in block " # debug_show res);
                    }else{
                        Debug.print("UPDATED tipMapping for artist: " # debug_show to # " in block " # debug_show res);
                    }
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

public func purchaseContent(id: contentID, user: Principal) : async (){
    
    var price : Nat64 = 0;
    var artistID : ?ArtistID = null;
    let now = Time.now();

    switch(contentMap.get(id)){
      case(?content){
        Debug.print("Price of content: " # debug_show content.price);
        price := await platformDeduction(user, content.price);
        // price := content.price;
        artistID := ?content.publisher;
      };
      case null { throw Error.reject("Could not find content object"); }
    };
    
    switch(artistID){
      case (?principal) { 
        switch(await transfer(user, principal, price)){
          
          case(#ok(res)){
            
            var x = Map.HashMap<contentID, Nat64>(2, Nat32.equal, func (a : Nat32) : Nat32 {a});
            x.put(id, price);
            putUserContentMap(user, x);

            Debug.print("Paid artist: " # debug_show principal # " in block " # debug_show res);
          }; case(#err(msg)){
            throw Error.reject("Unexpected error: " # debug_show msg);
          };
        };
      };
      case null { };
    };
  };


  private func platformDeduction(user: UserID, amount : Nat64) : async Nat64 {
    let traxAccount: Principal = Principal.fromText(TRAX_ACCOUNT);

    let amountFloat : Float = Float.fromInt(Nat64.toNat(amount));
    let deduction :  Float = amountFloat * 0.10;
    
    switch(await transfer(user, traxAccount, Nat64.fromNat(Int.abs(Float.toInt(deduction))))){
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
  public query func userHasPaid(id: contentID, user: UserID) : async Bool{
    var price : Nat64 = 0;
    switch(contentMap.get(id)){
      case(?content){
        price := content.price;
        Debug.print("@useHasPaid - price before deduction = " # debug_show price);
      };
      case null return false; 
    };
    
    switch(userContentMap.get(user)){
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

  public query func showSize () : async Nat {
      contentMap.size();
  };

  public query func lookup(id : Nat32) : async ?Content {
      contentMap.get(id);
  };


  public query func showEntries () : async [(contentID, Content)] {
      Iter.toArray(contentMap.entries());
  };

  public query func getCurrentContentID() : async contentID {
    nextContentId;
  };
// #endregion






//#region - TIPPING Query state
    public func getTippingMap(artist: ArtistID, user: UserID) : async ?Nat64 {
            switch(tippingMap.get(artist)){
              case(?nestedMap){
                nestedMap.get(user)
              };
              case null{
                return ?Nat64.fromNat(0)
              }
            };
    };
    public func getArtistTotalMap(artist: ArtistID) : async ?Nat64 {
            artistTotalMap.get(artist);
    };
//#endregion






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

  public shared func getSubaccount(user: Principal): async Blob {
        Account.principalToSubaccount(user);
  };


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