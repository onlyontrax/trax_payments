import Map        "mo:base/HashMap";
import Principal  "mo:base/Principal";
import Nat        "mo:base/Nat";
import Nat32      "mo:base/Nat32";
import Nat64      "mo:base/Nat64";
import Nat8       "mo:base/Nat64";
import Text       "mo:base/Text";
import Iter       "mo:base/Iter";
import T          "./types";
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

import Ledger     "canister:ledger";


actor Tipping {
  
  type ArtistID                  = T.ArtistID;
  type FanID                     = T.FanID;
  type AdminID                   = T.AdminID;
  type AccountIdentifier         = T.AccountIdentifier;
  type ICPTs                     = T.ICPTs;
  type Ticker                    = T.Ticker;
  type Timestamp                 = T.Timestamp;
  type SubAccount                = Blob;

  
  private type FanToTippingData       = Map.HashMap<FanID, TippingData>;
  public type TippingData       = Map.HashMap<Timestamp, (Nat64, Ticker)>;

  let FEE : Nat64 = 10000;
  stable var txNo : Nat64 = 0;

  private stable var _tippingMap : [(ArtistID, (Timestamp, (FanID, Nat64, Ticker)))] = [];
  var tippingMap = Map.HashMap<ArtistID, FanToTippingData>(1, Principal.equal, Principal.hash); // Keep record of every tip transaction






  public func sendTip(fan: FanID, artist: ArtistID, amount: Nat64, ticker: Ticker) : async (){
    assert(amount > 0);
    let now = Time.now();

      switch(await transfer(fan, artist, amount)){
        case(#ok(res)){

          switch(tippingMap.get(artist)){
              case(?fanToTippingData){
                
                switch(fanToTippingData.get(fan)){
                    case(?tippingData){
                        tippingData.put(now, (amount, ticker));
                    };
                    case null {
                        var y : TippingData = Map.HashMap<Timestamp, (Nat64, Ticker)>(2, Int.equal, Int.hash);
                        y.put(now, (amount, ticker));
                        fanToTippingData.put(fan, y);
                    };
                };
                
              };
              case null {
                  var x : FanToTippingData = Map.HashMap<FanID, TippingData>(2, Principal.equal, Principal.hash);
                  var y : TippingData = Map.HashMap<Timestamp, (Nat64, Ticker)>(2, Int.equal, Int.hash);
                  y.put(now, (amount, ticker));
                  x.put(fan, y);
                  tippingMap.put(artist, x);
              };
          };
         
          Debug.print("Paid artist: " # debug_show artist # " in block " # debug_show res);
          
        }; case(#err(msg)){
          throw Error.reject("Unexpected error: " # debug_show msg);
        };
      };
  };

  public query func getTipDataFan(fan: FanID) : async  [(ArtistID, Timestamp, Nat64, Ticker)]{

    var data = Buffer.Buffer<(ArtistID, Timestamp, Nat64, Ticker)>(2);

    for(entries in tippingMap.entries()){
     var artist: ArtistID = entries.0;

      switch(tippingMap.get(artist)){
        case(?fanToTippingData){

          for(fans in fanToTippingData.entries()){
            var fanId: FanID = fans.0;
            if(fanId == fan){
                switch(fanToTippingData.get(fan)){
                    case(?tippingData){
                        for(tipData in tippingData.entries()){
                            var timestamp: Timestamp = tipData.0;
                            var amount: Nat64 = tipData.1.0;
                            var ticker: Ticker = tipData.1.1;
                            data.add(artist, timestamp, amount, ticker);
                        };
                    };
                    case null { }
                };
              }
          }
        };case null { };
      };
    };
  return Buffer.toArray(data);
  };

//   public shared query func getTipDataFan(fan: FanID) : async ?TippingData {


//     for(entries in tippingMap.entries()){
//      var artist: ArtistID = entries.0;
//      Debug.print("ArtistID's: " # debug_show artist);

//       switch(tippingMap.get(artist)){
//         case(?fanToTippingData){

//           for(fans in fanToTippingData.entries()){

//             var fanId: FanID = fans.0;

//             Debug.print("FanID's: " # debug_show fanId);
//             if(fanId == fan){
//               return fanToTippingData.get(fan);
//               }
//           }
//         };case null { };
//       };
//     };
//   return Buffer.toArray(data);
//   };

// public query func getTipDataArtist2(artist: ArtistID) : async  ?[(FanID, TippingData)]{

//       switch(tippingMap.get(artist)){
//         case(?fanToTippingData){
//             ?Iter.toArray(fanToTippingData.entries()); 
          
//         };case null null;
//       };
//   };



  public query func getTipDataArtist(artist: ArtistID) : async  [(FanID, Timestamp, Nat64, Ticker)]{

    var res = Buffer.Buffer<(FanID, Timestamp, Nat64, Ticker)>(2);

      switch(tippingMap.get(artist)){
        case(?fanToTippingData){
            for(fans in fanToTippingData.entries()){
                var fanId : FanID = fans.0;
                switch(fanToTippingData.get(fanId)){
                    case(?tippingData){
                        for(data in tippingData.entries()){
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
  public func accountIdentifierToBlob (accountIdentifier : AccountIdentifier) : async T.AccountIdentifierToBlobResult {
    U.accountIdentifierToBlob({
      accountIdentifier;
      canisterId = ?Principal.fromActor(Tipping);
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
    let canisterId = Principal.fromActor(Tipping);
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
    Account.accountIdentifier(Principal.fromActor(Tipping), Account.defaultSubaccount());
  };

//   func principalKey(s : Principal) : Trie.Key<Principal> {
//         { key = s; hash = Principal.hash(s) };
//   };

//   func textKey(s : Text) : Trie.Key<Text> {
//         { key = s; hash = Text.hash(s) };
//   };
// #endregion











// // #region - Upgrading state
//   system func preupgrade() {

//     _tippingMap := [];
//         for (tipping in tippingMap.entries()){
//             // entry1: (ArtistID, (FanID, Nat64))
//             let artistID : ArtistID = tipping.0;
//             let tippingInfo: TippingInfo = tipping.1;
//             for (info in tippingInfo.entries()){
//                 // offer : (Principal,(Price,Time.Time))
//                 let timestamp : Timestamp = info.0;
//                 let fanID : FanID = info.1.0;
//                 let amount : Nat64 = info.1.1;
//                 let ticker : Ticker = info.1.2;
  
//                 _tippingMap := Array.append(_tippingMap, [(artistID,(timestamp, (fanID, amount, ticker)))])
//             };
//         };
//   };


//   system func postupgrade() {
    
//     for (entry in _tippingMap.vals()){
//         // entry: (ArtistID, (FanID, Nat64));
//         let artistID : ArtistID = entry.0;
//         let timestamp: Timestamp = entry.1.0;
//         let fanID : FanID =  entry.1.1.0;
//         let amount : Nat64 = entry.1.1.1;
//         let ticker : Ticker = entry.1.1.2;
        
//         switch (tippingMap.get(artistID)){
//             case (?tipMap){
//                 // offer is a hashmap
//                 tipMap.put(timestamp, (fanID, amount, ticker));
//                 tippingMap.put(artistID, tipMap);
//             };
//             case (_){
//                 let tipMap: TippingInfo = Map.HashMap<Timestamp, (FanID, Nat64, Ticker)>(1, Int.equal, Int.hash);
//                 tipMap.put(timestamp, (fanID, amount, ticker));
//                 tippingMap.put(artistID, tipMap);
//             };
//         };
//     };
//   };
// #endregion
}











// // #region - HTTP OUTCALL 
//   public func getCryptoPrice(ticker: Text) : async Float {

//       let url = "https://api.coinbase.com/v2/prices/"# ticker #"-USD/spot";
//       // let url = "https://api.coinpaprika.com/v1/tickers/icp-internet-computer";
//       let transform_context : T.TransformContext = {
//         function = transform;
//         context = Blob.fromArray([]);
//       };

//       // Construct canister request
//       let request : T.CanisterHttpRequestArgs = {
//         url = url;
//         max_response_bytes = null;
//         headers = [];
//         body = null;
//         method = #get;
//         transform = ?transform_context;
//       };
//       Cycles.add(220_000_000_000);
//       let ic : T.IC = actor ("aaaaa-aa");
//       let response : T.CanisterHttpResponsePayload = await ic.http_request(request);

//       // Debug.print("Decoded res: " # debug_show Text.decodeUtf8(Blob.fromArray(response.body)));
//       let price = parse(response, "amount");

//       let priceFloat = await textToFloat(price);
//       priceFloat;
//   };

  

       
//   public query func transform(raw : T.TransformArgs) : async T.CanisterHttpResponsePayload {
//     let transformed : T.CanisterHttpResponsePayload = {
//       status = raw.response.status;
//       body = raw.response.body;
//       headers = [
//         {
//           name = "Content-Security-Policy";
//           value = "default-src 'self'";
//         },
//         { name = "Referrer-Policy"; value = "strict-origin" },
//         { name = "Permissions-Policy"; value = "geolocation=(self)" },
//         {
//           name = "Strict-Transport-Security";
//           value = "max-age=63072000";
//         },
//         { name = "X-Frame-Options"; value = "DENY" },
//         { name = "X-Content-Type-Options"; value = "nosniff" },
//       ];
//     };
//     transformed;
//   };
// // #endregion 




//   private func parse(result: T.CanisterHttpResponsePayload, k: Text): Text {
//       switch (Text.decodeUtf8(Blob.fromArray(result.body))) {
//           case null {};
//           case (?decoded) {
//               for(e:Text in Text.split(decoded, #text "{")){
//                  if(Text.contains(e, #text k)){
//                if(Text.contains(e, #text "{")){
                  
//                  return parseVal(e, k);
//                } else {
//                  for(i:Text in Text.split(e, #text ",")){
//                    if(Text.contains(i, #text k)){
//                      for(s:Text in Text.split(i, #text ":")){
//                        if(Text.contains(s, #text k) == false){
//                          var r:Text = Text.replace(s, #text "\"", "");
//                          r := Text.replace(r, #text "]", "");
//                          r := Text.replace(r, #text "}", "");
//                         //  Debug.print("Parse res: "# debug_show r);
//                          return r;
//                        };
//                      };
//                    };
//                  };
//                };
//             };
//           };
//         };
//       };
//       return "Not found";
//     };  


//   private func parseVal(t: Text, k: Text): Text {
//       for(e:Text in Text.split(t, #text "{")){
//         if(Text.contains(e, #text k)){
//           if(Text.contains(e, #text "{")){
//             return parseVal(e, k);
//           } else {
//             for(i:Text in Text.split(e, #text ",")){
//               if(Text.contains(i, #text k)){
//                 for(s:Text in Text.split(i, #text ":")){
//                   if(Text.contains(s, #text k) == false){
//                     var r:Text = Text.replace(s, #text "\"", "");
//                     r := Text.replace(r, #text "]", "");
//                     r := Text.replace(r, #text "}", "");
//                     //  Debug.print("ParseVal res: "# debug_show r);
//                     return r;
//                   };
//                 };
//               };
//             };
//           };
//         };
//       };
//       return "Not found";
//   };  


//   private func textToFloat(t : Text) : async Float {  
//       var i : Float = 1;
//       var f : Float = 0;
//       var isDecimal : Bool = false;  
//       for (c in t.chars()) {
//         if (Char.isDigit(c)) {
//           let charToNat : Nat64 = Nat64.fromNat(Nat32.toNat(Char.toNat32(c) -48));
//           let natToFloat : Float = Float.fromInt64(Int64.fromNat64(charToNat));
//           if (isDecimal) {
//             let n : Float = natToFloat / Float.pow(10, i);
//             f := f + n;
//           } else {
//             f := f * 10 + natToFloat;
//           };
//           i := i + 1;
//         } else {
//           if (Char.equal(c, '.') or Char.equal(c, ',')) {
//             f := f / Float.pow(10, i); // Force decimal
//             f := f * Float.pow(10, i); // Correction
//             isDecimal := true;
//             i := 1;
//           } else {
//             throw Error.reject("NaN");
//           };
//         };
//       };  
//       return f;
//   };









    // Debug.print("from subaccount: "# debug_show ?Account.principalToSubaccount(from));
    // Debug.print("from subaccount: "# debug_show ?Account.accountIdentifier(from, Account.defaultSubaccount()));
    // Debug.print("from subaccount: "# debug_show ?Account.accountIdentifier(Principal.fromActor(Payments), Account.principalToSubaccount(from)));
