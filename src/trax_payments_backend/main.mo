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
import Char "mo:base/Char";
import Int64 "mo:base/Int64";


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
  type Ticker                    = T.Ticker;
  type Timestamp                 = T.Timestamp;
  type SubPrice                  = T.SubPrice;
  
  public type SubAccount         = Blob;
  type Percentage                = T.Percentage;
  public type Participants       = T.Participants;
  private type ContentToAmount   = Map.HashMap<ContentID, Nat64>;
  private type TippingInfo       = Map.HashMap<FanID, Nat64>;
  private type DateToInfo        = Map.HashMap<Text, (Nat64, Float)>;
  //                                            FanID =>  subPriceUSD
  private type FanToSubPayment   = Map.HashMap<Principal, Float>;

                                             // FanID =>  timeOfNextSubPayment, subPriceUSD
  private type SubInfo   = Map.HashMap<FanID, (Timestamp, SubPrice)>;



  
  
  // var nextContentId : contentID = 0;
  let TRAX_ACCOUNT = "2l26f-kcxq2-2rwa7-zy36b-3wive-m3hfd-xrbr4-gocr4-7rklt-gmj4y-nqe";
  // user testing account: gmv5g-o74g2-2qqbh-mmjtk-rmegk-yjl3k-ptcpg-agawk-lxmx6-zvlml-7ae
  let FEE : Nat64 = 10000;

  //PPV
  private stable var _contentMap : [(ContentID, Content)] = [];
  private stable var _artistTotalContentMap : [(ArtistID, Nat64)] = [];
  private stable var _fanPaymentMap : [(FanID, (ContentID, Nat64))] = [];
  private stable var _artistTotalPerContentMap : [(ArtistID, (ContentID, Nat64))] = [];

  var contentMap = Map.HashMap<ContentID, Content>(1, Text.equal, Text.hash); // ContentID -> Content data: publisherID, publisher %age, participantsID, participants %age, price 
  var artistTotalContentMap = Map.HashMap<ArtistID, Nat64>(1, Principal.equal, Principal.hash); // total amount received from all content purchases
  var artistTotalPerContentMap = Map.HashMap<ArtistID, ContentToAmount>(1, Principal.equal, Principal.hash); // total amount received for each contentID 
  var fanPaymentMap = Map.HashMap<FanID, ContentToAmount>(1, Principal.equal, Principal.hash); // the amount a fan has paid for a piece of content.

  //TIPPING 
  private stable var _artistTotalMap : [(ArtistID, Nat64)] = [];
  private stable var _tippingMap : [(ArtistID, (FanID, Nat64))] = [];
  
  var tippingMap = Map.HashMap<ArtistID, TippingInfo>(1, Principal.equal, Principal.hash); // Keep record of every tip transaction
  var artistTotalTipsMap = Map.HashMap<ArtistID, Nat64>(1, Principal.equal, Principal.hash); // Total recieved from tips 


  //SUBSCRIPTIONS
  var subMap = Map.HashMap<ArtistID, SubInfo>(1, Principal.equal, Principal.hash); // Mapping keeping trax of artist a fan subscribes to and the timestamp of next subscription payment.
  var artistTotalSubRevenue = Map.HashMap<ArtistID, Nat64>(1, Principal.equal, Principal.hash); // total revenue earned through subs
  // var artistSubInfoMap = Map.HashMap<ArtistID, (Nat64, Nat64)>(1, Principal.equal, Principal.hash);// ArtistID to priceOfSub and total revenue earned.

  let nMonth = 2629800;
  let n = 60;
  let oneMin: Nat64    =        60_000_000_000;
  let twoMins: Nat64   =       120_000_000_000;
  let fiveMins: Nat64  =       300_000_000_000;
  let oneMonth: Nat64  = 2_629_800_000_000_000;
  var count = 0;


  system func heartbeat() : async () {
    if (count % n == 0) {
      Debug.print("HERE :) @ " # debug_show Nat64.fromIntWrap(Time.now()));
      await payArtistsSub();
     };
    Debug.print("count " # debug_show count);
    count += 1;
  };

  // system func timer(set : Nat64 -> ()) : async () {
  //   set(fromIntWrap(Time.now()) + 60_000_000_000); // 60 seconds from now
  //   doSomething();
  // };

  // let hour = 60 * 60_000_000_000;
  // let deadline = Time.now() + hour; // nanos

  // system func timer(set : Nat64 -> ()) : async () {
  //   let time = Time.now();
  //   let toGo = deadline - time;
  //   if (toGo < 0) { set 0; return };

  //   debug { Debug.print("Still to go: " # debug_show toGo) };

  //   set(Nat64.fromIntWrap(time + toGo / 2));
  // };


  public func payArtistsSub() : async (){

    // let priceICP: Float = await getCryptoPrice("ICP");
    let priceICP: Float = 5.004;
    // Debug.print("@payArtistsSub ICP Price: "#debug_show priceICP);

    for(ids in subMap.entries()){
          Debug.print(debug_show ids.0);
          let artistID : ArtistID = ids.0;
          let subInfo: SubInfo =  ids.1;
            for (info in subInfo.entries()){
              Debug.print(debug_show (info.0, info.1.0, info.1.1));

                  let fanID : FanID = info.0;
                  let timestamp : Timestamp = info.1.0;
                  let priceOfSub : SubPrice = info.1.1;
                  let amount : Nat64 = await platformDeduction(fanID, Nat64.fromIntWrap(Float.toInt((priceOfSub / priceICP) * 100000000)));

          if(Nat64.fromNat(Int.abs(Time.now())) > timestamp){
            Debug.print("Next subscription payment initiated");
            switch(await transfer(fanID, artistID, amount)){
              case(#ok(res)){
                switch(subMap.get(artistID)){
                  case(?innerMap){
                    switch(innerMap.get(fanID)){   
                      case(?currVals){
                        Debug.print("current timestamp " # debug_show timestamp # "next timestamp: " # debug_show (timestamp + oneMin) );
                        var update = innerMap.replace(fanID, ((timestamp + oneMin), priceOfSub));
                        Debug.print(debug_show innerMap.get(fanID));
                      }; 
                      case null{};
                    };
                  }; 
                  case null {};
                };
                switch(artistTotalSubRevenue.get(artistID)){
                  case(?currVal){   var update = artistTotalSubRevenue.replace(artistID, (currVal + amount));   };
                  case null {   artistTotalSubRevenue.put(artistID, amount);    };
                };
              }; 
              case(#err(msg)){ 
                let success = await unsubscribe(artistID, fanID);
                assert(success);
                throw Error.reject("Your subscription has been terminated due to: " # debug_show msg);
                return
              };
            };
          };
        };
        };
  };


  public func subscribe(artist: ArtistID, fan: FanID, priceOfSub: Float): async Bool{
    let priceICP = await getCryptoPrice("ICP");
    Debug.print("price ICP: "# debug_show priceICP);
    var amountICP = Nat64.fromIntWrap(Float.toInt((priceOfSub / priceICP) * 100000000));

    switch(await transfer(fan, artist, amountICP)){
      case(#ok(res)){
        await addToSubMap(artist, fan, priceOfSub);
        true;
        }; case(#err(msg)){
          throw Error.reject("Unexpected error: " # debug_show msg);
          false;
        };
      };
  };


  private func addToSubMap(artist: ArtistID, fan: FanID, priceOfSub: Float) : async (){

    switch(subMap.get(artist)){
      case(?innerMap){
        Debug.print("Initial payment 2 initiated, next payment is @" # debug_show (Nat64.fromIntWrap(Time.now()) + oneMin));
        innerMap.put(fan, (Nat64.fromIntWrap(Time.now()) + oneMin, priceOfSub));

      }; case null {
        Debug.print("Initial payment initiated, next payment is @" # debug_show (Nat64.fromNat(Int.abs(Time.now())) + oneMin));
        var x : SubInfo = Map.HashMap<FanID, (Nat64, Float)>(2, Principal.equal, Principal.hash);
        x.put(fan, (Nat64.fromIntWrap(Time.now()) + oneMin, priceOfSub));
        subMap.put(artist, x);

      };
    };

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
    }
  };

  public func unsubscribe(artist: ArtistID, fan: FanID) : async Bool{
    switch(subMap.get(artist)){
      case(?innerMap){
        innerMap.delete(fan);
        true
      }; case null false;
    };
  };

  // public func changeSubPrice
  // 

  public func changeSubPrice(artist: ArtistID, newPrice: Float) :  async Bool{
    switch(subMap.get(artist)){
      case(?innerMap){
        for(info in innerMap.entries()){
          let fanID : FanID = info.0;
          let timestamp : Nat64 = info.1.0;
          let update = innerMap.replace(fanID, (timestamp , newPrice));
        };
        return true;
      }; 
      case null false;
    };
  };

  public func getArtistTotalSubRevenue(artist: ArtistID) : async ?Nat64{    artistTotalSubRevenue.get(artist)   };

  public func getTotalNumberOfSubscribers(artist: ArtistID) : async Nat32{
    switch(subMap.get(artist)){
      case(?innerMap){
        var numOfFans: Nat32 = 0;
        for(fans in innerMap.entries()){
          numOfFans := numOfFans + 1;
        };
        return numOfFans;
      }; case null {
        return 0;
      }
    }
  };


















// #region - HTTP OUTCALL 
  public func getCryptoPrice(ticker: Text) : async Float {

      let url = "https://api.coinbase.com/v2/prices/"# ticker #"-USD/spot";
      // let url = "https://api.coinpaprika.com/v1/tickers/icp-internet-computer";
      let transform_context : T.TransformContext = {
        function = transform;
        context = Blob.fromArray([]);
      };

      // Construct canister request
      let request : T.CanisterHttpRequestArgs = {
        url = url;
        max_response_bytes = null;
        headers = [];
        body = null;
        method = #get;
        transform = ?transform_context;
      };
      Cycles.add(220_000_000_000);
      let ic : T.IC = actor ("aaaaa-aa");
      let response : T.CanisterHttpResponsePayload = await ic.http_request(request);

      // Debug.print("Decoded res: " # debug_show Text.decodeUtf8(Blob.fromArray(response.body)));
      let price = parse(response, "amount");

      let priceFloat = await textToFloat(price);
      priceFloat;
  };


  private func parse(result: T.CanisterHttpResponsePayload, k: Text): Text {
      switch (Text.decodeUtf8(Blob.fromArray(result.body))) {
          case null {};
          case (?decoded) {
              for(e:Text in Text.split(decoded, #text "{")){
                 if(Text.contains(e, #text k)){
               if(Text.contains(e, #text "{")){
                  
                 return parseVal(e, k);
               } else {
                 for(i:Text in Text.split(e, #text ",")){
                   if(Text.contains(i, #text k)){
                     for(s:Text in Text.split(i, #text ":")){
                       if(Text.contains(s, #text k) == false){
                         var r:Text = Text.replace(s, #text "\"", "");
                         r := Text.replace(r, #text "]", "");
                         r := Text.replace(r, #text "}", "");
                         Debug.print("Parse res: "# debug_show r);
                         return r;
                       };
                     };
                   };
                 };
               };
            };
          };
        };
      };
      return "Not found";
    };  


  private func parseVal(t: Text, k: Text): Text {
      for(e:Text in Text.split(t, #text "{")){
        if(Text.contains(e, #text k)){
      if(Text.contains(e, #text "{")){
        return parseVal(e, k);
      } else {
        for(i:Text in Text.split(e, #text ",")){
          if(Text.contains(i, #text k)){
            for(s:Text in Text.split(i, #text ":")){
              if(Text.contains(s, #text k) == false){
                var r:Text = Text.replace(s, #text "\"", "");
                r := Text.replace(r, #text "]", "");
                r := Text.replace(r, #text "}", "");
                 Debug.print("ParseVal res: "# debug_show r);
                return r;
              };
            };
          };
        };
      };
        };
      };
      return "Not found";
  };  


  private func textToFloat(t : Text) : async Float {  
      var i : Float = 1;
      var f : Float = 0;
      var isDecimal : Bool = false;  
      for (c in t.chars()) {
        if (Char.isDigit(c)) {
          let charToNat : Nat64 = Nat64.fromNat(Nat32.toNat(Char.toNat32(c) -48));
          let natToFloat : Float = Float.fromInt64(Int64.fromNat64(charToNat));
          if (isDecimal) {
            let n : Float = natToFloat / Float.pow(10, i);
            f := f + n;
          } else {
            f := f * 10 + natToFloat;
          };
          i := i + 1;
        } else {
          if (Char.equal(c, '.') or Char.equal(c, ',')) {
            f := f / Float.pow(10, i); // Force decimal
            f := f * Float.pow(10, i); // Correction
            isDecimal := true;
            i := 1;
          } else {
            throw Error.reject("NaN");
          };
        };
      };  
      return f;
  };

       
  public query func transform(raw : T.TransformArgs) : async T.CanisterHttpResponsePayload {
    let transformed : T.CanisterHttpResponsePayload = {
      status = raw.response.status;
      body = raw.response.body;
      headers = [
        {
          name = "Content-Security-Policy";
          value = "default-src 'self'";
        },
        { name = "Referrer-Policy"; value = "strict-origin" },
        { name = "Permissions-Policy"; value = "geolocation=(self)" },
        {
          name = "Strict-Transport-Security";
          value = "max-age=63072000";
        },
        { name = "X-Frame-Options"; value = "DENY" },
        { name = "X-Content-Type-Options"; value = "nosniff" },
      ];
    };
    transformed;
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
        artistTotalContentMap.replace(artist, (currVal + amount))
      };case null null;
    };
  };

  private func updateArtistTotalPerContentMap(artist: ArtistID, id: ContentID, amount: Nat64): async (?Nat64){
    switch(artistTotalPerContentMap.get(artist)){
      case(?innerMap){
        switch(innerMap.get(id)){
          case(?currVal){
            innerMap.replace(id, (currVal + amount));
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
              nestedMap.replace(fan, (currVal + amount))
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
        artistTotalTipsMap.replace(artist, (currVal + amount))

      };case null ?Nat64.fromNat(0);
    };
  };         

// #endregion





// #region - Transfer  





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

            return #err("Insufficient balance of " # debug_show balance # " from account:" # debug_show from # "")
            
          };
          case (#Err(other)) {
            return #err("Unexpected error: " # debug_show other);
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

    
  };
// #endregion
}





//  var tokensMap = Map.HashMap<Principal, DateToInfo>(1, Principal.equal, Principal.hash);
//  private stable var _tokensMap : [(Principal, (Text, (Nat64, Float)))] = [];

//  var userTraxWallet = Map.HashMap<Principal, Nat64>(1, Principal.equal, Principal.hash);

// public func getAllTokenWalletTransfers(user: Principal) : async ?[(Text, (Nat64, Float))]{
//   switch(tokensMap.get(user)){
//     case(?innerMap){
//        return ?Iter.toArray<(Text, (Nat64, Float))>(innerMap.entries());

//     }; case null return null;
//   };
// };
// public func getUserWalletBalance(from: Principal) : async ?(Nat64) {    userTraxWallet.get(from);   };

// public func topUpTokenWallet(from: Principal, amount: Nat64, priceUSD: Float) : async (Text, Float){
 
//     let now : Text = Int.toText(Time.now());
//     var tokens: Float = 0;
//     var to : Principal = Principal.fromText(TRAX_ACCOUNT);

//     switch(await transfer(from, to, amount)){
//       case(#ok(res)){
//         // await E.get_rates((Time.now()-100),Time.now());
//         tokens := (Float.fromInt(Nat64.toNat(amount)) / 100000000) * priceUSD;
//         Debug.print("tokens: " # debug_show tokens);
//         // let tokensFormat = Nat32.fromNat(Int.abs(Float.toInt(tokens)));

//         switch(userTraxWallet.get(from)){
//           case(?currVal){
//             let newAmount = currVal + amount;
//             let update = userTraxWallet.replace(from, newAmount);
//           }; case null {
//             userTraxWallet.put(from, amount);
//           };
//         };

//         switch(tokensMap.get(from)){
//           case(?innerMap){
//                 innerMap.put(now,(amount, tokens));
//                 return (now, tokens);
//           };case null{
//             // func (a : Nat32) : Nat32 {a;}
//                 var x = Map.HashMap<Text, (Nat64, Float)>(1, Text.equal, Text.hash);
//                 x.put(now,(amount, tokens));
//                 tokensMap.put(from, x);
//                 return (now, tokens);
//           };
//         };
         

//       }; case(#err(msg)){
//         throw Error.reject("Unexpected error: " # debug_show msg);
//       };
//     };
// };



// _tokensMap := []; //preupgrade
//         for (tokensInfo in tokensMap.entries()){
//             // entry1: (FanID, FanPaymentInfo)
//             let user : Principal = tokensInfo.0;
//             let dateToInfo: DateToInfo = tokensInfo.1;
//             for (info in dateToInfo.entries()){
//                 // offer : (ContentID, Nat64)
//                 let timestamp : Text = info.0;
//                 let amount : Nat64 = info.1.0;
//                 let tokens : Float = info.1.1;
  
//                 _tokensMap := Array.append(_tokensMap, [(user,(timestamp,(amount, tokens)))]);
//             };
//         };

//     _tokensMap := [];
//     for (entry in _tokensMap.vals()){ //postupgrade
//         // entry: (ArtistID, (ContentID, Nat64))
//         let user : Principal = entry.0;
//         let timestamp : Text =  entry.1.0;
//         let amount : Nat64 = entry.1.1.0;
//         let tokens : Float = entry.1.1.1;
        
//         switch (tokensMap.get(user)){
//             case (?innerMap){
//                 // offer is a hashmap
//                 innerMap.put(timestamp,(amount, tokens));
//                 tokensMap.put(user, innerMap);
//             };
//             case (_){
//                 let dateToInfo: DateToInfo = Map.HashMap<Text, (Nat64, Float)>(1, Text.equal, Text.hash);
//                 dateToInfo.put(timestamp,(amount, tokens));
//                 tokensMap.put(user, dateToInfo);
//             };
//         };
//     };










// public shared func ring() : async () {
//     let from: Text = "gmv5g-o74g2-2qqbh-mmjtk-rmegk-yjl3k-ptcpg-agawk-lxmx6-zvlml-7ae";
//     let to : Text = "3hldd-sjv4d-2qdqs-2uj7z-rkmiu-hor4s-tfu7b-d3cgp-vj5i6-jq4lu-5qe";
//     let amount : Nat64 = 1000000000;

//     var hello = await transfer(Principal.fromText(from), Principal.fromText(to), amount);

//     Debug.print("Ring!");
//     Debug.print(debug_show Time.now());
//     Debug.print(debug_show Nat64.fromNat(Int.abs(Time.now())));
//     // 1673872629764597000
//     // 1673872698
//     //    2629800000000000
//   };


















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