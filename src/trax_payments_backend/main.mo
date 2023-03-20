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
import Timer "mo:base/Timer";


// TODO
// * Add functionality for PPV content (royalty sharing) to be able change participants and revenue share after initial posting
// * Adding tiers to subscriptions? (feature)
// * Ability to pause subscriptions (feature)

// * onlyOwner checks throughout contract 
// * add in msg.caller param to suitable 

// * Add mag.caller param to payment functions.
// * 


// import E "../exchange_rate/main";

actor Payments {
  // minter2 phrase: hundorp
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
  type Participants              = T.Participants;
  private type ContentToAmount   = Map.HashMap<ContentID, Nat64>;
  private type FanToTime         = Map.HashMap<FanID, (Timestamp, Nat64)>;
  
  private type SubInfo   = Map.HashMap<FanID, (Timestamp, SubPrice, SubType)>;

  stable var TRAX_ACCOUNT: Text = "2l26f-kcxq2-2rwa7-zy36b-3wive-m3hfd-xrbr4-gocr4-7rklt-gmj4y-nqe";
  let FEE : Nat64 = 10000;
  stable var txNo : Nat64 = 0;


  //SUBSCRIPTIONS
  private stable var _subMap : [(ArtistID, (FanID, (Timestamp, SubPrice, SubType)))] = [];
  private stable var _artistTotalSubRevenue : [(ArtistID, Nat64)] = [];

  var subMap = Map.HashMap<ArtistID, SubInfo>(1, Principal.equal, Principal.hash); // Mapping keeping trax of artist a fan subscribes to and the timestamp of next subscription payment.
  var artistTotalSubRevenue = Map.HashMap<ArtistID, Nat64>(1, Principal.equal, Principal.hash); // total revenue earned through subs


  //PPV
  private stable var _contentMap : [(ContentID, Content)] = [];
  private stable var _contentPaymentMap : [(ContentID, (FanID, (Timestamp, Nat64)))] = [];

  var contentMap = Map.HashMap<ContentID, Content>(1, Text.equal, Text.hash); // ContentID -> Content data: publisherID, publisher %age, participantsID, participants %age, price. 
  var contentPaymentMap = Map.HashMap<ContentID, FanToTime>(1, Text.equal, Text.hash); // true false conditional which verifies whether a fan has paid.


  //TIPPING 
  private type TippingInfo       = Map.HashMap<Timestamp, (FanID, Nat64)>;

  private stable var _tippingMap : [(ArtistID, (Timestamp, (FanID, Nat64)))] = [];

  var tippingMap = Map.HashMap<ArtistID, TippingInfo>(1, Principal.equal, Principal.hash); // Keep record of every tip transaction



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



  private func payArtistsSub() : async (){

    let priceICP: Float = await getCryptoPrice("ICP");
    var count = 0;
    // let priceICP: Float = 5;
    // Debug.print("@payArtistsSub ICP Price: "#debug_show priceICP);

    for(ids in subMap.entries()){
          // Debug.print(debug_show ids.0);
          let artistID : ArtistID = ids.0;
          let subInfo: SubInfo =  ids.1;
            for (info in subInfo.entries()){
              count := count +1;
              
              // Debug.print(debug_show (info.0, info.1.0, info.1.1));

              let fanID : FanID = info.0;
              let timestamp : Timestamp = info.1.0;
              let priceOfSub : SubPrice = info.1.1;
              let period : SubType = info.1.2;
              

          if(Time.now() > timestamp){

            Debug.print("period: " # debug_show period);
            var nextPayment : Int = 0;
            let amount : Nat64 = await platformDeduction(fanID, Nat64.fromIntWrap(Float.toInt((priceOfSub / priceICP) * 100000000)));
            // let amount : Nat64 = await getRemainingAfterDeduction(Nat64.fromIntWrap(Float.toInt((priceOfSub / priceICP) * 100000000)), 0.10);
            Debug.print("amount after deduction: " # debug_show amount);

            switch(await transfer(fanID, artistID, amount)){
              case(#ok(res)){
                switch(subMap.get(artistID)){
                  case(?innerMap){
                    switch(innerMap.get(fanID)){   
                      case(?currVals){
                        if (period == #monthly){    nextPayment := twoMins;    };
                        if (period == #yearly) {    nextPayment := fourMins;   };
                        // Debug.print("current timestamp " # debug_show timestamp # "next timestamp: " # debug_show (timestamp + oneMin) );
                        var update = innerMap.replace(fanID, ((timestamp + nextPayment), priceOfSub, period));
                        // Debug.print(debug_show innerMap.get(fanID));
                      };
                      case null {   Debug.print("Couldnt find or access FanID in subMap");    };
                    };
                  };
                  case null {   Debug.print("Couldnt find or access ArtistID in subMap");    };
                };

                //update state for artist sub revenue 
                switch(artistTotalSubRevenue.get(artistID)){
                  case(?currVal){   var update = artistTotalSubRevenue.replace(artistID, (currVal + amount));   };
                  case null {   artistTotalSubRevenue.put(artistID, amount);    };
                };
              }; 
              case(#err(msg)){  
                let success = await unsubscribe(artistID, fanID);
                // assert(success);
                throw Error.reject("Your subscription has been terminated due to: " # debug_show msg);
                return
              };
            };
          };
        };
      };
      Debug.print("Number of entries: " # debug_show count);
  };



  public func subscribe(artist: ArtistID, fan: FanID, priceOfSub: Float, ticker: Ticker, period: SubType, freeTrial: Bool, freeDays: Int): async Bool{
    
    let fanSubscribed = await isFanSubscribed(artist, fan);
    assert(fanSubscribed == false);
    let priceICP = await getCryptoPrice(ticker);
    Debug.print("price ICP: "# debug_show priceICP);

    Debug.print("SubType" # debug_show period);
    

    if(freeTrial){
      await addToSubMap(artist, fan, priceOfSub, period, freeTrial, freeDays);
      true;
    }else{
      var amountICP = await platformDeduction(fan, Nat64.fromIntWrap(Float.toInt((priceOfSub / priceICP) * 100000000)));
      // var amountICP = await getRemainingAfterDeduction(Nat64.fromIntWrap(Float.toInt((priceOfSub / priceICP) * 100000000)), 0.10);
      switch(await transfer(fan, artist, amountICP)){
          case(#ok(res)){
            await addToSubMap(artist, fan, priceOfSub, period, freeTrial, freeDays);
            true;
            }; case(#err(msg)){
              throw Error.reject("Unexpected error: " # debug_show msg);
              false;
            };
      };
    };
  };



  private func addToSubMap(artist: ArtistID, fan: FanID, priceOfSub: Float, period: SubType, freeTrial: Bool, freeDays: Int) : async (){
    let timeNow = Time.now();
    var nextPayment: Int = 0;
    if (period == #monthly){    nextPayment := twoMins;    };
    if (period == #yearly) {    nextPayment := fourMins;   };

    switch(subMap.get(artist)){
      case(?innerMap){
        
        if (freeTrial){
          innerMap.put(fan, (timeNow + (oneDay * freeDays), priceOfSub, period));
          Debug.print("Fan subscribed with free trial, first payment is at: " # debug_show (timeNow + (oneDay * freeDays)));
        } 
        else {
          innerMap.put(fan, (timeNow + nextPayment, priceOfSub, period));
          Debug.print("Fan subscribed, next payment is at: " # debug_show (timeNow + nextPayment));
        };
        // Debug.print("Fan subscribed, next payment is at: " # debug_show (Nat64.fromIntWrap(Time.now()) + nextPayment));

      }; case null {
        var x : SubInfo = Map.HashMap<FanID, (Timestamp, Float, SubType)>(2, Principal.equal, Principal.hash);

        if(freeTrial){    
          x.put(fan, (timeNow + (oneDay * freeDays), priceOfSub, period));
          Debug.print("Fan subscribed to new artist with free trial, first payment is at: " # debug_show (timeNow + nextPayment));
        }else{
           x.put(fan, (timeNow + nextPayment, priceOfSub, period));
           Debug.print("Fan subscribed to new artist, next payment is at:" # debug_show (timeNow + nextPayment));
        };

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



  public func getArtistTotalSubRevenue(artist: ArtistID) : async ?Nat64{    artistTotalSubRevenue.get(artist)   };



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











// #region - PAY-PER-VIEW 
  public func purchaseContent(id: ContentID, fan: Principal, ticker: Text) : async (){
      let hasPaid = await fanHasPaid(id, fan);
      assert(hasPaid == false);
      
      var publisherID : ?ArtistID = null;
      var publisherPercentage : Percentage = 0;
      var participants: [Participants] = [];
      let now = Time.now();
      let priceCrypto = await getCryptoPrice(ticker);
      var amountToSend : Nat64 = 0;


      switch(contentMap.get(id)){
        case(?content){
          Debug.print("Price of content: " # debug_show content.price);
          assert(content.price > 0);
          let amount: Nat64 =  Nat64.fromIntWrap(Float.toInt((content.price / priceCrypto) * 100000000));
          amountToSend := await platformDeduction(fan, amount);
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
                          
                          innerMap.put(fan, (now, amountToSend));
                          
                        }; case null {
                          var x = Map.HashMap<FanID, (Timestamp, Nat64)>(2, Principal.equal, Principal.hash);
                          x.put(fan, (now, amountToSend));
                          contentPaymentMap.put(id, x);
                        }
                      };
                      Debug.print("Paid artist: " # debug_show artist # " in block " # debug_show res);
                    }; case(#err(msg)){   throw Error.reject("Unexpected error: " # debug_show msg);    };
                  };
          }; case null { };
        };

  };



  public func addPPVContent(id: ContentID, content : Content): async () {   
    switch(contentMap.get(id)){
      case(?exists){
        throw Error.reject("This content ID has been taken");
      }; case null {
        contentMap.put(id, content);    
      }
    };
  };



  public func updatePPVContent (id: ContentID, content: Content) :  async (){
    switch(contentMap.get(id)){
      case(?exists){
        // contentMap.delete(id);
        // await addPPVContent(id, content);
        let update = contentMap.replace(id, content);

      }; case null{

      };
    };
  };

  public func removeContent(id: ContentID): async () {    contentMap.delete(id);    };


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
    var ids : [ContentID] = [];
    for(entries in contentMap.entries()){
      if(entries.1.publisher == artist){
        var id = entries.0;
        ids := Array.append(ids, [id]);
      } else {
        for(i in Iter.fromArray(entries.1.participants)){
          if(artist == i.participantID){
            var id = entries.0;
            ids := Array.append(ids, [id]);
          }
        }
      }
    };
    return ids;
  };


  // public func getAllData(artist: ArtistID) : async Nat64{  
  //   let contentIds =  await getAllArtistContentIDs(artist);

  //   for(entries in contentPaymentMap.entries()){

  //     for(eachId in Iter.fromArray(contentIds)){

  //       if(entries.0 == eachId){

  //       }
  //     }
  //   }

  // };

  public func getAllArtistContentPayments(artist: ArtistID) : async [(FanID, (Timestamp, Nat64))]{  
    let contentIds =  await getAllArtistContentIDs(artist);
    var res: [(FanID, (Timestamp, Nat64))] = [];

    for(entries in contentPaymentMap.entries()){

      for(eachId in Iter.fromArray(contentIds)){

        if(entries.0 == eachId){
          switch(contentPaymentMap.get(entries.0)){
            case(?innerMap){
              for(i in innerMap.entries()){
                var fanId: FanID = i.0;
                var timestamp: Timestamp = i.1.0;
                var amount: Nat64 = i.1.1;
                res := Array.append(res, [(fanId, (timestamp, amount))])
              }
            }; case null { }
          };
          
        };
      };
    };
    return res;

  };

  public func getAllFanContentPayments(fan: FanID) : async [(FanID, (Timestamp, Nat64))]{ 
    var res: [(FanID, (Timestamp, Nat64))] = [];

    for(entries in contentPaymentMap.entries()){ 
      switch(contentPaymentMap.get(entries.0)){
        case(?innerMap){
          for(i in innerMap.entries()){
            if(i.0 == fan){
              var fanId: FanID = i.0;
                var timestamp: Timestamp = i.1.0;
                var amount: Nat64 = i.1.1;
                res := Array.append(res, [(fanId, (timestamp, amount))])
            }
          }
        }; case null {};
      }
    };
    return res;
  };

  // public query func getArtistTotalPerContentMap(artist: ArtistID, contentID: ContentID) : async ?Nat64{
      
  // };



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











// #region -TIPPING  
  public func sendTip(fan: FanID, artist: ArtistID, amount: Nat64) : async (){
    let now = Time.now();
      switch(await transfer(fan, artist, amount)){
        case(#ok(res)){
          switch(tippingMap.get(fan)){
              case(?innerMap){
                innerMap.put(now, (fan, amount));

              };
              case null {
                  var x : TippingInfo = Map.HashMap<Timestamp, (FanID, Nat64)>(2, Int.equal, Int.hash);
                  x.put(now, (fan, amount));
                  tippingMap.put(artist, x);
              };
          };
         
          Debug.print("Paid artist: " # debug_show artist # " in block " # debug_show res);
          
        }; case(#err(msg)){
          throw Error.reject("Unexpected error: " # debug_show msg);
        };
      };
  };


  public query func getTotalTipsFromFan(artist: ArtistID, fan: FanID) : async Nat64 {
    var total: Nat64 = 0;
          switch(tippingMap.get(artist)){
            case(?nestedMap){
              for(entries in nestedMap.entries()){
                if(entries.1.0 == fan){
                  total += entries.1.1;
                };
              };
              return total;
            };
            case null{
              return Nat64.fromNat(0);
            }
          };
  };

  public query func getTotalTips(artist: ArtistID) : async Nat64 {
    var total: Nat64 = 0;
    switch(tippingMap.get(artist)){
      case(?nestedMap){
        for(entries in nestedMap.entries()){
            total += entries.1.1;
        };
        return total;
      };
      case null{
        return Nat64.fromNat(0);
      }
    };
  };

  public query func getTipDataFan(fan: FanID) : async  [(ArtistID, (Timestamp, (FanID, Nat64)))]{
    var data: [(ArtistID, (Timestamp, (FanID, Nat64)))] = [];

    for(entries in tippingMap.entries()){
     var artist: ArtistID = entries.0;

      switch(tippingMap.get(artist)){
        case(?innerMap){

          for(i in innerMap.entries()){
            if(i.1.0 == fan){
              var timestamp: Timestamp = i.0;
              var amount: Nat64 = i.1.1;
              var fanId: FanID = i.1.0;
              data := Array.append(data, [(artist, (timestamp, (fanId, amount)))]);
                // data :=  Array.append((artist, (timestamp, (fanId, amount))));
              }
          }
        };case null { };
      };
    };
  return data;
  };

  public query func getTipDataArtist(artist: ArtistID) : async  ?[(Timestamp, (FanID, Nat64))]{
      switch(tippingMap.get(artist)){
        case(?innerMap){
          ?Iter.toArray(innerMap.entries()); 
        };case null null;
      };
  };
  

// #endregion











// #region - SHARED FUNCTIONS  

  private func platformDeduction(fan: FanID, amount : Nat64) : async Nat64 {
    let traxAccount: Principal = Principal.fromText(TRAX_ACCOUNT);
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
                        //  Debug.print("Parse res: "# debug_show r);
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
                    //  Debug.print("ParseVal res: "# debug_show r);
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
    _artistTotalSubRevenue := Iter.toArray(artistTotalSubRevenue.entries());
    // _tokensMap := Iter.toArray(tokensMap.entries());

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
  
                _subMap := Array.append(_subMap, [(artistID, (fanID,(timestamp, subPrice, subType)))]);
            };
        };

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
  
                _contentPaymentMap := Array.append(_contentPaymentMap, [(contentID,(fanID, (timestamp, amount)))])
            };
        };
    
    _tippingMap := [];
        for (tipping in tippingMap.entries()){
            // entry1: (ArtistID, (FanID, Nat64))
            let artistID : ArtistID = tipping.0;
            let tippingInfo: TippingInfo = tipping.1;
            for (info in tippingInfo.entries()){
                // offer : (Principal,(Price,Time.Time))
                let timestamp : Timestamp = info.0;
                let fanID : FanID = info.1.0;
                let amount : Nat64 = info.1.1;
  
                _tippingMap := Array.append(_tippingMap, [(artistID,(timestamp, (fanID, amount)))])
            };
        };
    
    // _artistTotalPerContentMap := [];
    //   for (perContent in artistTotalPerContentMap.entries()){
    //         // entry1: (ArtistID, (ContentID, Nat64))
    //         let artistID : ArtistID = perContent.0;
    //         let contentInfo: ContentToAmount = perContent.1;
    //         for (content in contentInfo.entries()){
    //             // offer : (Principal,(Price,Time.Time))
    //             let contentID : ContentID = content.0;
    //             let amount : Nat64 = content.1;
  
    //             _artistTotalPerContentMap := Array.append(_artistTotalPerContentMap, [(artistID,(contentID, amount))])
    //         };
    //     };
  };


  system func postupgrade() {
    contentMap := Map.fromIter<ContentID, Content>(_contentMap.vals(), 10, Text.equal, Text.hash);
    _contentMap := [];

    artistTotalSubRevenue := Map.fromIter<ArtistID, Nat64>(_artistTotalSubRevenue.vals(), 10, Principal.equal, Principal.hash);
    _artistTotalSubRevenue := [];
    

    for (entry in _contentPaymentMap.vals()){
        // entry: (FanID, (ContentID, Nat64))
        let fanID : FanID = entry.1.0;
        let id : ContentID =  entry.0;
        let timestamp : Timestamp = entry.1.1.0;
        let amount : Nat64 = entry.1.1.1;
        
        switch (contentPaymentMap.get(id)){
            case (?contentPayment){
                // offer is a hashmap
                contentPayment.put(fanID, (timestamp, amount));
                contentPaymentMap.put(id, contentPayment);
            };
            case (_){
                let contentPayment: FanToTime = Map.HashMap<FanID, (Timestamp, Nat64)>(1, Principal.equal, Principal.hash);
                contentPayment.put(fanID, (timestamp, amount));
                contentPaymentMap.put(id, contentPayment);
            };
        };
    };
    
    for (entry in _tippingMap.vals()){
        // entry: (ArtistID, (FanID, Nat64));
        let artistID : ArtistID = entry.0;
        let timestamp: Timestamp = entry.1.0;
        let fanID : FanID =  entry.1.1.0;
        let amount : Nat64 = entry.1.1.1;
        
        switch (tippingMap.get(artistID)){
            case (?tipMap){
                // offer is a hashmap
                tipMap.put(timestamp, (fanID, amount));
                tippingMap.put(artistID, tipMap);
            };
            case (_){
                let tipMap: TippingInfo = Map.HashMap<Timestamp, (FanID, Nat64)>(1, Int.equal, Int.hash);
                tipMap.put(timestamp, (fanID, amount));
                tippingMap.put(artistID, tipMap);
            };
        };
    };

    // for (entry in _artistTotalPerContentMap.vals()){
    //     // entry: (ArtistID, (ContentID, Nat64))
    //     let artistID : ArtistID = entry.0;
    //     let contentID : ContentID =  entry.1.0;
    //     let amount : Nat64 = entry.1.1;
        
    //     switch (artistTotalPerContentMap.get(artistID)){
    //         case (?artistTotal){
    //             // offer is a hashmap
    //             artistTotal.put(contentID, amount);
    //             artistTotalPerContentMap.put(artistID, artistTotal);
    //         };
    //         case (_){
    //             let artistTotal: ContentToAmount = Map.HashMap<ContentID, Nat64>(1, Text.equal, Text.hash);
    //             artistTotal.put(contentID, amount);
    //             artistTotalPerContentMap.put(artistID, artistTotal);
    //         };
    //     };
    // };

    for (entry in _subMap.vals()){
        let artistID : ArtistID = entry.0;
        let fanID: FanID = entry.1.0;
        let timestamp : Timestamp = entry.1.1.0;
        let subPrice : SubPrice = entry.1.1.1;
        let subType : SubType = entry.1.1.2;

        
        switch (subMap.get(artistID)){
            case (?subInfo){

                subInfo.put(fanID,(timestamp, subPrice, subType));
                subMap.put(artistID, subInfo);
            };
            case (_){
                let subInfo: SubInfo = Map.HashMap<FanID, (Timestamp, SubPrice, SubType)>(1, Principal.equal, Principal.hash);
                subInfo.put(fanID,(timestamp, subPrice, subType));
                subMap.put(artistID, subInfo);
            };
        };
    };

    
  };
// #endregion
}



























// let timer1 = Timer.setTimer( 365 * 24 * 60* 60 *1000000000, sayHappyAnniversary);

  // let hour = 60 * 60_000_000_000;
  // let deadline = Time.now() + hour; // nanos

  // system func timer(set : Nat64 -> ()) : async () {
  //   let time = Time.now();
  //   let toGo = deadline - time;
  //   if (toGo < 0) { set 0; return };

  //   debug { Debug.print("Still to go: " # debug_show toGo) };

  //   set(Nat64.fromIntWrap(time + toGo / 2));
  // };


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





  // system func heartbeat() : async () {
  //   if (count % n == 0) {
  //     // Debug.print("HERE :) @ " # debug_show Nat64.fromIntWrap(Time.now()));
  //     await payArtistsSub();
  //    };
    
  //   count += 1;
  //   Debug.print("count " # debug_show count);
  // };












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