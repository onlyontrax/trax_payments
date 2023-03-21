import Hash "mo:base/Hash";
import Map "mo:base/HashMap";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Nat32 "mo:base/Nat32";
import Text "mo:base/Text";
import Iter "mo:base/Iter";
import Float "mo:base/Float";
import Result "mo:base/Result";

module Types{

  public type AccountIdentifier = {
    #text : Text;
    #principal : Principal;
    #blob : Blob;
  };
  // #region accountIdentifierToBlob
  public type AccountIdentifierToBlobArgs = {
    accountIdentifier : AccountIdentifier;
    canisterId : ?Principal;
  };
  public type AccountIdentifierToBlobResult = Result.Result<AccountIdentifierToBlobSuccess, AccountIdentifierToBlobErr>;
  public type AccountIdentifierToBlobSuccess = Blob;
  public type AccountIdentifierToBlobErr = {
    message : ?Text;
    kind : {
      #InvalidAccountIdentifier;
      #Other;
    };
  };
  // #endregion

// #region accountIdentifierToText
  public type AccountIdentifierToTextArgs = {
    accountIdentifier : AccountIdentifier;
    canisterId : ?Principal;
  };
  public type AccountIdentifierToTextResult = Result.Result<AccountIdentifierToTextSuccess, AccountIdentifierToTextErr>;
  public type AccountIdentifierToTextSuccess = Text;
  public type AccountIdentifierToTextErr = {
    message : ?Text;
    kind : {
      #InvalidAccountIdentifier;
      #Other;
    };
  };
// #endregion

// #region get_caller_identifier
  public type GetAccountIdentifierArgs = {
    token : Token;
    principal : Principal;
  };
  public type GetAccountIdentifierResult = Result.Result<GetAccountIdentifierSuccess, GetAccountIdentifierErr>;
  public type GetAccountIdentifierSuccess = {
    accountIdentifier : AccountIdentifier;
  };
  public type GetAccountIdentifierErr = {
    message : ?Text;
    kind : {
      #InvalidToken;
      #Other;
    };
  };
// #endregion

  public type Token =           { symbol : Text; };
  public type ArtistID          = Principal;
  public type FanID            =  Principal;
  public type ContentID         = Text;
  public type ICPTs             = { e8s : Nat64 };
  public type AdminID           = Principal;
  public type AccountId         = Blob;
  public type SubaccountBlob    = Nat8;
  public type AccountIdText     = Text;
  public type Percentage        = Float;
  public type Ticker                    = Text;
  public type Timestamp                 = Int;
  public type SubPrice                  = Float;
  public type TransactionID     = Text;

  public type AccessType = ?{ 
    #ppv;
    #subscriber;
  };

  public type SubscriptionType = { 
    #monthly;
    #yearly;
  };

  type TransferRequest = {
        info: Text;
        from: Principal;
        to: Principal;
        amount: ICPTs;
    };

  // public type Particiants = Map.HashMap<ParticipantID, Percentage>(1, Principal.equal, Principal.hash);
  public type Content = {
    publisher: ArtistID;
    publisherPercentage:  Percentage;
    price: Float; // usd price
    participants: [Participants];
    // participants: ?[(Percentage, ParticipantID)];
    // accessType: AccessType;
  };

  public type Participants = {
    participantID: ArtistID;
    participantPercentage: Percentage;
  };
  
  public type ContentStatus = ?{ 
    #unlocked;
    #locked;
  };

  // HTTP OUTPUT TYPES 
  public type HttpHeader = {
        name : Text;
        value : Text;
    };

    public type HttpMethod = {
        #get;
        #post;
        #head;
    };

    public type TransformContext = {
        function : shared query TransformArgs -> async CanisterHttpResponsePayload;
        context : Blob;
    };

    public type CanisterHttpRequestArgs = {
        url : Text;
        max_response_bytes : ?Nat64;
        headers : [HttpHeader];
        body : ?[Nat8];
        method : HttpMethod;
        transform : ?TransformContext;
    };

    public type CanisterHttpResponsePayload = {
        status : Nat;
        headers : [HttpHeader];
        body : [Nat8];
    };

    public type TransformArgs = {
        response : CanisterHttpResponsePayload;
        context : Blob;
    };

    public type IC = actor {
        http_request : Types.CanisterHttpRequestArgs -> async Types.CanisterHttpResponsePayload;
    };


}
