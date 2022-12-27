import Hash "mo:base/Hash";
import Map "mo:base/HashMap";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Nat32 "mo:base/Nat32";
import Text "mo:base/Text";
import Iter "mo:base/Iter";
import Float "mo:base/Float";
import Result "mo:base/Result";

module {

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
  public type UserID            =  Principal;
  public type contentID         = Nat32;
  public type ICPTs             = { e8s : Nat64 };
  public type AdminID           = Principal;
  public type AccountId         = Blob;
  public type SubaccountBlob = Nat8;
  public type AccountIdText     = Text;

  public type AccessType = ?{ 
    #ppv;
    #subscriber;
  };

  type TransferRequest = {
        info: Text;
        from: Principal;
        to: Principal;
        amount: ICPTs;
    };

  public type Content = {
    publisher: ArtistID;
    price: Nat64;
    accessType: AccessType;
  };

  public type User = {
    userId: UserID;
    status: ContentStatus;
  };
  
  public type ContentStatus = ?{ 
    #unlocked;
    #locked;
  };
}
