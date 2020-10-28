{ -----------------------------------------------------------------------------
  Unit Name: XERO.API
  Author: Tristan Marlow
  Purpose: XERO Accounting API  (http://developer.xero.com/)

  ----------------------------------------------------------------------------
  * Copyright (c) 2014 Tristan Marlow                                          *
  * Use this source code in open source or commercial software. You do not     *
  * need to provide any credit. However please provide any fixes or            *
  * enhancements to keep the component alive and helpful to everyone.          *
  *                                                                            *
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
  * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
  * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
  * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
  * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
  * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
  * DEALINGS IN THE SOFTWARE.                                                  *
  ******************************************************************************
  ----------------------------------------------------------------------------

  This component merges code from different sources

  Initial XERO RSA/OAuth code (Flow Software)
  ftp://ftp.flow.net.nz/release/code/OAuthWithXero.zip

  DCPcrypt Cryptographic Component Library
  http://sourceforge.net/projects/dcpcrypt/

  Fundamentals 4.00
  https://code.google.com/p/fundamentals/


  History: 01/12/2014 - First Release.
  28/10/2020 - OAuth2 Support


  ----------------------------------------------------------------------------- }

unit XERO.API;

interface

uses
  // System
  System.SysUtils, System.Classes, System.Variants, System.Generics.Collections,

  // Indy
  IdHTTP, IdIOHandler,

  // XERO
  XERO.Types;

type
  TOnLog = procedure(ASender: TObject; AMessage: string) of object;

  TXEROAuthenticatorBase = class;

  TXEROAppDetails = class(TXEROComponent)
  private
    FAppName: string;
    FClientID: string;
    FXEROAuthenticator: TXEROAuthenticatorBase;
    procedure SetClientID(const Value: string);
    procedure SetXEROAuthenticatorBase(const Value: TXEROAuthenticatorBase);
  protected
  public
    function ValidateSettings: Boolean; virtual;
  published
    property AppName: string read FAppName write FAppName;
    property ClientID: string read FClientID write SetClientID;
    property XEROAuthenticator: TXEROAuthenticatorBase read FXEROAuthenticator
      write SetXEROAuthenticatorBase;
  end;

  TXEROHTTPClientBase = class(TXEROComponent)
  private
    FXEROAppDetails: TXEROAppDetails;
    FSSLHandler: TIdIOHandler;
    FHTTPClient: TIdHTTP;
  protected
    procedure SetBearerHeader(AAccessToken: string);
    function HTTPClient: TIdHTTP;
    function GetXEROAppDetails: TXEROAppDetails;
    procedure SetXEROAppDetails(const Value: TXEROAppDetails);
    function GetXEROAuthenticator: TXEROAuthenticatorBase;
    property XEROAppDetails: TXEROAppDetails read GetXEROAppDetails
      write SetXEROAppDetails;
  end;

  TXEROAccessToken = class(TXEROComponent)
  private
    FrefreshToken: string;
    FidToken: string;
    FtokenType: string;
    FaccessToken: string;
    Fscope: string;
    FexpiresIn: integer;
    FlastRefresh: TDateTime;
    procedure SetaccessToken(const Value: string);
    procedure SetexpiresIn(const Value: integer);
    procedure SetidToken(const Value: string);
    procedure SetrefreshToken(const Value: string);
    procedure Setscope(const Value: string);
    procedure SettokenType(const Value: string);
    procedure SetlastRefresh(const Value: TDateTime);
  public
    constructor Create; reintroduce;
    procedure FromJSON(AJSON: string);
  published
    function IsValid: Boolean;
    function ExpiryDateTime: TDateTime;
    property IdToken: string read FidToken write SetidToken;
    property AccessToken: string read FaccessToken write SetaccessToken;
    property ExpiresIn: integer read FexpiresIn write SetexpiresIn;
    property TokenType: string read FtokenType write SettokenType;
    property RefreshToken: string read FrefreshToken write SetrefreshToken;
    property Scope: string read Fscope write Setscope;
    property LastRefresh: TDateTime read FlastRefresh write SetlastRefresh;
  end;

  TXEROTenant = class(TXEROComponent)
  private
    FTenantId: string;
    FupdatedDateUtc: string;
    FAuthEventId: string;
    FID: string;
    FcreatedDateUtc: string;
    FtenantName: string;
    FTenantType: string;
    procedure SetAuthEventId(const Value: string);
    procedure SetcreatedDateUtc(const Value: string);
    procedure SetID(const Value: string);
    procedure SetTenantId(const Value: string);
    procedure SettenantName(const Value: string);
    procedure SetupdatedDateUtc(const Value: string);
    procedure SetTenantType(const Value: string);
  public
    constructor Create; reintroduce;
    procedure FromJSON(AJSON: string);
  published
    property ID: string read FID write SetID;
    property AuthEventId: string read FAuthEventId write SetAuthEventId;
    property TenantId: string read FTenantId write SetTenantId;
    property TenantType: string read FTenantType write SetTenantType;
    property TenantName: string read FtenantName write SettenantName;
    property CreatedDateUtc: string read FcreatedDateUtc
      write SetcreatedDateUtc;
    property UpdatedDateUtc: string read FupdatedDateUtc
      write SetupdatedDateUtc;
  end;

  TXEROTenants = class(TObjectList<TXEROTenant>)
  public
    procedure FromJSON(AJSON: string);
  end;

  TXEROOnAuthenticationComplete = procedure(ASender: TObject; ASuccess: Boolean)
    of object;

  TXEROAuthenticatorBase = class(TXEROHTTPClientBase)
  private
    FXEROAccessToken: TXEROAccessToken;
    FXEROTenants: TXEROTenants;
    FThreadTimer: TThreadTimer;
    FOnAuthenticationComplete: TXEROOnAuthenticationComplete;
    function GetAccessToken: TXEROAccessToken;
    function GetExpiryDate: TDateTime;
    procedure SetOnAuthenticationComplete(const Value
      : TXEROOnAuthenticationComplete);
    function GetXEROTenants: TXEROTenants;
    procedure CheckAccessTokenRefresh;
  protected
    procedure Authenticate; virtual; abstract;
    procedure RefreshAuthentication; virtual;
    function UpdateTenants(var AMessage: string): Boolean; virtual;
    function GetAuthenticated: Boolean; virtual;
    procedure SetDefaultProperties; virtual;
    procedure InternalTimerEvent; virtual;
    function RefreshAccessToken(var AMessage: string): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    procedure Login;
    procedure Refresh;
    procedure OnThreadTimer(ASender: TObject);
    property AccessToken: TXEROAccessToken read GetAccessToken;
    property Tenants: TXEROTenants read GetXEROTenants;
  published
    property Authenticated: Boolean read GetAuthenticated;
    property Expirydate: TDateTime read GetExpiryDate;
    property OnAuthenticationComplete: TXEROOnAuthenticationComplete
      read FOnAuthenticationComplete write SetOnAuthenticationComplete;
  end;

  TXEROResponseBase = class(TXEROHTTPClientBase)
  private
    FResponse: TMemoryStream;
    FResponseCode: integer;
    FResult: Boolean;
    FErrorMessage: string;
    FResponseType: TResponseType;
  protected
    function GetDefaultResponseType: TResponseType; virtual;

    procedure SetResponse(AResult: Boolean; ACode: integer;
      ADetail: String); virtual;

    property ResponseType: TResponseType read FResponseType write FResponseType;
    function rStream: TStream;
  public
    procedure AfterConstruction; override;
    Procedure BeforeDestruction; override;

    procedure ClearStream;

    property Stream: TStream read rStream;

    function AsString: string;
    property Result: Boolean read FResult;
    property ResponseCode: integer read FResponseCode;
    property ErrorMessage: string read FErrorMessage;
  end;

  TXEROResponseBaseClass = class of TXEROResponseBase;

  TXEROFilter = class(TXEROObject)
  private
    FFilter: string;
  protected
    function GetDateTimeFilterString(ADateTime: TDateTime): string;
  public
    procedure ResetFilter;
    procedure AddToFilter(AFieldName, AData: string;
      AQuoteData: Boolean = true); overload;
    procedure AddToFilter(AFieldName: string; AData: integer;
      AQuoteData: Boolean = false)overload;
    procedure AddToFilter(AFieldName: string; AData: double;
      AQuoteData: Boolean = false)overload;
    procedure AddToFilter(AFieldName: string; AData: TDate;
      AQuoteData: Boolean = false)overload;
    procedure AddGUIDToFilter(AFieldName, AData: string);
    function Text: string;
  end;

  TXEROAPIBase = class(TXEROHTTPClientBase)
  private
    FTenantId: string;
    procedure SetTenantId(const Value: string);
  protected
    function NormalisedURL(const AURL: string): string;
    function GetGUIDString: string;
    // function OAuthTimeStamp: string;
    procedure ValidateSettings; virtual;
    function StreamToString(AStream: TStream): string;
    procedure StringToStream(AStream: TStream; AValue: string);
    function GetErrorResponse(AResponseCode: integer; AErrorDetail: string;
      AResponse: TStream = nil; AResponseType: TResponseType = rtXML): string;
    function ParamsToURL(AURL: string; AParams: TStrings = nil): string;

    procedure SetHTTPHeader;

    function Get(AURL: string; AParams: string; var AResponse: string;
      ALastModified: TDateTime = 0; AResponseType: TResponseType = rtXML)
      : Boolean; overload;
    function Get(AURL: string; AParams: TStrings; AResponse: TStream;
      var ResponseCode: integer; var ErrorDetail: string;
      ALastModified: TDateTime = 0; AResponseType: TResponseType = rtXML)
      : Boolean; overload;

    function Post(AURL: String; AParams: TStrings; ARequest: TStream;
      AResponse: TStream; var ResponseCode: integer; var ErrorDetail: String;
      AResponseType: TResponseType = rtXML): Boolean; overload;
    function Post(AURL: String; AParams: string; ARequest: string;
      var AResponse: string; AResponseType: TResponseType = rtXML)
      : Boolean; overload;
    function Post<T: TXEROResponseBase>(AURL: String; ARequest: string;
      AResponse: T; AParams: string = ''): Boolean; overload;

    function Put(AURL: String; AParams: TStrings; ARequest: TStream;
      AResponse: TStream; var ResponseCode: integer; var ErrorDetail: String;
      AResponseType: TResponseType = rtXML): Boolean; overload;
    function Put(AURL: String; AParams: string; ARequest: string;
      var AResponse: string; AResponseType: TResponseType = rtXML)
      : Boolean; overload;
    function Put<T: TXEROResponseBase>(AURL: String; ARequest: string;
      AResponse: T; AParams: string = ''): Boolean; overload;

    function Delete(AURL: String; AResponse: TStream; var ResponseCode: integer;
      var ErrorDetail: String; AResponseType: TResponseType = rtXML)
      : Boolean; overload;
    function Delete(AURL: String; var AResponse: string;
      AResponseType: TResponseType = rtXML): Boolean; overload;
    function Delete<T: TXEROResponseBase>(AURL: String; AResponse: T)
      : Boolean; overload;

    function GetFilterURL(AURL, AFilter, AOrderBy: string;
      APage: integer): string;

    function Find<T: TXEROResponseBase>(AURL: string; AResponse: T;
      AFilter: string = ''; AOrderBy: string = ''; APage: integer = 0;
      ALastModified: TDateTime = 0): Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property XEROAppDetails;
    property XEROAuthenticator: TXEROAuthenticatorBase
      read GetXEROAuthenticator;
    property TenantId: string read FTenantId write SetTenantId;
  end;

  TXEROAPI = class(TXEROAPIBase)
  private
  protected
    function GetAPIURL: string; virtual; abstract;
    function Find<T: TXEROResponseBase>(AResponse: T; AFilter: string = '';
      AOrderBy: string = ''; APage: integer = 0; ALastModified: TDateTime = 0)
      : Boolean; reintroduce;
    function Post<T: TXEROResponseBase>(ARequest: string; AResponse: T;
      AParams: string = ''): Boolean; reintroduce;
    function Put<T: TXEROResponseBase>(ARequest: string; AResponse: T;
      AParams: string = ''): Boolean; reintroduce;
    function Delete<T: TXEROResponseBase>(AResponse: T; AParams: string = '')
      : Boolean; reintroduce;
  public
  published

  end;

  TXDataOp = (xdoEqual, xdoLess, xdoGreater, xdoLessEqual, xdoGreaterEqual,
    xdoNotEqual);
  TXDataFunc = (xdfStarts, xdfEnds, xdfContains);
  TXDataLogic = (xdlAnd, xdlOr, xdlNot);

  TBuilderState = (bsInit, bsOk, bsPending);

  { : Create a XERO 'DotNet' query string.
    @Example
    qb := TXEROQueryBuilder.Create;
    qb.Op(xdoEqual, 'URL', 'http://place.com/url');
    qb.Logic(xdlOr);
    qb.Fn(xdfContains, 'Narration', 'Friend');
  }
  TXEROQueryBuilder = record
  private

    FBuildString: String;
    FLen, FCapacity: integer;

    FOpenParen: integer;
    FState: TBuilderState;
    procedure AppendStr(const strVal: String);

    procedure OutValue(fieldValue: Variant; ForceString: Boolean = false);
    function GetString: String;

    procedure DoOp(AOp: TXDataOp; FieldName: String; fieldValue: Variant);
    procedure DoFn(AFn: TXDataFunc; FieldName: String; fieldValue: Variant;
      ProtectNull: Boolean);
  public
    class function Create: TXEROQueryBuilder; static;
    procedure Init;

    // Standard equality/inequality between a field and a value.
    procedure Op(FieldName: String; AOp: TXDataOp; fieldValue: Variant);

    { Simple string functions (contains, starts, ends)

      With 'protectNull == true'  (the default) this surrounds the expression with ( Fieldname != null and <expression> ).
      This is because without this protection, the query will tend to inexplicably crash.
      For truly mandatory fields, it could be omitted, but it's safer to include it as the default.
    }
    procedure Fn(FieldName: String; AFn: TXDataFunc; fieldValue: Variant;
      ProtectNull: Boolean = true);

    // And/or/not logic operator
    procedure Logic(ALg: TXDataLogic);

    // Begin parenthesis (grouping terms)
    procedure StartParen;
    // End parenthesis (grouping terms)
    procedure EndParen;

    // Retrieve the value as a string for an OData query.
    property AsString: String read GetString;
  end;

function IsGUID(const strVal: String): Boolean;

implementation

uses
  // System
  Windows, System.DateUtils, System.StrUtils, JSON,

  // Indy
  IdGlobal, IdHMACSHA1, IdSSL, IdSSLOpenSSL,
  IdBaseComponent,
  IdIOHandlerStack,

  // XERO
  XERO.Utils,  XERO.VarUtil;

// TXEROAppDetails

procedure TXEROAppDetails.SetClientID(const Value: string);
begin
  FClientID := Value;
end;

procedure TXEROAppDetails.SetXEROAuthenticatorBase
  (const Value: TXEROAuthenticatorBase);
begin
  FXEROAuthenticator := Value;
  if Assigned(FXEROAuthenticator) then
    FXEROAuthenticator.XEROAppDetails := Self;
end;

function TXEROAppDetails.ValidateSettings: Boolean;
begin
  Result := true;
  if Result then
    Result := not IsEmptyString(FClientID);
  if Result then
    Result := Assigned(FXEROAuthenticator);
end;

// TXEROAPIBase
//

function TXEROAPIBase.NormalisedURL(const AURL: string): string;
begin
  Result := AURL;

  if (Pos(':80/', Result) <> 0) and (Copy(Result, 1, 5) = 'http:') then
    Result := StringReplace(Result, ':80/', '/', []);

  if (Pos(':443/', Result) <> 0) and (Copy(Result, 1, 6) = 'https:') then
    Result := StringReplace(Result, ':443/', '/', []);

  Result := String(UTF8Encode(Result));
end;

function TXEROAPIBase.GetGUIDString: string;
var
  Guid: TGUID;
begin
  CreateGUID(Guid);
  Result := GUIDToString(Guid);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
// function TXEROAPIBase.OAuthTimeStamp: string;
// const
// UNIX_BASE = 25569.0;
// var
// dt: TDateTime;
// ts: integer;
// begin
// dt := TTimeZone.Local.ToUniversaltime(now);
// ts := Round((dt - UNIX_BASE) * 86400);
// Result := IntToStr(ts);
// end;

constructor TXEROAPIBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TXEROAPIBase.Delete(AURL: String; var AResponse: string;
  AResponseType: TResponseType): Boolean;
var
  LResponseStream: TStringStream;
  LParams: TStringList;
  LResponseCode: integer;
  LErrorDetail: string;
begin
  Result := false;
  LResponseStream := TStringStream.Create;
  LParams := TStringList.Create;
  try
    if Delete(AURL, LResponseStream, LResponseCode, LErrorDetail, AResponseType)
    then
    begin
      AResponse := LResponseStream.DataString;
      Result := true;
    end
    else
    begin
      AResponse := GetErrorResponse(LResponseCode, LErrorDetail,
        LResponseStream, AResponseType);
    end;
  finally
    FreeAndNil(LResponseStream);
    FreeAndNil(LParams);
  end;
end;

function TXEROAPIBase.Delete<T>(AURL: String; AResponse: T): Boolean;
var
  LURL: string;
  LErrorMessage: string;
  LResponseCode: integer;
begin
  if not Assigned(AResponse) then
    AResponse := T.Create(nil);
  Result := Delete(LURL, AResponse.Stream, LResponseCode, LErrorMessage,
    AResponse.ResponseType);

  AResponse.SetResponse(Result, LResponseCode, LErrorMessage);

end;

destructor TXEROAPIBase.Destroy;
begin
  try
    if Assigned(FHTTPClient) then
      FHTTPClient.IOHandler := nil;
    FreeAndNil(FSSLHandler);
    FreeAndNil(FHTTPClient);
  finally
    inherited;
  end;
end;

function TXEROAPIBase.GetFilterURL(AURL, AFilter, AOrderBy: string;
  APage: integer): string;
begin
  Result := AURL;
  if not IsEmptyString(AFilter) then
  begin
    Result := Result + GetURLSeperator(Result) + 'where=' + URLEncode(AFilter);
  end;
  if not IsEmptyString(AOrderBy) then
  begin
    Result := Result + GetURLSeperator(Result) + 'order=' + URLEncode(AOrderBy);
  end;
  if APage > 0 then
  begin
    Result := Result + GetURLSeperator(Result) + 'page=' + IntToStr(APage);
  end;
end;

function TXEROAPIBase.Find<T>(AURL: string; AResponse: T;
  AFilter, AOrderBy: string; APage: integer; ALastModified: TDateTime): Boolean;
var
  LURL: string;
  LErrorMessage: string;
  LResponseCode: integer;
begin
  if not Assigned(AResponse) then
    AResponse := T.Create(nil);

  LURL := GetFilterURL(AURL, AFilter, AOrderBy, APage);

  Result := Get(LURL, nil, AResponse.Stream, LResponseCode, LErrorMessage,
    ALastModified, AResponse.ResponseType);

  AResponse.SetResponse(Result, LResponseCode, LErrorMessage);

end;

function TXEROAPIBase.Delete(AURL: String; AResponse: TStream;
  var ResponseCode: integer; var ErrorDetail: String;
  AResponseType: TResponseType): Boolean;
var
  LURL: string;
begin
  ValidateSettings;
  try

    // case FXEROAppDetails.OAuthSignatureMethod of
    // oaHMAC:
    // begin
    // OAuthSignRequest(HTTPClient.Request, 'DELETE', AURL, nil,
    // FXEROAppDetails.ConsumerKey, FXEROAppDetails.ConsumerKey,
    // FXEROAppDetails.ConsumerSecret, FXEROAppDetails.ConsumerSecret);
    // end;
    //
    // oaRSA:
    // begin
    // OAuthSignRequest(HTTPClient.Request, 'DELETE', AURL, nil,
    // FXEROAppDetails.ConsumerKey, FXEROAppDetails.ConsumerKey,
    // FXEROAppDetails.PrivateKey.Text);
    // end;
    // end;

    HTTPClient.Request.ContentType := 'application/x-www-form-urlencoded';
    case AResponseType of
      rtXML:
        ;
      rtJSON:
        HTTPClient.Request.Accept := 'application/json';
    end;
    HTTPClient.HTTPOptions := [hoForceEncodeParams];

    try
      LURL := ParamsToURL(AURL, nil);
      SetHTTPHeader;
      Log(Format('URL: %s, Headers: %s, Accept: %s',
        [LURL, HTTPClient.Request.CustomHeaders.Text,
        HTTPClient.Request.Accept]));
      HTTPClient.Delete(LURL, AResponse);
      ResponseCode := HTTPClient.Response.ResponseCode;
      ErrorDetail := HTTPClient.Response.ResponseText;
      Result := true;

    except
      on E: EIdHTTPProtocolException do
      begin
        ResponseCode := E.ErrorCode;
        ErrorDetail := E.Message;
        Result := false;
        StringToStream(AResponse, E.ErrorMessage);
      end;
    end;

  except
    on E: Exception do
    begin
      ResponseCode := 10000;
      ErrorDetail := E.Message;
      Result := false;
    end;
  end;

  Debug('Delete', 'Result: ' + BoolToStr(Result, true) + ', Response: ' +
    StreamToString(AResponse));
end;

procedure TXEROAPIBase.ValidateSettings;
begin
  if Assigned(FXEROAppDetails) then
  begin
    if not FXEROAppDetails.ValidateSettings then
    begin
      raise EXEROException.Create
        ('Application setting are incomplete. Please ensure ClientID and Authentication component are assiged.');
    end;
  end
  else
  begin
    raise EXEROException.Create('Application settings have not been assigned.');
  end;
  if Assigned(XEROAuthenticator) then
  begin
    if not XEROAuthenticator.Authenticated then
    begin
      raise EXEROException.Create('Authentication required.');
    end;
  end
  else
  begin
    raise EXEROException.Create('Authentication has not been assigned.');
  end;
end;

procedure TXEROAPIBase.SetHTTPHeader;
begin
  if XEROAuthenticator.AccessToken.IsValid then
  begin
    SetBearerHeader(XEROAuthenticator.AccessToken.AccessToken);
    HTTPClient.Request.CustomHeaders.Values['xero-tenant-id'] := FTenantId;
  end;
end;

procedure TXEROAPIBase.SetTenantId(const Value: string);
begin
  FTenantId := Value;
end;

function TXEROAPIBase.StreamToString(AStream: TStream): string;
var
  LStringStream: TStringStream;
begin
  Result := '';
  LStringStream := TStringStream.Create('');
  try
    try
      LStringStream.CopyFrom(AStream, 0);
      Result := LStringStream.DataString;
    except
      on E: Exception do
      begin
        Error(E, 'Failed to convert stream to string');
      end;
    end;
  finally
    FreeAndNil(LStringStream);
  end;
end;

procedure TXEROAPIBase.StringToStream(AStream: TStream; AValue: string);
var
  LStringStream: TStringStream;
begin
  LStringStream := TStringStream.Create(AValue);
  try
    try
      if Assigned(AStream) then
      begin
        AStream.CopyFrom(LStringStream, 0);
      end;
    except
      on E: Exception do
      begin
        Error(E, 'Failed to convert string to stream');
      end;
    end;
  finally
    FreeAndNil(LStringStream);
  end;
end;

function TXEROAPIBase.Get(AURL: string; AParams: string; var AResponse: string;
  ALastModified: TDateTime = 0; AResponseType: TResponseType = rtXML): Boolean;
var
  LResponseStream: TStringStream;
  LParams: TStringList;
  LResponseCode: integer;
  LErrorDetail: string;
begin
  Result := false;
  LResponseStream := TStringStream.Create;
  LParams := TStringList.Create;
  try
    LParams.Text := AParams;
    if Get(AURL, LParams, LResponseStream, LResponseCode, LErrorDetail,
      ALastModified, AResponseType) then
    begin
      AResponse := LResponseStream.DataString;
      Result := true;
    end
    else
    begin
      AResponse := GetErrorResponse(LResponseCode, LErrorDetail,
        LResponseStream, AResponseType);
    end;
  finally
    FreeAndNil(LResponseStream);
    FreeAndNil(LParams);
  end;
end;

function TXEROAPIBase.Get(AURL: string; AParams: TStrings; AResponse: TStream;
  var ResponseCode: integer; var ErrorDetail: string;
  ALastModified: TDateTime = 0; AResponseType: TResponseType = rtXML): Boolean;
var
  LURL: string;
begin
  Result := true;
  ValidateSettings;
  try
    LURL := ParamsToURL(AURL, AParams);

    // case FXEROAppDetails.OAuthSignatureMethod of
    // oaHMAC:
    // begin
    // OAuthSignRequest(HTTPClient.Request, 'GET', LURL, nil,
    // FXEROAppDetails.ConsumerKey, FXEROAppDetails.ConsumerKey,
    // FXEROAppDetails.ConsumerSecret, FXEROAppDetails.ConsumerSecret);
    // end;
    //
    // oaRSA:
    // begin
    // OAuthSignRequest(HTTPClient.Request, 'GET', LURL, nil,
    // FXEROAppDetails.ConsumerKey, FXEROAppDetails.ConsumerKey,
    // FXEROAppDetails.PrivateKey.Text);
    // end;
    // end;

    case AResponseType of
      rtXML:
        begin
        end;
      rtJSON:
        begin
          HTTPClient.Request.Accept := 'application/json';
          HTTPClient.Request.ContentType := 'application/json';
        end;
    end;

    if ALastModified <> 0 then
    begin
      HTTPClient.Request.LastModified := ALastModified;
    end;

    try
      SetHTTPHeader;
      Log(Format('URL: %s, Headers: %s, Accept: %s, Last Modified %s',
        [LURL, HTTPClient.Request.CustomHeaders.Text, HTTPClient.Request.Accept,
        DateTimeToStr(HTTPClient.Request.LastModified)]));
      HTTPClient.Get(LURL, AResponse);
      ResponseCode := HTTPClient.Response.ResponseCode;
      ErrorDetail := HTTPClient.Response.ResponseText;
    except
      on E: EIdHTTPProtocolException do
      begin
        ResponseCode := E.ErrorCode;
        ErrorDetail := E.Message + ' ' + E.ErrorMessage;
        Result := false;
        StringToStream(AResponse, E.ErrorMessage);
        Error(ErrorDetail, ResponseCode);
      end;
    end;

  except
    on E: Exception do
    begin
      ResponseCode := 10000;
      ErrorDetail := E.Message;
      Result := false;
      Error(ErrorDetail, ResponseCode);
    end;
  end;

  Debug('Get', 'Result: ' + BoolToStr(Result, true) + ', Response: ' +
    StreamToString(AResponse));

end;

function TXEROAPIBase.GetErrorResponse(AResponseCode: integer;
  AErrorDetail: string; AResponse: TStream;
  AResponseType: TResponseType): string;
var
  LMessage: TStringList;
  LDescription: string;
begin
  LMessage := TStringList.Create;
  try
    LDescription := AErrorDetail;
    if Assigned(AResponse) then
      LDescription := LDescription + #13#10 + StreamToString(AResponse);

    case AResponseType of
      rtJSON:
        begin
          LMessage.Add('{');
          LMessage.Add(Format('"ErrorCode" : %s',
            [AnsiQuotedStr(IntToStr(AResponseCode), '"')]));
          LMessage.Add(Format('"Description" : %s',
            [AnsiQuotedStr(LDescription, '"')]));
          LMessage.Add('}');
        end;
      rtXML:
        begin
          LMessage.Add('<?xml version=''1.0'' encoding=''utf-8''?>');
          LMessage.Add('<Error>');
          LMessage.Add(Format('<ErrorCode>%d</ErrorCode>', [AResponseCode]));
          LMessage.Add('<Description>');
          LMessage.Add('![CDATA[' + LDescription + ']]');
          LMessage.Add('</Description>');
          LMessage.Add('</Error>');
        end;
    end;
    Result := LMessage.Text;
  finally
    FreeAndNil(LMessage);
  end;
end;

function TXEROAPIBase.Post(AURL: String; AParams: TStrings; ARequest: TStream;
  AResponse: TStream; var ResponseCode: integer; var ErrorDetail: String;
  AResponseType: TResponseType): Boolean;
var
  LURL: string;
begin
  ValidateSettings;
  try
    LURL := ParamsToURL(AURL, AParams);

    // case FXEROAppDetails.OAuthSignatureMethod of
    // oaHMAC:
    // begin
    // OAuthSignRequest(HTTPClient.Request, 'POST', AURL, nil,
    // FXEROAppDetails.ConsumerKey, FXEROAppDetails.ConsumerKey,
    // FXEROAppDetails.ConsumerSecret, FXEROAppDetails.ConsumerSecret);
    // end;
    //
    // oaRSA:
    // begin
    // OAuthSignRequest(HTTPClient.Request, 'POST', AURL, nil,
    // FXEROAppDetails.ConsumerKey, FXEROAppDetails.ConsumerKey,
    // FXEROAppDetails.PrivateKey.Text);
    // end;
    // end;

    HTTPClient.Request.ContentType := 'text/xml;charset=UTF-8';
    case AResponseType of
      rtXML:
        ;
      rtJSON:
        begin
          HTTPClient.Request.Accept := 'application/json';
          HTTPClient.Request.ContentType := 'application/json';
        end;
    end;
    HTTPClient.HTTPOptions := [hoForceEncodeParams];

    try
      SetHTTPHeader;
      Log(Format('URL: %s, Headers: %s, Accept: %s',
        [LURL, HTTPClient.Request.CustomHeaders.Text,
        HTTPClient.Request.Accept]));
      HTTPClient.Post(LURL, ARequest, AResponse);
      ResponseCode := HTTPClient.Response.ResponseCode;
      ErrorDetail := HTTPClient.Response.ResponseText;
      Result := true;

    except
      on E: EIdHTTPProtocolException do
      begin
        ResponseCode := E.ErrorCode;
        ErrorDetail := E.Message;
        Result := false;
        StringToStream(AResponse, E.ErrorMessage);
      end;
    end;

  except
    on E: Exception do
    begin
      ResponseCode := 10000;
      ErrorDetail := E.Message;
      Result := false;
    end;
  end;

  Debug('Put', 'Result: ' + BoolToStr(Result, true) + ', Response: ' +
    StreamToString(AResponse));
end;

function TXEROAPIBase.ParamsToURL(AURL: string; AParams: TStrings): string;
var
  LParams: string;
begin
  Result := AURL;
  if Assigned(AParams) then
  begin
    LParams := AParams.Text;
    LParams := StringReplace(LParams, #13, '', [rfReplaceAll, rfIgnoreCase]);
    LParams := StringReplace(LParams, #10, '&', [rfReplaceAll, rfIgnoreCase]);
    if not IsEmptyString(LParams) then
    begin
      Result := Result + '?' + Copy(LParams, 1, Length(LParams) - 1);
    end;
  end;

end;

function TXEROAPIBase.Post(AURL, AParams, ARequest: string;
  var AResponse: string; AResponseType: TResponseType): Boolean;
var
  LRequestStream: TStringStream;
  LResponseStream: TStringStream;
  LParams: TStringList;
  LResponseCode: integer;
  LErrorDetail: string;
begin
  Result := false;
  LResponseStream := TStringStream.Create;
  LRequestStream := TStringStream.Create(ARequest);
  LParams := TStringList.Create;
  try
    Debug('Post', 'Request: ' + LRequestStream.DataString);
    LParams.Text := AParams;
    if Post(AURL, LParams, LRequestStream, LResponseStream, LResponseCode,
      LErrorDetail, AResponseType) then
    begin
      AResponse := LResponseStream.DataString;
      Result := true;
    end
    else
    begin
      AResponse := GetErrorResponse(LResponseCode, LErrorDetail,
        LResponseStream, AResponseType);
    end;
  finally
    FreeAndNil(LRequestStream);
    FreeAndNil(LResponseStream);
    FreeAndNil(LParams);
  end;
end;

function TXEROAPIBase.Post<T>(AURL, ARequest: string; AResponse: T;
  AParams: string): Boolean;
var
  LURL: string;
  LErrorMessage: string;
  LResponseCode: integer;
  LParams: TStringList;
  LRequest: TStringStream;
begin
  LParams := TStringList.Create;
  LRequest := TStringStream.Create(ARequest);
  try
    if not Assigned(AResponse) then
      AResponse := T.Create(nil);

    LParams.Text := AParams;

    Result := Post(LURL, LParams, LRequest, AResponse.Stream, LResponseCode,
      LErrorMessage, AResponse.ResponseType);

    AResponse.SetResponse(Result, LResponseCode, LErrorMessage);
  finally
    FreeAndNil(LParams);
    FreeAndNil(LRequest);
  end;
end;

function TXEROAPIBase.Put(AURL, AParams, ARequest: string;
  var AResponse: string; AResponseType: TResponseType): Boolean;
var
  LRequestStream: TStringStream;
  LResponseStream: TStringStream;
  LParams: TStringList;
  LResponseCode: integer;
  LErrorDetail: string;
begin
  Result := false;
  LResponseStream := TStringStream.Create;
  LRequestStream := TStringStream.Create(ARequest);
  LParams := TStringList.Create;
  try
    Debug('Put', 'Request: ' + LRequestStream.DataString);
    LParams.Text := AParams;
    if Put(AURL, LParams, LRequestStream, LResponseStream, LResponseCode,
      LErrorDetail, AResponseType) then
    begin
      AResponse := LResponseStream.DataString;
      Result := true;
    end
    else
    begin
      AResponse := GetErrorResponse(LResponseCode, LErrorDetail,
        LResponseStream, AResponseType);
    end;
  finally
    FreeAndNil(LRequestStream);
    FreeAndNil(LResponseStream);
    FreeAndNil(LParams);
  end;
end;

function TXEROAPIBase.Put<T>(AURL, ARequest: string; AResponse: T;
  AParams: string): Boolean;
var
  LErrorMessage: string;
  LResponseCode: integer;
  LParams: TStringList;
  LRequest: TStringStream;
begin
  LParams := TStringList.Create;
  LRequest := TStringStream.Create(ARequest);
  try
    if not Assigned(AResponse) then
      AResponse := T.Create(nil);

    LParams.Text := AParams;

    Result := Put(AURL, LParams, LRequest, AResponse.Stream, LResponseCode,
      LErrorMessage, AResponse.ResponseType);

    AResponse.SetResponse(Result, LResponseCode, LErrorMessage);
  finally
    FreeAndNil(LParams);
    FreeAndNil(LRequest);
  end;
end;

function TXEROAPIBase.Put(AURL: String; AParams: TStrings; ARequest: TStream;
  AResponse: TStream; var ResponseCode: integer; var ErrorDetail: String;
  AResponseType: TResponseType): Boolean;
var
  LURL: string;
begin
  ValidateSettings;
  try
    LURL := ParamsToURL(AURL, AParams);

    // case FXEROAppDetails.OAuthSignatureMethod of
    // oaHMAC:
    // begin
    // OAuthSignRequest(HTTPClient.Request, 'PUT', AURL, nil,
    // FXEROAppDetails.ConsumerKey, FXEROAppDetails.ConsumerKey,
    // FXEROAppDetails.ConsumerSecret, FXEROAppDetails.ConsumerSecret);
    // end;
    //
    // oaRSA:
    // begin
    // OAuthSignRequest(HTTPClient.Request, 'PUT', AURL, nil,
    // FXEROAppDetails.ConsumerKey, FXEROAppDetails.ConsumerKey,
    // FXEROAppDetails.PrivateKey.Text);
    // end;
    // end;

    HTTPClient.Request.ContentType := 'application/x-www-form-urlencoded';
    case AResponseType of
      rtXML:
        ;
      rtJSON:
        begin
          HTTPClient.Request.Accept := 'application/json';
          HTTPClient.Request.ContentType := 'application/json';
        end;
    end;
    HTTPClient.HTTPOptions := [hoForceEncodeParams];

    try
      SetHTTPHeader;
      Log(Format('URL: %s, Headers: %s, Accept: %s',
        [LURL, HTTPClient.Request.CustomHeaders.Text,
        HTTPClient.Request.Accept]));
      HTTPClient.Put(LURL, ARequest, AResponse);
      ResponseCode := HTTPClient.Response.ResponseCode;
      ErrorDetail := HTTPClient.Response.ResponseText;
      Result := true;

    except
      on E: EIdHTTPProtocolException do
      begin
        ResponseCode := E.ErrorCode;
        ErrorDetail := E.Message;
        Result := false;
        StringToStream(AResponse, E.ErrorMessage);
      end;
    end;

  except
    on E: Exception do
    begin
      ResponseCode := 10000;
      ErrorDetail := E.Message;
      Result := false;
    end;
  end;

  Debug('Put', 'Result: ' + BoolToStr(Result, true) + ', Response: ' +
    StreamToString(AResponse));
end;




// TXEROResponseBase

function TXEROResponseBase.rStream: TStream;
begin
  Result := FResponse;
end;

procedure TXEROResponseBase.ClearStream;
begin
  FResponse.Clear;
end;

procedure TXEROResponseBase.SetResponse(AResult: Boolean; ACode: integer;
  ADetail: String);
begin
  FResult := AResult;
  FResponseCode := ACode;
  FErrorMessage := ADetail;
end;

function TXEROResponseBase.AsString: string;
var
  sr: TStreamReader;
begin
  FResponse.Position := 0;
  sr := TStreamReader.Create(FResponse);
  try
    Result := sr.ReadToEnd;
  finally
    sr.Free;
  end;
end;

function TXEROResponseBase.GetDefaultResponseType: TResponseType;
begin
  Result := rtXML;
end;

procedure TXEROResponseBase.AfterConstruction;
begin
  inherited;

  FResponseType := GetDefaultResponseType;
  FResponse := TMemoryStream.Create;
  FResult := false;
  FErrorMessage := '';
  FResponseCode := -1;
end;

Procedure TXEROResponseBase.BeforeDestruction;
begin
  FreeAndNil(FResponse);
  inherited;
end;

function TXEROAPI.Find<T>(AResponse: T; AFilter, AOrderBy: string;
  APage: integer; ALastModified: TDateTime): Boolean;
begin
  Result := inherited Find<T>(GetAPIURL, AResponse, AFilter, AOrderBy, APage,
    ALastModified);
end;

function TXEROAPI.Post<T>(ARequest: string; AResponse: T;
  AParams: string): Boolean;
begin
  Result := inherited Post<T>(GetAPIURL, ARequest, AResponse);
end;

function TXEROAPI.Put<T>(ARequest: string; AResponse: T;
  AParams: string): Boolean;
begin
  Result := inherited Put<T>(GetAPIURL, ARequest, AResponse);
end;

// TXEROAPI

function TXEROAPI.Delete<T>(AResponse: T; AParams: string): Boolean;
begin
  Result := inherited Delete<T>(GetAPIURL, AResponse);
end;

// TXEROQueryBuilder
//
const
  CODataOp: array [TXDataOp] of string = ('==', '<', '>', '<=', '>=', '!=');
  CODataFunc: array [TXDataFunc] of string = ('StartsWith', 'EndsWith',
    'Contains');
  CODataLogic: array [TXDataLogic] of string = ('AND', 'OR', 'NOT');
  CXNull = 'null';

function IsGUID(const strVal: String): Boolean;
const
  CPattern = 'xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx';
var
  idx: integer;
begin
  Result := Length(strVal) = Length(CPattern);
  if Result then
  begin
    for idx := 1 to Length(strVal) do
    begin
      case strVal[idx] of
        'a' .. 'f', '0' .. '9':
          if CPattern[idx] = '-' then
          begin
            Result := false;
            break;
          end;
        '-':
          if CPattern[idx] <> '-' then
          begin
            Result := false;
            break;
          end;
      else
        Result := false;
        break;
      end
    end;

  end;
end;

// protected definitions

procedure TXEROQueryBuilder.OutValue(fieldValue: Variant; ForceString: Boolean);
var
  dt: TDateTime;
  strVal: String;
begin
  case VarType(fieldValue) of
    varDate:
      begin
        dt := TDateTime(fieldValue);
        AppendStr('DateTime(');
        if TimeOf(dt) = 0 then
          AppendStr(formatDatetime('yyyy,m,d', dt))
        else
          AppendStr(formatDatetime('yyyy,m,d,hh,nn,ss', dt));
        AppendStr(')');
      end;
    varBoolean:
      AppendStr(IfThen(CastVarAsBoolean(fieldValue, true, true, false), 'true',
        'false'));
  else
    if VarIsEmpty(fieldValue) then
      AppendStr(CXNull)
    else if VarIsInteger(fieldValue) then
      AppendStr(IntToStr(CastVarAsInt(fieldValue)))
    else if VarIsRealNumber(fieldValue) then
      AppendStr(FloatToStr(CastVarAsDouble(fieldValue)))
    else
    begin
      strVal := CastVarAsString(fieldValue);
      if IsGUID(strVal) then
      begin
        AppendStr('Guid("');
        AppendStr(strVal);
        AppendStr('")');
      end
      else
      begin
        AppendStr('"');
        AppendStr(StringReplace(StringReplace(strVal, '\', '\\',
          [rfReplaceAll]), '"', '\"', [rfReplaceAll]));
        AppendStr('"');
      end;
    end;
  end;
end;

function TXEROQueryBuilder.GetString: String;
begin
  if FOpenParen > 0 then
    Raise Exception.Create('Unclosed parenthesis');
  if FState = bsPending then
    Raise Exception.Create('Expected expression');

  if FCapacity <> FLen then
  begin
    FCapacity := FLen;
    SetLength(FBuildString, FLen);
  end;
  Result := FBuildString;
end;

procedure TXEROQueryBuilder.DoOp(AOp: TXDataOp; FieldName: String;
  fieldValue: Variant);
begin
  AppendStr(FieldName);
  AppendStr(' ');
  AppendStr(CODataOp[AOp]);
  AppendStr(' ');
  OutValue(fieldValue);
end;

procedure TXEROQueryBuilder.DoFn(AFn: TXDataFunc; FieldName: String;
  fieldValue: Variant; ProtectNull: Boolean);
begin
  if ProtectNull then
  begin
    AppendStr('(');
    AppendStr(FieldName);
    AppendStr(CODataOp[xdoNotEqual]);
    AppendStr(CXNull);
    AppendStr(' ');
    AppendStr(CODataLogic[xdlAnd]);
    AppendStr(' ');
  end;

  AppendStr(FieldName);
  AppendStr('.');
  AppendStr(CODataFunc[AFn]);
  AppendStr('(');
  OutValue(fieldValue);
  AppendStr(')');
  if ProtectNull then
    AppendStr(')');

end;

// public definitions
//
class function TXEROQueryBuilder.Create: TXEROQueryBuilder;
begin
  Result.Init;
end;

procedure TXEROQueryBuilder.Init;
begin
  FState := bsInit;
  FOpenParen := 0;
  FBuildString := '';
  FCapacity := 0;
  FLen := 0;
end;

procedure TXEROQueryBuilder.AppendStr(const strVal: String);
var
  newLen, newCapacity: integer;
begin
  newLen := FLen + Length(strVal);
  if newLen > FCapacity then
  begin
    newCapacity := FCapacity + 20;
    if newLen > newCapacity then
      newCapacity := newLen + 20;
    SetLength(FBuildString, newCapacity);
    FCapacity := newCapacity;
  end;
  Move(PChar(strVal)^, FBuildString[1 + FLen], ByteLength(strVal));
  FLen := newLen;
end;

// Standard equality/inequality between a field and a value.
procedure TXEROQueryBuilder.Op(FieldName: String; AOp: TXDataOp;
  fieldValue: Variant);
begin
  if FState = bsOk then
    Raise Exception.Create('Logic operator required');
  DoOp(AOp, FieldName, fieldValue);
  FState := bsOk;
end;

// Simple string functions (contains, starts, ends)
procedure TXEROQueryBuilder.Fn(FieldName: String; AFn: TXDataFunc;
  fieldValue: Variant; ProtectNull: Boolean = true);
begin
  if FState = bsOk then
    Raise Exception.Create('Logic operator required');
  DoFn(AFn, FieldName, fieldValue, ProtectNull);
  FState := bsOk;
end;

// And/or/not logic operator
procedure TXEROQueryBuilder.Logic(ALg: TXDataLogic);
begin
  case FState of
    bsInit:
      case ALg of
        xdlAnd, xdlOr:
          Raise Exception.Create('Can''t start with binary logic operator');
      end;
    bsPending:
      case ALg of
        xdlAnd, xdlOr:
          Raise Exception.Create('Already has Logic operator');
      end;
    bsOk:
      case ALg of
        xdlNot:
          Raise Exception.Create('Not operator not applicable here');
      end;
  end;
  case ALg of
    xdlAnd, xdlOr:
      FState := bsPending;
    xdlNot:
      FState := bsPending;
  end;
  AppendStr(' ');
  AppendStr(CODataLogic[ALg]);
  AppendStr(' ');
end;

// Begin parenthesis (grouping terms)
procedure TXEROQueryBuilder.StartParen;
begin
  if FState = bsOk then
    Raise Exception.Create('Logic operator required');
  Inc(FOpenParen);
  AppendStr('(');
  FState := bsPending;
end;

// End parenthesis (grouping terms)
procedure TXEROQueryBuilder.EndParen;
begin
  if FState = bsPending then
    Raise Exception.Create('Expected expression');

  if FOpenParen = 0 then
    Raise Exception.Create('No open parenthesis');
  Dec(FOpenParen);
  AppendStr(')');
  FState := bsOk;
end;

{ TXEROFilter }
procedure TXEROFilter.AddToFilter(AFieldName: string; AData: TDate;
  AQuoteData: Boolean);
begin
  AddToFilter(AFieldName, GetDateTimeFilterString(AData), true);
end;

procedure TXEROFilter.AddGUIDToFilter(AFieldName, AData: string);
begin
  if not IsEmptyString(AData) then
  begin
    AddToFilter(AFieldName, Format('GUID("%s")', [AData]), false);
  end;
end;

function TXEROFilter.GetDateTimeFilterString(ADateTime: TDateTime): string;
var
  Year, Month, Day: Word;
begin
  if ADateTime <> 0 then
  begin
    DecodeDate(ADateTime, Year, Month, Day);
    Result := Format('DateTime(%d,%d,%d)', [Year, Month, Day]);
  end
  else
  begin
    Result := '';
  end;
end;

procedure TXEROFilter.ResetFilter;
begin

end;

function TXEROFilter.Text: string;
begin
  Result := FFilter;
end;

procedure TXEROFilter.AddToFilter(AFieldName, AData: string;
  AQuoteData: Boolean);
var
  Data: string;
begin
  if AQuoteData then
  begin
    Data := '"' + AData + '"';
  end
  else
  begin
    Data := AData;
  end;
  if not IsEmptyString(AData) then
  begin
    if not IsEmptyString(FFilter) then
    begin
      FFilter := FFilter + ' AND ';
    end;
    FFilter := FFilter + AFieldName + '==' + Data;
  end;
end;

procedure TXEROFilter.AddToFilter(AFieldName: string; AData: integer;
  AQuoteData: Boolean);
begin
  AddToFilter(AFieldName, IntToStr(AData), AQuoteData);
end;

procedure TXEROFilter.AddToFilter(AFieldName: string; AData: double;
  AQuoteData: Boolean);
begin
  AddToFilter(AFieldName, FloatToStr(AData), AQuoteData);
end;

{ TXEROAccessToken }

constructor TXEROAccessToken.Create;
begin
  FlastRefresh := 0;
  FexpiresIn := 0;
  FaccessToken := '';
end;

function TXEROAccessToken.ExpiryDateTime: TDateTime;
begin
  Result := IncSecond(FlastRefresh, FexpiresIn);
end;

procedure TXEROAccessToken.FromJSON(AJSON: string);
var
  LJsonResponse: TJSONObject;
begin
  LJsonResponse := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;
  FaccessToken := LJsonResponse.GetValue<string>('access_token');
  FidToken := LJsonResponse.GetValue<string>('id_token');
  FexpiresIn := LJsonResponse.GetValue<integer>('expires_in');
  FtokenType := LJsonResponse.GetValue<string>('token_type');
  Fscope := LJsonResponse.GetValue<string>('scope');
  FrefreshToken := LJsonResponse.GetValue<string>('refresh_token');
  FlastRefresh := now;
end;

function TXEROAccessToken.IsValid: Boolean;
begin
  Result := true;
  if Result then
    Result := not IsEmptyString(FaccessToken);
  if Result then
    Result := ExpiryDateTime > now;
end;

procedure TXEROAccessToken.SetaccessToken(const Value: string);
begin
  FaccessToken := Value;
end;

procedure TXEROAccessToken.SetexpiresIn(const Value: integer);
begin
  FexpiresIn := Value;
end;

procedure TXEROAccessToken.SetidToken(const Value: string);
begin
  FidToken := Value;
end;

procedure TXEROAccessToken.SetlastRefresh(const Value: TDateTime);
begin
  FlastRefresh := Value;
end;

procedure TXEROAccessToken.SetrefreshToken(const Value: string);
begin
  FrefreshToken := Value;
end;

procedure TXEROAccessToken.Setscope(const Value: string);
begin
  Fscope := Value;
end;

procedure TXEROAccessToken.SettokenType(const Value: string);
begin
  FtokenType := Value;
end;

{ TXEROAuthenticatorBase }

constructor TXEROAuthenticatorBase.Create(AOwner: TComponent);
begin
  inherited;
  FXEROAccessToken := TXEROAccessToken.Create;
  FXEROTenants := TXEROTenants.Create;
  FThreadTimer := TThreadTimer.Create(OnThreadTimer, 5000);
  SetDefaultProperties;
end;

destructor TXEROAuthenticatorBase.Destroy;
begin
  try
    FreeAndNil(FXEROAccessToken);
    FreeAndNil(FXEROTenants);
    FThreadTimer.Terminate;
  finally
    inherited;
  end;
end;

procedure TXEROAuthenticatorBase.CheckAccessTokenRefresh;
begin
  try
    if AccessToken.IsValid then
    begin
      if MinutesBetween(now, AccessToken.ExpiryDateTime) < 5 then
      begin
        Refresh;
      end;
    end;
  except
    on E: Exception do
    begin
      Error(E);
    end;
  end;
end;

procedure TXEROAuthenticatorBase.OnThreadTimer(ASender: TObject);
begin
  CheckAccessTokenRefresh;
  InternalTimerEvent;
end;

function TXEROAuthenticatorBase.GetAccessToken: TXEROAccessToken;
begin
  Result := FXEROAccessToken;
end;

function TXEROAuthenticatorBase.GetAuthenticated: Boolean;
begin
  Result := FXEROAccessToken.IsValid;
end;

function TXEROAuthenticatorBase.GetExpiryDate: TDateTime;
begin
  Result := FXEROAccessToken.ExpiryDateTime;
end;

function TXEROAuthenticatorBase.GetXEROTenants: TXEROTenants;
begin
  Result := FXEROTenants;
end;

procedure TXEROAuthenticatorBase.InternalTimerEvent;
begin

end;

procedure TXEROAuthenticatorBase.Login;
begin
  Authenticate;
end;

procedure TXEROAuthenticatorBase.Refresh;
var
  LMessage: string;
begin
  if RefreshAccessToken(LMessage) then
  begin
    Log('Token refreshed');
  end
  else
  begin
    Error(LMessage);
  end;
end;

procedure TXEROAuthenticatorBase.SetDefaultProperties;
begin

end;

procedure TXEROAuthenticatorBase.SetOnAuthenticationComplete
  (const Value: TXEROOnAuthenticationComplete);
begin
  FOnAuthenticationComplete := Value;
end;

function TXEROAuthenticatorBase.RefreshAccessToken(var AMessage
  : string): Boolean;
var
  LParams: TStringList;
  LPostStream: TStringStream;
  LData: string;
begin
  Result := false;
  AMessage := '';
  LParams := TStringList.Create;
  LPostStream := TStringStream.Create;
  try
    LParams.Add('grant_type=refresh_token');
    LParams.Add('&client_id=' + XEROAppDetails.ClientID);
    LParams.Add('&refresh_token=' + AccessToken.RefreshToken);
    try
      Debug('RefreshAccessToken', LParams.Text);
      LPostStream.WriteString(StripCRLLF(LParams.Text));
      HTTPClient.AllowCookies := true;
      HTTPClient.Request.Clear;
      HTTPClient.Request.ContentType := 'application/x-www-form-urlencoded';
      LData := HTTPClient.Post(XERO_API_IDENTITY_URL + 'connect/token',
        LPostStream);
      Debug('RefreshAccessToken', LData);
      AccessToken.FromJSON(LData);
      Result := true;
    except
      on E: EIdHTTPProtocolException do
      begin
        AMessage := E.ErrorMessage;
        Error(E, LData);
      end;
      on E: Exception do
      begin
        AMessage := E.Message;
        Error(E);
      end;
    end;
  finally
    FreeAndNil(LParams);
    FreeAndNil(LPostStream)
  end;
end;

procedure TXEROAuthenticatorBase.RefreshAuthentication;
begin

end;

function TXEROAuthenticatorBase.UpdateTenants(var AMessage: string): Boolean;
var
  LData: string;
begin
  Result := false;
  AMessage := '';
  try
    HTTPClient.AllowCookies := true;
    HTTPClient.Request.Clear;
    SetBearerHeader(AccessToken.AccessToken);
    HTTPClient.Request.ContentType := 'application/json';
    LData := HTTPClient.Get(XERO_API_CONNECTION_URL);
    Debug('UpdateTenants', LData);
    Tenants.FromJSON(LData);
    Result := true;
  except
    on E: EIdHTTPProtocolException do
    begin
      AMessage := E.ErrorMessage;
      Error(E, LData);
    end;
    on E: Exception do
    begin
      AMessage := E.Message;
      Error(E);
    end;
  end;
end;

{ TXEROHTTPClientBase }

function TXEROHTTPClientBase.GetXEROAppDetails: TXEROAppDetails;
begin
  Result := FXEROAppDetails;
end;

function TXEROHTTPClientBase.GetXEROAuthenticator: TXEROAuthenticatorBase;
begin
  Result := nil;
  if Assigned(FXEROAppDetails) then
    Result := FXEROAppDetails.XEROAuthenticator;
end;

function TXEROHTTPClientBase.HTTPClient: TIdHTTP;
var
  handler: TIdSSLIOHandlerSocketOpenSSL;
begin
  if not Assigned(FHTTPClient) then
    FHTTPClient := TIdHTTP.Create(Self);
  if Assigned(FHTTPClient) then
  begin
    FHTTPClient.HandleRedirects := true;
    FHTTPClient.HTTPOptions := [];
  end;
  if not Assigned(FSSLHandler) then
  begin
    handler := TIdSSLIOHandlerSocketOpenSSL.Create(Self);
    FSSLHandler := handler;
    handler.SSLOptions.Method := sslvTLSv1_2;
    FHTTPClient.IOHandler := FSSLHandler;
  end;
  Result := FHTTPClient;
end;

procedure TXEROHTTPClientBase.SetBearerHeader(AAccessToken: string);
begin
  HTTPClient.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' +
    AAccessToken;
end;

procedure TXEROHTTPClientBase.SetXEROAppDetails(const Value: TXEROAppDetails);
begin
  FXEROAppDetails := Value;
end;

{ TXEROTenants }

constructor TXEROTenant.Create;
begin

end;

procedure TXEROTenant.FromJSON(AJSON: string);
var
  LJsonResponse: TJSONObject;
begin
  LJsonResponse := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;
  FID := LJsonResponse.GetValue<string>('id');
  FTenantId := LJsonResponse.GetValue<string>('tenantId');
  FTenantType := LJsonResponse.GetValue<string>('tenantType');
  FtenantName := LJsonResponse.GetValue<string>('tenantName');
  FcreatedDateUtc := LJsonResponse.GetValue<string>('createdDateUtc');
  FupdatedDateUtc := LJsonResponse.GetValue<string>('updatedDateUtc');
end;

procedure TXEROTenant.SetAuthEventId(const Value: string);
begin
  FAuthEventId := Value;
end;

procedure TXEROTenant.SetcreatedDateUtc(const Value: string);
begin
  FcreatedDateUtc := Value;
end;

procedure TXEROTenant.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TXEROTenant.SetTenantId(const Value: string);
begin
  FTenantId := Value;
end;

procedure TXEROTenant.SettenantName(const Value: string);
begin
  FtenantName := Value;
end;

procedure TXEROTenant.SetTenantType(const Value: string);
begin
  FTenantType := Value;
end;

procedure TXEROTenant.SetupdatedDateUtc(const Value: string);
begin
  FupdatedDateUtc := Value;
end;

{ TXEROTenants }

procedure TXEROTenants.FromJSON(AJSON: string);
var
  LArray: TJSONArray;
  LValue: TJSONValue;
  LTenant: TXEROTenant;
begin
  Clear;
  LArray := TJSONObject.ParseJSONValue(AJSON) as TJSONArray;
  for LValue in LArray do
  begin
    LTenant := TXEROTenant.Create;
    LTenant.FromJSON(LValue.ToJSON);
    Add(LTenant);
  end;
end;

end.
