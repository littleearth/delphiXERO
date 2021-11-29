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
    FUnitDP4: Boolean;
    procedure SetClientID(const Value: string);
    procedure SetXEROAuthenticatorBase(const Value: TXEROAuthenticatorBase);
    procedure SetUnitDP4(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    function ValidateSettings: Boolean; virtual;
  published
    property AppName: string read FAppName write FAppName;
    property ClientID: string read FClientID write SetClientID;
    property XEROAuthenticator: TXEROAuthenticatorBase read FXEROAuthenticator
      write SetXEROAuthenticatorBase;
    property UnitDP4: Boolean read FUnitDP4 write SetUnitDP4;
  end;

  TXEROHTTPClientBase = class(TXEROComponent)
  private
    FXEROAppDetails: TXEROAppDetails;
    FSSLHandler: TIdIOHandler;
    FHTTPClient: TIdHTTP;
  protected
    procedure SetBearerHeader(AAccessToken: string);
    procedure SetBasicHeader(AClientSecret: string);
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
    function GetBusy: Boolean; virtual;
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
    property Busy: Boolean read GetBusy;
    property Expirydate: TDateTime read GetExpiryDate;
    property OnAuthenticationComplete: TXEROOnAuthenticationComplete
      read FOnAuthenticationComplete write SetOnAuthenticationComplete;
  end;

  TXEROHTTPResponse = class(TXEROHTTPClientBase)
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

  TXEROHTTPResponseClass = class of TXEROHTTPResponse;

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
    FsummarizeErrors: Boolean;
    procedure SetTenantId(const Value: string);
    procedure SetsummarizeErrors(const Value: Boolean);
  protected
    function NormalisedURL(const AURL: string): string;
    function GetGUIDString: string;
    procedure ValidateSettings; virtual;
    function StreamToString(AStream: TStream): string;
    procedure StringToStream(AStream: TStream; AValue: string);
    function GetErrorResponse(AResponseCode: integer; AErrorDetail: string;
      AResponse: TStream = nil; AResponseType: TResponseType = rtXML): string;
    procedure SetResponseStream(var AHTTPResult: Boolean;
      AOutputStream: TStream; AHTTPResponse: TIdHTTPResponse;
      AResponseType: TResponseType); overload;
    procedure SetResponseStream(var AHTTPResult: Boolean;
      AOutputStream: TStream; AException: Exception; AResponseCode: integer;
      AResponseType: TResponseType); overload;
    function ParamsToURL(AURL: string; AParams: TStrings = nil): string;
    procedure ParamsToTStrings(AInput: String; AOutput: TStrings);
    function CreateParams(AParams: string): TStrings;

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
    function Post<T: TXEROHTTPResponse>(AURL: String; ARequest: string;
      AResponse: T; AParams: string = ''): Boolean; overload;

    function Put(AURL: String; AParams: TStrings; ARequest: TStream;
      AResponse: TStream; var ResponseCode: integer; var ErrorDetail: String;
      AResponseType: TResponseType = rtXML): Boolean; overload;
    function Put(AURL: String; AParams: string; ARequest: string;
      var AResponse: string; AResponseType: TResponseType = rtXML)
      : Boolean; overload;
    function Put<T: TXEROHTTPResponse>(AURL: String; ARequest: string;
      AResponse: T; AParams: string = ''): Boolean; overload;

    function Delete(AURL: String; AResponse: TStream; var ResponseCode: integer;
      var ErrorDetail: String; AResponseType: TResponseType = rtXML)
      : Boolean; overload;
    function Delete(AURL: String; var AResponse: string;
      AResponseType: TResponseType = rtXML): Boolean; overload;
    function Delete<T: TXEROHTTPResponse>(AURL: String; AResponse: T)
      : Boolean; overload;

    function GetFilterURL(AURL, AFilter, AOrderBy: string;
      APage: integer): string;

    function Find<T: TXEROHTTPResponse>(AURL: string; AResponse: T;
      AFilter: string = ''; AOrderBy: string = ''; APage: integer = 0;
      ALastModified: TDateTime = 0; AParams: string = ''): Boolean;
    property summarizeErrors: Boolean read FsummarizeErrors
      write SetsummarizeErrors;
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
    function GetAPIURLObjectID(AURLObjectID: string): string;
    function Find<T: TXEROHTTPResponse>(AResponse: T; AFilter: string = '';
      AOrderBy: string = ''; APage: integer = 0; ALastModified: TDateTime = 0;
      AParams: string = ''): Boolean; reintroduce;
    function Post<T: TXEROHTTPResponse>(ARequest: string; AResponse: T;
      AParams: string = ''; AURLObjectID: string = ''): Boolean; reintroduce;
    function Put<T: TXEROHTTPResponse>(ARequest: string; AResponse: T;
      AParams: string = ''; AURLObjectID: string = ''): Boolean; reintroduce;
    function Delete<T: TXEROHTTPResponse>(AResponse: T; AParams: string = '';
      AURLObjectID: string = ''): Boolean; reintroduce;
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
  System.NetEncoding,
  // Indy
  IdGlobal, IdHMACSHA1, IdSSL, IdSSLOpenSSL,
  IdBaseComponent,
  IdIOHandlerStack,

  // XERO
  XERO.Utils, XERO.VarUtil;

// TXEROAppDetails

constructor TXEROAppDetails.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUnitDP4 := false;
end;

procedure TXEROAppDetails.SetClientID(const Value: string);
begin
  FClientID := Value;
end;

procedure TXEROAppDetails.SetUnitDP4(const Value: Boolean);
begin
  FUnitDP4 := Value;
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

procedure TXEROAPIBase.SetTenantId(const Value: string);
begin
  FTenantId := Value;
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

function TXEROAPIBase.StreamToString(AStream: TStream): string;
var
  sr: TStreamReader;
begin
  AStream.Position := 0;
  sr := TStreamReader.Create(AStream, TEncoding.UTF8);
  try
    Result := sr.ReadToEnd;
  finally
    sr.Free;
  end;
end;

procedure TXEROAPIBase.StringToStream(AStream: TStream; AValue: string);
var
  LStringStream: TStringStream;
begin
  LStringStream := TStringStream.Create(AValue, TEncoding.UTF8);
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

function TXEROAPIBase.GetErrorResponse(AResponseCode: integer;
  AErrorDetail: string; AResponse: TStream;
  AResponseType: TResponseType): string;
var
  LMessage: TStringList;
  LErrorMessage: string;
  LErrorDetails: string;
begin
  LMessage := TStringList.Create;
  try
    LErrorMessage := AErrorDetail;
    LErrorDetails := '';
    if Assigned(AResponse) then
    begin
      LErrorDetails := StreamToString(AResponse);
    end;

    case AResponseType of
      rtJSON:
        begin
          LMessage.Add('{');
          LMessage.Add(Format('"ResponseCode" : %d,', [AResponseCode]));
          if not IsEmptyString(LErrorDetails) then
          begin
            case AResponseCode of
              400:
                begin
                  LMessage.Add('"ApiException" : ' + LErrorDetails + ',');
                end;
            else
              begin
                LErrorMessage := LErrorMessage + ' ' + LErrorDetails;
              end;
            end;
          end;
          LMessage.Add(Format('"ErrorMessage" : %s',
            [AnsiQuotedStr(LErrorMessage, '"')]));
          LMessage.Add('}');
        end;
      rtXML:
        begin
          LMessage.Add('<?xml version=''1.0'' encoding=''utf-8''?>');
          LMessage.Add('<Error>');
          LMessage.Add(Format('<ErrorCode>%d</ErrorCode>', [AResponseCode]));
          LMessage.Add('<Description>');
          LMessage.Add('![CDATA[' + LErrorMessage + ']]');
          LMessage.Add('</Description>');
          LMessage.Add('<Details>');
          LMessage.Add('![CDATA[' + LErrorDetails + ']]');
          LMessage.Add('</Details>');
          LMessage.Add('</Error>');
        end;
    end;
    Result := LMessage.Text;
  finally
    FreeAndNil(LMessage);
  end;
  Debug('GetErrorResponse', Result);
end;

procedure TXEROAPIBase.SetResponseStream(var AHTTPResult: Boolean;
  AOutputStream: TStream; AHTTPResponse: TIdHTTPResponse;
  AResponseType: TResponseType);
var
  LError: string;
begin
  AHTTPResult := false;
  case AHTTPResponse.ResponseCode of
    200 .. 299:
      begin
        AOutputStream.CopyFrom(AHTTPResponse.ContentStream, 0);
        AHTTPResult := true;
      end;
  else
    begin
      LError := GetErrorResponse(AHTTPResponse.ResponseCode,
        AHTTPResponse.ResponseText, AHTTPResponse.ContentStream, AResponseType);
      StringToStream(AOutputStream, LError);
    end;
  end;
end;

procedure TXEROAPIBase.SetResponseStream(var AHTTPResult: Boolean;
  AOutputStream: TStream; AException: Exception; AResponseCode: integer;
  AResponseType: TResponseType);
var
  LError: string;
begin
  AHTTPResult := false;
  LError := GetErrorResponse(AResponseCode, AException.Message, nil,
    AResponseType);
  StringToStream(AOutputStream, LError);
end;

procedure TXEROAPIBase.SetsummarizeErrors(const Value: Boolean);
begin
  FsummarizeErrors := Value;
end;

procedure TXEROAPIBase.ParamsToTStrings(AInput: String; AOutput: TStrings);
var
  LParams: String;
begin
  if Assigned(AOutput) then
  begin
    LParams := AInput;
    LParams := StringReplace(LParams, ';', #13#10,
      [rfReplaceAll, rfIgnoreCase]);
    AOutput.Text := LParams;
  end;
end;

function TXEROAPIBase.ParamsToURL(AURL: string; AParams: TStrings): string;
var
  LParams: string;
begin
  Result := AURL;
  LParams := '';

  if Assigned(AParams) then
  begin
    LParams := AParams.Text;
  end;

  if XEROAppDetails.UnitDP4 then
  begin
    if not IsEmptyString(LParams) then
      LParams := LParams + #13#10;
    LParams := LParams + 'unitdp=4';
  end;

  if not summarizeErrors then
  begin
    if not IsEmptyString(LParams) then
      LParams := LParams + #13#10;
    LParams := LParams + 'summarizeErrors=false';
  end;

  LParams := StringReplace(LParams, #13, '', [rfReplaceAll, rfIgnoreCase]);
  LParams := StringReplace(LParams, #10, '&', [rfReplaceAll, rfIgnoreCase]);
  if LParams.EndsWith('&') then
  begin
    LParams := Copy(LParams, 1, Length(LParams) - 1);
  end;

  if not IsEmptyString(LParams) then
  begin
    Result := Result + GetURLSeperator(Result) + LParams;
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

function TXEROAPIBase.Get(AURL: string; AParams: string; var AResponse: string;
  ALastModified: TDateTime = 0; AResponseType: TResponseType = rtXML): Boolean;
var
  LResponseStream: TStringStream;
  LParams: TStrings;
  LResponseCode: integer;
  LErrorDetail: string;
begin
  LResponseStream := TStringStream.Create('', TEncoding.UTF8);
  LParams := CreateParams(AParams);
  try
    Result := Get(AURL, LParams, LResponseStream, LResponseCode, LErrorDetail,
      ALastModified, AResponseType);
    AResponse := LResponseStream.DataString;
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
  LResponse: TMemoryStream;
begin
  Result := true;
  LResponse := TMemoryStream.Create;
  try
    try
      ValidateSettings;
      LURL := ParamsToURL(AURL, AParams);

      case AResponseType of
        rtXML:
          begin
          end;
        rtJSON:
          begin
            HTTPClient.Request.Accept := 'application/json';
            HTTPClient.Request.ContentType := 'application/json;charset=utf-8';
          end;
      end;

      if ALastModified <> 0 then
      begin
        HTTPClient.Request.LastModified := ALastModified;
      end;

      SetHTTPHeader;
      Log(Format('URL: %s, Headers: %s, Accept: %s, Last Modified %s',
        [LURL, HTTPClient.Request.CustomHeaders.Text, HTTPClient.Request.Accept,
        DateTimeToStr(HTTPClient.Request.LastModified)]));
      HTTPClient.Get(LURL, LResponse);
      ErrorDetail := StreamToString(HTTPClient.Response.ContentStream);
      ResponseCode := HTTPClient.Response.ResponseCode;
      ErrorDetail := HTTPClient.Response.ResponseText;
      SetResponseStream(Result, AResponse, HTTPClient.Response, AResponseType);
    except
      on E: Exception do
      begin
        ResponseCode := 10000;
        ErrorDetail := E.Message;
        SetResponseStream(Result, AResponse, E, ResponseCode, AResponseType);
      end;
    end;
  finally
    FreeAndNil(LResponse);
  end;

  Debug('Get', 'Result: ' + BoolToStr(Result, true) + ', Response: ' +
    StreamToString(AResponse));

end;

function TXEROAPIBase.Post(AURL: String; AParams: TStrings; ARequest: TStream;
  AResponse: TStream; var ResponseCode: integer; var ErrorDetail: String;
  AResponseType: TResponseType): Boolean;
var
  LURL: string;
  LResponse: TMemoryStream;
begin
  LResponse := TMemoryStream.Create;
  try
    try
      ValidateSettings;
      LURL := ParamsToURL(AURL, AParams);

      HTTPClient.Request.ContentType := 'text/xml;charset=UTF-8';
      case AResponseType of
        rtXML:
          ;
        rtJSON:
          begin
            HTTPClient.Request.Accept := 'application/json';
            HTTPClient.Request.ContentType := 'application/json;charset=utf-8';
          end;
      end;

      SetHTTPHeader;
      Log(Format('URL: %s, Headers: %s, Accept: %s',
        [LURL, HTTPClient.Request.CustomHeaders.Text,
        HTTPClient.Request.Accept]));
      HTTPClient.Post(LURL, ARequest, LResponse);
      ResponseCode := HTTPClient.Response.ResponseCode;
      ErrorDetail := HTTPClient.Response.ResponseText;
      SetResponseStream(Result, AResponse, HTTPClient.Response, AResponseType);
    except

      on E: Exception do
      begin
        ResponseCode := 10000;
        ErrorDetail := E.Message;
        SetResponseStream(Result, AResponse, E, ResponseCode, AResponseType);
      end;
    end;
  finally
    FreeAndNil(LResponse);
  end;

  Debug('Post', 'Result: ' + BoolToStr(Result, true) + ', Response: ' +
    StreamToString(AResponse));
end;

function TXEROAPIBase.Post(AURL, AParams, ARequest: string;
  var AResponse: string; AResponseType: TResponseType): Boolean;
var
  LRequestStream: TStringStream;
  LResponseStream: TStringStream;
  LParams: TStrings;
  LResponseCode: integer;
  LErrorDetail: string;
begin
  LResponseStream := TStringStream.Create('', TEncoding.UTF8);
  LRequestStream := TStringStream.Create(ARequest, TEncoding.UTF8);
  LParams := CreateParams(AParams);
  try
    Debug('Post', 'Request: ' + LRequestStream.DataString);
    Result := Post(AURL, LParams, LRequestStream, LResponseStream,
      LResponseCode, LErrorDetail, AResponseType);
    AResponse := LResponseStream.DataString;
  finally
    FreeAndNil(LRequestStream);
    FreeAndNil(LResponseStream);
    FreeAndNil(LParams);
  end;
end;

function TXEROAPIBase.Post<T>(AURL, ARequest: string; AResponse: T;
  AParams: string): Boolean;
var
  LErrorMessage: string;
  LResponseCode: integer;
  LParams: TStringList;
  LRequest: TStringStream;
begin
  LParams := TStringList.Create;
  LRequest := TStringStream.Create(ARequest, TEncoding.UTF8);
  try
    if not Assigned(AResponse) then
      AResponse := T.Create(nil);

    LParams.Text := AParams;

    Debug('Post', 'Request: ' + ARequest);

    Result := Post(AURL, LParams, LRequest, AResponse.Stream, LResponseCode,
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
  LResponse: TMemoryStream;
begin
  Result := false;
  LResponse := TMemoryStream.Create;
  try
    try
      ValidateSettings;
      LURL := ParamsToURL(AURL, AParams);
      HTTPClient.Request.ContentType := 'application/x-www-form-urlencoded';
      case AResponseType of
        rtXML:
          ;
        rtJSON:
          begin
            HTTPClient.Request.Accept := 'application/json';
            HTTPClient.Request.ContentType := 'application/json;charset=utf-8';
          end;
      end;

      SetHTTPHeader;
      Log(Format('URL: %s, Headers: %s, Accept: %s',
        [LURL, HTTPClient.Request.CustomHeaders.Text,
        HTTPClient.Request.Accept]));
      HTTPClient.Put(LURL, ARequest, LResponse);
      ResponseCode := HTTPClient.Response.ResponseCode;
      ErrorDetail := HTTPClient.Response.ResponseText;
      SetResponseStream(Result, AResponse, HTTPClient.Response, AResponseType);

    except
      on E: Exception do
      begin
        ResponseCode := 10000;
        ErrorDetail := E.Message;
        SetResponseStream(Result, AResponse, E, ResponseCode, AResponseType);
      end;
    end;
  finally
    FreeAndNil(LResponse);
  end;

  Debug('Put', 'Result: ' + BoolToStr(Result, true) + ', Response: ' +
    StreamToString(AResponse));
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
  LResponseStream := TStringStream.Create('', TEncoding.UTF8);
  LRequestStream := TStringStream.Create(ARequest, TEncoding.UTF8);
  LParams := TStringList.Create;
  try
    Debug('Put', 'Request: ' + LRequestStream.DataString);
    LParams.Text := AParams;
    Result := Put(AURL, LParams, LRequestStream, LResponseStream, LResponseCode,
      LErrorDetail, AResponseType);
    AResponse := LResponseStream.DataString;
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
  LParams: TStrings;
  LRequest: TStringStream;
begin
  LParams := CreateParams(AParams);
  LRequest := TStringStream.Create(ARequest, TEncoding.UTF8);
  try
    if not Assigned(AResponse) then
      AResponse := T.Create(nil);

    Debug('Put', 'Request: ' + ARequest);

    Result := Put(AURL, LParams, LRequest, AResponse.Stream, LResponseCode,
      LErrorMessage, AResponse.ResponseType);

    AResponse.SetResponse(Result, LResponseCode, LErrorMessage);
  finally
    FreeAndNil(LParams);
    FreeAndNil(LRequest);
  end;
end;

function TXEROAPIBase.Delete(AURL: String; AResponse: TStream;
  var ResponseCode: integer; var ErrorDetail: String;
  AResponseType: TResponseType): Boolean;
var
  LURL: string;
  LResponse: TMemoryStream;
begin
  LResponse := TMemoryStream.Create;
  try
    try
      ValidateSettings;
      HTTPClient.Request.ContentType := 'application/x-www-form-urlencoded';
      case AResponseType of
        rtXML:
          ;
        rtJSON:
          HTTPClient.Request.Accept := 'application/json';
      end;

      LURL := ParamsToURL(AURL, nil);
      SetHTTPHeader;
      Log(Format('URL: %s, Headers: %s, Accept: %s',
        [LURL, HTTPClient.Request.CustomHeaders.Text,
        HTTPClient.Request.Accept]));
      HTTPClient.Delete(LURL, LResponse);
      ResponseCode := HTTPClient.Response.ResponseCode;
      ErrorDetail := HTTPClient.Response.ResponseText;
      SetResponseStream(Result, AResponse, HTTPClient.Response, AResponseType);

    except
      on E: Exception do
      begin
        ResponseCode := 10000;
        ErrorDetail := E.Message;
        SetResponseStream(Result, AResponse, E, ResponseCode, AResponseType);
      end;
    end;
  finally
    FreeAndNil(LResponse);
  end;

  Debug('Delete', 'Result: ' + BoolToStr(Result, true) + ', Response: ' +
    StreamToString(AResponse));
end;

function TXEROAPIBase.CreateParams(AParams: string): TStrings;
begin
  Result := TStringList.Create;
  ParamsToTStrings(AParams, Result);
end;

function TXEROAPIBase.Delete(AURL: String; var AResponse: string;
  AResponseType: TResponseType): Boolean;
var
  LResponseStream: TStringStream;
  LParams: TStringList;
  LResponseCode: integer;
  LErrorDetail: string;
begin
  LResponseStream := TStringStream.Create('', TEncoding.UTF8);
  LParams := TStringList.Create;
  try
    Result := Delete(AURL, LResponseStream, LResponseCode, LErrorDetail,
      AResponseType);
    AResponse := LResponseStream.DataString;
  finally
    FreeAndNil(LResponseStream);
    FreeAndNil(LParams);
  end;
end;

function TXEROAPIBase.Delete<T>(AURL: String; AResponse: T): Boolean;
var
  LErrorMessage: string;
  LResponseCode: integer;
begin
  if not Assigned(AResponse) then
    AResponse := T.Create(nil);
  Result := Delete(AURL, AResponse.Stream, LResponseCode, LErrorMessage,
    AResponse.ResponseType);

  AResponse.SetResponse(Result, LResponseCode, LErrorMessage);

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
  AFilter, AOrderBy: string; APage: integer; ALastModified: TDateTime;
  AParams: string): Boolean;
var
  LURL: string;
  LErrorMessage: string;
  LResponseCode: integer;
  LParams: TStrings;
begin
  LParams := CreateParams(AParams);
  try
    if not Assigned(AResponse) then
      AResponse := T.Create(nil);

    LURL := GetFilterURL(AURL, AFilter, AOrderBy, APage);

    Result := Get(LURL, LParams, AResponse.Stream, LResponseCode, LErrorMessage,
      ALastModified, AResponse.ResponseType);

    AResponse.SetResponse(Result, LResponseCode, LErrorMessage);
  finally
    FreeAndNil(LParams);
  end;

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
  FsummarizeErrors := true;
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



// TXEROHTTPResponse

function TXEROHTTPResponse.rStream: TStream;
begin
  Result := FResponse;
end;

procedure TXEROHTTPResponse.ClearStream;
begin
  FResponse.Clear;
end;

procedure TXEROHTTPResponse.SetResponse(AResult: Boolean; ACode: integer;
  ADetail: String);
begin
  FResult := AResult;
  FResponseCode := ACode;
  FErrorMessage := ADetail;
end;

function TXEROHTTPResponse.AsString: string;
var
  sr: TStreamReader;
begin
  FResponse.Position := 0;
  sr := TStreamReader.Create(FResponse, TEncoding.UTF8);
  try
    Result := sr.ReadToEnd;
  finally
    sr.Free;
  end;
end;

function TXEROHTTPResponse.GetDefaultResponseType: TResponseType;
begin
  Result := rtXML;
end;

procedure TXEROHTTPResponse.AfterConstruction;
begin
  inherited;

  FResponseType := GetDefaultResponseType;
  FResponse := TMemoryStream.Create;
  FResult := false;
  FErrorMessage := '';
  FResponseCode := -1;
end;

Procedure TXEROHTTPResponse.BeforeDestruction;
begin
  FreeAndNil(FResponse);
  inherited;
end;

function TXEROAPI.Find<T>(AResponse: T; AFilter, AOrderBy: string;
  APage: integer; ALastModified: TDateTime; AParams: string): Boolean;
begin
  Result := inherited Find<T>(GetAPIURL, AResponse, AFilter, AOrderBy, APage,
    ALastModified, AParams);
end;

function TXEROAPI.GetAPIURLObjectID(AURLObjectID: string): string;
begin
  Result := GetAPIURL;
  if not IsEmptyString(AURLObjectID) then
  begin
    if not Result.EndsWith('/') then
      Result := Result + '/';
    Result := Result + AURLObjectID;
  end;
end;

function TXEROAPI.Post<T>(ARequest: string; AResponse: T;
  AParams, AURLObjectID: string): Boolean;
begin
  Result := inherited Post<T>(GetAPIURLObjectID(AURLObjectID), ARequest,
    AResponse, AParams);
end;

function TXEROAPI.Put<T>(ARequest: string; AResponse: T;
  AParams, AURLObjectID: string): Boolean;
begin
  Result := inherited Put<T>(GetAPIURLObjectID(AURLObjectID), ARequest,
    AResponse, AParams);
end;

// TXEROAPI

function TXEROAPI.Delete<T>(AResponse: T;
  AParams, AURLObjectID: string): Boolean;
begin
  Result := inherited Delete<T>(GetAPIURLObjectID(AURLObjectID), AResponse);
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
  LJsonResponse.TryGetValue<string>('refresh_token', FrefreshToken);
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
    if (Assigned(AccessToken) and (AccessToken.IsValid)) then
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
  if Assigned(FXEROAccessToken) then
  begin
    CheckAccessTokenRefresh;
    InternalTimerEvent;
  end;
end;

function TXEROAuthenticatorBase.GetAccessToken: TXEROAccessToken;
begin
  Result := FXEROAccessToken;
end;

function TXEROAuthenticatorBase.GetAuthenticated: Boolean;
begin
  Result := (Assigned(FXEROAccessToken) and (FXEROAccessToken.IsValid));
end;

function TXEROAuthenticatorBase.GetBusy: Boolean;
begin
  Result := false;
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
  if Assigned(AccessToken) and (not IsEmptyString(AccessToken.RefreshToken))
  then
  begin
    AMessage := '';
    LParams := TStringList.Create;
    LPostStream := TStringStream.Create('', TEncoding.UTF8);
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
  end
  else
  begin
    AMessage := 'Session cannot be refreshed as no refresh token is available';
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
    HTTPClient.Request.ContentType := 'application/json;charset=utf-8';
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
    // FHTTPClient.HTTPOptions := [];
    FHTTPClient.HTTPOptions := [hoForceEncodeParams, hoNoProtocolErrorException,
      hoWantProtocolErrorContent];
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

procedure TXEROHTTPClientBase.SetBasicHeader(AClientSecret: string);
var
  LBase64: string;
begin
  LBase64 := TNetEncoding.Base64.Encode(FXEROAppDetails.ClientID + ':' +
    AClientSecret);
  LBase64 := StripCRLLF(LBase64);
  HTTPClient.Request.CustomHeaders.Values['Authorization'] := 'Basic '
    + LBase64;
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
