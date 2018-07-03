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

  ----------------------------------------------------------------------------- }

unit XERO.API;

interface

uses
  // System
  System.SysUtils, System.Classes, System.Variants,

  // Indy
  IdHTTP, IdIOHandler,

  // XERO
  XERO.CipherRSA;

const
  XERO_API_BASE_URL = 'https://api.xero.com/api.xro/2.0/';

type
  EXEROException = class(Exception);
  TOAuthSignatureMethod = (oaHMAC, oaRSA);
  TLogLevel = (logDebug, logInformation);
  TResponseType = (rtXML, rtJSON);

type
  TOnLog = procedure(ASender: TObject; AMessage: string) of object;

type
  TXEROAppDetails = class(TComponent)
  private
    FOAuthSignatureMethod: TOAuthSignatureMethod;
    FAppName: string;
    FPrivateKey: TStringList;
    FPublicKey: TStringList;
    FConsumerKey: string;
    FConsumerSecret: string;
  protected
    function GetPivateKey: TStrings;
    function GetPublicKey: TStrings;
    procedure SetPrivateKey(AStrings: TStrings);
    procedure SetPublicKey(AStrings: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ValidateSettings: Boolean; virtual;
  published
    property AppName: string read FAppName write FAppName;
    property PrivateKey: TStrings read GetPivateKey write SetPrivateKey;
    property PublicKey: TStrings read GetPublicKey write SetPublicKey;
    property ConsumerKey: string read FConsumerKey write FConsumerKey;
    property ConsumerSecret: string read FConsumerSecret write FConsumerSecret;
    property OAuthSignatureMethod: TOAuthSignatureMethod
      read FOAuthSignatureMethod write FOAuthSignatureMethod;
  end;

type
  TXEROAPIBase = class(TComponent)
  private
    FLogLevel: TLogLevel;
    FOnLog: TOnLog;
    FXEROAppDetails: TXEROAppDetails;
    FSSLHandler : TIdIOHandler;
    FHTTPClient : TIdHTTP;
  protected
    procedure Log(AMessage: string);
    function HasLog(Level : TLogLevel) : boolean;
    procedure Debug(AProcedure: string; AMessage: string);
    procedure Error(AMessage: string); overload;
    procedure Error(AException: Exception); overload;
    procedure Warning(AMessage: string);
    function NormalisedURL(const AURL: string): string;
    function GetGUIDString: string;
    function OAuthBaseString(const aMethod: string; const AURL: string;
      const AParams: TStrings): string;
    function OAuthEncode(const aString: string): string;
    function OAuthSignature(const aBaseString: string;
      const aKey: TRSAPrivateKey): string; overload;
    function OAuthSignature(const aBaseString: string; const aKey: string)
      : string; overload;
    function OAuthSignature(const aBaseString: string;
      const aConsumerSecret: string; const aTokenSecret: string)
      : string; overload;
    function OAuthTimeStamp: string;
    procedure OAuthSignRequest(const aRequest: TIdHTTPRequest;
      const aMethod: string; const AURL: string; const AParams: TStrings;
      const AConsumerKey: string; const AToken: string;
      const aConsumerSecret: string; const aTokenSecret: string); overload;
    procedure OAuthSignRequest(const aRequest: TIdHTTPRequest;
      const aMethod: string; const AURL: string; const AParams: TStrings;
      const AConsumerKey: string; const AToken: string;
      const APrivateKey: string); overload;
    procedure ValidateSettings; virtual;

    function Get(AURL: string; AParams: string; var AResponse: string;
      ALastModified: TDateTime = 0;
      AResponseType: TResponseType = rtXML): Boolean; overload;

    function Get(AURL: string; AParams: TStrings; AResponse: TStream;
      var ResponseCode : integer; var ErrorDetail : string;
      ALastModified: TDateTime = 0;
      AResponseType: TResponseType = rtXML): Boolean; overload;

    function Post(AURL :String; AParams : TStrings; AResponse : TStream;
      var ResponseCode : Integer; var ErrorDetail : String;
      AResponseType : TResponseType = rtXML): boolean;

    procedure SetXEROAppDetails(AXEROAppDetails: TXEROAppDetails);
    function GetXEROAppDetails: TXEROAppDetails;

    function HTTPClient : TIdHTTP;
  public
    constructor Create(AOwner: TComponent); override;

    Procedure BeforeDestruction; override;

  published
    property LogLevel: TLogLevel read FLogLevel write FLogLevel;
    property XEROAppDetails: TXEROAppDetails read GetXEROAppDetails
      write SetXEROAppDetails;
    property OnLog: TOnLog read FOnLog write FOnLog;
  end;

type
  TXEROResponseBase = class(TComponent)
  private
    FResponse: TMemoryStream;
    FResponseCode : integer;
    FResult: Boolean;
    FErrorMessage: string;
    FResponseType: TResponseType;
  protected
    function GetDefaultResponseType: TResponseType; virtual;

    procedure SetResponse( AResult :boolean; ACode : Integer; ADetail : String); virtual;

    property ResponseType: TResponseType read FResponseType write FResponseType;
    function rStream: TStream;
  public
    procedure AfterConstruction; override;
    Procedure BeforeDestruction; override;

    procedure ClearStream;

    property Stream : TStream read rStream;

    function AsString: string;
    property Result: Boolean read FResult;
    property ResponseCode: integer read FResponseCode;
    property ErrorMessage: string read FErrorMessage;
  end;

type
  TXEROResponseBaseClass = class of TXEROResponseBase;

type
  TXEROAPI = class(TXEROAPIBase)
  private
    FXEROResponseBase: TXEROResponseBase;
  protected
    function GetAPIURL: string; virtual; abstract;
    procedure ValidateSettings; override;
    function GetDateTimeFilterString(ADateTime: TDateTime): string;
    procedure SetXEROResponseBase(AXEROResponseBase: TXEROResponseBase);
    function GetXEROResponseBase: TXEROResponseBase;
  public
    function Find(AFilter: string = ''; AOrderBy: string = '';
      APage: integer = 0; ALastModified: TDateTime = 0): Boolean;
  published
    property Response: TXEROResponseBase read GetXEROResponseBase
      write SetXEROResponseBase;

  end;

TXDataOp =
  ( xdoEqual, xdoLess, xdoGreater, xdoLessEqual, xdoGreaterEqual, xdoNotEqual);
TXDataFunc =
  ( xdfStarts, xdfEnds, xdfContains);
TXDataLogic =
  ( xdlAnd, xdlOr, xdlNot);

TBuilderState = (bsInit, bsOk, bsPending);

{: Create a XERO 'DotNet' query string.
  @Example
    qb := TXEROQueryBuilder.Create;
    qb.Op(xdoEqual, 'URL', 'http://place.com/url');
    qb.Logic(xdlOr);
    qb.Fn(xdfContains, 'Narration', 'Friend');
}
TXEROQueryBuilder = record
private

  FBuildString : String;
  FLen, FCapacity : integer;

  FOpenParen : Integer;
  FState : TBuilderState;
  procedure AppendStr( const strVal : String);

  procedure OutValue( fieldValue : Variant; ForceString: Boolean = false);
  function GetString : String;

  procedure DoOp(AOp : TXDataOp;  FieldName : String; fieldValue : Variant);
  procedure DoFn(AFn : TXDataFunc; FieldName : String; FieldValue : Variant; ProtectNull : boolean);
public
  class function Create : TXEROQueryBuilder; static;
  procedure Init;

  // Standard equality/inequality between a field and a value.
  procedure Op( FieldName : String; AOp : TXDataOp; fieldValue : Variant);

  { Simple string functions (contains, starts, ends)

    With 'protectNull == true'  (the default) this surrounds the expression with ( Fieldname != null and <expression> ).
    This is because without this protection, the query will tend to inexplicably crash.
    For truly mandatory fields, it could be omitted, but it's safer to include it as the default.
  }
  procedure Fn(FieldName : String; AFn : TXDataFunc; FieldValue : Variant; ProtectNull : boolean = true);

  // And/or/not logic operator
  procedure Logic( ALg : TXDataLogic);

  // Begin parenthesis (grouping terms)
  procedure StartParen;
  // End parenthesis (grouping terms)
  procedure EndParen;

  // Retrieve the value as a string for an OData query.
  property AsString : String read GetString;
end;


implementation

uses
  // System
  Windows, System.DateUtils, System.StrUtils,

  // Indy
  IdGlobal, IdHMACSHA1, IdURI, IdSSL, IdSSLOpenSSL,
  IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdIOHandlerSocket, IdIOHandlerStack,

  // XERO
  XERO.HugeInt, XERO.Base64, XERO.RSAUtils, XERO.Utils,
  XERO.MiscUtil, XERO.VarUtil;

// TXEROAppDetails

function TXEROAppDetails.GetPivateKey: TStrings;
begin
  Result := FPrivateKey;
end;

function TXEROAppDetails.GetPublicKey: TStrings;
begin
  Result := FPublicKey;
end;

procedure TXEROAppDetails.SetPrivateKey(AStrings: TStrings);
begin
  FPrivateKey.Assign(AStrings);
end;

procedure TXEROAppDetails.SetPublicKey(AStrings: TStrings);
begin
  FPublicKey.Assign(AStrings);
end;

constructor TXEROAppDetails.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrivateKey := TStringList.Create;
  FPublicKey := TStringList.Create;
  FOAuthSignatureMethod := oaRSA;
end;

destructor TXEROAppDetails.Destroy;
begin
  FreeAndNil(FPrivateKey);
  FreeAndNil(FPublicKey);
  inherited Destroy;
end;

function TXEROAppDetails.ValidateSettings: Boolean;
begin
  Result := true;
  if Result then
    Result := not IsEmptyString(FPrivateKey.Text);
  if Result then
    Result := not IsEmptyString(FConsumerKey);
  if Result then
    Result := not IsEmptyString(FConsumerSecret);
end;

// TXEROAPIBase
//
Procedure TXEROAPIBase.BeforeDestruction;
begin
  if assigned(FHTTPClient) then
    FHTTPClient.IOHandler := nil;
  FreeAndNil(FSSLHandler);
  FreeAndNil(FHTTPClient);
  inherited;
end;

procedure TXEROAPIBase.Log(AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, AMessage);
end;

function TXEROAPIBase.HasLog(Level : TLogLevel) : boolean;
begin
  result := assigned(FOnLog) and (ord(level) >= ord(FLogLevel));
end;

procedure TXEROAPIBase.Debug(AProcedure: string; AMessage: string);
begin
  if FLogLevel = logDebug then
  begin
    Log(Format('DEBUG: [%s] %s', [AProcedure, AMessage]));
  end;
end;

procedure TXEROAPIBase.Error(AMessage: string);
begin
  Log(Format('ERROR: %s', [AMessage]));
end;

procedure TXEROAPIBase.Error(AException: Exception);
begin
  Error(AException.Message);
end;

procedure TXEROAPIBase.Warning(AMessage: string);
begin
  Log(Format('WARNING: %s', [AMessage]));
end;

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
function TXEROAPIBase.OAuthBaseString(const aMethod: string; const AURL: string;
  const AParams: TStrings): string;

var
  i : integer;
  params: TStringList;
  s: string;
  url: string;
  par: string;
  pars: string;
begin
  params := TStringList.Create;
  try
    // First take any specified params
    params.Assign(AParams);

    // Now identify any query parameters that may be encoded in the URL
    url := String(UTF8Encode(AURL));
    i := Pos('?', url);
    if i <> 0 then
    begin
      pars := Copy(url, i + 1, Length(url) - i);
      SetLength(url, i - 1);

      while (pars <> '') do
      begin
        i := Pos('&', pars);
        if (i > 0) then
        begin
          par := Copy(pars, 1, i - 1);
          Delete(pars, 1, i);
        end
        else
        begin
          par := pars;
          pars := '';
        end;

        // JTS #1770
        //
        // If the param is a name/value pair (contains an '=') then
        // we add it then URLDecode() the parameter VALUE.
        //
        // This was discovered as required when testing against Xero, when
        // we found that the URL encoding of "where" filters required that
        // the URL encoded filter condition is required to be UN-encoded
        // in the parameter list for the purposes of OAuth base string:
        //
        // <xero url>/Invoices?where=Type%3d%3d%22ACCREC%22
        //
        // Contains the parameter:
        //
        // where      Type=="ACCREC"
        //
        i := Pos('=', par);
        if (i > 0) then
        begin
          s := params.Names[params.Add(par)];
          params.Values[s] := URLDecode(params.Values[s]);
        end
        else
          params.Add(par + '=');
      end;
    end;

    // Now sort all the params
    params.Sort;

    // Now compose the OAuth Base String:  METHOD&URL&PARAMS
    s := '';
    for i := 0 to Pred(params.Count) do
      s := s + OAuthEncode(String(UTF8Encode(params.Names[i]))) + '=' +
          OAuthEncode(String(UTF8Encode(params.ValueFromIndex[i]))) + '&';

    SetLength(s, Length(s) - 1);

    Result := aMethod + '&' + OAuthEncode(url) + '&' + OAuthEncode(s);

  finally
    params.Free;
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function TXEROAPIBase.OAuthEncode(const aString: string): string;
var
  i: integer;
begin
  Result := '';

  for i := 1 to Length(aString) do
    if ANSIChar(aString[i]) in ['0' .. '9', 'a' .. 'z', 'A' .. 'Z', '_', '-',
      '.', '~'] then
      Result := Result + aString[i]
    else
      Result := Result + '%' + IntToHex(Byte(aString[i]), 2);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function TXEROAPIBase.OAuthSignature(const aBaseString: string;
  const aKey: TRSAPrivateKey): string;
var
  key: TRSAPublicKey;
begin
  RSAPublicKeyFromPrivate(aKey, key);
  try
    Result := String(RSAPKCS1v15AsBase64(aBaseString, key));
  finally
    RSAPublicKeyFinalise(key);
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function TXEROAPIBase.OAuthSignature(const aBaseString: string;
  const aKey: string): string;
var
  key: TRSAPrivateKey;
begin

  RSAReadASN1PrivateKey(AnsiString(aKey), key);
  try
    Result := OAuthSignature(aBaseString, key);

  finally
    RSAPrivateKeyFinalise(key);
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function TXEROAPIBase.OAuthSignature(const aBaseString: string;
  const aConsumerSecret: string; const aTokenSecret: string): string;
var
  i: integer;
  key: string;
  hmac: TIdHMACSHA1;
  hash: TIdBytes;
begin
  key := String(UTF8Encode(aConsumerSecret + '&' + aTokenSecret));

  hmac := TIdHMACSHA1.Create;
  try
    hmac.key := ToBytes(key);
    hash := hmac.HashValue(ToBytes(aBaseString));

    SetLength(Result, Length(hash) * 2);
    i := XERO.Base64.Base64Encode(@hash[0], @Result[1], Length(hash));
    SetLength(Result, i);

  finally
    hmac.Free;
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function TXEROAPIBase.OAuthTimeStamp: string;
const
  UNIX_BASE = 25569.0;
var
  dt: TDateTime;
  ts: integer;
begin
  dt := TTimeZone.Local.ToUniversaltime(now);
  ts := Round((dt - UNIX_BASE) * 86400);
  Result := IntToStr(ts);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
procedure TXEROAPIBase.OAuthSignRequest(const aRequest: TIdHTTPRequest;
  const aMethod: string; const AURL: string; const AParams: TStrings;
  const AConsumerKey: string; const AToken: string;
  const aConsumerSecret: string; const aTokenSecret: string);
var
  i: integer;
  s: string;
  sig: string;
  timestamp: string;
  nonce: string;
  params: TStringList;
  url: string;
begin
  // Generate OAuth TIMESTAMP and NONCE

  timestamp := OAuthTimeStamp;
  nonce := GetGUIDString;

  Delete(nonce, 1, 1);
  SetLength(nonce, Length(nonce) - 1);
  nonce := StringReplace(nonce, '-', '', [rfReplaceAll]);

  params := TStringList.Create;
  try
    // Combine specified parameters and the OAuth headers

    if Assigned(AParams) then
      params.Assign(AParams);

    params.Values['oauth_nonce'] := nonce;
    params.Values['oauth_timestamp'] := timestamp;
    params.Values['oauth_signature_method'] := 'HMAC-SHA1';
    params.Values['oauth_version'] := '1.0';

    if (AConsumerKey <> '') then
      params.Values['oauth_consumer_key'] := AConsumerKey
    else if params.IndexOfName('oauth_consumer_key') <> -1 then
      params.Delete(params.IndexOfName('oauth_consumer_key'));

    if (AToken <> '') then
      params.Values['oauth_token'] := AToken
    else if params.IndexOfName('oauth_token') <> -1 then
      params.Delete(params.IndexOfName('oauth_token'));

    // Derive and sign the base string for the specified, Method, URL and Combined Params

    url := NormalisedURL(AURL);
    s := OAuthBaseString(aMethod, url, params);
    sig := OAuthSignature(s, aConsumerSecret, aTokenSecret);

    // Remove any params from the URL that we will include as the REALM in the Authorization header

    i := Pos('?', url);
    if i <> 0 then
      SetLength(url, i - 1);

    // Now add a custom Authorization Header with the OAuth credentials

    s := Format('OAuth realm="%s", ' + 'oauth_nonce="%s", ' +
      'oauth_timestamp="%s", ' + 'oauth_signature_method="HMAC-SHA1", ' +
      'oauth_version="1.0"', [url, nonce, timestamp]);

    if AConsumerKey <> '' then
      s := s + Format(', oauth_consumer_key="%s"', [OAuthEncode(AConsumerKey)]);

    if AToken <> '' then
      s := s + Format(', oauth_token="%s"', [OAuthEncode(AToken)]);

    s := s + Format(', oauth_signature="%s"', [OAuthEncode(sig)]);

    aRequest.CustomHeaders.Values['Authorization'] := s;

  finally
    params.Free;
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
procedure TXEROAPIBase.OAuthSignRequest(const aRequest: TIdHTTPRequest;
  const aMethod: string; const AURL: string; const AParams: TStrings;
  const AConsumerKey: string; const AToken: string; const APrivateKey: string);
var
  i: integer;
  s: string;
  timestamp: string;
  nonce: string;
  params: TStringList;
  url: string;
  LPrivateKey: string;
begin
  // Generate OAuth TIMESTAMP and NONCE

  timestamp := OAuthTimeStamp;
  nonce := GetGUIDString;
  LPrivateKey := StripCRLLF(APrivateKey);

  Delete(nonce, 1, 1);
  SetLength(nonce, Length(nonce) - 1);
  nonce := StringReplace(nonce, '-', '', [rfReplaceAll]);

  params := TStringList.Create;
  try
    // Combine specified parameters and the OAuth headers

    if Assigned(AParams) then
      params.Assign(AParams);

    params.Values['oauth_consumer_key'] := AConsumerKey;
    params.Values['oauth_token'] := AToken;
    params.Values['oauth_nonce'] := nonce;
    params.Values['oauth_timestamp'] := timestamp;
    params.Values['oauth_signature_method'] := 'RSA-SHA1';
    params.Values['oauth_version'] := '1.0';
    // Derive and sign the base string for the specified, Method, URL and Combined Params

    url := NormalisedURL(AURL);

    s := OAuthBaseString(aMethod, url, params);

    s := OAuthSignature(s, LPrivateKey);

    // Remove any params from the URL that we will include as the REALM in the Authorization header

    i := Pos('?', url);
    if i <> 0 then
      SetLength(url, i - 1);

    // Now add a custom Authorization Header with the OAuth credentials

    aRequest.CustomHeaders.Values['Authorization'] :=
      Format('OAuth realm="%s", ' + 'oauth_consumer_key="%s", ' +
      'oauth_token="%s", ' + 'oauth_nonce="%s", ' + 'oauth_timestamp="%s", ' +
      'oauth_signature_method="RSA-SHA1", ' + 'oauth_version="1.0", ' +
      'oauth_signature="%s"', [url, OAuthEncode(AConsumerKey),
      OAuthEncode(AToken), nonce, timestamp, OAuthEncode(s)]);

  finally
    params.Free;
  end;
end;

constructor TXEROAPIBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LogLevel := logInformation;
end;

function TXEROAPIBase.HTTPClient : TIdHTTP;
var
  handler : TIdSSLIOHandlerSocketOpenSSL;
begin
  if not assigned(FHTTPClient) then
    FHTTPClient := TIdHTTP.Create(self);
  if not assigned(FSSLHandler) then
  begin
    handler := TIdSSLIOHandlerSocketOpenSSL.Create(self);
    FSSLHandler := handler;
    handler.SSLOptions.Method := sslvTLSv1_2;
    FHTTPClient.IOHandler := FSSLHandler;
  end;
  result := FHTTPClient;
end;

procedure TXEROAPIBase.ValidateSettings;
begin
  if Assigned(FXEROAppDetails) then
  begin
    if not FXEROAppDetails.ValidateSettings then
    begin
      raise EXEROException.Create
        ('Application setting are incomplete. Please ensure ConsumerKey, ConsumerSecret and PrivateKey are assinged.');
    end;
  end
  else
  begin
    raise EXEROException.Create('Application settings have not been assigned.');
  end;
end;

procedure TXEROAPIBase.SetXEROAppDetails(AXEROAppDetails: TXEROAppDetails);
begin
  FXEROAppDetails := AXEROAppDetails;
end;

function TXEROAPIBase.GetXEROAppDetails: TXEROAppDetails;
begin
  Result := FXEROAppDetails;
end;

function TXEROAPIBase.Get(AURL: string; AParams: string; var AResponse: string;
  ALastModified: TDateTime = 0; AResponseType: TResponseType = rtXML): Boolean;
var
  HTTPStream: TStringStream;
  ResponseCode : integer;
  ErrorDetail : string;
  FormParams: TStringList;
begin
  FormParams := nil;
  HTTPStream := TStringStream.Create('');
  try
    if AParams <> '' then
    begin
      FormParams := TStringList.Create;
      FormParams.Text := AParams;
    end;
    HTTPStream.Position := 0;
    result := Get(AURL, FormParams, HTTPStream, ResponseCode, ErrorDetail, ALastModified, AResponseType);

    AResponse := HTTPStream.ReadString(HTTPStream.Size);
    if not result then
      AResponse := Format('ERROR: %s (%s)', [ErrorDetail, AResponse]);

  finally
    FreeAndNil(HTTPStream);
    FreeAndNil(FormParams);
  end;
end;

function TXEROAPIBase.Get(AURL: string; AParams: TStrings; AResponse: TStream; var ResponseCode : integer; var ErrorDetail : string; ALastModified: TDateTime = 0; AResponseType: TResponseType = rtXML): Boolean;
var
  strStream : TStringStream;
begin
  ValidateSettings;
  try
    if HasLog(logDebug) then
    begin
      if assigned(AParams) then
        Debug('Get', Format('URL: %s, Params: %s', [AURL, AParams.CommaText]))
      else
        Debug('Get', Format('URL: %s, Params: nil', [AURL]));
    end;

    case FXEROAppDetails.OAuthSignatureMethod of
      oaHMAC:
        begin
          OAuthSignRequest(HTTPClient.Request, 'GET', AURL, AParams,
            FXEROAppDetails.ConsumerKey, FXEROAppDetails.ConsumerKey,
            FXEROAppDetails.ConsumerSecret, FXEROAppDetails.ConsumerSecret);
        end;

      oaRSA:
        begin
          OAuthSignRequest(HTTPClient.Request, 'GET', AURL, AParams,
            FXEROAppDetails.ConsumerKey, FXEROAppDetails.ConsumerKey,
            FXEROAppDetails.PrivateKey.Text);
        end;
    end;

    case AResponseType of
      rtXML:
        begin
        end;
      rtJSON:
        begin
          HTTPClient.Request.Accept := 'application/json';
        end;
    end;

    if ALastModified <> 0 then
    begin
      HTTPClient.Request.LastModified := ALastModified;
    end;

    Log(Format('URL: %s, Headers: %s, Accept: %s, Last Modified %s',
      [AURL, HTTPClient.Request.CustomHeaders.Text, HTTPClient.Request.Accept,
      DateTimeToStr(HTTPClient.Request.LastModified)]));

    try
      HTTPClient.Get(AURL, AResponse);
      responseCode := HTTPClient.Response.ResponseCode;
      ErrorDetail  := HTTPClient.Response.ResponseText;
      Result := true;
    except
      on E:EIdHTTPProtocolException do
      begin
        ResponseCode := E.ErrorCode;
        ErrorDetail := E.Message;
        Result := false;
        strStream := TStringStream.Create(E.ErrorMessage);
        try
          AResponse.CopyFrom(StrStream, 0);
        finally
          strStream.Free;
        end;
      end;
    end;

  except
    on E: Exception do
    begin
      ResponseCode := 10000;
      ErrorDetail := E.Message;
      result := false;
    end;
  end;

  if HasLog(logDebug) then
  begin

    strStream := TStringStream.Create('');
    try
      strStream.CopyFrom(AResponse, 0);
      Debug('Get', 'Result: ' + BoolToStr(Result, true)
      + ', Response: ' + StrStream.DataString);
    finally
      FreeAndNil(strStream);
    end;
  end;

end;

function TXEROAPIBase.Post(AURL :String; AParams : TStrings; AResponse : TStream;
    var ResponseCode : Integer; var ErrorDetail : String;
    AResponseType : TResponseType = rtXML): boolean;
var
  strStream : TStringStream;
begin
  ValidateSettings;
  try
    if HasLog(logDebug) then
    begin
      if assigned(AParams) then
        Debug('Post', Format('URL: %s, Params: %s', [AURL, AParams.CommaText]))
      else
        Debug('Post', Format('URL: %s, Params: nil', [AURL]));
    end;

    case FXEROAppDetails.OAuthSignatureMethod of
      oaHMAC:
        begin
          OAuthSignRequest(HTTPClient.Request, 'POST', AURL, AParams,
            FXEROAppDetails.ConsumerKey, FXEROAppDetails.ConsumerKey,
            FXEROAppDetails.ConsumerSecret, FXEROAppDetails.ConsumerSecret);
        end;

      oaRSA:
        begin
          OAuthSignRequest(HTTPClient.Request, 'POST', AURL, AParams,
            FXEROAppDetails.ConsumerKey, FXEROAppDetails.ConsumerKey,
            FXEROAppDetails.PrivateKey.Text);
        end;
    end;

    HTTPClient.Request.ContentType := 'application/x-www-form-urlencoded';
    case AResponseType of
      rtXML: ;
      rtJSON: HTTPClient.Request.Accept := 'application/json';
    end;
    HTTPClient.HTTPOptions := [hoForceEncodeParams];

    Log(Format('URL: %s, Headers: %s, Accept: %s',
      [AURL, HTTPClient.Request.CustomHeaders.Text, HTTPClient.Request.Accept ]));

    try
      HTTPClient.Post(AURL, AParams, AResponse);
      responseCode := HTTPClient.Response.ResponseCode;
      ErrorDetail  := HTTPClient.Response.ResponseText;
      Result := true;

    except
      on E:EIdHTTPProtocolException do
      begin
        ResponseCode := E.ErrorCode;
        ErrorDetail := E.Message;
        // Convert error message to stream for parsing.
        Result := false;
        strStream := TStringStream.Create(E.ErrorMessage);
        try
          AResponse.CopyFrom(StrStream, 0);
        finally
          strStream.Free;
        end;
      end;
    end;

  except
    on E: Exception do
    begin
      ResponseCode := 10000;
      ErrorDetail := E.Message;
      result := false;
    end;
  end;

  if HasLog(logDebug) then
  begin
    strStream := TStringStream.Create('');
    try
      strStream.CopyFrom(AResponse, 0);
      Debug('Get', 'Result: ' + BoolToStr(Result, true)
      + ', Response: ' + StrStream.DataString);
    finally
      FreeAndNil(strStream);
    end;
  end;

end;



// TXEROResponseBase

function TXEROResponseBase.rStream: TStream;
begin
  result := FResponse;
end;

procedure TXEROResponseBase.ClearStream;
begin
  FResponse.Clear;
end;

procedure TXEROResponseBase.SetResponse( AResult :boolean; ACode : Integer; ADetail : String);
begin
  FResult := AResult;
  FResponseCode := ACode;
  FErrorMessage := ADetail;
end;

function TXEROResponseBase.AsString: string;
var
  sr : TStreamReader;
begin
  FResponse.Position := 0;
  sr := TStreamReader.Create(FResponse);
  try
    result := sr.ReadToEnd;
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
  FResult := False;
  FErrorMessage := '';
  FResponseCode := -1;
end;

Procedure TXEROResponseBase.BeforeDestruction;
begin
  FreeAndNil(FResponse);
  inherited;
end;

// TXEROAPI

procedure TXEROAPI.ValidateSettings;
begin
  inherited ValidateSettings;
  if not Assigned(FXEROResponseBase) then
  begin
    raise EXEROException.Create('Response has not been assigned.');
  end;
end;

function TXEROAPI.GetDateTimeFilterString(ADateTime: TDateTime): string;
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

procedure TXEROAPI.SetXEROResponseBase(AXEROResponseBase: TXEROResponseBase);
begin
  if Assigned(AXEROResponseBase) then
  begin
    FXEROResponseBase := AXEROResponseBase;
  end
  else
  begin
    FXEROResponseBase := nil;
  end;
end;

function TXEROAPI.GetXEROResponseBase: TXEROResponseBase;
begin
  Result := FXEROResponseBase;
end;

function TXEROAPI.Find(AFilter: string = ''; AOrderBy: string = '';
  APage: integer = 0; ALastModified: TDateTime = 0): Boolean;
var
  url: string;
  ResponseCode : integer;
  ErrorDetail : String;

begin
  ValidateSettings;

  url := GetAPIURL;

  if not IsEmptyString(AFilter) then
  begin
    url := url + GetURLSeperator(url) + 'where=' + URLEncode(AFilter);
  end;
  if not IsEmptyString(AOrderBy) then
  begin
    url := url + GetURLSeperator(url) + 'order=' + URLEncode(AOrderBy);
  end;
  if APage > 0 then
  begin
    url := url + GetURLSeperator(url) + 'page=' + IntToStr(APage);
  end;

  FXEROResponseBase.ClearStream;
  result := Get(url, nil, FXEROResponseBase.Stream,
    ResponseCode, ErrorDetail, ALastModified, FXEROResponseBase.ResponseType);

  FXEROResponseBase.SetResponse(result, ResponseCode, ErrorDetail);
end;

// TXEROQueryBuilder
//
const
  CODataOp : array[TXDataOp] of string =
  ( '==', '<', '>', '<=', '>=', '!=');
  CODataFunc : array[TXDataFunc] of string =
  ( 'StartsWith', 'EndsWith', 'Contains');
  CODataLogic : array[TXDataLogic] of string =
  ('AND','OR', 'NOT');
  CXNull = 'null';

function IsGUID( const strVal : String) :boolean;
const
  CPattern = 'xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx';
var
  idx : integer;
begin
  result := length(strVal) = length(CPattern);
  if result then
  begin
    for idx := 1 to length(strVal) do
    begin
      case strVal[idx] of
        'a'..'f', '0'..'9':
          if CPattern[idx] = '-' then
          begin
            result := false;
            break;
          end;
        '-':
          if CPattern[idx] <> '-' then
          begin
            result := false;
            break;
          end;
      else
        result := false;
        break;
      end
    end;

  end;
end;

// protected definitions

procedure TXEROQueryBuilder.OutValue( fieldValue : Variant; ForceString: Boolean);
var
  dt : TDateTime;
  strVal : String;
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
      AppendStr(IfThen(CastVarAsBoolean(fieldValue, true, true, false),'true','false'));
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
        AppendStr(StringReplace(StringReplace(strVal, '\' ,'\\',[rfReplaceAll]), '"', '\"',[rfReplaceAll]));
        AppendStr('"');
      end;
    end;
  end;
end;

function TXEROQueryBuilder.GetString : String;
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
  result := FBuildString;
end;

procedure TXEROQueryBuilder.DoOp( AOp : TXDataOp; FieldName : String; fieldValue : Variant);
begin
  AppendStr(FieldName);
  AppendStr(' ');
  AppendStr(CODataOp[AOp]);
  AppendStr(' ');
  OutValue(fieldValue);
end;

procedure TXEROQueryBuilder.DoFn(AFn : TXDataFunc; FieldName : String; FieldValue : Variant; ProtectNull : boolean );
begin
  if protectNull then
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
  if protectNull then
    AppendStr(')');

end;

// public definitions
//
class function TXEROQueryBuilder.Create : TXEROQueryBuilder;
begin
  result.Init;
end;

procedure TXEROQueryBuilder.Init;
begin
  FState := bsInit;
  FOpenParen := 0;
  FBuildString := '';
  FCapacity := 0;
  FLen := 0;
end;

procedure TXEROQueryBuilder.AppendStr( const strVal : String);
var
  newLen, newCapacity : Integer;
begin
  newLen := FLen + Length(StrVal);
  if newLen > FCapacity then
  begin
    newCapacity := FCapacity + 20;
    if newLen > newCapacity then
      newCapacity := newLen + 20;
    SetLength(FBuildString, newCapacity);
    FCapacity := newCapacity;
  end;
  Move(PChar(strVal)^, FBuildString[1+FLen], ByteLength(strVal) );
  FLen := newLen;
end;

// Standard equality/inequality between a field and a value.
procedure TXEROQueryBuilder.Op(FieldName : String;  AOp : TXDataOp; fieldValue : Variant);
begin
  if FState = bsOk then
    Raise Exception.Create('Logic operator required');
  DoOp(AOp, FieldName, fieldValue);
  FState := bsOK;
end;

// Simple string functions (contains, starts, ends)
procedure TXEROQueryBuilder.Fn(FieldName : String; AFn : TXDataFunc; FieldValue : Variant; ProtectNull : boolean = true);
begin
  if FState = bsOk then
    Raise Exception.Create('Logic operator required');
  DoFn(AFn, FieldName, FieldValue, protectNull);
  FState := bsOK;
end;

// And/or/not logic operator
procedure TXEROQueryBuilder.Logic( ALg : TXDataLogic);
begin
  case FState of
    bsInit:
      case Alg of
        xdlAnd, xdlOr:
          Raise Exception.Create('Can''t start with binary logic operator');
      end;
    bsPending:
      case Alg of
        xdlAnd, xdlOr:
          Raise Exception.Create('Already has Logic operator');
      end;
    bsOK:
      case Alg of
        xdlNot:
          Raise Exception.Create('Not operator not applicable here');
      end;
  end;
  case Alg of
    xdlAnd, xdlOr:
      FState := bsPending;
    xdlNot:
      FState := bsPending;
  end;
  AppendStr(' ');
  AppendStr(CODataLogic[Alg]);
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
  FState := bsOK;
end;

end.
