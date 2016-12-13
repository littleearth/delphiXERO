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
  System.SysUtils,
  Classes, IdHTTP, XERO.CipherRSA;

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
  protected
    procedure Log(AMessage: string);
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
      const aMethod: string; const AURL: string; const AParams: TStringList;
      const AConsumerKey: string; const AToken: string;
      const aConsumerSecret: string; const aTokenSecret: string); overload;
    procedure OAuthSignRequest(const aRequest: TIdHTTPRequest;
      const aMethod: string; const AURL: string; const AParams: TStringList;
      const AConsumerKey: string; const AToken: string;
      const APrivateKey: string); overload;
    procedure ValidateSettings; virtual;
    function Get(AURL: string; AParams: string; var AResponse: string;
      ALastModified: TDateTime = 0;
      AResponseType: TResponseType = rtXML): Boolean;
    procedure SetXEROAppDetails(AXEROAppDetails: TXEROAppDetails);
    function GetXEROAppDetails: TXEROAppDetails;
  public
    constructor Create(AOwner: TComponent); override;

  published
    property LogLevel: TLogLevel read FLogLevel write FLogLevel;
    property XEROAppDetails: TXEROAppDetails read GetXEROAppDetails
      write SetXEROAppDetails;
    property OnLog: TOnLog read FOnLog write FOnLog;
  end;

type
  TXEROResponseBase = class(TComponent)
  private
    FResponse: string;
    FResult: Boolean;
    FErrorMessage: string;
    FResponseType: TResponseType;
  protected
    function GetDefaultResponseType: TResponseType; virtual;
    procedure SetResponse(AResponse: string; AResult: Boolean = true;
      AErrorMessage: string = ''); virtual;
    property ResponseType: TResponseType read FResponseType write FResponseType;
  public
    constructor Create(AOwner: TComponent); override;
    function AsString: string;
    property Result: Boolean read FResult;
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

implementation

uses Windows, IdGlobal, IdHMACSHA1, IdURI, XERO.HugeInt, XERO.Base64,
  XERO.RSAUtils, XERO.Utils, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL,
  IdSSLOpenSSL;

function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation
  : PTimeZoneInformation; var lpLocalTime, lpUniversalTime: TSystemTime): BOOL;
  stdcall; external kernel32;


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

procedure TXEROAPIBase.Log(AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, AMessage);
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
  const aMethod: string; const AURL: string; const AParams: TStringList;
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
  const aMethod: string; const AURL: string; const AParams: TStringList;
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
  HTTPClient: TIdHTTP;
  IdSSLIOHandlerSocketOpenSSL: TIdSSLIOHandlerSocketOpenSSL;
  HTTPStream: TStringStream;
  FormParams: TStringList;
begin
  ValidateSettings;
  Result := False;
  HTTPClient := TIdHTTP.Create(nil);
  IdSSLIOHandlerSocketOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FormParams := TStringList.Create;
  HTTPStream := TStringStream.Create('');
  try
    try
      HTTPClient.IOHandler := IdSSLIOHandlerSocketOpenSSL;

      FormParams.Text := AParams;

      Debug('Get', Format('URL: %s, Params: %s', [AURL, AParams]));

      case FXEROAppDetails.OAuthSignatureMethod of
        oaHMAC:
          begin
            OAuthSignRequest(HTTPClient.Request, 'GET', AURL, FormParams,
              FXEROAppDetails.ConsumerKey, FXEROAppDetails.ConsumerKey,
              FXEROAppDetails.ConsumerSecret, FXEROAppDetails.ConsumerSecret);
          end;

        oaRSA:
          begin
            OAuthSignRequest(HTTPClient.Request, 'GET', AURL, FormParams,
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

      HTTPClient.Get(AURL, HTTPStream);

      HTTPStream.Position := 0;

      AResponse := HTTPStream.ReadString(HTTPStream.Size);

      Result := true;

    except
      on E: Exception do
      begin
        try
          HTTPStream.Position := 0;
          AResponse := HTTPStream.ReadString(HTTPStream.Size);
        except

        end;
        AResponse := Format('ERROR: %s (%s)', [E.Message, AResponse]);
      end;
    end;

    Debug('Get', 'Result: ' + BoolToStr(Result, true) + ', Response: ' +
      AResponse);

  finally
    FreeAndNil(HTTPStream);
    FreeAndNil(FormParams);
    FreeAndNil(HTTPClient);
    FreeAndNil(IdSSLIOHandlerSocketOpenSSL);
  end;
end;


// TXEROResponseBase

procedure TXEROResponseBase.SetResponse(AResponse: string;
  AResult: Boolean = true; AErrorMessage: string = '');
begin
  FResponse := AResponse;
  FResult := AResult;
  FErrorMessage := AErrorMessage;
end;

function TXEROResponseBase.AsString: string;
begin
  Result := FResponse;
end;

function TXEROResponseBase.GetDefaultResponseType: TResponseType;
begin
  Result := rtXML;
end;

constructor TXEROResponseBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResponseType := GetDefaultResponseType;
  FResponse := '';
  FResult := False;
  FErrorMessage := '';
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
  params: string;
  ResponseData: string;

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

  Result := Get(url, params, ResponseData, ALastModified,
    FXEROResponseBase.ResponseType);

  if Result then
  begin
    FXEROResponseBase.SetResponse(ResponseData);
  end
  else
  begin
    FXEROResponseBase.SetResponse('', False, ResponseData);
  end;
end;

end.
