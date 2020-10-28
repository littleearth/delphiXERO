unit XERO.Authenticator.PKCE;

{$INCLUDE 'XERO.inc'}

interface

uses
  XERO.Types, XERO.API, System.SysUtils, XERO.HTTPServer, System.Classes,
  IdHTTP, IdIOHandler,
  IdCustomHTTPServer,
  IdContext;

type

  TXEROOnAuthenticationURL = procedure(ASender: TObject; AURL: string)
    of object;

  TXEROAuthenticatorPKCE = class(TXEROAuthenticatorBase)
  private
    FXEROHTTPServer: TXEROHTTPServer;
    FCodeVerifier: string;
    FState: string;
    FOnAuthenticationURL: TXEROOnAuthenticationURL;
    FScope: string;
    FWebServerPortFirst: integer;
    FRedirectURLEndpoint: string;
    FMaxHTTPServerRunMinutes: integer;
    FHTTPServerStart: TDateTime;
    FAuthRequestACtive: boolean;
    function GetCodeChallenge: string;
    function GetCodeVerifier: string;
    procedure SetCodeVerifier(const Value: string);
    procedure SetState(const Value: string);
    procedure OnHTTPServerCommand(ACommand: string; AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
      var AHandled: boolean);
    procedure SetOnAuthenticationURL(const Value: TXEROOnAuthenticationURL);
    procedure SetScope(const Value: string);
    procedure SetWebServerPortFirst(const Value: integer);
    procedure SetRedirectURLEndpoint(const Value: string);
    function GetRedirectURLEndpoint: string;
    procedure SetMaxHTTPServerRunMinutes(const Value: integer);
  protected
    procedure InternalTimerEvent; override;
    function GetAccessToken(var AMessage: string; ACode: string;
      ARedirectURI: string): boolean;
    procedure Authenticate; override;
    procedure SetDefaultProperties; override;
    procedure GenerateCodeVerifier; virtual;
    procedure GenerateRandomState; virtual;
    function GenerateLoginURL: string; virtual;
    function GetRedirectURI: string; virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure CancelAuthenticationRequest;
  published
    property CodeVerifier: string read GetCodeVerifier write SetCodeVerifier;
    property State: string read FState write SetState;
    property CodeChallenge: string read GetCodeChallenge;
    property Scope: string read FScope write SetScope;
    property RedirectURLEndpoint: string read GetRedirectURLEndpoint
      write SetRedirectURLEndpoint;
    property WebServerPortFirst: integer read FWebServerPortFirst
      write SetWebServerPortFirst;
    property MaxHTTPServerRunMinutes: integer read FMaxHTTPServerRunMinutes
      write SetMaxHTTPServerRunMinutes;
    property OnAuthenticationURL: TXEROOnAuthenticationURL
      read FOnAuthenticationURL write SetOnAuthenticationURL;
  end;

implementation

uses
  XERO.Utils, XERO.PKCE, System.DateUtils;

{ TXEROAuthenticatorPKCE }

procedure TXEROAuthenticatorPKCE.OnHTTPServerCommand(ACommand: string;
  AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo; var AHandled: boolean);
var
  LCode, LState: string;
  LSuccess: boolean;
  LMessage: string;
  LRedirectURI: string;
begin
  LSuccess := False;

  Debug('OnHTTPServerCommand',
    format('IP: %s, Command: %s, URL: %s, Params: %s, ContentEncoding: %s, ContentType: %s, ContentLength: %d',
    [ARequestInfo.RemoteIP, ARequestInfo.Command, ARequestInfo.URI,
    ARequestInfo.Params.Text, ARequestInfo.ContentEncoding,
    ARequestInfo.ContentType, ARequestInfo.ContentLength]));

  if SameText(RedirectURLEndpoint, ARequestInfo.URI) then
  begin
    LCode := ARequestInfo.Params.Values['code'];
    LState := ARequestInfo.Params.Values['state'];
    if not IsEmptyString(LCode) then
    begin
      if SameText(LState, FState) then
      begin
        LRedirectURI := GetRedirectURI;
        if GetAccessToken(LMessage, LCode, LRedirectURI) then
        begin
          if UpdateTenants(LMessage) then
          begin
            LMessage := 'Login complete return to application';
            LSuccess := true;
          end;
        end;
      end
      else
      begin
        LMessage := 'Login failed with an invalid state';
      end;
    end
    else
    begin
      LMessage := 'Login failed not response code';
    end;

    if LSuccess then
    begin
      AResponseInfo.ResponseNo := 200;
      AResponseInfo.ResponseText := LMessage;
      AResponseInfo.ContentText := FXEROHTTPServer.GenerateBasicHTMLMessage
        (XEROAppDetails.AppName, LMessage);
    end
    else
    begin
      AResponseInfo.ResponseNo := 400;
      AResponseInfo.ResponseText := LMessage;
      AResponseInfo.ContentText := FXEROHTTPServer.GenerateBasicHTMLMessage
        (XEROAppDetails.AppName, LMessage);;
    end;
    if Assigned(OnAuthenticationComplete) then
      OnAuthenticationComplete(Self, LSuccess);
    FAuthRequestACtive := False;
    FHTTPServerStart := 0;
    AHandled := true;
  end;

end;

procedure TXEROAuthenticatorPKCE.AfterConstruction;
begin
  inherited;
  FXEROHTTPServer := TXEROHTTPServer.Create(nil);
  FXEROHTTPServer.OnCommand := OnHTTPServerCommand;
  FHTTPServerStart := 0;
end;

procedure TXEROAuthenticatorPKCE.Authenticate;
var
  LMEssge: string;
  LURL: string;
begin
  if not FAuthRequestACtive then
  begin
    if FXEROHTTPServer.StartWebServer(LMEssge) then
    begin
      FAuthRequestACtive := true;
      FHTTPServerStart := Now;
      LURL := GenerateLoginURL;
      Debug('Authenticate', LURL);
      if Assigned(FOnAuthenticationURL) then
        FOnAuthenticationURL(Self, LURL);
    end
    else
    begin
      raise EXEROException.Create(LMEssge);
    end;
  end
  else
  begin
    raise EXEROException.Create('Authentication request already active');
  end;
end;

procedure TXEROAuthenticatorPKCE.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FXEROHTTPServer);
end;

procedure TXEROAuthenticatorPKCE.CancelAuthenticationRequest;
begin
  try
    FXEROHTTPServer.StopWebServer;
  finally
    FAuthRequestACtive := False;
    FHTTPServerStart := 0;
  end;
end;

procedure TXEROAuthenticatorPKCE.GenerateCodeVerifier;
begin
  FCodeVerifier := TXEROPKCE.GenerateCodeVerifier;
end;

procedure TXEROAuthenticatorPKCE.GenerateRandomState;
begin
  FState := GenerateRandomString(10);
end;

function TXEROAuthenticatorPKCE.GenerateLoginURL: string;
begin
  Result := format
    ('%sconnect/authorize?response_type=code&code_challenge_method=S256&client_id=%s&scope=%s&redirect_uri=%s&state=%s&code_challenge=%s',
    [XERO_API_IDENTITY_URL, XEROAppDetails.ClientID, Scope, GetRedirectURI,
    State, CodeChallenge]);
end;

function TXEROAuthenticatorPKCE.GetAccessToken(var AMessage: string;
  ACode, ARedirectURI: string): boolean;
var
  LParams: TStringList;
  LPostStream: TStringStream;
  LData: string;
begin
  Result := False;
  AMessage := '';
  LParams := TStringList.Create;
  LPostStream := TStringStream.Create;
  try
    LParams.Add('grant_type=authorization_code');
    LParams.Add('&client_id=' + XEROAppDetails.ClientID);
    LParams.Add('&code=' + ACode);
    LParams.Add('&redirect_uri=' + ARedirectURI);
    LParams.Add('&code_verifier=' + CodeVerifier);
    try
      Debug('GetAccessToken', LParams.Text);
      LPostStream.WriteString(StripCRLLF(LParams.Text));
      HTTPClient.AllowCookies := true;
      HTTPClient.Request.Clear;
      HTTPClient.Request.ContentType := 'application/x-www-form-urlencoded';
      LData := HTTPClient.Post(XERO_API_IDENTITY_URL + 'connect/token',
        LPostStream);
      Debug('GetAccessToken', LData);
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

function TXEROAuthenticatorPKCE.GetCodeChallenge: string;
begin
  Result := TXEROPKCE.GetCodeChallenge(FCodeVerifier);
end;

function TXEROAuthenticatorPKCE.GetCodeVerifier: string;
begin
  if IsEmptyString(FCodeVerifier) then
    GenerateCodeVerifier;
  Result := FCodeVerifier;
end;

function TXEROAuthenticatorPKCE.GetRedirectURI: string;
begin
  Result := FXEROHTTPServer.GetServerAddress;
  if not IsEmptyString(Result) then
    Result := Result + FRedirectURLEndpoint;
end;

function TXEROAuthenticatorPKCE.GetRedirectURLEndpoint: string;
begin
  if IsEmptyString(FRedirectURLEndpoint) then
    FRedirectURLEndpoint := '/';
  Result := FRedirectURLEndpoint;
end;

procedure TXEROAuthenticatorPKCE.InternalTimerEvent;
begin
  inherited;
  if MinutesBetween(Now, FHTTPServerStart) >= FMaxHTTPServerRunMinutes then
  begin
    FXEROHTTPServer.StopWebServer;
  end;
end;

procedure TXEROAuthenticatorPKCE.SetCodeVerifier(const Value: string);
begin
  FCodeVerifier := Value;
end;

procedure TXEROAuthenticatorPKCE.SetDefaultProperties;
begin
  inherited;
  FScope := XERO_API_IDENTITY_DEFAULT_SCOPE;
  FWebServerPortFirst := XERO_HTTP_SERVER_FIRST_PORT;
  FRedirectURLEndpoint := XERO_HTTP_REDIRECT_ENDPOINT;
  FMaxHTTPServerRunMinutes := XERO_HTTP_MAX_RUN_MINUTES;
  GenerateCodeVerifier;
  GenerateRandomState;
end;

procedure TXEROAuthenticatorPKCE.SetMaxHTTPServerRunMinutes
  (const Value: integer);
begin
  FMaxHTTPServerRunMinutes := Value;
end;

procedure TXEROAuthenticatorPKCE.SetOnAuthenticationURL
  (const Value: TXEROOnAuthenticationURL);
begin
  FOnAuthenticationURL := Value;
end;

procedure TXEROAuthenticatorPKCE.SetRedirectURLEndpoint(const Value: string);
begin
  FRedirectURLEndpoint := Value;
end;

procedure TXEROAuthenticatorPKCE.SetScope(const Value: string);
begin
  FScope := Value;
end;

procedure TXEROAuthenticatorPKCE.SetState(const Value: string);
begin
  FState := Value;
end;

procedure TXEROAuthenticatorPKCE.SetWebServerPortFirst(const Value: integer);
begin
  FWebServerPortFirst := Value;
end;

end.
