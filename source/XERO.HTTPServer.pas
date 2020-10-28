unit XERO.HTTPServer;

{$INCLUDE 'XERO.inc'}

interface

uses
  XERO.Types,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, IdBaseComponent,
  IdComponent, IdCustomTCPServer, IdCustomHTTPServer, IdHTTPServer,
  IdTCPServer, IdSocketHandle, IdContext;

type
  TXEROHttpCommandEvent = procedure(ACommand: string; AContext: TIdContext;
    ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
    var AHandled: boolean) of object;

  TXEROHTTPServer = class(TXEROComponent)
  private
    FIdHTTPServer: TIdHTTPServer;
    FPort: integer;
    FOnCommand: TXEROHttpCommandEvent;
    procedure SetOnCommand(const Value: TXEROHttpCommandEvent);
  protected
    procedure IdHTTPServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    function FindAvailablePort(First, Count: Word): Word;
  public
    function GenerateBasicHTMLMessage(ATitle: string; AMessage: string): string;
    function GetServerAddress: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function StartWebServer(var AMessage: string;
      AWebServerPortFirst: integer = XERO_HTTP_SERVER_FIRST_PORT): boolean;
    procedure StopWebServer;
    property Port: integer read FPort;
    property OnCommand: TXEROHttpCommandEvent read FOnCommand
      write SetOnCommand;
  end;

  TXEROPortCheck = class(TIdTCPServer)
  protected
    procedure OnExec(AContext: TIdContext);
  public
    constructor Create; reintroduce;
    function FindAvailablePort(First, Count: Word): Word;
  end;

implementation

{ TXEROHTTPServer }

constructor TXEROHTTPServer.Create(AOwner: TComponent);
begin
  inherited;
  FIdHTTPServer := TIdHTTPServer.Create(nil);
  FIdHTTPServer.OnCommandGet := IdHTTPServerCommandGet;
  FIdHTTPServer.OnCommandOther := IdHTTPServerCommandGet;
  FPort := 0;
end;

destructor TXEROHTTPServer.Destroy;
begin
  try
    FreeAndNil(FIdHTTPServer);
  finally
    inherited;
  end;
end;

function TXEROHTTPServer.FindAvailablePort(First, Count: Word): Word;
var
  LPortChk: TXEROPortCheck;
begin
  LPortChk := TXEROPortCheck.Create;
  try
    Result := LPortChk.FindAvailablePort(First, Count);
  finally
    LPortChk.Free;
  end;
end;

function TXEROHTTPServer.GenerateBasicHTMLMessage(ATitle,
  AMessage: string): string;
begin
  Result := Format
    ('<!DOCTYPE html><html><body><center><h1>%s</h1><p>%s</p></center></body></html>',
    [ATitle, AMessage]);
end;

function TXEROHTTPServer.GetServerAddress: string;
begin
  Result := '';
  if FIdHTTPServer.Active then
  begin
    Result := Format('http://localhost:%d', [FPort]);
  end;
end;

procedure TXEROHTTPServer.IdHTTPServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LRequestedDocument: string;
  LHandled: boolean;
  LMessage: string;
begin
  LHandled := False;
  LMessage := '';
  LRequestedDocument := ARequestInfo.Document;
  if Copy(LRequestedDocument, 1, 1) = '/' then
  begin
    if Assigned(FOnCommand) then
    begin
      FOnCommand(ARequestInfo.Command, AContext, ARequestInfo, AResponseInfo,
        LHandled);
    end
    else
    begin
      LMessage := 'No command handler available';
    end;
  end
  else
  begin
    LMessage := Format('Invalid request: %s', [LRequestedDocument]);
  end;

  if not LHandled then
  begin
    AResponseInfo.ResponseNo := 400;
    AResponseInfo.ResponseText := LMessage;
    AResponseInfo.ContentText := GenerateBasicHTMLMessage('XERO Authentication',
      LMessage);
  end;

end;

procedure TXEROHTTPServer.SetOnCommand(const Value: TXEROHttpCommandEvent);
begin
  FOnCommand := Value;
end;

function TXEROHTTPServer.StartWebServer(var AMessage: string;
  AWebServerPortFirst: integer): boolean;
var
  LBind: TIdSocketHandle;
begin
  AMessage := '';
  StopWebServer;
  FPort := FindAvailablePort(AWebServerPortFirst, 2);
  try
    FIdHTTPServer.Bindings.Clear;
    LBind := FIdHTTPServer.Bindings.ADd;
    LBind.IP := '127.0.0.1';
    LBind.Port := FPort;
    FIdHTTPServer.Active := True;
    Debug('StartWebServer', 'Server Started');
  except
    on E: Exception do
    begin
      AMessage := E.Message;
    end;
  end;
  Result := FIdHTTPServer.Active;
end;

procedure TXEROHTTPServer.StopWebServer;
begin
  try
    if FIdHTTPServer.Active then
    begin
      FIdHTTPServer.Active := False;
      FIdHTTPServer.Bindings.Clear;
      Debug('StopWebServer', 'Server Stopped');
    end;
  except
    on E: Exception do
    begin
      Error(E);
    end;
  end;
end;

{ TXEROPortCheck }

constructor TXEROPortCheck.Create;
begin
  inherited;
  OnExecute := OnExec;
end;

function TXEROPortCheck.FindAvailablePort(First, Count: Word): Word;
var
  LBind: TIdSocketHandle;
begin
  Result := First;

  while (Result - First) < Count do
  begin
    try
      Self.Bindings.Clear;
      LBind := Self.Bindings.ADd;
      LBind.IP := '127.0.0.1';
      LBind.Port := Result;
      Self.Active := True;
      Self.Active := False;
      Exit;
    except
      Inc(Result);
      Self.Bindings.Clear;
    end;
  end;
  Result := $FFFF;
end;

procedure TXEROPortCheck.OnExec(AContext: TIdContext);
begin
  // Nothing
end;

end.
