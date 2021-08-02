unit XERO.Types;

{$INCLUDE 'XERO.inc'}

interface

uses
  Windows, Messages,
  System.SyncObjs, System.SysUtils, System.Variants, System.Classes;

const
  XERO_API_BASE_URL = 'https://api.xero.com/api.xro/2.0/';
  XERO_API_LOGIN_URL = 'https://login.xero.com/identity/';
  XERO_API_IDENTITY_URL = 'https://identity.xero.com/';
  XERO_API_CONNECTION_URL = 'https://api.xero.com/connections';
  XERO_API_IDENTITY_DEFAULT_SCOPE =
    'openid profile email accounting.transactions accounting.contacts accounting.settings';
  XERO_HTTP_SERVER_FIRST_PORT = 5885;
  XERO_HTTP_REDIRECT_ENDPOINT = '/xero';
  XERO_HTTP_MAX_RUN_MINUTES = 2;

type
  EXEROException = class(Exception);
  TLogLevel = (logError, logWarning, logInformation, logDebug);
  TResponseType = (rtXML, rtJSON);

  { ************************************ }
  { A few predefined types to help out }
  { ************************************ }

type
  Pbyte = ^byte;
  Pword = ^word;
  Pdword = ^dword;
  Pint64 = ^int64;
  dword = longword;
  Pwordarray = ^Twordarray;
  Twordarray = array [0 .. 19383] of word;
  Pdwordarray = ^Tdwordarray;
  Tdwordarray = array [0 .. 8191] of dword;
{$IFNDEF COMPILER12_UP}
  RawByteString = AnsiString;
{$ENDIF}
{$IFNDEF COMPILER11_UP}
  TBytes = array of byte;
{$ENDIF ~COMPILER11_UP}
{$IFNDEF SUPPORTS_UINT64}
  UInt64 = int64;
{$ENDIF ~SUPPORTS_UINT64}

type
{$IFNDEF COMPILER16_UP}
  NativeUInt = {$IFDEF CPU64} UInt64 {$ELSE} Cardinal {$ENDIF};
{$ENDIF}
  PointerToInt = {$IFDEF COMPILER16_UP} Pbyte {$ELSE} NativeUInt {$ENDIF};

type
  TXEROObject = class(TObject)
  protected
    procedure Log(AMessage: string);
    procedure Debug(AProcedure: string; AMessage: string);
    procedure Warning(AMessage: string);
    procedure Error(AMessage: string; AErrorCode: integer = 0); overload;
    procedure Error(AException: Exception; AMessage: string = ''); overload;
  end;

type
  TXEROComponent = class(TComponent)
  protected
    procedure Log(AMessage: string);
    procedure Debug(AProcedure: string; AMessage: string);
    procedure Warning(AMessage: string);
    procedure Error(AMessage: string; AErrorCode: integer = 0); overload;
    procedure Error(AException: Exception; AMessage: string = ''); overload;
  end;

type
  TThreadStringList = class
  private
    FStringList: TStringList;
    FLock: TCriticalSection;
    function GetDuplicates: TDuplicates;
    procedure SetDuplicates(dup: TDuplicates);
    function GetCapacity: integer;
    procedure SetCapacity(capa: integer);
    function GetCommaText: string;
    procedure SetCommaText(const S: string);
    function GetCount: integer;
    function GetDelimiter: Char;
    procedure SetDelimiter(delim: Char);
    function GetDelimitedText: string;
    procedure SetDelimitedText(const S: string);
    function GetNames(Index: integer): string;
    function GetValues(const Name: string): string;
    procedure SetValues(const Name: string; S: string);
    function GetStrings(Index: integer): string;
    procedure SetStrings(Index: integer; S: string);
    function GetAsText: string;
    procedure SetAsText(S: string);
    function GetSorted: boolean;
    procedure SetSorted(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function LockList: boolean;
    procedure UnlockList;
    function Add(const S: string): integer;
    procedure AddStrings(Strings: TStrings);
    procedure Delete(Index: integer);
    procedure Clear;
    procedure Exchange(Index1, Index2: integer);
    function Find(const S: string; var Index: integer): boolean;
    procedure Insert(Index: integer; const S: string);
    function IndexOf(const S: string): integer;
    function IndexOfName(const Name: string): integer;
    procedure Sort;
    function GetText: PChar;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Capacity: integer read GetCapacity write SetCapacity;
    property CommaText: string read GetCommaText write SetCommaText;
    property Count: integer read GetCount;
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property Names[Index: integer]: string read GetNames;
    property Values[const Name: string]: string read GetValues write SetValues;
    property Strings[Index: integer]: string read GetStrings
      write SetStrings; default;
    property Text: string read GetAsText write SetAsText;
    property Sorted: boolean read GetSorted write SetSorted;
  end;

  TThreadTimer = class(TThread)
  private
    FActive: boolean;
    FTerminateEvent: TEvent;
    FOnTimer: TNotifyEvent;
    FTimeout: integer;
    procedure SyncOnTimer;
  public
    constructor Create(AOnTimer: TNotifyEvent; ATimeout: integer = 1000);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Terminate; reintroduce;
  end;

implementation

uses
  XERO.Log;

{ TXEROObject }

procedure TXEROObject.Debug(AProcedure, AMessage: string);
begin
  XEROLog.Debug(Self, AProcedure, AMessage);
end;

procedure TXEROObject.Error(AException: Exception; AMessage: string);
begin
  XEROLog.Error(Self, AException, AMessage);
end;

procedure TXEROObject.Error(AMessage: string; AErrorCode: integer);
begin
  XEROLog.Error(Self, AMessage, AErrorCode);
end;

procedure TXEROObject.Log(AMessage: string);
begin
  XEROLog.Log(Self, AMessage);
end;

procedure TXEROObject.Warning(AMessage: string);
begin
  XEROLog.Warning(Self, AMessage);
end;

{ TXEROComponent }

procedure TXEROComponent.Debug(AProcedure, AMessage: string);
begin
  XEROLog.Debug(Self, AProcedure, AMessage);
end;

procedure TXEROComponent.Error(AException: Exception; AMessage: string);
begin
  XEROLog.Error(Self, AException, AMessage);
end;

procedure TXEROComponent.Error(AMessage: string; AErrorCode: integer);
begin
  XEROLog.Error(Self, AMessage, AErrorCode);
end;

procedure TXEROComponent.Log(AMessage: string);
begin
  XEROLog.Log(Self, AMessage);
end;

procedure TXEROComponent.Warning(AMessage: string);
begin
  XEROLog.Warning(Self, AMessage);
end;

constructor TThreadStringList.Create;
begin
  // InitializeCriticalSection(FLock);
  FLock := TCriticalSection.Create;
  FStringList := TStringList.Create;
  FStringList.Sorted := True;
  FStringList.Duplicates := dupIgnore;
end;

destructor TThreadStringList.Destroy;
begin
  // LockList;
  // try
  // FStringList.Free;
  // inherited Destroy;
  // finally
  // UnlockList;
  // DeleteCriticalSection(FLock);
  // FreeAndNil(FLock);
  // end;

  if LockList then
  begin
    try
      FStringList.Free;
    finally
      UnlockList;
    end;
  end;

  try
    FreeAndNil(FLock);
  finally
    inherited Destroy;
  end;

end;

function TThreadStringList.LockList: boolean;
begin
  Result := false;
  // EnterCriticalSection(FLock);
  if Assigned(FLock) and Assigned(FStringList) then
  begin
    try
      FLock.Acquire;
      Result := True;
    except

    end;
  end;
end;

procedure TThreadStringList.UnlockList;
begin
  // LeaveCriticalSection(FLock);
  if Assigned(FLock) then
  begin
    FLock.Release;
  end;
end;

function TThreadStringList.Add(const S: string): integer;
begin
  Result := -1;
  if not LockList then
    Exit;
  try
    Result := FStringList.Add(S);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.AddStrings(Strings: TStrings);
begin
  if not LockList then
    Exit;
  try
    FStringList.AddStrings(Strings);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Delete(Index: integer);
begin
  if not LockList then
    Exit;
  try
    FStringList.Delete(Index);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Clear;
begin
  if not LockList then
    Exit;
  try
    FStringList.Clear;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Exchange(Index1, Index2: integer);
begin
  if not LockList then
    Exit;
  try
    FStringList.Exchange(Index1, Index2);
  finally
    UnlockList;
  end;
end;

function TThreadStringList.Find(const S: string; var Index: integer): boolean;
begin
  Result := false;
  if not LockList then
    Exit;
  try
    Result := FStringList.Find(S, Index);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Insert(Index: integer; const S: string);
begin
  if not LockList then
    Exit;
  try
    FStringList.Insert(Index, S);
  finally
    UnlockList;
  end;
end;

function TThreadStringList.IndexOf(const S: string): integer;
begin
  Result := -1;
  if not LockList then
    Exit;
  LockList;
  try
    Result := FStringList.IndexOf(S);
  finally
    UnlockList;
  end;
end;

function TThreadStringList.IndexOfName(const Name: string): integer;
begin
  Result := -1;
  if not LockList then
    Exit;
  try
    Result := FStringList.IndexOfName(Name);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Sort;
begin
  if not LockList then
    Exit;
  try
    FStringList.Sort;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetText: PChar;
begin
  Result := nil;
  if not LockList then
    Exit;
  try
    Result := FStringList.GetText;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.LoadFromFile(const FileName: string);
begin
  if not LockList then
    Exit;
  try
    FStringList.LoadFromFile(FileName);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.LoadFromStream(Stream: TStream);
begin
  if not LockList then
    Exit;
  try
    FStringList.LoadFromStream(Stream);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SaveToFile(const FileName: string);
begin
  if not LockList then
    Exit;
  try
    FStringList.SaveToFile(FileName);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SaveToStream(Stream: TStream);
begin
  if not LockList then
    Exit;
  try
    FStringList.SaveToStream(Stream);
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetDuplicates: TDuplicates;
begin
  Result := dupIgnore;
  if not LockList then
    Exit;
  try
    Result := FStringList.Duplicates;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetDuplicates(dup: TDuplicates);
begin
  if not LockList then
    Exit;
  try
    FStringList.Duplicates := dup;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetCapacity: integer;
begin
  Result := 0;
  if not LockList then
    Exit;
  try
    Result := FStringList.Capacity;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetCapacity(capa: integer);
begin
  if not LockList then
    Exit;
  try
    FStringList.Capacity := capa;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetCommaText: string;
begin
  if not LockList then
    Exit;
  try
    Result := FStringList.CommaText;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetCommaText(const S: string);
begin
  if not LockList then
    Exit;
  try
    FStringList.CommaText := S;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetCount: integer;
begin
  Result := 0;
  if not LockList then
    Exit;
  try
    Result := FStringList.Count;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetDelimiter: Char;
begin
  Result := ',';
  if not LockList then
    Exit;
  try
    Result := FStringList.Delimiter;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetDelimiter(delim: Char);
begin
  if not LockList then
    Exit;
  try
    FStringList.Delimiter := delim;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetDelimitedText: string;
begin
  if not LockList then
    Exit;
  try
    Result := FStringList.DelimitedText;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetDelimitedText(const S: string);
begin
  if not LockList then
    Exit;
  try
    FStringList.DelimitedText := S;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetNames(Index: integer): string;
begin
  if not LockList then
    Exit;
  try
    Result := FStringList.Names[Index];
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetValues(const Name: string): string;
begin
  if not LockList then
    Exit;
  try
    Result := FStringList.Values[Name];
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetValues(const Name: string; S: string);
begin
  if not LockList then
    Exit;
  try
    FStringList.Values[Name] := S;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetSorted: boolean;
begin
  Result := false;
  if not LockList then
    Exit;
  try
    Result := FStringList.Sorted;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetStrings(Index: integer): string;
begin
  if not LockList then
    Exit;
  try
    Result := FStringList.Strings[Index];
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetSorted(const Value: boolean);
begin
  if not LockList then
    Exit;
  try
    if Value <> FStringList.Sorted then
    begin
      FStringList.Sorted := Value;
    end;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetStrings(Index: integer; S: string);
begin
  if not LockList then
    Exit;
  try
    FStringList.Strings[Index] := S;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetAsText: string;
begin
  if not LockList then
    Exit;
  try
    Result := FStringList.Text;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetAsText(S: string);
begin
  if not LockList then
    Exit;
  try
    FStringList.Text := S;
  finally
    UnlockList;
  end;
end;

{ TThreadTimer }

constructor TThreadTimer.Create(AOnTimer: TNotifyEvent;
  ATimeout: integer = 1000);
begin
  FTerminateEvent := TEvent.Create;
  FOnTimer := AOnTimer;
  FTimeout := ATimeout;
  inherited Create(false);
  FreeOnTerminate := True;
end;

destructor TThreadTimer.Destroy;
begin
  try
    Terminate;
    FOnTimer := nil;
    FActive := false;
    FTerminateEvent.Free;
    FTerminateEvent := nil;
  finally
    inherited;
  end;
end;

procedure TThreadTimer.SyncOnTimer;
begin
  if Assigned(FOnTimer) and (FActive) then
  begin
    try
      FOnTimer(Self);
    except
    end;
  end;
end;

procedure TThreadTimer.Terminate;
begin
  try
    FTerminateEvent.SetEvent;
  finally
    inherited;
  end;
end;

procedure TThreadTimer.Execute;
begin
  FActive := True;
  while (not Terminated) do
  begin
    try
      if not Terminated then
      begin
        Synchronize(SyncOnTimer);
      end;
      WaitForSingleObject(FTerminateEvent.Handle, FTimeout);
    except

    end;
  end;
end;

end.
