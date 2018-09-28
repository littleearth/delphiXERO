unit XERO.Log;

interface

uses
  SysUtils, Classes, XERO.Types;

type
  TXEROLog = class(TObject)
  private
    FLogLevel: TLogLevel;
    procedure SetLogLevel(const Value: TLogLevel);
  protected
    procedure OutputToDebugger(const AMessage: String);
    function GetLogLevelText(ALogLevel: TLogLevel): string;
    function IsLogLevel(ALogLevel: TLogLevel): boolean;
  public
    constructor Create;
    procedure Log(ASender: TObject; AMessage: string); virtual;
    procedure Debug(ASender: TObject; AProcedure: string;
      AMessage: string); virtual;
    procedure Warning(ASender: TObject; AMessage: string); virtual;
    procedure Error(ASender: TObject; AMessage: string;
      AErrorCode: integer = 0); overload; virtual;
    procedure Error(ASender: TObject; AException: Exception;
      AMessage: string = ''); overload; virtual;
    property LogLevel: TLogLevel read FLogLevel write SetLogLevel;
  end;

  TXEROLogClass = class of TXEROLog;

var
  _XEROLog: TXEROLog;
  _XEROLogClass: TXEROLogClass;

procedure SetXEROLogClass(AXEROLogClass: TXEROLogClass);
function XEROLog: TXEROLog;

implementation

uses
  Winapi.Windows;

{ TXEROLog }

procedure TXEROLog.OutputToDebugger(const AMessage: String);
begin
  OutputDebugString(PChar(AMessage))
end;

procedure TXEROLog.SetLogLevel(const Value: TLogLevel);
begin
  FLogLevel := Value;
end;

constructor TXEROLog.Create;
begin
  inherited;
{$IFDEF DEBUG}
  FLogLevel := logDebug;
{$ELSE}
  FLogLevel := logError;
{$ENDIF}
end;

procedure TXEROLog.Debug(ASender: TObject; AProcedure, AMessage: string);
begin
  // OutputToDebugger('DEBUG:' + AProcedure + ': ' + AMessage);
end;

procedure TXEROLog.Error(ASender: TObject; AException: Exception;
  AMessage: string);
begin
{$IFDEF DEBUG}
  OutputToDebugger('ERROR:' + AException.Message + ': ' + AMessage);
{$ENDIF}
end;

function TXEROLog.GetLogLevelText(ALogLevel: TLogLevel): string;
begin
  case ALogLevel of
    logDebug:
      Result := 'DEBUG';
    logInformation:
      Result := 'INFO';
    logWarning:
      Result := 'WARN';
  else
    begin
      Result := 'ERROR';
    end;
  end;
end;

function TXEROLog.IsLogLevel(ALogLevel: TLogLevel): boolean;
begin
  Result := (ord(ALogLevel) <= ord(FLogLevel));
end;

procedure TXEROLog.Error(ASender: TObject; AMessage: string;
  AErrorCode: integer);
begin
{$IFDEF DEBUG}
  OutputToDebugger('ERROR:' + IntToStr(AErrorCode) + ': ' + AMessage);
{$ENDIF}
end;

procedure TXEROLog.Log(ASender: TObject; AMessage: string);
begin
  // OutputToDebugger('LOG:' + AMessage);
end;

procedure TXEROLog.Warning(ASender: TObject; AMessage: string);
begin
  // OutputToDebugger('WARN:' + AMessage);
end;

procedure SetXEROLogClass(AXEROLogClass: TXEROLogClass);
begin
  _XEROLogClass := AXEROLogClass;
end;

function XEROLog: TXEROLog;
begin
  Result := nil;
  if not Assigned(_XEROLog) then
  begin
    if Assigned(_XEROLogClass) then
    begin
      _XEROLog := _XEROLogClass.Create;
    end;
  end;
  if Assigned(_XEROLog) then
  begin
    Result := _XEROLog;
  end;
end;

end.
