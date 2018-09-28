unit XERO.Log.Basic;

interface

uses
  SysUtils, Classes, XERO.Log, XERO.Types;

type
  TXEROLogBasic = class(TXEROLog)
  private
    FLog: TThreadStringList;
  protected
    procedure LogMessage(ALogLevel: TLogLevel; AMessage: string);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Log(ASender: TObject; AMessage: string); override;
    procedure Debug(ASender: TObject; AProcedure: string;
      AMessage: string); override;
    procedure Warning(ASender: TObject; AMessage: string); override;
    procedure Error(ASender: TObject; AMessage: string;
      AErrorCode: integer = 0); overload; override;
    procedure Error(ASender: TObject; AException: Exception;
      AMessage: string = ''); overload; override;
    function LogText: string;
  end;

implementation

{ TXEROLogBasic }

procedure TXEROLogBasic.AfterConstruction;
begin
  inherited;
  FLog := TThreadStringList.Create;
  FLog.Sorted := False;
end;

procedure TXEROLogBasic.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FLog);
end;

procedure TXEROLogBasic.Debug(ASender: TObject; AProcedure, AMessage: string);
begin
  LogMessage(logDebug, Format('[%s] %s', [AProcedure, AMessage]));
end;

procedure TXEROLogBasic.Error(ASender: TObject; AException: Exception;
  AMessage: string);
begin
  LogMessage(logError, Format('%s %s', [AException.Message, AMessage]));
end;

procedure TXEROLogBasic.Error(ASender: TObject; AMessage: string;
  AErrorCode: integer);
begin
  LogMessage(logError, Format('(%d) %s', [AErrorCode, AMessage]));
end;

procedure TXEROLogBasic.Log(ASender: TObject; AMessage: string);
begin
  LogMessage(logInformation, AMessage);

end;

procedure TXEROLogBasic.LogMessage(ALogLevel: TLogLevel; AMessage: string);
begin
  if IsLogLevel(ALogLevel) then
  begin
    FLog.Insert(0, GetLogLevelText(ALogLevel) + ':' + AMessage);
  end;
end;

function TXEROLogBasic.LogText: string;
begin
  if Assigned(FLog) then
    Result := FLog.Text;

end;

procedure TXEROLogBasic.Warning(ASender: TObject; AMessage: string);
begin
  LogMessage(logWarning, AMessage);
end;

end.
