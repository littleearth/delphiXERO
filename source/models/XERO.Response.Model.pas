unit XERO.Response.Model;

interface

uses
  Classes, SysUtils, XERO.Model;

type
  TXEROResponse = class(TXEROModel)
  private
    FProviderName: string;
    FID: string;
    FStatus: string;
    FDateTimeUTC: string;
    procedure SetDateTimeUTC(const Value: string);
    procedure SetID(const Value: string);
    procedure SetProviderName(const Value: string);
    procedure SetStatus(const Value: string);
  public
    property ID: string read FID write SetID;
    property Status: string read FStatus write SetStatus;
    property ProviderName: string read FProviderName write SetProviderName;
    property DateTimeUTC: string read FDateTimeUTC write SetDateTimeUTC;
  end;

implementation

{ TXEROResponseModel }

procedure TXEROResponse.SetDateTimeUTC(const Value: string);
begin
  FDateTimeUTC := Value;
end;

procedure TXEROResponse.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TXEROResponse.SetProviderName(const Value: string);
begin
  FProviderName := Value;
end;

procedure TXEROResponse.SetStatus(const Value: string);
begin
  FStatus := Value;
end;

end.
