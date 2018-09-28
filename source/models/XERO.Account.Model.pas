unit XERO.Account.Model;

interface

uses
  Classes, SysUtils, XERO.Model;

type
  TXEROAccount = class(TXEROModel)
  private
    FName: string;
    FCode: string;
    FStatus: string;
    FAccountType: string;
    FDescription: string;
    procedure SetAccountType(const Value: string);
    procedure SetCode(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetName(const Value: string);
    procedure SetStatus(const Value: string);

  public
    property Code: string read FCode write SetCode;
    property Name: string read FName write SetName;
    [XEROModelJSONPropertyNameAttribute('Type')]
    property AccountType: string read FAccountType write SetAccountType;
    property Status: string read FStatus write SetStatus;
    property Description: string read FDescription write SetDescription;
  end;

  TXMAccounts = TXEROModelList<TXEROAccount>;

implementation

{ TXEROAccount }

procedure TXEROAccount.SetAccountType(const Value: string);
begin
  FAccountType := Value;
end;

procedure TXEROAccount.SetCode(const Value: string);
begin
  FCode := Value;
end;

procedure TXEROAccount.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TXEROAccount.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TXEROAccount.SetStatus(const Value: string);
begin
  FStatus := Value;
end;

end.
