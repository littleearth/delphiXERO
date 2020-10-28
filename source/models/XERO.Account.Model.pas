unit XERO.Account.Model;

interface

uses
  Classes, SysUtils, XERO.Model;

type
  TXMAccount = class(TXEROModel)
  private
    FName: string;
    FCode: string;
    FStatus: string;
    FAccountType: string;
    FDescription: string;
    FAccountID: string;
    procedure SetAccountType(const Value: string);
    procedure SetCode(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetName(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetAccountID(const Value: string);

  public
    property AccountID: string read FAccountID write SetAccountID;
    property Code: string read FCode write SetCode;
    property Name: string read FName write SetName;
    [XEROModelJSONPropertyNameAttribute('Type')]
    property AccountType: string read FAccountType write SetAccountType;
    property Status: string read FStatus write SetStatus;
    property Description: string read FDescription write SetDescription;
  end;

  TXMAccounts = TXEROModelList<TXMAccount>;

implementation

{ TXMAccount }

procedure TXMAccount.SetAccountID(const Value: string);
begin
  FAccountID := Value;
end;

procedure TXMAccount.SetAccountType(const Value: string);
begin
  FAccountType := Value;
end;

procedure TXMAccount.SetCode(const Value: string);
begin
  FCode := Value;
end;

procedure TXMAccount.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TXMAccount.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TXMAccount.SetStatus(const Value: string);
begin
  FStatus := Value;
end;

end.
