unit XERO.Accounts;

interface

uses
  System.SysUtils, DB,
  Classes, XERO.API, XERO.Types, XERO.API.Response.JSON, XERO.Model, XERO.Response.Model,
  XERO.Account.Model;

type
  EXEROContactException = EXEROException;

  TXEROAccountResponse = class(TXEROResponse)
  private
    [XEROModelManagedAttribute]
    FAccounts: TXMAccounts;
  public
    property Accounts: TXMAccounts read FAccounts;
  end;

  TXEROAccounts = class(TXEROAPI)
  private
    FXEROResponseJSON: TXEROResponseJSON;
  protected
    function GetAPIURL: string; override;
    property XEROResponseJSON: TXEROResponseJSON read FXEROResponseJSON;
  public
    procedure AfterConstruction; override;
    function Search(APage: Integer = 0; AOrderBy: string = '';
      AItemID: string = ''; ACode: string = ''; ALastModified: TDateTime = 0)
      : TXEROAccountResponse;
  end;

implementation

{ TXEROAccounts }

function TXEROAccounts.Search(APage: Integer; AOrderBy, AItemID, ACode: string;
  ALastModified: TDateTime): TXEROAccountResponse;
begin
  Result := TXEROAccountResponse.Create;
  ResetFilter;
  AddGUIDToFilter('ContactID', AItemID);
  AddToFilter('Code', ACode);
  if Find(Filter, AOrderBy, APage, ALastModified) then
  begin
    if XEROResponseJSON.Result then
    begin
      Result.FromJSON(XEROResponseJSON.AsString);
    end
    else
    begin
      raise EXEROContactException.Create(XEROResponseJSON.ErrorMessage);
    end;
  end;
end;

procedure TXEROAccounts.AfterConstruction;
begin
  inherited;
  FXEROResponseJSON := TXEROResponseJSON.Create(nil);
  Response := FXEROResponseJSON;
end;

function TXEROAccounts.GetAPIURL: string;
begin
  Result := XERO_API_BASE_URL + 'Accounts';
end;

end.
