unit XERO.Accounts;

interface

uses
  System.SysUtils, DB,
  Classes, XERO.API, XERO.Types, XERO.API.Response.JSON, XERO.Model,
  XERO.Response.Model, XERO.Request.Model,
  XERO.Account.Model;

type
  EXEROAccountException = EXEROException;

  TXEROAccountResponse = class(TXEROResponse)
  private
    [XEROModelManagedAttribute]
    FAccounts: TXMAccounts;
  public
    property Accounts: TXMAccounts read FAccounts;
  end;

  TXEROAccounts = class(TXEROAPI)
  private
  protected
    function GetAPIURL: string; override;
  public
    function Search(APage: Integer = 0; AOrderBy: string = '';
      AAccountID: string = ''; ACode: string = ''; ALastModified: TDateTime = 0)
      : TXEROAccountResponse;
  end;

implementation

{ TXEROAccounts }

function TXEROAccounts.Search(APage: Integer;
  AOrderBy, AAccountID, ACode: string; ALastModified: TDateTime)
  : TXEROAccountResponse;
var
  LXEROFilter: TXEROFilter;
  LXEROResponseJSON: TXEROResponseJSON;
begin
  LXEROFilter := TXEROFilter.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    LXEROFilter.AddGUIDToFilter('AccountID', AAccountID);
    LXEROFilter.AddToFilter('Code', ACode);
    Find<TXEROResponseJSON>(LXEROResponseJSON, LXEROFilter.Text, AOrderBy,
      APage, ALastModified);
    Result := LXEROResponseJSON.ToResponse<TXEROAccountResponse>;
  finally
    FreeAndNil(LXEROFilter);
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROAccounts.GetAPIURL: string;
begin
  Result := XERO_API_BASE_URL + 'Accounts';
end;

end.
