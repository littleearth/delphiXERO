unit XERO.Items;

interface

uses
  System.SysUtils, DB,
  Classes, XERO.API, XERO.Types, XERO.API.Response.JSON, XERO.Model,
  XERO.Response.Model, XERO.Request.Model,
  XERO.Item.Model;

type
  EXEROContactException = EXEROException;

  TXEROItemResponse = class(TXEROResponse)
  private
    [XEROModelManagedAttribute]
    FItems: TXMItems;
  public
    property Items: TXMItems read FItems;
  end;

  TXEROItems = class(TXEROAPI)
  private
  protected
    function GetAPIURL: string; override;
  public
    function Search(APage: Integer = 0; AOrderBy: string = '';
      AItemID: string = ''; ACode: string = ''; ALastModified: TDateTime = 0)
      : TXEROItemResponse;
  end;

implementation

{ TXEROItems }

function TXEROItems.Search(APage: Integer; AOrderBy, AItemID, ACode: string;
  ALastModified: TDateTime): TXEROItemResponse;
var
  LXEROFilter: TXEROFilter;
  LXEROResponseJSON: TXEROResponseJSON;
begin
  LXEROFilter := TXEROFilter.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    LXEROFilter.AddGUIDToFilter('ContactID', AItemID);
    LXEROFilter.AddToFilter('Code', ACode);
    Find<TXEROResponseJSON>(LXEROResponseJSON, LXEROFilter.Text, AOrderBy,
      APage, ALastModified);
    Result := LXEROResponseJSON.ToResponse<TXEROItemResponse>;
  finally
    FreeAndNil(LXEROFilter);
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROItems.GetAPIURL: string;
begin
  Result := XERO_API_BASE_URL + 'Items';
end;

end.
