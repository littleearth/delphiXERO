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
  Result := TXEROItemResponse.Create;
  LXEROFilter := TXEROFilter.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    LXEROFilter.AddGUIDToFilter('ContactID', AItemID);
    LXEROFilter.AddToFilter('Code', ACode);
    if Find<TXEROResponseJSON>(LXEROResponseJSON, LXEROFilter.Text, AOrderBy,
      APage, ALastModified) then
    begin
      if LXEROResponseJSON.Result then
      begin
        Result.FromJSON(LXEROResponseJSON.AsString);
      end
      else
      begin
        raise EXEROContactException.Create(LXEROResponseJSON.ErrorMessage);
      end;
    end;
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
