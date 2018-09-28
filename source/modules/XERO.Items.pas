unit XERO.Items;

interface

uses
  System.SysUtils, DB,
  Classes, XERO.API, XERO.Types, XERO.API.Response.JSON, XERO.Model, XERO.Response.Model,
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
    FXEROResponseJSON: TXEROResponseJSON;
  protected
    function GetAPIURL: string; override;
    property XEROResponseJSON: TXEROResponseJSON read FXEROResponseJSON;
  public
    procedure AfterConstruction; override;
    function Search(APage: Integer = 0; AOrderBy: string = '';
      AItemID: string = ''; ACode: string = ''; ALastModified: TDateTime = 0)
      : TXEROItemResponse;
  end;

implementation

{ TXEROItems }

function TXEROItems.Search(APage: Integer; AOrderBy, AItemID, ACode: string;
  ALastModified: TDateTime): TXEROItemResponse;
begin
  Result := TXEROItemResponse.Create;
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

procedure TXEROItems.AfterConstruction;
begin
  inherited;
  FXEROResponseJSON := TXEROResponseJSON.Create(nil);
  Response := FXEROResponseJSON;
end;

function TXEROItems.GetAPIURL: string;
begin
  Result := XERO_API_BASE_URL + 'Items';
end;

end.
