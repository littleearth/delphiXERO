unit XERO.PurchaseOrders;

interface

uses
  System.SysUtils, DB,
  Classes, XERO.API, XERO.API.Response.JSON, XERO.Types, XERO.Model,
  XERO.Response.Model, XERO.Request.Model,
  XERO.PurchaseOrder.Model;

type
  EXEROPurchaseOrderException = EXEROException;

  TXEROPurchaseOrderResponse = class(TXEROResponse)
  private
    [XEROModelManagedAttribute]
    FPurchaseOrders: TXMPurchaseOrders;
  public
    property PurchaseOrders: TXMPurchaseOrders read FPurchaseOrders;
  end;

  TXEROPurchaseOrders = class(TXEROAPI)
  private
  protected
    function GetAPIURL: string; override;
  public
    function Search(APage: Integer = 0; AOrderBy: string = '';
      APurchaseOrderID: string = ''; APurchaseOrderNumber: string = '';
      ALastModified: TDateTime = 0): TXEROPurchaseOrderResponse;
    function Insert(APurchaseOrders: TXMPurchaseOrders): TXEROPurchaseOrderResponse; overload;
    function Insert(APurchaseOrder: TXMPurchaseOrder): TXEROPurchaseOrderResponse; overload;
    function Update(APurchaseOrders: TXMPurchaseOrders): TXEROPurchaseOrderResponse; overload;
    function Update(APurchaseOrder: TXMPurchaseOrder): TXEROPurchaseOrderResponse; overload;
  end;

implementation

{ TXEROPurchaseOrders }

function TXEROPurchaseOrders.Search(APage: Integer;
  AOrderBy, APurchaseOrderID, APurchaseOrderNumber: string; ALastModified: TDateTime)
  : TXEROPurchaseOrderResponse;
var
  LXEROFilter: TXEROFilter;
  LXEROResponseJSON: TXEROResponseJSON;
begin
  Result := TXEROPurchaseOrderResponse.Create;
  LXEROFilter := TXEROFilter.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    LXEROFilter.AddGUIDToFilter('PurchaseOrderID', APurchaseOrderID);
    LXEROFilter.AddToFilter('PurchaseOrderNumber', APurchaseOrderNumber);
    if Find<TXEROResponseJSON>(LXEROResponseJSON, LXEROFilter.Text, AOrderBy,
      APage, ALastModified) then
    begin
      if LXEROResponseJSON.Result then
      begin
        Result.FromJSON(LXEROResponseJSON.AsString);
      end
      else
      begin
        raise EXEROPurchaseOrderException.Create(LXEROResponseJSON.ErrorMessage);
      end;
    end;
  finally
    FreeAndNil(LXEROFilter);
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROPurchaseOrders.Update(APurchaseOrders: TXMPurchaseOrders): TXEROPurchaseOrderResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
begin
  Result := TXEROPurchaseOrderResponse.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    if Post<TXEROResponseJSON>(APurchaseOrders.AsJSONArray('PurchaseOrders'),
      LXEROResponseJSON) then
    begin
      if LXEROResponseJSON.Result then
      begin
        Result.FromJSON(LXEROResponseJSON.AsString);
      end
      else
      begin
        raise EXEROPurchaseOrderException.Create(LXEROResponseJSON.ErrorMessage);
      end;
    end;
  finally
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROPurchaseOrders.Update(APurchaseOrder: TXMPurchaseOrder): TXEROPurchaseOrderResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
begin
  Result := TXEROPurchaseOrderResponse.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    if Post<TXEROResponseJSON>(APurchaseOrder.AsJSON, LXEROResponseJSON) then
    begin
      if LXEROResponseJSON.Result then
      begin
        Result.FromJSON(LXEROResponseJSON.AsString);
      end
      else
      begin
        raise EXEROPurchaseOrderException.Create(LXEROResponseJSON.ErrorMessage);
      end;
    end;
  finally
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROPurchaseOrders.GetAPIURL: string;
begin
  Result := XERO_API_BASE_URL + 'PurchaseOrders';
end;

function TXEROPurchaseOrders.Insert(APurchaseOrders: TXMPurchaseOrders): TXEROPurchaseOrderResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
begin
  Result := TXEROPurchaseOrderResponse.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    if Put<TXEROResponseJSON>(APurchaseOrders.AsJSONArray('PurchaseOrders'),
      LXEROResponseJSON) then
    begin
      if LXEROResponseJSON.Result then
      begin
        Result.FromJSON(LXEROResponseJSON.AsString);
      end
      else
      begin
        raise EXEROPurchaseOrderException.Create(LXEROResponseJSON.ErrorMessage);
      end;
    end;
  finally
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROPurchaseOrders.Insert(APurchaseOrder: TXMPurchaseOrder): TXEROPurchaseOrderResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
begin
  Result := TXEROPurchaseOrderResponse.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    if Put<TXEROResponseJSON>(APurchaseOrder.AsJSON, LXEROResponseJSON) then
    begin
      if LXEROResponseJSON.Result then
      begin
        Result.FromJSON(LXEROResponseJSON.AsString);
      end
      else
      begin
        raise EXEROPurchaseOrderException.Create(LXEROResponseJSON.ErrorMessage);
      end;
    end;
  finally
    FreeAndNil(LXEROResponseJSON);
  end;
end;

end.
