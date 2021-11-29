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
    function Insert(APurchaseOrders: TXMPurchaseOrders)
      : TXEROPurchaseOrderResponse; overload;
    function Insert(APurchaseOrder: TXMPurchaseOrder)
      : TXEROPurchaseOrderResponse; overload;
    function Update(APurchaseOrders: TXMPurchaseOrders)
      : TXEROPurchaseOrderResponse; overload;
    function Update(APurchaseOrder: TXMPurchaseOrder)
      : TXEROPurchaseOrderResponse; overload;
    property summarizeErrors;
  end;

implementation

{ TXEROPurchaseOrders }

function TXEROPurchaseOrders.Search(APage: Integer;
  AOrderBy, APurchaseOrderID, APurchaseOrderNumber: string;
  ALastModified: TDateTime): TXEROPurchaseOrderResponse;
var
  LXEROFilter: TXEROFilter;
  LXEROResponseJSON: TXEROResponseJSON;
begin
  LXEROFilter := TXEROFilter.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    LXEROFilter.AddGUIDToFilter('PurchaseOrderID', APurchaseOrderID);
    LXEROFilter.AddToFilter('PurchaseOrderNumber', APurchaseOrderNumber);
    Find<TXEROResponseJSON>(LXEROResponseJSON, LXEROFilter.Text, AOrderBy,
      APage, ALastModified);
    Result := LXEROResponseJSON.ToResponse<TXEROPurchaseOrderResponse>;
  finally
    FreeAndNil(LXEROFilter);
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROPurchaseOrders.Update(APurchaseOrders: TXMPurchaseOrders)
  : TXEROPurchaseOrderResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
begin
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    summarizeErrors := false;
    Post<TXEROResponseJSON>(APurchaseOrders.AsJSONArray('PurchaseOrders'),
      LXEROResponseJSON);
    Result := LXEROResponseJSON.ToResponse<TXEROPurchaseOrderResponse>;
  finally
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROPurchaseOrders.Update(APurchaseOrder: TXMPurchaseOrder)
  : TXEROPurchaseOrderResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
begin
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    summarizeErrors := true;
    Post<TXEROResponseJSON>(APurchaseOrder.AsJSON, LXEROResponseJSON);
    Result := LXEROResponseJSON.ToResponse<TXEROPurchaseOrderResponse>;
  finally
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROPurchaseOrders.GetAPIURL: string;
begin
  Result := XERO_API_BASE_URL + 'PurchaseOrders';
end;

function TXEROPurchaseOrders.Insert(APurchaseOrders: TXMPurchaseOrders)
  : TXEROPurchaseOrderResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
begin
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    Put<TXEROResponseJSON>(APurchaseOrders.AsJSONArray('PurchaseOrders'),
      LXEROResponseJSON);
    Result := LXEROResponseJSON.ToResponse<TXEROPurchaseOrderResponse>;
  finally
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROPurchaseOrders.Insert(APurchaseOrder: TXMPurchaseOrder)
  : TXEROPurchaseOrderResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
begin
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    Put<TXEROResponseJSON>(APurchaseOrder.AsJSON, LXEROResponseJSON);
    Result := LXEROResponseJSON.ToResponse<TXEROPurchaseOrderResponse>;
  finally
    FreeAndNil(LXEROResponseJSON);
  end;
end;

end.
