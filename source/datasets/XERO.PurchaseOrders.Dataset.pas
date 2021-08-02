unit XERO.PurchaseOrders.Dataset;

interface

uses
  System.SysUtils, DB,
  Classes, XERO.API, XERO.Model.Dataset, XERO.Model, XERO.PurchaseOrder.Model,
  XERO.PurchaseOrders;

type
  TXEROPurchaseOrderLineItemDataset = class(TXEROModelDataset<TXMPurchaseOrderLineItem>)
  protected
    function GetIndexFields: string; override;
  end;

  TXEROPurchaseOrdersDataset = class(TXEROModelDataset<TXMPurchaseOrder>)
  private
    FItems: TXEROPurchaseOrderLineItemDataset;
    procedure SetItems(const Value: TXEROPurchaseOrderLineItemDataset);
  protected
    function GetIndexFields: string; override;
    procedure AfterStoreModel(ADataset: TDataset; AModel: TXeroModel); override;
    procedure AfterOpen; override;
    procedure AfterClose; override;
    procedure AfterClear; override;
  public
    property Items: TXEROPurchaseOrderLineItemDataset read FItems write SetItems;
  end;

implementation

{ TXEROContactDataset }

procedure TXEROPurchaseOrdersDataset.AfterClear;
begin
  inherited;
  if Assigned(FItems) then
  begin
    FItems.Clear;
  end;
end;

procedure TXEROPurchaseOrdersDataset.AfterClose;
begin
  inherited;
  if Assigned(FItems) then
  begin
    FItems.Close;
  end;
end;

procedure TXEROPurchaseOrdersDataset.AfterOpen;
begin
  inherited;
  if not Assigned(FItems) then
  begin
    FItems := TXEROPurchaseOrderLineItemDataset.Create;
  end
  else
  begin
    FItems.Close;
    FItems.CreateFields;
    FItems.Open;
  end;
end;

procedure TXEROPurchaseOrdersDataset.AfterStoreModel(ADataset: TDataset;
  AModel: TXeroModel);
begin
  inherited;
//  if Assigned(FItems) then
//  begin
//    FItems.StoreModelList((AModel as TXMPurchaseOrder).LineItems);
//  end;
end;

function TXEROPurchaseOrdersDataset.GetIndexFields: string;
begin
  Result := 'PurchaseOrderID';
end;

{ TXEROPurchaseOrderLineItemDataset }

function TXEROPurchaseOrderLineItemDataset.GetIndexFields: string;
begin
  Result := 'LineItemID';
end;

procedure TXEROPurchaseOrdersDataset.SetItems(const Value
  : TXEROPurchaseOrderLineItemDataset);
begin
  FItems := Value;
end;

end.
