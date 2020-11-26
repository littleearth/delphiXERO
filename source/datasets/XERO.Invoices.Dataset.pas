unit XERO.Invoices.Dataset;

interface

uses
  System.SysUtils, DB,
  Classes, XERO.API, XERO.Model.Dataset, XERO.Model, XERO.Invoice.Model,
  XERO.Invoices;

type
  TXEROInvoiceLineItemDataset = class(TXEROModelDataset<TXMInvoiceLineItem>)
  protected
    function GetIndexFields: string; override;
  end;

  TXEROInvoicesDataset = class(TXEROModelDataset<TXMInvoice>)
  private
    FItems: TXEROInvoiceLineItemDataset;
    procedure SetItems(const Value: TXEROInvoiceLineItemDataset);
  protected
    function GetIndexFields: string; override;
    procedure AfterStoreModel(ADataset: TDataset; AModel: TXeroModel); override;
    procedure AfterOpen; override;
    procedure AfterClose; override;
    procedure AfterClear; override;
  public
    property Items: TXEROInvoiceLineItemDataset read FItems write SetItems;
  end;

implementation

{ TXEROContactDataset }

procedure TXEROInvoicesDataset.AfterClear;
begin
  inherited;
  if Assigned(FItems) then
  begin
    FItems.Clear;
  end;
end;

procedure TXEROInvoicesDataset.AfterClose;
begin
  inherited;
  if Assigned(FItems) then
  begin
    FItems.Close;
  end;
end;

procedure TXEROInvoicesDataset.AfterOpen;
begin
  inherited;
  if not Assigned(FItems) then
  begin
    FItems := TXEROInvoiceLineItemDataset.Create;
  end
  else
  begin
    FItems.Close;
    FItems.CreateFields;
    FItems.Open;
  end;
end;

procedure TXEROInvoicesDataset.AfterStoreModel(ADataset: TDataset;
  AModel: TXeroModel);
begin
  inherited;
//  if Assigned(FItems) then
//  begin
//    FItems.StoreModelList((AModel as TXMInvoice).LineItems);
//  end;
end;

function TXEROInvoicesDataset.GetIndexFields: string;
begin
  Result := 'InvoiceID';
end;

{ TXEROInvoiceLineItemDataset }

function TXEROInvoiceLineItemDataset.GetIndexFields: string;
begin
  Result := 'LineItemID';
end;

procedure TXEROInvoicesDataset.SetItems(const Value
  : TXEROInvoiceLineItemDataset);
begin
  FItems := Value;
end;

end.
