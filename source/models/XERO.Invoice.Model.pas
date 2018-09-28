unit XERO.Invoice.Model;

interface

uses
  Classes, SysUtils, XERO.Model, XERO.Contact.Model;

type
  TXMInvoiceLineItem = class(TXeroModel)
  private
    FAccountCode: string;
    FLineAmount: string;
    FTaxAmount: string;
    FTaxType: string;
    FUnitAmount: single;
    FQuantity: single;
    FDescription: string;
    FLineItemID: string;
    FItemCode: string;
    FDiscountRate: string;
    procedure SetAccountCode(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetDiscountRate(const Value: string);
    procedure SetItemCode(const Value: string);
    procedure SetLineAmount(const Value: string);
    procedure SetLineItemID(const Value: string);
    procedure SetQuantity(const Value: single);
    procedure SetTaxAmount(const Value: string);
    procedure SetTaxType(const Value: string);
    procedure SetUnitAmount(const Value: single);
  public
    property Description: string read FDescription write SetDescription;
    property Quantity: single read FQuantity write SetQuantity;
    property UnitAmount: single read FUnitAmount write SetUnitAmount;
    property ItemCode: string read FItemCode write SetItemCode;
    property AccountCode: string read FAccountCode write SetAccountCode;
    property LineItemID: string read FLineItemID write SetLineItemID;
    property TaxType: string read FTaxType write SetTaxType;
    property TaxAmount: string read FTaxAmount write SetTaxAmount;
    property LineAmount: string read FLineAmount write SetLineAmount;
    property DiscountRate: string read FDiscountRate write SetDiscountRate;
    // property Tracking;
  end;

  TXMInvoiceLineItems = TXEROModelList<TXMInvoiceLineItem>;

  TXMInvoice = class(TXeroModel)
  private
    [XEROModelManagedAttribute]
    FXMContact: TXMContact;
    [XEROModelManagedAttribute]
    FXMInvoiceLineItems: TXMInvoiceLineItems;
    FDate: string;
    FAmountPaid: single;
    FSubTotal: single;
    FAmountDue: single;
    FInvoiceID: string;
    FInvoiceNumber: string;
    FStatus: string;
    FInvoiceType: string;
    FLineAmountTypes: string;
    FDueDate: string;
    FReference: string;
    procedure SetAmountDue(const Value: single);
    procedure SetAmountPaid(const Value: single);
    procedure Sestring(const Value: string);
    procedure SetDueDate(const Value: string);
    procedure SetInvoiceID(const Value: string);
    procedure SetInvoiceNumber(const Value: string);
    procedure SetInvoiceType(const Value: string);
    procedure SetLineAmountTypes(const Value: string);
    procedure SetReference(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetSubTotal(const Value: single);
  public
    [XEROModelJSONPropertyNameAttribute('Type')]
    property InvoiceType: string read FInvoiceType write SetInvoiceType;
    property Contact: TXMContact read FXMContact;
    property Date: string read FDate write Sestring;
    property DueDate: string read FDueDate write SetDueDate;
    property Status: string read FStatus write SetStatus;
    property LineAmountTypes: string read FLineAmountTypes
      write SetLineAmountTypes;
    property LineItems: TXMInvoiceLineItems read FXMInvoiceLineItems;
    property SubTotal: single read FSubTotal write SetSubTotal;
    property InvoiceID: string read FInvoiceID write SetInvoiceID;
    property InvoiceNumber: string read FInvoiceNumber write SetInvoiceNumber;
    property Reference: string read FReference write SetReference;
    property AmountDue: single read FAmountDue write SetAmountDue;
    property AmountPaid: single read FAmountPaid write SetAmountPaid;
  end;

  TXMInvoices = TXEROModelList<TXMInvoice>;

implementation

{ TXMInvoiceLineItem }

procedure TXMInvoiceLineItem.SetAccountCode(const Value: string);
begin
  FAccountCode := Value;
end;

procedure TXMInvoiceLineItem.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TXMInvoiceLineItem.SetDiscountRate(const Value: string);
begin
  FDiscountRate := Value;
end;

procedure TXMInvoiceLineItem.SetItemCode(const Value: string);
begin
  FItemCode := Value;
end;

procedure TXMInvoiceLineItem.SetLineAmount(const Value: string);
begin
  FLineAmount := Value;
end;

procedure TXMInvoiceLineItem.SetLineItemID(const Value: string);
begin
  FLineItemID := Value;
end;

procedure TXMInvoiceLineItem.SetQuantity(const Value: single);
begin
  FQuantity := Value;
end;

procedure TXMInvoiceLineItem.SetTaxAmount(const Value: string);
begin
  FTaxAmount := Value;
end;

procedure TXMInvoiceLineItem.SetTaxType(const Value: string);
begin
  FTaxType := Value;
end;

procedure TXMInvoiceLineItem.SetUnitAmount(const Value: single);
begin
  FUnitAmount := Value;
end;

{ TXMInvoice }

procedure TXMInvoice.SetAmountDue(const Value: single);
begin
  FAmountDue := Value;
end;

procedure TXMInvoice.SetAmountPaid(const Value: single);
begin
  FAmountPaid := Value;
end;

procedure TXMInvoice.Sestring(const Value: string);
begin
  FDate := Value;
end;

procedure TXMInvoice.SetDueDate(const Value: string);
begin
  FDueDate := Value;
end;

procedure TXMInvoice.SetInvoiceID(const Value: string);
begin
  FInvoiceID := Value;
end;

procedure TXMInvoice.SetInvoiceNumber(const Value: string);
begin
  FInvoiceNumber := Value;
end;

procedure TXMInvoice.SetInvoiceType(const Value: string);
begin
  FInvoiceType := Value;
end;

procedure TXMInvoice.SetLineAmountTypes(const Value: string);
begin
  FLineAmountTypes := Value;
end;

procedure TXMInvoice.SetReference(const Value: string);
begin
  FReference := Value;
end;

procedure TXMInvoice.SetStatus(const Value: string);
begin
  FStatus := Value;
end;

procedure TXMInvoice.SetSubTotal(const Value: single);
begin
  FSubTotal := Value;
end;

end.
