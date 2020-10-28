unit XERO.Invoice.Model;

interface

uses
  Classes, SysUtils, XERO.Model, XERO.Contact.Model;

type

  TXEROInvoiceStatus = (isUnspecified, isDraft, isSubmitted, isDeleted,
    isAuthorised, isPaid, isVoided);

  TXEROInvoiceType = (itUnspecified, itAccPay, itAccRec);

  TXMInvoiceLineItem = class(TXeroModel)
  private
    FAccountCode: string;
    FLineAmount: single;
    FTaxAmount: single;
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
    procedure SetLineAmount(const Value: single);
    procedure SetLineItemID(const Value: string);
    procedure SetQuantity(const Value: single);
    procedure SetTaxAmount(const Value: single);
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
    property TaxAmount: single read FTaxAmount write SetTaxAmount;
    property LineAmount: single read FLineAmount write SetLineAmount;
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
    FDate: TDate;
    FAmountPaid: single;
    FSubTotal: single;
    FAmountDue: single;
    FInvoiceID: string;
    FInvoiceNumber: string;
    FStatus: string;
    FInvoiceType: string;
    FLineAmountTypes: string;
    FDueDate: TDate;
    FReference: string;
    procedure SetAmountDue(const Value: single);
    procedure SetAmountPaid(const Value: single);
    procedure SetDate(const Value: TDate);
    procedure SetDueDate(const Value: TDate);
    procedure SetInvoiceID(const Value: string);
    procedure SetInvoiceNumber(const Value: string);
    procedure SetInvoiceType(const Value: string);
    procedure SetLineAmountTypes(const Value: string);
    procedure SetReference(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetSubTotal(const Value: single);
  public
    class function GetInvoiceStatus(AInvoiceStatus: TXEROInvoiceStatus)
      : string; static;
    class function GetInvoiceType(AInvoiceType: TXEROInvoiceType)
      : string; static;
    [XEROModelJSONPropertyNameAttribute('Type')]
    property InvoiceType: string read FInvoiceType write SetInvoiceType;
    property Contact: TXMContact read FXMContact;
    property Date: TDate read FDate write SetDate;
    property DueDate: TDate read FDueDate write SetDueDate;
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

procedure TXMInvoiceLineItem.SetLineAmount(const Value: single);
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

procedure TXMInvoiceLineItem.SetTaxAmount(const Value: single);
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

procedure TXMInvoice.SetDate(const Value: TDate);
begin
  FDate := Value;
end;

procedure TXMInvoice.SetDueDate(const Value: TDate);
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

class function TXMInvoice.GetInvoiceStatus(AInvoiceStatus
  : TXEROInvoiceStatus): string;
begin
  case AInvoiceStatus of
    isUnspecified:
      Result := '';
    isDraft:
      Result := 'DRAFT';
    isSubmitted:
      Result := 'SUBMITTED';
    isDeleted:
      Result := 'DELETED';
    isAuthorised:
      Result := 'AUTHORISED';
    isPaid:
      Result := 'PAID';
    isVoided:
      Result := 'VOIDED';
  end;
end;

class function TXMInvoice.GetInvoiceType(AInvoiceType
  : TXEROInvoiceType): string;
begin
  case AInvoiceType of
    itUnspecified:
      Result := '';
    itAccPay:
      Result := 'ACCPAY';
    itAccRec:
      Result := 'ACCREC';
  end;
end;

end.
