unit XERO.PurchaseOrder.Model;

interface

uses
  Classes, SysUtils, XERO.Model, XERO.Contact.Model,
  XERO.TrackingCategory.Model;

type

  TXEROPurchaseOrderStatus = (posUnspecified, posDraft, posSubmitted,
    posDeleted, posAuthorised, posBilled);

  TXMPurchaseOrderLineItem = class(TXeroModel)
  private
    [XEROModelManagedAttribute]
    FXMTrackingCategories: TXMLineItemTrackingCategories;
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
    FReference: string;
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
    procedure SetReference(const Value: string);
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
    property Reference: string read FReference write SetReference;
    property Tracking: TXMLineItemTrackingCategories read FXMTrackingCategories;
  end;

  TXMPurchaseOrderLineItems = TXEROModelList<TXMPurchaseOrderLineItem>;

  TXMPurchaseOrder = class(TXeroModel)
  private
    [XEROModelManagedAttribute]
    FXMContact: TXMContact;
    [XEROModelManagedAttribute]
    FXMPurchaseOrderLineItems: TXMPurchaseOrderLineItems;
    FDate: TDate;
    FSubTotal: single;
    FPurchaseOrderID: string;
    FPurchaseOrderNumber: string;
    FStatus: string;
    FLineAmountTypes: string;
    FDeliveryDate: TDate;
    FReference: string;
    FDeliveryInstructions: string;
    FExpectedArrivalDate: TDate;
    FTelephone: string;
    FDeliveryAddress: string;
    FAttentionTo: string;
    FTotalTax: single;
    FCurrencyCode: string;
    FTotal: single;
    FSentToContact: string;
    FHasAttachments: boolean;
    FCurrencyRate: single;
    procedure SetDate(const Value: TDate);
    procedure SetDeliveryDate(const Value: TDate);
    procedure SetPurchaseOrderID(const Value: string);
    procedure SetPurchaseOrderNumber(const Value: string);
    procedure SetLineAmountTypes(const Value: string);
    procedure SetReference(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetSubTotal(const Value: single);
    procedure SetAttentionTo(const Value: string);
    procedure SetDeliveryAddress(const Value: string);
    procedure SetDeliveryInstructions(const Value: string);
    procedure SetExpectedArrivalDate(const Value: TDate);
    procedure SetTelephone(const Value: string);
    procedure SetCurrencyCode(const Value: string);
    procedure SetTotal(const Value: single);
    procedure SetTotalTax(const Value: single);
    procedure SetSentToContact(const Value: string);
    procedure SetHasAttachments(const Value: boolean);
    procedure SetCurrencyRate(const Value: single);
  public
    class function GetPurchaseOrderStatus(APurchaseOrderStatus
      : TXEROPurchaseOrderStatus): string; static;
    property PurchaseOrderID: string read FPurchaseOrderID
      write SetPurchaseOrderID;
    property Contact: TXMContact read FXMContact;
    property Date: TDate read FDate write SetDate;
    property DeliveryDate: TDate read FDeliveryDate write SetDeliveryDate;
    property Status: string read FStatus write SetStatus;
    property LineAmountTypes: string read FLineAmountTypes
      write SetLineAmountTypes;
    property LineItems: TXMPurchaseOrderLineItems
      read FXMPurchaseOrderLineItems;
    property SubTotal: single read FSubTotal write SetSubTotal;
    property TotalTax: single read FTotalTax write SetTotalTax;
    property Total: single read FTotal write SetTotal;
    property CurrencyCode: string read FCurrencyCode write SetCurrencyCode;
    property CurrencyRate: single read FCurrencyRate write SetCurrencyRate;
    property PurchaseOrderNumber: string read FPurchaseOrderNumber
      write SetPurchaseOrderNumber;
    property Reference: string read FReference write SetReference;
    property DeliveryAddress: string read FDeliveryAddress
      write SetDeliveryAddress;
    property AttentionTo: string read FAttentionTo write SetAttentionTo;
    property Telephone: string read FTelephone write SetTelephone;
    property DeliveryInstructions: string read FDeliveryInstructions
      write SetDeliveryInstructions;
    property ExpectedArrivalDate: TDate read FExpectedArrivalDate
      write SetExpectedArrivalDate;
    property SentToContact: string read FSentToContact write SetSentToContact;
    property HasAttachments: boolean read FHasAttachments
      write SetHasAttachments;
  end;

  TXMPurchaseOrders = TXEROModelList<TXMPurchaseOrder>;

implementation

{ TXMPurchaseOrderLineItem }

procedure TXMPurchaseOrderLineItem.SetAccountCode(const Value: string);
begin
  FAccountCode := Value;
end;

procedure TXMPurchaseOrderLineItem.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TXMPurchaseOrderLineItem.SetDiscountRate(const Value: string);
begin
  FDiscountRate := Value;
end;

procedure TXMPurchaseOrderLineItem.SetItemCode(const Value: string);
begin
  FItemCode := Value;
end;

procedure TXMPurchaseOrderLineItem.SetLineAmount(const Value: single);
begin
  FLineAmount := Value;
end;

procedure TXMPurchaseOrderLineItem.SetLineItemID(const Value: string);
begin
  FLineItemID := Value;
end;

procedure TXMPurchaseOrderLineItem.SetQuantity(const Value: single);
begin
  FQuantity := Value;
end;

procedure TXMPurchaseOrderLineItem.SetReference(const Value: string);
begin
  FReference := Value;
end;

procedure TXMPurchaseOrderLineItem.SetTaxAmount(const Value: single);
begin
  FTaxAmount := Value;
end;

procedure TXMPurchaseOrderLineItem.SetTaxType(const Value: string);
begin
  FTaxType := Value;
end;

procedure TXMPurchaseOrderLineItem.SetUnitAmount(const Value: single);
begin
  FUnitAmount := Value;
end;

{ TXMPurchaseOrder }

procedure TXMPurchaseOrder.SetAttentionTo(const Value: string);
begin
  FAttentionTo := Value;
end;

procedure TXMPurchaseOrder.SetCurrencyCode(const Value: string);
begin
  FCurrencyCode := Value;
end;

procedure TXMPurchaseOrder.SetCurrencyRate(const Value: single);
begin
  FCurrencyRate := Value;
end;

procedure TXMPurchaseOrder.SetDate(const Value: TDate);
begin
  FDate := Value;
end;

procedure TXMPurchaseOrder.SetDeliveryAddress(const Value: string);
begin
  FDeliveryAddress := Value;
end;

procedure TXMPurchaseOrder.SetDeliveryDate(const Value: TDate);
begin
  FDeliveryDate := Value;
end;

procedure TXMPurchaseOrder.SetDeliveryInstructions(const Value: string);
begin
  FDeliveryInstructions := Value;
end;

procedure TXMPurchaseOrder.SetExpectedArrivalDate(const Value: TDate);
begin
  FExpectedArrivalDate := Value;
end;

procedure TXMPurchaseOrder.SetHasAttachments(const Value: boolean);
begin
  FHasAttachments := Value;
end;

procedure TXMPurchaseOrder.SetPurchaseOrderID(const Value: string);
begin
  FPurchaseOrderID := Value;
end;

procedure TXMPurchaseOrder.SetPurchaseOrderNumber(const Value: string);
begin
  FPurchaseOrderNumber := Value;
end;

procedure TXMPurchaseOrder.SetLineAmountTypes(const Value: string);
begin
  FLineAmountTypes := Value;
end;

procedure TXMPurchaseOrder.SetReference(const Value: string);
begin
  FReference := Value;
end;

procedure TXMPurchaseOrder.SetSentToContact(const Value: string);
begin
  FSentToContact := Value;
end;

procedure TXMPurchaseOrder.SetStatus(const Value: string);
begin
  FStatus := Value;
end;

procedure TXMPurchaseOrder.SetSubTotal(const Value: single);
begin
  FSubTotal := Value;
end;

procedure TXMPurchaseOrder.SetTelephone(const Value: string);
begin
  FTelephone := Value;
end;

procedure TXMPurchaseOrder.SetTotal(const Value: single);
begin
  FTotal := Value;
end;

procedure TXMPurchaseOrder.SetTotalTax(const Value: single);
begin
  FTotalTax := Value;
end;

class function TXMPurchaseOrder.GetPurchaseOrderStatus(APurchaseOrderStatus
  : TXEROPurchaseOrderStatus): string;
begin
  case APurchaseOrderStatus of
    posUnspecified:
      Result := '';
    posDraft:
      Result := 'DRAFT';
    posSubmitted:
      Result := 'SUBMITTED';
    posDeleted:
      Result := 'DELETED';
    posAuthorised:
      Result := 'AUTHORISED';
    posBilled:
      Result := 'BILLED';
  end;
end;

end.
