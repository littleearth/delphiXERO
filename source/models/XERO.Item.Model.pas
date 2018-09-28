unit XERO.Item.Model;

interface

uses
  Classes, SysUtils, XERO.Model;

type
  TXMItemPurchaseAndSale = class(TXeroModel)
  private
    FAccountCode: string;
    FUnitPrice: single;
    FTaxType: string;
    FCOGSAccountCode: string;
    procedure SetAccountCode(const Value: string);
    procedure SetCOGSAccountCode(const Value: string);
    procedure SetTaxType(const Value: string);
    procedure SetUnitPrice(const Value: single);
  public
    property UnitPrice: single read FUnitPrice write SetUnitPrice;
    property AccountCode: string read FAccountCode write SetAccountCode;
    property COGSAccountCode: string read FCOGSAccountCode
      write SetCOGSAccountCode;
    property TaxType: string read FTaxType write SetTaxType;
  end;


  TXMItem = class(TXeroModel)
  private
    FName: string;
    FItemID: string;
    FInventoryAssetAccountCode: string;
    FCode: string;
    [XEROModelManagedAttribute]
    FPurchaseDetails: TXMItemPurchaseAndSale;
    FIsSold: Boolean;
    FUpdatedDateUTC: string;
    [XEROModelManagedAttribute]
    FSalesDetails: TXMItemPurchaseAndSale;
    FIsPurchased: Boolean;
    FDescription: string;
    FTotalCostPool: single;
    FQuantityOnHand: single;
    FIsTrackedAsInventory: Boolean;
    FPurchaseDescription: string;
    procedure SetCode(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetInventoryAssetAccountCode(const Value: string);
    procedure SetIsPurchased(const Value: Boolean);
    procedure SetIsSold(const Value: Boolean);
    procedure SetIsTrackedAsInventory(const Value: Boolean);
    procedure SetItemID(const Value: string);
    procedure SetName(const Value: string);
    procedure SetPurchaseDescription(const Value: string);
    procedure SetQuantityOnHand(const Value: single);
    procedure SetTotalCostPool(const Value: single);
    procedure SetUpdatedDateUTC(const Value: string);
  public
    property ItemID: string read FItemID write SetItemID;
    property Code: string read FCode write SetCode;
    property Name: string read FName write SetName;
    property IsSold: Boolean read FIsSold write SetIsSold;
    property IsPurchased: Boolean read FIsPurchased write SetIsPurchased;
    property Description: string read FDescription write SetDescription;
    property PurchaseDescription: string read FPurchaseDescription
      write SetPurchaseDescription;
    // [XEROModelManagedAttribute]
    property PurchaseDetails: TXMItemPurchaseAndSale
      read FPurchaseDetails;
    // [XEROModelManagedAttribute]
    property SalesDetails: TXMItemPurchaseAndSale read FSalesDetails;
    property IsTrackedAsInventory: Boolean read FIsTrackedAsInventory
      write SetIsTrackedAsInventory;
    property InventoryAssetAccountCode: string read FInventoryAssetAccountCode
      write SetInventoryAssetAccountCode;
    property TotalCostPool: single read FTotalCostPool write SetTotalCostPool;
    property QuantityOnHand: single read FQuantityOnHand
      write SetQuantityOnHand;
    property UpdatedDateUTC: string read FUpdatedDateUTC
      write SetUpdatedDateUTC;
  end;

  TXMItems = TXEROModelList<TXMItem>;

implementation

{ TXMItem }

procedure TXMItem.SetCode(const Value: string);
begin
  FCode := Value;
end;

procedure TXMItem.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TXMItem.SetInventoryAssetAccountCode(const Value: string);
begin
  FInventoryAssetAccountCode := Value;
end;

procedure TXMItem.SetIsPurchased(const Value: Boolean);
begin
  FIsPurchased := Value;
end;

procedure TXMItem.SetIsSold(const Value: Boolean);
begin
  FIsSold := Value;
end;

procedure TXMItem.SetIsTrackedAsInventory(const Value: Boolean);
begin
  FIsTrackedAsInventory := Value;
end;

procedure TXMItem.SetItemID(const Value: string);
begin
  FItemID := Value;
end;

procedure TXMItem.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TXMItem.SetPurchaseDescription(const Value: string);
begin
  FPurchaseDescription := Value;
end;


procedure TXMItem.SetQuantityOnHand(const Value: single);
begin
  FQuantityOnHand := Value;
end;


procedure TXMItem.SetTotalCostPool(const Value: single);
begin
  FTotalCostPool := Value;
end;

procedure TXMItem.SetUpdatedDateUTC(const Value: string);
begin
  FUpdatedDateUTC := Value;
end;

{ TXMItemPurchaseAndSale }

procedure TXMItemPurchaseAndSale.SetAccountCode(const Value: string);
begin
  FAccountCode := Value;
end;

procedure TXMItemPurchaseAndSale.SetCOGSAccountCode(const Value: string);
begin
  FCOGSAccountCode := Value;
end;

procedure TXMItemPurchaseAndSale.SetTaxType(const Value: string);
begin
  FTaxType := Value;
end;

procedure TXMItemPurchaseAndSale.SetUnitPrice(const Value: single);
begin
  FUnitPrice := Value;
end;

end.
