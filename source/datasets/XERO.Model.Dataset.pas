unit XERO.Model.Dataset;

interface

uses
  SysUtils, Classes, DB, System.Generics.Collections,
  XERO.Types, XERO.Model,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Comp.Dataset,
  FireDAC.Comp.Client, FireDAC.Stan.Def, FireDAC.UI.Intf, FireDAC.VCLUI.Wait,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef, FireDAC.Stan.Async,
  FireDAC.DApt, FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageXML,
  FireDAC.Stan.StorageBin, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Comp.UI;

type
  EXERODatasetException = class(EXEROException);

  TXEROModelDatasetBase = class(TXEROObject);

  TXEROModelDataset<T: TXEROModel> = class(TXEROModelDatasetBase)
  private
    FDataset: TFDMemTable;
    function GetDataset: TDataset;
  protected
    procedure AddFieldDef(AFieldName: string; AFieldType: TFieldType;
      ASize: integer = 2000);
    procedure CreateModelFields; virtual;
    procedure CreateCustomField; virtual;
    procedure CreateIndexFields; virtual;
    procedure CreateFields; virtual;
    procedure StoreCustomFields(ADataset: TDataset); virtual;
    procedure AfterStoreModel(ADataset: TDataset; AModel: TXEROModel); virtual;
    procedure AfterOpen; virtual;
    procedure AfterClose; virtual;
    procedure AfterClear; virtual;
    function GetIndexFields: string; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Open;
    procedure StoreModel(AModel: T);
    procedure StoreModelList(AModelList: TXEROModelList<T>);
    procedure Clear;
    procedure Close;
    property Dataset: TDataset read GetDataset;
  end;

  TXEROModelDatasets = class(TXEROObject)
  private
    FList: TObjectList<TXEROModelDatasetBase>;
  public
    constructor Create;
    destructor Destroy; override;
    function Add<T: TXEROModelDatasetBase>(AXEROModelDataset
      : TXEROModelDatasetBase): T;
    function AddJSON(AJSON: string): TDataset;
    procedure Clear;
  end;

implementation

uses
  XERO.Utils, XERO.Response.Model,
  XERO.Contacts, XERO.Contacts.Dataset,
  XERO.Invoices, XERO.Invoices.Dataset,
  XERO.PurchaseOrders, XERO.PurchaseOrders.Dataset,
  XERO.TrackingCategory, XERO.TrackingCategory.Dataset;

{ TXEROModelDataset }

procedure TXEROModelDataset<T>.AddFieldDef(AFieldName: string;
  AFieldType: TFieldType; ASize: integer);
var
  LSize: integer;
begin
  LSize := 0;
  if AFieldType in [ftString, ftWideString, ftMemo, ftWideMemo] then
  begin
    LSize := ASize;
  end;
  FDataset.FieldDefs.Add(AFieldName, AFieldType, LSize);
end;

procedure TXEROModelDataset<T>.AfterClear;
begin

end;

procedure TXEROModelDataset<T>.AfterClose;
begin

end;

procedure TXEROModelDataset<T>.AfterOpen;
begin

end;

procedure TXEROModelDataset<T>.AfterStoreModel(ADataset: TDataset;
  AModel: TXEROModel);
begin

end;

procedure TXEROModelDataset<T>.Clear;
begin
  if FDataset.Active then
  begin
    FDataset.EmptyDataSet;
  end;
  AfterClear;
end;

procedure TXEROModelDataset<T>.Close;
begin
  Clear;
  FDataset.Active := False;
  FDataset.FieldDefs.Clear;
  AfterClose;
end;

constructor TXEROModelDataset<T>.Create;
begin
  FDataset := TFDMemTable.Create(nil);
  FDataset.FieldDefs.Clear;
  CreateFields;
end;

procedure TXEROModelDataset<T>.CreateCustomField;
begin

end;

procedure TXEROModelDataset<T>.CreateFields;
begin
  CreateModelFields;
  CreateCustomField;
end;

procedure TXEROModelDataset<T>.CreateIndexFields;
var
  LIndexField: string;
begin
  FDataset.IndexDefs.Clear;
  LIndexField := GetIndexFields;
  if not IsEmptyString(LIndexField) then
  begin
    FDataset.IndexDefs.Add('MasterDetail', LIndexField, [ixPrimary]);
    FDataset.IndexName := 'MasterDetail';
  end;
end;

procedure TXEROModelDataset<T>.CreateModelFields;
var
  LModel: T;
begin
  LModel := LModel.CreateModel<T>;
  try
    LModel.AddFieldDefs(FDataset);
  finally
    FreeAndNil(LModel);
  end;
end;

destructor TXEROModelDataset<T>.Destroy;
begin
  try
    FreeAndNil(FDataset);
  finally
    inherited;
  end;
end;

function TXEROModelDataset<T>.GetDataset: TDataset;
begin
  Result := FDataset;
end;

function TXEROModelDataset<T>.GetIndexFields: string;
begin

end;

procedure TXEROModelDataset<T>.Open;
begin
  if not FDataset.Active then
  begin
    FDataset.CreateDataSet;
    FDataset.Active := True;
    AfterOpen;
  end;
end;

procedure TXEROModelDataset<T>.StoreCustomFields(ADataset: TDataset);
begin

end;

procedure TXEROModelDataset<T>.StoreModel(AModel: T);
begin
  if not FDataset.Active then
    Open;
  if FDataset.Active then
  begin
    FDataset.Insert;
    AModel.SaveToDataset(FDataset, False);
    StoreCustomFields(FDataset);
    try
      FDataset.Post;
    except
      FDataset.Cancel;
      raise;
    end;
    AfterStoreModel(FDataset, AModel);
  end;
end;

procedure TXEROModelDataset<T>.StoreModelList(AModelList: TXEROModelList<T>);
var
  LModel: T;
begin
  for LModel in AModelList do
  begin
    StoreModel(LModel);
  end;
  if FDataset.Active then
    FDataset.First;
end;

{ TXEROModelDatasets }

function TXEROModelDatasets.Add<T>(AXEROModelDataset: TXEROModelDatasetBase): T;
begin
  FList.Add(AXEROModelDataset);
end;

function TXEROModelDatasets.AddJSON(AJSON: string): TDataset;
var
  LResponse: TXEROResponse;
begin
  Result := nil;
  if (Pos('"Contacts"', AJSON) > 0) and (not Assigned(Result)) then
  begin
    LResponse := TXEROContactResponse.Create;
    try
      LResponse.FromJSON(AJSON);

      with Add<TXEROContactDataset>(TXEROContactDataset.Create) do
      begin
        StoreModelList((LResponse as TXEROContactResponse).Contacts);
        Result := Dataset;
      end;
    finally
      FreeAndNil(LResponse);
    end;
  end;

  if (Pos('"Invoices"', AJSON) > 0) and (not Assigned(Result)) then
  begin
    LResponse := TXEROInvoiceResponse.Create;
    try
      LResponse.FromJSON(AJSON);

      with Add<TXEROInvoicesDataset>(TXEROInvoicesDataset.Create) do
      begin
        StoreModelList((LResponse as TXEROInvoiceResponse).Invoices);
        Result := Dataset;
      end;
    finally
      FreeAndNil(LResponse);
    end;
  end;

  if (Pos('"PurchaseOrders"', AJSON) > 0) and (not Assigned(Result)) then
  begin
    LResponse := TXEROPurchaseOrderResponse.Create;
    try
      LResponse.FromJSON(AJSON);

      with Add<TXEROPurchaseOrdersDataset>(TXEROPurchaseOrdersDataset.Create) do
      begin
        StoreModelList((LResponse as TXEROPurchaseOrderResponse)
          .PurchaseOrders);
        Result := Dataset;
      end;
    finally
      FreeAndNil(LResponse);
    end;
  end;

  if (Pos('"TrackingCategories"', AJSON) > 0) and (not Assigned(Result)) then
  begin
    LResponse := TXEROTrackingCategoriesResponse.Create;
    try
      LResponse.FromJSON(AJSON);

      with Add<TXEROTrackingCagegoryDataset>
        (TXEROTrackingCagegoryDataset.Create) do
      begin
        StoreModelList((LResponse as TXEROTrackingCategoriesResponse)
          .TrackingCategories);
        Result := Dataset;
      end;
    finally
      FreeAndNil(LResponse);
    end;
  end;

end;

procedure TXEROModelDatasets.Clear;
begin
  try
    FList.Clear;
  except
    on E: Exception do
    begin
      Error(E);
    end;
  end;
end;

constructor TXEROModelDatasets.Create;
begin
  inherited;
  FList := TObjectList<TXEROModelDatasetBase>.Create;
end;

destructor TXEROModelDatasets.Destroy;
begin
  try
    Clear;
    FreeAndNil(FList);
  finally
    inherited;
  end;
end;

end.
