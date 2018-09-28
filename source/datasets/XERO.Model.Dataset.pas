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

  TXEROModelDataset<T: TXEROModel> = class(TXEROObject)
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
    FList: TList;
  public
    constructor Create;
    destructor Destroy;
    function Add<T: TXEROObject>(AXEROModelDataset: TXEROObject): T;
    procedure Clear;
  end;

implementation

uses
  XERO.Utils;

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

procedure TXEROModelDataset<T>.Clear;
begin
  if FDataset.Active then
  begin
    FDataset.EmptyDataSet;
  end;
end;

procedure TXEROModelDataset<T>.Close;
begin
  Clear;
  FDataset.Active := False;
  FDataset.FieldDefs.Clear;
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
  FDataset.First;
end;

{ TXEROModelDatasets }

function TXEROModelDatasets.Add<T>(AXEROModelDataset: TXEROObject): T;
begin
  Result := AXEROModelDataset as T;
  FList.Add(@Result);
end;

procedure TXEROModelDatasets.Clear;
begin
  while FList.Count > 0 do
  begin
    try
      try
        TObject(FList.Items[0]^).Free;
      except
        on E: Exception do
        begin
          Warning(Format('Failed to remove dataset, Error: %s', [E.Message]));
        end;
      end;
    finally
      FList.Delete(0);
    end;
  end;
end;

constructor TXEROModelDatasets.Create;
begin
  inherited;
  FList := TList.Create;
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
