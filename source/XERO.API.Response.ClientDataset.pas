unit XERO.API.Response.ClientDataset;

interface

uses
  System.Classes, System.SysUtils,
  XERO.API, DB, DBClient, XERO.XMLParser;

type
  EXEROResponseDataset = class(Exception);

type
  TOnCustomFields = procedure(ASender: TObject; const AFields: TStrings)
    of object;
  TOnGetFieldInformation = procedure(ASender: TObject; const AFieldName: string;
    var AFieldType: TFieldType; var ASize: Integer) of object;

type
  TXEROResponseDatasetBase = class(TXEROResponseBase)
  private
    FMasterDataSet: TClientDataSet;
    FMasterSource: TDataSource;
    FDetailDataset: TClientDataSet;
    FMasterListNodeName: string;
    FMasterNodeName: string;
    FDetailListNodeName: string;
    FDetailNodeName: string;
    procedure XMLToDataset(AXML: string; var AResult: Boolean;
      var AErrorMessage: string);
  protected
    procedure CreateDataset(AClientDataset: TClientDataSet);
    procedure DestroyDataset(AClientDataset: TClientDataSet);
    procedure AddDatasetFields(AClientDataset: TClientDataSet;
      AXMLNode: TXmlNode); overload;
    procedure AddDatasetFields(AClientDataset: TClientDataSet;
      AFields: TStrings); overload;
    procedure AddMasterDatasetFields(AXMLNode: TXmlNode);
    procedure AddDetailDatasetFields(AXMLNode: TXmlNode);
    procedure AddDetailMasterDatasetFields;
    procedure AddCustomMasterDatasetFields; virtual; abstract;
    procedure AddCustomDetailDatasetFields; virtual; abstract;
    procedure GetDefaultValues; virtual; abstract;
    procedure GetFieldInformation(AFieldName: string;
      var AFieldType: TFieldType; var ASize: Integer); virtual;
    function ImportXMLData(AXMLNodeList: TXmlNodeList;
      AErrorLog: TStrings): Boolean;
    procedure SetResponse( AResult :boolean; ACode : Integer; ADetail : String); override;
    function GetMasterFields: string;
    procedure SetMasterFields(AValue: string);
    property MasterListNodeName: string read FMasterListNodeName
      write FMasterListNodeName;
    property MasterNodeName: string read FMasterNodeName write FMasterNodeName;
    property DetailListNodeName: string read FDetailListNodeName
      write FDetailListNodeName;
    property DetailNodeName: string read FDetailNodeName write FDetailNodeName;
    property MasterFields: string read GetMasterFields write SetMasterFields;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MasterDataset: TClientDataSet read FMasterDataSet;
    property DetailDataset: TClientDataSet read FDetailDataset;
  end;

type
  TXEROResponseDataset = class(TXEROResponseDatasetBase)
  private
    FOnCustomMasterDatasetFields: TOnCustomFields;
    FOnCustomDetailDatasetFields: TOnCustomFields;
    FOnGetFieldInformation: TOnGetFieldInformation;
  protected
    procedure AddCustomMasterDatasetFields; override;
    procedure AddCustomDetailDatasetFields; override;
    procedure GetFieldInformation(AFieldName: string;
      var AFieldType: TFieldType; var ASize: Integer); override;
    procedure GetDefaultValues; override;
  published
    property MasterListNodeName;
    property MasterNodeName;
    property DetailListNodeName;
    property DetailNodeName;
    property MasterFields;
    property OnCustomMasterDatasetFields: TOnCustomFields
      read FOnCustomMasterDatasetFields write FOnCustomMasterDatasetFields;
    property OnCustomDetailDatasetFields: TOnCustomFields
      read FOnCustomDetailDatasetFields write FOnCustomDetailDatasetFields;
    property OnGetFieldInformation: TOnGetFieldInformation
      read FOnGetFieldInformation write FOnGetFieldInformation;
  end;

implementation

uses
  StrUtils, XERO.Utils;

constructor TXEROResponseDatasetBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMasterDataSet := TClientDataSet.Create(nil);
  FDetailDataset := TClientDataSet.Create(nil);
  FMasterSource := TDataSource.Create(nil);
  FMasterSource.DataSet := FMasterDataSet;
  FDetailDataset.MasterSource := FMasterSource;
  GetDefaultValues;
end;

destructor TXEROResponseDatasetBase.Destroy;
begin
  FreeAndNil(FMasterSource);
  FreeAndNil(FMasterDataSet);
  FreeAndNil(FDetailDataset);
  inherited Destroy;
end;

procedure TXEROResponseDatasetBase.CreateDataset(AClientDataset
  : TClientDataSet);
begin
  if not AClientDataset.Active then
  begin
    if AClientDataset.FieldDefs.Count > 0 then
    begin
      AClientDataset.CreateDataset;
      try
        AClientDataset.Open;
      except
        DestroyDataset(AClientDataset);
      end;
    end;
  end;
end;

procedure TXEROResponseDatasetBase.DestroyDataset(AClientDataset
  : TClientDataSet);
begin
  if Assigned(AClientDataset) then
  begin
    if AClientDataset.Active then
    begin
      AClientDataset.EmptyDataSet;
    end;
    AClientDataset.Active := False;
    AClientDataset.FieldDefs.Clear;
  end;
end;

procedure TXEROResponseDatasetBase.AddDatasetFields(AClientDataset
  : TClientDataSet; AXMLNode: TXmlNode);

var
  Fields: TStringList;
  Idx, ChildIdx: Integer;
  FieldName: string;

begin
  Fields := TStringList.Create;
  try
    if Assigned(AXMLNode) then
    begin
      for Idx := 0 to Pred(AXMLNode.ChildNodes.Count) do
      begin
        FieldName := AXMLNode.ChildNodes[Idx].Name;

        if AXMLNode.ChildNodes[Idx].ChildNodes.Count = 0 then
        begin
          Fields.Add(FieldName);
        end;

        for ChildIdx := 0 to Pred(AXMLNode.ChildNodes[Idx].ChildNodes.Count) do
        begin
          FieldName := AXMLNode.ChildNodes[Idx].ChildNodes[ChildIdx].Name;
          if AXMLNode.ChildNodes[Idx].ChildNodes[ChildIdx].ChildNodes.Count = 0
          then
          begin
            Fields.Add(FieldName);
          end;
        end;

      end;
      AddDatasetFields(AClientDataset, Fields);
    end;
  finally
    FreeAndNil(Fields);
  end;
end;

procedure TXEROResponseDatasetBase.AddDatasetFields(AClientDataset
  : TClientDataSet; AFields: TStrings);

var
  FieldIdx: Integer;
  FieldType: TFieldType;
  FieldSize: Integer;

  function FieldExists(AFieldName: string): Boolean;
  begin
    try
      Result := AClientDataset.FieldDefList.Find(AFieldName) <> nil;
    except
      Result := true;
    end;
  end;

begin
  if Assigned(AFields) then
  begin
    for FieldIdx := 0 to Pred(AFields.Count) do
    begin
      if not FieldExists(AFields[FieldIdx]) then
      begin
        FieldType := ftString;
        FieldSize := 255;
        GetFieldInformation(AFields[FieldIdx], FieldType, FieldSize);
        AClientDataset.FieldDefs.Add(AFields[FieldIdx], FieldType,
          FieldSize, False);
      end;
    end;
  end;
end;

procedure TXEROResponseDatasetBase.AddMasterDatasetFields(AXMLNode: TXmlNode);
begin
  AddDatasetFields(FMasterDataSet, AXMLNode);
end;

procedure TXEROResponseDatasetBase.AddDetailDatasetFields(AXMLNode: TXmlNode);
begin
  AddDatasetFields(FDetailDataset, AXMLNode);
end;

procedure TXEROResponseDatasetBase.AddDetailMasterDatasetFields;
var
  CustomFields: TStringList;
  Fields: string;
begin
  CustomFields := TStringList.Create;
  try
    Fields := StringReplace(GetMasterFields, ';', #13#10,
      [rfReplaceAll, rfIgnoreCase]);
    CustomFields.Text := Fields;

    AddDatasetFields(FDetailDataset, CustomFields);

    FDetailDataset.IndexDefs.Clear;

    FDetailDataset.IndexDefs.Add('MasterDetail', GetMasterFields, [ixPrimary]);
    FDetailDataset.IndexName := 'MasterDetail';
  finally
    FreeAndNil(CustomFields);
  end;
end;

procedure TXEROResponseDatasetBase.GetFieldInformation(AFieldName: string;
  var AFieldType: TFieldType; var ASize: Integer);
begin
end;

function TXEROResponseDatasetBase.ImportXMLData(AXMLNodeList: TXmlNodeList;
  AErrorLog: TStrings): Boolean;
var
  MasterIdx, DetailIdx: Integer;
  DetailsNodes: TXmlNode;
  DetailsIdx: Integer;

  procedure AddDataFromChildNodes(AClientDataset: TClientDataSet;
    AChildNodes: TXmlNodeList);
  var
    Idx: Integer;
  begin
    for Idx := 0 to Pred(AChildNodes.Count) do
    begin
      if AClientDataset.FindField(AChildNodes[Idx].Name) <> nil then
      begin
        case AClientDataset.FieldByName(AChildNodes[Idx].Name).DataType of
          ftDate:
            begin
              AClientDataset.FieldByName(AChildNodes[Idx].Name).AsDateTime :=
                AChildNodes[Idx].AsDate;
            end;
          ftDateTime:
            begin
              AClientDataset.FieldByName(AChildNodes[Idx].Name).AsDateTime :=
                AChildNodes[Idx].AsDateTime;
            end;
          ftTime:
            begin
              AClientDataset.FieldByName(AChildNodes[Idx].Name).AsDateTime :=
                AChildNodes[Idx].AsTime;
            end;
          ftInteger, ftLargeint:
            begin
              AClientDataset.FieldByName(AChildNodes[Idx].Name).AsInteger :=
                StrToInt(AChildNodes[Idx].Text);
            end;
          ftFloat, ftCurrency:
            begin
              AClientDataset.FieldByName(AChildNodes[Idx].Name).AsFloat :=
                StrToFloat(AChildNodes[Idx].Text);
            end
        else
          begin
            AClientDataset.FieldByName(AChildNodes[Idx].Name).AsString :=
              AChildNodes[Idx].Text;
          end;
        end;

      end;
      AddDataFromChildNodes(AClientDataset, AChildNodes[Idx].ChildNodes);
    end;
  end;

  procedure UpdateMasterDetailsFields;
  var
    CustomFields: TStringList;
    Fields: string;
    Idx: Integer;
  begin
    CustomFields := TStringList.Create;
    try
      Fields := StringReplace(GetMasterFields, ';', #13#10,
        [rfReplaceAll, rfIgnoreCase]);
      CustomFields.Text := Fields;
      for Idx := 0 to Pred(CustomFields.Count) do
      begin
        if (FMasterDataSet.FindField(CustomFields[Idx]) <> nil) and
          (FDetailDataset.FindField(CustomFields[Idx]) <> nil) then
        begin
          FDetailDataset.FieldByName(CustomFields[Idx]).Value :=
            FMasterDataSet.FieldByName(CustomFields[Idx]).Value;
        end;
      end;
    finally
      FreeAndNil(CustomFields);
    end;
  end;

begin
  Result := False;
  if Assigned(AXMLNodeList) and Assigned(AErrorLog) then
  begin
    for MasterIdx := 0 to Pred(AXMLNodeList.Count) do
    begin
      if AXMLNodeList[MasterIdx].Name = FMasterNodeName then
      begin
        AddMasterDatasetFields(AXMLNodeList[MasterIdx]);
        AddDetailMasterDatasetFields;
        DetailsNodes := AXMLNodeList[MasterIdx].Find(FDetailListNodeName);
        if DetailsNodes <> nil then
        begin
          for DetailIdx := 0 to Pred(DetailsNodes.ChildNodes.Count) do
          begin
            if DetailsNodes.ChildNodes[DetailIdx].Name = FDetailNodeName then
            begin
              AddDetailDatasetFields(DetailsNodes.ChildNodes[DetailIdx]);
            end;
          end;
        end;
      end;

    end;

    AddCustomMasterDatasetFields;
    AddCustomDetailDatasetFields;

    CreateDataset(FMasterDataSet);
    CreateDataset(FDetailDataset);

    if (FMasterDataSet.Active) then
    begin
      for MasterIdx := 0 to Pred(AXMLNodeList.Count) do
      begin
        try
          FMasterDataSet.Insert;
          AddDataFromChildNodes(FMasterDataSet,
            AXMLNodeList[MasterIdx].ChildNodes);
          FMasterDataSet.Post;

          if FDetailDataset.Active then
          begin
            DetailsNodes := AXMLNodeList[MasterIdx].ChildNodes.Find
              (FDetailListNodeName);
            if DetailsNodes <> nil then
            begin
              for DetailsIdx := 0 to Pred(DetailsNodes.ChildNodes.Count) do
              begin
                try
                  FDetailDataset.Insert;
                  AddDataFromChildNodes(FDetailDataset,
                    DetailsNodes.ChildNodes[DetailsIdx].ChildNodes);
                  UpdateMasterDetailsFields;
                  FDetailDataset.Post;
                except
                  on E: Exception do
                  begin
                    AErrorLog.Add(E.Message);
                  end;
                end;
              end;
            end;
          end;

        except
          on E: Exception do
          begin
            AErrorLog.Add(E.Message);
          end;
        end;
      end;
      Result := true;
    end
    else
    begin
      AErrorLog.Add('Failed to create master dataset.');
    end;
  end
  else
  begin
    raise EXEROResponseDataset.Create
      ('AXMLNodeList or AErrorLog has not been assigned');
  end;
end;

function TXEROResponseDatasetBase.GetMasterFields: string;
begin
  Result := FDetailDataset.MasterFields;
end;

procedure TXEROResponseDatasetBase.SetMasterFields(AValue: string);
begin
  FDetailDataset.MasterFields := AValue;
end;

procedure TXEROResponseDatasetBase.XMLToDataset(AXML: string;
  var AResult: Boolean; var AErrorMessage: string);
var
  XMLParser: TXMLParser;
  ResponseNode, MasterNode: TXmlNode;
  ErrorLog: TStringList;
begin
  AResult := False;
  AErrorMessage := '';
  XMLParser := TXMLParser.Create;
  ErrorLog := TStringList.Create;
  try
    try
      XMLParser.Text := AXML;
      DestroyDataset(FMasterDataSet);
      DestroyDataset(FDetailDataset);
      ResponseNode := XMLParser.ChildNodes.Find('Response');
      if ResponseNode <> nil then
      begin
        if UpperCase(ResponseNode.GetChildNodeText('Status')) = 'OK' then
        begin
          MasterNode := ResponseNode.ChildNodes.Find(FMasterListNodeName);
          if MasterNode <> nil then
          begin
            if ImportXMLData(MasterNode.ChildNodes, ErrorLog) then
            begin
              if FMasterDataSet.Active then
                FMasterDataSet.First;
              if FDetailDataset.Active then
                FDetailDataset.First;
              AResult := true;
            end
            else
            begin
              raise EXEROResponseDataset.CreateFmt('XML Import failed: %s',
                [ErrorLog.Text]);
            end;
          end
          else
          begin
            raise EXEROResponseDataset.CreateFmt
              ('No data returned in XML node: "%s"', [FMasterListNodeName]);
          end;
        end
        else
        begin
          raise EXEROResponseDataset.CreateFmt('Invalid response status: "%s"',
            [ResponseNode.GetChildNodeText('Status')]);
        end;
      end
      else
      begin
        raise EXEROResponseDataset.Create('No valid response found in XML');
      end;
    except
      on E: Exception do
      begin
        AErrorMessage := E.Message;
        AResult := False;
      end;
    end;
  finally
    FreeAndNil(XMLParser);
    FreeAndNil(ErrorLog);
  end;
end;

procedure TXEROResponseDatasetBase.SetResponse( AResult :boolean; ACode : Integer; ADetail : String);
begin
  inherited SetResponse(AResult, ACode, ADetail);
  if AResult then
  begin
    XMLToDataset(ToString, AResult, ADetail);
  end;
end;

// TXEROResponseDataset

procedure TXEROResponseDataset.AddCustomMasterDatasetFields;
var
  Fields: TStringList;
begin
  Fields := TStringList.Create;
  try
    if Assigned(FOnCustomMasterDatasetFields) then
    begin
      FOnCustomMasterDatasetFields(Self, Fields);
      AddDatasetFields(MasterDataset, Fields);
    end;
  finally
    FreeAndNil(Fields);
  end;
end;

procedure TXEROResponseDataset.AddCustomDetailDatasetFields;
var
  Fields: TStringList;
begin
  Fields := TStringList.Create;
  try
    if Assigned(FOnCustomDetailDatasetFields) then
    begin
      FOnCustomDetailDatasetFields(Self, Fields);
      AddDatasetFields(DetailDataset, Fields);
    end;
  finally
    FreeAndNil(Fields);
  end;
end;

procedure TXEROResponseDataset.GetFieldInformation(AFieldName: string;
  var AFieldType: TFieldType; var ASize: Integer);
begin
  inherited GetFieldInformation(AFieldName, AFieldType, ASize);
  if Assigned(FOnGetFieldInformation) then
  begin
    FOnGetFieldInformation(Self, AFieldName, AFieldType, ASize);
  end;
end;

procedure TXEROResponseDataset.GetDefaultValues;
begin

end;

end.
