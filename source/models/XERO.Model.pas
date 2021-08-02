unit XERO.Model;

interface

{$WARN UNSUPPORTED_CONSTRUCT OFF}

uses
  XERO.Types,
  DBClient, DB, System.Generics.Collections,
  SysUtils, Classes,
  Rtti, TypInfo,
  REST.Json.Types, System.Json, REST.Json, System.Threading,
  System.SyncObjs;

type

  TXEROModel = class;
  TXEROModelClass = class of TXEROModel;

  TCustomAttributeClass = class of TCustomAttribute;

  XEROModelDatasetIgnoreAttribute = class(TCustomAttribute)
  private
    FLoad: Boolean;
    FSave: Boolean;
    procedure SetLoad(const Value: Boolean);
    procedure SetSave(const Value: Boolean);
  public
    constructor Create(ASave: Boolean = true; ALoad: Boolean = false);
    property Save: Boolean read FSave write SetSave;
    property Load: Boolean read FLoad write SetLoad;
  end;

  XEROModelDatasetFieldNameAttribute = class(TCustomAttribute)
  private
    FFieldName: string;
    procedure SetFieldName(const Value: string);
  public
    constructor Create(AFieldName: string);
    property FieldName: string read FFieldName write SetFieldName;
  end;

  XEROModelIgnoreAttribute = class(TCustomAttribute);
  XEROModelManagedAttribute = class(TCustomAttribute);

  XEROModelJSONPropertyNameAttribute = class(TCustomAttribute)
  private
    FPropertyName: string;
    procedure SetPropertyName(const Value: string);
  public
    constructor Create(APropertyName: string);
    property PropertyName: string read FPropertyName write SetPropertyName;
  end;

  TXEROModelRTTI = class(TXEROObject)
  private
  protected
    function DoFieldHasAttribute(ARTTIField: TRttiField;
      AAttribute: TCustomAttributeClass): Boolean; overload;
    function DoFieldGetAttribute(ARTTIField: TRttiField;
      AAttribute: TCustomAttributeClass): TCustomAttribute; overload;
    function DoPropertyGetAttribute(ARttiProperty: TRttiProperty;
      AAttribute: TCustomAttributeClass): TCustomAttribute; overload;
    function DoFieldGetAttribute(AXEROModel: TXEROModel; APropertyName: string;
      AAttribute: TCustomAttributeClass): TCustomAttribute; overload;
    class function DoGetProperty(AXEROModel: TXEROModel; APropertyName: string)
      : TRttiProperty;
    class function DoGetField(AXEROModel: TXEROModel; APropertyName: string)
      : TRttiField; static;
    procedure DoCreateModelField(AXEROModel: TXEROModel; AField: TRttiField);
    procedure DoDestroyModelField(AXEROModel: TXEROModel; AField: TRttiField);
    procedure DoCreateModelFields(AXEROModel: TXEROModel);
    procedure DoDestroyModelFields(AXEROModel: TXEROModel);
    function DoClone(AXEROModel: TXEROModel): TXEROModel; virtual;
    procedure DoAssign(ASourceModel: TXEROModel;
      ADestinationModel: TXEROModel); virtual;
  public
    class function HasAttribute(ARTTIField: TRttiField;
      AAttribute: TCustomAttributeClass): Boolean; overload;
    class function HasAttribute(ARttiProperty: TRttiProperty;
      AAttribute: TCustomAttributeClass): Boolean; overload;
    class function HasAttribute(AXEROModel: TXEROModel; APropertyName: string;
      AAttribute: TCustomAttributeClass): Boolean; overload;

    class function GetAttribute(ARTTIField: TRttiField;
      AAttribute: TCustomAttributeClass): TCustomAttribute; overload;
    class function GetAttribute(ARttiProperty: TRttiProperty;
      AAttribute: TCustomAttributeClass): TCustomAttribute; overload;
    class function GetAttribute(AXEROModel: TXEROModel; APropertyName: string;
      AAttribute: TCustomAttributeClass): TCustomAttribute; overload;

    class function GetProperty(AXEROModel: TXEROModel; APropertyName: string)
      : TRttiProperty;
    class function GetField(AXEROModel: TXEROModel; APropertyName: string)
      : TRttiField;
    class procedure CreateModelFields(AXEROModel: TXEROModel);
    class procedure DestroyModelFields(AXEROModel: TXEROModel);
    class function Clone(AXEROModel: TXEROModel): TXEROModel;
    class procedure Assign(ASourceModel: TXEROModel;
      ADestinationModel: TXEROModel);
  end;

  TXEROModelDateTimeHelper = class(TXEROObject)
  private
    FFormatSettings: TFormatSettings;
  protected
    function IsXERODate(AValue: string): Boolean;
    function DoXeroDateToDateTime(AValue: string): TDateTime; virtual;
    function DoDateTimeToXeroDateTime(AValue: TDateTime): string; virtual;
    function SetFormatSettings: TFormatSettings; virtual;
    function DoStringToDate(AValue: string): TDate; virtual;
    function DoStringToTime(AValue: string): TTime; virtual;
    function DoStringToDateTime(AValue: string): TDateTime; virtual;
    function DoDateToString(AValue: TDate): string; virtual;
    function DoTimeToString(AValue: TTime): string; virtual;
    function DoDateTimeToString(AValue: TDateTime): string; virtual;
    property FormatSettings: TFormatSettings read FFormatSettings;
  public
    constructor Create;
    class function StringToDate(AValue: string): TDate;
    class function StringToTime(AValue: string): TTime;
    class function StringToDateTime(AValue: string): TDateTime;
    class function DateToString(AValue: TDate): string;
    class function TimeToString(AValue: TTime): string;
    class function DateTimeToString(AValue: TDateTime): string;
    class function GetFormatSettings: TFormatSettings;
  end;

  TXEROModelDateTimeHelperClass = class of TXEROModelDateTimeHelper;

  TXEROModelDatasetMarshallerBase = class(TXEROObject)
  private
    FXEROModel: TXEROModel;
  protected
    property Model: TXEROModel read FXEROModel;
    function IsIgnoredProperty(AProp: TRttiProperty): Boolean; virtual;
  public
    constructor Create(AModel: TXEROModel);
    destructor Destroy; override;
  end;

  TXEROModelDatasetMarshaller = class(TXEROModelDatasetMarshallerBase)
  private
    procedure LoadFieldValueInternal(AField: TField; AProp: TRttiProperty;
      var AHandled: Boolean);
    procedure SaveFieldValueInternal(AField: TField; AProp: TRttiProperty;
      var AHandled: Boolean);
  protected
    procedure DoLoadFromDataset(ADataset: TDataSet); virtual;
    procedure DoSaveToDataset(ADataset: TDataSet;
      APost: Boolean = true); virtual;
    procedure DoAddFieldDefs(ADataset: TDataSet;
      AClear: Boolean = false); virtual;

    function IsIgnoredField(AField: TField; ASave: Boolean): Boolean; virtual;
    function GetPropertyFieldName(APropertyName: string): string; virtual;
    function IsDatasetIgnoredProperty(AProp: TRttiProperty; ASave: Boolean;
      ALoad: Boolean): Boolean; virtual;
    function MatchPropertyField(ADataset: TDataSet; APropertyName: string;
      var AFieldName: string): Boolean; virtual;

    procedure LoadFieldValue(AField: TField; AProp: TRttiProperty;
      var AHandled: Boolean); virtual;
    procedure SaveFieldValue(AField: TField; AProp: TRttiProperty;
      var AHandled: Boolean); virtual;
  public
    class procedure LoadFromDataset(AModel: TXEROModel;
      ADataset: TDataSet); overload;
    class procedure SaveToDataset(AModel: TXEROModel; ADataset: TDataSet;
      APost: Boolean = true); overload;
    class procedure AddFieldDefs(AModel: TXEROModel; ADataset: TDataSet;
      AClear: Boolean = false);
  end;

  TXEROModelDatasetMarshallerClass = class of TXEROModelDatasetMarshaller;

  TXEROModelJSONMarshaller = class(TXEROModelDatasetMarshallerBase)
  private
    procedure DoDeserializePropertyInternal(AProp: TRttiProperty;
      AJsonValue: TJSONPair; var AHandled: Boolean);
    procedure DoDeserializeClassInternal(AProp: TRttiProperty;
      AJsonValue: TJSONPair; var AHandled: Boolean);
    function DoSerializePropertyInternal(AProp: TRttiProperty): TJSONValue;
    function DoSerializeClassInternal(AProp: TRttiProperty): TJSONValue;
  protected
    procedure DoDeserializeProperty(AProp: TRttiProperty; AJsonValue: TJSONPair;
      var AHandled: Boolean); virtual;
    procedure DoDeserializeClass(AProp: TRttiProperty; AJsonValue: TJSONPair;
      var AHandled: Boolean);
    function DoSerializeProperty(AProp: TRttiProperty): TJSONValue; virtual;
    function DoSerializeClass(AProp: TRttiProperty): TJSONValue; virtual;
    function DefaultObjectSerialize(AObject: TObject): TJSONValue; virtual;
    procedure DefaultObjectDeserialize(AObject: TObject;
      AJsonValue: TJSONValue); virtual;
    function DefaultJSONParse(AJSON: string): TJSONValue;
    function DoFormat(AJSON: string): string; overload; virtual;
    function DoFormat(AJsonValue: TJSONValue): string; overload; virtual;
    function DoSerialize(AFormat: Boolean): string; virtual;
    procedure DoDeserialize(AJSON: string);
    function GetPropertyJSONName(AProp: TRttiProperty): string; virtual;
  public
    class function Serialize(AXEROModel: TXEROModel;
      AFormat: Boolean = false): string;
    class procedure Deserialize(AJSON: string; AXEROModel: TXEROModel);
    class function Format(AJSON: string): string;
  end;

  TXEROModelJSONMarshallerClass = class of TXEROModelJSONMarshaller;

  TXEROModel = class
  private
    [JSONMarshalledAttribute(false)]
    FDirty: Boolean;
    [JSONMarshalledAttribute(false)]
    FDirtyTimeStamp: TDateTime;
    [JSONMarshalledAttribute(false)]
    FLoaded: Boolean;
    [JSONMarshalledAttribute(false)]
    FXEROModelDateTimeHelperClass: TXEROModelDateTimeHelperClass;
    [JSONMarshalledAttribute(false)]
    FXEROModelJSONMarshallerClass: TXEROModelJSONMarshallerClass;
    [JSONMarshalledAttribute(false)]
    FXEROModelDatasetMarshallerClass: TXEROModelDatasetMarshallerClass;
    FInternalID: string;
    procedure SetDirty(const Value: Boolean);
    procedure SetDirtyTimeStamp(const Value: TDateTime);
    procedure SetLoaded(const Value: Boolean);
    function GetModified: Boolean;
    procedure SetModified(const Value: Boolean);
    procedure SetXEROModelDatasetMarshallerClass(const Value
      : TXEROModelDatasetMarshallerClass);
    procedure SetXEROModelDateTimeHelperClass(const Value
      : TXEROModelDateTimeHelperClass);
    procedure SetXEROModelJSONMarshallerClass(const Value
      : TXEROModelJSONMarshallerClass);
    procedure SetInternalID(const Value: string);
  protected
    [JSONMarshalledAttribute(false)]
    property _Dirty: Boolean read FDirty write SetDirty;
    [JSONMarshalledAttribute(false)]
    property _DirtyTimeStamp: TDateTime read FDirtyTimeStamp
      write SetDirtyTimeStamp;
    [JSONMarshalledAttribute(false)]
    property XEROModelDateTimeHelperClass: TXEROModelDateTimeHelperClass
      read FXEROModelDateTimeHelperClass write SetXEROModelDateTimeHelperClass;
    [JSONMarshalledAttribute(false)]
    property XEROModelDatasetMarshallerClass: TXEROModelDatasetMarshallerClass
      read FXEROModelDatasetMarshallerClass
      write SetXEROModelDatasetMarshallerClass;
    [JSONMarshalledAttribute(false)]
    property XEROModelJSONMarshallerClass: TXEROModelJSONMarshallerClass
      read FXEROModelJSONMarshallerClass write SetXEROModelJSONMarshallerClass;

    function GetAsJSON(AFormat: Boolean): string; virtual;
    procedure SetFromJSON(AJSON: string); virtual;
    procedure LoadDataset(ADataset: TDataSet); virtual;
    procedure SaveDataSet(ADataset: TDataSet; APost: Boolean); virtual;
    procedure CreateModelFields; virtual;
    procedure DestroyModelFields; virtual;
  public
    // JSON
    function AsJSON(AFormat: Boolean = false): string; overload;
    function AsJSONAsync(AProc: TProc<string>; AFormat: Boolean = false;
      ASync: Boolean = true; ARun: Boolean = true): ITask;
    procedure FromJSON(AJSON: string);
    function FromJSONAsync(AJSON: string; AProc: TProc<TXEROModel> = nil;
      ASync: Boolean = true; ARun: Boolean = true): ITask;
    function FormatJSON(AJSON: string): string;
    // Dataset
    procedure LoadFromDataset(ADataset: TDataSet); overload;
    procedure LoadFromDataset(ADatasource: TDataSource); overload;
    procedure SaveToDataset(ADataset: TDataSet; APost: Boolean = true);
      overload;
    procedure SaveToDataset(ADatasource: TDataSource;
      APost: Boolean = true); overload;
    procedure AddFieldDefs(ADataset: TDataSet; AClear: Boolean = false);

    class function CreateModel<T: TXEROModel>: T;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(AXEROModel: TXEROModel); virtual;
    function Clone: TXEROModel; overload; virtual;
    function Clone<T: TXEROModel>: T; overload;
    procedure Reset; virtual;
    [XEROModelIgnoreAttribute]
    property Loaded: Boolean read FLoaded write SetLoaded;
    [XEROModelIgnoreAttribute]
    property Modified: Boolean read GetModified write SetModified;
    [XEROModelIgnoreAttribute]
    property InternalID: string read FInternalID write SetInternalID;
  end;

  TXEROModelList<T: TXEROModel> = class(TXEROObject)
  private
    [JSONMarshalledAttribute(false)]
    FList: TObjectList<T>;
    function GetList: TObjectList<T>;
    function GetDirtyCount: integer;
  protected
    function GetItem(AIndex: integer): T;
    function GetCount: integer;
    procedure DatasetRowToModel(ADataset: TDataSet); virtual;
    property List: TObjectList<T> read GetList;
    function CreateModel: T; virtual;
    function Find: T; virtual;
    property DirtyCount: integer read GetDirtyCount;
    function GetJSONArray(AJSON: string): TJSONArray; virtual;
    function GetAsJSON(AName: string = ''): string; virtual;
    procedure GetAsJSONAsync(AProc: TProc<string>; AFormat: Boolean = false;
      AName: string = ''); virtual;
    procedure SetFromJSON(AJSON: string); virtual;
    procedure SetFromJSONAsync(AJSON: string;
      AProc: TProc < TXEROModelList < T >> ); virtual;
  public
    function New: T;
    constructor Create; reintroduce;
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<T>;
    procedure LoadFromDataset(ADataset: TDataSet); overload; virtual;
    procedure LoadFromDataset(ADatasource: TDataSource); overload; virtual;
    procedure Add(AXEROModel: T); virtual;
    procedure Delete(AXEROModel: T); virtual;
    procedure Clone(AXEROModel: T); virtual;
    procedure CloneList(AList: TXEROModelList<T>; AClear: Boolean = true);
    procedure Assign(AList: TXEROModelList<T>); reintroduce;
    procedure Clear;
    function AsJSONArray(AName: string; AFormat: Boolean = false): string;
    function AsJSON(AFormat: Boolean = false): string;
    procedure AsJSONAsync(AProc: TProc<string>; AFormat: Boolean = false;
      AName: string = '');
    procedure FromJSON(AJSON: string);
    procedure FromJSONAsync(AJSON: string;
      AProc: TProc < TXEROModelList < T >> = nil);
    function FormatJSON(AJSON: string): string;
    property Item[AIndex: integer]: T read GetItem;
    property Count: integer read GetCount;
  end;

  TOnXEROModelCallback<TXEROModel> = reference to procedure(AModel: TXEROModel;
    ASuccess: Boolean; AMessage: string);

var
  RttiContext: TRTTIContext;
  DefaultXEROModelDateTimeHelperClass: TXEROModelDateTimeHelperClass;
  DefaultXEROModelDatasetMarshallerClass: TXEROModelDatasetMarshallerClass;
  DefaultXEROModelJSONMarshallerClass: TXEROModelJSONMarshallerClass;

implementation

uses
  System.StrUtils, XERO.Utils, XERO.MiscUtil, System.IOUtils, System.DateUtils,
  REST.JsonReflect, System.Math;

function TXEROModelList<T>.GetList: TObjectList<T>;
begin
  Result := FList;
end;

function TXEROModelList<T>.GetDirtyCount: integer;
var
  LXEROModel: TXEROModel;
begin
  Result := 0;
  for LXEROModel in FList do
  begin
    if LXEROModel._Dirty then
      Inc(Result);
  end;
end;

function TXEROModelList<T>.GetItem(AIndex: integer): T;
begin
  Result := FList.Items[AIndex];
end;

function TXEROModelList<T>.GetCount: integer;
begin
  Result := FList.Count;
end;

procedure TXEROModelList<T>.DatasetRowToModel(ADataset: TDataSet);
var
  LXEROModel: T;
  LAdd: Boolean;
begin
  LAdd := false;
  LXEROModel := CreateModel;
  try
    LXEROModel.LoadFromDataset(ADataset);
    LAdd := true;
  finally
    if LAdd then
    begin
      Add(LXEROModel);
    end
    else
    begin
      LXEROModel.Free;
    end;
  end;
end;

function TXEROModelList<T>.CreateModel: T;
begin
  Result := TXEROModel.CreateModel<T>;
end;

function TXEROModelList<T>.Find: T;
begin
  Result := nil;
end;

function TXEROModelList<T>.GetJSONArray(AJSON: string): TJSONArray;
begin
  Result := nil;
  try
    Result := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(AJSON), 0)
      as TJSONArray;
  except
    on E: Exception do
    begin
      Error(E, AJSON);
    end;
  end;
end;

function TXEROModelList<T>.GetAsJSON(AName: string): string;
var
  LModel: T;
  LModelJSON: string;
begin
  Result := '';
  try
    for LModel in Self do
    begin
      try
        LModelJSON := LModel.AsJSON;
        if not IsEmptyString(Result) then
          Result := Result + ',';
        Result := Result + LModelJSON;
      except
        on E: Exception do
        begin
          Error(E);
        end;
      end;
    end;
  finally
    if not IsEmptyString(AName) then
    begin
      Result := Format('{ "%s":[ %s ] }', [AName, Result]);
    end
    else
    begin
      Result := Format('[ %s ]', [Result]);
    end;
  end;
end;

procedure TXEROModelList<T>.GetAsJSONAsync(AProc: TProc<string>;
  AFormat: Boolean; AName: string);
var
  LXEROModel: T;
  LJSON: string;
begin
  TTask.Create(
    procedure
    begin
      LJSON := GetAsJSON(AName);
      if AFormat then
        LJSON := FormatJSON(LJSON);
      if Assigned(AProc) then
      begin
        TThread.Queue(nil,
          procedure
          begin
            AProc(LJSON);
          end);
      end;
    end).Start;
end;

procedure TXEROModelList<T>.SetFromJSON(AJSON: string);
var
  LJsonArr: TJSONArray;
  LJsonValue: TJSONValue;
  LModel: T;
  LJSON: string;
begin
  Self.Clear;
  if not IsEmptyString(AJSON) and (not SameText('null', AJSON)) then
  begin
    LJsonArr := GetJSONArray(AJSON);
    if Assigned(LJsonArr) then
    begin
      try
        for LJsonValue in LJsonArr do
        begin
          try
            LJSON := LJsonValue.ToJSON;
            LModel := New;
            LModel.FromJSON(LJSON);
          except
            on E: Exception do
            begin
              Error(E);
            end;
          end;
        end;
      finally
        FreeANdNil(LJsonArr);
      end;
    end;
  end;
end;

procedure TXEROModelList<T>.SetFromJSONAsync(AJSON: string;
AProc: TProc < TXEROModelList < T >> );
var
  LJsonArr: TJSONArray;
  LJsonValue: TJSONValue;
  LModel: T;
  LJSON: string;
begin
  TTask.Create(
    procedure
    begin
      SetFromJSON(AJSON);
      if Assigned(AProc) then
      begin
        AProc(Self);
      end;
    end).Start;
end;

function TXEROModelList<T>.New: T;
begin
  Result := CreateModel;
  FList.Add(Result);
end;

constructor TXEROModelList<T>.Create;
begin
  inherited;
  FList := TObjectList<T>.Create(true);
end;

destructor TXEROModelList<T>.Destroy;
begin
  try
    Clear;
    FreeANdNil(FList);
  finally
    inherited;
  end;
end;

function TXEROModelList<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := FList.GetEnumerator;
end;

procedure TXEROModelList<T>.LoadFromDataset(ADataset: TDataSet);
begin
  Debug('LoadFromDataset', Format('Loading %d Model(s) from dataset',
    [ADataset.RecordCount]));
  ADataset.First;
  while not ADataset.Eof do
  begin
    DatasetRowToModel(ADataset);
    ADataset.Next;
  end;
  Debug('LoadFromDataset', Format('Loaded %d Model(s)', [Self.Count]));
end;

procedure TXEROModelList<T>.LoadFromDataset(ADatasource: TDataSource);
begin
  LoadFromDataset(ADatasource.DataSet);
end;

{ TXEROModelList<T> }

procedure TXEROModelList<T>.Add(AXEROModel: T);
begin
  FList.Add(AXEROModel);
end;

procedure TXEROModelList<T>.Delete(AXEROModel: T);
begin
  if FList.IndexOf(AXEROModel) <> -1 then
  begin
    FList.Remove(AXEROModel);
  end;
end;

procedure TXEROModelList<T>.Clone(AXEROModel: T);
begin
  FList.Add(AXEROModel.Clone);
end;

procedure TXEROModelList<T>.CloneList(AList: TXEROModelList<T>;
AClear: Boolean);
var
  LXEROModel: T;
begin
  if AClear then
    Clear;
  For LXEROModel in AList do
  begin
    Clone(LXEROModel);
  end;
end;

procedure TXEROModelList<T>.Assign(AList: TXEROModelList<T>);
begin
  CloneList(AList, true);
end;

procedure TXEROModelList<T>.Clear;
begin
  FList.Clear;
end;

function TXEROModelList<T>.AsJSON(AFormat: Boolean): string;
begin
  try
    Result := GetAsJSON;
  finally
    if AFormat then
      Result := FormatJSON(Result);
  end;
end;

function TXEROModelList<T>.AsJSONArray(AName: string; AFormat: Boolean): string;
begin
  try
    Result := GetAsJSON(AName);
  finally
    if AFormat then
      Result := FormatJSON(Result);
  end;
end;

procedure TXEROModelList<T>.AsJSONAsync(AProc: TProc<string>; AFormat: Boolean;
AName: string);
begin
  GetAsJSONAsync(AProc, AFormat);
end;

procedure TXEROModelList<T>.FromJSON(AJSON: string);
begin
  SetFromJSON(AJSON);
end;

procedure TXEROModelList<T>.FromJSONAsync(AJSON: string;
AProc: TProc < TXEROModelList < T >> );
begin
  SetFromJSONAsync(AJSON, AProc);
end;

function TXEROModelList<T>.FormatJSON(AJSON: string): string;
var
  LModel: T;
begin
  Result := '[]';
  if Count > 0 then
  begin
    LModel := FList.Items[0];
    // Use a model to generate JSON to use correct XEROModelJSONMarshallerClass
    Result := LModel.FormatJSON(AJSON);
  end;
end;

procedure TXEROModel.SetDirty(const Value: Boolean);
begin
  FDirty := Value;
end;

procedure TXEROModel.SetDirtyTimeStamp(const Value: TDateTime);
begin
  FDirtyTimeStamp := Value;
end;

procedure TXEROModel.SetLoaded(const Value: Boolean);
begin
  FLoaded := Value;
end;

function TXEROModel.GetModified: Boolean;
begin
  Result := _Dirty;
end;

procedure TXEROModel.SetModified(const Value: Boolean);
begin
  _Dirty := true;
  _DirtyTimeStamp := Now;
end;

procedure TXEROModel.SetXEROModelDatasetMarshallerClass
  (const Value: TXEROModelDatasetMarshallerClass);
begin
  FXEROModelDatasetMarshallerClass := Value;
end;

procedure TXEROModel.SetXEROModelDateTimeHelperClass
  (const Value: TXEROModelDateTimeHelperClass);
begin
  FXEROModelDateTimeHelperClass := Value;
end;

procedure TXEROModel.SetXEROModelJSONMarshallerClass
  (const Value: TXEROModelJSONMarshallerClass);
begin
  FXEROModelJSONMarshallerClass := Value;
end;

function TXEROModel.GetAsJSON(AFormat: Boolean): string;
begin
  Result := XEROModelJSONMarshallerClass.Serialize(Self, AFormat);
end;

procedure TXEROModel.SetFromJSON(AJSON: string);
begin
  XEROModelJSONMarshallerClass.Deserialize(AJSON, Self);
end;

procedure TXEROModel.SetInternalID(const Value: string);
begin
  FInternalID := Value;
end;

procedure TXEROModel.LoadDataset(ADataset: TDataSet);
begin
  XEROModelDatasetMarshallerClass.LoadFromDataset(Self, ADataset);
end;

procedure TXEROModel.SaveDataSet(ADataset: TDataSet; APost: Boolean);
begin
  XEROModelDatasetMarshallerClass.SaveToDataset(Self, ADataset, APost);
end;

procedure TXEROModel.CreateModelFields;
begin
  TXEROModelRTTI.CreateModelFields(Self);
end;

procedure TXEROModel.DestroyModelFields;
begin
  TXEROModelRTTI.DestroyModelFields(Self);
end;

{ TXEROModel }

procedure TXEROModel.AddFieldDefs(ADataset: TDataSet; AClear: Boolean);
begin
  XEROModelDatasetMarshallerClass.AddFieldDefs(Self, ADataset, AClear);
end;

function TXEROModel.AsJSON(AFormat: Boolean = false): string;
begin
  Result := GetAsJSON(AFormat);
end;

function TXEROModel.FormatJSON(AJSON: string): string;
begin
  Result := XEROModelJSONMarshallerClass.Format(AJSON);
end;

procedure TXEROModel.FromJSON(AJSON: string);
begin
  SetFromJSON(AJSON);
end;

function TXEROModel.FromJSONAsync(AJSON: string; AProc: TProc<TXEROModel>;
ASync, ARun: Boolean): ITask;
begin
  Result := TTask.Create(
    procedure
    begin
      try
        FromJSON(AJSON);
      finally
        if Assigned(AProc) then
        begin

          if ASync then
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                AProc(Self)
              end);
          end
          else
          begin
            AProc(Self);
          end;
        end;
      end;
    end);
  if ARun then
    Result.Start;
end;

procedure TXEROModel.LoadFromDataset(ADataset: TDataSet);
begin
  LoadDataset(ADataset);
end;

procedure TXEROModel.LoadFromDataset(ADatasource: TDataSource);
begin
  LoadFromDataset(ADatasource.DataSet);
end;

procedure TXEROModel.SaveToDataset(ADataset: TDataSet; APost: Boolean);
begin
  SaveDataSet(ADataset, APost);
end;

procedure TXEROModel.SaveToDataset(ADatasource: TDataSource; APost: Boolean);
begin
  SaveToDataset(ADatasource.DataSet, APost);
end;

class function TXEROModel.CreateModel<T>: T;
var
  AValue: TValue;
  rType: TRttiType;
  AMethCreate: TRttiMethod;
  instanceType: TRttiInstanceType;
begin
  Result := nil;
  rType := RttiContext.GetType(TypeInfo(T));
  AMethCreate := rType.GetMethod('Create');

  if Assigned(AMethCreate) and rType.IsInstance then
  begin
    instanceType := rType.AsInstance;
    AValue := AMethCreate.Invoke(instanceType.MetaclassType, []);
    Result := AValue.AsType<T>;
  end;
end;

constructor TXEROModel.Create;
begin
  try
    FXEROModelDateTimeHelperClass := DefaultXEROModelDateTimeHelperClass;
    FXEROModelJSONMarshallerClass := DefaultXEROModelJSONMarshallerClass;
    FXEROModelDatasetMarshallerClass := DefaultXEROModelDatasetMarshallerClass;
    CreateModelFields;
  finally
    Reset;
  end;
end;

destructor TXEROModel.Destroy;
begin
  try
    DestroyModelFields;
  finally
    inherited;
  end;
end;

function TXEROModel.AsJSONAsync(AProc: TProc<string>;
AFormat, ASync, ARun: Boolean): ITask;

begin

  Result := TTask.Create(
    procedure
    var
      LJSON: string;
    begin

      try

        LJSON := AsJSON(AFormat);

      finally
        if Assigned(AProc) then
        begin
          if ASync then
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                AProc(LJSON)
              end);
          end
          else
          begin
            AProc(LJSON);
          end;
        end;

      end;
    end);
  if ARun then
    Result.Start;

end;

procedure TXEROModel.Assign(AXEROModel: TXEROModel);
begin
  Self.FromJSON(AXEROModel.AsJSON);
end;

function TXEROModel.Clone: TXEROModel;
var
  AValue: TValue;
  rType: TRttiType;
  AMethCreate: TRttiMethod;
  instanceType: TRttiInstanceType;
begin
  Result := nil;
  rType := RttiContext.GetType(Self.ClassInfo);
  AMethCreate := rType.GetMethod('Create');

  if Assigned(AMethCreate) and rType.IsInstance then
  begin
    instanceType := rType.AsInstance;
    AValue := AMethCreate.Invoke(instanceType.MetaclassType, []);
    Result := AValue.AsType<TXEROModel>;
    Result.FromJSON(Self.AsJSON);
  end;
end;

function TXEROModel.Clone<T>: T;
begin
  Result := Clone as T;
end;

procedure TXEROModel.Reset;
begin
  FDirty := false;
  FDirtyTimeStamp := 0;
  FLoaded := false;
end;

{ TXEROModelIgnoreField }

constructor XEROModelDatasetIgnoreAttribute.Create(ASave, ALoad: Boolean);
begin
  FSave := ASave;
  FLoad := ALoad;
end;

procedure XEROModelDatasetIgnoreAttribute.SetLoad(const Value: Boolean);
begin
  FLoad := Value;
end;

procedure XEROModelDatasetIgnoreAttribute.SetSave(const Value: Boolean);
begin
  FSave := Value;
end;

{ XEROModelDatasetFieldNameAttribute }

constructor XEROModelDatasetFieldNameAttribute.Create(AFieldName: string);
begin
  FFieldName := AFieldName;
end;

procedure XEROModelDatasetFieldNameAttribute.SetFieldName(const Value: string);
begin
  FFieldName := Value;
end;

{ XEROModelPropertyNameAttribute }

constructor XEROModelJSONPropertyNameAttribute.Create(APropertyName: string);
begin
  FPropertyName := APropertyName;
end;

procedure XEROModelJSONPropertyNameAttribute.SetPropertyName
  (const Value: string);
begin
  FPropertyName := Value;
end;

function TXEROModelRTTI.DoFieldHasAttribute(ARTTIField: TRttiField;
AAttribute: TCustomAttributeClass): Boolean;
begin
  Result := DoFieldGetAttribute(ARTTIField, AAttribute) <> nil;
end;

function TXEROModelRTTI.DoFieldGetAttribute(ARTTIField: TRttiField;
AAttribute: TCustomAttributeClass): TCustomAttribute;
var
  LAttribute: TCustomAttribute;
begin
  Result := nil;
  if ARTTIField <> nil then
  begin
    for LAttribute in ARTTIField.GetAttributes do
    begin
      if (LAttribute is AAttribute) then
      begin
        Result := LAttribute;
      end;
    end;
  end;
end;

function TXEROModelRTTI.DoFieldGetAttribute(AXEROModel: TXEROModel;
APropertyName: string; AAttribute: TCustomAttributeClass): TCustomAttribute;
begin
  Result := DoPropertyGetAttribute(DoGetProperty(AXEROModel, APropertyName),
    AAttribute);
end;

class function TXEROModelRTTI.DoGetProperty(AXEROModel: TXEROModel;
APropertyName: string): TRttiProperty;
var
  // LRTTICtx: TRTTIContext;
  LRTTIType: TRttiType;
begin
  // LRTTICtx := TRTTIContext.Create;
  LRTTIType := RttiContext.GetType(AXEROModel.ClassType);
  Result := LRTTIType.GetProperty(APropertyName);
end;

function TXEROModelRTTI.DoPropertyGetAttribute(ARttiProperty: TRttiProperty;
AAttribute: TCustomAttributeClass): TCustomAttribute;
var
  LAttribute: TCustomAttribute;
begin
  Result := nil;
  if ARttiProperty <> nil then
  begin
    for LAttribute in ARttiProperty.GetAttributes do
    begin
      if (LAttribute is AAttribute) then
      begin
        Result := LAttribute;
      end;
    end;
  end;
end;

class function TXEROModelRTTI.DoGetField(AXEROModel: TXEROModel;
APropertyName: string): TRttiField;
var
  // LRTTICtx: TRTTIContext;
  LRTTIType: TRttiType;
begin
  // LRTTICtx := TRTTIContext.Create;
  LRTTIType := RttiContext.GetType(AXEROModel.ClassType);
  Result := LRTTIType.GetField(APropertyName);
end;

procedure TXEROModelRTTI.DoCreateModelField(AXEROModel: TXEROModel;
AField: TRttiField);
var
  LMethod: TRttiMethod;
  LInstanceType: TRttiInstanceType;
  LValue: TValue;
begin
  if (AField.FieldType.TypeKind = tkClass) then
  begin
    if HasAttribute(AField, XEROModelManagedAttribute) then
    begin
      LValue := AField.GetValue(AXEROModel);
      if LValue.IsEmpty then
      begin
        LMethod := AField.FieldType.GetMethod('Create');
        if Assigned(LMethod) and (LMethod.IsConstructor) and
          (AField.FieldType.IsInstance) then
        begin
          LInstanceType := AField.FieldType.AsInstance;
          LValue := LMethod.Invoke(LInstanceType.MetaclassType, []);
          AField.SetValue(AXEROModel, LValue);
        end;
      end;
    end;
  end;
end;

procedure TXEROModelRTTI.DoDestroyModelField(AXEROModel: TXEROModel;
AField: TRttiField);
var
  LMethod: TRttiMethod;
begin
  if (AField.FieldType.TypeKind = tkClass) then
  begin
    if HasAttribute(AField, XEROModelManagedAttribute) then
    begin
      for LMethod in AField.FieldType.GetMethods('Destroy') do
      begin
        if LMethod.IsDestructor then
        begin
          LMethod.Invoke(AField.GetValue(AXEROModel), []);
          AField.SetValue(AXEROModel, nil);
          break;
        end;
      end;
    end;
  end;
end;

procedure TXEROModelRTTI.DoCreateModelFields(AXEROModel: TXEROModel);
var
  LRecordType: TRttiType;
  LField: TRttiField;
begin
  LRecordType := RttiContext.GetType(AXEROModel.ClassType);
  for LField in LRecordType.GetFields do
  begin
    try
      if (LField.FieldType.TypeKind = tkClass) then
      begin
        DoCreateModelField(AXEROModel, LField);
      end;
    except
      on E: Exception do
      begin
        Error(E);
      end;
    end;
  end;
end;

procedure TXEROModelRTTI.DoDestroyModelFields(AXEROModel: TXEROModel);
var
  LRecordType: TRttiType;
  LField: TRttiField;
begin
  LRecordType := RttiContext.GetType(AXEROModel.ClassType);
  for LField in LRecordType.GetFields do
  begin
    try
      if (LField.FieldType.TypeKind = tkClass) then
      begin
        DoDestroyModelField(AXEROModel, LField);
      end;
    except
      on E: Exception do
      begin
        Error(E);
      end;
    end;
  end;
end;

function TXEROModelRTTI.DoClone(AXEROModel: TXEROModel): TXEROModel;
begin
  Result := TXEROModelClass(AXEROModel.ClassType).Create;
  try
    DoAssign(AXEROModel, Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TXEROModelRTTI.DoAssign(ASourceModel, ADestinationModel: TXEROModel);
var
  // Context: TRTTIContext;
  RttiType: TRttiType;
  LProp: TRttiProperty;
  SourceAsPointer, ResultAsPointer: Pointer;
  LAllowProperty: Boolean;
begin
  RttiType := RttiContext.GetType(ASourceModel.ClassType);
  try
    Move(ASourceModel, SourceAsPointer, SizeOf(Pointer));
    Move(ADestinationModel, ResultAsPointer, SizeOf(Pointer));
    for LProp in RttiType.GetProperties do
    begin
      LAllowProperty := true;

      if LAllowProperty then
      begin
        LAllowProperty := LProp.IsReadable;
      end;
      if LAllowProperty then
      begin
        LAllowProperty := LProp.Visibility >= mvPublic;
      end;
      if LAllowProperty then
      begin
        if (LProp.PropertyType.TypeKind = tkClass) then
        begin

        end
        else
        begin
          if LProp.IsWritable then
          begin
            LProp.SetValue(ResultAsPointer, LProp.GetValue(SourceAsPointer));
          end;
        end;

      end;
    end;
  except
    raise;
  end;
end;

class function TXEROModelRTTI.HasAttribute(ARTTIField: TRttiField;
AAttribute: TCustomAttributeClass): Boolean;
var
  LXEROModelRTTI: TXEROModelRTTI;
begin
  LXEROModelRTTI := TXEROModelRTTI.Create;
  try
    Result := LXEROModelRTTI.DoFieldHasAttribute(ARTTIField, AAttribute);
  finally
    FreeANdNil(LXEROModelRTTI);
  end;
end;

class function TXEROModelRTTI.HasAttribute(AXEROModel: TXEROModel;
APropertyName: string; AAttribute: TCustomAttributeClass): Boolean;
var
  LXEROModelRTTI: TXEROModelRTTI;
begin
  LXEROModelRTTI := TXEROModelRTTI.Create;
  try
    Result := LXEROModelRTTI.DoFieldGetAttribute(AXEROModel, APropertyName,
      AAttribute) <> nil;
  finally
    FreeANdNil(LXEROModelRTTI);
  end;
end;

class function TXEROModelRTTI.GetAttribute(ARTTIField: TRttiField;
AAttribute: TCustomAttributeClass): TCustomAttribute;
var
  LXEROModelRTTI: TXEROModelRTTI;
begin
  LXEROModelRTTI := TXEROModelRTTI.Create;
  try
    Result := LXEROModelRTTI.DoFieldGetAttribute(ARTTIField, AAttribute);
  finally
    FreeANdNil(LXEROModelRTTI);
  end;
end;

class function TXEROModelRTTI.GetAttribute(AXEROModel: TXEROModel;
APropertyName: string; AAttribute: TCustomAttributeClass): TCustomAttribute;
var
  LXEROModelRTTI: TXEROModelRTTI;
begin
  LXEROModelRTTI := TXEROModelRTTI.Create;
  try
    Result := LXEROModelRTTI.DoFieldGetAttribute(AXEROModel, APropertyName,
      AAttribute);
  finally
    FreeANdNil(LXEROModelRTTI);
  end;
end;

class function TXEROModelRTTI.GetAttribute(ARttiProperty: TRttiProperty;
AAttribute: TCustomAttributeClass): TCustomAttribute;
var
  LXEROModelRTTI: TXEROModelRTTI;
begin
  LXEROModelRTTI := TXEROModelRTTI.Create;
  try
    Result := LXEROModelRTTI.DoPropertyGetAttribute(ARttiProperty, AAttribute);
  finally
    FreeANdNil(LXEROModelRTTI);
  end;
end;

class function TXEROModelRTTI.GetField(AXEROModel: TXEROModel;
APropertyName: string): TRttiField;
var
  LXEROModelRTTI: TXEROModelRTTI;
begin
  LXEROModelRTTI := TXEROModelRTTI.Create;
  try
    Result := LXEROModelRTTI.DoGetField(AXEROModel, APropertyName);
  finally
    FreeANdNil(LXEROModelRTTI);
  end;
end;

class function TXEROModelRTTI.GetProperty(AXEROModel: TXEROModel;
APropertyName: string): TRttiProperty;
var
  LXEROModelRTTI: TXEROModelRTTI;
begin
  LXEROModelRTTI := TXEROModelRTTI.Create;
  try
    Result := LXEROModelRTTI.DoGetProperty(AXEROModel, APropertyName);
  finally
    FreeANdNil(LXEROModelRTTI);
  end;
end;

class function TXEROModelRTTI.HasAttribute(ARttiProperty: TRttiProperty;
AAttribute: TCustomAttributeClass): Boolean;
var
  LXEROModelRTTI: TXEROModelRTTI;
begin
  LXEROModelRTTI := TXEROModelRTTI.Create;
  try
    Result := LXEROModelRTTI.DoPropertyGetAttribute(ARttiProperty,
      AAttribute) <> nil;
  finally
    FreeANdNil(LXEROModelRTTI);
  end;
end;

class procedure TXEROModelRTTI.CreateModelFields(AXEROModel: TXEROModel);
var
  LXEROModelRTTI: TXEROModelRTTI;
begin
  LXEROModelRTTI := TXEROModelRTTI.Create;
  try
    LXEROModelRTTI.DoCreateModelFields(AXEROModel);
  finally
    FreeANdNil(LXEROModelRTTI);
  end;
end;

class procedure TXEROModelRTTI.DestroyModelFields(AXEROModel: TXEROModel);
var
  LXEROModelRTTI: TXEROModelRTTI;
begin
  LXEROModelRTTI := TXEROModelRTTI.Create;
  try
    LXEROModelRTTI.DoDestroyModelFields(AXEROModel);
  finally
    FreeANdNil(LXEROModelRTTI);
  end;
end;

class function TXEROModelRTTI.Clone(AXEROModel: TXEROModel): TXEROModel;
var
  LXEROModelRTTI: TXEROModelRTTI;
begin
  LXEROModelRTTI := TXEROModelRTTI.Create;
  try
    Result := LXEROModelRTTI.DoClone(AXEROModel);
  finally
    FreeANdNil(LXEROModelRTTI);
  end;
end;

class procedure TXEROModelRTTI.Assign(ASourceModel, ADestinationModel
  : TXEROModel);
var
  LXEROModelRTTI: TXEROModelRTTI;
begin
  LXEROModelRTTI := TXEROModelRTTI.Create;
  try
    LXEROModelRTTI.DoAssign(ASourceModel, ADestinationModel);
  finally
    FreeANdNil(LXEROModelRTTI);
  end;
end;

function TXEROModelDateTimeHelper.SetFormatSettings: TFormatSettings;
begin
  Result := GetJSONFormat;
  Result.DateSeparator := '-';
  Result.TimeSeparator := ':';
  Result.ShortDateFormat := 'yyyy-mm-dd';
  Result.ShortTimeFormat := 'hh:nn:ss';
  Result.LongDateFormat := 'yyyy-mm-dd';
  Result.LongTimeFormat := 'hh:nn:ss';
end;

function TXEROModelDateTimeHelper.DoStringToDate(AValue: string): TDate;
begin
  if IsXERODate(AValue) then
  begin
    Result := DateOf(DoXeroDateToDateTime(AValue));
  end
  else
  begin
    Result := StrToDateDef(AValue, 0, FFormatSettings);
  end;
end;

function TXEROModelDateTimeHelper.DoStringToTime(AValue: string): TTime;
begin
  if IsXERODate(AValue) then
  begin
    Result := TimeOf(DoXeroDateToDateTime(AValue));
  end
  else
  begin
    Result := StrToTimeDef(AValue, 0, FFormatSettings);
  end;
end;

function TXEROModelDateTimeHelper.DoStringToDateTime(AValue: string): TDateTime;
begin
  if IsXERODate(AValue) then
  begin
    Result := DoXeroDateToDateTime(AValue);
  end
  else
  begin
    Result := ISO8601ToDate(AValue, true);
  end;
end;

function TXEROModelDateTimeHelper.DoDateToString(AValue: TDate): string;
begin
  // Result := DateToStr(AValue, FFormatSettings);
  Result := DoDateTimeToXeroDateTime(AValue);
end;

function TXEROModelDateTimeHelper.DoTimeToString(AValue: TTime): string;
begin
  // Result := TimeToStr(AValue, FFormatSettings);
  Result := DoDateTimeToXeroDateTime(AValue);
end;

function TXEROModelDateTimeHelper.DoXeroDateToDateTime(AValue: string)
  : TDateTime;
begin
  Result := ConvertJSONDate(AValue);
end;

function TXEROModelDateTimeHelper.DoDateTimeToString(AValue: TDateTime): string;
begin
  Result := DoDateTimeToXeroDateTime(AValue);
  // Result := DateToISO8601(AValue, true);
end;

function TXEROModelDateTimeHelper.DoDateTimeToXeroDateTime
  (AValue: TDateTime): string;
begin
  Result := DateTimeAsMicrosoftJSONDate(AValue);
end;

constructor TXEROModelDateTimeHelper.Create;
begin
  FFormatSettings := SetFormatSettings;
end;

class function TXEROModelDateTimeHelper.StringToDate(AValue: string): TDate;
var
  LXEROModelDateTimeHelper: TXEROModelDateTimeHelper;
begin
  LXEROModelDateTimeHelper := TXEROModelDateTimeHelper.Create;
  try
    Result := LXEROModelDateTimeHelper.DoStringToDate(AValue);
  finally
    FreeANdNil(LXEROModelDateTimeHelper);
  end;
end;

class function TXEROModelDateTimeHelper.StringToTime(AValue: string): TTime;
var
  LXEROModelDateTimeHelper: TXEROModelDateTimeHelper;
begin
  LXEROModelDateTimeHelper := TXEROModelDateTimeHelper.Create;
  try
    Result := LXEROModelDateTimeHelper.DoStringToTime(AValue);
  finally
    FreeANdNil(LXEROModelDateTimeHelper);
  end;
end;

class function TXEROModelDateTimeHelper.StringToDateTime(AValue: string)
  : TDateTime;
var
  LXEROModelDateTimeHelper: TXEROModelDateTimeHelper;
begin
  LXEROModelDateTimeHelper := TXEROModelDateTimeHelper.Create;
  try
    Result := LXEROModelDateTimeHelper.DoStringToDateTime(AValue);
  finally
    FreeANdNil(LXEROModelDateTimeHelper);
  end;
end;

class function TXEROModelDateTimeHelper.DateToString(AValue: TDate): string;
var
  LXEROModelDateTimeHelper: TXEROModelDateTimeHelper;
begin
  LXEROModelDateTimeHelper := TXEROModelDateTimeHelper.Create;
  try
    Result := LXEROModelDateTimeHelper.DoDateToString(AValue);
  finally
    FreeANdNil(LXEROModelDateTimeHelper);
  end;
end;

class function TXEROModelDateTimeHelper.TimeToString(AValue: TTime): string;
var
  LXEROModelDateTimeHelper: TXEROModelDateTimeHelper;
begin
  LXEROModelDateTimeHelper := TXEROModelDateTimeHelper.Create;
  try
    Result := LXEROModelDateTimeHelper.DoTimeToString(AValue);
  finally
    FreeANdNil(LXEROModelDateTimeHelper);
  end;
end;

class function TXEROModelDateTimeHelper.DateTimeToString
  (AValue: TDateTime): string;
var
  LXEROModelDateTimeHelper: TXEROModelDateTimeHelper;
begin
  LXEROModelDateTimeHelper := TXEROModelDateTimeHelper.Create;
  try
    Result := LXEROModelDateTimeHelper.DoDateTimeToString(AValue);
  finally
    FreeANdNil(LXEROModelDateTimeHelper);
  end;
end;

class function TXEROModelDateTimeHelper.GetFormatSettings: TFormatSettings;
var
  LXEROModelDateTimeHelper: TXEROModelDateTimeHelper;
begin
  LXEROModelDateTimeHelper := TXEROModelDateTimeHelper.Create;
  try
    Result := LXEROModelDateTimeHelper.FormatSettings;
  finally
    FreeANdNil(LXEROModelDateTimeHelper);
  end;
end;

function TXEROModelDateTimeHelper.IsXERODate(AValue: string): Boolean;
begin
  Result := Pos('/Date', AValue) > 0;
end;

{ TXEROModelDatasetMarshallerBase }

constructor TXEROModelDatasetMarshallerBase.Create(AModel: TXEROModel);
begin
  FXEROModel := AModel;
end;

destructor TXEROModelDatasetMarshallerBase.Destroy;
begin
  try
    FXEROModel := nil;
  finally
    inherited;
  end;
end;

function TXEROModelDatasetMarshallerBase.IsIgnoredProperty
  (AProp: TRttiProperty): Boolean;
var
  LPRopertyName: string;
begin
  LPRopertyName := AProp.Name;
  Result := SameText(LPRopertyName, 'Loaded') or
    SameText(LPRopertyName, 'DirtyTimeStamp') or
    SameText(LPRopertyName, 'Dirty');
  if not Result then
  begin
    Result := TXEROModelRTTI.HasAttribute(AProp, XEROModelIgnoreAttribute);
  end;
end;

{ TXEROModelDatasetMarshaller }

class procedure TXEROModelDatasetMarshaller.AddFieldDefs(AModel: TXEROModel;
ADataset: TDataSet; AClear: Boolean);
var
  LXEROModelDatasetMarshaller: TXEROModelDatasetMarshaller;
begin
  LXEROModelDatasetMarshaller := TXEROModelDatasetMarshaller.Create(AModel);
  try
    LXEROModelDatasetMarshaller.DoAddFieldDefs(ADataset, AClear);
  finally
    FreeANdNil(LXEROModelDatasetMarshaller);
  end;
end;

procedure TXEROModelDatasetMarshaller.DoAddFieldDefs(ADataset: TDataSet;
AClear: Boolean);
var
  LRecordType: TRttiType;
  LProp: TRttiProperty;
  LFieldName, LPRopertyName: string;
  LAllowProperty: Boolean;
begin
  if AClear then
    ADataset.FieldDefs.Clear;
  LRecordType := RttiContext.GetType(Model.ClassType);
  for LProp in LRecordType.GetProperties do
  begin
    LPRopertyName := LProp.Name;
    LAllowProperty := true;

    if LAllowProperty then
    begin
      LAllowProperty := LProp.IsWritable;
    end;
    if LAllowProperty then
    begin
      LAllowProperty := LProp.Visibility >= mvPublic;
    end;
    if LAllowProperty then
    begin
      LAllowProperty := not IsDatasetIgnoredProperty(LProp, false, true);
    end;
    if LAllowProperty then
    begin
      LFieldName := GetPropertyFieldName(LPRopertyName);
      if (LProp.PropertyType.Handle = TypeInfo(TDate)) then
      begin
        ADataset.FieldDefs.Add(LFieldName, ftDate);
      end;
      if (LProp.PropertyType.Handle = TypeInfo(TTime)) then
      begin
        ADataset.FieldDefs.Add(LFieldName, ftTime);
      end;
      if (LProp.PropertyType.Handle = TypeInfo(TDateTime)) then
      begin
        ADataset.FieldDefs.Add(LFieldName, ftDateTime);
      end;
      if LProp.PropertyType.Handle = TypeInfo(Boolean) then
      begin
        ADataset.FieldDefs.Add(LFieldName, ftBoolean);
      end;

      if (LProp.PropertyType.Handle = TypeInfo(string)) then
      begin
        ADataset.FieldDefs.Add(LFieldName, ftString, 255);
      end;
      if (LProp.PropertyType.Handle = TypeInfo(integer)) or
        (LProp.PropertyType.Handle = TypeInfo(int64)) then
      begin
        ADataset.FieldDefs.Add(LFieldName, ftInteger);
      end;
      if (LProp.PropertyType.Handle = TypeInfo(double)) or
        (LProp.PropertyType.Handle = TypeInfo(single)) then
      begin
        ADataset.FieldDefs.Add(LFieldName, ftFloat);
      end;
      if (LProp.PropertyType.Handle = TypeInfo(currency)) then
      begin
        ADataset.FieldDefs.Add(LFieldName, ftCurrency);
      end;
    end;
  end;
end;

procedure TXEROModelDatasetMarshaller.DoLoadFromDataset(ADataset: TDataSet);
var
  LRecordType: TRttiType;
  LProp: TRttiProperty;
  LFieldName, LPRopertyName: string;
  LField: TField;
  LHandled: Boolean;
  LAllowProperty: Boolean;
begin
  LRecordType := RttiContext.GetType(Model.ClassType);
  for LProp in LRecordType.GetProperties do
  begin
    LPRopertyName := LProp.Name;
    LAllowProperty := true;

    if LAllowProperty then
    begin
      LAllowProperty := LProp.IsWritable;
    end;
    if LAllowProperty then
    begin
      LAllowProperty := LProp.Visibility >= mvPublic;
    end;
    if LAllowProperty then
    begin
      LAllowProperty := not IsDatasetIgnoredProperty(LProp, false, true);
    end;
    if LAllowProperty then
    begin
      if MatchPropertyField(ADataset, LPRopertyName, LFieldName) then
      begin
        LField := ADataset.FieldByName(LFieldName);
        if Assigned(LField) and (not IsIgnoredField(LField, false)) then
        begin
          try
            LHandled := false;
            LoadFieldValueInternal(LField, LProp, LHandled);
          except
            on E: Exception do
            begin
              Error(E);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TXEROModelDatasetMarshaller.DoSaveToDataset(ADataset: TDataSet;
APost: Boolean);
var
  LRecordType: TRttiType;
  LProp: TRttiProperty;
  LFieldName, LPRopertyName: string;
  LField: TField;
  LHandled: Boolean;
  LAllowProperty: Boolean;
begin
  if ADataset.State in [dsEdit, dsInsert] then
  begin
    LRecordType := RttiContext.GetType(Model.ClassType);
    for LProp in LRecordType.GetProperties do
    begin
      LPRopertyName := LProp.Name;
      LAllowProperty := true;

      if LAllowProperty then
      begin
        LAllowProperty := LProp.IsReadable;
      end;
      if LAllowProperty then
      begin
        LAllowProperty := LProp.Visibility >= mvPublic;
      end;
      if LAllowProperty then
      begin
        LAllowProperty := not IsDatasetIgnoredProperty(LProp, true, false);
      end;
      if LAllowProperty then
      begin
        if MatchPropertyField(ADataset, LPRopertyName, LFieldName) then
        begin

          LField := ADataset.FieldByName(LFieldName);

          if Assigned(LField) and (not IsIgnoredField(LField, true)) then
          begin
            try
              SaveFieldValueInternal(LField, LProp, LHandled);
            except
              on E: Exception do
              begin
                Error(E);
              end;
            end;
          end;
        end;
      end;
    end;

    if APost then
    begin
      try
        ADataset.Post;
      except
        ADataset.Cancel;
        raise;
      end;
    end;
  end
  else
  begin
    raise EXEROException.Create('Dataset is not in insert or edit mode.');
  end;
end;

function TXEROModelDatasetMarshaller.IsIgnoredField(AField: TField;
ASave: Boolean): Boolean;
begin
  Result := false;
  if ASave then
  begin
    Result := (AField.DataType = ftAutoInc) or (AField.ReadOnly);
  end;
end;

function TXEROModelDatasetMarshaller.GetPropertyFieldName
  (APropertyName: string): string;
var
  LAttribute: TCustomAttribute;
begin
  Result := APropertyName;
  LAttribute := TXEROModelRTTI.GetAttribute(Model, APropertyName,
    XEROModelDatasetFieldNameAttribute);
  if LAttribute <> nil then
  begin
    Result := XEROModelDatasetFieldNameAttribute(LAttribute).FieldName;
  end;

end;

function TXEROModelDatasetMarshaller.IsDatasetIgnoredProperty
  (AProp: TRttiProperty; ASave, ALoad: Boolean): Boolean;
var
  LAttribute: TCustomAttribute;
begin
  Result := IsIgnoredProperty(AProp);
  if not Result then
  begin
    LAttribute := TXEROModelRTTI.GetAttribute(AProp,
      XEROModelDatasetIgnoreAttribute);
    if LAttribute <> nil then
    begin
      if ALoad then
        Result := XEROModelDatasetIgnoreAttribute(LAttribute).Load;
      if ASave then
        Result := XEROModelDatasetIgnoreAttribute(LAttribute).Save;
    end;
  end;
end;

function TXEROModelDatasetMarshaller.MatchPropertyField(ADataset: TDataSet;
APropertyName: string; var AFieldName: string): Boolean;
var
  LFieldIdx: integer;
  LFieldName: string;
begin
  Result := false;
  AFieldName := '';
  LFieldIdx := 0;
  try
    LFieldName := GetPropertyFieldName(APropertyName);

    if ADataset.FindField(LFieldName) <> nil then
    begin
      AFieldName := LFieldName;
      Result := true;
    end;

    while (not Result) and (LFieldIdx < ADataset.FieldDefs.Count) do
    begin
      LFieldName := ADataset.FieldDefs[LFieldIdx].DisplayName;
      LFieldName := StringReplace(LFieldName, '_', '',
        [rfReplaceAll, rfIgnoreCase]);
      if SameText(APropertyName, LFieldName) then
      begin
        AFieldName := ADataset.FieldDefs[LFieldIdx].DisplayName;
        Result := true;
      end;
      Inc(LFieldIdx);
    end;
  except
    on E: Exception do
    begin
      Error(E);
    end;
  end;
end;

procedure TXEROModelDatasetMarshaller.LoadFieldValue(AField: TField;
AProp: TRttiProperty; var AHandled: Boolean);
begin
  AHandled := false;
end;

procedure TXEROModelDatasetMarshaller.LoadFieldValueInternal(AField: TField;
AProp: TRttiProperty; var AHandled: Boolean);
var
  LValue: TValue;
begin

  LoadFieldValue(AField, AProp, AHandled);

  if not AHandled then
  begin
    if AField.IsNull then
    begin
      AProp.SetValue(Model, TValue.Empty);
      AHandled := true;
    end;
  end;

  if not AHandled then
  begin
    if (AProp.PropertyType.Handle = TypeInfo(string)) then
    begin
      case AField.DataType of

        ftString, ftWideString, ftMemo, ftWideMemo:
          begin
            LValue := AField.AsString;
            AProp.SetValue(Model, LValue);
            AHandled := true;
          end;
        ftDate, ftTime, ftDateTime:
          begin
            if not AField.IsNull then
            begin
              case AField.DataType of
                ftDate:
                  begin
                    LValue := Model.XEROModelDateTimeHelperClass.DateToString
                      (AField.AsDateTime);
                  end;
                ftTime:
                  begin
                    LValue := Model.XEROModelDateTimeHelperClass.TimeToString
                      (AField.AsDateTime);
                  end;
              else
                begin
                  LValue := Model.XEROModelDateTimeHelperClass.DateTimeToString
                    (AField.AsDateTime);
                end;
              end;
            end
            else
            begin
              LValue := '';
            end;
            AProp.SetValue(Model, LValue);
            AHandled := true;
          end;
      else
        begin
          AProp.SetValue(Model, AField.DisplayText);
          AHandled := true;
        end;
      end;
    end;
  end;

  if not AHandled then
  begin
    if (AProp.PropertyType.Handle = TypeInfo(TDateTime)) or
      (AProp.PropertyType.Handle = TypeInfo(TDate)) or
      (AProp.PropertyType.Handle = TypeInfo(TTime)) then
    begin
      case AField.DataType of
        ftDateTime, ftDate, ftTime, ftTimeStamp:
          begin
            AProp.SetValue(Model, AField.AsDateTime);
            AHandled := true;
          end;
        ftString, ftWideString, ftMemo, ftWideMemo:
          begin
            AProp.SetValue(Model, StrToDateTimeDef(AField.AsString, 0));
            AHandled := true;
          end;
      end;
    end;
  end;

  if not AHandled then
  begin
    if AProp.PropertyType.Handle = TypeInfo(Boolean) then
    begin
      case AField.DataType of
        ftBoolean:
          begin
            AProp.SetValue(Model, AField.AsBoolean);
            AHandled := true;
          end;
        ftInteger, ftSmallint, ftLargeint, ftWord, ftLongWord, ftShortint:
          begin
            LValue := AField.AsInteger = 1;
            AProp.SetValue(Model, LValue);
            AHandled := true;
          end;
        ftString, ftWideString, ftMemo, ftWideMemo:
          begin
            if SameText(AField.AsString, 'yes') or
              SameText(AField.AsString, 'true') or
              SameText(AField.AsString, 'y') or
              (StrToBoolDef(AField.AsString, false) = true) then
            begin
              AProp.SetValue(Model, true);
            end
            else
            begin
              AProp.SetValue(Model, false);
            end;
            AHandled := true;
          end;
      end;
    end;
  end;

  if not AHandled then
  begin
    case AField.DataType of
      ftBCD, ftFMTBcd:
        begin
          LValue := AField.AsFloat;
          AProp.SetValue(Model, LValue);
          AHandled := true;
        end
    else
      begin
        LValue := TValue.FromVariant(AField.Value);
        AProp.SetValue(Model, LValue);
        AHandled := true;
      end;
    end;
  end;
end;

procedure TXEROModelDatasetMarshaller.SaveFieldValue(AField: TField;
AProp: TRttiProperty; var AHandled: Boolean);
begin
  AHandled := false;
end;

procedure TXEROModelDatasetMarshaller.SaveFieldValueInternal(AField: TField;
AProp: TRttiProperty; var AHandled: Boolean);
var
  LValue: TValue;
begin
  AHandled := false;

  SaveFieldValue(AField, AProp, AHandled);

  LValue := AProp.GetValue(Model);

  if LValue.IsEmpty then
  begin
    AField.Clear;
    AHandled := true;
  end
  else
  begin
    case AField.DataType of
      ftDate:
        begin
          if (AProp.PropertyType.Handle = TypeInfo(string)) then
          begin
            AField.AsDateTime := Model.XEROModelDateTimeHelperClass.StringToDate
              (LValue.AsString);
          end;
          if (AProp.PropertyType.Handle = TypeInfo(TDate)) or
            (AProp.PropertyType.Handle = TypeInfo(TDateTime)) or
            (AProp.PropertyType.Handle = TypeInfo(TTime)) then
          begin
            AField.AsDateTime := LValue.AsExtended;
            AHandled := true;
          end;
          if not AHandled then
          begin
            AField.Clear;
            AHandled := true;
          end;
        end;
      ftTime:
        begin
          if (AProp.PropertyType.Handle = TypeInfo(string)) then
          begin
            AField.AsDateTime := Model.XEROModelDateTimeHelperClass.StringToTime
              (LValue.AsString);
            AHandled := true;
          end;
          if (AProp.PropertyType.Handle = TypeInfo(TDate)) or
            (AProp.PropertyType.Handle = TypeInfo(TDateTime)) or
            (AProp.PropertyType.Handle = TypeInfo(TTime)) then
          begin
            AField.AsDateTime := LValue.AsExtended;
            AHandled := true;
          end;
          if not AHandled then
          begin
            AField.Clear;
            AHandled := true;
          end;
        end;
      ftDateTime:
        begin
          if (AProp.PropertyType.Handle = TypeInfo(string)) then
          begin
            AField.AsDateTime := Model.XEROModelDateTimeHelperClass.
              StringToDateTime(LValue.AsString);
          end;
          if (AProp.PropertyType.Handle = TypeInfo(TDate)) or
            (AProp.PropertyType.Handle = TypeInfo(TDateTime)) or
            (AProp.PropertyType.Handle = TypeInfo(TTime)) then
          begin
            AField.AsDateTime := LValue.AsExtended;
            AHandled := true;
          end;
          if not AHandled then
          begin
            AField.Clear;
            AHandled := true;
          end;
        end
    else
      AField.Value := LValue.AsVariant;
      AHandled := true;
    end;

  end;
end;

class procedure TXEROModelDatasetMarshaller.LoadFromDataset(AModel: TXEROModel;
ADataset: TDataSet);
var
  LXEROModelDatasetMarshaller: TXEROModelDatasetMarshaller;
begin
  LXEROModelDatasetMarshaller := TXEROModelDatasetMarshaller.Create(AModel);
  try
    LXEROModelDatasetMarshaller.DoLoadFromDataset(ADataset);
  finally
    FreeANdNil(LXEROModelDatasetMarshaller);
  end;
end;

class procedure TXEROModelDatasetMarshaller.SaveToDataset(AModel: TXEROModel;
ADataset: TDataSet; APost: Boolean);
var
  LXEROModelDatasetMarshaller: TXEROModelDatasetMarshaller;
begin
  LXEROModelDatasetMarshaller := TXEROModelDatasetMarshaller.Create(AModel);
  try
    LXEROModelDatasetMarshaller.DoSaveToDataset(ADataset, APost);
  finally
    FreeANdNil(LXEROModelDatasetMarshaller);
  end;
end;

procedure TXEROModelJSONMarshaller.DoDeserializePropertyInternal
  (AProp: TRttiProperty; AJsonValue: TJSONPair; var AHandled: Boolean);
var
  LValue: TValue;
begin
  AHandled := false;

  DoDeserializeProperty(AProp, AJsonValue, AHandled);

  if not AHandled then
  begin
    if AJsonValue.Null then
    begin
      AProp.SetValue(Model, TValue.Empty);
      AHandled := true;
    end;
  end;

  if not AHandled then
  begin
    if (AProp.PropertyType.Handle = TypeInfo(string)) then
    begin
      LValue := (AJsonValue.JsonValue as TJSONString).Value;
      AProp.SetValue(Model, LValue);
      AHandled := true;
    end;
  end;

  if not AHandled then
  begin

    if (AProp.PropertyType.Handle = TypeInfo(TDate)) then
    begin
      LValue := Model.XEROModelDateTimeHelperClass.StringToDate
        ((AJsonValue.JsonValue as TJSONString).Value);
      AProp.SetValue(Model, LValue);
      AHandled := true;
    end;
    if (AProp.PropertyType.Handle = TypeInfo(TTime)) then
    begin
      LValue := Model.XEROModelDateTimeHelperClass.StringToTime
        ((AJsonValue.JsonValue as TJSONString).Value);
      AProp.SetValue(Model, LValue);
      AHandled := true;
    end;
    if (AProp.PropertyType.Handle = TypeInfo(TDateTime)) then
    begin
      LValue := Model.XEROModelDateTimeHelperClass.StringToDateTime
        ((AJsonValue.JsonValue as TJSONString).Value);
      AProp.SetValue(Model, LValue);
      AHandled := true;
    end;

  end;

  if not AHandled then
  begin
    if (AProp.PropertyType.Handle = TypeInfo(integer)) then
    begin
      LValue := (AJsonValue.JsonValue as TJSONNumber).AsInt;
      AProp.SetValue(Model, LValue);
      AHandled := true;
    end;
    if (AProp.PropertyType.Handle = TypeInfo(int64)) then
    begin
      LValue := (AJsonValue.JsonValue as TJSONNumber).AsInt64;
      AProp.SetValue(Model, LValue);
      AHandled := true;
    end;
  end;

  if not AHandled then
  begin
    if (AProp.PropertyType.Handle = TypeInfo(double)) or
      (AProp.PropertyType.Handle = TypeInfo(single)) or
      (AProp.PropertyType.Handle = TypeInfo(currency)) then
    begin
      LValue := (AJsonValue.JsonValue as TJSONNumber).AsDouble;
      AProp.SetValue(Model, LValue);
      AHandled := true;
    end;
  end;

  if not AHandled then
  begin
    if AProp.PropertyType.Handle = TypeInfo(Boolean) then
    begin
      AProp.SetValue(Model, (AJsonValue.JsonValue as TJSONBool).AsBoolean);
      AHandled := true;
    end;
  end;
end;

function TXEROModelJSONMarshaller.DoFormat(AJsonValue: TJSONValue): string;
begin
  try
    Result := AJsonValue.Format;
  except
    Result := AJsonValue.ToJSON;
  end;
end;

function TXEROModelJSONMarshaller.DoFormat(AJSON: string): string;
var
  LJsonValue: TJSONValue;
begin
  try
    LJsonValue := DefaultJSONParse(AJSON);
    if Assigned(LJsonValue) then
    begin
      Result := DoFormat(LJsonValue);
      LJsonValue.Free;
    end;
  except
    Result := AJSON;
  end;
end;

procedure TXEROModelJSONMarshaller.DoDeserializeClassInternal
  (AProp: TRttiProperty; AJsonValue: TJSONPair; var AHandled: Boolean);
var
  LValue: TValue;
  LJSONMeth: TRttiMethod;
  LObject: TObject;
begin
  DoDeserializeClass(AProp, AJsonValue, AHandled);
  if not AHandled then
  begin
    LJSONMeth := AProp.PropertyType.GetMethod('FromJSON');
    if Assigned(LJSONMeth) then
    begin
      LValue := AProp.GetValue(Model);
      LJSONMeth.Invoke(LValue, [AJsonValue.JsonValue.ToJSON]);
    end
    else
    begin
      LValue := AProp.GetValue(Model);
      if LValue.IsObject then
      begin
        LObject := LValue.AsObject;
        DefaultObjectDeserialize(LObject, AJsonValue.JsonValue);
      end;
    end;
  end;
end;

function TXEROModelJSONMarshaller.DoSerializePropertyInternal
  (AProp: TRttiProperty): TJSONValue;
var
  LDateTime: TDateTime;
  LExtended: Extended;
  LValue: string;
  LHandled: Boolean;
begin
  Result := DoSerializeProperty(AProp);
  LHandled := Assigned(Result);

  if not LHandled then
    if (AProp.PropertyType.TypeKind in [tkInteger, tkInt64]) then
    begin
      Result := TJSONNumber.Create(AProp.GetValue(Model).AsInteger);
      LHandled := true;
    end;

  if not LHandled then
    if AProp.PropertyType.Handle = TypeInfo(Boolean) then
    begin
      Result := TJSONBool.Create(AProp.GetValue(Model).AsBoolean);
      LHandled := true;
    end;

  if not LHandled then
  begin
    if (AProp.PropertyType.Handle = TypeInfo(TDate)) then
    begin
      LHandled := true;
      LDateTime := AProp.GetValue(Model).AsExtended;
      if LDateTime <> 0 then
      begin
        Result := TJSONString.Create
          (Model.XEROModelDateTimeHelperClass.DateToString(LDateTime));
      end;
    end;
    if (AProp.PropertyType.Handle = TypeInfo(TTime)) then
    begin
      LHandled := true;
      LDateTime := AProp.GetValue(Model).AsExtended;
      if LDateTime <> 0 then
        Result := TJSONString.Create
          (Model.XEROModelDateTimeHelperClass.TimeToString(LDateTime));
    end;
    if (AProp.PropertyType.Handle = TypeInfo(TDateTime)) then
    begin
      LHandled := true;
      LDateTime := AProp.GetValue(Model).AsExtended;
      if LDateTime <> 0 then
        Result := TJSONString.Create
          (Model.XEROModelDateTimeHelperClass.DateTimeToString(LDateTime));
    end;
  end;

  if not LHandled then
    if (AProp.PropertyType.TypeKind in [tkFloat]) then
    begin
      LExtended := AProp.GetValue(Model).AsExtended;
      LExtended := RoundTo(LExtended, -4);
      Result := TJSONNumber.Create(LExtended);
      LHandled := true;
    end;

  if not LHandled then
    if (AProp.PropertyType.TypeKind in [tkString, tkLString, tkWString,
      tkUString]) then
    begin
      LHandled := true;
      LValue := AProp.GetValue(Model).AsType<string>;
      if not IsEmptyString(LValue) then
      begin
        Result := TJSONString.Create(LValue);
      end
      else
      begin
        Result := nil;
      end;
    end;
  if (not LHandled) then
  begin
    Result := TJSONString.Create(AProp.GetValue(Model).ToString);
  end;
end;

class function TXEROModelJSONMarshaller.Format(AJSON: string): string;
var
  LXEROModelJSONMarshaller: TXEROModelJSONMarshaller;
begin
  LXEROModelJSONMarshaller := TXEROModelJSONMarshaller.Create(nil);
  try
    Result := LXEROModelJSONMarshaller.DoFormat(AJSON);
  finally
    FreeANdNil(LXEROModelJSONMarshaller);
  end;
end;

function TXEROModelJSONMarshaller.DoSerializeClassInternal(AProp: TRttiProperty)
  : TJSONValue;
var
  LValue: TValue;
  LJSONMeth: TRttiMethod;
begin
  Result := DoSerializeClass(AProp);

  if not Assigned(Result) then
  begin
    LJSONMeth := AProp.PropertyType.GetMethod('AsJSON');
    if Assigned(LJSONMeth) then
    begin
      LValue := AProp.GetValue(Model);
      if not LValue.IsEmpty then
      begin
        LValue := LJSONMeth.Invoke(LValue, [false]);
        Result := TJSONObject.ParseJSONValue(LValue.AsString);
      end;
    end
    else
    begin
      LValue := AProp.GetValue(Model);
      if not LValue.IsEmpty then
      begin
        Result := DefaultObjectSerialize(LValue.AsObject);
      end;
    end;
  end;
end;

procedure TXEROModelJSONMarshaller.DoDeserializeProperty(AProp: TRttiProperty;
AJsonValue: TJSONPair; var AHandled: Boolean);
begin
  AHandled := false;
end;

procedure TXEROModelJSONMarshaller.DoDeserializeClass(AProp: TRttiProperty;
AJsonValue: TJSONPair; var AHandled: Boolean);
begin
  AHandled := false;
end;

function TXEROModelJSONMarshaller.DoSerializeProperty(AProp: TRttiProperty)
  : TJSONValue;
begin
  Result := nil;
end;

function TXEROModelJSONMarshaller.DoSerializeClass(AProp: TRttiProperty)
  : TJSONValue;
begin
  Result := nil;
end;

{ TXEROModelJSONMarshaller }

procedure TXEROModelJSONMarshaller.DefaultObjectDeserialize(AObject: TObject;
AJsonValue: TJSONValue);
var
  LJSONUnMarshal: TJSONUnMarshal;
begin
  if Assigned(AObject) then
  begin
    LJSONUnMarshal := TJSONUnMarshal.Create;
    try
      LJSONUnMarshal.CreateObject(AObject.ClassType,
        AJsonValue as TJSONObject, AObject);
    finally
      FreeANdNil(LJSONUnMarshal);
    end;
  end;
end;

function TXEROModelJSONMarshaller.DefaultObjectSerialize(AObject: TObject)
  : TJSONValue;
begin
  Result := nil;
  if Assigned(AObject) then
  begin
    Result := TJson.ObjectToJsonObject(AObject);
  end;

end;

function TXEROModelJSONMarshaller.DoSerialize(AFormat: Boolean): string;
var
  LJSON: TJSONObject;
  LRecordType: TRttiType;
  LProp: TRttiProperty;
  LPRopertyName: string;
  LAllowProperty: Boolean;
  LValue: TJSONValue;
begin
  LJSON := TJSONObject.Create;
  try
    LRecordType := RttiContext.GetType(Model.ClassType);
    for LProp in LRecordType.GetProperties do
    begin
      LAllowProperty := true;

      if LAllowProperty then
      begin
        LAllowProperty := LProp.IsReadable;
      end;
      if LAllowProperty then
      begin
        LAllowProperty := LProp.Visibility >= mvPublic;
      end;
      if LAllowProperty then
      begin
        LAllowProperty := not IsIgnoredProperty(LProp);
      end;
      if LAllowProperty then
      begin

        LPRopertyName := GetPropertyJSONName(LProp);

        if (LProp.PropertyType.TypeKind = tkClass) then
        begin
          LValue := DoSerializeClassInternal(LProp);
        end
        else
        begin
          LValue := DoSerializePropertyInternal(LProp);
        end;

        if Assigned(LValue) then
        begin
          LJSON.AddPair(LPRopertyName, LValue);
        end;
      end;
    end;
    if AFormat then
    begin
      Result := DoFormat(LJSON);
    end
    else
    begin
      Result := LJSON.ToJSON;
    end;
  finally
    LJSON.Free;
  end;
end;

function TXEROModelJSONMarshaller.DefaultJSONParse(AJSON: string): TJSONValue;
begin
  Result := nil;
  try
    Result := TJSONObject.ParseJSONValue(AJSON, true);
  except
    on E: Exception do
    begin
      Error(E);
    end;
  end;
end;

procedure TXEROModelJSONMarshaller.DoDeserialize(AJSON: string);
var
  LRecordType: TRttiType;
  LProp: TRttiProperty;
  LPRopertyName: string;
  LHandled: Boolean;
  LAllowProperty: Boolean;
  LJSON: TJSONObject;
  LJsonValue: TJSONValue;
  LJSONPair: TJSONPair;
begin
  Model.Reset;
  if IsEmptyString(AJSON) or SameText('null', AJSON) then
  begin
    Exit;
  end;

  LJSON := nil;
  LJsonValue := DefaultJSONParse(AJSON);
  if Assigned(LJsonValue) then
  begin
    if (LJsonValue is TJSONObject) then
    begin
      LJSON := LJsonValue as TJSONObject;
    end;
  end;

  if Assigned(LJSON) then
  begin
    try
      LRecordType := RttiContext.GetType(Model.ClassType);
      for LProp in LRecordType.GetProperties do
      begin
        LPRopertyName := LProp.Name;
        LAllowProperty := true;

        if LAllowProperty then
        begin
          LAllowProperty := LProp.Visibility >= mvPublic;
        end;
        if LAllowProperty then
        begin
          LAllowProperty := not IsIgnoredProperty(LProp);
        end;
        if LAllowProperty then
        begin
          LPRopertyName := GetPropertyJSONName(LProp);
          LJSONPair := LJSON.Get(LPRopertyName);

          if Assigned(LJSONPair) then
          begin
            try
              LHandled := false;
              if (LProp.PropertyType.TypeKind = tkClass) then
              begin
                DoDeserializeClassInternal(LProp, LJSONPair, LHandled);
              end
              else
              begin
                if LProp.IsWritable then
                begin
                  DoDeserializePropertyInternal(LProp, LJSONPair, LHandled);
                end;
              end;

            except
              on E: Exception do
              begin
                Error(E);
              end;
            end;
          end
          else
          begin
{$IFDEF DEBUG}
            Debug('DoDeserialize',
              SysUtils.Format
              ('Failed to get property, check your property case %s (PropertyName: %s)',
              [LPRopertyName, LProp.Name]));
{$ENDIF}
          end;
        end;

      end;
    finally
      FreeANdNil(LJSON);
    end;
  end
  else
  begin
    raise EXEROException.Create('Invalid JSON');
  end;
end;

function TXEROModelJSONMarshaller.GetPropertyJSONName
  (AProp: TRttiProperty): string;
var
  LAttribute: TCustomAttribute;
begin
  Result := AProp.Name;
  // if Length(Result) > 0 then
  // begin
  // LlowerText := AnsiLeftStr(Result, 1);
  // LlowerText := AnsiLowerCase(LlowerText);
  // LrestText := AnsiRightStr(Result, Length(Result) - 1);
  // Result := LlowerText + LrestText;
  // end
  // else
  // begin
  // Result := Result;
  // end;
  LAttribute := TXEROModelRTTI.GetAttribute(AProp,
    XEROModelJSONPropertyNameAttribute);
  if LAttribute <> nil then
  begin
    Result := XEROModelJSONPropertyNameAttribute(LAttribute).PropertyName;
  end;

end;

class function TXEROModelJSONMarshaller.Serialize(AXEROModel: TXEROModel;
AFormat: Boolean): string;
var
  LXEROModelJSONMarshaller: TXEROModelJSONMarshaller;
begin
  LXEROModelJSONMarshaller := TXEROModelJSONMarshaller.Create(AXEROModel);
  try
    Result := LXEROModelJSONMarshaller.DoSerialize(AFormat);
  finally
    FreeANdNil(LXEROModelJSONMarshaller);
  end;
end;

class procedure TXEROModelJSONMarshaller.Deserialize(AJSON: string;
AXEROModel: TXEROModel);
var
  LXEROModelJSONMarshaller: TXEROModelJSONMarshaller;
begin
  LXEROModelJSONMarshaller := TXEROModelJSONMarshaller.Create(AXEROModel);
  try
    LXEROModelJSONMarshaller.DoDeserialize(AJSON);
  finally
    FreeANdNil(LXEROModelJSONMarshaller);
  end;
end;

initialization

DefaultXEROModelDateTimeHelperClass := TXEROModelDateTimeHelper;
DefaultXEROModelDatasetMarshallerClass := TXEROModelDatasetMarshaller;
DefaultXEROModelJSONMarshallerClass := TXEROModelJSONMarshaller;

end.
