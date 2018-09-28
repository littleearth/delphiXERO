unit frmXERODemoU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls, XERO.API, XERO.Model.Dataset,
  Data.DB, Vcl.Grids, Vcl.DBGrids;

type
  TfrmXERODemo = class(TForm)
    DataSourceDataset: TDataSource;
    PageControlMenu: TPageControl;
    tabAPI: TTabSheet;
    tabDataset: TTabSheet;
    PageControlData: TPageControl;
    tabDataJSON: TTabSheet;
    memoJSON: TMemo;
    tabLog: TTabSheet;
    Panel13: TPanel;
    btnRefreshLog: TButton;
    memoLog: TMemo;
    tabOptions: TTabSheet;
    PageControlOptions: TPageControl;
    tabConsumerDetails: TTabSheet;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    lblFileName: TLabel;
    lblInfo: TLabel;
    editConsumerKey: TEdit;
    editConsumerSecret: TEdit;
    tabPublicKey: TTabSheet;
    memoPublicKey: TMemo;
    tabPrivateKey: TTabSheet;
    memoPrivateKey: TMemo;
    tabAbout: TTabSheet;
    GroupBox4: TGroupBox;
    Image1: TImage;
    memoAbout: TMemo;
    Panel14: TPanel;
    GroupBoxJSONDatasetType: TGroupBox;
    DBGridDataset: TDBGrid;
    memoDatasetJSON: TMemo;
    Panel15: TPanel;
    btnJSONtoDataset: TButton;
    PageControlMain: TPanel;
    Panel1: TPanel;
    btnSearch: TButton;
    Panel4: TPanel;
    Label1: TLabel;
    editPage: TEdit;
    Panel6: TPanel;
    Label5: TLabel;
    editOrderBy: TEdit;
    PageControlSearch: TPageControl;
    tabContacts: TTabSheet;
    Panel2: TPanel;
    Panel5: TPanel;
    Label2: TLabel;
    editContactsContactID: TEdit;
    Panel7: TPanel;
    Label6: TLabel;
    editContactsContactNumber: TEdit;
    Panel8: TPanel;
    cbContactsIncludeArchived: TCheckBox;
    Panel3: TPanel;
    tabItems: TTabSheet;
    tabAccounts: TTabSheet;
    tabInvoices: TTabSheet;
    Panel9: TPanel;
    Panel10: TPanel;
    Label7: TLabel;
    editInvoicesInvoiceNumber: TEdit;
    Panel11: TPanel;
    Label8: TLabel;
    editInvoicesInvoiceID: TEdit;
    Panel12: TPanel;
    btnJSONClear: TButton;
    btnJSONPaste: TButton;
    Panel16: TPanel;
    btnAPIJSONCopy: TButton;
    btnAPIJsonToDataset: TButton;
    tabCustomSearch: TTabSheet;
    Panel17: TPanel;
    Panel18: TPanel;
    Label9: TLabel;
    editSearchCustomParams: TEdit;
    Panel19: TPanel;
    Label10: TLabel;
    editSearchCustomURL: TEdit;
    Panel20: TPanel;
    Panel22: TPanel;
    Label12: TLabel;
    editSearchCustomLastModified: TDateTimePicker;
    procedure btnAPIJSONCopyClick(Sender: TObject);
    procedure btnAPIJsonToDatasetClick(Sender: TObject);
    procedure btnJSONClearClick(Sender: TObject);
    procedure btnJSONPasteClick(Sender: TObject);
    procedure btnJSONtoDatasetClick(Sender: TObject);
    procedure btnRefreshLogClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblFileNameDblClick(Sender: TObject);
    procedure PageControlDataChange(Sender: TObject);
  private
    FSettingsFileName: TFileName;
    FXEROAppDetails: TXEROAppDetails;
    FDatasetList: TXEROModelDatasets;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure ApplySettings;
    procedure SearchContacts;
    procedure SearchItems;
    procedure SearchAccounts;
    procedure SearchInvoices;
    procedure SearchCustom;
    function GetPage: integer;
    function GetOrderBy: string;
    procedure DestroyDataset;
    procedure CreateDataset;
    procedure ResizeDBGrid(ADBGrid: TDBGrid);
  public
    { Public declarations }
  end;

var
  frmXERODemo: TfrmXERODemo;

implementation

{$R *.dfm}

uses
  System.IniFiles, System.DateUtils, XERO.Utils,
  XERO.Log, XERO.Log.Basic, XERO.Response.Model,
  XERO.Contacts, XERO.Contacts.Dataset, XERO.API.JSON,
  XERO.Items,
  XERO.Accounts,
  XERO.Invoices;

procedure TfrmXERODemo.ResizeDBGrid(ADBGrid: TDBGrid);
var
  ColumnIdx: integer;
begin
  with ADBGrid do
  begin
    Columns.BeginUpdate;
    try
      try
        for ColumnIdx := 0 to Pred(Columns.Count) do
        begin
          if Assigned(Columns.Items[ColumnIdx].Field) then
          begin
            with Columns.Items[ColumnIdx] do
            begin
              case Columns.Items[ColumnIdx].Field.DataType of
                ftString, ftMemo, ftfmtMemo, ftWideString, ftWideMemo:
                  begin
                    Columns.Items[ColumnIdx].Width := 100;
                  end;
                ftBoolean, ftSmallint, ftBytes:
                  begin
                    Columns.Items[ColumnIdx].Width := 50;
                  end;
                ftDate, ftTime, ftDateTime:
                  begin
                    Columns.Items[ColumnIdx].Width := 75;
                  end;
              else
                begin
                  Columns.Items[ColumnIdx].Width := 50;
                end;
              end;
            end;
          end;
        end;
      except
      end;
    finally
      Columns.EndUpdate;
    end;
  end;
end;

procedure TfrmXERODemo.FormDestroy(Sender: TObject);
begin
  SaveSettings;
  FreeAndNil(FDatasetList);
end;

function TfrmXERODemo.GetOrderBy: string;
begin
  Result := editOrderBy.Text;
end;

function TfrmXERODemo.GetPage: integer;
begin
  Result := StrToIntDef(editPage.Text, 0);
end;

procedure TfrmXERODemo.ApplySettings;
begin
  FXEROAppDetails.Privatekey.Text := memoPrivateKey.Lines.Text;
  FXEROAppDetails.PublicKey.Text := memoPublicKey.Lines.Text;
  FXEROAppDetails.ConsumerKey := editConsumerKey.Text;
  FXEROAppDetails.ConsumerSecret := editConsumerSecret.Text;
end;

procedure TfrmXERODemo.btnAPIJSONCopyClick(Sender: TObject);
begin
  memoJSON.CopyToClipboard;
end;

procedure TfrmXERODemo.btnAPIJsonToDatasetClick(Sender: TObject);
begin
  PageControlMenu.ActivePage := tabDataset;
  memoDatasetJSON.Text := memoJSON.Text;
  btnJSONtoDatasetClick(Sender);
end;

procedure TfrmXERODemo.btnJSONClearClick(Sender: TObject);
begin
  memoDatasetJSON.Clear;
  DestroyDataset;
end;

procedure TfrmXERODemo.btnJSONPasteClick(Sender: TObject);
begin
  memoDatasetJSON.PasteFromClipboard;
end;

procedure TfrmXERODemo.btnJSONtoDatasetClick(Sender: TObject);
begin
  CreateDataset;
end;

procedure TfrmXERODemo.btnRefreshLogClick(Sender: TObject);
begin
  memoLog.Text := (XEROLog as TXEROLogBasic).LogText;
end;

procedure TfrmXERODemo.btnSearchClick(Sender: TObject);
begin
  ApplySettings;
  PageControlData.ActivePageIndex := 0;
  memoLog.Clear;
  if PageControlSearch.ActivePage = tabContacts then
  begin
    SearchContacts;
  end;
  if PageControlSearch.ActivePage = tabItems then
  begin
    SearchItems;
  end;
  if PageControlSearch.ActivePage = tabAccounts then
  begin
    SearchAccounts;
  end;
  if PageControlSearch.ActivePage = tabInvoices then
  begin
    SearchInvoices;
  end;
  if PageControlSearch.ActivePage = tabCustomSearch then
  begin
    SearchCustom;
  end;

end;

procedure TfrmXERODemo.CreateDataset;
var
  LResponse: TXEROResponse;
begin
  DestroyDataset;
  if Pos('"Contacts"', memoDatasetJSON.Text) > 0 then
  begin
    LResponse := TXEROContactResponse.Create;
    try
      LResponse.FromJSON(memoDatasetJSON.Text);

      with FDatasetList.Add<TXEROContactDataset>(TXEROContactDataset.Create) do
      begin
        StoreModelList((LResponse as TXEROContactResponse).Contacts);
        DataSourceDataset.Dataset := Dataset;
      end;
    finally
      FreeAndNil(LResponse);
    end;
  end;

  if Assigned(DataSourceDataset.Dataset) then
  begin
    ResizeDBGrid(DBGridDataset);
  end
  else
  begin
    MessageDlg('Failed to convert JSON', mtError, [mbOK], 0);
  end;
end;

procedure TfrmXERODemo.SearchCustom;
var
  LXEROAPI: TXEROApiJSON;
  LJSON: string;
  LLastModified: TDate;
begin
  LXEROAPI := TXEROApiJSON.Create(nil);
  try
    LXEROAPI.XEROAppDetails := FXEROAppDetails;
    LLastModified := 0;
    if editSearchCustomLastModified.Checked then
      LLastModified := editSearchCustomLastModified.date;
    LJSON := LXEROAPI.Get(editSearchCustomURL.Text, editSearchCustomParams.Text,
      LLastModified);
    memoJSON.Text := LJSON;
  finally
    FreeAndNil(LXEROAPI);
  end;
end;

procedure TfrmXERODemo.DestroyDataset;
begin
  if Assigned(DataSourceDataset.Dataset) then
  begin
    DataSourceDataset.Dataset := nil;
  end;
  FDatasetList.Clear;
end;

procedure TfrmXERODemo.FormCreate(Sender: TObject);
begin
  FXEROAppDetails := TXEROAppDetails.Create(Self);
  PageControlMenu.ActivePageIndex := 0;
  PageControlOptions.ActivePageIndex := 0;
  PageControlSearch.ActivePageIndex := 0;
  PageControlData.ActivePageIndex := 0;
  FSettingsFileName := GetUserAppDataDir + 'settings.ini';
  FDatasetList := TXEROModelDatasets.Create;
  LoadSettings;
end;

procedure TfrmXERODemo.lblFileNameDblClick(Sender: TObject);
begin
  ExecuteFile('open', lblFileName.Hint, '', ExtractFilePath(lblFileName.Hint),
    SW_NORMAL);
end;

procedure TfrmXERODemo.LoadSettings;
var
  INIFile: TIniFile;
begin
  INIFile := TIniFile.Create(FSettingsFileName);
  try
    editConsumerKey.Text := INIFile.ReadString('XERO', 'ConsumerKey', '');
    editConsumerSecret.Text := INIFile.ReadString('XERO', 'ConsumerSecret', '');
    memoPrivateKey.Lines.Text := INIFile.ReadString('XERO', 'PrivateKey', '');
    memoPublicKey.Lines.Text := INIFile.ReadString('XERO', 'PublicKey', '');
    lblFileName.Hint := FSettingsFileName;
    lblFileName.Caption := ExtractFileName(FSettingsFileName);
  finally
    FreeAndNil(INIFile);
  end;
end;

procedure TfrmXERODemo.PageControlDataChange(Sender: TObject);
begin
  if PageControlData.ActivePage = tabLog then
  begin
    btnRefreshLogClick(Sender);
  end;
end;

procedure TfrmXERODemo.SaveSettings;
var
  INIFile: TIniFile;
  PublicKey, Privatekey: string;
begin
  INIFile := TIniFile.Create(FSettingsFileName);
  try
    PublicKey := StringReplace(memoPublicKey.Lines.Text, #13, '',
      [rfReplaceAll, rfIgnoreCase]);
    PublicKey := StringReplace(PublicKey, #10, '',
      [rfReplaceAll, rfIgnoreCase]);

    Privatekey := StringReplace(memoPrivateKey.Lines.Text, #13, '',
      [rfReplaceAll, rfIgnoreCase]);
    Privatekey := StringReplace(Privatekey, #10, '',
      [rfReplaceAll, rfIgnoreCase]);

    INIFile.WriteString('XERO', 'ConsumerKey', editConsumerKey.Text);
    INIFile.WriteString('XERO', 'ConsumerSecret', editConsumerSecret.Text);
    INIFile.WriteString('XERO', 'PrivateKey', Privatekey);
    INIFile.WriteString('XERO', 'PublicKey', PublicKey);
  finally
    FreeAndNil(INIFile);
  end;
end;

procedure TfrmXERODemo.SearchContacts;
var
  LAPI: TXEROContacts;
  LData: TXEROContactResponse;
begin
  LAPI := TXEROContacts.Create(nil);
  try
    LAPI.XEROAppDetails := FXEROAppDetails;
    LData := LAPI.Search(GetPage, GetOrderBy, editContactsContactID.Text,
      editContactsContactNumber.Text, '', cbContactsIncludeArchived.Checked);
    memoJSON.Text := LData.AsJSON(true);
  finally
    FreeAndNil(LAPI);
  end;
end;

procedure TfrmXERODemo.SearchItems;
var
  LAPI: TXEROItems;
  LData: TXEROItemResponse;
begin
  LAPI := TXEROItems.Create(nil);
  try
    LAPI.XEROAppDetails := FXEROAppDetails;
    LData := LAPI.Search(GetPage, GetOrderBy, '');
    memoJSON.Text := LData.AsJSON(true);
  finally
    FreeAndNil(LAPI);
  end;
end;

procedure TfrmXERODemo.SearchAccounts;
var
  LAPI: TXEROAccounts;
  LData: TXEROAccountResponse;
begin
  LAPI := TXEROAccounts.Create(nil);
  try
    LAPI.XEROAppDetails := FXEROAppDetails;
    LData := LAPI.Search(GetPage, GetOrderBy, '');
    memoJSON.Text := LData.AsJSON(true);
  finally
    FreeAndNil(LAPI);
  end;
end;

procedure TfrmXERODemo.SearchInvoices;
var
  LAPI: TXEROInvoices;
  LData: TXEROInvoiceResponse;
begin
  LAPI := TXEROInvoices.Create(nil);
  try
    LAPI.XEROAppDetails := FXEROAppDetails;
    LData := LAPI.Search(GetPage, GetOrderBy, '');
    memoJSON.Text := LData.AsJSON(true);
  finally
    FreeAndNil(LAPI);
  end;
end;

end.
