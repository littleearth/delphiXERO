unit frmXERODemoU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.UITypes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls, XERO.API, XERO.Model.Dataset,
  XERO.Authenticator.PKCE, Data.DB, Vcl.Grids, Vcl.DBGrids;

type
  TfrmXERODemo = class(TForm)
    DataSourceDataset: TDataSource;
    PageControlMenu: TPageControl;
    tabAPISearch: TTabSheet;
    tabDataset: TTabSheet;
    PageControlSearchData: TPageControl;
    tabDataJSON: TTabSheet;
    memoJSON: TMemo;
    tabLog: TTabSheet;
    Panel13: TPanel;
    btnRefreshLog: TButton;
    memoLog: TMemo;
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
    pnlAPISearch: TPanel;
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
    tabAPIStore: TTabSheet;
    Panel21: TPanel;
    Panel23: TPanel;
    btnStoreExecute: TButton;
    Panel24: TPanel;
    Label11: TLabel;
    Panel25: TPanel;
    comboStoreModel: TComboBox;
    RadioGroupStoreMethod: TRadioGroup;
    memoStoreJSON: TMemo;
    Panel26: TPanel;
    Label13: TLabel;
    memoStoreResponse: TMemo;
    Panel27: TPanel;
    Label14: TLabel;
    editStoreGUID: TEdit;
    Panel28: TPanel;
    btnAuthenticate: TButton;
    Panel29: TPanel;
    Label15: TLabel;
    comboTenants: TComboBox;
    Panel30: TPanel;
    Label3: TLabel;
    editClientID: TEdit;
    Panel31: TPanel;
    Label4: TLabel;
    editScope: TEdit;
    procedure btnAPIJSONCopyClick(Sender: TObject);
    procedure btnAPIJsonToDatasetClick(Sender: TObject);
    procedure btnAuthenticateClick(Sender: TObject);
    procedure btnJSONClearClick(Sender: TObject);
    procedure btnJSONPasteClick(Sender: TObject);
    procedure btnJSONtoDatasetClick(Sender: TObject);
    procedure btnRefreshLogClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnStoreExecuteClick(Sender: TObject);
    procedure comboStoreModelChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControlMenuChange(Sender: TObject);
  private
    FSettingsFileName: TFileName;
    FXEROAppDetails: TXEROAppDetails;
    FXEROAuthenticatorPKCE: TXEROAuthenticatorPKCE;
    FDatasetList: TXEROModelDatasets;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure ApplySettings;
    procedure UpdateTenants;
    procedure SearchContacts;
    procedure SearchItems;
    procedure SearchAccounts;
    procedure SearchInvoices;
    procedure SearchCustom;
    function GetTenant: string;
    function GetPage: integer;
    function GetOrderBy: string;
    procedure DestroyDataset;
    procedure CreateDataset;
    procedure ResizeDBGrid(ADBGrid: TDBGrid);
    function GetStoreModel(AModel: string): string;
    procedure Post(AURL: string; AJSON: string);
    procedure Put(AURL, AJSON: string);
    procedure OnXEROAuthenticationURL(ASender: TObject; AURL: string);
    procedure OnXEROAuthenticationComplete(ASender: TObject; ASuccess: Boolean);
  public
    { Public declarations }
  end;

var
  frmXERODemo: TfrmXERODemo;

implementation

{$R *.dfm}

uses
  System.IniFiles, System.DateUtils, XERO.Utils,
  XERO.Log, XERO.Log.Basic,
  XERO.Response.Model, XERO.Request.Model,
  XERO.Contacts, XERO.API.JSON,
  XERO.Items,
  XERO.Accounts,
  XERO.Invoices;

procedure TfrmXERODemo.OnXEROAuthenticationURL(ASender: TObject; AURL: string);
begin
  OpenDefaultBrowser(AURL);
end;

procedure TfrmXERODemo.OnXEROAuthenticationComplete(ASender: TObject;
  ASuccess: Boolean);
begin
  if ASuccess then
  begin
    MessageDlg('Success', mtInformation, [mbOk], 0);
    UpdateTenants;
  end
  else
  begin
    MessageDlg('Failed', mtError, [mbOk], 0);
  end;
end;

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

function TfrmXERODemo.GetStoreModel(AModel: string): string;
begin
  Result := '';

end;

function TfrmXERODemo.GetTenant: string;
begin
  Result := '';
  if comboTenants.ItemIndex <> -1 then
    Result := comboTenants.Items[comboTenants.ItemIndex];
end;

procedure TfrmXERODemo.ApplySettings;
begin
  FXEROAppDetails.ClientID := editClientID.Text;
  FXEROAuthenticatorPKCE.Scope := editScope.Text;
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

procedure TfrmXERODemo.btnAuthenticateClick(Sender: TObject);
begin
  ApplySettings;
  if (not FXEROAuthenticatorPKCE.Authenticated) then
  begin
    if (not FXEROAuthenticatorPKCE.Busy) then
    begin
      FXEROAuthenticatorPKCE.Login;
    end
    else
    begin
      if MessageDlg('Authentication already in progress, cancel request?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        FXEROAuthenticatorPKCE.CancelAuthenticationRequest;
      end;
    end;
  end;
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
  if FXEROAuthenticatorPKCE.Authenticated then
  begin
    PageControlSearchData.ActivePageIndex := 0;
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
end;

procedure TfrmXERODemo.btnStoreExecuteClick(Sender: TObject);
var
  LURL: string;
begin
  ApplySettings;
  LURL := comboStoreModel.Text;
  case RadioGroupStoreMethod.ItemIndex of
    0:
      begin
        if not IsEmptyString(editStoreGUID.Text) then
          LURL := LURL + '/' + editStoreGUID.Text;
        Post(LURL, memoStoreJSON.Text);
      end;
  else
    begin
      Put(LURL, memoStoreJSON.Text);
    end;
  end;
end;

procedure TfrmXERODemo.comboStoreModelChange(Sender: TObject);
begin
  memoStoreJSON.Lines.Text := GetStoreModel(comboStoreModel.Text);
end;

procedure TfrmXERODemo.CreateDataset;
begin
  DestroyDataset;

  DataSourceDataset.Dataset := FDatasetList.AddJSON(memoDatasetJSON.Text);

  if Assigned(DataSourceDataset.Dataset) then
  begin
    ResizeDBGrid(DBGridDataset);
  end
  else
  begin
    MessageDlg('Failed to convert JSON', mtError, [mbOk], 0);
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
    LXEROAPI.TenantId := GetTenant;
    LLastModified := 0;
    if editSearchCustomLastModified.Checked then
      LLastModified := editSearchCustomLastModified.date;
    LXEROAPI.Get(editSearchCustomURL.Text, editSearchCustomParams.Text, LJSON,
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
  FXEROAppDetails.AppName := Application.Title;
  FXEROAuthenticatorPKCE := TXEROAuthenticatorPKCE.Create(Self);
  FXEROAuthenticatorPKCE.OnAuthenticationURL := OnXEROAuthenticationURL;
  FXEROAuthenticatorPKCE.OnAuthenticationComplete :=
    OnXEROAuthenticationComplete;
  FXEROAppDetails.XEROAuthenticator := FXEROAuthenticatorPKCE;
  PageControlMenu.ActivePageIndex := 0;
  PageControlSearch.ActivePageIndex := 0;
  PageControlSearchData.ActivePageIndex := 0;
  FSettingsFileName := GetUserAppDataDir + 'settings.ini';
  FDatasetList := TXEROModelDatasets.Create;
  comboStoreModelChange(Sender);
  LoadSettings;
end;

procedure TfrmXERODemo.LoadSettings;
var
  INIFile: TIniFile;
begin
  INIFile := TIniFile.Create(FSettingsFileName);
  try
    editClientID.Text := INIFile.ReadString('XERO', 'ClientID', '');
    editScope.Text := INIFile.ReadString('XERO', 'Scope',
      FXEROAuthenticatorPKCE.Scope);
  finally
    FreeAndNil(INIFile);
  end;
end;

procedure TfrmXERODemo.PageControlMenuChange(Sender: TObject);
begin
  if PageControlMenu.ActivePage = tabLog then
  begin
    btnRefreshLogClick(Sender);
  end;
end;

procedure TfrmXERODemo.Post(AURL, AJSON: string);
var
  LXEROAPI: TXEROApiJSON;
  LJSON: string;
begin
  LXEROAPI := TXEROApiJSON.Create(nil);
  try
    LXEROAPI.XEROAppDetails := FXEROAppDetails;
    LXEROAPI.TenantId := GetTenant;
    LXEROAPI.Post(AURL, AJSON, LJSON);
    memoStoreResponse.Text := LJSON;
  finally
    FreeAndNil(LXEROAPI);
  end;
end;

procedure TfrmXERODemo.Put(AURL, AJSON: string);
var
  LXEROAPI: TXEROApiJSON;
  LJSON: string;
begin
  LXEROAPI := TXEROApiJSON.Create(nil);
  try
    LXEROAPI.XEROAppDetails := FXEROAppDetails;
    LXEROAPI.TenantId := GetTenant;
    LXEROAPI.Put(AURL, AJSON, LJSON);
    memoStoreResponse.Text := LJSON;
  finally
    FreeAndNil(LXEROAPI);
  end;
end;

procedure TfrmXERODemo.SaveSettings;
var
  INIFile: TIniFile;
begin
  INIFile := TIniFile.Create(FSettingsFileName);
  try
    INIFile.WriteString('XERO', 'ClientID', editClientID.Text);
    INIFile.WriteString('XERO', 'Scope', editScope.Text);
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
    LAPI.TenantId := GetTenant;
    LData := LAPI.Search(GetPage, GetOrderBy, editContactsContactID.Text,
      editContactsContactNumber.Text, '', cbContactsIncludeArchived.Checked);
    memoJSON.Text := LData.AsJSON(True);
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
    LAPI.TenantId := GetTenant;
    LData := LAPI.Search(GetPage, GetOrderBy, '');
    memoJSON.Text := LData.AsJSON(True);
  finally
    FreeAndNil(LAPI);
  end;
end;

procedure TfrmXERODemo.UpdateTenants;
var
  LTenant: TXEROTenant;
begin
  if FXEROAuthenticatorPKCE.Authenticated then
  begin
    comboTenants.Items.Clear;
    comboTenants.Items.BeginUpdate;
    try
      for LTenant in FXEROAuthenticatorPKCE.Tenants do
      begin
        comboTenants.Items.Add(LTenant.TenantId);
      end;
    finally
      comboTenants.Items.EndUpdate;
      if comboTenants.Items.Count > 0 then
        comboTenants.ItemIndex := 0;
    end;
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
    LAPI.TenantId := GetTenant;
    LData := LAPI.Search(GetPage, GetOrderBy, '');
    memoJSON.Text := LData.AsJSON(True);
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
    LAPI.TenantId := GetTenant;
    LData := LAPI.Search(GetPage, GetOrderBy, editInvoicesInvoiceID.Text,
      editInvoicesInvoiceNumber.Text);
    memoJSON.Text := LData.AsJSON(True);
  finally
    FreeAndNil(LAPI);
  end;
end;

end.
