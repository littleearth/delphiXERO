unit frmXEROTestU;

interface

uses
  MidasLib,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, XERO.API,
  XERO.API.Response.ClientDataset, XERO.API.Invoices, Data.DB, Vcl.Grids,
  Vcl.DBGrids, Vcl.Samples.Spin, Vcl.ComCtrls, Vcl.Imaging.pngimage;

type
  TfrmXERO = class(TForm)
    memoLog: TMemo;
    XEROAppDetails: TXEROAppDetails;
    XEROInvoices: TXEROInvoices;
    XEROInvoiceResponse: TXEROInvoiceResponse;
    dsInvoices: TDataSource;
    dsInvoiceItems: TDataSource;
    PageControlMain: TPageControl;
    tabSearch: TTabSheet;
    tabOptions: TTabSheet;
    Panel1: TPanel;
    btnSearch: TButton;
    PageControlOptions: TPageControl;
    tabConsumerDetails: TTabSheet;
    tabPublicKey: TTabSheet;
    tabPrivateKey: TTabSheet;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    editConsumerKey: TEdit;
    Label4: TLabel;
    editConsumerSecret: TEdit;
    memoPrivateKey: TMemo;
    memoPublicKey: TMemo;
    PageControlSearch: TPageControl;
    tabSearchGetDateRange: TTabSheet;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    editStartDate: TDateTimePicker;
    editEndDate: TDateTimePicker;
    comboInvoiceType: TComboBox;
    tabSearchLowLevel: TTabSheet;
    GroupBox3: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    comboLowLevelSearch: TComboBox;
    comboLowLevelOrderBy: TComboBox;
    editLowLevelPage: TSpinEdit;
    Label8: TLabel;
    Label9: TLabel;
    editLowLevelLastModifiedTime: TDateTimePicker;
    tabAbout: TTabSheet;
    PageControlData: TPageControl;
    tabDataView: TTabSheet;
    DBGridInvoices: TDBGrid;
    DBGridInvoiceItems: TDBGrid;
    tabXML: TTabSheet;
    memoXML: TMemo;
    GroupBox4: TGroupBox;
    Image1: TImage;
    memoAbout: TMemo;
    Splitter1: TSplitter;
    lblFileName: TLabel;
    lblInfo: TLabel;
    editLowLevelLastModifiedDate: TDateTimePicker;
    procedure btnSearchClick(Sender: TObject);
    procedure OnLog(Sender: TObject; AMessage: string);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblFileNameClick(Sender: TObject);
  private
    FSettingsFileName: TFileName;
    procedure ResizeDBGrid(ADBGrid: TDBGrid);
    procedure LoadSettings;
    procedure SaveSettings;
  public
    { Public declarations }
  end;

var
  frmXERO: TfrmXERO;

implementation

{$R *.dfm}

uses
  System.DateUtils, System.IniFiles, Winapi.ShlObj, Winapi.ShellAPI;

function GetShellFolderPath(AFolder: integer): string;
const
  SHGFP_TYPE_CURRENT = 0;
var
  path: array [0 .. MAX_PATH] of char;
begin
  if SUCCEEDED(SHGetFolderPath(0, AFolder, 0, SHGFP_TYPE_CURRENT, @path[0]))
  then
    Result := path
  else
    Result := '';
end;

function CheckDirectoryExists(ADirectory: string; ACreate: boolean): boolean;
begin
  try
    if ACreate then
    begin
      if not DirectoryExists(ADirectory) then
      begin
        ForceDirectories(ADirectory);
      end;
    end;
  finally
    Result := DirectoryExists(ADirectory);
  end;
end;

function ExecuteFile(const Operation, FileName, Params, DefaultDir: string;
  ShowCmd: word): integer;
var
  zFileName, zParams, zDir: array [0 .. 255] of char;
begin
  Result := ShellExecute(Application.Handle, PChar(Operation),
    StrPCopy(zFileName, FileName), StrPCopy(zParams, Params),
    StrPCopy(zDir, DefaultDir), ShowCmd);
end;

function GetUserAppDataDir: string;
begin
  Result := IncludeTrailingPathDelimiter
    (IncludeTrailingPathDelimiter(GetShellFolderPath(CSIDL_APPDATA)) +
    Application.Title);
  CheckDirectoryExists(Result, True);
end;

procedure TfrmXERO.FormCreate(Sender: TObject);
begin
  PageControlMain.ActivePageIndex := 0;
  PageControlOptions.ActivePageIndex := 0;
  PageControlSearch.ActivePageIndex := 0;
  PageControlData.ActivePageIndex := 0;
  editLowLevelLastModifiedTime.Time := 0;
  editLowLevelLastModifiedDate.Date := 0;
  editStartDate.Date := StartOfTheMonth(Today);
  editEndDate.Date := Today;
  FSettingsFileName := GetUserAppDataDir + 'settings.ini';
  LoadSettings;
end;

procedure TfrmXERO.FormDestroy(Sender: TObject);
begin
  SaveSettings;
end;

procedure TfrmXERO.lblFileNameClick(Sender: TObject);
begin
  ExecuteFile('open', lblFileName.Hint, '', ExtractFilePath(lblFileName.Hint),
    SW_NORMAL);
end;

procedure TfrmXERO.LoadSettings;
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

procedure TfrmXERO.SaveSettings;
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

procedure TfrmXERO.OnLog(Sender: TObject; AMessage: string);
begin
  memoLog.Lines.Insert(0, AMessage);
  Application.ProcessMessages;
end;

procedure TfrmXERO.ResizeDBGrid(ADBGrid: TDBGrid);
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
                ftString, ftMemo, ftFmtMemo, ftWideString:
                  begin
                    Columns.Items[ColumnIdx].Width := 100;
                  end;
                ftBoolean, ftSmallint, ftBytes:
                  begin
                    Columns.Items[ColumnIdx].Width := 25;
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

procedure TfrmXERO.btnSearchClick(Sender: TObject);
var
  LastModified: TDateTime;
begin
  dsInvoices.DataSet := nil;
  dsInvoiceItems.DataSet := nil;

  XEROAppDetails.Privatekey.Text := memoPrivateKey.Lines.Text;
  XEROAppDetails.PublicKey.Text := memoPublicKey.Lines.Text;
  XEROAppDetails.ConsumerKey := editConsumerKey.Text;
  XEROAppDetails.ConsumerSecret := editConsumerSecret.Text;
  XEROInvoices.XEROAppDetails := XEROAppDetails;
  XEROInvoices.OnLog := OnLog;
  XEROInvoices.LogLevel := logDebug;

  if PageControlSearch.ActivePage = tabSearchGetDateRange then
  begin
    XEROInvoices.GetDateRange(editStartDate.Date, editEndDate.Date, 1,
      TXEROInvoiceType(comboInvoiceType.ItemIndex));
  end;

  if PageControlSearch.ActivePage = tabSearchLowLevel then
  begin
    if editLowLevelLastModifiedDate.Date <> 0 then
    begin
      LastModified := DateOf(editLowLevelLastModifiedDate.Date) +
        TimeOf(editLowLevelLastModifiedTime.Time);
    end
    else
    begin
      LastModified := 0;
    end;
    XEROInvoices.Find(comboLowLevelSearch.Text, comboLowLevelOrderBy.Text,
      editLowLevelPage.Value, LastModified);
  end;

  if XEROInvoices.Response.Result then
  begin
    memoXML.Lines.Text := XEROInvoiceResponse.AsString;
    dsInvoices.DataSet := XEROInvoiceResponse.MasterDataset;
    dsInvoiceItems.DataSet := XEROInvoiceResponse.DetailDataset;
    ResizeDBGrid(DBGridInvoices);
    ResizeDBGrid(DBGridInvoiceItems);
  end
  else
  begin
    MessageDlg(XEROInvoices.Response.ErrorMessage, mtError, [mbOK], 0);
  end;
end;

end.
