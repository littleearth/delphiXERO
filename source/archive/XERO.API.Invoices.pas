unit XERO.API.Invoices;

interface

uses
  System.SysUtils, DB,
  Classes, XERO.API, XERO.API.Response.ClientDataset;

// Invoice Status
// ---
// DRAFT	A Draft Invoice (default)
// SUBMITTED	An Awaiting Approval Invoice
// DELETED	A Deleted Invoice
// AUTHORISED	An Invoice that is Approved and Awaiting Payment OR partially paid
// PAID	An Invoice that is completely Paid
// VOIDED	A Voided Invoice
type
  TXEROInvoiceStatus = (isUnspecified, isDraft, isSubmitted, isDeleted,
    isAuthorised, isPaid, isVoided);

  // Invoice Type
  // ---
  // ACCPAY	A bill – commonly known as a Accounts Payable or supplier invoice
  // ACCREC	A sales invoice – commonly known as an Accounts Receivable or customer invoice
type
  TXEROInvoiceType = (itUnspecified, itAccPay, itAccRec);

type
  TXEROInvoiceResponse = class(TXEROResponseDatasetBase)
  protected
    procedure AddCustomMasterDatasetFields; override;
    procedure AddCustomDetailDatasetFields; override;
    procedure GetFieldInformation(AFieldName: string;
      var AFieldType: TFieldType; var ASize: Integer); override;
    procedure GetDefaultValues; override;
  published
    property MasterDataset;
    property DetailDataset;
  end;

type
  TXEROInvoices = class(TXEROAPI)
  private
  protected
    function GetAPIURL: string; override;
    function GetInvoiceStatus(AInvoiceStatus: TXEROInvoiceStatus): string;
    function GetInvoiceType(AInvoiceType: TXEROInvoiceType): string;
  public
    function Find(APage: Integer = 0; AOrderBy: string = '';
      AInvoiceType: TXEROInvoiceType = itUnspecified;
      AInvoiceStatus: TXEROInvoiceStatus = isUnspecified;
      AInvoiceID: string = ''; AInvoiceNumber: string = '';
      AReference: string = ''; ADate: TDateTime = 0; ADueDate: TDateTime = 0;
      ALastModified: TDateTime = 0): Boolean; overload;
    function GetDateRange(AStartDate: TDateTime; AEndDate: TDateTime;
      APage: Integer = 0;
      AInvoiceType: TXEROInvoiceType = itUnspecified): Boolean;
  published
  end;

implementation

uses
  XERO.Utils;

// TXEROInvoiceResponse

procedure TXEROInvoiceResponse.AddCustomMasterDatasetFields;
begin

end;

procedure TXEROInvoiceResponse.AddCustomDetailDatasetFields;
begin
end;

procedure TXEROInvoiceResponse.GetFieldInformation(AFieldName: string;
  var AFieldType: TFieldType; var ASize: Integer);
begin
  inherited GetFieldInformation(AFieldName, AFieldType, ASize);
  if (UpperCase(AFieldName) = 'DATE') or (UpperCase(AFieldName) = 'DUEDATE') or
    (UpperCase(AFieldName) = 'FULLYPAIDONDATE') then
  begin
    AFieldType := ftDate;
    ASize := 0;
  end;

  if (UpperCase(AFieldName) = 'UPDATEDDATEUTC') then
  begin
    AFieldType := ftDateTime;
    ASize := 0;
  end;

  if (UpperCase(AFieldName) = 'SUBTOTAL') or
    (UpperCase(AFieldName) = 'TOTALTAX') or (UpperCase(AFieldName) = 'TOTAL') or
    (UpperCase(AFieldName) = 'AMOUNTDUE') or
    (UpperCase(AFieldName) = 'AMOUNTPAID') or
    (UpperCase(AFieldName) = 'AMOUNTCREDITED') or
    (UpperCase(AFieldName) = 'UNITAMOUNT') or
    (UpperCase(AFieldName) = 'TAXAMOUNT')  then
  begin
    AFieldType := ftCurrency;
    ASize := 0;
  end;
end;

procedure TXEROInvoiceResponse.GetDefaultValues;
begin
  MasterListNodeName := 'Invoices';
  MasterNodeName := 'Invoice';
  DetailListNodeName := 'LineItems';
  DetailNodeName := 'LineItem';
  MasterFields := 'InvoiceID';
end;


// TXEROInvoices

function TXEROInvoices.GetAPIURL: string;
begin
  Result := XERO_API_BASE_URL + 'Invoices';
end;

function TXEROInvoices.GetInvoiceStatus(AInvoiceStatus
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

function TXEROInvoices.GetInvoiceType(AInvoiceType: TXEROInvoiceType): string;
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

function TXEROInvoices.Find(APage: Integer = 0; AOrderBy: string = '';
  AInvoiceType: TXEROInvoiceType = itUnspecified;
  AInvoiceStatus: TXEROInvoiceStatus = isUnspecified; AInvoiceID: string = '';
  AInvoiceNumber: string = ''; AReference: string = ''; ADate: TDateTime = 0;
  ADueDate: TDateTime = 0; ALastModified: TDateTime = 0): Boolean;
var
  Filter: string;
  InvoiceStatus, InvoiceType, InvoiceDueDate, InvoiceDate: string;

  procedure AddToFilter(AFieldName: string; AData: string;
    AQuoteData: Boolean = true);
  var
    Data: string;
  begin
    if AQuoteData then
    begin
      Data := '"' + AData + '"';
    end
    else
    begin
      Data := AData;
    end;
    if not IsEmptyString(AData) then
    begin
      if not IsEmptyString(Filter) then
      begin
        Filter := Filter + ' AND ';
      end;
      Filter := Filter + AFieldName + '==' + Data;
    end;
  end;

begin
  Filter := '';
  InvoiceStatus := GetInvoiceStatus(AInvoiceStatus);
  InvoiceType := GetInvoiceType(AInvoiceType);
  InvoiceDueDate := GetDateTimeFilterString(ADueDate);
  InvoiceDate := GetDateTimeFilterString(ADate);
  AddToFilter('Status', InvoiceStatus);
  AddToFilter('Type', InvoiceType);
  AddToFilter('InvoiceID', AInvoiceID);
  AddToFilter('InvoiceNumber', AInvoiceNumber);
  AddToFilter('Reference', AReference);
  AddToFilter('DueDate', InvoiceDueDate, False);
  AddToFilter('Date', InvoiceDate, False);
  Debug('Find', Format('Filter: %s', [Filter]));
  Result := Find(Filter, AOrderBy, APage, ALastModified);
end;

function TXEROInvoices.GetDateRange(AStartDate: TDateTime; AEndDate: TDateTime;
  APage: Integer = 0; AInvoiceType: TXEROInvoiceType = itUnspecified): Boolean;
var
  Filter: string;
  InvoiceType: string;
  StartDate, EndDate: string;

begin
  Filter := '';
  InvoiceType := GetInvoiceType(AInvoiceType);
  StartDate := GetDateTimeFilterString(AStartDate);
  EndDate := GetDateTimeFilterString(AEndDate);
  if not IsEmptyString(InvoiceType) then
  begin
    Filter := Filter + 'Type=="' + InvoiceType + '"';
  end;

  if not IsEmptyString(Filter) then
  begin
    Filter := Filter + ' AND ';
  end;

  Filter := Filter + 'Date>=' + StartDate + ' AND Date <= ' + EndDate;
  Debug('GetDateRange', Format('Filter: %s', [Filter]));
  Result := Find(Filter, 'Date ASC', APage);

end;

end.
