unit XERO.Invoices;

interface

uses
  System.SysUtils, DB,
  Classes, XERO.API, XERO.API.Response.JSON, XERO.Types, XERO.Model,
  XERO.Response.Model,
  XERO.Invoice.Model;

type
  EXEROInvoiceException = EXEROException;

  TXEROInvoiceStatus = (isUnspecified, isDraft, isSubmitted, isDeleted,
    isAuthorised, isPaid, isVoided);

  TXEROInvoiceType = (itUnspecified, itAccPay, itAccRec);

  TXEROInvoiceResponse = class(TXEROResponse)
  private
    [XEROModelManagedAttribute]
    FInvoices: TXMInvoices;
  public
    property Invoices: TXMInvoices read FInvoices;
  end;

  TXEROInvoices = class(TXEROAPI)
  private
    FXEROResponseJSON: TXEROResponseJSON;
  protected
    function GetAPIURL: string; override;
    property XEROResponseJSON: TXEROResponseJSON read FXEROResponseJSON;
  public
    procedure AfterConstruction; override;
    function Search(APage: Integer = 0; AOrderBy: string = '';
      AInvoiceID: string = ''; AInvoiceNumber: string = '';
      ALastModified: TDateTime = 0): TXEROInvoiceResponse;
    function GetInvoiceStatus(AInvoiceStatus: TXEROInvoiceStatus): string;
    function GetInvoiceType(AInvoiceType: TXEROInvoiceType): string;
  end;

implementation

{ TXEROInvoices }

function TXEROInvoices.Search(APage: Integer;
  AOrderBy, AInvoiceID, AInvoiceNumber: string; ALastModified: TDateTime)
  : TXEROInvoiceResponse;
begin
  Result := TXEROInvoiceResponse.Create;
  ResetFilter;
  AddGUIDToFilter('InvoiceID', AInvoiceID);
  AddToFilter('InvoiceNumber', AInvoiceNumber);
  if Find(Filter, AOrderBy, APage, ALastModified) then
  begin
    if XEROResponseJSON.Result then
    begin
      Result.FromJSON(XEROResponseJSON.AsString);
    end
    else
    begin
      raise EXEROInvoiceException.Create(XEROResponseJSON.ErrorMessage);
    end;
  end;
end;

procedure TXEROInvoices.AfterConstruction;
begin
  inherited;
  FXEROResponseJSON := TXEROResponseJSON.Create(nil);
  Response := FXEROResponseJSON;
end;

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

end.
