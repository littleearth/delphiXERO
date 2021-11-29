unit XERO.Invoices;

interface

uses
  System.SysUtils, DB,
  Classes, XERO.API, XERO.API.Response.JSON, XERO.Types, XERO.Model,
  XERO.Response.Model, XERO.Request.Model,
  XERO.Invoice.Model;

type
  EXEROInvoiceException = EXEROException;

  TXEROInvoiceResponse = class(TXEROResponse)
  private
    [XEROModelManagedAttribute]
    FInvoices: TXMInvoices;
  public
    property Invoices: TXMInvoices read FInvoices;
  end;

  TXEROInvoices = class(TXEROAPI)
  private
  protected
    function GetAPIURL: string; override;
  public
    function Search(APage: Integer = 0; AOrderBy: string = '';
      AInvoiceID: string = ''; AInvoiceNumber: string = '';
      ALastModified: TDateTime = 0; ASummaryOnly: boolean = false)
      : TXEROInvoiceResponse;
    function Insert(AInvoices: TXMInvoices): TXEROInvoiceResponse; overload;
    function Insert(AInvoice: TXMInvoice): TXEROInvoiceResponse; overload;
    function Update(AInvoices: TXMInvoices): TXEROInvoiceResponse; overload;
    function Update(AInvoice: TXMInvoice): TXEROInvoiceResponse; overload;
    property summarizeErrors;
  end;

implementation

{ TXEROInvoices }

function TXEROInvoices.Search(APage: Integer;
  AOrderBy, AInvoiceID, AInvoiceNumber: string; ALastModified: TDateTime;
  ASummaryOnly: boolean): TXEROInvoiceResponse;
var
  LXEROFilter: TXEROFilter;
  LXEROResponseJSON: TXEROResponseJSON;
  LParams: string;
begin
  LXEROFilter := TXEROFilter.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    LParams := '';
    if ASummaryOnly then
      LParams := 'summaryOnly=True';
    LXEROFilter.AddGUIDToFilter('InvoiceID', AInvoiceID);
    LXEROFilter.AddToFilter('InvoiceNumber', AInvoiceNumber);
    Find<TXEROResponseJSON>(LXEROResponseJSON, LXEROFilter.Text, AOrderBy,
      APage, ALastModified, LParams);
    Result := LXEROResponseJSON.ToResponse<TXEROInvoiceResponse>;
  finally
    FreeAndNil(LXEROFilter);
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROInvoices.Update(AInvoices: TXMInvoices): TXEROInvoiceResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
begin
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    Post<TXEROResponseJSON>(AInvoices.AsJSONArray('Invoices'),
      LXEROResponseJSON);
    Result := LXEROResponseJSON.ToResponse<TXEROInvoiceResponse>;
  finally
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROInvoices.Update(AInvoice: TXMInvoice): TXEROInvoiceResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
begin
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    Post<TXEROResponseJSON>(AInvoice.AsJSON, LXEROResponseJSON);
    Result := LXEROResponseJSON.ToResponse<TXEROInvoiceResponse>;
  finally
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROInvoices.GetAPIURL: string;
begin
  Result := XERO_API_BASE_URL + 'Invoices';
end;

function TXEROInvoices.Insert(AInvoices: TXMInvoices): TXEROInvoiceResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
begin
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    Put<TXEROResponseJSON>(AInvoices.AsJSONArray('Invoices'),
      LXEROResponseJSON);
    Result := LXEROResponseJSON.ToResponse<TXEROInvoiceResponse>;
  finally
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROInvoices.Insert(AInvoice: TXMInvoice): TXEROInvoiceResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
begin
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    Put<TXEROResponseJSON>(AInvoice.AsJSON, LXEROResponseJSON);
    Result := LXEROResponseJSON.ToResponse<TXEROInvoiceResponse>;
  finally
    FreeAndNil(LXEROResponseJSON);
  end;
end;

end.
