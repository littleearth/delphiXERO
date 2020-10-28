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
      ALastModified: TDateTime = 0): TXEROInvoiceResponse;
    function Insert(AInvoices: TXMInvoices): TXEROInvoiceResponse; overload;
    function Insert(AInvoice: TXMInvoice): TXEROInvoiceResponse; overload;
    function Update(AInvoices: TXMInvoices): TXEROInvoiceResponse; overload;
    function Update(AInvoice: TXMInvoice): TXEROInvoiceResponse; overload;
  end;

implementation

{ TXEROInvoices }

function TXEROInvoices.Search(APage: Integer;
  AOrderBy, AInvoiceID, AInvoiceNumber: string; ALastModified: TDateTime)
  : TXEROInvoiceResponse;
var
  LXEROFilter: TXEROFilter;
  LXEROResponseJSON: TXEROResponseJSON;
begin
  Result := TXEROInvoiceResponse.Create;
  LXEROFilter := TXEROFilter.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    LXEROFilter.AddGUIDToFilter('InvoiceID', AInvoiceID);
    LXEROFilter.AddToFilter('InvoiceNumber', AInvoiceNumber);
    if Find<TXEROResponseJSON>(LXEROResponseJSON, LXEROFilter.Text, AOrderBy,
      APage, ALastModified) then
    begin
      if LXEROResponseJSON.Result then
      begin
        Result.FromJSON(LXEROResponseJSON.AsString);
      end
      else
      begin
        raise EXEROInvoiceException.Create(LXEROResponseJSON.ErrorMessage);
      end;
    end;
  finally
    FreeAndNil(LXEROFilter);
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROInvoices.Update(AInvoices: TXMInvoices): TXEROInvoiceResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
begin
  Result := TXEROInvoiceResponse.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    if Post<TXEROResponseJSON>(AInvoices.AsJSONArray('Invoices'),
      LXEROResponseJSON) then
    begin
      if LXEROResponseJSON.Result then
      begin
        Result.FromJSON(LXEROResponseJSON.AsString);
      end
      else
      begin
        raise EXEROInvoiceException.Create(LXEROResponseJSON.ErrorMessage);
      end;
    end;
  finally
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROInvoices.Update(AInvoice: TXMInvoice): TXEROInvoiceResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
begin
  Result := TXEROInvoiceResponse.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    if Post<TXEROResponseJSON>(AInvoice.AsJSON, LXEROResponseJSON) then
    begin
      if LXEROResponseJSON.Result then
      begin
        Result.FromJSON(LXEROResponseJSON.AsString);
      end
      else
      begin
        raise EXEROInvoiceException.Create(LXEROResponseJSON.ErrorMessage);
      end;
    end;
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
  Result := TXEROInvoiceResponse.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    if Put<TXEROResponseJSON>(AInvoices.AsJSONArray('Invoices'),
      LXEROResponseJSON) then
    begin
      if LXEROResponseJSON.Result then
      begin
        Result.FromJSON(LXEROResponseJSON.AsString);
      end
      else
      begin
        raise EXEROInvoiceException.Create(LXEROResponseJSON.ErrorMessage);
      end;
    end;
  finally
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROInvoices.Insert(AInvoice: TXMInvoice): TXEROInvoiceResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
begin
  Result := TXEROInvoiceResponse.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    if Put<TXEROResponseJSON>(AInvoice.AsJSON, LXEROResponseJSON) then
    begin
      if LXEROResponseJSON.Result then
      begin
        Result.FromJSON(LXEROResponseJSON.AsString);
      end
      else
      begin
        raise EXEROInvoiceException.Create(LXEROResponseJSON.ErrorMessage);
      end;
    end;
  finally
    FreeAndNil(LXEROResponseJSON);
  end;
end;

end.
