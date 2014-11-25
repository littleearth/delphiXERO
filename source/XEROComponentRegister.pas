unit XEROComponentRegister;

interface

uses Classes,
  Windows, SysUtils;

procedure Register;

implementation

uses
  XERO.API, XERO.API.Response.Text, XERO.API.Response.ClientDataset,
  XERO.API.Invoices;

procedure Register;
begin
  RegisterComponents('XERO Base', [TXEROAppDetails, TXEROAPI, TXEROResponseText,
    TXEROResponseDataset]);
  RegisterComponents('XERO API', [TXEROInvoices, TXEROInvoiceResponse]);
end;

end.
