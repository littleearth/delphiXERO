unit XEROComponentRegister;

interface

uses Classes,
  Windows, SysUtils;

procedure Register;

implementation

uses
  XERO.API, XERO.API.Response.Text;

procedure Register;
begin
  RegisterComponents('XERO', [TXEROAppDetails, TXEROAPI, TXEROResponseText]);
end;

end.
