unit XEROComponentRegister;

interface

uses Classes,
  Windows, SysUtils;

procedure Register;

implementation

uses
  XERO.API, XERO.API.Response.Text,  XERO.Authenticator.PKCE;

procedure Register;
begin
  RegisterComponents('XERO Base', [TXEROAppDetails, TXEROAuthenticatorPKCE,
    TXEROAPI, TXEROResponseText]);
end;

end.
