unit XERO.API.Response.JSON;

interface

uses
  XERO.API, XERO.Types;

type
  TXEROResponseJSON = class(TXEROResponseBase)
  protected
    function GetDefaultResponseType: TResponseType; override;
  end;

implementation

{ TXEROResponseJSON }

{ TXEROResponseJSON }

function TXEROResponseJSON.GetDefaultResponseType: TResponseType;
begin
  Result := rtJSON;
end;

end.
