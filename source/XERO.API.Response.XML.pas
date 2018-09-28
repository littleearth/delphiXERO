unit XERO.API.Response.XML;

interface

uses
  XERO.API, XERO.Types;

type
  TXEROResponseXML = class(TXEROResponseBase)
  protected
    function GetDefaultResponseType: TResponseType; override;
  end;

implementation

{ TXEROResponseXML }

{ TXEROResponseXML }

function TXEROResponseXML.GetDefaultResponseType: TResponseType;
begin
  Result := rtXML;
end;

end.
