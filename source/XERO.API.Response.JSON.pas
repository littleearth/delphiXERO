unit XERO.API.Response.JSON;

interface

uses
  XERO.API, XERO.Types, XERO.Response.Model;

type
  TXEROResponseJSON = class(TXEROHTTPResponse)
  protected
    function GetDefaultResponseType: TResponseType; override;
  public
    function ToResponse<T: TXEROResponse>: T;
  end;

implementation

{ TXEROResponseJSON }

function TXEROResponseJSON.ToResponse<T>: T;
begin
  Result := T.CreateModel<T>;
  Result.FromJSON(Self.AsString);
end;

{ TXEROResponseJSON }

function TXEROResponseJSON.GetDefaultResponseType: TResponseType;
begin
  Result := rtJSON;
end;

end.
