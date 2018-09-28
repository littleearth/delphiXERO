unit XERO.Contacts.Dataset;

interface

uses
  System.SysUtils, DB,
  Classes, XERO.API, XERO.Model.Dataset, XERO.Contact.Model, XERO.Contacts;

type
  TXEROContactDataset = class(TXEROModelDataset<TXMContact>)
  protected
    function GetIndexFields: string; override;
  end;

implementation

{ TXEROContactDataset }

function TXEROContactDataset.GetIndexFields: string;
begin
  Result := 'ContactID';
end;

end.
