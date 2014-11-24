unit XERO.API.Response.ClientDataset;

interface

uses
  XERO.API, DB, DBClient;

type
  TXEROResponseDataset = class(TXEROResponseBase)
  private
    FClientDataSet: TClientDataSet;
  public
    property Dataset: TClientDataSet read FClientDataSet;
  end;

implementation

end.
