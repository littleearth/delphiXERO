unit XERO.PurchaseOrder.Model;

interface

uses
  Classes, SysUtils, XERO.Model;

type
  TXMPurchaseOrder = class(TXeroModel)
  private
  public
  end;

type
  TXMPurchaseOrders = TXEROModelList<TXMPurchaseOrder>;

implementation

end.
