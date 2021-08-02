unit XERO.TrackingCategory.Dataset;

interface

uses
  System.SysUtils, DB,
  Classes, XERO.API, XERO.Model.Dataset, XERO.TrackingCategory.Model,
  XERO.TrackingCategory;

type
  TXEROTrackingCagegoryDataset = class(TXEROModelDataset<TXMTrackingCategory>)
  protected
    function GetIndexFields: string; override;
  end;

implementation

{ TXEROContactDataset }

function TXEROTrackingCagegoryDataset.GetIndexFields: string;
begin
  Result := 'TrackingCategoryID';
end;

end.
