unit XERO.TrackingCategory.Model;

interface

uses
  Classes, SysUtils, XERO.Model;

type
  TXMTrackingOption = class(TXeroModel)
  private
    FOptionName: string;
    FTrackingOptionID: string;
    FStatus: string;
    procedure SetOptionName(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetTrackingOptionID(const Value: string);
  public
    property TrackingOptionID: string read FTrackingOptionID
      write SetTrackingOptionID;
    [XEROModelJSONPropertyNameAttribute('Name')]
    property OptionName: string read FOptionName write SetOptionName;
    property Status: string read FStatus write SetStatus;
  end;

  TXMTrackingOptions = TXEROModelList<TXMTrackingOption>;

  TXMTrackingCategory = class(TXeroModel)
  private
    [XEROModelManagedAttribute]
    FXMTrackingOptions: TXMTrackingOptions;
    FStatus: string;
    FCategoryName: string;
    FTrackingCategoryID: string;
    procedure SetCategoryName(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetTrackingCategoryID(const Value: string);
  public
    property TrackingCategoryID: string read FTrackingCategoryID
      write SetTrackingCategoryID;
    [XEROModelJSONPropertyNameAttribute('Name')]
    property CategoryName: string read FCategoryName write SetCategoryName;
    property Status: string read FStatus write SetStatus;
    property Options: TXMTrackingOptions read FXMTrackingOptions;
  end;

  TXMTrackingCategories = TXEROModelList<TXMTrackingCategory>;

  TXMLineItemTrackingCategory = class(TXeroModel)
  private
    FOptionName: string;
    FCategoryName: string;
    FTrackingCategoryID: string;
    procedure SetCategoryName(const Value: string);
    procedure SetOptionName(const Value: string);
    procedure SetTrackingCategoryID(const Value: string);
  public
    property TrackingCategoryID: string read FTrackingCategoryID
      write SetTrackingCategoryID;
    [XEROModelJSONPropertyNameAttribute('Name')]
    property CategoryName: string read FCategoryName write SetCategoryName;
    [XEROModelJSONPropertyNameAttribute('Option')]
    property OptionName: string read FOptionName write SetOptionName;
  end;

  TXMLineItemTrackingCategories = TXEROModelList<TXMLineItemTrackingCategory>;

implementation

{ TXMTrackingCategory }

procedure TXMTrackingCategory.SetCategoryName(

  const Value: string);
begin
  FCategoryName := Value;
end;

procedure TXMTrackingCategory.SetStatus(

  const Value: string);
begin
  FStatus := Value;
end;

procedure TXMTrackingCategory.SetTrackingCategoryID(

  const Value: string);
begin
  FTrackingCategoryID := Value;
end;

{ TXMTrackingOption }

procedure TXMTrackingOption.SetOptionName(

  const Value: string);
begin
  FOptionName := Value;
end;

procedure TXMTrackingOption.SetStatus(

  const Value: string);
begin
  FStatus := Value;
end;

procedure TXMTrackingOption.SetTrackingOptionID(

  const Value: string);
begin
  FTrackingOptionID := Value;
end;

{ TXMLineItemTrackingCategory }

procedure TXMLineItemTrackingCategory.SetCategoryName(const Value: string);
begin
  FCategoryName := Value;
end;

procedure TXMLineItemTrackingCategory.SetOptionName(const Value: string);
begin
  FOptionName := Value;
end;

procedure TXMLineItemTrackingCategory.SetTrackingCategoryID
  (const Value: string);
begin
  FTrackingCategoryID := Value;
end;

end.
