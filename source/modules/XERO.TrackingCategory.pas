unit XERO.TrackingCategory;

interface

uses
  System.SysUtils, DB,
  Classes, XERO.API, XERO.Types, XERO.API.Response.JSON, XERO.Model,
  XERO.Response.Model, XERO.Request.Model,
  XERO.TrackingCategory.Model;

type
  EXEROContactException = EXEROException;

  TXEROTrackingCategoriesResponse = class(TXEROResponse)
  private
    [XEROModelManagedAttribute]
    FTrackingCategories: TXMTrackingCategories;
  public
    property TrackingCategories: TXMTrackingCategories read FTrackingCategories;
  end;

  TXEROTrackingCategoryResponse = class(TXEROResponse)
  private
    [XEROModelManagedAttribute]
    FTrackingCategory: TXMTrackingCategory;
  public
    property TrackingCategory: TXMTrackingCategory read FTrackingCategory;
  end;

  TXEROTrackingOptionsResponse = class(TXEROResponse)
  private
    [XEROModelManagedAttribute]
    FTrackingOptions: TXMTrackingOptions;
  public
    property Options: TXMTrackingOptions read FTrackingOptions;
  end;

  TXEROTrackingCategory = class(TXEROAPI)
  private
  protected
    function GetAPIURL: string; override;
  public
    function Search(AOrderBy: string = ''; ACategoryName: string = '';
      ATrackingCategoryID: string = ''): TXEROTrackingCategoriesResponse;
    function AddCategory(ACategoryName: string): TXEROTrackingCategoryResponse;
    function AddOption(ATrackingCategoryID: string; AOptionName: string)
      : TXEROTrackingOptionsResponse;
  end;

implementation

{ TXEROTrackingCategory }

function TXEROTrackingCategory.Search(AOrderBy: string = '';
  ACategoryName: string = ''; ATrackingCategoryID: string = '')
  : TXEROTrackingCategoriesResponse;
var
  LXEROFilter: TXEROFilter;
  LXEROResponseJSON: TXEROResponseJSON;
begin
  LXEROFilter := TXEROFilter.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    LXEROFilter.AddGUIDToFilter('TrackingCategoryID', ATrackingCategoryID);
    LXEROFilter.AddToFilter('Name', ACategoryName);
    Find<TXEROResponseJSON>(LXEROResponseJSON, LXEROFilter.Text, AOrderBy);
    Result := LXEROResponseJSON.ToResponse<TXEROTrackingCategoriesResponse>;
  finally
    FreeAndNil(LXEROFilter);
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROTrackingCategory.AddCategory(ACategoryName: string)
  : TXEROTrackingCategoryResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
  LCategory: TXMTrackingCategory;
begin
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  LCategory := TXMTrackingCategory.Create;
  try
    LCategory.CategoryName := ACategoryName;
    Put<TXEROResponseJSON>(LCategory.AsJSON, LXEROResponseJSON);
    Result := LXEROResponseJSON.ToResponse<TXEROTrackingCategoryResponse>;
  finally
    FreeAndNil(LCategory);
    FreeAndNil(LXEROResponseJSON);
  end;
end;

function TXEROTrackingCategory.AddOption(ATrackingCategoryID,
  AOptionName: string): TXEROTrackingOptionsResponse;
var
  LXEROResponseJSON: TXEROResponseJSON;
  LOption: TXMTrackingOption;
begin
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  LOption := TXMTrackingOption.Create;
  try
    LOption.OptionName := AOptionName;
    Put<TXEROResponseJSON>(LOption.AsJSON, LXEROResponseJSON, '',
      ATrackingCategoryID + '\Options');
    Result := LXEROResponseJSON.ToResponse<TXEROTrackingOptionsResponse>;
  finally
    FreeAndNil(LXEROResponseJSON);
    FreeAndNil(LOption);
  end;
end;

function TXEROTrackingCategory.GetAPIURL: string;
begin
  Result := XERO_API_BASE_URL + 'TrackingCategories';
end;

end.
