unit XERO.Contacts;

interface

uses
  System.SysUtils, DB,
  Classes, XERO.API, XERO.Types, XERO.API.Response.JSON, XERO.Model,
  XERO.Response.Model, XERO.Request.Model,
  XERO.Contact.Model;

type
  EXEROContactException = EXEROException;

  TXEROContactResponse = class(TXEROResponse)
  private
    [XEROModelManagedAttribute]
    FContacts: TXMContacts;
  public
    property Contacts: TXMContacts read FContacts;
  end;

  TXEROContactRequest = class(TXERORequest)
  private
    [XEROModelManagedAttribute]
    FContacts: TXMContacts;
  public
    property Contacts: TXMContacts read FContacts;
  end;

  TXEROContacts = class(TXEROAPI)
  private
  protected
    function GetAPIURL: string; override;
  public
    function Search(APage: Integer = 0; AOrderBy: string = '';
      AContactID: string = ''; ContactNumber: string = ''; AIDList: string = '';
      AIncludeArchived: boolean = false; ALastModified: TDateTime = 0)
      : TXEROContactResponse;
  end;

implementation

{ TXEROContacts }

function TXEROContacts.Search(APage: Integer;
  AOrderBy, AContactID, ContactNumber, AIDList: string;
  AIncludeArchived: boolean; ALastModified: TDateTime): TXEROContactResponse;
var
  LXEROFilter: TXEROFilter;
  LXEROResponseJSON: TXEROResponseJSON;
begin
  Result := TXEROContactResponse.Create;
  LXEROFilter := TXEROFilter.Create;
  LXEROResponseJSON := TXEROResponseJSON.Create(nil);
  try
    LXEROFilter.AddGUIDToFilter('ContactID', AContactID);
    LXEROFilter.AddToFilter('ContactNumber', ContactNumber);
    LXEROFilter.AddToFilter('IDs', AIDList);
    if AIncludeArchived then
      LXEROFilter.AddToFilter('includeArchived', 'true');
    if Find<TXEROResponseJSON>(LXEROResponseJSON, LXEROFilter.Text, AOrderBy,
      APage, ALastModified) then
    begin
      if LXEROResponseJSON.Result then
      begin
        Result.FromJSON(LXEROResponseJSON.AsString);
      end
      else
      begin
        raise EXEROContactException.Create(LXEROResponseJSON.ErrorMessage);
      end;
    end;
  finally
    FreeAndNil(LXEROFilter);
    FreeAndNil(LXEROResponseJSON);
  end;
end;


function TXEROContacts.GetAPIURL: string;
begin
  Result := XERO_API_BASE_URL + 'Contacts';
end;

end.
