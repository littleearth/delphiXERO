unit XERO.Contacts;

interface

uses
  System.SysUtils, DB,
  Classes, XERO.API, XERO.Types, XERO.API.Response.JSON, XERO.Model, XERO.Response.Model,
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

  TXEROContacts = class(TXEROAPI)
  private
    FXEROResponseJSON: TXEROResponseJSON;
  protected
    function GetAPIURL: string; override;
    property XEROResponseJSON: TXEROResponseJSON read FXEROResponseJSON;
  public
    procedure AfterConstruction; override;
    function Search(APage: Integer = 0; AOrderBy: string = '';
      AContactID: string = ''; ContactNumber: string = ''; AIDList: string = '';
      AIncludeArchived: boolean = false; ALastModified: TDateTime = 0)
      : TXEROContactResponse;
  end;

implementation

{ TXEROContacts }

function TXEROContacts.Search(APage: Integer;
  AOrderBy, AContactID, ContactNumber, AIDList: string;
  AIncludeArchived: boolean; ALastModified: TDateTime)
  : TXEROContactResponse;
begin
  Result := TXEROContactResponse.Create;
  ResetFilter;
  AddGUIDToFilter('ContactID', AContactID);
  AddToFilter('ContactNumber', ContactNumber);
  AddToFilter('IDs', AIDList);
  if AIncludeArchived then
    AddToFilter('includeArchived', 'true');
  if Find(Filter, AOrderBy, APage, ALastModified) then
  begin
    if XEROResponseJSON.Result then
    begin
      Result.FromJSON(XEROResponseJSON.AsString);
    end
    else
    begin
      raise EXEROContactException.Create(XEROResponseJSON.ErrorMessage);
    end;
  end;
end;

procedure TXEROContacts.AfterConstruction;
begin
  inherited;
  FXEROResponseJSON := TXEROResponseJSON.Create(nil);
  Response := FXEROResponseJSON;
end;

function TXEROContacts.GetAPIURL: string;
begin
  Result := XERO_API_BASE_URL + 'Contacts';
end;

end.
