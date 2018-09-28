unit XERO.Contact.Model;

interface

uses
  Classes, SysUtils, XERO.Model, XERO.Address.Model, XERO.Phone.Model;

type
  TXMContactPerson = class(TXEROModel)
  private
    FLastName: string;
    FIncludeInEmails: Boolean;
    FFirstName: string;
    FEmailAddress: string;
    procedure SetEmailAddress(const Value: string);
    procedure SetFirstName(const Value: string);
    procedure SetIncludeInEmails(const Value: Boolean);
    procedure SetLastName(const Value: string);
  public
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property EmailAddress: string read FEmailAddress write SetEmailAddress;
    property IncludeInEmails: Boolean read FIncludeInEmails
      write SetIncludeInEmails;
  end;

type
  TXMContactPersons = TXEROModelList<TXMContactPerson>;

type
  TXMContact = clasS(TXEROModel)
  private
    FName: string;
    FIsCustomer: Boolean;
    FLastName: string;
    FHasAttachments: Boolean;
    [XEROModelManagedAttribute]
    FPhones: TXMPhones;
    FPaymentTerms: string;
    FContactID: string;
    FUpdatedDateUTC: TDateTime;
    FTaxNumber: string;
    FSkypeUserName: string;
    FContactStatus: string;
    FTrackingCategoryName: string;
    FIsSupplier: Boolean;
    FContactNumber: string;
    FDefaultCurrency: string;
    FAccountNumber: string;
    [XEROModelManagedAttribute]
    FContactPersons: TXMContactPersons;
    FDiscount: string;
    FBankAccountDetails: string;
    FWebsite: string;
    FBatchPayments: string;
    FFirstName: string;
    FTrackingOptionName: string;
    FAccountsPayableTaxType: string;
    FAccountsReceivableTaxType: string;
    FEmailAddress: string;
    FXeroNetworkKey: string;
    [XEROModelManagedAttribute]
    FAddresses: TXMAddresses;
    procedure SetAccountNumber(const Value: string);
    procedure SetAccountsPayableTaxType(const Value: string);
    procedure SetAccountsReceivableTaxType(const Value: string);

    procedure SetBankAccountDetails(const Value: string);
    procedure SetBatchPayments(const Value: string);
    procedure SetContactID(const Value: string);
    procedure SetContactNumber(const Value: string);
    procedure SetContactStatus(const Value: string);
    procedure SetDefaultCurrency(const Value: string);
    procedure SetDiscount(const Value: string);
    procedure SetEmailAddress(const Value: string);
    procedure SetFirstName(const Value: string);
    procedure SetHasAttachments(const Value: Boolean);
    procedure SetIsCustomer(const Value: Boolean);
    procedure SetIsSupplier(const Value: Boolean);
    procedure SetLastName(const Value: string);
    procedure SetName(const Value: string);
    procedure SetPaymentTerms(const Value: string);
    procedure SetSkypeUserName(const Value: string);
    procedure SetTaxNumber(const Value: string);
    procedure SetTrackingCategoryName(const Value: string);
    procedure SetTrackingOptionName(const Value: string);
    procedure SetUpdatedDateUTC(const Value: TDateTime);
    procedure SetWebsite(const Value: string);
    procedure SetXeroNetworkKey(const Value: string);
  public
    property ContactID: string read FContactID write SetContactID;
    property ContactNumber: string read FContactNumber write SetContactNumber;
    property AccountNumber: string read FAccountNumber write SetAccountNumber;
    property ContactStatus: string read FContactStatus write SetContactStatus;
    property Name: string read FName write SetName;
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property EmailAddress: string read FEmailAddress write SetEmailAddress;
    property SkypeUserName: string read FSkypeUserName write SetSkypeUserName;
    property BankAccountDetails: string read FBankAccountDetails
      write SetBankAccountDetails;
    property TaxNumber: string read FTaxNumber write SetTaxNumber;
    property AccountsReceivableTaxType: string read FAccountsReceivableTaxType
      write SetAccountsReceivableTaxType;
    property AccountsPayableTaxType: string read FAccountsPayableTaxType
      write SetAccountsPayableTaxType;
    // [XEROModelManagedAttribute]
    property Addresses: TXMAddresses read FAddresses;
    // [XEROModelManagedAttribute]
    property Phones: TXMPhones read FPhones;
    property IsSupplier: Boolean read FIsSupplier write SetIsSupplier;
    property IsCustomer: Boolean read FIsCustomer write SetIsCustomer;
    property DefaultCurrency: string read FDefaultCurrency
      write SetDefaultCurrency;
    property UpdatedDateUTC: TDateTime read FUpdatedDateUTC
      write SetUpdatedDateUTC;
    // The following are only retrieved on GET requests for a single contact or when pagination is used
    // [XEROModelManagedAttribute]
    property ContactPersons: TXMContactPersons read FContactPersons;
    property XeroNetworkKey: string read FXeroNetworkKey
      write SetXeroNetworkKey;
    // property SalesDefaultAccountCode
    // property SalesTrackingCategories :
    // property PurchasesTrackingCategories :
    property TrackingCategoryName: string read FTrackingCategoryName
      write SetTrackingCategoryName;
    property TrackingOptionName: string read FTrackingOptionName
      write SetTrackingOptionName;
    property PaymentTerms: string read FPaymentTerms write SetPaymentTerms;
    // property ContactGroups
    property Website: string read FWebsite write SetWebsite;
    // property BrandingTheme
    property BatchPayments: string read FBatchPayments write SetBatchPayments;
    property Discount: string read FDiscount write SetDiscount;
    property HasAttachments: Boolean read FHasAttachments
      write SetHasAttachments;

  end;

type
  TXMContacts = TXEROModelList<TXMContact>;

implementation

{ TXMContactPerson }

procedure TXMContactPerson.SetEmailAddress(const Value: string);
begin
  FEmailAddress := Value;
end;

procedure TXMContactPerson.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TXMContactPerson.SetIncludeInEmails(const Value: Boolean);
begin
  FIncludeInEmails := Value;
end;

procedure TXMContactPerson.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

{ TXMContact }

procedure TXMContact.SetAccountNumber(const Value: string);
begin
  FAccountNumber := Value;
end;

procedure TXMContact.SetAccountsPayableTaxType(const Value: string);
begin
  FAccountsPayableTaxType := Value;
end;

procedure TXMContact.SetAccountsReceivableTaxType(const Value: string);
begin
  FAccountsReceivableTaxType := Value;
end;

procedure TXMContact.SetBankAccountDetails(const Value: string);
begin
  FBankAccountDetails := Value;
end;

procedure TXMContact.SetBatchPayments(const Value: string);
begin
  FBatchPayments := Value;
end;

procedure TXMContact.SetContactID(const Value: string);
begin
  FContactID := Value;
end;

procedure TXMContact.SetContactNumber(const Value: string);
begin
  FContactNumber := Value;
end;

procedure TXMContact.SetContactStatus(const Value: string);
begin
  FContactStatus := Value;
end;

procedure TXMContact.SetDefaultCurrency(const Value: string);
begin
  FDefaultCurrency := Value;
end;

procedure TXMContact.SetDiscount(const Value: string);
begin
  FDiscount := Value;
end;

procedure TXMContact.SetEmailAddress(const Value: string);
begin
  FEmailAddress := Value;
end;

procedure TXMContact.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TXMContact.SetHasAttachments(const Value: Boolean);
begin
  FHasAttachments := Value;
end;

procedure TXMContact.SetIsCustomer(const Value: Boolean);
begin
  FIsCustomer := Value;
end;

procedure TXMContact.SetIsSupplier(const Value: Boolean);
begin
  FIsSupplier := Value;
end;

procedure TXMContact.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

procedure TXMContact.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TXMContact.SetPaymentTerms(const Value: string);
begin
  FPaymentTerms := Value;
end;

procedure TXMContact.SetSkypeUserName(const Value: string);
begin
  FSkypeUserName := Value;
end;

procedure TXMContact.SetTaxNumber(const Value: string);
begin
  FTaxNumber := Value;
end;

procedure TXMContact.SetTrackingCategoryName(const Value: string);
begin
  FTrackingCategoryName := Value;
end;

procedure TXMContact.SetTrackingOptionName(const Value: string);
begin
  FTrackingOptionName := Value;
end;

procedure TXMContact.SetUpdatedDateUTC(const Value: TDateTime);
begin
  FUpdatedDateUTC := Value;
end;

procedure TXMContact.SetWebsite(const Value: string);
begin
  FWebsite := Value;
end;

procedure TXMContact.SetXeroNetworkKey(const Value: string);
begin
  FXeroNetworkKey := Value;
end;

end.
