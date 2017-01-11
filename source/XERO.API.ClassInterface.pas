{: XERO API Class-based interface
 @cat
}
unit XERO.API.ClassInterface;

interface
uses
  // System
  System.Classes, System.Sysutils,
  System.JSON.Writers,
  System.Rtti,
  Generics.Collections,

  // XERO
  XERO.API,
  XERO.JSONSAX,
  XERO.MiscUtil,
  XERO.VarUtil ;

type

  {: Object Serialisation output mode.
  }
  TXEROSerialiseOutputMode = (xsomUpdate, xsomDisplay);

  //: Base for parsing a XERO JSON Object.
  TXEROObjectLoaderBase = class(TJSONSAXNestableHandler)
  public
    //: Called before loading a single object.
    procedure StartItem; virtual; abstract;
    //: Called after loading a single object
    procedure FinishItem; virtual; abstract;
  end;

  {: Validation information for XERO objects.

    This information is provided with errors/responses to operations.
  }
  TXEROValidation = class
  protected
    FErrors : TStrings;
    FWarnings : TStrings;
  public
    procedure AfterConstruction; override;
    Procedure BeforeDestruction; override;
    property Errors: TStrings read FErrors;
    property Warnings: TStrings read FWarnings;
  end;

  TXEROValidationError = (xveWarning, xveError);
  {: Which validation errors have been returned.
  }
  TXEROValidationErrors = set of TXEROValidationError;

  {: Currently Available Property Types
  }
  TXEROPropertyType = (xptUnknown, xptString, xptCurrency, xptDateTime, xptInteger, xptBoolean, xptEnum, xptObject, xptList);
  {: Options defining when properties can be used.
  }
  TXEROPropertyUsage = (
    xpuReqNew,   // Required for NEW objects
    xpuReqUpdate,// Required for UPDATING objects
    xpuNew,      // Allowed for NEW objects
    xpuUpdate,   // Allowed for UPDATING objects
    xpuConditional, // Conditional (Calls CanOutField)
    xpuSkipBlank // Skip output if the property is blank.
    );
  TXEROPropertyUsageSet = set of TXEROPropertyUsage;

  {: Type information entry for a property.
  }
  TXEROPropertyEntry = record
    PropType : TXEROPropertyType;
    PropName : String;
    PropDefault : String;
    PropUsage : TXEROPropertyUsageSet;
  end;
  {: Calculated type information for a property.
  }
  TXEROPropertyMapEntry = record
    PropType : TXEROPropertyType; // Property Type
    TypeIdx  : integer;           // Index into array for that type.
  end;

  {: Format for object serialisation.
  }
  TXEROOutputType = ( xotJSON, xotXML);

  {: When outputting a property with the writer, there is a difference
    between JSON and XML.  This primarily applies to when the JSON
    would be an Anonymous (Unnamed) property.
  }
  TXEROPropertyMode = (xpmNamed, xpmAnon);

  {: Base writer interface for outputting properties to a TTextWriter.
  }
  TXEROObjectWriter = class
  protected
    FTextWriter : TTextWriter;
    FOwnsStream : boolean;
    FForDisplay : boolean;
  public
    {: Create a text writer.
      @param AForDisplay  Set to true for displaying/debug.(pretty print with CRLFs and indent)
      @param ATextWriter Text Writer object (TStringWriter and TStreamWriter);
      @aprams AOwns  Set to true for the Object Writer to take ownership of the ATextWriter object.
    }
    constructor Create(AForDisplay : boolean; ATextWriter : TTextWriter; AOwns : boolean);
    Procedure BeforeDestruction; override;

    //: Write properties
    procedure WriteProperty( const APropName : String; AValue : String; AMode : TXEROPropertyMode = xpmNamed); overload; virtual; abstract;
    procedure WriteProperty( const APropName : String; AValue : currency; AMode : TXEROPropertyMode = xpmNamed); overload; virtual; abstract;
    procedure WriteProperty( const APropName : String; AValue : TDateTime; AMode : TXEROPropertyMode = xpmNamed); overload; virtual; abstract;
    procedure WriteProperty( const APropName : String; AValue : Boolean; AMode : TXEROPropertyMode = xpmNamed); overload; virtual; abstract;
    procedure WriteProperty( const APropName : String; AValue : Integer; AMode : TXEROPropertyMode = xpmNamed); overload; virtual; abstract;
    procedure WritePropertyNull( const APropName : String; AMode : TXEROPropertyMode = xpmNamed); virtual; abstract;

    //: Start a sub-object
    procedure WriteStartObject(Const APropName : String; AMode : TXEROPropertyMode); virtual; abstract;
    //: Finish a sub-object
    procedure WriteEndObject(Const APropName : String; AMode : TXEROPropertyMode); virtual; abstract;
    //: Start an array
    procedure WriteStartArray(const APropName : String; AMode : TXEROPropertyMode); virtual; abstract;
    //: End an array
    procedure WriteEndArray(const APropName : String; AMode : TXEROPropertyMode); virtual; abstract;
  end;


  TXEROObjectListBase = class;

  //: Status relevant to various objects.
  TXEROStatus = (xsNotSet, xsActive, xsDeleted, xsArchived);

  //: Indicate Special fields
  TXEROSpecialField = (xsfUID, xsfName);

  //: Getting a field for Read or Write
  TXEROFieldAccessMode = (xfamRead, xfamWrite);

  TXERODescribeIdent = (xdiNone, xdiConditional, xdiForce);

  //: Base XERO object.
  TXEROObject = class(TPersistent)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; virtual;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; virtual;
    class function PropStringAsEnum( AField : Word; const StgVal : String) : integer; virtual;
    class function PropEnumAsString( AField : Word; IntVal : Integer) : string; virtual;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; virtual;
    class function PropFieldID( AFieldName : String) : integer; virtual;

    class function PropObjectName : String; virtual;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; virtual;
  protected
    FIsNew: Boolean;
    FValidation: TXEROValidation;

    //----------------------------
    // Handle properties
    //

    FValsStg : TArray<String>;
    FValsCy : TArray<Currency>;
    FValsDT : TArray<TDateTime>;
    FValsInt : TArray<Integer>;
    FValsBool : TArray<Byte>;
    FModified : Int64;

    procedure wVariantVal( AField : Integer; newVal : Variant);
    function rVariantVal(AField : Integer) : Variant;
    procedure wModified(AField : Integer);
    function rModified(AField : Integer) : boolean; inline;

    procedure wStringVal( AField : Integer; newVal : String);
    function rStringVal(AField : Integer) : String;
    procedure wCurrencyVal( AField : Integer; newVal : Currency);
    function rCurrencyVal(AField : Integer) : Currency;
    procedure wDateTimeVal( AField : Integer; newVal : TDateTime);
    function rDateTimeVal(AField : Integer) : TDateTime;
    procedure wIntegerVal( AField : Integer; newVal : Integer);
    function rIntegerVal(AField : Integer) : Integer;
    procedure wBooleanVal(AField : Integer; NewVal : boolean);
    function rBooleanVal(AField : Integer) : boolean;

    procedure wEnumStgVal(AField : Integer; newVal : String);
    function rEnumStgVal(AField : Integer) : String;

    procedure wXStatus(AField : integer; newVal : TXEROStatus);
    function rXStatus(AField : integer) : TXEROStatus;
    //----------------------------

    // Get at validation
    function rValidation: TXEROValidation;

    //: Return the primary Field Name for using as a reference.
    function GetReferenceName : String; virtual;
    //: Return the primary field ID value for using as a reference.
    function GetReferenceID : String; virtual;

    //: Return true if this is a null reference.
    function IsNullReference : Boolean; virtual;

    // Can output a conditional field.
    function CanOutField( Field : integer; mode : TXEROSerialiseOutputMode) : boolean; virtual;

    //: Output fields to a generic writer.
    function OutFields( writer : TXEROObjectWriter; mode : TXEROSerialiseOutputMode) : boolean;

    //: Output contained object fields
    function OutObjectField( Field : Integer; Writer : TXEROObjectWriter; mode: TXEROSerialiseOutputMode) :boolean; virtual;

    function GetObject(field : Integer) :TXEROObject; virtual;

    //: Get at list field
    function GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase; virtual;

    function DoCheckValidation : TXEROValidationErrors; virtual;
    procedure wIsNew(NewVal: Boolean); virtual;
  public
    procedure AfterConstruction; override;

    // Output this object to a writer.
    function OutObject( writer : TXEROObjectWriter; APropName : String; propMode : TXEROPropertyMode; mode : TXEROSerialiseOutputMode) : boolean;

    // Serialise this object to a text writer.
    function Serialise( textStream : TTextWriter; AOutType : TXEROOutputType; mode : TXEROSerialiseOutputMode) : boolean;

    // Clear object
    procedure Clear; virtual;

    // Assign Object.
    procedure Assign( ASource : TPersistent); override;

    // Reset modified flag.
    procedure ResetModified; virtual;

    // Check validation settings
    function HasValidation : boolean;
    function HasValidationWarnings : boolean;
    function HasValidationErrors : boolean;

    // Describe any validation errors
    function DescribeValidation(Identify : boolean; IndentLevel : integer = 0; Errors : TXEROValidationErrors =[xveError, xveWarning]) : String; virtual;

    // Set a field by name.  Return false if unable to find field.
    function SetField(const fieldName : String; newVal : TValue) : boolean;

    // XERO returns special Validation  sections for objects returned from a PUT/POST
    property Validation : TXEROValidation read rValidation;

    // This object IsNew
    property IsNew : Boolean read FIsNew write wIsNew;
  end;

  {: A Base class representing a list of XERO Objects.
    Allows us to have general functions operating/returning on a XERO list
    The generic implementation TXEROObjectList is there to provide the implementation
    via a generics TList.
  }
  TXEROObjectListBase = class(TPersistent)
  protected
    function GetXEROObject(idx : integer) : TXEROObject; virtual; abstract;
  public
    //: Override to provide the Single item name.
    class function PropItemName : String; virtual; abstract;
    class function PropListName : String; virtual; abstract;

  public
    //: Return a JSON loader for a single XERO object.
    function GetItemLoader : TXEROObjectLoaderBase; virtual; abstract;

    //: Returns a loader for the list, constructed with the single JSON loader.
    function GetListLoader : TJSONSAXNestableHandler;

    // Output this object list to a writer.
    function OutObject( writer : TXEROObjectWriter; APropName : String; propMode : TXEROPropertyMode; mode : TXEROSerialiseOutputMode) : boolean;
    // Serialise this object lise to a text writer.
    function Serialise( textStream : TTextWriter; AOutType : TXEROOutputType; mode : TXEROSerialiseOutputMode) : boolean;

    // Reset modified flag on all items.
    procedure ResetModified;
    // Set 'IsNew' for all items.
    procedure SetIsNew(newVal : boolean);

    // Add a XERO Object to the list.
    function AddXEROObject( AObj : TXEROObject) : integer; virtual; abstract;

    // Return the number of items in the list.
    function Count : Integer; virtual; abstract;

    // Clear items from list.
    procedure Clear; virtual; abstract;

    // Create an item for the list.
    class function CreateListObject : TXEROObject; virtual; abstract;

    // Describe any validation errors
    function DescribeValidation(Identify : boolean; IndentLevel : integer = 0; Errors : TXEROValidationErrors =[xveError, xveWarning]) : String; virtual;

    // Access to the XERO Objects in the list.
    property XEROItems[idx : integer] : TXEROObject read GetXEROObject;
  end;

  //: The XERO Object List loader.  (In XERO _everything_ gets loaded as a list)
  TXEROObjectList<XOBJ : TXEROObject, constructor> = class(TXEROObjectListBase)
  public
    class function PropItemName : String; override;
  protected
    FList : Generics.Collections.TObjectList<XOBJ>;
    function GetXEROObject(idx : integer) : TXEROObject; override;
    function rItems(idx : integer): XOBJ;
  public
    procedure AfterConstruction; override;
    Procedure BeforeDestruction; override;

    procedure Assign( ASource : TPersistent); override;

    procedure Insert(Index: Integer; const Value: XOBJ); inline;
    function Add(const Value: XOBJ): Integer; inline;

    function AddXEROObject( AObj : TXEROObject) : integer; override;

    class function CreateListObject : TXEROObject; override;
    function GetItemLoader : TXEROObjectLoaderBase; override;

    function Count : Integer; override;
    procedure Clear; override;

    function GetEnumerator : Generics.Collections.TObjectList<XOBJ>.TEnumerator;
    property List: Generics.Collections.TObjectList<XOBJ> read FList;
    property Items[idx : integer] : XOBJ read rItems; default;
  end;

  TXeroOrganisationType = (
    xotNotSet,
    xotCompany,
    xotCharity,
    xotClubsociety,
    xotPartnership,
    xotPractice,
    xotPerson,
    xotSoletrader,
    xotTrust);

  TXeroLinkType = (
    xltNotSet,
    xltFacebook,
    xltGooglePlus,
    xltLinkedIn,
    xltTwitter,
    xltWebsite
  );

  //: External Link/URL
  TXEROExternalLink = class(TXEROObject)
  public
    class function PropObjectName : string; override;
  protected
    FLinkType: TXeroLinkType;
    FUrl: String;
    function rLinkTypString: String;
    procedure wLinkTypString(NewVal: String);
  public
    procedure Clear; override;
    procedure Assign( ASource : TPersistent); override;

    property LinkTypeString : String read rLinkTypString write wLinkTypString;
  published
    // See External link types
    property   LinkType : TXeroLinkType read FLinkType write FLinkType;
    // URL for service e.g. http://twitter.com/xeroapi
    property   URL : String read FUrl write FUrl;
  end;

  //: List of External links
  TXEROExternalLinkList = class(TXEROObjectList<TXEROExternalLink>)
  public
    class function PropListName : String; override;
    function GetItemLoader : TXEROObjectLoaderBase; override;
  end;
  TXEROOrganisationField = (
    xofName,
    xofLegalName,
    xofPaysTax,
    xofVersion,
    xofOrganisationType,
    xofBaseCurrency,
    xofCountryCode,
    xofIsDemoCompany,
    xofOrganisationStatus,
    xofRegistrationNumber,
    xofTaxNumber,
    xofFinancialYearEndDay,
    xofFinancialYearEndMonth,
    xofSalesTaxBasis,
    xofSalesTaxPeriod,
    xofDefaultSalesTax,
    xofDefaultPurchasesTax,
    xofPeriodLockDate,
    xofEndOfYearLockDate,
    xofCreatedDateUTC,
    xofTimezone,
    xofOrganisationEntityType,
    xofShortCode,
    xofLineOfBusiness,
    xofAddresses,
    xofPhones,
    xofPaymentTerms,
    xofExternalLinks );

  //: XERO Organisation information.
  TXEROOrganisation = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropStringAsEnum( AField : Word; const StgVal : String) : integer; override;
    class function PropEnumAsString( AField : Word; IntVal : Integer) : string; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropObjectName : string; override;
  protected
    FExternalLinks: TXEROExternalLinkList;
    procedure wExternalLinks(NewVal: TXEROExternalLinkList);

    function rXeroOrganisationType(AField : Integer) : TXeroOrganisationType;
    procedure wXeroOrganisationType(AField : Integer; NewVal: TXeroOrganisationType);

    //: Get at list field
    function GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase; override;
  public
    procedure AfterConstruction; override;
    procedure Clear; override;

    Procedure BeforeDestruction; override;
    property OrganisationTypeString : String  index ord(xofOrganisationType) read rEnumStgVal write wEnumStgVal;
    property OrganisationEntityTypeString : String index ord(xofOrganisationEntityType) read rEnumStgVal write wEnumStgVal;
  published
    // Display name of organisation shown in Xero
    property OrganisationName : String index ord(xofName) read rStringVal write wStringVal;
    // Organisation name shown on Reports
    property LegalName : String index ord(xofLegalName) read rStringVal write wStringVal;
    // Boolean to describe if organisation is registered with a local tax authority i.e. true, false
    property PaysTax : Boolean index ord(xofPaysTax) read rBooleanVal write wBooleanVal;
    // See Version Types
    property Version : String index ord(xofVersion) read rStringVal write wStringVal;
    // Organisation Type
    property OrganisationType : TXeroOrganisationType index ord(xofOrganisationType) read rXeroOrganisationType write wXeroOrganisationType;
    // Default currency for organisation. See ISO 4217 Currency Codes
    property BaseCurrency : String index ord(xofBaseCurrency) read rStringVal write wStringVal;
    // Country code for organisation. See ISO 3166-2 Country Codes
    property CountryCode : String index ord(xofCountryCode) read rStringVal write wStringVal;
    // Boolean to describe if organisation is a demo company.
    property IsDemoCompany : boolean index ord(xofIsDemoCompany) read rbooleanVal write wbooleanVal;
    // Will be set to ACTIVE if you can connect to organisation via the Xero API
    property OrganisationStatus : String index ord(xofOrganisationStatus) read rStringVal write wStringVal;
    // Shows for New Zealand, Australian and UK organisations
    property RegistrationNumber : String index ord(xofRegistrationNumber) read rStringVal write wStringVal;
    // Shown if set. Displays in the Xero UI as Tax File Number (AU), GST Number (NZ), VAT Number (UK) and Tax ID Number (US & Global).
    property TaxNumber : String index ord(xofTaxNumber) read rStringVal write wStringVal;
    // Calendar day e.g. 0-31
    property FinancialYearEndDay : Integer index ord(xofFinancialYearEndDay) read rIntegerVal write wIntegerVal;
    // Calendar Month e.g. 1-12
    property FinancialYearEndMonth : Integer index ord(xofFinancialYearEndMonth) read rIntegerVal write wIntegerVal;
    // The accounting basis used for tax returns. See Sales Tax Basis
    property SalesTaxBasis : String index ord(xofSalesTaxBasis) read rStringVal write wStringVal;
    // The frequency with which tax returns are processed. See Sales Tax Period
    property SalesTaxPeriod : String index ord(xofSalesTaxPeriod) read rStringVal write wStringVal;
    // The default for LineAmountTypes on sales transactions
    property DefaultSalesTax : String index ord(xofDefaultSalesTax) read rStringVal write wStringVal;
    // The default for LineAmountTypes on purchase transactions
    property DefaultPurchasesTax : String index ord(xofDefaultPurchasesTax) read rStringVal write wStringVal;
    // Shown if set. See lock dates
    property PeriodLockDate : TDateTime index ord(xofPeriodLockDate) read rDateTimeVal write wDateTimeVal;
    // Shown if set. See lock dates
    property EndOfYearLockDate : TDateTime index ord(xofEndOfYearLockDate) read rDateTimeVal write wDateTimeVal;
    // Timestamp when the organisation was created in Xero
    property CreatedDateUTC : TDateTime index ord(xofCreatedDateUTC) read rDateTimeVal write wDateTimeVal;
    // Timezone specifications
    property Timezone : String index ord(xofTimezone) read rStringVal write wStringVal;
    // Organisation Type
    property OrganisationEntityType : TXeroOrganisationType index ord(xofOrganisationEntityType) read rXeroOrganisationType write wXeroOrganisationType;
    // A unique identifier for the organisation. Potential uses.
    property ShortCode : String index ord(xofShortCode) read rStringVal write wStringVal;
    // Description of business type as defined in Organisation settings
    property LineOfBusiness : String index ord(xofLineOfBusiness) read rStringVal write wStringVal;
    // Address details for organisation - see Addresses
    property Addresses : String index ord(xofAddresses) read rStringVal write wStringVal;
    // Phones details for organisation - see Phones
    property Phones : String index ord(xofPhones) read rStringVal write wStringVal;
    // Default payment terms for the organisation if set - See Payment Terms below
    property PaymentTerms : String index ord(xofPaymentTerms) read rStringVal write wStringVal;

    // Organisation profile links for popular services such as Facebook, Twitter, GooglePlus and LinkedIn.
    // You can also add link to your website here.
    // Shown if Organisation settings is updated in Xero.
    property ExternalLinks : TXEROExternalLinkList read FExternalLinks write wExternalLinks;
  end;

  TXEROAccountClass =(
    xacNotSet,
    xacAsset,
    xacEquity,
    xacExpense,
    xacLiability,
    xacRevenue
  );
  TXEROAccountType = (
    xatNotSet,
    xatBank,
    xatCurrentAsset,
    xatCurrentLiability,
    xatDepreciation,
    xatDirectCosts,
    xatEquity,
    xatExpense,
    xatFixedAsset,
    xatInventoryAsset,
    xatLiability,
    xatNonCurrentAsset,
    xatOtherIncome,
    xatOverhead,
    xatPrepayment,
    xatRevenue,
    xatSale,
    xatNonCurrentLiability,
    xatPAYGLiability,
    xatSuperannuationExpense,
    xatSuperannuationLiability,
    xatWagesExpense,
    xatWagesPayableLiability
  );
  TXEROAccountStatus = (
    xasNotSet,
    xasActive,
    xasArchived
  );

  TXEROAccountField = (
    xafAccountID,
    xafCode,
    xafName,
    xafType,
    xafBankAccountNumber,
    xafStatus,
    xafDescription,
    xafBankAccountType,
    xafCurrencyCode,
    xafTaxType,
    xafEnablePaymentsToAccount,
    xafShowInExpenseClaims,
    xafClass,
    xafSystemAccount,
    xafReportingCode,
    xafReportingCodeName,
    xafHasAttachments,
    xafUpdatedDateUTC
  );

  {: XERO Account object.
  }
  TXEROAccount = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropStringAsEnum( AField : Word; const StgVal : String) : integer; override;
    class function PropEnumAsString( AField : Word; IntVal : Integer) : string; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropObjectName : string; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
  protected

    // Can output a conditional field.
    function CanOutField( Field : integer; mode : TXEROSerialiseOutputMode) : boolean; override;

    function rAccountType: TXEROAccountType;
    procedure wAccountType(NewVal: TXEROAccountType);
    function rStatus: TXEROAccountStatus;
    procedure wStatus(NewVal: TXEROAccountStatus);
    function rAccountClass: TXEROAccountClass;
    procedure wAccountClass(NewVal: TXEROAccountClass);
  public
    procedure Clear; override;
    procedure Assign( ASource : TPersistent); override;

    // System defined ID for identification
    property AccountID : string index ord(xafAccountID) read rStringVal write wStringVal;

    // Customer defined alpha numeric account code e.g 200 or SALES (max length = 10)
    property Code : String index ord(xafCode) read rStringVal write wStringVal;
    // Name of account (max length = 150)
    property AccountName : String index ord(xafName) read rStringVal write wStringVal;

    // See Account Types
    property AccountType : TXEROAccountType read rAccountType write wAccountType;

    // See Account Types
    property AccountTypeString : String index ord(xafType) read rEnumStgVal write wEnumStgVal;

    // For bank accounts only (Account Type BANK)
    property BankAccountNumber : String index ord(xafBankAccountNumber) read rStringVal write wStringVal;

    // Accounts with a status of ACTIVE can be updated to ARCHIVED. See Account Status Codes
    property Status : TXEROAccountStatus read rStatus write wStatus;
    property StatusString : String index ord(xafStatus) read rEnumStgVal write wEnumStgVal;

    // Description of the Account. Valid for all types of accounts except bank accounts (max length = 4000)
    property Description : String index ord(xafDescription) read rStringVal write wStringVal;
    // For bank accounts only. See Bank Account types
    property BankAccountType : String index ord(xafBankAccountType) read rStringVal write wStringVal;
    // For bank accounts only
    property CurrencyCode : String index ord(xafCurrencyCode) read rStringVal write wStringVal;
    // See Tax Types
    property TaxType : String index ord(xafTaxType) read rStringVal write wStringVal;
    // Boolean - describes whether account can have payments applied to it
    property EnablePaymentsToAccount : Boolean index ord(xafEnablePaymentsToAccount) read rBooleanVal write wBooleanVal;
    // Boolean - describes whether account code is available for use with expense claims
    property ShowInExpenseClaims : Boolean index ord(xafShowInExpenseClaims) read rBooleanVal write wBooleanVal;

    // See Account Class Types
    property AccountClass : TXEROAccountClass read rAccountClass write wAccountClass;
    property AccountClassString : String index ord(xafClass) read rEnumStgVal write wEnumStgVal;

    // If this is a system account then this element is returned. See System Account types. Note that non-system accounts may have this element set as either "" or null.
    property SystemAccount : String index ord(xafSystemAccount) read rStringVal write wStringVal;
    // Shown if set
    property ReportingCode : String index ord(xafReportingCode) read rStringVal write wStringVal;
    // Shown if set
    property ReportingCodeName : String index ord(xafReportingCodeName) read rStringVal write wStringVal;
    // boolean to indicate if an account has an attachment (read only)
    property HasAttachments : Boolean index ord(xafHasAttachments) read rBooleanVal write wBooleanVal;
    // Last modified date UTC format
    property UpdatedDateUTC : TDateTime index ord(xafUpdatedDateUTC) read rDateTimeVal write wDateTimeVal;
  end;

  {: XERO Account List.
  }
  TXEROAccountList = class(TXEROObjectList<TXEROAccount>)
  public
    class function PropListName : String; override;
    function GetItemLoader : TXEROObjectLoaderBase; override;
  end;

  TXEROTrackingOptionField = (xtofTrackingOptionID, xtofName, xtofStatus);

  TXEROTrackingOption = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropStringAsEnum( AField : Word; const StgVal : String) : integer; override;
    class function PropEnumAsString( AField : Word; IntVal : Integer) : string; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
    class function PropObjectName : string; override;
  public
    property TrackingOptionID : String index ord(xtofTrackingOptionID) read rStringVal write wStringVal;
    property OptionName : String index ord(xtofName) read rStringVal write wStringVal;
    // The status of a tracking category
    property Status : TXEROStatus index ord(xtofStatus) read rXStatus write wXStatus;
    property StatusAsString : string index ord(xtofStatus) read rEnumStgVal write wEnumStgVal;
  end;

  TXEROTrackingOptionList = class(TXEROObjectList<TXEROTrackingOption>)
  public
    class function PropListName : String; override;
    function GetItemLoader : TXEROObjectLoaderBase; override;
  end;

  TXEROTrackingCategoryField = (xtcfTrackingCategoryID, xtcfName, xtcfStatus, xtcfOptions);

  {: XERO Tracking category
  }
  TXEROTrackingCategory = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropStringAsEnum( AField : Word; const StgVal : String) : integer; override;
    class function PropEnumAsString( AField : Word; IntVal : Integer) : string; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
    class function PropObjectName : string; override;
  protected
    FOptions: TXEROTrackingOptionList;
    procedure wOptions(NewVal: TXEROTrackingOptionList);
        //: Get at list field
    function GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase; override;

  public
    procedure AfterConstruction; override;
    Procedure BeforeDestruction; override;

    // The Xero identifier for a tracking category
    property TrackingCategoryID : String index ord(xtcfTrackingCategoryID) read rStringVal write wStringVal;
    // The name of the tracking category e.g. Department, Region (max length = 100)
    property CategoryName : String index ord(xtcfName) read rStringVal write wStringVal;
    // The status of a tracking category
    property Status : TXEROStatus index ord(xtcfStatus) read rXStatus write wXStatus;
    property StatusAsString : string index ord(xtcfStatus) read rEnumStgVal write wEnumStgVal;
    // See Tracking Options
    property Options : TXEROTrackingOptionList read FOptions write wOptions;
  end;

  {: Tracking Category list.
  }
  TXEROTrackingCategoryList = class(TXEROObjectList<TXEROTrackingCategory>)
  public
    class function PropListName : String; override;
    function GetItemLoader : TXEROObjectLoaderBase; override;
  end;

  TXEROTrackingCategoryOptionField = (
    xtcofCategoryID, xtcofName,
    xtcofOptionID, xtcofOption);

  {: Tracking category on manual journal line
  }
  TXEROTrackingCategoryOption = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
    class function PropObjectName : string; override;
  protected
    // Can output a conditional field.
    function CanOutField( Field : integer; mode : TXEROSerialiseOutputMode) : boolean; override;
  public
    property TrackingCategoryID : String index ord(xtcofCategoryID) read rStringVal write wStringVal;
    property CategoryName : String index ord(xtcofName) read rStringVal write wStringVal;
    property TrackingOptionID : String index ord(xtcofOptionID) read rStringVal write wStringVal;
    property Option : String index ord(xtcofOption) read rStringVal write wStringVal;
  end;

  {: option for a category.
  }
  TXEROTrackingCategoryOptionList = class(TXEROObjectList<TXEROTrackingCategoryOption>)
  public
    class function PropListName : String; override;
  end;

  TXEROManualJournalLineField = (
    xmjlLineAmount,
    xmjlAccountCode,
    xmjlDescription,
    xmjlTaxType,
    xmjlTracking,
    xmjlTaxAmount);

  {: XERO Manual Journal line.
  }
  TXEROManualJournalLine = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropStringAsEnum( AField : Word; const StgVal : String) : integer; override;
    class function PropEnumAsString( AField : Word; IntVal : Integer) : string; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
    class function PropObjectName : string; override;
  protected
    FTracking: TXEROTrackingCategoryOptionList;
    procedure wTracking(NewVal: TXEROTrackingCategoryOptionList);
        //: Get at list field
    function GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase; override;

  public
    procedure AfterConstruction; override;
    Procedure BeforeDestruction; override;
    procedure Clear; override;
    procedure Assign( ASource : TPersistent); override;

    // total for line. Debits are positive, credits are negative value
    property LineAmount : Currency index ord(xmjlLineAmount) read rCurrencyVal write wCurrencyVal;

    // Code of the account
    property AccountCode : String index ord(xmjlAccountCode) read rStringVal write wStringVal;

    // The following are optional for a PUT / POST request

    // Description for journal line
    property Description : String index ord(xmjlDescription) read rStringVal write wStringVal;
    // Used as an override if the default Tax Code for the selected <AccountCode> is not correct - see TaxTypes.
    property TaxType : String index ord(xmjlTaxType) read rStringVal write wStringVal;

    // Optional Tracking Category - see Tracking. Any JournalLine can have a maximum of 2 <TrackingCategory> elements.
    property Tracking : TXEROTrackingCategoryOptionList read FTracking write wTracking;

    // The following are only returned on a GET request
    //
    // The calculated tax amount based on the TaxType and LineAmount
    property TaxAmount : Currency index ord(xmjlTaxAmount) read rCurrencyVal write wCurrencyVal;
  end;

  {: Manual journal line list.
  }
  TXEROManualJournalLineList = class(TXEROObjectList<TXEROManualJournalLine>)
  public
    class function PropListName : String; override;
    function GetItemLoader : TXEROObjectLoaderBase; override;
  end;

  TXEROManualJournalStatus = ( xmjsNotSet, xmjsDraft, xmjsPosted, xmjsDeleted, xmjsVoided);

  TXEROManualJournalField = (
    xmjfManualJournalID,
    xmjfNarration,
    xmjfDate,
    xmjfLineAmountTypes,
    xmjfStatus,
    xmjfUrl,
    xmjfShowOnCashBasisReports,
    xmjfHasAttachments,
    xmjfUpdatedDateUTC,
    xmjfJournalLines
    );

  {: XERO Manual Journal.
  }
  TXEROManualJournal = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropStringAsEnum( AField : Word; const StgVal : String) : integer; override;
    class function PropEnumAsString( AField : Word; IntVal : Integer) : string; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
    class function PropObjectName : string; override;
  protected
    FJournalLines : TXEROManualJournalLineList;
    function rJournalLines: TXEROManualJournalLineList;
    procedure wJournalLines(NewVal: TXEROManualJournalLineList);
    function rHasJournalLines: Boolean;

    function GetReferenceName : String; override;

    function DoCheckValidation : TXEROValidationErrors; override;

    procedure wIsNew(NewVal: Boolean); override;

    //:Get at list field
    function GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase; override;

    function rStatus: TXEROManualJournalStatus;
    procedure wStatus(NewVal: TXEROManualJournalStatus);
  public
    procedure Clear; override;
    procedure Assign( ASource : TPersistent); override;

    Procedure BeforeDestruction; override;

    procedure ResetModified; override;

    // Describe any validation errors
    function DescribeValidation(Identify : boolean; IndentLevel : integer = 0; Errors : TXEROValidationErrors =[xveError, xveWarning]) : String; override;

    // Identifier
    property ManualJournalID : String index ord(xmjfManualJournalID) read rStringVal write wStringVal;

    // The following are mandatory for a PUT / POST request

    // Description of journal being posted
    property Narration : String index ord(xmjfNarration) read rStringVal write wStringVal;

    // Return true if the journal lines have already been loaded.
    property HasLoadedJournalLines : Boolean read rHasJournalLines;

    // JournalLines
    property JournalLines : TXEROManualJournalLineList read rJournalLines write wJournalLines;

    // The following are recommended for a PUT / POST request

    // Date journal was posted - YYYY-MM-DD
    property JournalDate : TDateTime index ord(xmjfDate) read rDateTimeVal write wDateTimeVal;

    // The following are optional for a PUT / POST request

    // NoTax by default if you don't specify this element. See Line Amount Types
    property LineAmountTypes : String index ord(xmjfLineAmountTypes) read rStringVal write wStringVal;

    // See Manual Journal Status Codes
    property StatusAsString : String index ord(xmjfStatus) read rEnumStgVal write wEnumStgVal;
    property Status : TXEROManualJournalStatus read rStatus write wStatus;

    // Url link to a source document - shown as "Go to [appName]" in the Xero app
    property URL : String index ord(xmjfUrl) read rStringVal write wStringVal;

    // Boolean - default is true if not specified
    property ShowOnCashBasisReports : Boolean index ord(xmjfShowOnCashBasisReports) read rBooleanVal write wBooleanVal;

    // The following are only returned on a GET request

    // Boolean to indicate if a manual journal has an attachment
    property HasAttachments : boolean index ord(xmjfHasAttachments) read rBooleanVal write wbooleanVal;

    // Last modified date UTC format
    property UpdatedDateUTC : TDateTime index ord(xmjfUpdatedDateUTC) read rDateTimeVal write wDateTimeVal;
  end;
  {: Manual journal list.
  }
  TXEROManualJournalList = class(TXEROObjectList<TXEROManualJournal>)
  public
    class function PropListName : String; override;
    function GetItemLoader : TXEROObjectLoaderBase; override;
  end;

  //: Sort direction for 'Order by'
  TXEROSortDirection = (xsdAscending, xsdDescending );

  TXEROOrderItem = record
    FieldName : String;
    Direction : TXEROSortDirection;
  end;

  //: Sort order constructor.
  TXEROOrder = record
  private
    FOrders : TArray<TXEROOrderItem>;
  public
    constructor Order(AFieldName : String; ADirection : TXEROSortDirection);
    function Add(AFieldName : String; ADirection : TXEROSortDirection) : TXEROOrder;
    function AsString : String;
    function IsEmpty : boolean;

    //: No Order specified
    class function None: TXEROOrder;static;
  end;
  const OrderNone : TXEROOrder = ();

  type
  XEROException = class(Exception)
  protected
    FCode : Integer;
    FError : String;
  public
    constructor Create( ACode : integer; AError : String);
    property Code : integer read FCode;
    property Error : string read FError;
  end;

  type
  //: Internal HTTP action type for building URI
  THttpActionType = (hatGet, hatSend);

  TXEROSerialise = reference to function ( writer : TTextWriter; mode : TXEROSerialiseOutputMode) : boolean;

  //: Connection to XERO with class based response.
  TXEROConnect = class(TXEROAPIBase)
  protected
    // Build the URI required for an action.
    function BuildURI( const APath, ADoc, AParams  : String; AActionType : THttpActionType = hatGet) : String; overload;
    function BuildURI( const APath, ADoc : String;const AQuery : String; AOrder : TXEROOrder;  AActionType : THttpActionType; Page : Integer) : String; overload;

    // Load a list of objects returned.
    // Raises an exception if an error happens.
    function GetLoadList( const AURI : String; AParams : TStrings;
      const AColnName : String; ALoader : TJSONSAXNestableHandler) : boolean;

    // Load a single object returned.
    // Raises an exception if an error happens.
    function GetLoadSingleResult( const AURI : String; AParams : TStrings;
      const AColnName : String; ALoader : TJSONSAXNestableHandler) : boolean;

    // Store an object
    function Store( const AURL : String; const AColnName, AObjName : String; AXeroObject : TXEROObject; AMode : TXEROSerialiseOutputMode; AObjectLoader : TJSONSAXNestableHandler) : boolean;

    // Store a list of objects.
    function StoreList( const AURL : String; const AColnName, AObjName : String;
      AXeroListStore, AXeroListResponse : TXEROObjectListBase; AMode : TXEROSerialiseOutputMode) : boolean;
    function DoStore( const AURL : String; const AColnName, AObjName, AOuterName  : String;
      ASerialise : TXEROSerialise; AResponseListHandler : TJSONSAXNestableHandler) : boolean;

    // Handle the auto-load property
    function HandleAutoLoad( APropertyCode : String; AReferenceID : String; Loader : TXEROObjectLoaderBase) : boolean;
  public
    // Fetches Organisation information and constructs an organisation object if
    // successful
    function GetOrganisationProperties(properties : TXEROOrganisation) : boolean;

    // Fetches a list of accounts.
    function GetAccounts( accountList : TXEROAccountList; Const AQuery : String = '') : boolean;
    // Fetches a single account by UID/Code
    function GetAccount( account : TXEROAccount; Const AIdent : String) : boolean;

    // Store a single account
    function StoreAccount(account : TXEROAccount) : boolean;

    // Store multiple accounts
    function StoreAccounts(accountList, ResponseList : TXEROAccountList) : boolean;

    // Fetches a list  of Manual Journals
    function GetManualJournals( journalList : TXEROManualJournalList; const AQuery : String = '') : boolean; overload; inline;
    function GetManualJournals( journalList : TXEROManualJournalList; const AQuery : String; const AOrder : TXEROOrder; APageNo : integer = -1) : boolean; overload;

    // Fetches a manual Journal by UID
    function GetManualJournal( manualJournal : TXEROManualJournal; const AUID : String) : boolean;

    // Store a single manual journal.
    function StoreManualJournal(ManualJournal : TXEROManualJournal) : boolean;

    // Store a list  of manual journals.
    function StoreManualJournals(ManualJournalList, ResponseJournals : TXEROManualJournalList) : boolean;

    // Refresh (and load all) a manual journal object
    function Refresh(manualJournal : TXEROManualJournal) : boolean;

    // Load tracking categories
    function GetTrackingCategories(categoryList : TXEROTrackingCategoryList; const AQuery : String = '') : boolean; overload;
    function GetTrackingCategories(categoryList : TXEROTrackingCategoryList; const AQuery : String; const AOrder : TXEROOrder; APageNo : integer = -1) : boolean; overload;
    function GetTrackingCategory(categoryItem : TXEROTrackingCategory; Const AUID : String) : boolean;

    // Store a single tracking category
    function StoreTrackingCategory( categoryItem : TXEROTrackingCategory) : boolean;

    // Store list of tracking categories
    function StoreTrackingCategories( categoryList, responseList : TXEROTrackingCategoryList) : boolean;

  end;

  { Internal loader for a list.
    Required to be in the interface because it is referenced by generic class.
  }
  TXEROListLoader = class(TJSONSAXNestableHandler)
  protected
    FObjectHandler : TXEROObjectLoaderBase;
    FOwnsHandler : Boolean;
  public
    constructor Create(const AName : String; AOBjectHandler : TXEROObjectLoaderBase; AOwns : boolean = true);
    Procedure BeforeDestruction; override;
    procedure StartChildObject; override;
    procedure EndingChildObject; override;
  end;
    TXEROLoaderGenericBase = class(TXEROObjectLoaderBase)
  protected
    FList : TXEROObjectListBase;
    FOwnsCur : boolean;
    FCur : TXEROObject;
    procedure ClearCur;
  public
    //: Create a loader for standard list-mode
    constructor Create(AName : String; AList : TXEROObjectListBase); overload;
    //: Create a loader for a single responses.
    constructor Create(AName : String; AItem : TXEROObject); overload;

    Procedure BeforeDestruction; override;

    //: Handle Creating  new item to add to the list.
    procedure StartItem; override;
    //: Add a new item to the list.
    procedure FinishItem; override;

    // Handle a value pair
    procedure MyPair(const FieldName : String; const FieldValue : TValue); override;

    { Start a child array element.  This needs to be overridden to call
       SetChildElement with the appropriate handler for the section.
    }
    procedure StartChildArrayElement( const aName: String); override;
  end;

  {: A base for the JSon loader for XERO objects that handles putting them into
  a list.
    All XERO responses are lists of items, even if some mean that there's always only one.
  }
  TXEROLoaderGeneric<ITEM : TXEROObject, constructor> = class(TXEROLoaderGenericBase)
  protected
    function GetCur : ITEM;
  public
    //: Create a loader for standard list-mode
    constructor Create(AName : String; AList : TXEROObjectList<ITEM>); overload;
    //: Create a loader for a single responses.
    constructor Create(AName : String; AItem : ITEM); overload;

  end;

  // Construct a XERO Object Writer depending on output type.
  function CreateWriter(forDisplay : boolean; textStream : TTextWriter; AOutType : TXEROOutputType) : TXEROObjectWriter;
implementation

uses
  System.StrUtils,
  System.JSON.Types,
  Generics.Defaults,

  XERO.Utils, XERo.XMLParser,

  IdURI ;

const
  COrganisationProperties : array[ord(low(TXEROOrganisationField))..ord(high(TXEROOrganisationField))] of TXEROPropertyEntry =  (
    ( PropType: xptString;     PropName: 'Name';                   PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'LegalName';              PropDefault: ''; PropUsage : []; ),
    ( PropType: xptBoolean;    PropName: 'PaysTax';                PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'Version';                PropDefault: ''; PropUsage : []; ),
    ( PropType: xptEnum;       PropName: 'OrganisationType';       PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'BaseCurrency';           PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'CountryCode';            PropDefault: ''; PropUsage : []; ),
    ( PropType: xptBoolean;    PropName: 'IsDemoCompany';          PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'OrganisationStatus';     PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'RegistrationNumber';     PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'TaxNumber';              PropDefault: ''; PropUsage : []; ),
    ( PropType: xptInteger;    PropName: 'FinancialYearEndDay';    PropDefault: ''; PropUsage : []; ),
    ( PropType: xptInteger;    PropName: 'FinancialYearEndMonth';  PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'SalesTaxBasis';          PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'SalesTaxPeriod';         PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'DefaultSalesTax';        PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'DefaultPurchasesTax';    PropDefault: ''; PropUsage : []; ),
    ( PropType: xptDateTime;   PropName: 'PeriodLockDate';         PropDefault: ''; PropUsage : []; ),
    ( PropType: xptDateTime;   PropName: 'EndOfYearLockDate';      PropDefault: ''; PropUsage : []; ),
    ( PropType: xptDateTime;   PropName: 'CreatedDateUTC';         PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'Timezone';               PropDefault: ''; PropUsage : []; ),
    ( PropType: xptEnum;       PropName: 'OrganisationEntityType'; PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'ShortCode';              PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'LineOfBusiness';         PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'Addresses';              PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'Phones';                 PropDefault: ''; PropUsage : []; ),
    ( PropType: xptString;     PropName: 'PaymentTerms';           PropDefault: ''; PropUsage : []; ),
    ( PropType: xptList;       PropName: 'ExternalLinks';          PropDefault: ''; PropUsage : []; )
  );

CAccountProperties : array[ord(low(TXEROAccountField))..ord(high(TXEROAccountField))] of TXEROPropertyEntry =  (
  ( PropType: xptString;    PropName: 'AccountID';              PropDefault: ''; PropUsage: [xpuReqUpdate, xpuUpdate];),
  ( PropType: xptString;    PropName: 'Code';                   PropDefault: ''; PropUsage: [xpuReqNew, xpuUpdate];),
  ( PropType: xptString;    PropName: 'Name';                   PropDefault: ''; PropUsage: [xpuReqNew, xpuUpdate];),
  ( PropType: xptEnum;      PropName: 'Type';                   PropDefault: ''; PropUsage: [xpuReqNew, xpuUpdate];),
  ( PropType: xptString;    PropName: 'BankAccountNumber';      PropDefault: ''; PropUsage: [xpuReqNew, xpuUpdate, xpuConditional];),
  ( PropType: xptEnum;      PropName: 'Status';                 PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank];),
  ( PropType: xptString;    PropName: 'Description';            PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank];),
  ( PropType: xptString;    PropName: 'BankAccountType';        PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuConditional];),
  ( PropType: xptString;    PropName: 'CurrencyCode';           PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuConditional];),
  ( PropType: xptString;    PropName: 'TaxType';                PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank];),
  ( PropType: xptBoolean;   PropName: 'EnablePaymentsToAccount';PropDefault: ''; PropUsage: [xpuNew, xpuUpdate];),
  ( PropType: xptBoolean;   PropName: 'ShowInExpenseClaims';    PropDefault: ''; PropUsage: [xpuNew, xpuUpdate];),
  ( PropType: xptEnum;      PropName: 'Class';                  PropDefault: ''; PropUsage: [];),
  ( PropType: xptString;    PropName: 'SystemAccount';          PropDefault: ''; PropUsage: [];),
  ( PropType: xptString;    PropName: 'ReportingCode';          PropDefault: ''; PropUsage: [];),
  ( PropType: xptString;    PropName: 'ReportingCodeName';      PropDefault: ''; PropUsage: [];),
  ( PropType: xptBoolean;   PropName: 'HasAttachments';         PropDefault: ''; PropUsage: [];),
  ( PropType: xptDateTime;  PropName: 'UpdatedDateUTC';         PropDefault: ''; PropUsage: [];)
);

CManualJournalLineProperties : array[ord(low(TXEROManualJournalLineField))..ord(high(TXEROManualJournalLineField))] of TXEROPropertyEntry =  (
  ( PropType: xptCurrency;  PropName: 'LineAmount';  PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptString;    PropName: 'AccountCode'; PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptString;    PropName: 'Description'; PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank];),
  ( PropType: xptString;    PropName: 'TaxType';     PropDefault: ''; PropUsage: [xpuNew, xpuUpdate];),
  ( PropType: xptList;      PropName: 'Tracking';    PropDefault: ''; PropUsage: [xpuNew, xpuUpdate];),
  ( PropType: xptCurrency;  PropName: 'TaxAmount';   PropDefault: ''; PropUsage: [];)
);

CManualJournalProperties : array[ord(low(TXEROManualJournalField))..ord(high(TXEROManualJournalField))] of TXEROPropertyEntry =  (
  ( PropType: xptString;   PropName: 'ManualJournalID';          PropDefault: ''; PropUsage: [xpuReqUpdate];),
  ( PropType: xptString;   PropName: 'Narration';                PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptDateTime; PropName: 'Date';                     PropDefault: ''; PropUsage: [xpuReqNew, xpuUpdate];),
  ( PropType: xptString;   PropName: 'LineAmountTypes';          PropDefault: ''; PropUsage: [xpuNew, xpuUpdate];),
  ( PropType: xptEnum;     PropName: 'Status';                   PropDefault: 'DRAFT'; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank];),
  ( PropType: xptString;   PropName: 'Url';                      PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank];),
  ( PropType: xptBoolean;  PropName: 'ShowOnCashBasisReports';   PropDefault: 'true'; PropUsage: [xpuNew, xpuUpdate];),
  ( PropType: xptboolean;  PropName: 'HasAttachments';           PropDefault: ''; PropUsage: [];),
  ( PropType: xptDateTime; PropName: 'UpdatedDateUTC';           PropDefault: ''; PropUsage: [];),
  ( PropType: xptList;     PropName: 'JournalLines';             PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate];)
);

CTrackingCategoryProperties : array[ord(low(TXEROTrackingCategoryField))..ord(high(TXEROTrackingCategoryField))] of TXEROPropertyEntry =  (
  ( PropType: xptString; PropName: 'TrackingCategoryID'; PropDefault: ''; PropUsage: [xpuReqUpdate];),
  ( PropType: xptString; PropName: 'Name';               PropDefault: ''; PropUsage: [xpuReqNew, xpuUpdate];),
  ( PropType: xptEnum;   PropName: 'Status';             PropDefault: 'ACTIVE'; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank];),
  ( PropType: xptList;   PropName: 'Options';            PropDefault: ''; PropUsage: [xpuNew, xpuUpdate];)
);

CTrackingOptionProperties : array[ord(low(TXEROTrackingOptionField))..ord(high(TXEROTrackingOptionField))] of TXEROPropertyEntry = (
  ( PropType: xptString;  PropName: 'TrackingOptionID'; PropDefault: ''; PropUsage: [xpuReqUpdate];),
  ( PropType: xptString;  PropName: 'Name';             PropDefault: ''; PropUsage: [xpuReqNew, xpuUpdate];),
  ( PropType: xptEnum;    PropName: 'Status';           PropDefault: 'ACTIVE'; PropUsage: [ xpuNew, xpuUpdate, xpuSkipBlank];)
);

CTrackingCategoryOptionProperties : array[ord(low(TXEROTrackingCategoryOptionField))..ord(high(TXEROTrackingCategoryOptionField))] of TXEROPropertyEntry = (

  ( PropType: xptString;  PropName: 'TrackingCategoryID'; PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate, xpuSkipBlank];),
  ( PropType: xptString;  PropName: 'Name';               PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate, xpuConditional];),
  ( PropType: xptString;  PropName: 'TrackingOptionID';   PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate, xpuSkipBlank];),
  ( PropType: xptString;  PropName: 'Option';             PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate, xpuConditional];)
);

type
  TXERONameEntry = record
    PropName : String;
    FieldNo : integer;
    constructor Search(APropName : String);
  end;
  TXEROPropertyMap = record
    Count : array[TXEROPropertyType] of word;
    Map : TArray<TXEROPropertyMapEntry>;
    NameMap : TArray<TXERONameEntry>;
    function NameToFieldID( const AFieldName : String) : integer;
  end;

var
  G_Comparer : Generics.Defaults.IComparer<TXERONameEntry>;
  G_TrackingCategoryMap,
  G_TrackingOptionMap,
  G_TrackingCategoryOptionMap,
  G_OrganisationMap,
  G_AccountMap,
  G_ManualJournalMap,
  G_ManualJournalLineMap : TXEROPropertyMap;


  COrganisationType :array[TXeroOrganisationType] of string =
  ( '', 'COMPANY', 'CHARITY', 'CLUBSOCIETY',
    'PARTNERSHIP', 'PRACTICE', 'PERSON', 'SOLETRADER', 'TRUST'
    );
  CLinkType : array[TXeroLinkType] of string = (
    '', 'Facebook', 'GooglePlus', 'LinkedIn', 'Twitter', 'Website'
  );
  CManualJournalStatus : array[TXEROManualJournalStatus] of string = (
   '', 'DRAFT', 'POSTED', 'DELETED', 'VOIDED'
  );
  CXEROStatusType : array[TXEROStatus] of string = (
    '', 'ACTIVE', 'DELETED', 'ARCHIVED'
    );
  CXEROAccountClassType : array [TXEROAccountClass] of string =
  ( '', 'ASSET', 'EQUITY', 'EXPENSE', 'LIABILITY', 'REVENUE');
  CXEROAccountTypeType : array [TXEROAccountType] of string = (
    '',
    'BANK',
    'CURRENT',
    'CURRLIAB',
    'DEPRECIATN',
    'DIRECTCOSTS',
    'EQUITY',
    'EXPENSE',
    'FIXED',
    'INVENTORY',
    'LIABILITY',
    'NONCURRENT',
    'OTHERINCOME',
    'OVERHEADS',
    'PREPAYMENT',
    'REVENUE',
    'SALES',
    'TERMLIAB',
    'PAYGLIABILITY',
    'SUPERANNUATIONEXPENSE',
    'SUPERANNUATIONLIABILITY',
    'WAGESEXPENSE',
    'WAGESPAYABLELIABILITY'
  );
  CXEROAccountStatustype : array [TXEROAccountStatus] of string = (
    '', 'ACTIVE', 'ARCHIVED'
  );

function DateTimeAsJSONString(ADateTime : TDateTime; forDisplay : boolean) : string; forward;

function XEROOrganisationTypeAsEnum( stg : String) : TXeroOrganisationType;
var
  idx : TXeroOrganisationType;
begin
  result := low(TXeroOrganisationType);
  for idx := low(idx) to high(idx) do
  begin
    if CompareText(stg, COrganisationType[idx]) = 0 then
    begin
      result := idx;
      break;
    end;
  end;
end;
function XEROOrganisationTypeAsString( enumVal : Integer) : String;
begin
  if (enumVal < 0) or (enumVal > ord(high(TXeroOrganisationType))) then
    enumVal := ord(low(TXeroOrganisationType));
  result := COrganisationType[TXeroOrganisationType(enumVal)];
end;

function XEROStatusAsEnum( Status : String) : TXEROStatus;
var
  idx : TXEROStatus;
begin
  result := xsNotSet;
  for idx := low(idx) to high(idx) do
  begin
    if CompareText(status, CXEROStatusType[idx]) = 0 then
    begin
      result := idx;
      break;
    end;
  end;
end;
function XEROStatusAsString( enumVal : Integer) : String;
begin
  if (enumVal < 0) or (enumVal > ord(high(TXEROStatus))) then
    enumVal := ord(low(TXEROStatus));
  result := CXEROStatusType[TXEROStatus(enumVal)];
end;

function XEROAccountClassAsEnum( stg : String) : TXEROAccountClass;
var
  idx : TXEROAccountClass;
begin
  result := low(TXEROAccountClass);
  for idx := low(idx) to high(idx) do
  begin
    if CompareText(stg, CXEROAccountClassType[idx]) = 0 then
    begin
      result := idx;
      break;
    end;
  end;
end;
function XEROAccountClassAsString( enumVal : Integer) : String;
begin
  if (enumVal < 0) or (enumVal > ord(high(TXEROAccountClass))) then
    enumVal := ord(low(TXEROAccountClass));
  result := CXEROAccountClassType[TXEROAccountClass(enumVal)];
end;

function XEROAccountTypeAsEnum( stg : String) : TXEROAccountType;
var
  idx : TXEROAccountType;
begin
  result := low(TXEROAccountType);
  for idx := low(idx) to high(idx) do
  begin
    if CompareText(stg, CXEROAccountTypeType[idx]) = 0 then
    begin
      result := idx;
      break;
    end;
  end;
end;
function XEROAccountTypeAsString( enumVal : Integer) : String;
begin
  if (enumVal < 0) or (enumVal > ord(high(TXEROAccountType))) then
    enumVal := ord(low(TXEROAccountType));
  result := CXEROAccountTypeType[TXEROAccountType(enumVal)];
end;

function XEROAccountStatusAsEnum( stg : String) : TXEROAccountStatus;
var
  idx : TXEROAccountStatus;
begin
  result := low(TXEROAccountStatus);
  for idx := low(idx) to high(idx) do
  begin
    if CompareText(stg, CXEROAccountStatusType[idx]) = 0 then
    begin
      result := idx;
      break;
    end;
  end;
end;
function XEROAccountStatusAsString( enumVal : Integer) : String;
begin
  if (enumVal < 0) or (enumVal > ord(high(TXEROAccountStatus))) then
    enumVal := ord(low(TXEROAccountStatus));
  result := CXEROAccountStatusType[TXEROAccountStatus(enumVal)];
end;


constructor TXERONameEntry.Search(APropName : String);
begin
  PropName := APropName;
  FieldNo := -1;
end;

procedure TXEROValidation.AfterConstruction;
begin
  inherited;
  FWarnings := TStringList.Create;
  FErrors := TStringList.Create;
end;

Procedure TXEROValidation.BeforeDestruction;
begin
  FreeAndNil(FWarnings);
  FreeAndNIl(FErrors);
  inherited;
end;

constructor TXEROObjectWriter.Create(AForDisplay : boolean;ATextWriter : TTextWriter; AOwns : boolean);
begin
  FTextWriter := ATextWriter;
  FOwnsStream := AOwns;
  FForDisplay := AForDisplay;
  inherited Create;
end;

Procedure TXEROObjectWriter.BeforeDestruction;
begin
  if FOwnsStream then
    FreeAndNil(FTextWriter)
  else
    FTextWriter := nil;
  inherited;
end;

type
TXEROXMLWriter = class(TXEROObjectWriter)
protected
  FIndent : Integer;
  procedure WriteIndent;
public
  procedure WriteProperty( const APropName : String; AValue : String; AMode : TXEROPropertyMode = xpmNamed); overload; override;
  procedure WriteProperty( const APropName : String; AValue : currency; AMode : TXEROPropertyMode = xpmNamed); overload; override;
  procedure WriteProperty( const APropName : String; AValue : TDateTime; AMode : TXEROPropertyMode = xpmNamed); overload; override;
  procedure WriteProperty( const APropName : String; AValue : Boolean; AMode : TXEROPropertyMode = xpmNamed); overload; override;
  procedure WriteProperty( const APropName : String; AValue : Integer; AMode : TXEROPropertyMode = xpmNamed); overload; override;
  procedure WritePropertyNull( const APropName : String; AMode : TXEROPropertyMode = xpmNamed); override;

  procedure WriteStartObject(Const APropName : String; AMode : TXEROPropertyMode); override;
  procedure WriteEndObject(Const APropName : String; AMode : TXEROPropertyMode); override;
  procedure WriteStartArray( const APropName : String; AMode : TXEROPropertyMode); override;
  procedure WriteEndArray(const APropName : String; AMode : TXEROPropertyMode); override;
end;

procedure TXEROXMLWriter.WriteIndent;
var
  idx : integer;
begin
  if FForDisplay then
  begin
    for idx := 1 to FIndent do
      FTextWriter.Write('    ');
  end;
end;

procedure TXEROXMLWriter.WriteProperty( const APropName : String; AValue : String; AMode : TXEROPropertyMode = xpmNamed);
begin
  WriteIndent;
  FTextWriter.Write('<');
  FTextWriter.Write(APropName);
  FTextWriter.Write('>');
  FTextWriter.Write(TXmlAttribute.Escape(AValue));
  FTextWriter.Write('</');
  FTextWriter.Write(APropName);
  FTextWriter.Write('>');
  if FForDisplay then
    FTextWriter.WriteLine;
end;

procedure TXEROXMLWriter.WriteProperty( const APropName : String; AValue : currency; AMode : TXEROPropertyMode = xpmNamed);
begin
  WriteIndent;
  FTextWriter.Write('<');
  FTextWriter.Write(APropName);
  FTextWriter.Write('>');
  FTextWriter.Write(CurrToStr(AValue));
  FTextWriter.Write('</');
  FTextWriter.Write(APropName);
  FTextWriter.Write('>');
  if FForDisplay then
    FTextWriter.WriteLine;
end;

procedure TXEROXMLWriter.WriteProperty( const APropName : String; AValue : TDateTime; AMode : TXEROPropertyMode = xpmNamed);
begin
  WriteIndent;
  FTextWriter.Write('<');
  FTextWriter.Write(APropName);
  FTextWriter.Write('>');
  if AValue > 0 then
    FTextWriter.Write(XMLDateTimeToStr(AValue));
  FTextWriter.Write('</');
  FTextWriter.Write(APropName);
  FTextWriter.Write('>');
end;

procedure TXEROXMLWriter.WriteProperty( const APropName : String; AValue : Boolean; AMode : TXEROPropertyMode = xpmNamed);
begin
  WriteIndent;
  FTextWriter.Write('<');
  FTextWriter.Write(APropName);
  FTextWriter.Write('>');
  FTextWriter.Write(IfThen(AValue, 'true','false'));
  FTextWriter.Write('</');
  FTextWriter.Write(APropName);
  FTextWriter.Write('>');
  if FForDisplay then
    FTextWriter.WriteLine;
end;

procedure TXEROXMLWriter.WriteProperty( const APropName : String; AValue : Integer; AMode : TXEROPropertyMode = xpmNamed);
begin
  WriteIndent;
  FTextWriter.Write('<');
  FTextWriter.Write(APropName);
  FTextWriter.Write('>');
  FTextWriter.Write(IntToStr(AValue));
  FTextWriter.Write('</');
  FTextWriter.Write(APropName);
  FTextWriter.Write('>');
  if FForDisplay then
    FTextWriter.WriteLine;
end;

procedure TXEROXMLWriter.WritePropertyNull( const APropName : String; AMode : TXEROPropertyMode = xpmNamed);
begin
  WriteIndent;
  FTextWriter.Write('<');
  FTextWriter.Write(APropName);
  FTextWriter.Write(' />');
  if FForDisplay then
    FTextWriter.WriteLine;
end;

procedure TXEROXMLWriter.WriteStartObject(Const APropName : String; AMode : TXEROPropertyMode);
begin
  WriteIndent;
  FTextWriter.Write('<');
  FTextWriter.Write(APropName);
  FTextWriter.Write('>');
  if FForDisplay then
    FTextWriter.WriteLine;
  Inc(FIndent);
end;

procedure TXEROXMLWriter.WriteEndObject(Const APropName : String; AMode : TXEROPropertyMode);
begin
  Dec(FIndent);
  WriteIndent;
  FTextWriter.Write('</');
  FTextWriter.Write(APropName);
  FTextWriter.Write('>');
  if FForDisplay then
    FTextWriter.WriteLine;
end;

procedure TXEROXMLWriter.WriteStartArray( const APropName : String; AMode : TXEROPropertyMode);
begin
  WriteIndent;
  FTextWriter.Write('<');
  FTextWriter.Write(APropName);
  FTextWriter.Write('>');
  if FForDisplay then
    FTextWriter.WriteLine;
  Inc(FIndent);
end;

procedure TXEROXMLWriter.WriteEndArray(const APropName : String; AMode : TXEROPropertyMode);
begin
  Dec(FIndent);
  WriteIndent;
  FTextWriter.Write('</');
  FTextWriter.Write(APropName);
  FTextWriter.Write('>');
  if FForDisplay then
    FTextWriter.WriteLine;
end;

type
TXEROJSONWriter = class(TXEROObjectWriter)
protected
  FWriter : TJSONTextWriter;
public
  procedure AfterConstruction; override;
  Procedure BeforeDestruction; override;

  procedure WriteProperty( const APropName : String; AValue : String; AMode : TXEROPropertyMode = xpmNamed); overload; override;
  procedure WriteProperty( const APropName : String; AValue : currency; AMode : TXEROPropertyMode = xpmNamed); overload; override;
  procedure WriteProperty( const APropName : String; AValue : TDateTime; AMode : TXEROPropertyMode = xpmNamed); overload; override;
  procedure WriteProperty( const APropName : String; AValue : Boolean; AMode : TXEROPropertyMode = xpmNamed); overload; override;
  procedure WriteProperty( const APropName : String; AValue : Integer; AMode : TXEROPropertyMode = xpmNamed); overload; override;
  procedure WritePropertyNull( const APropName : String; AMode : TXEROPropertyMode = xpmNamed); override;

  procedure WriteStartObject(Const APropName : String; AMode : TXEROPropertyMode); override;
  procedure WriteEndObject(Const APropName : String; AMode : TXEROPropertyMode); override;
  procedure WriteStartArray( const APropName : String; AMode : TXEROPropertyMode); override;
  procedure WriteEndArray(const APropName : String; AMode : TXEROPropertyMode); override;
end;

procedure TXEROJSONWriter.AfterConstruction;
begin
  inherited;
  FWriter := TJSONTextWriter.Create(FTextWriter);
  if FForDisplay then
    FWriter.Formatting := TJsonFormatting.Indented;
end;
Procedure TXEROJSONWriter.BeforeDestruction;
begin
  FreeAndNil(FWriter);
  inherited;
end;

procedure TXEROJSONWriter.WriteProperty( const APropName : String; AValue : String; AMode : TXEROPropertyMode = xpmNamed);
begin
  case AMode of
    xpmNamed: FWriter.WritePropertyName(APropName);
  end;
  Fwriter.WriteValue(AValue);
end;

procedure TXEROJSONWriter.WriteProperty( const APropName : String; AValue : currency; AMode : TXEROPropertyMode = xpmNamed);
begin
  case AMode of
    xpmNamed: FWriter.WritePropertyName(APropName);
  end;
  Fwriter.WriteValue(AValue);
end;

procedure TXEROJSONWriter.WriteProperty( const APropName : String; AValue : TDateTime; AMode : TXEROPropertyMode = xpmNamed);
begin
  case AMode of
    xpmNamed: FWriter.WritePropertyName(APropName);
  end;
  Fwriter.WriteValue(DateTimeAsJSONString(AValue, FForDisplay));
end;

procedure TXEROJSONWriter.WriteProperty( const APropName : String; AValue : Boolean; AMode : TXEROPropertyMode = xpmNamed);
begin
  case AMode of
    xpmNamed: FWriter.WritePropertyName(APropName);
  end;
  Fwriter.WriteValue(AValue);
end;

procedure TXEROJSONWriter.WriteProperty( const APropName : String; AValue : Integer; AMode : TXEROPropertyMode = xpmNamed);
begin
  case AMode of
    xpmNamed: FWriter.WritePropertyName(APropName);
  end;
  Fwriter.WriteValue(AValue);
end;

procedure TXEROJSONWriter.WritePropertyNull( const APropName : String; AMode : TXEROPropertyMode = xpmNamed);
begin
  case AMode of
    xpmNamed: FWriter.WritePropertyName(APropName);
  end;
  FWriter.WriteNull;
end;

procedure TXEROJSONWriter.WriteStartObject(Const APropName : String; AMode : TXEROPropertyMode);
begin
  case AMode of
    xpmNamed: FWriter.WritePropertyName(APropName);
  end;
  Fwriter.WriteStartObject;
end;

procedure TXEROJSONWriter.WriteEndObject(Const APropName : String; AMode : TXEROPropertyMode);
begin
  Fwriter.WriteEndObject;
end;

procedure TXEROJSONWriter.WriteStartArray( const APropName : String; AMode : TXEROPropertyMode);
begin
  case AMode of
    xpmNamed: FWriter.WritePropertyName(APropName);
  end;
  Fwriter.WriteStartArray;
end;

procedure TXEROJSONWriter.WriteEndArray(const APropName : String; AMode : TXEROPropertyMode);
begin
  Fwriter.WriteEndArray;
end;

function CreateWriter(forDisplay : boolean; textStream : TTextWriter; AOutType : TXEROOutputType) : TXEROObjectWriter;
begin
  case AOutType of
    xotJSON: result := TXEROJSONWriter.Create(forDisplay, textStream, false );
    xotXML:  result := TXEROXMLWriter.Create(forDisplay, textStream, false);
  else result := nil;
  end;
end;

procedure TXEROObject.AfterConstruction;
var
  idx : integer;
begin
  inherited;
  SetLength(FValsStg, PropTypeCount(xptString));
  for idx := low(FValsStg) to high(FValsStg) do
    FValsStg[idx] := '';
  SetLength(FValsCy, PropTypeCount(xptCurrency));
  for idx := low(FValsCy) to high(FValsCy) do
    FValsCy[idx] := 0;
  SetLength(FValsDT, PropTypeCount(xptDateTime));
  for idx := low(FValsDt) to high(FValsDt) do
    FValsDt[idx] := 0;
  SetLength(FValsInt, PropTypeCount(xptInteger));
  for idx := low(FValsInt) to high(FValsInt) do
    FValsInt[idx] := 0;
  SetLength(FValsBool, (PropTypeCount(xptBoolean)+7) div 8);
  for idx := low(FValsBool) to high(FValsBool) do
    FValsBool[idx] := 0;
  FModified := 0;
  Clear;
end;

procedure TXEROObject.Assign( ASource : TPersistent);
var
  obj : TXEROObject;
  idx : integer;
begin
  if ASource is ClassType then
  begin
    obj := TXEROObject(ASource);
    for idx := 0 to PropTypeCount(xptUnknown)-1 do
    begin
      case PropInfo(idx).PropType of
        xptString: wStringVal(idx, obj.rStringVal(idx));
        xptCurrency: wCurrencyVal(idx, obj.rCurrencyVal(idx));
        xptDateTime: wDateTimeVal(idx, obj.rDateTimeVal(idx));
        xptEnum,
        xptInteger:  wIntegerVal(idx, obj.rIntegerVal(idx));
        xptBoolean:  wBooleanVal(idx, obj.rbooleanVal(idx));
      end;
    end;
    FModified := obj.FModified;
  end
  else
    inherited Assign(ASource);
end;

class function TXEROObject.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  result.PropType := xptUnknown;
  result.TypeIdx := -1;
end;

class function TXEROObject.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  result.PropType := xptString;
  result.PropName := '';
  result.PropUsage := [];
end;

class function TXEROObject.PropStringAsEnum( AField : Word; const StgVal : String) : integer;
begin
  result := StrToInt(stgVal);
end;

class function TXEROObject.PropEnumAsString( AField : Word; IntVal : Integer) : string;
begin
  result := IntToStr(intval);
end;

class function TXEROObject.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := 0;
end;

class function TXEROObject.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  result := -1;
end;

class function TXEROObject.PropFieldID( AFieldName : String) : integer;
begin
  result := -1;
end;

class function TXEROObject.PropObjectName : string;
begin
  result := 'Unknown';
end;

procedure TXEROObject.wVariantVal( AField : Integer; newVal : Variant);
begin
  with PropTypeIndex(AField) do
  begin
    case PropType of
      xptString:
        wStringVal(AField, CastVarAsString(newVal));
      xptCurrency:
        wCurrencyVal(AField, CastVarAsCurrency(newVal));
      xptDateTime:
        wDateTimeVal(AField, CastVarAsDateTime(newVal));
      xptInteger:
        wIntegerVal(AField, CastVarAsInt(newVal));
      xptBoolean:
        wBooleanVal(AField, CastVarAsBoolean(newVal, false, true, true));
      xptEnum:
        if VarIsInteger(newVal) then
          wIntegerVal(AField, CastVarAsInt(newVal))
        else
          wIntegerVal(AField, PropStringAsEnum(Afield,CastVarAsString(newVal)));
    end;
  end;
end;

function TXEROObject.rVariantVal(AField : Integer) : Variant;
begin
  with PropTypeIndex(AField) do
  begin
    case PropType of
      xptString:   result := rStringVal(AField);
      xptCurrency: result := rCurrencyVal(AField);
      xptDateTime: result := rDateTimeVal(AField);
      xptInteger:  result := rIntegerVal(AField);
      xptBoolean:  result := rBooleanVal(AField);
      xptEnum:     result := rEnumStgVal(AField);
    else result := '';
    end;
  end;
end;

procedure TXEROObject.wModified(AField : Integer);
begin
  if AField <= 63 then
  begin
    if not rModified(AField) then
      FModified := FModified and (1 shl AField);
  end;
end;

function TXEROObject.rModified(AField : Integer) : boolean;
begin
  result := ((FModified shr AField) and 1) = 1;
end;

procedure TXEROObject.wStringVal( AField : Integer; newVal : String);
begin
  with PropTypeIndex(AField) do
  begin
    if PropType <> xptString then
      wVariantVal(AField, newVal)
    else if newVal <> FValsStg[TypeIdx] then
    begin
      FValsStg[TypeIdx] := newVal;
      wModified(AField);
    end;
  end;
end;

function TXEROObject.rStringVal(AField : Integer) : String;
begin
  with PropTypeIndex(AField) do
  begin
    if PropType <> xptString then
      result := CastVarAsString(rVariantVal(AField))
    else
      result := FValsStg[TypeIdx];
  end;
end;

procedure TXEROObject.wCurrencyVal( AField : Integer; newVal : Currency);
begin
  with PropTypeIndex(AField) do
  begin
    if PropType <> xptCurrency then
      wVariantVal(AField, newVal)
    else if newVal <> FValsCy[TypeIdx] then
    begin
      FValsCy[TypeIdx] := newVal;
      wModified(AField);
    end;
  end;
end;

function TXEROObject.rCurrencyVal(AField : Integer) : Currency;
begin
  with PropTypeIndex(AField) do
  begin
    if PropType <> xptCurrency then
      result := CastVarAsCurrency(rVariantVal(AField))
    else
      result := FValsCy[TypeIdx];
  end;
end;

procedure TXEROObject.wDateTimeVal( AField : Integer; newVal : TDateTime);
begin
  with PropTypeIndex(AField) do
  begin
    if PropType <> xptDateTime then
      wVariantVal(AField, newVal)
    else if newVal <> FValsDT[TypeIdx] then
    begin
      FValsDT[TypeIdx] := newVal;
      wModified(AField);
    end;
  end;
end;

function TXEROObject.rDateTimeVal(AField : Integer) : TDateTime;
begin
  with PropTypeIndex(AField) do
  begin
    if PropType <> xptDateTime then
      result := CastVarAsDateTime(rVariantVal(AField))
    else
      result := FValsDT[TypeIdx];
  end;
end;

procedure TXEROObject.wIntegerVal( AField : Integer; newVal : Integer);
begin
  with PropTypeIndex(AField) do
  begin
    if not (PropType in [xptInteger, xptEnum]) then
      wVariantVal(AField, newVal)
    else if newVal <> FValsInt[TypeIdx] then
    begin
      FValsInt[TypeIdx] := newVal;
      wModified(AField);
    end;
  end;
end;

function TXEROObject.rIntegerVal(AField : Integer) : Integer;
begin
  with PropTypeIndex(AField) do
  begin
    if not (PropType in [xptInteger, xptEnum]) then
      result := CastVarAsInt(rVariantVal(AField))
    else
      result := FValsInt[TypeIdx];
  end;
end;

procedure TXEROObject.wBooleanVal(AField : Integer; NewVal : boolean);
var
  oldVal : boolean;
  divid, rem : word;
  wrd : Word;
begin
  with PropTypeIndex(AField) do
  begin
    if PropType <> xptBoolean then
      wVariantVal(AField, newVal)
    else
    begin
      divid := AField div 8;
      rem := AField mod 8;
      wrd := FValsBool[divid];
      oldVal := ((wrd shr rem) and 1) = 1;
      if newVal <> oldVal then
      begin
        wrd := wrd or (1 shl rem);
        FValsBool[divid] := wrd;
        wModified(AField);
      end;
    end;
  end;
end;

function TXEROObject.rBooleanVal(AField : Integer) : boolean;
begin
  with PropTypeIndex(AField) do
  begin
    if PropType <> xptBoolean then
      result := CastVarAsBoolean(rVariantVal(AField), false, true, true)
    else
      result := ((FValsBool[AField div 8] shr (AField mod 8)) and 1) = 1;
  end;
end;

procedure TXEROObject.wEnumStgVal(AField : Integer; newVal : String);
begin
  wIntegerVal(AField, PropStringAsEnum(AField, newVal));
end;

function TXEROObject.rEnumStgVal(AField : Integer) : String;
begin
  result := PropEnumAsString(AField, rIntegerVal(AField));
end;

procedure TXEROObject.wXStatus(AField : integer; newVal : TXEROStatus);
begin
  wIntegerVal(AField, ord(newVal));
end;

function TXEROObject.rXStatus(AField : integer) : TXEROStatus;
var
  idx : integer;
begin
  idx := rIntegerVal(AField);
  if (idx < 0) or (idx > ord(high(TXEROStatus))) then
    idx := 0;
  result := TXEROStatus(idx);
end;

function TXEROObject.rValidation: TXEROValidation;
begin
  if not assigned(FValidation) then
    FValidation := TXEROValidation.Create;
  result := FValidation;
end;

// Describe any validation errors
function TXEROObject.DescribeValidation(Identify : boolean; IndentLevel : integer = 0; Errors : TXEROValidationErrors =[xveError, xveWarning]) : String;
var
  idx : integer;
  haserrors : TXEROValidationErrors;
  headIndent,
  indent : String;
  procedure AppendStrings( Msgs : TStrings);
  var
    msg : string;
  begin
    for msg in msgs do
      result := result + #13#10 + indent + msg;
  end;
begin
  haserrors := Errors * DoCheckValidation;

  if haserrors = []  then
    result := ''
  else
  begin
    indent := '';
    for idx := 1 to IndentLevel do
      indent := indent + '  ';
    headIndent := indent;
    indent := indent + '  ';
    Result := '';
    if xveError in hasErrors then
    begin
      result := result +headindent+ 'Errors';
      if Identify then
      begin
        if IsNew then
          result := result + ' for new'
        else
          result := result + ' for '+GetReferenceID;
      end;
      AppendStrings(FValidation.Errors);
    end;
    if xveWarning in hasErrors then
    begin
      result := result + headIndent+ 'Warnings';
      if Identify then
      begin
        if IsNew then
          result := result + ' for new'
        else
          result := result + ' for '+GetReferenceID;
      end;
      AppendStrings(FValidation.Warnings);
    end;
  end;
end;

function TXEROObject.DoCheckValidation : TXEROValidationErrors;
begin
  result := [];
  if assigned(FValidation) then
  begin
    if FValidation.Errors.Count > 0 then
      Include(result, xveError);
    if FValidation.Warnings.Count >0 then
      Include(result, xveWarning);
  end;
end;

procedure TXEROObject.wIsNew(NewVal: Boolean);
begin
  if newVal <> FIsNew then
  begin
    FIsNew := newVal;
  end;
end;

function TXEROObject.SetField(const fieldName : String; newVal : TValue) : boolean;
var
  fieldID : integer;
begin
  result := false;
  fieldID := PropFieldID(fieldName);
  if (fieldID >= 0) then
  begin
    case PropInfo(fieldID).PropType of
      xptEnum:     wEnumStgVal(fieldID, newVal.AsString);
      xptInteger:  wIntegerVal(fieldID, newVal.AsInteger);
      xptString:   wStringVal(fieldID, newVal.AsString);
      xptCurrency: wCurrencyVal(fieldId, newVal.AsCurrency);
      xptDateTime: wDateTimeVal(fieldID, ConvertJSONDate(newVal.AsString));
      xptBoolean:  wBooleanVal(fieldID, newVal.AsBoolean);
    end;
  end;

end;

function TXEROObject.HasValidation : boolean;
begin
  result := DoCheckValidation <> [];
end;

function TXEROObject.HasValidationWarnings : boolean;
begin
  result := xveWarning in DoCheckValidation;
end;

function TXEROObject.HasValidationErrors : boolean;
begin
  result := xveError in DoCheckValidation;
end;

function TXEROObject.CanOutField( Field : integer; mode : TXEROSerialiseOutputMode) : boolean;
begin
  result := true;
end;

// Output contained object fields
function TXEROObject.OutObjectField( Field : Integer; Writer : TXEROObjectWriter; mode: TXEROSerialiseOutputMode) :boolean;
begin
  result := true;
end;

function TXEROObject.GetObject(field : Integer) :TXEROObject;
begin
  result := nil;
end;

// Get at list field
function TXEROObject.GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase;
begin
  result := nil;
end;

// Output fields to a generic writer.
function TXEROObject.OutFields( writer : TXEROObjectWriter; mode : TXEROSerialiseOutputMode) : boolean;
var
  strval : String;
  idx : integer;
  display : boolean;
  obj : TXEROObject;
  list : TXEROObjectListBase;
  dtVal : TDateTime;
begin
  result := true;
  for idx := 0 to PropTypeCount(xptUnknown)-1 do
  begin
    with PropInfo(idx) do
    begin
      case mode of
        xsomUpdate:
          begin
            if IsNew then
            begin
              if xpuReqNew in PropUsage then
                display := true
              else
                display := (xpuNew in PropUsage);
            end
            else
            begin
              if xpuReqUpdate in PropUsage then
                display := true
              else if not (xpuUpdate in PropUsage) then
                display := false
              else
              begin
                display := rModified(idx);
              end;
            end;
            if display and (xpuSkipBlank in PropUsage) then
            begin
              case PropType of
                xptString:   display := not IsEmptyString(rStringVal(idx));
                xptEnum:     display := not IsEmptyString(rEnumStgVal(idx));
                xptDateTime: display := rDateTimeVal(idx) <> 0;
              end;
            end;
          end;
      else
        display := true;
      end;
      if display then
      begin
        if (not (xpuConditional in Propusage)) or CanOutField(idx, mode) then
        begin
          case PropType of
            xptString:   writer.WriteProperty(PropName, rStringVal(idx));
            xptCurrency: writer.WriteProperty(PropName, rCurrencyVal(idx));
            xptEnum:     writer.WriteProperty(PropName, rEnumStgVal(idx));
            xptInteger:  writer.WriteProperty(PropName, rIntegerVal(idx));
            xptDateTime:
              begin
                dtVal := rDateTimeVal(idx);
                if dtVal = 0 then
                  writer.WritePropertyNull(PropName)
                else
                  writer.WriteProperty(PropName, dtVal);
              end;
            xptBoolean:  writer.WriteProperty(PropName, rBooleanVal(idx));
            xptObject:
              begin
                obj := GetObject(idx);
                if assigned(obj) then
                  obj.OutObject(writer, PropName, xpmNamed, mode)
                else
                  OutObjectField(idx, Writer, mode);
              end;
            xptList:
              begin
                list := GetListObject(idx, xfamRead);
                if assigned(list) then
                  list.OutObject(writer, PropName, xpmNamed, mode)
                else
                  OutObjectField(idx, Writer, mode);
              end;
          end;
        end;
      end;
    end;
  end;
  if mode = xsomDisplay then
  begin
    if HasValidation then
    begin
      if HasValidationErrors then
      begin
        Writer.WriteStartArray('ValidationErrors', xpmNamed);
        for strval in Validation.Errors do
          writer.WriteProperty('Message', strVal, xpmAnon);
        Writer.WriteEndArray('ValidationErrors', xpmNamed);
      end;
      if HasValidationWarnings then
      begin
        writer.WriteStartArray('Warnings', xpmNamed);
        for strval in Validation.Warnings do
          writer.WriteProperty('Message', strVal, xpmAnon);
        Writer.WriteEndArray('Warnings', xpmNamed);
      end;
    end;
  end;
end;

procedure TXEROObject.ResetModified;
begin
  FModified := 0;
end;

procedure TXEROObject.Clear;
var
  idx : integer;
begin
  for idx := 0 to PropTypeCount(xptUnknown)-1 do
  begin
    with PropInfo(idx) do
    begin
      if PropDefault = '' then
      begin
        case PropType of
          xptString: wStringVal(idx, '');
          xptCurrency: wCurrencyVal(idx, 0);
          xptDateTime: wDateTimeVal(idx, 0);
          xptEnum,
          xptInteger:  wIntegerVal(idx, 0);
          xptBoolean:  wBooleanVal(idx, false);
        end;
      end
      else
        wVariantVal(idx, PropDefault);
    end;
  end;
  ResetModified;
  wIsNew(true);
end;

// public definitions
//
function TXEROObject.GetReferenceID : String;
var
  fieldID : integer;
begin
  fieldID := PropSpecialFieldID(xsfUID);
  if fieldID < 0 then
    result := ''
  else
    result := rStringVal(fieldID);
end;

function TXEROObject.GetReferenceName : String;
var
  fieldID : integer;
begin
  fieldID := PropSpecialFieldID(xsfUID);
  if fieldID < 0 then
    result := ''
  else
    result := PropInfo(fieldID).PropName;
end;

function TXEROObject.IsNullReference : Boolean;
begin
  result := false;
end;

function TXEROObject.OutObject( writer : TXEROObjectWriter; APropName : String; propMode : TXEROPropertyMode; mode : TXEROSerialiseOutputMode) : boolean;
begin
  writer.WriteStartObject( APropName, propMode);
  OutFields(writer, mode);
  Writer.WriteEndObject(APropName, propmode);
  result := true;
end;


function TXEROObject.Serialise( textStream : TTextWriter; AOutType : TXEROOutputType; mode : TXEROSerialiseOutputMode) : boolean;
var
  writer : TXEROObjectWriter;
begin
  writer := CreateWriter(mode = xsomDisplay, textStream, AOutType);
  try
    if not assigned(Writer) then
      result := false
    else
      result := OutObject(writer, PropObjectName, xpmAnon, mode);
  finally
    writer.Free;
  end;
end;

type
TXEROValidationLoader = class(TJSONSAXNestableHandler)
protected
  FStrings : TStrings;
public
  constructor Create(aName : String; Strings : TStrings);
  // Called when a value is encountered without a name pair
  procedure MyValue( const FieldValue : TValue); override;
  procedure MyPair(const FieldName : String; const FieldValue : TValue); override;
  procedure StartChildObject; override;
end;

TXEROValidationMessageLoader = class(TJSONSAXNestableHandler)
protected
  FStrings : TStrings;
public
  constructor Create(aName : String; Strings : TStrings);
  procedure MyPair(const FieldName : String; const FieldValue : TValue); override;
end;

constructor TXEROValidationLoader.Create(aName : String; Strings : TStrings);
begin
  inherited Create(aName);
  FStrings := Strings;
end;

// Called when a value is encountered without a name pair
procedure TXEROValidationLoader.MyValue( const FieldValue : TValue);
begin
  FStrings.Add(fieldValue.ToString);
end;

procedure TXEROValidationLoader.MyPair(const FieldName : String; const FieldValue : TValue);
begin
  case indexText(fieldname, ['Message']) of
    0: FStrings.Add(FieldValue.ToString);
  else
    inherited MyPair(FieldName, FieldValue);
  end;
end;

procedure TXEROValidationLoader.StartChildObject;
begin
  SetChildElement(TXEROValidationMessageLoader.Create('', FStrings));
end;

// TXEROValidationMessageLoader
//
constructor TXEROValidationMessageLoader.Create(aName : String; Strings : TStrings);
begin
  inherited Create(aName);
  FStrings := Strings;
end;


procedure TXEROValidationMessageLoader.MyPair(const FieldName : String; const FieldValue : TValue);
begin
  case indexText(fieldname, ['Message']) of
    0: FStrings.Add(FieldValue.ToString);
  else
    inherited MyPair(FieldName, FieldValue);
  end;
end;

// TXEROLoaderGenericBase
//

// protected definitions

procedure TXEROLoaderGenericBase.ClearCur;
begin
  if FOwnsCur then
    FreeAndNil(FCur)
  else
    FCur := nil;
  FOwnsCur := false;
end;

// public definitions

// Create a loader for standard list-mode
constructor TXEROLoaderGenericBase.Create(AName : String; AList : TXEROObjectListBase);
begin
  FList := AList;
  inherited Create(AName);
end;

// Create a loader for a single responses.
constructor TXEROLoaderGenericBase.Create(AName : String; AItem : TXEROObject);
begin
  FCur := AItem;
  FOwnsCur := false;
  inherited Create(AName);
end;

Procedure TXEROLoaderGenericBase.BeforeDestruction;
begin
  ClearCur;
  inherited;
end;

// Handle Creating  new item to add to the list.
procedure TXEROLoaderGenericBase.StartItem;
begin
  ClearCur;
  if assigned(FList) then
  begin
    FCur := FList.CreateListObject;
    FOwnsCur := true;
  end;
end;

// Add a new item to the list.
procedure TXEROLoaderGenericBase.FinishItem;
begin
  if not assigned(FList) then
    ClearCur
  else if assigned(FCur) then
  begin
    FList.AddXEROObject(FCur);
    FCur := nil;
    FOwnsCur := false;
  end;
end;

// Handle a value pair
procedure TXEROLoaderGenericBase.MyPair(const FieldName : String; const FieldValue : TValue);
begin
  if not assigned(FCur)
  or not FCur.SetField(FieldName, FieldValue) then
  begin
    inherited MyPair(fieldName, fieldValue)
  end;
end;

{ Start a child array element.  This needs to be overridden to call
SetChildElement with the appropriate handler for the section.
}
procedure TXEROLoaderGenericBase.StartChildArrayElement( const aName: String);
var
  fieldID : integer;
  listObj : TXEROObjectListBase;
begin
  if not assigned(FCur) then
    inherited StartChildArrayElement(aName)
  else
  begin
    case IndexText(aName, ['ValidationErrors', 'Warnings']) of
      0: SetChildElement(TXEROValidationLoader.Create(aName, FCur.Validation.Errors));
      1: SetChildElement(TXEROValidationLoader.Create(aName, FCur.Validation.Warnings));
    else
      fieldId := FCur.PropFieldID(aName);
      if (fieldID >= 0) and (FCur.PropInfo(fieldID).PropType <> xptList) then
        inherited StartChildArrayElement(aName)
      else
      begin
        listObj := FCur.GetListObject(fieldID, xfamWrite);
        if not assigned(listObj) then
          inherited StartChildArrayElement(aName)
        else
          SetChildElement(listObj.GetListLoader);
      end;
    end;
  end;
end;

// public definitions
constructor TXEROLoaderGeneric<ITEM>.Create(AName : String; AList : TXEROObjectList<ITEM>);
begin
  inherited Create(AName, AList);
end;

constructor TXEROLoaderGeneric<ITEM>.Create(AName : String; AItem : ITEM);
begin
  inherited Create(AName, AItem);
end;

function TXEROLoaderGeneric<ITEM>.GetCur: ITEM;
begin
  result := ITEM(FCur);
end;

// TXEROOrder
//
class function TXEROOrder.None: TXEROOrder;
begin
  result.FOrders := nil;
end;

function TXEROOrder.IsEmpty : boolean;
begin
  result := Length(FOrders) = 0;
end;

constructor TXEROOrder.Order(AFieldName : String; ADirection : TXEROSortDirection);
begin
  SetLength(FOrders, 1);
  with FOrders[0] do
  begin
    FieldName := AFieldName;
    Direction := ADirection;
  end;
end;

function TXEROOrder.Add(AFieldName : String; ADirection : TXEROSortDirection) : TXEROOrder;
begin
  SetLength(FOrders, Length(FOrders)+1);
  with FOrders[high(Forders)] do
  begin
    FieldName := AFieldName;
    Direction := ADirection;
  end;
  result := self;
end;

function TXEROOrder.AsString : String;
var
  idx : integer;
  item : String;
begin
  result := '';
  for idx := low(FOrders) to high(FOrders) do
  begin
    with FOrders[idx] do
    begin
      item := FieldName;
      if Direction = xsdDescending then
        item := item + ' DESC';
    end;
    if result = '' then
      result := item
    else
      result := result + ', '+item;
  end;
end;

function TXEROConnect.BuildURI( const APath, ADoc : String;const AQuery : String; AOrder : TXEROOrder;  AActionType : THttpActionType; Page : Integer) : String;
var
  Params : String;
begin
  params := '';
  if not IsEmptyString(AQuery) then
    AddHTMLParam(Params, 'where',AQuery);
  if not AOrder.IsEmpty then
    AddHTMLParam(params, 'order', AOrder.AsString);
  if Page >= 0 then
    AddHTMLParam(params, 'page', IntToStr(page));
  result := BuildURI(APath, ADoc, Params, AActionType);
end;

function TXEROConnect.BuildURI( const APath, ADoc, AParams  : String; AActionType : THttpActionType = hatGet) : String;
var
  params,
  path : String;
  uri : TIDURI;
begin
  uri := TIDURI.Create;
  try
    uri.Protocol := 'https';
    uri.Host := 'api.xero.com';
    uri.Port := '443';
    path := '/api.xro/2.0';
    if APath <> '' then
      path := IncludeTrailingURIDelimeter( path ) + APath;

    uri.Path := TIDURI.PathEncode(IncludeTrailingURIDelimeter(path));
    uri.Document := TIDURI.PathEncode(ADoc);
    params := AParams;
    uri.Params := params;
    result := uri.URI;
  finally
    uri.Free;
  end;
end;


constructor TXEROListLoader.Create(const AName : String; AOBjectHandler : TXEROObjectLoaderBase; AOwns : boolean = true);
begin
  FObjectHandler := AOBjectHandler;
  FOwnsHandler := AOwns;
  inherited Create(AName);
end;

Procedure TXEROListLoader.BeforeDestruction;
begin
  inherited;
  if FOwnsHandler then
    FreeAndNil(FObjectHandler)
  else
    FObjectHandler := nil;

end;

procedure TXEROListLoader.StartChildObject;
begin
  if assigned(FObjectHandler) then
    FObjectHandler.StartItem;
  //
  if not assigned(FObjectHandler) then
    inherited StartChildObject
  else
    SetChildElement(FobjectHandler, false);

end;

procedure TXEROListLoader.EndingChildObject;
begin
  if assigned(FObjectHandler) then
    FObjectHandler.FinishItem;
  inherited EndingChildObject;
end;

type
  { Internal loader for a list of one.
  }
  TXEROSingleResultLoader = class(TJSONSAXNestableHandler)
  protected
    type TXEROSingleResultState = (xssInit, xssFirstStart, xssFirstEnd, xssOtherStart, xssOtherEnd, xssErrorStart, xssErrorEnd);
  protected
    FObjectHandler : TJSONSAXNestableHandler;
    FOwnsHandler : Boolean;
    FSingleResultState : TXEROSingleResultState;
    function rLoaded: boolean; inline;
    function rSingleLoaded: boolean;
  public
    constructor Create(const AName : String; AOBjectHandler : TJSONSAXNestableHandler; AOwns : boolean = true);

    //: Return true if an item was loaded.
    property Loaded : boolean read rLoaded;
    //: return true if only 1 single item was found and loaded.
    property SingleLoaded : boolean read rSingleLoaded;

    Procedure BeforeDestruction; override;
    procedure StartChildObject; override;
    procedure EndingChildObject; override;
  end;

constructor TXEROSingleResultLoader.Create(const AName : String;AOBjectHandler : TJSONSAXNestableHandler; AOwns : boolean = true);
begin
  FObjectHandler := AOBjectHandler;
  FOwnsHandler := AOwns;
  FSingleResultState := xssInit;
  inherited Create(AName);
end;

Procedure TXEROSingleResultLoader.BeforeDestruction;
begin
  inherited;
  if FOwnsHandler then
    FreeAndNil(FObjectHandler)
  else
    FObjectHandler := nil;
end;

procedure TXEROSingleResultLoader.StartChildObject;
begin
  case FSingleResultState of
    xssInit:
      if  assigned(FObjectHandler) then
      begin
        FSingleResultState := xssFirstStart;
        SetChildElement(FobjectHandler, false);
      end
      else
      begin
        inherited StartChildObject;
        FSingleResultState := xssErrorStart;
      end;
    xssFirstEnd:
      begin
        inherited StartChildObject;
        FSingleResultState := xssOtherStart;
      end;
  else
    inherited StartChildObject;
  FSingleResultState := xssErrorStart;
  end;

end;

procedure TXEROSingleResultLoader.EndingChildObject;
begin
  case FSingleResultState of
    xssFirstStart:
  FSingleResultState := xssFirstEnd;
    xssOtherStart:
    FSingleResultState := xssOtherEnd;
  else
    FSingleResultState := xssErrorEnd;
  end;
  inherited EndingChildObject;
end;

function TXEROSingleResultLoader.rLoaded: boolean;
begin
  result := FSingleResultState in [xssFirstEnd, xssOtherStart, xssOtherEnd];
end;

function TXEROSingleResultLoader.rSingleLoaded: boolean;
begin
  result := FSingleResultState = xssFirstEnd;
end;
type
  TXEROErrorResponse = record
    ErrorNumber: Integer;
    TypeStr : String;
    ErrorMessage : String;
    ValidationErrors : TArray<String>;
  end;
  PXEROErrorResponse = ^TXEROErrorResponse;

  TXEROErrorResponseLoader = class(TJSONSAXNestableHandler)
  protected
    FPResponse : PXEROErrorResponse;
    FListHandler : TJSONSAXNestableHandler;
    FOwnsHandler : Boolean;
  public
    constructor Create(AName : String; pResponse : PXEROErrorResponse; AListHandler : TJSONSAXNestableHandler=nil; AOwns : boolean = true);

    Procedure BeforeDestruction; override;

    // Called when normal pair encountered
    procedure MyPair(const FieldName : String; const FieldValue : TValue); override;
    procedure StartChildArrayElement( const aName: String); override;
  end;

  TXEROValidationErrorsLoader = class(TXEROErrorResponseLoader)
  public
    // Called when normal pair encountered
    procedure MyPair(const FieldName : String; const FieldValue : TValue); override;
    procedure StartChildArrayElement( const aName: String); override;
    //: Called when a value is encountered without a name pair
    procedure MyValue( const FieldValue : TValue); override;
  end;

constructor TXEROErrorResponseLoader.Create(AName : String; pResponse : PXEROErrorResponse; AListHandler : TJSONSAXNestableHandler=nil; AOwns : boolean = true);
begin
  FPResponse := pResponse;
  FListHandler := AListHandler;
  FOwnsHandler := AOwns;
  inherited Create(AName);
end;

Procedure TXEROErrorResponseLoader.BeforeDestruction;
begin
  inherited;
end;

// Called when normal pair encountered
procedure TXEROErrorResponseLoader.MyPair(const FieldName : String; const FieldValue : TValue);
begin
  case IndexText(fieldname, ['ErrorNumber', 'Type', 'Message' ] ) of
    0: FPResponse^.ErrorNumber := FieldValue.AsInteger;
    1: FPResponse^.TypeStr := FieldValue.AsString;
    2: FPResponse^.ErrorMessage := FieldValue.AsString;
  else
    inherited MyPair(FieldName, FieldValue);
  end;
end;

procedure TXEROErrorResponseLoader.StartChildArrayElement( const aName: String);
begin
  case IndexText(aName, ['ValidationErrors', 'Elements']) of
    0: SetChildElement(TXEROValidationErrorsLoader.Create(aName, FPResponse));
    1:
      if Assigned(FListHandler) then
        SetChildElement(FListHandler, false)
      else
        inherited StartChildArrayElement(aName);
  else
    inherited StartChildArrayElement(aName);
  end;
end;

procedure TXEROValidationErrorsLoader.MyPair(const FieldName : String; const FieldValue : TValue);
begin
end;

procedure TXEROValidationErrorsLoader.StartChildArrayElement( const aName: String);
begin
  SetChildElement(TJSONSAXNestableHandler.Create(aName));
end;

// Called when a value is encountered without a name pair
procedure TXEROValidationErrorsLoader.MyValue( const FieldValue : TValue);
begin
  SetLength(FPResponse^.ValidationErrors, Length(FPResponse^.ValidationErrors)+1);
  FPResponse^.ValidationErrors[high(FPResponse^.ValidationErrors)] := FieldValue.AsString;
end;

type
  // Parsed response from XERO
  TXEROResponse = record
    Id: String;
    Status: String;
    DateTimeUTC : TDateTime;
  end;
  PXEROResponse = ^TXEROResponse;

  // Handler to parse the main response from XERO.
  // Passes off the list element to a list handler.
  TXEROResponseLoader = class(TJSONSAXNestableHandler)
  protected
    FListName : String;
    FListHandler : TJSONSAXNestableHandler;
    FOwnsHandler : Boolean;
    FPResponse : PXEROResponse;
  public
    constructor Create(AName, AListName : String; pResponse : PXEROResponse; AListHandler : TJSONSAXNestableHandler; AOwns : boolean = true);

    Procedure BeforeDestruction; override;

    // Called when normal pair encountered
    procedure MyPair(const FieldName : String; const FieldValue : TValue); override;
    { Start a child array element.  This needs to be overridden to call
       SetChildElement with the appropriate handler for the section.
    }
    procedure StartChildArrayElement( const aName: String); override;
  end;

constructor TXEROResponseLoader.Create(AName, AListName : String; pResponse : PXEROResponse; AListHandler : TJSONSAXNestableHandler; AOwns : boolean = true);
begin
  FListName  := AListName;
  FListHandler := AListHandler;
  FOwnsHandler := AOwns;
  FPResponse := pResponse;
  inherited Create(Aname);
end;

Procedure TXEROResponseLoader.BeforeDestruction;
begin
  inherited;
  if FOwnsHandler then
    FListHandler.Free;
  FListHandler := nil;
end;

// Called when normal pair encountered
procedure TXEROResponseLoader.MyPair(const FieldName : String; const FieldValue : TValue);
begin
  case IndexText(FieldName, ['Id', 'Status', 'DateTimeUTC']) of
    0: FPResponse^.Id := FieldValue.AsString;
    1: FPResponse^.Status := FieldValue.AsString;
    2: FPResponse^.DateTimeUTC := ConvertJSONDate(FieldValue.AsString);
  else
    inherited MyPair(FieldName, FieldValue);
  end;
end;

{ Start a child array element.  This needs to be overridden to call
SetChildElement with the appropriate handler for the section.
}
procedure TXEROResponseLoader.StartChildArrayElement( const aName: String);
begin
  if Assigned(FListHandler) and (CompareText(aName, FListName) = 0) then
    SetChildElement(FListHandler, false)
  else
    inherited StartChildArrayElement(aName);
end;

type
  TXEROOrganisationList = class(TXEROObjectList<TXEROOrganisation>)
  public
    class function PropListName : String; override;
  protected
    function GetItemLoader : TXEROObjectLoaderBase; override;
  end;

type
TXEROOrganisationLoader = TXEROLoaderGenericBase;

TXEROManualJournalLoader = TXEROLoaderGenericBase;

TXEROAccountLoader = TXEROLoaderGenericBase;

TXEROTrackingCategoryLoader = TXEROLoaderGenericBase;

TXEROTrackingOptionLoader = TXEROLoaderGenericBase;

function TXEROConnect.GetLoadList( const AURI : String; AParams : TStrings;
    const AColnName : String; ALoader : TJSONSAXNestableHandler) : boolean;
var
  respStream : TStream;
  ResponseCode : integer;
  ErrorDetail : string;
  respLoad : TXEROResponseLoader;
  parser : TJSONEventReader;
  resp : TXEROResponse;
begin
  respStream := TMemoryStream.Create;
  try
    result := Get(AURI, AParams, respStream, ResponseCode, ErrorDetail, 0, rtJSON);
    if not result then
    begin
      Raise XEROException.Create(responseCode, ErrorDetail);
    end
    else
    begin
      // Parse the response. Optionally into a list loader.

      respStream.Position := 0;
      // ALoader can be nil.

      respLoad := TXEROResponseLoader.Create('',AColnName, @resp, ALoader, False);
      try
        parser := TJSONEventReader.Create(respLoad);
        try
          result := parser.ParseStream(respStream);
        finally
          parser.Free;
        end;
      finally
        respLoad.Free;
      end;
    end;

  finally
    respStream.Free;
  end;
end;

// Load a single object returned.
// Raises an exception if an error happens.
function TXEROConnect.GetLoadSingleResult( const AURI : String; AParams : TStrings;
  const AColnName : String; ALoader : TJSONSAXNestableHandler) : boolean;
var
  respStream : TStream;
  ResponseCode : integer;
  ErrorDetail : string;
  snglLoader : TXEROSingleResultLoader;
  respLoad : TXEROResponseLoader;
  parser : TJSONEventReader;
  resp : TXEROResponse;
begin
  respStream := TMemoryStream.Create;
  try
    result := Get(AURI, AParams, respStream, ResponseCode, ErrorDetail, 0, rtJSON);
    if not result then
    begin
      Raise XEROException.Create(responseCode, ErrorDetail);
    end
    else
    begin
      // Parse the response. Optionally into a SingleResult loader.

      respStream.Position := 0;
      // ALoader can be nil.

      // Use a SingleResult loader
      if assigned(ALoader) then
        snglLoader := TXEROSingleResultLoader.Create(AColnName,ALoader, false)
      else
        snglLoader := nil;
      respLoad := TXEROResponseLoader.Create('',AColnName, @resp, snglLoader, false);
      try
        parser := TJSONEventReader.Create(respLoad);
        try
          result := parser.ParseStream(respStream);
        finally
          parser.Free;
        end;
        if result then
          result := snglLoader.Loaded;
      finally
        respLoad.Free;
        snglLoader.Free;
      end;
    end;

  finally
    respStream.Free;
  end;
end;

function TXEROConnect.HandleAutoLoad( APropertyCode : String; AReferenceID : String; Loader : TXEROObjectLoaderBase) : boolean;
begin
  case IndexText(APropertyCode,['AccountCode', 'ManualJournalLines']) of
    0:
    begin
      if AReferenceID = '' then
        result := false
      else
      begin
        result := GetLoadSingleResult(BuildURI('accounts', AReferenceID, '', hatGet), nil, 'Accounts', loader);
      end;
    end;
    1:
    begin
      result := false;
    end;
  else
    result := false;
  end;

end;

function TXEROConnect.GetOrganisationProperties(properties : TXEROOrganisation) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := TXEROOrganisationLoader.Create('Organisation', properties);
  try
    properties.Clear;
    result := GetLoadSingleResult(BuildURI('', 'Organisations', '', hatGet), nil, 'Organisations', loader);
    if result then
    begin
      properties.ResetModified;
      properties.IsNew := false;
    end;
  finally
    loader.Free;
  end;
end;

function TXEROConnect.GetAccounts( accountList : TXEROAccountList; Const AQuery : String) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := accountList.GetListLoader;
  try
    result := GetLoadList(BuildURI('','accounts', ''), nil, 'Accounts', loader);
    if result then
    begin
      accountList.ResetModified;
      accountList.SetIsNew(false);
    end;
  finally
    loader.Free;
  end;
end;

// Fetches a single account by UID/Code
function TXEROConnect.GetAccount( account : TXEROAccount; Const AIdent : String) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := TXEROAccountLoader.Create('Account',account);
  try
    if AIdent = '' then
      result := false
    else
    begin
      account.Clear;
      result := GetLoadSingleResult(BuildURI('accounts', AIdent, '', hatGet), nil, 'Accounts', loader);
      if result then
      begin
        account.ResetModified;
        account.IsNew := false;
      end;
    end;
  finally
    loader.Free;
  end;
end;

// Store a single account
function TXEROConnect.StoreAccount(account : TXEROAccount) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := TXEROAccountLoader.Create('Account', account);
  try
    result := Store(BuildURI('', 'accounts','', hatSend),
      'Accounts', 'Account', account, xsomUpdate, loader);
  finally
    loader.Free;
  end;
end;

// Store multiple accounts
function TXEROConnect.StoreAccounts(accountList, ResponseList : TXEROAccountList) : boolean;
begin
  if accountList.Count = 0 then
    result := true
  else
    result := StoreList(BuildURI('','accounts','',hatSend),
      'Accounts', 'Account', accountList,ResponseList, xsomUpdate);
end;

// Fetches a list  of Manual Journals
function TXEROConnect.GetManualJournals( journalList : TXEROManualJournalList; const AQuery : String; const AOrder : TXEROOrder; APageNo : integer) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := journalList.GetListLoader;
  try
    result := GetLoadList(BuildURI('','manualjournals', AQuery, AOrder, hatGet, APageNo), nil, 'ManualJournals', loader);
    if result then
    begin
      if result then
      begin
        journalList.ResetModified;
        journalList.SetIsNew(false);
      end;
    end;
  finally
    loader.Free;
  end;
end;

function TXEROConnect.GetManualJournals(journalList: TXEROManualJournalList;
  const AQuery: String): boolean;
begin
  result := GetManualJournals( journalList, AQuery,TXEROOrder.None, -1);
end;

function TXEROConnect.GetManualJournal( manualJournal : TXEROManualJournal; const AUID : String) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := TXEROManualJournalLoader.Create('ManualJournal',manualJournal);
  try
    if AUID = '' then
      result := false
    else
    begin
      manualJournal.Clear;
      result := GetLoadSingleResult(BuildURI('manualjournals',AUID, '', hatGet), nil, 'ManualJournals', loader);
      if result then
      begin
        manualJournal.ResetModified;
        manualJournal.IsNew := false;
      end;
    end;
  finally
    loader.Free;
  end;
end;

// Refresh (and load all) a manual journal object
function TXEROConnect.Refresh(manualJournal : TXEROManualJournal) : boolean;
begin
  result := GetManualJournal(manualJournal, manualJournal.ManualJournalID);
end;

function TXEROConnect.StoreManualJournal(ManualJournal : TXEROManualJournal) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := TXEROManualJournalLoader.Create('ManualJournal', manualJournal);
  try
    result := Store(BuildURI('', 'manualjournals','', hatSend),
      'ManualJournals', 'ManualJournal', manualJournal, xsomUpdate, loader);
  finally
    loader.Free;
  end;
end;

// Store a list  of manual journals.
function TXEROConnect.StoreManualJournals(ManualJournalList, ResponseJournals : TXEROManualJournalList) : boolean;
begin
  if ManualJournalList.Count = 0 then
    result := true
  else
    result := StoreList(BuildURI('','manualjournals','',hatSend),
      'ManualJournals', 'ManualJournal', ManualJournalList,ResponseJournals, xsomUpdate);
end;

// Load tracking categories
function TXEROConnect.GetTrackingCategories(categoryList : TXEROTrackingCategoryList; const AQuery : String = '') : boolean;
begin
  result := GetTrackingCategories(categoryList, AQuery, TXEROOrder.None, -1);
end;

function TXEROConnect.GetTrackingCategories(categoryList : TXEROTrackingCategoryList; const AQuery : String; const AOrder : TXEROOrder; APageNo : integer = -1) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := categoryList.GetListLoader;
  try
    result := GetLoadList(BuildURI('','trackingcategories', AQuery, AOrder, hatGet, APageNo), nil, 'TrackingCategories', loader);
    if result then
    begin
      if result then
      begin
        categoryList.ResetModified;
        categoryList.SetIsNew(false);
      end;
    end;
  finally
    loader.Free;
  end;
end;

function TXEROConnect.GetTrackingCategory(categoryItem : TXEROTrackingCategory; Const AUID : String) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := TXEROTrackingCategoryLoader.Create('TrackingCategory',categoryItem);
  try
    if AUID = '' then
      result := false
    else
    begin
      categoryItem.Clear;
      result := GetLoadSingleResult(BuildURI('trackingcategories',AUID, '', hatGet), nil, 'TrackingCategories', loader);
      if result then
      begin
        categoryItem.ResetModified;
        categoryItem.IsNew := false;
      end;
    end;
  finally
    loader.Free;
  end;
end;

// Store a single tracking category
function TXEROConnect.StoreTrackingCategory( categoryItem : TXEROTrackingCategory) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := TXEROTrackingCategoryLoader.Create('TrackingCategory', categoryItem);
  try
    result := Store(BuildURI('', 'trackingcategories','', hatSend),
    'TrackingCategories', 'TrackingCategory', categoryItem, xsomUpdate, loader);
  finally
    loader.Free;
  end;
end;

// Store list of tracking categories
function TXEROConnect.StoreTrackingCategories( categoryList, responseList : TXEROTrackingCategoryList) : boolean;
begin
  if categoryList.Count = 0 then
    result := true
  else
    result := StoreList(BuildURI('','trackingcategories','',hatSend),
      'TrackingCategories', 'TrackingCategory', categoryList, responselist, xsomUpdate);
end;

// Store an object
function TXEROConnect.Store( const AURL : String; const AColnName, AObjName : String; AXeroObject : TXEROObject; AMode : TXEROSerialiseOutputMode; AObjectLoader : TJSONSAXNestableHandler) : boolean;
var
  SingleResultLoader : TXEROSingleResultLoader;
begin
  SingleResultLoader := TXEROSingleResultLoader.Create(AColnName, AObjectLoader, false);
  try
    result := DoStore(AURL, AColnName, AObjName, AObjName,
          function ( writer : TTextWriter; mode : TXEROSerialiseOutputMode) : boolean
          begin
            result := AXeroObject.Serialise( writer, xotXML, mode);
          end, SingleResultLoader);
  finally
    SingleResultLoader.Free;
  end;
end;

// Store a list of objects.
function TXEROConnect.StoreList( const AURL : String; const AColnName, AObjName : String;
  AXeroListStore, AXeroListResponse : TXEROObjectListBase; AMode : TXEROSerialiseOutputMode) : boolean;
var
  handler : TJSONSAXNestableHandler;
begin
  if assigned(AXeroListResponse) then
    handler := AXeroListResponse.GetListLoader
  else
    handler := nil;
  try
    result := DoStore(AURL, AColnName, AObjName, AColnName,
          function ( writer : TTextWriter; mode : TXEROSerialiseOutputMode) : boolean
          begin
            result := AXeroListStore.Serialise( writer, xotXML, mode);
          end,  handler);
  finally
    handler.Free;
  end;
end;

function TXEROConnect.DoStore( const AURL : String; const AColnName, AObjName, AOuterName : String;
  ASerialise : TXEROSerialise; AResponseListHandler : TJSONSAXNestableHandler) : boolean;
var
  dataStream : TStringStream;
  respStream : TStringStream;
  ResponseCode : integer;
  ErrorDetail : string;
  strWriter: TStreamWriter;
  params : TStrings;

  respLoad : TJSONSAXNestableHandler;
  resp : TXEROResponse;
  errresp : TXEROErrorResponse;
  parser : TJSONEventReader;
  loadedResponse : boolean;
begin
  result := false;

  dataStream := TStringStream.Create;
  respStream := TStringStream.Create;
  try

    strWriter := TStreamWriter.Create(dataStream);
    try

      if ASerialise(strWriter, xsomUpdate) then
      begin
        params := TStringList.Create;
        try
          dataStream.Position := 0;
          params.Values['xml'] := dataStream.DataString;
          result := Post(AURL, params, respStream, ResponseCode, ErrorDetail, rtJSON);
        finally
          params.Free;
        end;

        respLoad := nil;
        try
          respStream.Position := 0;
          if not result then
          begin
            respLoad := TXEROErrorResponseLoader.Create('', @errresp, AResponseListHandler, false);
            parser := TJSONEventReader.Create(respLoad);
            try
              loadedResponse := parser.ParseStream(respStream);
            finally
              parser.Free;
            end;
            if loadedresponse then
              Raise XEROException.Create(errresp.ErrorNumber, errresp.ErrorMessage)
            else
              Raise XEROException.Create(responseCode, ErrorDetail);
          end
          else
          begin
            respLoad := TXEROResponseLoader.Create('',AColnName, @resp, AResponseListHandler, False);
            parser := TJSONEventReader.Create(respLoad);
            try
              loadedResponse := parser.ParseStream(respStream);
            finally
              parser.Free;
            end;

            if loadedresponse then
              Log('Response: ['+resp.ID+'] '+resp.Status)
            else
              Log('Response: '+respStream.DataString);
          end;
        finally
          FreeAndNil(respload);
        end

      end;
    finally
      //writer.Free;
      strWriter.Free;
    end;

  finally
    FreeAndNil(dataStream);
    FreeAndNil(respStream);
  end;

end;

type
  TXEROExternalLinkLoader = TXEROLoaderGenericBase;

// TXEROExternalLink
//

// protected definitions

function TXEROExternalLink.rLinkTypString: String;
begin
  result := CLinkType[FLinkType];
end;

procedure TXEROExternalLink.wLinkTypString(NewVal: String);
var
  idx,
  newLinkType: TXeroLinkType;

begin
  newLinkType := xltNotSet;
  for idx := low(idx) to high(idx) do
  begin
    if CompareText(CLinkType[idx], newVal) = 0 then
    begin
      newLinkType := idx;
      break;
    end;
  end;
  LinkType := newLinkType;
end;

procedure TXEROExternalLink.Clear;
begin
  inherited Clear;
  FLinkType := xltNotSet;
  FURL := '';
end;

procedure TXEROExternalLink.Assign( ASource : TPersistent);
var
  src : TXEROExternalLink;
begin
  if not (ASource is TXEROExternalLink) then
    inherited Assign(ASource)
  else
  begin
    src := TXEROExternalLink(ASource);
    FLinkType := src.LinkType;
    FURL := src.URL;
  end;
end;

class function TXEROExternalLink.PropObjectName : string;
begin
  Result := 'ExternalLink';
end;

function TXEROExternalLinkList.GetItemLoader : TXEROObjectLoaderBase;
begin
  result := TXEROExternalLinkLoader.Create(PropItemName, self);
end;

class function TXEROExternalLinkList.PropListName : String;
begin
  Result := 'ExternalLinks';
end;

// TXEROOrganisation
//

class function TXEROOrganisation.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_OrganisationMap.Map) then
    Raise Exception.CreateFmt('Invalid Fieldid %d', [AField]);
  result := G_OrganisationMap.Map[AField];
end;

class function TXEROOrganisation.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  inherited;
  if AField > high(COrganisationProperties) then
    Raise Exception.CreateFmt('Invalid Fieldid %d', [AField]);
  result := COrganisationProperties[AField];
end;

class function TXEROOrganisation.PropStringAsEnum( AField : Word; const StgVal : String) : integer;
begin
  case AField of
    ord(xofOrganisationType),
    ord(xofOrganisationEntityType): result := ord(XEROOrganisationTypeAsEnum(stgVal));
  else
    result := inherited PropStringAsEnum(AField, StgVal);
  end;
end;

class function TXEROOrganisation.PropEnumAsString( AField : Word; IntVal : Integer) : string;
begin
  case AField of
    ord(xofOrganisationType),
    ord(xofOrganisationEntityType): result := XEROOrganisationTypeAsString(intVal);
  else
    result := inherited PropEnumAsString(AField, IntVal);
  end;
end;

class function TXEROOrganisation.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_OrganisationMap.Count[APropType];
end;

class function TXEROOrganisation.PropFieldID( AFieldName : String) : integer;
begin
  result := G_OrganisationMap.NameToFieldID(AFieldName);
end;

class function TXEROOrganisation.PropObjectName : string;
begin
  result := 'Organisation';
end;

// Get at list field
function TXEROOrganisation.GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase;
begin
  case field of
    ord(xofExternalLinks): result := FExternalLinks;
  else result := inherited GetListObject(field, Access);
  end;
end;

function TXEROOrganisation.rXeroOrganisationType(AField : Integer) : TXeroOrganisationType;
begin
  result := TXeroOrganisationType(rIntegerVal(AField));
end;

procedure TXEROOrganisation.wXeroOrganisationType(AField : Integer; NewVal: TXeroOrganisationType);
begin
  wIntegerVal(AField, ord(newVal));
end;

procedure TXEROOrganisation.AfterConstruction;
begin
  FExternalLinks := TXEROExternalLinkList.Create;
  inherited;

end;

Procedure TXEROOrganisation.BeforeDestruction;
begin
  FreeAndNil(FExternalLinks);
  inherited;
end;

procedure TXEROOrganisation.Clear;
begin
  inherited Clear;
  if assigned(FExternalLinks) then
    FExternalLinks.Clear;
end;


// protected definitions

procedure TXEROOrganisation.wExternalLinks(NewVal: TXEROExternalLinkList);
begin
  if Assigned(newVal) then
    FExternalLinks.clear
  else
    FExternalLinks.Assign(newVal);
end;

function TXEROOrganisationList.GetItemLoader : TXEROObjectLoaderBase;
begin
  result := TXEROOrganisationLoader.Create(PropItemName, self);
end;
class function TXEROOrganisationList.PropListName : String;
begin
  result := 'Organisations';
end;

class function TXEROAccount.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_AccountMap.Map) then
    Raise Exception.CreateFmt('Invalid Account Field %d', [AField]);
  result := G_AccountMap.Map[AField];
end;

class function TXEROAccount.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_AccountMap.Map) then
    Raise Exception.CreateFmt('Invalid Account Field %d', [AField]);
  result := CAccountProperties[AField];
end;

class function TXEROAccount.PropStringAsEnum( AField : Word; const StgVal : String) : integer;
begin
  case AField of
    ord(xafClass): result := ord(XEROAccountClassAsEnum(stgVal));
    ord(xafType):  result := ord(XEROAccountTypeAsEnum(stgVal));
    ord(xafStatus):result := ord(XEROAccountStatusAsEnum(stgVal));
  else
    result := inherited PropStringAsEnum(AField, StgVal);
  end;
end;

class function TXEROAccount.PropEnumAsString( AField : Word; IntVal : Integer) : string;
begin
  case AField of
    ord(xafClass): result := XEROAccountClassAsString(IntVal);
    ord(xafType):  result := XEROAccountTypeAsString(intVal);
    ord(xafStatus):result := XEROAccountStatusAsString(intVal);
  else
    result := inherited PropEnumAsString(AField, IntVal);
  end;
end;

class function TXEROAccount.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_AccountMap.Count[APropType];
end;

class function TXEROAccount.PropFieldID( AFieldName : String) : integer;
begin
  result := G_AccountMap.NameToFieldID(AFieldName);
end;

class function TXEROAccount.PropObjectName : string;
begin
  result := 'Account';
end;

class function TXEROAccount.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  case field of
    xsfUID: result := ord(xafAccountID);
    xsfName: result := ord(xafName);
  else
    result := inherited PropSpecialFieldID(Field);
  end;
end;

function TXEROAccount.rAccountType: TXEROAccountType;
begin
  result := TXEROAccountType(rIntegerVal(ord(xafType)));
end;

procedure TXEROAccount.wAccountType(NewVal: TXEROAccountType);
begin
  wIntegerVal(ord(xafType), ord(newVal));
end;

function TXEROAccount.rStatus: TXEROAccountStatus;
begin
  result := TXEROAccountStatus(rIntegerVal(ord(xafStatus)));
end;

procedure TXEROAccount.wStatus(NewVal: TXEROAccountStatus);
begin
  wIntegerVal(ord(xafStatus), ord(newVal));
end;

function TXEROAccount.rAccountClass: TXEROAccountClass;
begin
  result := TXEROAccountClass(rIntegerVal(ord(xafClass)));
end;

procedure TXEROAccount.wAccountClass(NewVal: TXEROAccountClass);
begin
  wIntegerVal(ord(xafClass), ord(newVal));
end;

procedure TXEROAccount.Clear;
begin
  inherited Clear;
end;

procedure TXEROAccount.Assign( ASource : TPersistent);
begin
  inherited Assign(ASource)
end;

// Can output a conditional field.
function TXEROAccount.CanOutField( Field : integer; mode : TXEROSerialiseOutputMode) : boolean;
begin
  case field of
    ord(xafBankAccountNumber),
    ord(xafBankAccountType),
    ord(xafCurrencyCode):
      result := AccountType = xatBank;
  else
    result := inherited CanOutField(field, mode);
  end;
end;

function TXEROAccountList.GetItemLoader : TXEROObjectLoaderBase;
begin
  result := TXEROAccountLoader.Create(PropItemName, self);
end;

class function TXEROAccountList.PropListName : String;
begin
  result := 'Accounts';
end;

class function TXEROTrackingOption.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_TrackingOptionMap.Map) then
    Raise Exception.CreateFmt('Invalid Tracking Option Field %d', [AField]);
  result := G_TrackingOptionMap.Map[AField];
end;

class function TXEROTrackingOption.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_TrackingOptionMap.Map) then
    Raise Exception.CreateFmt('Invalid Tracking Option Field %d', [AField]);
  result := CTrackingOptionProperties[AField];
end;

class function TXEROTrackingOption.PropStringAsEnum( AField : Word; const StgVal : String) : integer;
begin
  case AField of
    ord(xtofStatus): result := ord(XEROStatusAsEnum(StgVal));
  else result := inherited PropStringAsEnum(AField, StgVal);
  end;
end;

class function TXEROTrackingOption.PropEnumAsString( AField : Word; IntVal : Integer) : string;
begin
  case AField of
    ord(xtofStatus): result := XEROStatusAsString(intVal);
  else result := inherited PropEnumAsString(AField, IntVal);
  end;
end;

class function TXEROTrackingOption.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_TrackingOptionMap.Count[APropType];
end;

class function TXEROTrackingOption.PropFieldID( AFieldName : String) : integer;
begin
  result := G_TrackingOptionMap.NameToFieldID(AFieldName);
end;

class function TXEROTrackingOption.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  case field of
    xsfUID: result := ord(xtofTrackingOptionID);
    xsfName:result := ord(xtofName);
  else      result := inherited PropSpecialFieldID(field);
  end;
end;

class function TXEROTrackingOption.PropObjectName : string;
begin
  result := 'TrackingOption';
end;

class function TXEROTrackingOptionList.PropListName : String;
begin
  result := 'TrackingOptions';
end;

// protected definitions

function TXEROTrackingOptionList.GetItemLoader : TXEROObjectLoaderBase;
begin
  result := TXEROTrackingOptionLoader.Create(PropItemName, self);
end;

// TXEROTrackingCategory
//
//

// public definitions

class function TXEROTrackingCategory.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_TrackingCategoryMap.Map) then
    Raise Exception.CreateFmt('Invalid Tracking Category Field %d', [AField]);
  result := G_TrackingCategoryMap.Map[AField];
end;

class function TXEROTrackingCategory.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_TrackingCategoryMap.Map) then
    Raise Exception.CreateFmt('Invalid Tracking Category Field %d', [AField]);
  result := CTrackingCategoryProperties[AField];
end;

class function TXEROTrackingCategory.PropStringAsEnum( AField : Word; const StgVal : String) : integer;
begin
  case AField of
    ord(xtcfStatus): result := ord(XEROStatusAsEnum(StgVal));
  else result := inherited PropStringAsEnum(AField, StgVal);
  end;
end;

class function TXEROTrackingCategory.PropEnumAsString( AField : Word; IntVal : Integer) : string;
begin
  case AField of
    ord(xtcfStatus):
      begin
        if (intVal < 0) or (intVal > ord(high(TXEROStatus))) then
          intVal := ord(xsNotSet);
        result := CXEROStatusType[TXEROStatus(intVal)];
      end;
  else result := inherited PropEnumAsString(AField, IntVal);
  end;
end;

class function TXEROTrackingCategory.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_TrackingCategoryMap.Count[APropType];
end;

class function TXEROTrackingCategory.PropFieldID( AFieldName : String) : integer;
begin
  result := G_TrackingCategoryMap.NameToFieldID(AFieldName);
end;

class function TXEROTrackingCategory.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  case field of
    xsfUID: result := ord(xtcfTrackingCategoryID);
    xsfName:result := ord(xtcfName);
  else      result := inherited PropSpecialFieldID(field);
  end;
end;

class function TXEROTrackingCategory.PropObjectName : string;
begin
  result := 'TrackingCategory';
end;

procedure TXEROTrackingCategory.wOptions(NewVal: TXEROTrackingOptionList);
begin
  FOptions.Assign(newVal);
end;

// public definitions

procedure TXEROTrackingCategory.AfterConstruction;
begin
  FOptions := TXEROTrackingOptionList.Create;
  inherited;
end;

Procedure TXEROTrackingCategory.BeforeDestruction;
begin
  FreeAndNil(FOptions);
  inherited;
end;

function TXEROTrackingCategory.GetListObject(  Field: Integer; Access : TXEROFieldAccessMode): TXEROObjectListBase;
begin
  case field of
    ord(xtcfOptions): result := FOptions;
  else result := inherited GetListObject(Field, Access);
  end;
end;

// TXEROTrackingCategoryList
//

// public definitions

class function TXEROTrackingCategoryList.PropListName : String;
begin
  result := 'TrackingCategories';
end;

// protected definitions

function TXEROTrackingCategoryList.GetItemLoader : TXEROObjectLoaderBase;
begin
  result := TXEROTrackingCategoryLoader.Create(PropItemName, self);
end;

class function TXEROTrackingCategoryOption.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_TrackingCategoryMap.Map) then
    Raise Exception.CreateFmt('Invalid Tracking Category Option Field %d', [AField]);
  result := G_TrackingCategoryOptionMap.Map[AField];
end;

class function TXEROTrackingCategoryOption.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_TrackingCategoryMap.Map) then
    Raise Exception.CreateFmt('Invalid Tracking Category Option Field %d', [AField]);
  result := CTrackingCategoryOptionProperties[AField];
end;

class function TXEROTrackingCategoryOption.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_TrackingCategoryOptionMap.Count[APropType];
end;

class function TXEROTrackingCategoryOption.PropFieldID( AFieldName : String) : integer;
begin
  result := G_TrackingCategoryOptionMap.NameToFieldID(AFieldName);
end;

class function TXEROTrackingCategoryOption.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  case field of
    xsfName:result := ord(xtofName);
  else      result := inherited PropSpecialFieldID(field);
  end;
end;

class function TXEROTrackingCategoryOption.PropObjectName : string;
begin
  result := 'TrackingCategoryOption';
end;

// Can output a conditional field.
function TXEROTrackingCategoryOption.CanOutField( Field : integer; mode : TXEROSerialiseOutputMode) : boolean;
begin
  if mode = xsomDisplay then
    result := true // output both
  else
  begin
    // Don't output the user viewable options if the UIDs are set
    case field of
      ord(xtcofName):   result := (TrackingCategoryID = '');
      ord(xtcofOption): result := (TrackingOptionID = '');
    else
      result := inherited CanOutField(field, mode);
    end;
  end;
end;

class function TXEROTrackingCategoryOptionList.PropListName : String;
begin
  result := 'Tracking';
end;

// Loader for TXEROManualJournalLine
type
TXEROManualJournalLineLoader = TXEROLoaderGenericBase;

class function TXEROManualJournalLine.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_ManualJournalLineMap.Map) then
    Raise Exception.CreateFmt('Invalid Manual Journal Line Field %d', [AField]);
  result := G_ManualJournalLineMap.Map[AField];
end;

class function TXEROManualJournalLine.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_ManualJournalLineMap.Map) then
    Raise Exception.CreateFmt('Invalid Manual Journal Line Field %d', [AField]);
  result := CManualJournalLineProperties[AField];
end;

class function TXEROManualJournalLine.PropStringAsEnum( AField : Word; const StgVal : String) : integer;
begin
  result := inherited PropStringAsEnum(AField, StgVal);
end;

class function TXEROManualJournalLine.PropEnumAsString( AField : Word; IntVal : Integer) : string;
begin
  result := inherited PropEnumAsString(AField, IntVal);
end;

class function TXEROManualJournalLine.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_ManualJournalLineMap.Count[APropType];
end;

class function TXEROManualJournalLine.PropFieldID( AFieldName : String) : integer;
begin
  result := G_ManualJournalLineMap.NameToFieldID(AFieldName);
end;

class function TXEROManualJournalLine.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  result := inherited PropSpecialFieldID(Field);
end;

class function TXEROManualJournalLine.PropObjectName : string;
begin
  result := 'JournalLine';
end;

procedure TXEROManualJournalLine.wTracking(NewVal: TXEROTrackingCategoryOptionList);
begin
  FTracking.Assign(newVal);
end;

procedure TXEROManualJournalLine.AfterConstruction;
begin
  FTracking:= TXEROTrackingCategoryOptionList.Create;
  inherited;
end;

Procedure TXEROManualJournalLine.BeforeDestruction;
begin
  FreeAndNil(FTracking);
  inherited;
end;

procedure TXEROManualJournalLine.Clear;
begin
  inherited Clear;
  if assigned(FTracking) then
    FTracking.Clear;
end;

function TXEROManualJournalLine.GetListObject(Field: Integer;
  Access: TXEROFieldAccessMode): TXEROObjectListBase;
begin
  case field of
    ord(xmjlTracking): result := FTracking;
  else
    result := inherited GetListObject(Field,Access);
  end;
end;

procedure TXEROManualJournalLine.Assign( ASource : TPersistent);
begin
  inherited Assign(ASource)
end;
// TXEROManualJournalLineList
//

function TXEROManualJournalLineList.GetItemLoader : TXEROObjectLoaderBase;
begin
  result := TXEROManualJournalLineLoader.Create(PropItemName, self);
end;

class function TXEROManualJournalLineList.PropListName : String;
begin
  inherited;
  result := 'JournalLines';
end;

{ XERO Manual Journal.
}

procedure TXEROManualJournal.Clear;
begin
  inherited Clear;
  if assigned(FJournalLines) then
    FJournalLines.Clear;
end;

procedure TXEROManualJournal.wIsNew(NewVal: Boolean);
begin
  if IsNew <> NewVal then
  begin
    inherited wIsNew(newVal);
    if Assigned(FJournalLines) then
      FJournalLines.SetIsNew(newVal);
  end;
end;

function TXEROManualJournal.rStatus: TXEROManualJournalStatus;
begin
  result := TXEROManualJournalStatus(rIntegerVal(ord(xmjfStatus)));
end;

procedure TXEROManualJournal.wStatus(NewVal: TXEROManualJournalStatus);
begin
  wIntegerVal(ord(xmjfStatus), ord(newVal));
end;

procedure TXEROManualJournal.ResetModified;
begin
  inherited ResetModified;
  if Assigned(FJournalLines) then
    FJournalLines.ResetModified;
end;

procedure TXEROManualJournal.Assign( ASource : TPersistent);
var
  src : TXEROManualJournal;
begin
  inherited Assign(ASource);
  if (ASource is TXEROManualJournal) then
  begin
    src := TXEROManualJournal(ASource);

    if not assigned(src.FJournalLines) then
      FreeAndNil(FJournalLines)
    else
    begin
      if not assigned(FJournalLines) then
        FJournalLines := TXEROManualJournalLineList.Create;
      FJournalLines.Assign(src.FJournalLines);
    end;
  end;
end;

class function TXEROManualJournal.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_ManualJournalMap.Map) then
    Raise Exception.CreateFmt('Invalid Manual Journal Field %d', [AField]);
  result := G_ManualJournalMap.Map[AField];
end;

class function TXEROManualJournal.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_ManualJournalMap.Map) then
    Raise Exception.CreateFmt('Invalid Manual Journal Field %d', [AField]);
  result := CManualJournalProperties[AField];
end;

class function TXEROManualJournal.PropStringAsEnum( AField : Word; const StgVal : String) : integer;
var
  idx,
  newStatus : TXEROManualJournalStatus;
begin
  case AField of
    ord(xmjfStatus):
      begin
        newStatus := xmjsNotSet;
        for idx := low(idx) to high(idx) do
        begin
          if CompareText(CManualJournalStatus[idx], stgVal)= 0 then
          begin
            newStatus := idx;
            break;
          end;
        end;
        result := ord(newStatus);
      end;
  else
    result := inherited PropStringAsEnum(AField, StgVal);
  end;
end;

class function TXEROManualJournal.PropEnumAsString( AField : Word; IntVal : Integer) : string;
begin
  case AField of
    ord(xmjfStatus):
      begin
        if (intVal < 0) or (intVal > ord(high(TXEROManualJournalStatus))) then
          intVal := ord(xmjsNotSet);
        result := CManualJournalStatus[TXEROManualJournalStatus(intval)];
      end;
  else
    result := inherited PropEnumAsString(AField, IntVal);
  end;
end;

class function TXEROManualJournal.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_ManualJournalMap.Count[APropType];
end;

class function TXEROManualJournal.PropFieldID( AFieldName : String) : integer;
begin
  result := G_ManualJournalMap.NameToFieldID(AFieldName);
end;

class function TXEROManualJournal.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  case field of
    xsfUID: result := ord(xafAccountID);
    xsfName: result := ord(xafName);
  else
    result := inherited PropSpecialFieldID(Field);
  end;
end;

class function TXEROManualJournal.PropObjectName : string;
begin
  result := 'ManualJournal';
end;

function TXEROManualJournal.DoCheckValidation : TXEROValidationErrors;
var
  curline : TXEROManualJournalLine;
begin
  result := inherited DoCheckValidation;
  if result <>  [low(TXEROValidationError)..high(TXEROValidationError)] then
  begin
    if assigned(FJournalLines) then
    begin
      for curLine in FJournalLines do
        result := result + curLine.DoCheckValidation;
    end;
  end;
end;

function TXEROManualJournal.rJournalLines: TXEROManualJournalLineList;
begin
  if not assigned(FJournalLines) then
  begin
    if ManualJournalID = '' then
      FJournalLines := TXEROManualJournalLineList.Create
    else
      Raise Exception.Create('Journal Lines not loaded');
  end;
  result := FJournalLines;
end;

function TXEROManualJournal.rHasJournalLines: Boolean;
begin
  result := assigned(FJournalLines) and (FJournalLines.Count > 0);
end;

Procedure TXEROManualJournal.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FJournalLines);
end;

procedure TXEROManualJournal.wJournalLines(NewVal: TXEROManualJournalLineList);
begin
  if not assigned(FJournalLines) then
    FJournalLines := TXEROManualJournalLineList.Create;
  if newVal = nil then
    FJournalLines.Clear
  else
    FJournalLines.Assign(newVal);
end;

function TXEROManualJournal.GetListObject(Field: Integer; Access : TXEROFieldAccessMode): TXEROObjectListBase;
begin
  case field of
    ord(xmjfJournalLines):
      begin
        if (access = xfamWrite) and (not assigned(FJournalLines)) then
          FJournalLines := TXEROManualJournalLineList.Create;
        result := FJournalLines;
      end;
  else result := inherited GetListObject(field, Access);
  end;
end;

function TXEROManualJournal.GetReferenceName : String;
begin
  result := 'ManualJournalID';
end;

// Describe any validation errors
function TXEROManualJournal.DescribeValidation(Identify : boolean; IndentLevel : integer = 0; Errors : TXEROValidationErrors =[xveError, xveWarning]) : String;
var
  haserrors : TXEROValidationErrors;
  curLine : TXEROManualJournalLine;
begin
  result := '';

  haserrors := Errors * DoCheckValidation;

  if xveError in hasErrors then
  begin
    result := inherited DescribeValidation(Identify, IndentLevel, [xveError]);
    for curLine in JournalLines do
      result := result + curLine.DescribeValidation(true, IndentLevel+1, [xveError]);
  end;

  if xveWarning in hasErrors then
  begin
    result := result + inherited DescribeValidation(Identify, IndentLevel, [xveWarning]);
    for curLine in JournalLines do
      result := result + curLine.DescribeValidation(true, IndentLevel+1, [xveWarning]);
  end;
end;

// TXEROManualJournalList
//

function TXEROManualJournalList.GetItemLoader : TXEROObjectLoaderBase;
begin
  result := TXEROManualJournalLoader.Create('ManualJournal',self);
end;

class function TXEROManualJournalList.PropListName : String;
begin
  inherited;
  result := 'ManualJournals';
end;

function TXEROObjectListBase.GetListLoader : TJSONSAXNestableHandler;
begin
  result := TXEROListLoader.Create(PropItemName, GetItemLoader);
end;

function TXEROObjectListBase.OutObject( writer : TXEROObjectWriter; APropName : String; propMode : TXEROPropertyMode; mode : TXEROSerialiseOutputMode) : boolean;
var
  idx : integer;
begin
  result := true;
  Writer.WriteStartArray(APropName, propMode);
  for idx := 0 to Count-1 do
  begin
    result := XEROItems[idx].OutObject(writer, XEROItems[idx].PropObjectName, xpmAnon, mode);
    if not result then
      break;
  end;
  Writer.WriteEndArray(APropName, propmode);
end;

function TXEROObjectListBase.Serialise( textStream : TTextWriter; AOutType : TXEROOutputType; mode : TXEROSerialiseOutputMode) : boolean;
var
  writer : TXEROObjectWriter;
begin
  writer := CreateWriter(mode = xsomDisplay, textStream, AOutType);
  try
    if not assigned(Writer) then
      result := false
    else
      result := OutObject(writer, PropListName, xpmAnon, mode);
  finally
    writer.Free;
  end;
end;

procedure TXEROObjectListBase.ResetModified;
var
  idx : integer;
begin
  for idx := 0 to Count-1 do
    XEROItems[idx].ResetModified;
end;

procedure TXEROObjectListBase.SetIsNew(newVal : boolean);
var
  idx : integer;
begin
  for idx := 0 to Count-1 do
    XEROItems[idx].IsNew := newVal;
end;

// Describe any validation errors
function TXEROObjectListBase.DescribeValidation(Identify : boolean; IndentLevel : integer = 0; Errors : TXEROValidationErrors =[xveError, xveWarning]) : String;
var
  idx : integer;
begin
  result := '';
  for idx := 0 to Count-1 do
  begin
    if result <> '' then
      result := result + #13#10;
    result := result + XEROItems[idx].DescribeValidation(true, IndentLevel+1);
  end;
end;

{ TXEROObjectList<XOBJ> }

procedure TXEROObjectList<XOBJ>.Assign( ASource : TPersistent);
var
  cur, clone : XOBJ;
  src : TXEROObjectList<XOBJ>;
begin
  if ASource is TXEROObjectList<XOBJ> then
  begin
    src := TXEROObjectList<XOBJ>(ASource);
    Clear;

    for cur in src.List do
    begin
      clone := XOBJ.Create;
      try
        clone.Assign(cur);
        List.Add(clone);
        clone := nil;
      finally
        FreeAndNil(clone);
      end;
    end;
  end
  else
    inherited Assign(ASource);
end;

class function TXEROObjectList<XOBJ>.PropItemName : String;
begin
  result := XOBJ.PropObjectName;
end;

procedure TXEROObjectList<XOBJ>.AfterConstruction;
begin
  inherited;
  FList := Generics.Collections.TObjectList<XOBJ>.Create(true);
end;

// protected definitions

function TXEROObjectList<XOBJ>.GetXEROObject(idx : integer) : TXEROObject;
begin
  result := FList[idx];
end;

function TXEROObjectList<XOBJ>.GetEnumerator: Generics.Collections.TObjectList<XOBJ>.TEnumerator;
begin
  result := FList.GetEnumerator;
end;

function TXEROObjectList<XOBJ>.GetItemLoader : TXEROObjectLoaderBase;
begin
  result := TXEROLoaderGenericBase.Create( XOBJ.PropObjectName, self);
end;

Procedure TXEROObjectList<XOBJ>.BeforeDestruction;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TXEROObjectList<XOBJ>.Insert(Index: Integer; const Value: XOBJ);
begin
  Flist.Insert(Index, Value);
end;

function TXEROObjectList<XOBJ>.Add(const Value: XOBJ): Integer;
begin
  result := FList.Add(Value);
end;

function TXEROObjectList<XOBJ>.AddXEROObject( AObj : TXEROObject) : integer;
begin
  result := FList.Add(AObj as XOBJ);
end;

function TXEROObjectList<XOBJ>.rItems(idx : integer): XOBJ;
begin
  result := FList.Items[idx];
end;

class function TXEROObjectList<XOBJ>.CreateListObject : TXEROObject;
begin
  result := XOBJ.Create;
end;

procedure TXEROObjectList<XOBJ>.Clear;
begin
  FList.Clear;
end;

function TXEROObjectList<XOBJ>.Count : Integer;
begin
  result := FList.Count;
end;

function DateTimeAsJSONString(ADateTime : TDateTime; forDisplay : boolean) : string;
begin
  if forDisplay then
     result := FormatDateTime('yyyy-mm-dd''T''hh:mm:ss:nn', ADateTime)
  else
    result := DateTimeAsMicrosoftJSONDate(ADateTime);
end;

function TXEROPropertyMap.NameToFieldID( const AFieldName : String) : integer;
var
  idx : integer;
begin
  if not Generics.Collections.TArray.BinarySearch<TXERONameEntry>(NameMap,
      TXERoNameEntry.Search(AFieldName), idx, G_Comparer) then
    result := -1
  else
    result := NameMap[idx].FieldNo;
end;

type
PXEROPropertyEntry = ^TXEROPropertyEntry;
{ Load the type info from the array.
  This determines the length of the arrays of property values.
  It makes it easier to have a common base implementation for property assignments.
  This also facilitates reading and wring JSON/XML blobs for communicating with XERO API
}
procedure LoadTypeinfoMap( var AMap : TXEROPropertyMap; props : PXEROPropertyEntry; len : integer);
var
  idx : TXEROPropertyType;
  idy, ids : integer;
  prop : PXEROPropertyEntry;
  mapper : Generics.Collections.TList<TXERONameEntry>;
  nameEntry : TXERONameEntry;
begin
  for idx := low(idx) to high(idx) do
    AMap.Count[idx] := 0;
  setLength(AMap.Map, len);
  SetLength(AMap.NameMap, len);
  mapper := Generics.Collections.TList<TXERONameEntry>.Create(G_Comparer);
  try
    prop := props;
    for idy := 0 to len-1 do
    begin
      with AMap.Map[idy] do
      begin
        PropType := prop^.PropType;
        case PropType of
          xptEnum:
            begin
              TypeIdx := AMap.Count[xptInteger];
              Inc(AMap.Count[xptInteger]); // Storage
            end;
          xptUnknown:
            begin
              TypeIdx := 0;
            end;
        else
          TypeIdx := AMap.Count[prop^.PropType];
          Inc(AMap.Count[prop^.PropType]);
        end;
      end;
      AMap.Count[xptUnknown] := len;
      nameEntry.PropName := prop^.PropName;
      nameEntry.fieldNo := idy;
      if mapper.BinarySearch(nameEntry, ids) then
        Raise Exception.Create('Duplicate property name: '+prop^.PropName);
      mapper.Insert(ids, nameEntry);
      Inc(prop);
    end;
    // Copy the sorted names into the mapper array.
    for idy := 0 to len-1 do
      AMap.NameMap[idy] := mapper[idy];
  finally
    FreeAndNil(mapper);
  end;
end;

{ XEROException }

constructor XEROException.Create(ACode: integer; AError: String);
begin
  FCode := ACode;
  FError := AError;
  inherited CreateFmt('[%d] %s', [ACode, AError]);
end;

initialization
  // This is a comparer for the property name sorted map.
  G_Comparer := Generics.Defaults.TComparer<TXERONameEntry>.Construct(
          function(const LHS, RHS : TXERONameEntry): integer
          begin
            result := CompareText(lhs.PropName, rhs.PropName);
          end);
  // Load all the type information for the various objects.
  LoadTypeInfoMap( G_OrganisationMap,      @COrganisationProperties[0],     length(COrganisationProperties) );
  LoadTypeInfoMap( G_AccountMap,           @CAccountProperties[0],          length(CAccountProperties) );
  LoadTypeInfoMap( G_ManualJournalMap,     @CManualJournalProperties[0],    length(CManualJournalProperties) );
  LoadTypeInfoMap( G_ManualJournalLineMap, @CManualJournalLineProperties[0],length(CManualJournalLineProperties) );
  LoadTypeInfoMap( G_TrackingCategoryMap,  @CTrackingCategoryProperties[0], length(CTrackingCategoryProperties) );
  LoadTypeInfoMap( G_TrackingOptionMap,    @CTrackingOptionProperties[0],   length(CTrackingOptionProperties) );
  LoadTypeInfoMap( G_TrackingCategoryOptionMap, @CTrackingCategoryOptionProperties[0], length(CTrackingCategoryOptionProperties) );
finalization
  G_Comparer := nil;
end.
