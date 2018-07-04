{: XERO API Class-based interface.

  These objects correspond to endpoints in the Xero Rest interface.
  In order for generic loading/serialising/copying/getters/setters to be easily achieved,
  the classes use static custom class information arrays.

  Also where possible, enumerated types are used to make programming more efficient.

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

  //: Getting a field for Read or Write or Clear (Can return null or empty list)
  TXEROFieldAccessMode = (xfamRead, xfamWrite, xfamClear);

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

    function GetObject(field : Integer; Access : TXEROFieldAccessMode) :TXEROObject; virtual;

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

    function ExtractIndex( AIndex : integer) : XOBJ;

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

  TXEROAddressList = class;
  TXEROPhonesList = class;

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
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
  protected
    FExternalLinks: TXEROExternalLinkList;
    FAddressesList : TXEROAddressList;
    FPhonesList : TXEROPhonesList;
    procedure wExternalLinks(NewVal: TXEROExternalLinkList);

    function rXeroOrganisationType(AField : Integer) : TXeroOrganisationType;
    procedure wXeroOrganisationType(AField : Integer; NewVal: TXeroOrganisationType);
    function rAddressesList : TXEROAddressList;
    procedure wAddressesList(ANewVal : TXEROAddressList);
    function rPhonesList : TXEROPhonesList;
    procedure wPhonesList(ANewVal : TXEROPhonesList);

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
    property Addresses : TXEROAddressList read rAddressesList write wAddressesList;
    // Phones details for organisation - see Phones
    property Phones : TXEROPhonesList read rPhonesList write wPhonesList;
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

  TXEROLineAmountTypes = (xlatUnspecified, xlatNoTax, xlatExclusiveOfTax, xlatInclusiveOfTax);

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
    function rLineAmountTypesEnum(AField : integer) : TXEROLineAmountTypes;
    procedure wLineAmountTypesEnum(AField : integer; ANewVal : TXEROLineAmountTypes);
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
    property LineAmountTypes : TXEROLineAmountTypes index ord(xmjfLineAmountTypes) read rLineAmountTypesEnum write wLineAmountTypesEnum;

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

  TXEROTaxComponentField = (
    // xtxf prefix because of conflict with TXEROTrackingCategory
    xtxfName,
    xtxfRate,
    xtxfIsCompound,
    xtxfIsNonRecoverable
  );

  TXEROTaxComponent = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
    class function PropObjectName : string; override;
  public
    // Name of Tax Component
    property ComponentName : String index ord(xtxfName) read rStringVal write wStringVal;
    // Tax Rate (up to 4dp)
    property Rate : Currency index ord(xtxfRate) read rCurrencyVal write wCurrencyVal;

    // Boolean to describe if Tax rate is compounded. Learn more
    property IsCompound : boolean index ord(xtxfIsCompound) read rBooleanVal write wBooleanVal;
    // Boolean to describe if tax rate is non-recoverable. Non-recoverable rates are only applicable to Canadian organisations.
    property IsNonRecoverable : boolean index ord(xtxfIsNonRecoverable) read rBooleanVal write wBooleanVal;
  end;

  TXEROTaxComponentList= class(TXEROObjectList<TXEROTaxComponent>)
  public
    class function PropListName : String; override;
  end;

  TXEROTaxRateField = (
    xtrfName,
    xtrfTaxType,
    xtrfTaxComponents,
    xtrfStatus,
    xtrfReportTaxType,
    xtrfCanApplyToAssets,
    xtrfCanApplyToEquity,
    xtrfCanApplyToExpenses,
    xtrfCanApplyToLiabilities,
    xtrfCanApplyToRevenue,
    xtrfDisplayTaxRate,
    xtrfEffectiveRate
  );

  TXeroTaxRate = class(TXEROObject)
  protected
    FTaxComponents: TXeroTaxComponentList;
    function rTaxComponents: TXeroTaxComponentList;
    procedure wTaxComponents(NewVal: TXeroTaxComponentList);
    function rHasTaxComponents : boolean;

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
    //:Get at list field
    function GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase; override;
  public
    // Name of tax rate	TaxType	See Tax Types - can only be used on update calls
    property RateName : String index ord(xtrfName) read rStringVal write wStringVal;
    property TaxType : String index ord(xtrfTaxType) read rStringVal write wStringVal;

    // See TaxComponents
    property TaxComponents : TXeroTaxComponentList read rTaxComponents write wTaxComponents;
    property HasTaxComponents: boolean read rHasTaxComponents;

    // See Status Codes
    property Status :  TXEROStatus index ord(xtrfStatus) read rXStatus write wXStatus;

    // See ReportTaxTypes
    property ReportTaxType : String index ord(xtrfReportTaxType) read rStringVal write wStringVal;
    // Boolean to describe if tax rate can be used for asset accounts i.e. true,false
    property CanApplyToAssets : Boolean index ord(xtrfCanApplyToAssets) read rBooleanVal write wBooleanVal;
    // Boolean to describe if tax rate can be used for equity accounts i.e. true,false
    property CanApplyToEquity : Boolean index ord(xtrfCanApplyToEquity) read rBooleanVal write wBooleanVal;
    // Boolean to describe if tax rate can be used for expense accounts i.e. true,false
    property CanApplyToExpenses : Boolean index ord(xtrfCanApplyToExpenses) read rBooleanVal write wBooleanVal;
    // Boolean to describe if tax rate can be used for liability accounts i.e. true,false
    property CanApplyToLiabilities : Boolean index ord(xtrfCanApplyToLiabilities) read rBooleanVal write wBooleanVal;
    // Boolean to describe if tax rate can be used for revenue accounts i.e. true,false
    property CanApplyToRevenue : Boolean index ord(xtrfCanApplyToRevenue) read rBooleanVal write wBooleanVal;
    // Tax Rate (decimal to 4dp) e.g 12.5000
    property DisplayTaxRate : Currency index ord(xtrfDisplayTaxRate) read rCurrencyVal write wCurrencyVal;
    // Effective Tax Rate (decimal to 4dp) e.g 12.5000
    property EffectiveRate : Currency index ord(xtrfEffectiveRate) read rCurrencyVal write wCurrencyVal;
  end;

  TXEROTaxRateList = class(TXEROObjectList<TXeroTaxRate>)
  public
    class function PropListName : String; override;
  end;


  // Invoices Status  (invoices so as not in conflict with the one in XERO.API.INVOICES)
  TXEROInvoicesStatus = (xisUnspecified, xisDraft, xisSubmitted, xisDeleted,
    xisAuthorised, xisPaid, xisVoided);

  TXEROInvoicesType = (xitUnspecified, xitAccPay, xitAccRec);

  TXEROBrandingThemeField = (xbtfBrandingThemeID, xbtfName, xbtfSortOrder, xbtfCreatedDateUTC);
  TXEROBrandingTheme = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropObjectName : String; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
  published
    // Xero identifier
    property BrandingThemeID : String index xbtfBrandingThemeID read rStringVal write wStringVal;
    // Name of branding theme
    property Name : String index xbtfName read rStringVal write wStringVal;
    // Integer - ranked order of branding theme. The default branding theme has a value of 0
    property SortOrder : String index xbtfSortOrder read rStringVal write wStringVal;
    // UTC timestamp of creation date of branding theme
    property CreatedDateUTC : TDateTime index xbtfCreatedDateUTC read rDateTimeVal write wDateTimeVal;
  end;

  TXEROAddressType = (xaddUnspecified, xaddPOBox, xaddStreet, xaddDelivery);

  TXEROAddressField = (xafAddressType, xafAddressLine1, xafAddressLine2, xafAddressLine3, xafAddressLine4, xafCity, xafRegion, xafPostalCode, xafCountry, xafAttentionTo);
  TXEROAddress = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropObjectName : String; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
    class function PropStringAsEnum( AField : Word; const StgVal : String) : integer; override;
    class function PropEnumAsString( AField : Word; IntVal : Integer) : string; override;
  protected
    function rAddressTypeEnum(AField : integer) : TXEROAddressType;
    procedure wAddressTypeEnum(AField : integer; ANewVal : TXEROAddressType);
  published
    property AddressType : TXEROAddressType index xafAddressType read rAddressTypeEnum write wAddressTypeEnum;
    property AddressLine1 : String index xafAddressLine1 read rStringVal write wStringVal;
    property AddressLine2 : String index xafAddressLine2 read rStringVal write wStringVal;
    property AddressLine3 : String index xafAddressLine3 read rStringVal write wStringVal;
    property AddressLine4 : String index xafAddressLine4 read rStringVal write wStringVal;
    property City : String index xafCity read rStringVal write wStringVal;
    property Region : String index xafRegion read rStringVal write wStringVal;
    property PostalCode : String index xafPostalCode read rStringVal write wStringVal;
    property Country : String index xafCountry read rStringVal write wStringVal;
    property AttentionTo : String index xafAttentionTo read rStringVal write wStringVal;
  end;
  TXEROAddressList = class(TXEROObjectList<TXEROAddress>)
  public
    class function PropListName : String; override;
  end;

  // STUB
  TXEROPhone = class(TXEROObject)
  public
    class function PropObjectName : String; override;
  end;
  TXEROPhonesList = class(TXEROObjectList<TXEROPhone>)
  public
    class function PropListName : String; override;
  end;

  TXEROInvoice = class;
  TXEROCreditNote = class;
  TXEROPrepayment = class;
  TXEROOverpayment = class;

  TXEROContactStatus = (xcsUnspecified, xcsActive, xcsArchived, xcsGDPRRequest);
  TXEROPaymentTerms = (xptUnspecified, xptDaysAfterBillDate, xptOfCurrentMonth, xptOfFollowingMonth);

  TXEROPaymentType = (
    xptUnspecifiedPayment,
    xptAccRecPayment, // Accounts Receivable Payment
    xptAccPayPayment, // Accounts Payable Payment
    xptARCreditPayment, // Accounts Receivable Credit Payment (Refund)
    xptAPCreditPayment, // Accounts Payable Credit Payment (Refund)
    xptAROverPaymentPayment, // Accounts Receivable Overpayment Payment (Refund)
    xptARPrePaymentPayment, // Accounts Receivable Prepayment Payment (Refund)
    xptAPPrePaymentPayment, // Accounts Payable Prepayment Payment (Refund)
    xptAPOverPaymentPayment // Accounts Payable Overpayment Payment (Refund)
  );

  TXEROPaymentStatus = (
    xpsUnspecified,
    xpsAuthorised,
    xpsDeleted
  );

  TXEROPaymentField = (
    xpfPaymentID, xpfDate, xpfCurrencyRate, xpfAmount, xpfReference,
    xpfIsReconciled, xpfStatus, xpfPaymentType, xpfUpdatedDateUTC, xpfAccount,
    xpfInvoice, xpfCreditNote, xpfPrepayments, xpfOverpayment);

  TXEROPayment = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropObjectName : String; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
    class function PropStringAsEnum( AField : Word; const StgVal : String) : integer; override;
    class function PropEnumAsString( AField : Word; IntVal : Integer) : string; override;
  protected
    FAccount : TXEROAccount;
    FInvoice : TXEROInvoice;
    FCreditNote : TXEROCreditNote;
    FPrepayments : TXEROPrepayment;
    FOverpayment : TXEROOverpayment;
    function rStatusEnum(AField : integer) : TXEROPaymentStatus;
    procedure wStatusEnum(AField : integer; ANewVal : TXEROPaymentStatus);
    function rPaymentTypeEnum(AField : integer) : TXEROPaymentType;
    procedure wPaymentTypeEnum(AField : integer; ANewVal : TXEROPaymentType);
    function rAccount : TXEROAccount;
    procedure wAccount(ANewVal : TXEROAccount);
    function rInvoice : TXEROInvoice;
    procedure wInvoice(ANewVal : TXEROInvoice);
    function rCreditNote : TXEROCreditNote;
    procedure wCreditNote(ANewVal : TXEROCreditNote);
    function rPrepayments : TXEROPrepayment;
    procedure wPrepayments(ANewVal : TXEROPrepayment);
    function rOverpayment : TXEROOverpayment;
    procedure wOverpayment(ANewVal : TXEROOverpayment);
    function GetObject(field : Integer; Access : TXEROFieldAccessMode) :TXEROObject; override;
  public
    procedure BeforeDestruction; override;
  published
    // Ident of payment
    property PaymentID : String index xpfPaymentID read rStringVal write wStringVal;
    // Date the payment is being made (YYYY-MM-DD) e.g. 2009-09-06
    property Date : TDateTime index xpfDate read rDateTimeVal write wDateTimeVal;
    // Exchange rate when payment is received. Only used for non base currency invoices and credit notes e.g. 0.7500
    property CurrencyRate : Currency index xpfCurrencyRate read rCurrencyVal write wCurrencyVal;
    // The amount of the payment. Must be less than or equal to the outstanding amount owing on the invoice e.g. 200.00
    property Amount : Currency index xpfAmount read rCurrencyVal write wCurrencyVal;
    // An optional description for the payment e.g. Direct Debit
    property Reference : String index xpfReference read rStringVal write wStringVal;
    // An optional parameter for the payment. Conversion related apps can utilise the IsReconciled flag in scenarios when a matching bank statement line is not available. Learn more
    property IsReconciled : Boolean index xpfIsReconciled read rBooleanVal write wBooleanVal;
    // The status of the payment.
    property Status : TXEROPaymentStatus index xpfStatus read rStatusEnum write wStatusEnum;
    // See Payment Types.
    property PaymentType : TXEROPaymentType index xpfPaymentType read rPaymentTypeEnum write wPaymentTypeEnum;
    // UTC timestamp of last update to the payment
    property UpdatedDateUTC : TDateTime index xpfUpdatedDateUTC read rDateTimeVal write wDateTimeVal;
    // The Account the payment was made from
    property Account : TXEROAccount read rAccount write wAccount;
    // The Invoice the payment was made against
    property Invoice : TXEROInvoice read rInvoice write wInvoice;
    // The Credit Note the payment was made against
    property CreditNote : TXEROCreditNote read rCreditNote write wCreditNote;
    // The Prepayment the payment was made against
    property Prepayment : TXEROPrepayment read rPrepayments write wPrepayments;
    // The Overpayment the payment was made against
    property Overpayment : TXEROOverpayment read rOverpayment write wOverpayment;
  end;
  TXEROPaymentList = class(TXEROObjectList<TXEROPayment>)
  public
    class function PropListName : String; override;
  end;

  TXEROContact = class;
  // Credit notes just use the invoice line items.
  TXEROInvoiceLineItemList = class;
  TXEROCreditNoteStatus = (xcnsUspecified, xcnsSubmitted, xcnsAuthorised, scnsPAID); // ??? These are currently copied from ExpenseClaims enum

  TXEROCreditNoteField = (xcnfCreditNoteID, xcnfType, xcnfContact, xcnfDate, xcnfStatus, xcnfLineAmountTypes, xcnfLineItems, xcnfSubTotal, xcnfTotalTax, xcnfTotal, xcnfCISDeduction, xcnfUpdatedDateUTC, xcnfCurrencyCode, xcnfFullyPaidOnDate, xcnfCreditNoteNumber, xcnfReference, xcnfSentToContact, xcnfCurrencyRate, xcnfRemainingCredit, xcnfAllocations, xcnfBrandingThemeID, xcnfHasAttachments);
  TXEROCreditNote = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropObjectName : String; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
    class function PropStringAsEnum( AField : Word; const StgVal : String) : integer; override;
    class function PropEnumAsString( AField : Word; IntVal : Integer) : string; override;
  protected
    FContact : TXEROContact;
    FLineItemsList : TXEROInvoiceLineItemList;
    function rTypeEnum(AField : integer) : TXEROInvoicesType;
    procedure wTypeEnum(AField : integer; ANewVal : TXEROInvoicesType);
    function rContact : TXEROContact;
    procedure wContact(ANewVal : TXEROContact);
    function rStatusEnum(AField : integer) : TXEROCreditNoteStatus;
    procedure wStatusEnum(AField : integer; ANewVal : TXEROCreditNoteStatus);
    function rLineAmountTypesEnum(AField : integer) : TXEROLineAmountTypes;
    procedure wLineAmountTypesEnum(AField : integer; ANewVal : TXEROLineAmountTypes);
    function rLineItemsList : TXEROInvoiceLineItemList;
    procedure wLineItemsList(ANewVal : TXEROInvoiceLineItemList);
    function GetObject(field : Integer; Access : TXEROFieldAccessMode) :TXEROObject; override;
    function GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase; override;
  public
    procedure BeforeDestruction; override;
  published
    // Xero generated unique identifier
    property CreditNoteID : String index xcnfCreditNoteID read rStringVal write wStringVal;
    // See Credit Note Types
    property CreditNoteType : TXEROInvoicesType index xcnfType read rTypeEnum write wTypeEnum;
    // See Contacts
    property Contact : TXEROContact read rContact write wContact;
    // The date the credit note is issued YYYY-MM-DD. If the Date element is not specified then it will default to the current date based on the timezone setting of the organisation
    property Date : TDateTime index xcnfDate read rDateTimeVal write wDateTimeVal;
    // See Credit Note Status Codes
    property Status : TXEROCreditNoteStatus index xcnfStatus read rStatusEnum write wStatusEnum;
    // See Invoice Line Amount Types
    property LineAmountTypes : TXEROLineAmountTypes index xcnfLineAmountTypes read rLineAmountTypesEnum write wLineAmountTypesEnum;
    // See Invoice Line Items
    property LineItems : TXEROInvoiceLineItemList read rLineItemsList write wLineItemsList;
    // The subtotal of the credit note excluding taxes
    property SubTotal : Currency index xcnfSubTotal read rCurrencyVal write wCurrencyVal;
    // The total tax on the credit note
    property TotalTax : Currency index xcnfTotalTax read rCurrencyVal write wCurrencyVal;
    // The total of the Credit Note(subtotal + total tax)
    property Total : Currency index xcnfTotal read rCurrencyVal write wCurrencyVal;
    // CISDeduction withheld by the contractor to be paid to HMRC on behalf of subcontractor (Available for organisations under UK Construction Industry Scheme)
    property CISDeduction : Currency index xcnfCISDeduction read rCurrencyVal write wCurrencyVal;
    // UTC timestamp of last update to the credit note
    property UpdatedDateUTC : TDateTime index xcnfUpdatedDateUTC read rDateTimeVal write wDateTimeVal;
    // Currency used for the Credit Note
    property CurrencyCode : String index xcnfCurrencyCode read rStringVal write wStringVal;
    // Date when credit note was fully paid(UTC format)
    property FullyPaidOnDate : TDateTime index xcnfFullyPaidOnDate read rDateTimeVal write wDateTimeVal;
    // ACCRECCREDIT - Unique alpha numeric code identifying credit note.  ACCPAYCREDIT - Non-unique alpha numeric code identifying credit note. This value will also display as Reference in the UI.
    property CreditNoteNumber : String index xcnfCreditNoteNumber read rStringVal write wStringVal;
    // ACCRECCREDIT only - additional reference number
    property Reference : String index xcnfReference read rStringVal write wStringVal;
    // boolean to indicate if a credit note has been sent to a contact via the Xero app (currently read only)
    property SentToContact : Boolean index xcnfSentToContact read rBooleanVal write wBooleanVal;
    // The currency rate for a multicurrency invoice. If no rate is specified, the XE.com day rate is used
    property CurrencyRate : Currency index xcnfCurrencyRate read rCurrencyVal write wCurrencyVal;
    // The remaining credit balance on the Credit Note
    property RemainingCredit : Currency index xcnfRemainingCredit read rCurrencyVal write wCurrencyVal;
    // See Allocations
    property Allocations : String index xcnfAllocations read rStringVal write wStringVal;
    // See BrandingThemes
    property BrandingThemeID : String index xcnfBrandingThemeID read rStringVal write wStringVal;
    // boolean to indicate if a credit note has an attachment
    property HasAttachments : Boolean index xcnfHasAttachments read rBooleanVal write wBooleanVal;
  end;
  TXEROCreditNoteList = class(TXEROObjectList<TXEROCreditNote>)
  public
    class function PropListName : String; override;
  end;

  // STUB
  TXEROPrepayment = class(TXEROObject)
  public
    class function PropObjectName : String; override;
  end;
  TXEROPrepaymentList = class(TXEROObjectList<TXEROPrepayment>)
  public
    class function PropListName : String; override;
  end;

  // STUB
  TXEROOverpayment = class(TXEROObject)
  public
    class function PropObjectName : String; override;
  end;
  TXEROOverpaymentList = class(TXEROObjectList<TXEROOverpayment>)
  public
    class function PropListName : String; override;
  end;

  TXEROContactPersonField = (xcpfFirstName, xcpfLastName, xcpfEmailAddress, xcpfIncludeInEmails);
  TXEROContactPerson = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropObjectName : String; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
  published
    // First name of person
    property FirstName : String index xcpfFirstName read rStringVal write wStringVal;
    // Last name of person
    property LastName : String index xcpfLastName read rStringVal write wStringVal;
    // Email address of person
    property EmailAddress : String index xcpfEmailAddress read rStringVal write wStringVal;
    // boolean to indicate whether contact should be included on emails with invoices etc.
    property IncludeInEmails : Boolean index xcpfIncludeInEmails read rBooleanVal write wBooleanVal;
  end;

  TXEROContactPersonsList = class(TXEROObjectList<TXEROContactPerson>)
  public
    class function PropListName : String; override;
  end;

  // contact
  TXEROContactField = (xcfContactID, xcfContactNumber, xcfAccountNumber, xcfContactStatus, xcfName, xcfFirstName, xcfLastName, xcfEmailAddress, xcfSkypeUserName, xcfBankAccountDetails, xcfTaxNumber, xcfAccountsReceivableTaxType, xcfAccountsPayableTaxType, xcfAddresses, xcfPhones, xcfIsSupplier, xcfIsCustomer, xcfDefaultCurrency, xcfUpdatedDateUTC, xcfContactPersons, xcfXeroNetworkKey, xcfSalesDefaultAccountCode, xcfPurchasesDefaultAccountCode, xcfSalesTrackingCategories, xcfPurchasesTrackingCategories, xcfTrackingCategoryName, xcfTrackingOptionName, xcfPaymentTerms, xcfContactGroups, xcfWebsite, xcfBrandingTheme, xcfBatchPayments, xcfDiscount, xcfBalances, xcfHasAttachments);
  TXEROContact = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropObjectName : String; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
    class function PropStringAsEnum( AField : Word; const StgVal : String) : integer; override;
    class function PropEnumAsString( AField : Word; IntVal : Integer) : string; override;
  protected
    FAddressesList : TXEROAddressList;
    FPhonesList : TXEROPhonesList;
    FContactPersonsList : TXEROContactPersonsList;
    FSalesTrackingCategoriesList : TXEROTrackingCategoryList;
    FPurchasesTrackingCategoriesList : TXEROTrackingCategoryList;
    FBrandingTheme : TXEROBrandingTheme;
    function rContactStatusEnum(AField : integer) : TXEROContactStatus;
    procedure wContactStatusEnum(AField : integer; ANewVal : TXEROContactStatus);
    function rAddressesList : TXEROAddressList;
    procedure wAddressesList(ANewVal : TXEROAddressList);
    function rPhonesList : TXEROPhonesList;
    procedure wPhonesList(ANewVal : TXEROPhonesList);
    function rContactPersonsList : TXEROContactPersonsList;
    procedure wContactPersonsList(ANewVal : TXEROContactPersonsList);
    function rSalesTrackingCategoriesList : TXEROTrackingCategoryList;
    procedure wSalesTrackingCategoriesList(ANewVal : TXEROTrackingCategoryList);
    function rPurchasesTrackingCategoriesList : TXEROTrackingCategoryList;
    procedure wPurchasesTrackingCategoriesList(ANewVal : TXEROTrackingCategoryList);
    function rPaymentTermsEnum(AField : integer) : TXEROPaymentTerms;
    procedure wPaymentTermsEnum(AField : integer; ANewVal : TXEROPaymentTerms);
    function rBrandingTheme : TXEROBrandingTheme;
    procedure wBrandingTheme(ANewVal : TXEROBrandingTheme);
    function GetObject(field : Integer; Access : TXEROFieldAccessMode) :TXEROObject; override;
    function GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase; override;
  public
    procedure BeforeDestruction; override;
  published
    // Xero identifier
    property ContactID : String index xcfContactID read rStringVal write wStringVal;
    // This field is read only in the Xero UI, used to identify contacts in external systems. It is displayed as Contact Code in the Contacts UI in Xero.
    property ContactNumber : String index xcfContactNumber read rStringVal write wStringVal;
    // A user defined account number. This can be updated via the API and the Xero UI
    property AccountNumber : String index xcfAccountNumber read rStringVal write wStringVal;
    // Current status of a contact - see contact status types
    property ContactStatus : TXEROContactStatus index xcfContactStatus read rContactStatusEnum write wContactStatusEnum;
    // Full name of contact/organisation
    property Name : String index xcfName read rStringVal write wStringVal;
    // First name of contact person
    property FirstName : String index xcfFirstName read rStringVal write wStringVal;
    // Last name of contact person
    property LastName : String index xcfLastName read rStringVal write wStringVal;
    // Email address of contact person
    property EmailAddress : String index xcfEmailAddress read rStringVal write wStringVal;
    // Skype user name of contact
    property SkypeUserName : String index xcfSkypeUserName read rStringVal write wStringVal;
    // Bank account number of contact
    property BankAccountDetails : String index xcfBankAccountDetails read rStringVal write wStringVal;
    // Tax number of contact - this is also known as the ABN (Australia), GST Number (New Zealand), VAT Number (UK) or Tax ID Number (US and global) in the Xero UI depending on which regionalized version of Xero you are using
    property TaxNumber : String index xcfTaxNumber read rStringVal write wStringVal;
    // Default tax type used for contact on AR invoices
    property AccountsReceivableTaxType : String index xcfAccountsReceivableTaxType read rStringVal write wStringVal;
    // Default tax type used for contact on AP invoices
    property AccountsPayableTaxType : String index xcfAccountsPayableTaxType read rStringVal write wStringVal;

    // Store certain address types for a contact - see address types
    property Addresses : TXEROAddressList read rAddressesList write wAddressesList;
    // Store certain phone types for a contact - see phone types
    property Phones : TXEROPhonesList read rPhonesList write wPhonesList;

    // true or false - Boolean that describes if a contact that has any AP invoices entered against them
    property IsSupplier : Boolean index xcfIsSupplier read rBooleanVal write wBooleanVal;
    // true or false - Boolean that describes if a contact has any AR invoices entered against them
    property IsCustomer : Boolean index xcfIsCustomer read rBooleanVal write wBooleanVal;
    // Default currency for raising invoices against contact
    property DefaultCurrency : String index xcfDefaultCurrency read rStringVal write wStringVal;
    // UTC timestamp of last update to contact
    property UpdatedDateUTC : TDateTime index xcfUpdatedDateUTC read rDateTimeVal write wDateTimeVal;

    // The following are only retrieved on GET requests for a single contact or when pagination is used

    // See contact persons. A contact can have a maximum of 5 ContactPersons
    property ContactPersons : TXEROContactPersonsList read rContactPersonsList write wContactPersonsList;
    // Store XeroNetworkKey for contacts.
    property XeroNetworkKey : String index xcfXeroNetworkKey read rStringVal write wStringVal;
    // The default sales account code for contacts
    property SalesDefaultAccountCode : String index xcfSalesDefaultAccountCode read rStringVal write wStringVal;
    // The default purchases account code for contacts
    property PurchasesDefaultAccountCode : String index xcfPurchasesDefaultAccountCode read rStringVal write wStringVal;
    // The default sales tracking categories for contacts
    property SalesTrackingCategories : TXEROTrackingCategoryList read rSalesTrackingCategoriesList write wSalesTrackingCategoriesList;
    // The default purchases tracking categories for contacts
    property PurchasesTrackingCategories : TXEROTrackingCategoryList read rPurchasesTrackingCategoriesList write wPurchasesTrackingCategoriesList;
    // The name of the Tracking Category assigned to the contact under SalesTrackingCategories and PurchasesTrackingCategories
    property TrackingCategoryName : String index xcfTrackingCategoryName read rStringVal write wStringVal;
    // The name of the Tracking Option assigned to the contact under SalesTrackingCategories and PurchasesTrackingCategories
    property TrackingOptionName : String index xcfTrackingOptionName read rStringVal write wStringVal;
    // The default payment terms for the contact - see Payment Terms
    property PaymentTerms : TXEROPaymentTerms index xcfPaymentTerms read rPaymentTermsEnum write wPaymentTermsEnum;
    // Displays which contact groups a contact is included in
    property ContactGroups : String index xcfContactGroups read rStringVal write wStringVal;
    // Website address for contact
    property Website : String index xcfWebsite read rStringVal write wStringVal;
    // Default branding theme for contact - see Branding Themes
    property BrandingTheme : TXEROBrandingTheme read rBrandingTheme write wBrandingTheme;
    // batch payment details for contact
    property BatchPayments : String index xcfBatchPayments read rStringVal write wStringVal;
    // The default discount rate for the contact
    property Discount : Currency index xcfDiscount read rCurrencyVal write wCurrencyVal;
    // The raw AccountsReceivable(sales invoices) and AccountsPayable(bills) outstanding and overdue amounts, not converted to base currency
    property Balances : String index xcfBalances read rStringVal write wStringVal;
    // A boolean to indicate if a contact has an attachment
    property HasAttachments : Boolean index xcfHasAttachments read rBooleanVal write wBooleanVal;
  end;

  TXEROContactList = class(TXEROObjectList<TXEROContact>)
  public
    class function PropListName : String; override;
  end;

  TXEROInvoiceLineItemField = (xilifDescription, xilifQuantity, xilifUnitAmount, xilifItemCode, xilifAccountCode, xilifLineItemID, xilifTaxType, xilifTaxAmount, xilifLineAmount, xilifDiscountRate, xilifTracking);
  TXEROInvoiceLineItem = class(TXEROObject)
  protected
    FTrackingList : TXEROTrackingCategoryOptionList;
    function rTrackingList : TXEROTrackingCategoryOptionList;
    procedure wTrackingList(ANewVal : TXEROTrackingCategoryOptionList);
    function GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase; override;
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropObjectName : String; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;

    procedure BeforeDestruction; override;
  published
    // The description of the line item
    property Description : String index xilifDescription read rStringVal write wStringVal;
    // LineItem Quantity
    property Quantity : Currency index xilifQuantity read rCurrencyVal write wCurrencyVal;
    // Lineitem unit amount. By default, unit amount will be rounded to two decimal places. You can opt in to use four decimal places by adding the querystring parameter unitdp=4 to your query. See the Rounding in Xero guide for more information.
    property UnitAmount : Currency index xilifUnitAmount read rCurrencyVal write wCurrencyVal;
    // See Items
    property ItemCode : String index xilifItemCode read rStringVal write wStringVal;
    // See Accounts
    property AccountCode : String index xilifAccountCode read rStringVal write wStringVal;
    // The Xero generated identifier for a LineItem
    property LineItemID : String index xilifLineItemID read rStringVal write wStringVal;
    // Used as an override if the default Tax Code for the selected AccountCode is not correct - see TaxTypes.
    property TaxType : String index xilifTaxType read rStringVal write wStringVal;
    // The tax amount is auto calculated as a percentage of the line amount based on the tax rate
    property TaxAmount : Currency index xilifTaxAmount read rCurrencyVal write wCurrencyVal;
    // The line amount reflects the discounted price if a DiscountRate has been used i.e LineAmount = Quantity * Unit Amount * ((100 - DiscountRate)/100)
    property LineAmount : Currency index xilifLineAmount read rCurrencyVal write wCurrencyVal;
    // Percentage discount being applied to a line item (only supported on ACCREC invoices - ACC PAY invoices and credit notes in Xero do not support discounts
    property DiscountRate : Currency index xilifDiscountRate read rCurrencyVal write wCurrencyVal;
    // Section for optional Tracking Category - see TrackingCategory. Any LineItem can have a maximum of 2 TrackingCategory elements.
    property Tracking : TXEROTrackingCategoryOptionList read rTrackingList write wTrackingList;
  end;

  TXEROInvoiceLineItemList = class(TXEROObjectList<TXEROInvoiceLineItem>)
  public
    class function PropListName : String; override;
  end;

  TXEROInvoicesField = (
    xifType, xifContact, xifDate, xifDueDate, xifStatus, xifLineAmountTypes,
    xifLineItems, xifSubTotal, xifTotalTax, xifTotal, xifTotalDiscount,
    xifUpdatedDateUTC, xifCurrencyCode, xifCurrencyRate, xifInvoiceID,
    xifInvoiceNumber, xifReference, xifBrandingThemeID, xifUrl, xifSentToContact,
    xifExpectedPaymentDate, xifPlannedPaymentDate, xifHasAttachments, xifPayments,
    xifCreditNotes, xifPrepayments, xifOverpayments, xifAmountDue, xifAmountPaid,
    xifCISDeduction, xifFullyPaidOnDate, xifAmountCredited);

  // Represents a single invoice
  TXEROInvoice = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropObjectName : String; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
    class function PropStringAsEnum( AField : Word; const StgVal : String) : integer; override;
    class function PropEnumAsString( AField : Word; IntVal : Integer) : string; override;
  protected
    FContact : TXEROContact;
    FLineItemsList : TXEROInvoiceLineItemList;
    FPaymentsList : TXEROPaymentList;
    FCreditNotesList : TXEROCreditNoteList;
    FPrepaymentsList : TXEROPrepaymentList;
    FOverpaymentsList : TXEROOverpaymentList;
    function rTypeEnum(AField : integer) : TXEROInvoicesType;
    procedure wTypeEnum(AField : integer; ANewVal : TXEROInvoicesType);

    function rContact : TXEROContact;
    procedure wContact(ANewVal : TXEROContact);
    function rStatusEnum(AField : integer) : TXEROInvoicesStatus;
    procedure wStatusEnum(AField : integer; ANewVal : TXEROInvoicesStatus);
    function rLineAmountTypesEnum(AField : integer) : TXEROLineAmountTypes;
    procedure wLineAmountTypesEnum(AField : integer; ANewVal : TXEROLineAmountTypes);
    function rLineItemsList : TXEROInvoiceLineItemList;
    procedure wLineItemsList(ANewVal : TXEROInvoiceLineItemList);
    function rPaymentsList : TXEROPaymentList;
    procedure wPaymentsList(ANewVal : TXEROPaymentList);
    function rCreditNotesList : TXEROCreditNoteList;
    procedure wCreditNotesList(ANewVal : TXEROCreditNoteList);
    function rPrepaymentsList : TXEROPrepaymentList;
    procedure wPrepaymentsList(ANewVal : TXEROPrepaymentList);
    function rOverpaymentsList : TXEROOverpaymentList;
    procedure wOverpaymentsList(ANewVal : TXEROOverpaymentList);
    function GetObject(field : Integer; Access : TXEROFieldAccessMode) :TXEROObject; override;
    function GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase; override;
  public
    procedure BeforeDestruction; override;
  published
    // See Invoice Types
    property InvoiceType : TXEROInvoicesType index xifType read rTypeEnum write wTypeEnum;
    // See Contacts
    property Contact : TXEROContact read rContact write wContact;
    // Date invoice was issued - YYYY-MM-DD
    property Date : TDateTime index xifDate read rDateTimeVal write wDateTimeVal;
    // Date invoice is due - YYYY-MM-DD
    property DueDate : TDateTime index xifDueDate read rDateTimeVal write wDateTimeVal;
    // See Invoice Status Codes
    property Status : TXEROInvoicesStatus index xifStatus read rStatusEnum write wStatusEnum;
    // See Line Amount Types
    property LineAmountTypes : TXEROLineAmountTypes index xifLineAmountTypes read rLineAmountTypesEnum write wLineAmountTypesEnum;
    // See LineItems. The LineItems collection can contain any number of individual LineItem sub-elements.
    property LineItems : TXEROInvoiceLineItemList read rLineItemsList write wLineItemsList;
    // Total of invoice excluding taxes
    property SubTotal : Currency index xifSubTotal read rCurrencyVal write wCurrencyVal;
    // Total tax on invoice
    property TotalTax : Currency index xifTotalTax read rCurrencyVal write wCurrencyVal;
    // Total of Invoice tax inclusive (i.e. SubTotal + TotalTax)
    property Total : Currency index xifTotal read rCurrencyVal write wCurrencyVal;
    // Total of discounts applied on the invoice line items
    property TotalDiscount : Currency index xifTotalDiscount read rCurrencyVal write wCurrencyVal;
    // Last modified date UTC format
    property UpdatedDateUTC : TDateTime index xifUpdatedDateUTC read rDateTimeVal write wDateTimeVal;
    // The currency that invoice has been raised in (see Currencies)
    property CurrencyCode : String index xifCurrencyCode read rStringVal write wStringVal;
    // The currency rate for a multicurrency invoice
    property CurrencyRate : Currency index xifCurrencyRate read rCurrencyVal write wCurrencyVal;
    // Xero generated unique identifier for invoice
    property InvoiceID : String index xifInvoiceID read rStringVal write wStringVal;
    { ACCREC - Unique alpha numeric code identifying invoice
      ACCPAY - non-unique alpha numeric code identifying invoice. This value will also display as Reference in the UI.
    }
    property InvoiceNumber : String index xifInvoiceNumber read rStringVal write wStringVal;

    // ACCREC only - additional reference number
    property Reference : String index xifReference read rStringVal write wStringVal;
    // See BrandingThemes
    property BrandingThemeID : String index xifBrandingThemeID read rStringVal write wStringVal;
    // URL link to a source document - shown as "Go to [appName]" in the Xero app
    property Url : String index xifUrl read rStringVal write wStringVal;
    // Boolean to indicate whether the invoice in the Xero app displays as "sent"
    property SentToContact : Boolean index xifSentToContact read rBooleanVal write wBooleanVal;
    // Shown on sales invoices (Accounts Receivable) when this has been set
    property ExpectedPaymentDate : TDateTime index xifExpectedPaymentDate read rDateTimeVal write wDateTimeVal;
    // Shown on bills (Accounts Payable) when this has been set
    property PlannedPaymentDate : TDateTime index xifPlannedPaymentDate read rDateTimeVal write wDateTimeVal;
    // boolean to indicate if an invoice has an attachment
    property HasAttachments : Boolean index xifHasAttachments read rBooleanVal write wBooleanVal;
    // See Payments
    property Payments : TXEROPaymentList read rPaymentsList write wPaymentsList;
    // Details of credit notes that have been applied to an invoice
    property CreditNotes : TXEROCreditNoteList read rCreditNotesList write wCreditNotesList;
    // See Prepayments
    property Prepayments : TXEROPrepaymentList read rPrepaymentsList write wPrepaymentsList;
    // See Overpayments
    property Overpayments : TXEROOverpaymentList read rOverpaymentsList write wOverpaymentsList;
    // Amount remaining to be paid on invoice
    property AmountDue : Currency index xifAmountDue read rCurrencyVal write wCurrencyVal;
    // Sum of payments received for invoice
    property AmountPaid : Currency index xifAmountPaid read rCurrencyVal write wCurrencyVal;
    // CISDeduction withheld by the contractor to be paid to HMRC on behalf of subcontractor (Available for organisations under UK Construction Industry Scheme)
    property CISDeduction : Currency index xifCISDeduction read rCurrencyVal write wCurrencyVal;
    // The date the invoice was fully paid. Only returned on fully paid invoices
    property FullyPaidOnDate : TDateTime index xifFullyPaidOnDate read rDateTimeVal write wDateTimeVal;
    // Sum of all credit notes, over-payments and pre-payments applied to invoice
    property AmountCredited : Currency index xifAmountCredited read rCurrencyVal write wCurrencyVal;
  end;

  TXEROInvoiceList = class(TXEROObjectList<TXEROInvoice>)
  public
    class function PropListName : String; override;
  end;

  // Detail Elements for Purchases and Sales
  TXEROItemPriceDetailField = (xipdfUnitPrice, xipdfAccountCode, xipdfCOGSAccountCode, xipdfTaxType);
  TXEROItemPriceDetail = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropObjectName : String; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
  published
    // Unit Price of the item. By default UnitPrice is returned to two decimal places. You can use 4 decimal places by adding the unitdp=4 querystring parameter to your request.
    property UnitPrice : Currency index xipdfUnitPrice read rCurrencyVal write wCurrencyVal;
    // Default account code to be used for purchased/sale. Not applicable to the purchase details of tracked items
    property AccountCode : String index xipdfAccountCode read rStringVal write wStringVal;
    // Cost of goods sold account. Only applicable to the purchase details of tracked items.
    property COGSAccountCode : String index xipdfCOGSAccountCode read rStringVal write wStringVal;
    // Used as an override if the default Tax Code for the selected AccountCode is not correct - see TaxTypes.
    property TaxType : String index xipdfTaxType read rStringVal write wStringVal;
  end;

  TXEROPurchaseDetails = TXEROItemPriceDetail;
  TXEROSalesDetails = TXEROItemPriceDetail;

  TXEROItemField = (
    xifItemID, xifCode, xifName, xifIsSold, xifIsPurchased, xifDescription,
    xifPurchaseDescription, xifPurchaseDetails, xifSalesDetails,
    xifIsTrackedAsInventory, xifInventoryAssetAccountCode, xifTotalCostPool,
    xifQuantityOnHand, xifStockUpdatedDateUTC
    );
  TXEROItem = class(TXEROObject)
  public
    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;
    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;
    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;
    class function PropFieldID( AFieldName : String) : integer; override;
    class function PropObjectName : String; override;
    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;
  protected
    FPurchaseDetails : TXEROPurchaseDetails;
    FSalesDetails : TXEROSalesDetails;
    function rPurchaseDetails : TXEROPurchaseDetails;
    procedure wPurchaseDetails(ANewVal : TXEROPurchaseDetails);
    function rSalesDetails : TXEROSalesDetails;
    procedure wSalesDetails(ANewVal : TXEROSalesDetails);
    function GetObject(field : Integer; Access : TXEROFieldAccessMode) :TXEROObject; override;

    // Can output a conditional field.
    function CanOutField( Field : integer; mode : TXEROSerialiseOutputMode) : boolean; override;
  public
    procedure BeforeDestruction; override;
  published
    // Xero generated identifier for an item
    property ItemID : String index xifItemID read rStringVal write wStringVal;
    // User defined item code
    property Code : String index xifCode read rStringVal write wStringVal;
    // The name of the item
    property ItemName : String index xifName read rStringVal write wStringVal;
    // Boolean value. When IsSold is true the item will be available on sales transactions in the Xero UI.
    property IsSold : Boolean index xifIsSold read rBooleanVal write wBooleanVal;
    // Boolean value. When IsPurchased is true the item is available for purchase transactions in the Xero UI.
    property IsPurchased : Boolean index xifIsPurchased read rBooleanVal write wBooleanVal;
    // The sales description of the item
    property Description : String index xifDescription read rStringVal write wStringVal;
    // The purchase description of the item
    property PurchaseDescription : String index xifPurchaseDescription read rStringVal write wStringVal;
    // See Purchases & Sales. The PurchaseDetails element can contain a number of individual sub-elements.
    property PurchaseDetails : TXEROPurchaseDetails read rPurchaseDetails write wPurchaseDetails;
    // See Purchases & Sales. The SalesDetails element can contain a number of individual sub-elements.
    property SalesDetails : TXEROSalesDetails read rSalesDetails write wSalesDetails;
    // True for items that are tracked as inventory. An item will be tracked as inventory if the InventoryAssetAccountCode and COGSAccountCode are set.
    property IsTrackedAsInventory : Boolean index xifIsTrackedAsInventory read rBooleanVal write wBooleanVal;
    // The inventory asset account for the item. The account must be of type INVENTORY. The COGSAccountCode in PurchaseDetails is also required to create a tracked item
    property InventoryAssetAccountCode : String index xifInventoryAssetAccountCode read rStringVal write wStringVal;
    // The value of the item on hand. Calculated using average cost accounting.
    property TotalCostPool : Currency index xifTotalCostPool read rCurrencyVal write wCurrencyVal;
    // The quantity of the item on hand
    property QuantityOnHand : Currency index xifQuantityOnHand read rCurrencyVal write wCurrencyVal;
    // Last modified date in UTC format
    property UpdatedDateUTC : TDateTime index xifStockUpdatedDateUTC read rDateTimeVal write wDateTimeVal;
  end;
  TXEROItemList = class(TXEROObjectList<TXEROItem>)
  public
    class function PropListName : String; override;
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
    function BuildURI( const APath, ADoc : String; const AParams : TRestParams; AActionType : THttpActionType = hatGet) : String; overload;
    function BuildURI( const APath, ADoc : String;const AParams : TRestParams; const AQuery : String; AOrder : TXEROOrder;  AActionType : THttpActionType; Page : Integer) : String; overload;
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

    // Load Tax rate settings
    function GetTaxRates(taxRateList : TXEROTaxRateList; const AQuery : String = '') : boolean; overload;
    function GetTaxRates(taxRateList : TXEROTaxRateList; const AQuery : String; const AOrder : TXEROOrder; APageNo : integer = -1) : boolean; overload;
    function GetTaxRate(taxRate : TXeroTaxRate; const ATaxType : String) : boolean;

    // Update a tax rate
    function StoreTaxRate(taxRate: TXeroTaxRate) : boolean;

    // Contacts.
    function GetContacts(AContactList : TXEROContactList; const AQuery : String = '') : boolean; overload;
    function GetContacts(AContactList : TXEROContactList; const AQuery : String; const AOrder : TXEROOrder; APageNo : Integer = -1 ) : boolean; overload;
    function GetContact(AContact : TXEROContact; const AContactUID : String) : boolean;

    function StoreContact(AContact : TXEROContact) : boolean;
    function StoreContacts(AContactList, responseList : TXEROContactList) : boolean;

    // Invoices
    function GetInvoices(AInvoiceList : TXEROInvoiceList; const AQuery : String = '') : boolean; overload;
    function GetInvoices(AInvoiceList : TXEROInvoiceList; const AQuery : String; const AOrder : TXEROOrder; APageNo : Integer = -1 ) : boolean; overload;
    function GetInvoice(AInvoice : TXEROInvoice; const AInvoiceUID : String) : boolean;

    {: Store an invoice.
      Invoice is updated from call.
      @param AInvoice Invoice to Store
      @param AUnitDp  Decimal Points for 'Unit Cost' (defaults to 2)
    }
    function StoreInvoice(AInvoice : TXEROInvoice; AUnitDp : integer =-1 ) : boolean;
    {: Store multiple invoices.
      @param AInvoicList Invoices to Store
      @param responseList  Invoices returned from store operation.
      @param AUnitDp  Decimal Points for 'Unit Cost' (defaults to 2)
    }
    function StoreInvoices(AInvoiceList, responseList : TXEROInvoiceList; AUnitDp : integer =-1 ) : boolean;

    // Items (stock)
    function GetItems(AItemList : TXEROItemList; const AQuery : String = '') : boolean; overload;
    function GetItems(AItemList : TXEROItemList; const AQuery : String; const AOrder : TXEROOrder; APageNo : Integer = -1 ) : boolean; overload;
    {: Retrieve Item by ItemID or Code.
    }
    function GetItem(AItem : TXEROItem; const AItemUID : String) : boolean;

    function StoreItem(AItem : TXEROItem) : boolean;
    function StoreItems(AItemList, responseList : TXEROItemList) : boolean;

    // CreditNotes
    function GetCreditNotes(ACreditNoteList : TXEROCreditNoteList; const AQuery : String = '') : boolean; overload;
    function GetCreditNotes(ACreditNoteList : TXEROCreditNoteList; const AQuery : String; const AOrder : TXEROOrder; APageNo : Integer = -1 ) : boolean; overload;
    function GetCreditNote(ACreditNote : TXEROCreditNote; const ACreditNoteUID : String) : boolean;

    function StoreCreditNote(ACreditNote : TXEROCreditNote) : boolean;
    function StoreCreditNotes(ACreditNoteList, responseList : TXEROCreditNoteList) : boolean;

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

    procedure StartMyElement; override;

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
    ( PropType: xptList;       PropName: 'Addresses';              PropDefault: ''; PropUsage : []; ),
    ( PropType: xptList;       PropName: 'Phones';                 PropDefault: ''; PropUsage : []; ),
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
  ( PropType: xptEnum;     PropName: 'LineAmountTypes';          PropDefault: ''; PropUsage: [xpuNew, xpuUpdate];),
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
CTaxComponentProperties : array[ord(low(TXEROTaxComponentField))..ord(high(TXEROTaxComponentField))] of TXEROPropertyEntry = (
  ( PropType: xptString;  PropName: 'Name';            PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptCurrency;PropName: 'Rate';            PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptboolean; PropName: 'IsCompound';      PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptboolean; PropName: 'IsNonRecoverable';PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate];)
);
CTaxRateProperties  : array[ord(low(TXEROTaxRateField))..ord(high(TXEROTaxRateField))] of TXEROPropertyEntry = (
  ( PropType: xptString;  PropName: 'Name';           PropDefault: '';       PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptString;  PropName: 'TaxType';        PropDefault: '';       PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptList;    PropName: 'TaxComponents';         PropDefault: '';       PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptEnum;    PropName: 'Status';                PropDefault: 'ACTIVE'; PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptString;  PropName: 'ReportTaxType';         PropDefault: '';       PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptBoolean; PropName: 'CanApplyToAssets';      PropDefault: '';       PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptBoolean; PropName: 'CanApplyToEquity';      PropDefault: '';       PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptBoolean; PropName: 'CanApplyToExpenses';    PropDefault: '';       PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptBoolean; PropName: 'CanApplyToLiabilities'; PropDefault: '';       PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptBoolean; PropName: 'CanApplyToRevenue';     PropDefault: '';       PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptCurrency;PropName: 'DisplayTaxRate';        PropDefault: '';       PropUsage: [xpuReqNew, xpuReqUpdate];),
  ( PropType: xptCurrency; PropName: 'EffectiveRate';        PropDefault: '';       PropUsage: [xpuReqNew, xpuReqUpdate];)
);

CInvoicesProperties : array[ord(low(TXEROInvoicesField))..ord(high(TXEROInvoicesField))] of TXEROPropertyEntry = (
( PropType: xptEnum;     PropName: 'Type';                PropDefault: ''; PropUsage : [xpuReqNew, xpuReqUpdate]; ), // See Invoice Types
( PropType: xptObject;   PropName: 'Contact';             PropDefault: ''; PropUsage : [xpuReqNew, xpuReqUpdate]; ), // See Contacts
( PropType: xptDateTime; PropName: 'Date';                PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Date invoice was issued - YYYY-MM-DD
( PropType: xptDateTime; PropName: 'DueDate';             PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Date invoice is due - YYYY-MM-DD
( PropType: xptEnum;     PropName: 'Status';              PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // See Invoice Status Codes
( PropType: xptEnum;     PropName: 'LineAmountTypes';     PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // See Line Amount Types
( PropType: xptList;     PropName: 'LineItems';           PropDefault: ''; PropUsage : [xpuReqNew, xpuReqUpdate]; ), // See LineItems. The LineItems collection can contain any number of individual LineItem sub-elements.
( PropType: xptCurrency; PropName: 'SubTotal';            PropDefault: ''; PropUsage : []; ), // Total of invoice excluding taxes
( PropType: xptCurrency; PropName: 'TotalTax';            PropDefault: ''; PropUsage : []; ), // Total tax on invoice
( PropType: xptCurrency; PropName: 'Total';               PropDefault: ''; PropUsage : []; ), // Total of Invoice tax inclusive (i.e. SubTotal + TotalTax)
( PropType: xptCurrency; PropName: 'TotalDiscount';       PropDefault: ''; PropUsage : []; ), // Total of discounts applied on the invoice line items
( PropType: xptDateTime; PropName: 'UpdatedDateUTC';      PropDefault: ''; PropUsage : []; ), // Last modified date UTC format
( PropType: xptString;   PropName: 'CurrencyCode';        PropDefault: ''; PropUsage : [xpuNew, xpuUpdate, xpuSkipBlank]; ), // The currency that invoice has been raised in (see Currencies)
( PropType: xptCurrency; PropName: 'CurrencyRate';        PropDefault: ''; PropUsage : [xpuNew, xpuUpdate, xpuSkipBlank]; ), // The currency rate for a multicurrency invoice
( PropType: xptString;   PropName: 'InvoiceID';           PropDefault: ''; PropUsage : [xpuReqUpdate]; ), // Xero generated unique identifier for invoice
( PropType: xptString;   PropName: 'InvoiceNumber';       PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // ACCREC - Unique alpha numeric code identifying invoice .. ACCPAY - non-unique alpha numeric code identifying invoice. This value will also display as Reference in the UI.

( PropType: xptString;   PropName: 'Reference';           PropDefault: ''; PropUsage : [xpuNew, xpuUpdate, xpuSkipBlank]; ), // ACCREC only - additional reference number
( PropType: xptString;   PropName: 'BrandingThemeID';     PropDefault: ''; PropUsage : [xpuNew, xpuUpdate, xpuSkipBlank]; ), // See BrandingThemes
( PropType: xptString;   PropName: 'Url';                 PropDefault: ''; PropUsage : [xpuNew, xpuUpdate, xpuSkipBlank]; ), // URL link to a source document - shown as "Go to [appName]" in the Xero app
( PropType: xptBoolean;  PropName: 'SentToContact';       PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Boolean to indicate whether the invoice in the Xero app displays as "sent"
( PropType: xptDateTime; PropName: 'ExpectedPaymentDate'; PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Shown on sales invoices (Accounts Receivable) when this has been set
( PropType: xptDateTime; PropName: 'PlannedPaymentDate';  PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Shown on bills (Accounts Payable) when this has been set
( PropType: xptBoolean;  PropName: 'HasAttachments';      PropDefault: ''; PropUsage : []; ), // boolean to indicate if an invoice has an attachment
( PropType: xptList;     PropName: 'Payments';            PropDefault: ''; PropUsage : []; ), // See Payments
( PropType: xptList;     PropName: 'CreditNotes';         PropDefault: ''; PropUsage : []; ), // Details of credit notes that have been applied to an invoice
( PropType: xptList;     PropName: 'Prepayments';         PropDefault: ''; PropUsage : []; ), // See Prepayments
( PropType: xptList;     PropName: 'Overpayments';        PropDefault: ''; PropUsage : []; ), // See Overpayments
( PropType: xptCurrency; PropName: 'AmountDue';           PropDefault: ''; PropUsage : []; ), // Amount remaining to be paid on invoice
( PropType: xptCurrency; PropName: 'AmountPaid';          PropDefault: ''; PropUsage : []; ), // Sum of payments received for invoice
( PropType: xptCurrency; PropName: 'CISDeduction';        PropDefault: ''; PropUsage : []; ), // CISDeduction withheld by the contractor to be paid to HMRC on behalf of subcontractor (Available for organisations under UK Construction Industry Scheme)
( PropType: xptDateTime; PropName: 'FullyPaidOnDate';     PropDefault: ''; PropUsage : []; ), // The date the invoice was fully paid. Only returned on fully paid invoices
( PropType: xptCurrency; PropName: 'AmountCredited';      PropDefault: ''; PropUsage : []; ) // Sum of all credit notes, over-payments and pre-payments applied to invoice
);

CInvoiceLineItemProperties : array[ord(low(TXEROInvoiceLineItemField))..ord(high(TXEROInvoiceLineItemField))] of TXEROPropertyEntry = (
( PropType: xptString;   PropName: 'Description';  PropDefault: ''; PropUsage : [xpuReqNew, xpuReqUpdate]; ), // The description of the line item
( PropType: xptCurrency; PropName: 'Quantity';     PropDefault: ''; PropUsage : [xpuReqNew, xpuReqUpdate]; ), // LineItem Quantity
( PropType: xptCurrency; PropName: 'UnitAmount';   PropDefault: ''; PropUsage : [xpuReqNew, xpuReqUpdate]; ), // Lineitem unit amount. By default, unit amount will be rounded to two decimal places. You can opt in to use four decimal places by adding the querystring parameter unitdp=4 to your query. See the Rounding in Xero guide for more information.
( PropType: xptString;   PropName: 'ItemCode';     PropDefault: ''; PropUsage : [xpuReqNew, xpuReqUpdate]; ), // See Items
( PropType: xptString;   PropName: 'AccountCode';  PropDefault: ''; PropUsage : [xpuReqNew, xpuReqUpdate]; ), // See Accounts
( PropType: xptString;   PropName: 'LineItemID';   PropDefault: ''; PropUsage : [xpuReqUpdate]; ), // The Xero generated identifier for a LineItem
( PropType: xptString;   PropName: 'TaxType';      PropDefault: ''; PropUsage : [xpuNew, xpuUpdate,xpuSkipBlank]; ), // Used as an override if the default Tax Code for the selected AccountCode is not correct - see TaxTypes.
( PropType: xptCurrency; PropName: 'TaxAmount';    PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // The tax amount is auto calculated as a percentage of the line amount based on the tax rate
( PropType: xptCurrency; PropName: 'LineAmount';   PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // The line amount reflects the discounted price if a DiscountRate has been used i.e LineAmount = Quantity * Unit Amount * ((100 - DiscountRate)/100)
( PropType: xptCurrency; PropName: 'DiscountRate'; PropDefault: ''; PropUsage : [xpuNew, xpuUpdate, xpuSkipBlank]; ), // Percentage discount being applied to a line item (only supported on ACCREC invoices - ACC PAY invoices and credit notes in Xero do not support discounts
( PropType: xptList{TXEROTrackingCategoryOptionList};PropName: 'Tracking';PropDefault: ''; PropUsage : [xpuNew, xpuUpdate, xpuSkipBlank]; ) // Section for optional Tracking Category - see TrackingCategory. Any LineItem can have a maximum of 2 TrackingCategory elements.
);

// The following elements are returned in a GET BrandingThemes response
CBrandingThemeProperties  : array[ord(low(TXEROBrandingThemeField))..ord(high(TXEROBrandingThemeField))] of TXEROPropertyEntry = (
( PropType: xptString;   PropName: 'BrandingThemeID'; PropDefault: ''; PropUsage : [xpuReqUpdate]; ), // Xero identifier
( PropType: xptString;   PropName: 'Name';            PropDefault: ''; PropUsage : [xpuReqNew, xpuReqUpdate]; ), // Name of branding theme
( PropType: xptString;   PropName: 'SortOrder';       PropDefault: ''; PropUsage : [xpuReqNew, xpuReqUpdate]; ), // Integer - ranked order of branding theme. The default branding theme has a value of 0
( PropType: xptDateTime; PropName: 'CreatedDateUTC';  PropDefault: ''; PropUsage : []; ) // UTC timestamp of creation date of branding theme
);

CContactProperties :  array[ord(low(TXEROContactField))..ord(high(TXEROContactField))] of TXEROPropertyEntry = (
( PropType: xptString;   PropName: 'ContactID';                 PropDefault: ''; PropUsage : [xpuReqUpdate]; ), // Xero identifier
( PropType: xptString;   PropName: 'ContactNumber';             PropDefault: ''; PropUsage : [xpuNew, xpuUpdate, xpuSkipBlank]; ), // This field is read only in the Xero UI, used to identify contacts in external systems. It is displayed as Contact Code in the Contacts UI in Xero.
( PropType: xptString;   PropName: 'AccountNumber';             PropDefault: ''; PropUsage : [xpuNew, xpuUpdate, xpuSkipBlank]; ), // A user defined account number. This can be updated via the API and the Xero UI
( PropType: xptEnum;     PropName: 'ContactStatus';             PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Current status of a contact - see contact status types
( PropType: xptString;   PropName: 'Name';                      PropDefault: ''; PropUsage : [xpuReqNew, xpuReqUpdate]; ), // Full name of contact/organisation
( PropType: xptString;   PropName: 'FirstName';                 PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // First name of contact person
( PropType: xptString;   PropName: 'LastName';                  PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Last name of contact person
( PropType: xptString;   PropName: 'EmailAddress';              PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Email address of contact person
( PropType: xptString;   PropName: 'SkypeUserName';             PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Skype user name of contact
( PropType: xptString;   PropName: 'BankAccountDetails';        PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Bank account number of contact
( PropType: xptString;   PropName: 'TaxNumber';                 PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Tax number of contact - this is also known as the ABN (Australia), GST Number (New Zealand), VAT Number (UK) or Tax ID Number (US and global) in the Xero UI depending on which regionalized version of Xero you are using
( PropType: xptString;   PropName: 'AccountsReceivableTaxType'; PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Default tax type used for contact on AR invoices
( PropType: xptString;   PropName: 'AccountsPayableTaxType';    PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Default tax type used for contact on AP invoices

( PropType: xptList;     PropName: 'Addresses';                 PropDefault: ''; PropUsage : [xpuNew, xpuUpdate, xpuSkipBlank]; ), // Store certain address types for a contact - see address types
( PropType: xptList;     PropName: 'Phones';                    PropDefault: ''; PropUsage : [xpuNew, xpuUpdate, xpuSkipBlank]; ), // Store certain phone types for a contact - see phone types

( PropType: xptBoolean;  PropName: 'IsSupplier';                PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // true or false - Boolean that describes if a contact that has any AP invoices entered against them
( PropType: xptBoolean;  PropName: 'IsCustomer';                PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // true or false - Boolean that describes if a contact has any AR invoices entered against them
( PropType: xptString;   PropName: 'DefaultCurrency';           PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Default currency for raising invoices against contact
( PropType: xptDateTime; PropName: 'UpdatedDateUTC';            PropDefault: ''; PropUsage : []; ), // UTC timestamp of last update to contact
// The following are only retrieved on GET requests for a single contact or when pagination is used
( PropType: xptList;     PropName: 'ContactPersons';              PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // See contact persons. A contact can have a maximum of 5 ContactPersons
( PropType: xptString;   PropName: 'XeroNetworkKey';              PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Store XeroNetworkKey for contacts.
( PropType: xptString;   PropName: 'SalesDefaultAccountCode';     PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // The default sales account code for contacts
( PropType: xptString;   PropName: 'PurchasesDefaultAccountCode'; PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // The default purchases account code for contacts
( PropType: xptList;     PropName: 'SalesTrackingCategories';     PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // The default sales tracking categories for contacts
( PropType: xptList;     PropName: 'PurchasesTrackingCategories'; PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // The default purchases tracking categories for contacts
( PropType: xptString;   PropName: 'TrackingCategoryName';        PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // The name of the Tracking Category assigned to the contact under SalesTrackingCategories and PurchasesTrackingCategories
( PropType: xptString;   PropName: 'TrackingOptionName';          PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // The name of the Tracking Option assigned to the contact under SalesTrackingCategories and PurchasesTrackingCategories
( PropType: xptEnum;     PropName: 'PaymentTerms';                PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // The default payment terms for the contact - see Payment Terms
( PropType: xptString;   PropName: 'ContactGroups';               PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Displays which contact groups a contact is included in
( PropType: xptString;   PropName: 'Website';                     PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Website address for contact
( PropType: xptObject;   PropName: 'BrandingTheme';               PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // Default branding theme for contact - see Branding Themes
( PropType: xptString;   PropName: 'BatchPayments';               PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // batch payment details for contact
( PropType: xptCurrency; PropName: 'Discount';                    PropDefault: ''; PropUsage : [xpuNew, xpuUpdate]; ), // The default discount rate for the contact
( PropType: xptString;   PropName: 'Balances';                    PropDefault: ''; PropUsage : []; ), // The raw AccountsReceivable(sales invoices) and AccountsPayable(bills) outstanding and overdue amounts, not converted to base currency
( PropType: xptBoolean;  PropName: 'HasAttachments';              PropDefault: ''; PropUsage : []; ) // A boolean to indicate if a contact has an attachment
);

// Elements for ContactPerson
CContactPersonProperties :  array[ord(low(TXEROContactPersonField))..ord(high(TXEROContactPersonField))] of TXEROPropertyEntry = (
( PropType: xptString;     PropName: 'FirstName';       PropDefault: ''; PropUsage : [xpuReqNew, xpuReqUpdate]; ), // First name of person
( PropType: xptString;     PropName: 'LastName';        PropDefault: ''; PropUsage : [xpuReqNew, xpuReqUpdate]; ), // Last name of person
( PropType: xptString;     PropName: 'EmailAddress';    PropDefault: ''; PropUsage : [xpuReqNew, xpuReqUpdate]; ), // Email address of person
( PropType: xptBoolean;    PropName: 'IncludeInEmails'; PropDefault: ''; PropUsage : [xpuReqNew, xpuReqUpdate]; ) // boolean to indicate whether contact should be included on emails with invoices etc.
);

// Elements for Item
CItemProperties :  array[ord(low(TXEROItemField))..ord(high(TXEROItemField))] of TXEROPropertyEntry = (
( PropType: xptString;     PropName: 'ItemID';                   PropDefault: ''; PropUsage: [xpuReqUpdate]; ), // Xero generated identifier for an item
( PropType: xptString;     PropName: 'Code';                     PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ), // User defined item code
( PropType: xptString;     PropName: 'Name';                     PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ), // The name of the item
( PropType: xptBoolean;    PropName: 'IsSold';                   PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ), // Boolean value. When IsSold is true the item will be available on sales transactions in the Xero UI.
( PropType: xptBoolean;    PropName: 'IsPurchased';              PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ), // Boolean value. When IsPurchased is true the item is available for purchase transactions in the Xero UI.
( PropType: xptString;     PropName: 'Description';              PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank]; ), // The sales description of the item
( PropType: xptString;     PropName: 'PurchaseDescription';      PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank]; ), // The purchase description of the item
( PropType: xptObject;     PropName: 'PurchaseDetails';          PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuConditional]; ), // See Purchases & Sales. The PurchaseDetails element can contain a number of individual sub-elements.
( PropType: xptObject;     PropName: 'SalesDetails';             PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuConditional]; ), // See Purchases & Sales. The SalesDetails element can contain a number of individual sub-elements.
( PropType: xptBoolean;    PropName: 'IsTrackedAsInventory';     PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ), // True for items that are tracked as inventory. An item will be tracked as inventory if the InventoryAssetAccountCode and COGSAccountCode are set.
( PropType: xptString;     PropName: 'InventoryAssetAccountCode';PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ), // The inventory asset account for the item. The account must be of type INVENTORY. The COGSAccountCode in PurchaseDetails is also required to create a tracked item
( PropType: xptCurrency;   PropName: 'TotalCostPool';            PropDefault: ''; PropUsage: []; ), // The value of the item on hand. Calculated using average cost accounting.
( PropType: xptCurrency;   PropName: 'QuantityOnHand';           PropDefault: ''; PropUsage: []; ), // The quantity of the item on hand
( PropType: xptDateTime;   PropName: 'UpdatedDateUTC';           PropDefault: ''; PropUsage: []; ) // Last modified date in UTC format
);

// Detail Elements for Purchases and Sales
CItemPriceDetailProperties :  array[ord(low(TXEROItemPriceDetailField))..ord(high(TXEROItemPriceDetailField))] of TXEROPropertyEntry = (
( PropType: xptCurrency;   PropName: 'UnitPrice';        PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ), // Unit Price of the item. By default UnitPrice is returned to two decimal places. You can use 4 decimal places by adding the unitdp=4 querystring parameter to your request.
( PropType: xptString;     PropName: 'AccountCode';      PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ), // Default account code to be used for purchased/sale. Not applicable to the purchase details of tracked items
( PropType: xptString;     PropName: 'COGSAccountCode';  PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ), // Cost of goods sold account. Only applicable to the purchase details of tracked items.
( PropType: xptString;     PropName: 'TaxType';          PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ) // Used as an override if the default Tax Code for the selected AccountCode is not correct - see TaxTypes.
);

CPaymentProperties :  array[ord(low(TXEROPaymentField))..ord(high(TXEROPaymentField))] of TXEROPropertyEntry = (
  ( PropType: xptString;     PropName: 'PaymentID';      PropDefault: ''; PropUsage: [xpuReqUpdate]; ), // Ident of payment
  ( PropType: xptDateTime;   PropName: 'Date';           PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ), // Date the payment is being made (YYYY-MM-DD) e.g. 2009-09-06
  ( PropType: xptCurrency;   PropName: 'CurrencyRate';   PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ), // Exchange rate when payment is received. Only used for non base currency invoices and credit notes e.g. 0.7500
  ( PropType: xptCurrency;   PropName: 'Amount';         PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ), // The amount of the payment. Must be less than or equal to the outstanding amount owing on the invoice e.g. 200.00
  ( PropType: xptString;     PropName: 'Reference';      PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ), // An optional description for the payment e.g. Direct Debit
  ( PropType: xptBoolean;    PropName: 'IsReconciled';   PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ), // An optional parameter for the payment. Conversion related apps can utilise the IsReconciled flag in scenarios when a matching bank statement line is not available. Learn more
  ( PropType: xptEnum;       PropName: 'Status';         PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ), // The status of the payment.
  ( PropType: xptEnum;       PropName: 'PaymentType';    PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ), // See Payment Types.
  ( PropType: xptDateTime;   PropName: 'UpdatedDateUTC'; PropDefault: ''; PropUsage: []; ), // UTC timestamp of last update to the payment
  ( PropType: xptObject;     PropName: 'Account';        PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ), // The Account the payment was made from
  ( PropType: xptObject;     PropName: 'Invoice';        PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ), // The Invoice the payment was made against
  ( PropType: xptObject;     PropName: 'CreditNote';     PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ), // The Credit Note the payment was made against
  ( PropType: xptObject;     PropName: 'Prepayments';    PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ), // The Prepayment the payment was made against
  ( PropType: xptObject;     PropName: 'Overpayment';    PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; )  // The Overpayment the payment was made against
);

CCreditNoteProperties :  array[ord(low(TXEROCreditNoteField))..ord(high(TXEROCreditNoteField))] of TXEROPropertyEntry = (
( PropType: xptString;     PropName: 'CreditNoteID';    PropDefault: ''; PropUsage: [xpuReqUpdate]; ), // Xero generated unique identifier
( PropType: xptEnum;       PropName: 'Type';            PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ), // See Credit Note Types
( PropType: xptObject;     PropName: 'Contact';         PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ), // See Contacts
( PropType: xptDateTime;   PropName: 'Date';            PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ), // The date the credit note is issued YYYY-MM-DD. If the Date element is not specified then it will default to the current date based on the timezone setting of the organisation
( PropType: xptEnum;       PropName: 'Status';          PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ), // See Credit Note Status Codes
( PropType: xptEnum;       PropName: 'LineAmountTypes'; PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ), // See Invoice Line Amount Types
( PropType: xptList;       PropName: 'LineItems';       PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ), // See Invoice Line Items
( PropType: xptCurrency;   PropName: 'SubTotal';        PropDefault: ''; PropUsage: []; ), // The subtotal of the credit note excluding taxes
( PropType: xptCurrency;   PropName: 'TotalTax';        PropDefault: ''; PropUsage: []; ), // The total tax on the credit note
( PropType: xptCurrency;   PropName: 'Total';           PropDefault: ''; PropUsage: []; ), // The total of the Credit Note(subtotal + total tax)
( PropType: xptCurrency;   PropName: 'CISDeduction';    PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank]; ), // CISDeduction withheld by the contractor to be paid to HMRC on behalf of subcontractor (Available for organisations under UK Construction Industry Scheme)
( PropType: xptDateTime;   PropName: 'UpdatedDateUTC';  PropDefault: ''; PropUsage: []; ), // UTC timestamp of last update to the credit note
( PropType: xptString;     PropName: 'CurrencyCode';    PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank]; ), // Currency used for the Credit Note
( PropType: xptDateTime;   PropName: 'FullyPaidOnDate'; PropDefault: ''; PropUsage: []; ), // Date when credit note was fully paid(UTC format)
( PropType: xptString;     PropName: 'CreditNoteNumber';PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank]; ), // ACCRECCREDIT - Unique alpha numeric code identifying credit note.  ACCPAYCREDIT - Non-unique alpha numeric code identifying credit note. This value will also display as Reference in the UI.
( PropType: xptString;     PropName: 'Reference';       PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank]; ), // ACCRECCREDIT only - additional reference number
( PropType: xptBoolean;    PropName: 'SentToContact';   PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ), // boolean to indicate if a credit note has been sent to a contact via the Xero app (currently read only)
( PropType: xptCurrency;   PropName: 'CurrencyRate';    PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank]; ), // The currency rate for a multicurrency invoice. If no rate is specified, the XE.com day rate is used
( PropType: xptCurrency;   PropName: 'RemainingCredit'; PropDefault: ''; PropUsage: []; ), // The remaining credit balance on the Credit Note
( PropType: xptString;     PropName: 'Allocations';     PropDefault: ''; PropUsage: []; ), // See Allocations
( PropType: xptString;     PropName: 'BrandingThemeID'; PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank]; ), // See BrandingThemes
( PropType: xptBoolean;    PropName: 'HasAttachments';  PropDefault: ''; PropUsage: []; )  // boolean to indicate if a credit note has an attachment
);

CAddressProperties :  array[ord(low(TXEROAddressField))..ord(high(TXEROAddressField))] of TXEROPropertyEntry = (
( PropType: xptEnum;       PropName: 'AddressType';     PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ),
( PropType: xptString;     PropName: 'AddressLine1';    PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ),
( PropType: xptString;     PropName: 'AddressLine2';    PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank]; ),
( PropType: xptString;     PropName: 'AddressLine3';    PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank]; ),
( PropType: xptString;     PropName: 'AddressLine4';    PropDefault: ''; PropUsage: [xpuNew, xpuUpdate, xpuSkipBlank]; ),
( PropType: xptString;     PropName: 'City';            PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ),
( PropType: xptString;     PropName: 'Region';          PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ),
( PropType: xptString;     PropName: 'PostalCode';      PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ),
( PropType: xptString;     PropName: 'Country';         PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; ),
( PropType: xptString;     PropName: 'AttentionTo';     PropDefault: ''; PropUsage: [xpuNew, xpuUpdate]; )
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
  G_ManualJournalLineMap,
  G_TaxComponentMap,
  G_TaxRateMap,
  G_InvoicesMap,
  G_InvoiceLineItemMap,
  G_BrandingThemeMap,
  G_ContactMap,
  G_ContactPersonMap,
  G_ItemMap,
  G_ItemPriceDetailMap,
  G_PaymentMap,
  G_CreditNoteMap,
  G_AddressMap
  : TXEROPropertyMap;


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
  CXEROAccountType : array [TXEROAccountType] of string = (
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

  CXEROInvoicesStatus : array [TXEROInvoicesStatus] of string = (
    '', 'DRAFT', 'SUBMITTED', 'DELETED', 'AUTHORISED', 'PAID', 'VOIDED'
  );

  CXEROInvoicesType : array[TXEROInvoicesType ] of string = (
    '', 'ACCPAY', 'ACCREC'
  );

  // Credit note has same options as invoice, but with 'CREDIT' suffix
  CXEROCreditNoteType : array[TXEROInvoicesType] of string = (
    '', 'ACCPAYCREDIT', 'ACCRECCREDIT'
  );

  CXEROCreditNoteStatus : array[TXEROCreditNoteStatus] of string = (
  '', 'SUBMITTED', 'AUTHORISED', 'PAID'
  );

  CXEROLineAmountTypes: array[TXEROLineAmountTypes] of string = (
    '', 'NoTax', 'Exclusive', 'Inclusive'
  );

  CXEROContactStatus : array[TXEROContactStatus] of string = (
    '', 'ACTIVE', 'ARCHIVED', 'GDPRREQUEST'
  );
  CXEROPaymentTerms : array[TXEROPaymentTerms] of string = (
    '', 'DAYSAFTERBILLDATE', 'DAYSAFTERBILLMONTH', 'OFCURRENTMONTH'
  );


  CXEROPaymentType : array[TXEROPaymentType] of string = (
    '',
    'ACCRECPAYMENT',        'ACCPAYPAYMENT',
    'ARCREDITPAYMENT',      'APCREDITPAYMENT',
    'AROVERPAYMENTPAYMENT', 'ARPREPAYMENTPAYMENT',
    'APPREPAYMENTPAYMENT',  'APOVERPAYMENTPAYMENT'
  );

  CXEROPaymentStatus : array[TXEROPaymentStatus] of string= (
  '', 'AUTHORISED', 'DELETED'
  );

  CXEROAddressType : array[TXEROAddressType] of string = (
    '', 'POBOX', 'STREET','DELIVERY'
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
    if CompareText(stg, CXEROAccountType[idx]) = 0 then
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
  result := CXEROAccountType[TXEROAccountType(enumVal)];
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

function XEROInvoicesStatusAsEnum(const stg : String) : TXEROInvoicesStatus;
var
  idx : TXEROInvoicesStatus;
begin
  result := low(TXEROInvoicesStatus);
  for idx := low(idx) to high(idx) do
  begin
    if CompareText(stg, CXEROInvoicesStatus[idx]) = 0 then
    begin
      result := idx;
      break;
    end;
  end;
end;
function XEROInvoicesStatusAsString(enumVal : integer) : String;
begin
  if (enumVal < 0) or (enumVal > ord(high(TXEROInvoicesStatus))) then
    enumVal := ord(low(TXEROInvoicesStatus));
  result := CXEROInvoicesStatus[TXEROInvoicesStatus(enumVal)];
end;

function XEROInvoicesTypeAsEnum(const stg : String) : TXEROInvoicesType;
var
  idx : TXEROInvoicesType;
begin
  result := low(TXEROInvoicesType);
  for idx := low(idx) to high(idx) do
  begin
    if CompareText(stg, CXEROInvoicesType[idx]) = 0 then
    begin
      result := idx;
      break;
    end;
  end;
end;
function XEROInvoicesTypeAsString(enumVal : integer) : String;
begin
  if (enumVal < 0) or (enumVal > ord(high(TXEROInvoicesType))) then
    enumVal := ord(low(TXEROInvoicesType));
  result := CXEROInvoicesType[TXEROInvoicesType(enumVal)];
end;

function XEROCreditNoteTypeAsEnum(const stg : String) : TXEROInvoicesType;
var
  idx : TXEROInvoicesType;
begin
  result := low(TXEROInvoicesType);
  for idx := low(idx) to high(idx) do
  begin
    if CompareText(stg, CXEROCreditNoteType[idx]) = 0 then
    begin
      result := idx;
      break;
    end;
  end;
end;
function XEROCreditNoteTypeAsString(enumVal : integer) : String;
begin
  if (enumVal < 0) or (enumVal > ord(high(TXEROInvoicesType))) then
    enumVal := ord(low(TXEROInvoicesType));
  result := CXEROCreditNoteType[TXEROInvoicesType(enumVal)];
end;

function XEROCreditNoteStatusAsEnum(const stg : String) : TXEROCreditNoteStatus;
var
  idx : TXEROCreditNoteStatus;
begin
  result := low(TXEROCreditNoteStatus);
  for idx := low(idx) to high(idx) do
  begin
    if CompareText(stg, CXEROCreditNoteStatus[idx]) = 0 then
    begin
      result := idx;
      break;
    end;
  end;
end;
function XEROCreditNoteStatusAsString(enumVal : integer) : String;
begin
  if (enumVal < 0) or (enumVal > ord(high(TXEROCreditNoteStatus))) then
    enumVal := ord(low(TXEROCreditNoteStatus));
  result := CXEROCreditNoteStatus[TXEROCreditNoteStatus(enumVal)];
end;

function XEROLineAmountTypesAsEnum(const stg : String) : TXEROLineAmountTypes;
var
  idx : TXEROLineAmountTypes;
begin
  result := low(TXEROLineAmountTypes);
  for idx := low(idx) to high(idx) do
  begin
    if CompareText(stg, CXEROLineAmountTypes[idx]) = 0 then
    begin
      result := idx;
      break;
    end;
  end;
end;
function XEROLineAmountTypesAsString(enumVal : integer) : String;
begin
  if (enumVal < 0) or (enumVal > ord(high(TXEROLineAmountTypes))) then
    enumVal := ord(low(TXEROLineAmountTypes));
  result := CXEROLineAmountTypes[TXEROLineAmountTypes(enumVal)];
end;

function XEROContactStatusAsEnum(const stg : String) : TXEROContactStatus;
var
  idx : TXEROContactStatus;
begin
  result := low(TXEROContactStatus);
  for idx := low(idx) to high(idx) do
  begin
    if CompareText(stg, CXEROContactStatus[idx]) = 0 then
    begin
      result := idx;
      break;
    end;
  end;
end;
function XEROContactStatusAsString(enumVal : integer) : String;
begin
  if (enumVal < 0) or (enumVal > ord(high(TXEROContactStatus))) then
    enumVal := ord(low(TXEROContactStatus));
  result := CXEROContactStatus[TXEROContactStatus(enumVal)];
end;

function XEROPaymentTermsAsEnum(const stg : String) : TXEROPaymentTerms;
var
  idx : TXEROPaymentTerms;
begin
  result := low(TXEROPaymentTerms);
  for idx := low(idx) to high(idx) do
  begin
    if CompareText(stg, CXEROPaymentTerms[idx]) = 0 then
    begin
      result := idx;
      break;
    end;
  end;
end;

function XEROPaymentTermsAsString(enumVal : integer) : String;
begin
  if (enumVal < 0) or (enumVal > ord(high(TXEROPaymentTerms))) then
    enumVal := ord(low(TXEROPaymentTerms));
  result := CXEROPaymentTerms[TXEROPaymentTerms(enumVal)];
end;

function XEROPaymentTypeAsEnum(const stg : String) : TXEROPaymentType;
var
  idx : TXEROPaymentType;
begin
  result := low(TXEROPaymentType);
  for idx := low(idx) to high(idx) do
  begin
    if CompareText(stg, CXEROPaymentType[idx]) = 0 then
    begin
      result := idx;
      break;
    end;
  end;
end;

function XEROPaymentTypeAsString(enumVal : integer) : String;
begin
  if (enumVal < 0) or (enumVal > ord(high(TXEROPaymentType))) then
    enumVal := ord(low(TXEROPaymentType));
  result := CXEROPaymentType[TXEROPaymentType(enumVal)];
end;

function XEROPaymentStatusAsEnum(const stg : String) : TXEROPaymentStatus;
var
  idx : TXEROPaymentStatus;
begin
  result := low(TXEROPaymentStatus);
  for idx := low(idx) to high(idx) do
  begin
    if CompareText(stg, CXEROPaymentStatus[idx]) = 0 then
    begin
      result := idx;
      break;
    end;
  end;
end;

function XEROPaymentStatusAsString(enumVal : integer) : String;
begin
  if (enumVal < 0) or (enumVal > ord(high(TXEROPaymentStatus))) then
    enumVal := ord(low(TXEROPaymentStatus));
  result := CXEROPaymentStatus[TXEROPaymentStatus(enumVal)];
end;

function XEROAddressTypeAsEnum( stg : String) : TXEROAddressType;
var
  idx : TXEROAddressType;
begin
  result := low(TXEROAddressType);
  for idx := low(idx) to high(idx) do
  begin
    if CompareText(stg, CXEROAddressType[idx]) = 0 then
    begin
      result := idx;
      break;
    end;
  end;
end;
function XEROAddressTypeAsString( enumVal : Integer) : String;
begin
  if (enumVal < 0) or (enumVal > ord(high(TXEROAddressType))) then
    enumVal := ord(low(TXEROAddressType));
  result := CXEROAddressType[TXEROAddressType(enumVal)];
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
  srcLines, dstLines : TXEROObjectListBase;
  obj, srcObj, dstObj : TXEROObject;
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
        xptObject:
          begin
            srcObj := obj.GetObject(idx, xfamRead);
            if assigned(srcObj) then
              GetObject(idx, xfamWrite).Assign(srcObj)
            else
            begin
              dstObj := GetObject(idx, xfamClear);
              if assigned(dstObj) then
                dstObj.Clear;
            end;
          end;
        xptList:
          begin
            srcLines := obj.GetListObject(idx, xfamRead);
            if assigned(srcLines) then
            begin
              dstLines := GetListObject(idx, xfamWrite);
              dstLines.Assign(srcLines);
            end
            else
            begin
              dstLines := GetListObject(idx, xfamClear);
              if assigned(dstLines) then
                dstLines.Clear;
            end;
          end;
      end;
    end;
    FModified := obj.FModified;
    wIsNew(obj.FIsNew);
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

function TXEROObject.GetObject(field : Integer; Access : TXEROFieldAccessMode) :TXEROObject;
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
                xptCurrency: display := rCurrencyVal(idx) <> 0;
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
                obj := GetObject(idx, xfamRead);
                if assigned(obj) then
                  obj.OutObject(writer, PropName, xpmNamed, mode)
                else if (mode = xsomDisplay) or not (xpuSkipBlank in PropUsage) then
                  writer.WritePropertyNull(propName);
              end;
            xptList:
              begin
                list := GetListObject(idx, xfamRead);
                if assigned(list) then
                  list.OutObject(writer, PropName, xpmNamed, mode)
                else if (mode = xsomDisplay) or not (xpuSkipBlank in PropUsage) then
                  writer.WritePropertyNull(propName);
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
  list : TXEROObjectListBase;
  obj : TXEROObject;
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
          xptList:
            begin
              list := GetListObject(idx, xfamClear);
              if assigned(list) then
                list.Clear;
            end;
          xptObject:
            begin
              obj := GetObject(idx, xfamClear);
              if Assigned(obj) then
                obj.Clear;
            end;
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

procedure TXEROLoaderGenericBase.StartMyElement;
begin
  if assigned(FCur) then
    FCur.Clear;

  inherited;

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
begin
  result := BuildURI(APath, ADoc, TRestParams.None, AQuery, AOrder, AActionType, Page);
end;

function TXEROConnect.BuildURI( const APath, ADoc : String;const AParams : TRestParams; const AQuery : String; AOrder : TXEROOrder;  AActionType : THttpActionType; Page : Integer) : String;
var
  Params : TRestParams;
begin
  params := AParams;
  if not IsEmptyString(AQuery) then
    Params.Add('where',AQuery);
  if not AOrder.IsEmpty then
    Params.Add('order', AOrder.AsString);
  if Page >= 0 then
    params.Add('page', IntToStr(page));
  result := BuildURI(APath, ADoc, Params, AActionType);
end;

function TXEROConnect.BuildURI( const APath, ADoc : String; const AParams : TRestParams; AActionType : THttpActionType = hatGet) : String;
var
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
    uri.Params := AParams.AsString;
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
    //try
      result := Get(AURI, AParams, respStream, ResponseCode, ErrorDetail, 0, rtJSON);
   // except

    if not result then
    begin
      // 404 response getting a single item means it's not there.
      if ResponseCode <> 404 then
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
        result := GetLoadSingleResult(BuildURI('accounts', AReferenceID, TRestParams.None, hatGet), nil, 'Accounts', loader);
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
    result := GetLoadSingleResult(BuildURI('', 'Organisations', TRestParams.None, hatGet), nil, 'Organisations', loader);
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
    result := GetLoadList(BuildURI('','accounts', TRestParams.None), nil, 'Accounts', loader);
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
      result := GetLoadSingleResult(BuildURI('accounts', AIdent, TRestParams.None, hatGet), nil, 'Accounts', loader);
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
    result := Store(BuildURI('', 'accounts',TRestParams.None, hatSend),
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
    result := StoreList(BuildURI('','accounts',TRestParams.None,hatSend),
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
      result := GetLoadSingleResult(BuildURI('manualjournals',AUID, TRestParams.None, hatGet), nil, 'ManualJournals', loader);
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
    result := Store(BuildURI('', 'manualjournals',TRestParams.None, hatSend),
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
    result := StoreList(BuildURI('','manualjournals',TRestParams.None,hatSend),
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
      result := GetLoadSingleResult(BuildURI('trackingcategories',AUID, TRestParams.None, hatGet), nil, 'TrackingCategories', loader);
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
    result := Store(BuildURI('', 'trackingcategories',TRestParams.None, hatSend),
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
    result := StoreList(BuildURI('','trackingcategories',TRestParams.None,hatSend),
      'TrackingCategories', 'TrackingCategory', categoryList, responselist, xsomUpdate);
end;

function TXEROConnect.GetTaxRates(taxRateList : TXEROTaxRateList; const AQuery : String = '') : boolean;
begin
  result := GetTaxRates(taxRateList, AQuery,  TXEROOrder.None, -1);
end;

function TXEROConnect.GetTaxRates(taxRateList : TXEROTaxRateList; const AQuery : String; const AOrder : TXEROOrder; APageNo : integer = -1) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := taxRateList.GetListLoader;
  try
    result := GetLoadList(BuildURI('','taxrates', AQuery, AOrder, hatGet, APageNo), nil, 'TaxRates', loader);
    if result then
    begin
      taxRateList.ResetModified;
      taxRateList.SetIsNew(false);
    end;
  finally
    loader.Free;
  end;
end;

function TXEROConnect.GetTaxRate(taxRate : TXeroTaxRate; const ATaxType : String) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := TXEROLoaderGenericBase.Create('TaxRate',taxRate);
  try
    if ATaxType = '' then
      result := false
    else
    begin
      taxRate.Clear;
      result := GetLoadSingleResult(BuildURI('taxrates',ATaxType, TRestParams.None, hatGet), nil, 'TaxRates', loader);
      if result then
      begin
        taxRate.ResetModified;
        taxRate.IsNew := false;
      end;
    end;
  finally
    loader.Free;
  end;
end;

function TXEROConnect.StoreTaxRate(taxRate: TXeroTaxRate) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := TXEROLoaderGenericBase.Create('TaxRate', taxRate);
  try
    result := Store(BuildURI('', 'taxrates',TRestParams.None, hatSend),
    'TaxRates', 'TaxRate', taxRate, xsomUpdate, loader);
  finally
    loader.Free;
  end;
end;

// Contacts.
function TXEROConnect.GetContacts(AContactList : TXEROContactList; const AQuery : String = '') : boolean;
begin
  result := GetContacts(AContactList, AQuery, TXEROOrder.None, -1);
end;

function TXEROConnect.GetContacts(AContactList : TXEROContactList; const AQuery : String; const AOrder : TXEROOrder; APageNo : Integer = -1 ) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := AContactList.GetListLoader;
  try
    result := GetLoadList(BuildURI('','contacts', AQuery, AOrder, hatGet, APageNo), nil, 'Contacts', loader);
    if result then
    begin
      AContactList.ResetModified;
      AContactList.SetIsNew(false);
    end;
  finally
    loader.Free;
  end;
end;

function TXEROConnect.GetContact(AContact : TXEROContact; const AContactUID : String) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := TXEROLoaderGenericBase.Create('Contact',AContact);
  try
    if AContactUID = '' then
      result := false
    else
    begin
      AContact.Clear;
      result := GetLoadSingleResult(BuildURI('contacts',AContactUID, TRestParams.None, hatGet), nil, 'Contacts', loader);
      if result then
      begin
        AContact.ResetModified;
        AContact.IsNew := false;
      end;
    end;
  finally
    loader.Free;
  end;
end;

function TXEROConnect.StoreContact(AContact : TXEROContact) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := TXEROLoaderGenericBase.Create('Contact', AContact);
  try
    result := Store(BuildURI('', 'contacts',TRestParams.None, hatSend),
      'Contacts', 'Contact', AContact, xsomUpdate, loader);
  finally
    loader.Free;
  end;
end;

function TXEROConnect.StoreContacts(AContactList, responseList : TXEROContactList) : boolean;
begin
  if AContactList.Count = 0 then
    result := true
  else
    result := StoreList(BuildURI('','contacts',TRestParams.None,hatSend),
      'Contacts', 'Contact', AContactList, responselist, xsomUpdate);
end;

// Invoices
function TXEROConnect.GetInvoices(AInvoiceList : TXEROInvoiceList; const AQuery : String = '') : boolean;
begin
  result := GetInvoices(AInvoiceList, AQuery, TXEROOrder.None, -1);
end;

function TXEROConnect.GetInvoices(AInvoiceList : TXEROInvoiceList; const AQuery : String; const AOrder : TXEROOrder; APageNo : Integer = -1 ) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := AInvoiceList.GetListLoader;
  try
    result := GetLoadList(BuildURI('','invoices', AQuery, AOrder, hatGet, APageNo), nil, 'Invoices', loader);
    if result then
    begin
      AInvoiceList.ResetModified;
      AInvoiceList.SetIsNew(false);
    end;
  finally
    loader.Free;
  end;
end;

function TXEROConnect.GetInvoice(AInvoice : TXEROInvoice; const AInvoiceUID : String) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := TXEROLoaderGenericBase.Create('Invoice',AInvoice);
  try
    if AInvoiceUID = '' then
      result := false
    else
    begin
      AInvoice.Clear;
      result := GetLoadSingleResult(BuildURI('invoices',AInvoiceUID, TRestParams.None, hatGet), nil, 'Invoices', loader);
      if result then
      begin
        AInvoice.ResetModified;
        AInvoice.IsNew := false;
      end;
    end;
  finally
    loader.Free;
  end;
end;

function TXEROConnect.StoreInvoice(AInvoice : TXEROInvoice; AUnitDp : integer =-1 ) : boolean;
var
  loader : TJSONSAXNestableHandler;
  params : TRestParams;
begin
  loader := TXEROLoaderGenericBase.Create('Invoice', AInvoice);
  try
    params.Clear;
    if AUnitDp >= 0 then
      params.Add('unitdp', IntToStr(AUnitDp));

    result := Store(BuildURI('', 'invoices', params, hatSend),
      'Invoices', 'Invoice', AInvoice, xsomUpdate, loader);
  finally
    loader.Free;
  end;
end;

function TXEROConnect.StoreInvoices(AInvoiceList, responseList : TXEROInvoiceList; AUnitDp : integer =-1 ) : boolean;
var
  params : TRestParams;
begin
  if AInvoiceList.Count = 0 then
    result := true
  else
  begin
    params.Clear;
    if AUnitDp >= 0 then
      params.Add('unitdp', IntToStr(AUnitDP));
    params.Add('SummarizeErrors','false');
    result := StoreList(BuildURI('','invoices',params,hatSend),
      'Invoices', 'Invoice', AInvoiceList, responselist, xsomUpdate);
  end;
end;

// CreditNotes
function TXEROConnect.GetCreditNotes(ACreditNoteList : TXEROCreditNoteList; const AQuery : String = '') : boolean;
begin
  result := GetCreditNotes(ACreditNoteList, AQuery, TXEROOrder.None, -1);
end;

function TXEROConnect.GetCreditNotes(ACreditNoteList : TXEROCreditNoteList; const AQuery : String; const AOrder : TXEROOrder; APageNo : Integer = -1 ) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := ACreditNoteList.GetListLoader;
  try
    result := GetLoadList(BuildURI('','creditnotes', AQuery, AOrder, hatGet, APageNo), nil, 'CreditNotes', loader);
    if result then
    begin
      ACreditNoteList.ResetModified;
      ACreditNoteList.SetIsNew(false);
    end;
  finally
    loader.Free;
  end;
end;

function TXEROConnect.GetCreditNote(ACreditNote : TXEROCreditNote; const ACreditNoteUID : String) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := TXEROLoaderGenericBase.Create('CreditNote',ACreditNote);
  try
    if ACreditNoteUID = '' then
      result := false
    else
    begin
      ACreditNote.Clear;
      result := GetLoadSingleResult(BuildURI('creditnotes',ACreditNoteUID, TRestParams.None, hatGet), nil, 'CreditNotes', loader);
      if result then
      begin
        ACreditNote.ResetModified;
        ACreditNote.IsNew := false;
      end;
    end;
  finally
    loader.Free;
  end;
end;

function TXEROConnect.StoreCreditNote(ACreditNote : TXEROCreditNote) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := TXEROLoaderGenericBase.Create('CreditNote', ACreditNote);
  try
    result := Store(BuildURI('', 'creditnotes',TRestParams.None, hatSend),
      'CreditNotes', 'CreditNote', ACreditNote, xsomUpdate, loader);
  finally
    loader.Free;
  end;
end;

function TXEROConnect.StoreCreditNotes(ACreditNoteList, responseList : TXEROCreditNoteList) : boolean;
begin
  if ACreditNoteList.Count = 0 then
    result := true
  else
    result := StoreList(BuildURI('','creditnotes',TRestParams.Param('SummarizeErrors','false'),hatSend),
      'CreditNotes', 'CreditNote', ACreditNoteList, responselist, xsomUpdate);
end;

// Items
function TXEROConnect.GetItems(AItemList : TXEROItemList; const AQuery : String = '') : boolean;
begin
  result := GetItems(AItemList, AQuery, TXEROOrder.None, -1);
end;

function TXEROConnect.GetItems(AItemList : TXEROItemList; const AQuery : String; const AOrder : TXEROOrder; APageNo : Integer = -1 ) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := AItemList.GetListLoader;
  try
    result := GetLoadList(BuildURI('','items', AQuery, AOrder, hatGet, APageNo), nil, 'Items', loader);
    if result then
    begin
      AItemList.ResetModified;
      AItemList.SetIsNew(false);
    end;
  finally
    loader.Free;
  end;
end;

function TXEROConnect.GetItem(AItem : TXEROItem; const AItemUID : String) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := TXEROLoaderGenericBase.Create('Item',AItem);
  try
    if AItemUID = '' then
      result := false
    else
    begin
      AItem.Clear;
      result := GetLoadSingleResult(BuildURI('items',AItemUID, TRestParams.None, hatGet), nil, 'Items', loader);
      if result then
      begin
        AItem.ResetModified;
        AItem.IsNew := false;
      end;
    end;
  finally
    loader.Free;
  end;
end;

function TXEROConnect.StoreItem(AItem : TXEROItem) : boolean;
var
  loader : TJSONSAXNestableHandler;
begin
  loader := TXEROLoaderGenericBase.Create('Item', AItem);
  try
    result := Store(BuildURI('', 'items',TRestParams.None, hatSend),
      'Items', 'Item', AItem, xsomUpdate, loader);
  finally
    loader.Free;
  end;
end;

function TXEROConnect.StoreItems(AItemList, responseList : TXEROItemList) : boolean;
begin
  if AItemList.Count = 0 then
    result := true
  else
    result := StoreList(BuildURI('','items',TRestParams.None,hatSend),
      'Items', 'Item', AItemList, responselist, xsomUpdate);
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
    if result then
    begin
      AXeroObject.IsNew := false;
      AXeroObject.ResetModified;
    end;
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
            // 404 and 401 don't have any other error information. Assume that is the
            // case for the rest.
            if (ResponseCode div 100) = 4 then
              raise XEROException.Create(ResponseCode, ErrorDetail);

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
//
function TXEROOrganisation.rAddressesList : TXEROAddressList;
begin
  if not assigned(FAddressesList) then
    FAddressesList := TXEROAddressList.Create;
  result := FAddressesList
end;

procedure TXEROOrganisation.wAddressesList(ANewVal : TXEROAddressList);
begin
  if assigned(ANewVal) then
    rAddressesList.Assign(ANewVal)
  else if Assigned(FAddressesList) then
    FAddressesList.Clear;
end;
function TXEROOrganisation.rPhonesList : TXEROPhonesList;
begin
  if not assigned(FPhonesList) then
    FPhonesList := TXEROPhonesList.Create;
  result := FPhonesList
end;
procedure TXEROOrganisation.wPhonesList(ANewVal : TXEROPhonesList);
begin
  if assigned(ANewVal) then
    rPhonesList.Assign(ANewVal)
  else if Assigned(FPhonesList) then
    FPhonesList.Clear;
end;

class function TXEROOrganisation.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  case field of
    xsfName:result := ord(xofName);
  else      result := inherited PropSpecialFieldID(field);
  end;
end;

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
    ord(xofAddresses):
      case access of
        xfamWrite: result := rAddressesList;
        xfamClear:
          begin
            FreeAndNil(FAddressesList);
            result := nil
          end;
      else result := FAddressesList;
      end;
    ord(xofPhones):
      case access of
        xfamWrite: result := rPhonesList;
        xfamClear:
          begin
            FreeAndNil(FPhonesList);
            result := nil
          end;
      else result := FPhonesList;
      end;
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
  FreeAndNil(FAddressesList);
  FreeAndNil(FPhonesList);
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

function TXEROManualJournal.rLineAmountTypesEnum(AField : integer) : TXEROLineAmountTypes;
var
  idx : integer;
begin
  idx := rIntegerVal(AField);
  if (idx < 0) or (idx > ord(high(TXEROLineAmountTypes))) then
    idx := 0;
  result := TXEROLineAmountTypes(idx);
end;

procedure TXEROManualJournal.wLineAmountTypesEnum(AField : integer; ANewVal : TXEROLineAmountTypes);
begin
  wIntegerVal(AField, ord(ANewVal));
end;

procedure TXEROManualJournal.ResetModified;
begin
  inherited ResetModified;
  if Assigned(FJournalLines) then
    FJournalLines.ResetModified;
end;

procedure TXEROManualJournal.Assign( ASource : TPersistent);
(*
var
  src : TXEROManualJournal;
  *)
begin
  inherited Assign(ASource);
  (*
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
  *)
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
    ord(xmjfLineAmountTypes): result := ord(XEROLineAmountTypesAsEnum(StgVal));
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
    ord(xmjfLineAmountTypes): result := XEROLineAmountTypesAsString(intVal);
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
        case access of
          xfamWrite:
            if not assigned(FJournalLines) then
              FJournalLines := TXEROManualJournalLineList.Create;
          xfamRead: ;
          xfamClear:
            FreeAndNil(FJournalLines);
        end;
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
  result := 'ManualJournals';
end;

// TXEROTaxComponent
//

// protected definitions

class function TXEROTaxComponent.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_TaxComponentMap.Map) then
    Raise Exception.CreateFmt('Invalid Tax Component Field %d', [AField]);
  result := G_TaxComponentMap.Map[AField];
end;

class function TXEROTaxComponent.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_TaxComponentMap.Map) then
    Raise Exception.CreateFmt('Invalid Tax Component Field %d', [AField]);
  result := CTaxComponentProperties[AField];
end;

class function TXEROTaxComponent.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_TaxComponentMap.Count[APropType];
end;

class function TXEROTaxComponent.PropFieldID( AFieldName : String) : integer;
begin
  result := G_TaxComponentMap.NameToFieldID(AFieldName);
end;

class function TXEROTaxComponent.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  case field of
    xsfName:result := ord(xtxfName);
  else      result := inherited PropSpecialFieldID(field);
  end;
end;

class function TXEROTaxComponent.PropObjectName : string;
begin
  result := 'TaxComponent';
end;

// public definitions

// TXEROTaxComponentList
//

// public definitions

class function TXEROTaxComponentList.PropListName : String;
begin
  result := 'TaxComponents';
end;


// TXEROTaxComponent.TXeroTaxRate
//

// protected definitions

function TXeroTaxRate.rHasTaxComponents : boolean;
begin
  result := Assigned(FTaxComponents) and (FTaxComponents.Count > 0);
end;
function TXeroTaxRate.rTaxComponents: TXeroTaxComponentList;
begin
  if not assigned(FTaxComponents) then
    FTaxComponents:= TXeroTaxComponentList.Create;
  result := FTaxComponents;
end;

procedure TXeroTaxRate.wTaxComponents(NewVal: TXeroTaxComponentList);
begin
  if not assigned(FTaxComponents) then
  begin
    if assigned(newVal) then
      FTaxComponents:= TXeroTaxComponentList.Create;
  end;
  if assigned(newVal) then
    FTaxComponents.Assign(newVal)
  else if assigned(FTaxComponents) then
    FTaxComponents.Clear;
end;

class function TXeroTaxRate.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_TaxRateMap.Map) then
    Raise Exception.CreateFmt('Invalid Tax Rate Field %d', [AField]);
  result := G_TaxRateMap.Map[AField];
end;

class function TXeroTaxRate.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_TaxRateMap.Map) then
    Raise Exception.CreateFmt('Invalid Tax Rate Field %d', [AField]);
  result := CTaxRateProperties[AField];
end;

class function TXeroTaxRate.PropStringAsEnum( AField : Word; const StgVal : String) : integer;
begin
  case AField of
    ord(xtrfStatus): result := ord(XEROStatusAsEnum(StgVal));
  else result := inherited PropStringAsEnum(AField, StgVal);
  end;
end;

class function TXeroTaxRate.PropEnumAsString( AField : Word; IntVal : Integer) : string;
begin
  case AField of
    ord(xtrfStatus): result := XEROStatusAsString(intVal);
  else result := inherited PropEnumAsString(AField, IntVal);
  end;
end;

class function TXeroTaxRate.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_TaxRateMap.Count[APropType];
end;

class function TXeroTaxRate.PropFieldID( AFieldName : String) : integer;
begin
  result := G_TaxRateMap.NameToFieldID(AFieldName);
end;

class function TXeroTaxRate.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  case field of
    xsfName:result := ord(xtrfName);
  else      result := inherited PropSpecialFieldID(field);
  end;
end;

class function TXeroTaxRate.PropObjectName : string;
begin
  result := 'TaxRate';
end;

function TXeroTaxRate.GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase;
begin
  case field of
    ord(xtrfTaxComponents):
      case access of
        xfamWrite: result := rTaxComponents;
        xfamClear:
          begin
            FreeAndNil(FTaxComponents);
            result := nil;
          end;
      else
        result := FTaxComponents; // Allowed to be null.
      end;
  else result := inherited GetListObject(field, Access);
  end;
end;

// TXEROTaxRateList
//

// public definitions

class function TXEROTaxRateList.PropListName : String;
begin
  result := 'TaxRates';
end;

// TXEROBrandingTheme
//
class function TXEROBrandingTheme.PropObjectName : string;
begin
  result := 'BrandingTheme';
end;
class function TXEROBrandingTheme.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_BrandingThemeMap.Map) then
    Raise Exception.CreateFmt('Invalid Branding Theme Field %d', [AField]);
  result := G_BrandingThemeMap.Map[AField];
end;

class function TXEROBrandingTheme.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_BrandingThemeMap.Map) then
    Raise Exception.CreateFmt('Invalid Branding Theme Field %d', [AField]);
  result := CBrandingThemeProperties[AField];
end;

class function TXEROBrandingTheme.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_BrandingThemeMap.Count[APropType];
end;

class function TXEROBrandingTheme.PropFieldID( AFieldName : String) : integer;
begin
  result := G_BrandingThemeMap.NameToFieldID(AFieldName);
end;

class function TXEROBrandingTheme.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  case field of
    xsfUID: result := ord(xbtfBrandingThemeID );
    xsfName:result := ord(xbtfName);
  else      result := inherited PropSpecialFieldID(field);
  end;
end;


// TXEROInvoiceLineItem
//
procedure TXEROInvoiceLineItem.BeforeDestruction;
begin
  FreeAndNil(FTrackingList);
  inherited;
end;
//
class function TXEROInvoiceLineItem.PropObjectName : string;
begin
  result := 'LineItem';
end;
function TXEROInvoiceLineItem.rTrackingList : TXEROTrackingCategoryOptionList;
begin
  if not assigned(FTrackingList) then
    FTrackingList := TXEROTrackingCategoryOptionList.Create;
  result := FTrackingList
end;

function TXEROInvoiceLineItem.GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase;
begin
  case field of
    ord(xilifTracking):
      case access of
        xfamWrite: result := rTrackingList;
        xfamClear:
          begin
            FreeAndNil(FTrackingList);
            result := nil
          end;
      else result := FTrackingList;
      end;
  else
    result := inherited GetListObject(field, Access);
  end
end;

procedure TXEROInvoiceLineItem.wTrackingList(ANewVal : TXEROTrackingCategoryOptionList);
begin
  if assigned(ANewVal) then
    rTrackingList.Assign(ANewVal)
  else if Assigned(FTrackingList) then
    FTrackingList.Clear;
end;

class function TXEROInvoiceLineItem.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_InvoiceLineItemMap.Map) then
    Raise Exception.CreateFmt('Invalid Invoice Line Item Field %d', [AField]);
  result := G_InvoiceLineItemMap.Map[AField];
end;

class function TXEROInvoiceLineItem.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_InvoiceLineItemMap.Map) then
    Raise Exception.CreateFmt('Invalid Invoice Line Item Field %d', [AField]);
  result := CInvoiceLineItemProperties[AField];
end;

class function TXEROInvoiceLineItem.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_InvoiceLineItemMap.Count[APropType];
end;

class function TXEROInvoiceLineItem.PropFieldID( AFieldName : String) : integer;
begin
  result := G_InvoiceLineItemMap.NameToFieldID(AFieldName);
end;

class function TXEROInvoiceLineItem.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  case field of
    xsfUID: result := ord(xilifLineItemID);
  else      result := inherited PropSpecialFieldID(field);
  end;
end;

class function TXEROInvoiceLineItemList.PropListName : String;
begin
  result := 'LineItems';
end;


// TXEROInvoice
//

procedure TXEROInvoice.BeforeDestruction;
begin
  FreeAndNil(FContact);
  FreeAndNil(FLineItemsList);
  FreeAndNil(FPaymentsList);
  FreeAndNil(FCreditNotesList);
  FreeAndNil(FPrepaymentsList);
  FreeAndNil(FOverpaymentsList);
  inherited;
end;

function TXEROInvoice.rTypeEnum(AField : integer) : TXEROInvoicesType;
var
  idx : integer;
begin
  idx := rIntegerVal(AField);
  if (idx < 0) or (idx > ord(high(TXEROInvoicesType))) then
    idx := 0;
  result := TXEROInvoicesType(idx);
end;

procedure TXEROInvoice.wTypeEnum(AField : integer; ANewVal : TXEROInvoicesType);
begin
  wIntegerVal(AField, ord(ANewVal));
end;

function TXEROInvoice.rContact : TXEROContact;
begin
  if not assigned(FContact) then
    FContact := TXEROContact.Create;
  result := FContact
end;
procedure TXEROInvoice.wContact(ANewVal : TXEROContact);
begin
  if assigned(ANewVal) then
    rContact.Assign(ANewVal)
  else if Assigned(FContact) then
    FContact.Clear;
end;
function TXEROInvoice.rStatusEnum(AField : integer) : TXEROInvoicesStatus;
var
  idx : integer;
begin
  idx := rIntegerVal(AField);
  if (idx < 0) or (idx > ord(high(TXEROInvoicesStatus))) then
    idx := 0;
  result := TXEROInvoicesStatus(idx);
end;
procedure TXEROInvoice.wStatusEnum(AField : integer; ANewVal : TXEROInvoicesStatus);
begin
  wIntegerVal(AField, ord(ANewVal));
end;
function TXEROInvoice.rLineAmountTypesEnum(AField : integer) : TXEROLineAmountTypes;
var
  idx : integer;
begin
  idx := rIntegerVal(AField);
  if (idx < 0) or (idx > ord(high(TXEROLineAmountTypes))) then
    idx := 0;
  result := TXEROLineAmountTypes(idx);
end;
procedure TXEROInvoice.wLineAmountTypesEnum(AField : integer; ANewVal : TXEROLineAmountTypes);
begin
  wIntegerVal(AField, ord(ANewVal));
end;
function TXEROInvoice.rLineItemsList : TXEROInvoiceLineItemList;
begin
  if not assigned(FLineItemsList) then
    FLineItemsList := TXEROInvoiceLineItemList.Create;
  result := FLineItemsList;
end;
procedure TXEROInvoice.wLineItemsList(ANewVal : TXEROInvoiceLineItemList);
begin
  if assigned(ANewVal) then
    rLineItemsList.Assign(ANewVal)
  else if Assigned(FLineItemsList) then
    FLineItemsList.Clear;
end;
function TXEROInvoice.rPaymentsList : TXEROPaymentList;
begin
  if not assigned(FPaymentsList) then
    FPaymentsList := TXEROPaymentList.Create;
  result := FPaymentsList
end;
procedure TXEROInvoice.wPaymentsList(ANewVal : TXEROPaymentList);
begin
  if assigned(ANewVal) then
    rPaymentsList.Assign(ANewVal)
  else if Assigned(FPaymentsList) then
    FPaymentsList.Clear;
end;
function TXEROInvoice.rCreditNotesList : TXEROCreditNoteList;
begin
  if not assigned(FCreditNotesList) then
    FCreditNotesList := TXEROCreditNoteList.Create;
  result := FCreditNotesList
end;
procedure TXEROInvoice.wCreditNotesList(ANewVal : TXEROCreditNoteList);
begin
  if assigned(ANewVal) then
    rCreditNotesList.Assign(ANewVal)
  else if Assigned(FCreditNotesList) then
    FCreditNotesList.Clear;
end;
function TXEROInvoice.rPrepaymentsList : TXEROPrepaymentList;
begin
  if not assigned(FPrepaymentsList) then
    FPrepaymentsList := TXEROPrepaymentList.Create;
  result := FPrepaymentsList
end;
procedure TXEROInvoice.wPrepaymentsList(ANewVal : TXEROPrepaymentList);
begin
  if assigned(ANewVal) then
    rPrepaymentsList.Assign(ANewVal)
  else if Assigned(FPrepaymentsList) then
    FPrepaymentsList.Clear;
end;
function TXEROInvoice.rOverpaymentsList : TXEROOverpaymentList;
begin
  if not assigned(FOverpaymentsList) then
    FOverpaymentsList := TXEROOverpaymentList.Create;
  result := FOverpaymentsList
end;
procedure TXEROInvoice.wOverpaymentsList(ANewVal : TXEROOverpaymentList);
begin
  if assigned(ANewVal) then
    rOverpaymentsList.Assign(ANewVal)
  else if Assigned(FOverpaymentsList) then
    FOverpaymentsList.Clear;
end;
class function TXEROInvoice.PropObjectName : string;
begin
  result := 'Invoice';
end;
class function TXEROInvoice.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_InvoicesMap.Map) then
    Raise Exception.CreateFmt('Invalid Invoices Field %d', [AField]);
  result := G_InvoicesMap.Map[AField];
end;

class function TXEROInvoice.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_InvoicesMap.Map) then
    Raise Exception.CreateFmt('Invalid Invoices Field %d', [AField]);
  result := CInvoicesProperties[AField];
end;

class function TXEROInvoice.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_InvoicesMap.Count[APropType];
end;

class function TXEROInvoice.PropFieldID( AFieldName : String) : integer;
begin
  result := G_InvoicesMap.NameToFieldID(AFieldName);
end;

class function TXEROInvoice.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  case field of
    xsfUID: result := ord(xifInvoiceID);
    xsfName:result := ord(xifInvoiceNumber);
  else      result := inherited PropSpecialFieldID(field);
  end;
end;
class function TXEROInvoice.PropStringAsEnum( AField : Word; const StgVal : String) : integer;
begin
  case AField of
    ord(xifType): result := ord(XEROInvoicesTypeAsEnum(StgVal));
    ord(xifStatus): result := ord(XEROInvoicesStatusAsEnum(StgVal));
    ord(xifLineAmountTypes): result := ord(XEROLineAmountTypesAsEnum(StgVal));
  else result := inherited PropStringAsEnum(AField, StgVal);
  end;
end;

class function TXEROInvoice.PropEnumAsString( AField : Word; IntVal : Integer) : string;
begin
  case AField of
    ord(xifType): result := XEROInvoicesTypeAsString(intVal);
    ord(xifStatus): result := XEROInvoicesStatusAsString(intVal);
    ord(xifLineAmountTypes): result := XEROLineAmountTypesAsString(intVal);
  else result := inherited PropEnumAsString(AField, IntVal);
  end;
end;

function TXEROInvoice.GetObject(field : Integer; Access : TXEROFieldAccessMode) :TXEROObject;
begin
  case field of
    ord(xifContact):
      case Access of
        xfamWrite: result := rContact;
        xfamClear:
          begin
            FreeAndNil(FContact);
            result := nil;
          end;
      else
        result := FContact;
      end;
  else
    result := inherited GetObject(field, Access);
  end
end;

function TXEROInvoice.GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase;
begin
  case field of
    ord(xifLineItems):
      case access of
        xfamWrite: result := rLineItemsList;
        xfamClear:
          begin
            FreeAndNil(FLineItemsList);
            result := nil
          end;
      else result := FLineItemsList;
      end;
    ord(xifPayments):
      case access of
        xfamWrite: result := rPaymentsList;
        xfamClear:
          begin
            FreeAndNil(FPaymentsList);
            result := nil
          end;
      else result := FPaymentsList;
      end;
    ord(xifCreditNotes):
      case access of
        xfamWrite: result := rCreditNotesList;
        xfamClear:
          begin
            FreeAndNil(FCreditNotesList);
            result := nil
          end;
      else result := FCreditNotesList;
      end;
    ord(xifPrepayments):
      case access of
        xfamWrite: result := rPrepaymentsList;
        xfamClear:
          begin
            FreeAndNil(FPrepaymentsList);
            result := nil
          end;
      else result := FPrepaymentsList;
      end;
    ord(xifOverpayments):
      case access of
        xfamWrite: result := rOverpaymentsList;
        xfamClear:
          begin
            FreeAndNil(FOverpaymentsList);
            result := nil
          end;
      else result := FOverpaymentsList;
      end;
  else
    result := inherited GetListObject(field, Access);
  end
end;

class function TXEROInvoiceList.PropListName : String;
begin
  result := 'Invoices';
end;

function TXEROContact.rContactStatusEnum(AField : integer) : TXEROContactStatus;
var
  idx : integer;
begin
  idx := rIntegerVal(AField);
  if (idx < 0) or (idx > ord(high(TXEROContactStatus))) then
    idx := 0;
  result := TXEROContactStatus(idx);
end;
procedure TXEROContact.wContactStatusEnum(AField : integer; ANewVal : TXEROContactStatus);
begin
  wIntegerVal(AField, ord(ANewVal));
end;
function TXEROContact.rAddressesList : TXEROAddressList;
begin
  if not assigned(FAddressesList) then
    FAddressesList := TXEROAddressList.Create;
  result := FAddressesList
end;
procedure TXEROContact.wAddressesList(ANewVal : TXEROAddressList);
begin
  if assigned(ANewVal) then
    rAddressesList.Assign(ANewVal)
  else if Assigned(FAddressesList) then
    FAddressesList.Clear;
end;
function TXEROContact.rPhonesList : TXEROPhonesList;
begin
  if not assigned(FPhonesList) then
    FPhonesList := TXEROPhonesList.Create;
  result := FPhonesList
end;
procedure TXEROContact.wPhonesList(ANewVal : TXEROPhonesList);
begin
  if assigned(ANewVal) then
    rPhonesList.Assign(ANewVal)
  else if Assigned(FPhonesList) then
    FPhonesList.Clear;
end;
function TXEROContact.rContactPersonsList : TXEROContactPersonsList;
begin
  if not assigned(FContactPersonsList) then
    FContactPersonsList := TXEROContactPersonsList.Create;
  result := FContactPersonsList
end;
procedure TXEROContact.wContactPersonsList(ANewVal : TXEROContactPersonsList);
begin
  if assigned(ANewVal) then
    rContactPersonsList.Assign(ANewVal)
  else if Assigned(FContactPersonsList) then
    FContactPersonsList.Clear;
end;
function TXEROContact.rSalesTrackingCategoriesList : TXEROTrackingCategoryList;
begin
  if not assigned(FSalesTrackingCategoriesList) then
    FSalesTrackingCategoriesList := TXEROTrackingCategoryList.Create;
  result := FSalesTrackingCategoriesList
end;
procedure TXEROContact.wSalesTrackingCategoriesList(ANewVal : TXEROTrackingCategoryList);
begin
  if assigned(ANewVal) then
    rSalesTrackingCategoriesList.Assign(ANewVal)
  else if Assigned(FSalesTrackingCategoriesList) then
    FSalesTrackingCategoriesList.Clear;
end;
function TXEROContact.rPurchasesTrackingCategoriesList : TXEROTrackingCategoryList;
begin
  if not assigned(FPurchasesTrackingCategoriesList) then
    FPurchasesTrackingCategoriesList := TXEROTrackingCategoryList.Create;
  result := FPurchasesTrackingCategoriesList
end;
procedure TXEROContact.wPurchasesTrackingCategoriesList(ANewVal : TXEROTrackingCategoryList);
begin
  if assigned(ANewVal) then
    rPurchasesTrackingCategoriesList.Assign(ANewVal)
  else if Assigned(FPurchasesTrackingCategoriesList) then
    FPurchasesTrackingCategoriesList.Clear;
end;
function TXEROContact.rPaymentTermsEnum(AField : integer) : TXEROPaymentTerms;
var
  idx : integer;
begin
  idx := rIntegerVal(AField);
  if (idx < 0) or (idx > ord(high(TXEROPaymentTerms))) then
    idx := 0;
  result := TXEROPaymentTerms(idx);
end;
procedure TXEROContact.wPaymentTermsEnum(AField : integer; ANewVal : TXEROPaymentTerms);
begin
  wIntegerVal(AField, ord(ANewVal));
end;
function TXEROContact.rBrandingTheme : TXEROBrandingTheme;
begin
  if not assigned(FBrandingTheme) then
    FBrandingTheme := TXEROBrandingTheme.Create;
  result := FBrandingTheme
end;
procedure TXEROContact.wBrandingTheme(ANewVal : TXEROBrandingTheme);
begin
  if assigned(ANewVal) then
    rBrandingTheme.Assign(ANewVal)
  else if Assigned(FBrandingTheme) then
    FBrandingTheme.Clear;
end;
class function TXEROContact.PropObjectName : string;
begin
  result := 'Contact';
end;
class function TXEROContact.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_ContactMap.Map) then
    Raise Exception.CreateFmt('Invalid Contact Field %d', [AField]);
  result := G_ContactMap.Map[AField];
end;

class function TXEROContact.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_ContactMap.Map) then
    Raise Exception.CreateFmt('Invalid Contact Field %d', [AField]);
  result := CContactProperties[AField];
end;

class function TXEROContact.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_ContactMap.Count[APropType];
end;

class function TXEROContact.PropFieldID( AFieldName : String) : integer;
begin
  result := G_ContactMap.NameToFieldID(AFieldName);
end;

class function TXEROContact.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  case field of
    xsfUID: result := ord(xcfContactID );
    xsfName:result := ord(xcfName);
  else      result := inherited PropSpecialFieldID(field);
  end;
end;
class function TXEROContact.PropStringAsEnum( AField : Word; const StgVal : String) : integer;
begin
  case AField of
    ord(xcfContactStatus): result := ord(XEROContactStatusAsEnum(StgVal));
    ord(xcfPaymentTerms): result := ord(XEROPaymentTermsAsEnum(StgVal));
  else result := inherited PropStringAsEnum(AField, StgVal);
  end;
end;

class function TXEROContact.PropEnumAsString( AField : Word; IntVal : Integer) : string;
begin
  case AField of
    ord(xcfContactStatus): result := XEROContactStatusAsString(intVal);
    ord(xcfPaymentTerms): result := XEROPaymentTermsAsString(intVal);
  else result := inherited PropEnumAsString(AField, IntVal);
  end;
end;

function TXEROContact.GetObject(field : Integer; Access : TXEROFieldAccessMode) :TXEROObject;
begin
  case field of
    ord(xcfBrandingTheme):
      case Access of
        xfamWrite: result := rBrandingTheme;
        xfamClear:
          begin
            FreeAndNil(FBrandingTheme);
            result := nil;
          end;
      else
        result := FBrandingTheme;
      end;
  else
    result := inherited GetObject(field, Access);
  end
end;

function TXEROContact.GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase;
begin
  case field of
    ord(xcfAddresses):
      case access of
        xfamWrite: result := rAddressesList;
        xfamClear:
          begin
            FreeAndNil(FAddressesList);
            result := nil
          end;
      else result := FAddressesList;
      end;
    ord(xcfPhones):
      case access of
        xfamWrite: result := rPhonesList;
        xfamClear:
          begin
            FreeAndNil(FPhonesList);
            result := nil
          end;
      else result := FPhonesList;
      end;
    ord(xcfContactPersons):
      case access of
        xfamWrite: result := rContactPersonsList;
        xfamClear:
          begin
            FreeAndNil(FContactPersonsList);
            result := nil
          end;
      else result := FContactPersonsList;
      end;
    ord(xcfSalesTrackingCategories):
      case access of
        xfamWrite: result := rSalesTrackingCategoriesList;
        xfamClear:
          begin
            FreeAndNil(FSalesTrackingCategoriesList);
            result := nil
          end;
      else result := FSalesTrackingCategoriesList;
      end;
    ord(xcfPurchasesTrackingCategories):
      case access of
        xfamWrite: result := rPurchasesTrackingCategoriesList;
        xfamClear:
          begin
            FreeAndNil(FPurchasesTrackingCategoriesList);
            result := nil
          end;
      else result := FPurchasesTrackingCategoriesList;
      end;
  else
    result := inherited GetListObject(field, Access);
  end
end;

procedure TXEROContact.BeforeDestruction;
begin
  FreeAndNil(FAddressesList);
  FreeAndNil(FPhonesList);
  FreeAndNil(FContactPersonsList);
  FreeAndNil(FSalesTrackingCategoriesList);
  FreeAndNil(FPurchasesTrackingCategoriesList);
  FreeAndNil(FBrandingTheme);
  inherited BeforeDestruction;
end;

class function TXEROContactList.PropListName : String;
begin
  result := 'Contacts';
end;

class function TXEROContactPerson.PropObjectName : string;
begin
  result := 'ContactPerson';
end;
class function TXEROContactPerson.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_ContactPersonMap.Map) then
    Raise Exception.CreateFmt('Invalid Contact Person Field %d', [AField]);
  result := G_ContactPersonMap.Map[AField];
end;

class function TXEROContactPerson.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_ContactPersonMap.Map) then
    Raise Exception.CreateFmt('Invalid Contact Person Field %d', [AField]);
  result := CContactPersonProperties[AField];
end;

class function TXEROContactPerson.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_ContactPersonMap.Count[APropType];
end;

class function TXEROContactPerson.PropFieldID( AFieldName : String) : integer;
begin
  result := G_ContactPersonMap.NameToFieldID(AFieldName);
end;

class function TXEROContactPerson.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  result := inherited PropSpecialFieldID(field);
end;

class function TXEROContactPersonsList.PropListName : String;
begin
  result := 'ContactPersons';
end;

// TXEROAddress
//


function TXEROAddress.rAddressTypeEnum(AField : integer) : TXEROAddressType;
var
  idx : integer;
begin
  idx := rIntegerVal(AField);
  if (idx < 0) or (idx > ord(high(TXEROAddressType))) then
    idx := 0;
  result := TXEROAddressType(idx);
end;
procedure TXEROAddress.wAddressTypeEnum(AField : integer; ANewVal : TXEROAddressType);
begin
  wIntegerVal(AField, ord(ANewVal));
end;
class function TXEROAddress.PropObjectName : string;
begin
  result := 'Address';
end;
class function TXEROAddress.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_AddressMap.Map) then
    Raise Exception.CreateFmt('Invalid Address Field %d', [AField]);
  result := G_AddressMap.Map[AField];
end;

class function TXEROAddress.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_AddressMap.Map) then
    Raise Exception.CreateFmt('Invalid Address Field %d', [AField]);
  result := CAddressProperties[AField];
end;

class function TXEROAddress.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_AddressMap.Count[APropType];
end;

class function TXEROAddress.PropFieldID( AFieldName : String) : integer;
begin
  result := G_AddressMap.NameToFieldID(AFieldName);
end;

class function TXEROAddress.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  result := inherited PropSpecialFieldID(field);
end;

class function TXEROAddress.PropStringAsEnum( AField : Word; const StgVal : String) : integer;
begin
  case AField of
    ord(xafAddressType): result := ord(XEROAddressTypeAsEnum(StgVal));
  else result := inherited PropStringAsEnum(AField, StgVal);
  end;
end;

class function TXEROAddress.PropEnumAsString( AField : Word; IntVal : Integer) : string;
begin
  case AField of
    ord(xafAddressType): result := XEROAddressTypeAsString(intVal);
  else result := inherited PropEnumAsString(AField, IntVal);
  end;
end;


class function TXEROAddressList.PropListName : String;
begin
  result :=  'Addresses'; // TODO Check
end;


// TXEROPhone
//
class function TXEROPhone.PropObjectName : String;
begin
  result := 'Phone';
end;
// TXEROPhonesList
//
class function TXEROPhonesList.PropListName : String;
begin
  result := 'Phones';
end;

  function TXEROPayment.rStatusEnum(AField : integer) : TXEROPaymentStatus;
var
  idx : integer;
begin
  idx := rIntegerVal(AField);
  if (idx < 0) or (idx > ord(high(TXEROPaymentStatus))) then
    idx := 0;
  result := TXEROPaymentStatus(idx);
end;
procedure TXEROPayment.wStatusEnum(AField : integer; ANewVal : TXEROPaymentStatus);
begin
  wIntegerVal(AField, ord(ANewVal));
end;

function TXEROPayment.rPaymentTypeEnum(AField : integer) : TXEROPaymentType;
var
  idx : integer;
begin
  idx := rIntegerVal(AField);
  if (idx < 0) or (idx > ord(high(TXEROPaymentType))) then
    idx := 0;
  result := TXEROPaymentType(idx);
end;
procedure TXEROPayment.wPaymentTypeEnum(AField : integer; ANewVal : TXEROPaymentType);
begin
  wIntegerVal(AField, ord(ANewVal));
end;
function TXEROPayment.rAccount : TXEROAccount;
begin
  if not assigned(FAccount) then
    FAccount := TXEROAccount.Create;
  result := FAccount
end;
procedure TXEROPayment.wAccount(ANewVal : TXEROAccount);
begin
  if assigned(ANewVal) then
    rAccount.Assign(ANewVal)
  else if Assigned(FAccount) then
    FAccount.Clear;
end;
function TXEROPayment.rInvoice : TXEROInvoice;
begin
  if not assigned(FInvoice) then
    FInvoice := TXEROInvoice.Create;
  result := FInvoice
end;
procedure TXEROPayment.wInvoice(ANewVal : TXEROInvoice);
begin
  if assigned(ANewVal) then
    rInvoice.Assign(ANewVal)
  else if Assigned(FInvoice) then
    FInvoice.Clear;
end;
function TXEROPayment.rCreditNote : TXEROCreditNote;
begin
  if not assigned(FCreditNote) then
    FCreditNote := TXEROCreditNote.Create;
  result := FCreditNote
end;
procedure TXEROPayment.wCreditNote(ANewVal : TXEROCreditNote);
begin
  if assigned(ANewVal) then
    rCreditNote.Assign(ANewVal)
  else if Assigned(FCreditNote) then
    FCreditNote.Clear;
end;
function TXEROPayment.rPrepayments : TXEROPrepayment;
begin
  if not assigned(FPrepayments) then
    FPrepayments := TXEROPrepayment.Create;
  result := FPrepayments
end;
procedure TXEROPayment.wPrepayments(ANewVal : TXEROPrepayment);
begin
  if assigned(ANewVal) then
    rPrepayments.Assign(ANewVal)
  else if Assigned(FPrepayments) then
    FPrepayments.Clear;
end;
function TXEROPayment.rOverpayment : TXEROOverpayment;
begin
  if not assigned(FOverpayment) then
    FOverpayment := TXEROOverpayment.Create;
  result := FOverpayment
end;
procedure TXEROPayment.wOverpayment(ANewVal : TXEROOverpayment);
begin
  if assigned(ANewVal) then
    rOverpayment.Assign(ANewVal)
  else if Assigned(FOverpayment) then
    FOverpayment.Clear;
end;
class function TXEROPayment.PropObjectName : string;
begin
  result := 'Payment';
end;
class function TXEROPayment.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_PaymentMap.Map) then
    Raise Exception.CreateFmt('Invalid Payment Field %d', [AField]);
  result := G_PaymentMap.Map[AField];
end;

class function TXEROPayment.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_PaymentMap.Map) then
    Raise Exception.CreateFmt('Invalid Payment Field %d', [AField]);
  result := CPaymentProperties[AField];
end;

class function TXEROPayment.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_PaymentMap.Count[APropType];
end;

class function TXEROPayment.PropFieldID( AFieldName : String) : integer;
begin
  result := G_PaymentMap.NameToFieldID(AFieldName);
end;

class function TXEROPayment.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  case field of
    xsfUID: result := ord(xpfPaymentID);
  else      result := inherited PropSpecialFieldID(field);
  end;
end;
class function TXEROPayment.PropStringAsEnum( AField : Word; const StgVal : String) : integer;
begin
  case AField of
    ord(xpfStatus): result := ord(XEROPaymentStatusAsEnum(StgVal));
    ord(xpfPaymentType): result := ord(XEROPaymentTypeAsEnum(StgVal));
  else result := inherited PropStringAsEnum(AField, StgVal);
  end;
end;

class function TXEROPayment.PropEnumAsString( AField : Word; IntVal : Integer) : string;
begin
  case AField of
    ord(xpfStatus): result := XEROPaymentStatusAsString(intVal);
    ord(xpfPaymentType): result := XEROPaymentTypeAsString(intVal);
  else result := inherited PropEnumAsString(AField, IntVal);
  end;
end;

function TXEROPayment.GetObject(field : Integer; Access : TXEROFieldAccessMode) :TXEROObject;
begin
  case field of
    ord(xpfAccount):
      case Access of
        xfamWrite: result := rAccount;
        xfamClear:
          begin
            FreeAndNil(FAccount);
            result := nil;
          end;
      else
        result := FAccount;
      end;
    ord(xpfInvoice):
      case Access of
        xfamWrite: result := rInVoice;
        xfamClear:
          begin
            FreeAndNil(FInVoice);
            result := nil;
          end;
      else
        result := FInVoice;
      end;
    ord(xpfCreditNote):
      case Access of
        xfamWrite: result := rCreditNote;
        xfamClear:
          begin
            FreeAndNil(FCreditNote);
            result := nil;
          end;
      else
        result := FCreditNote;
      end;
    ord(xpfPrepayments):
      case Access of
        xfamWrite: result := rPrepayments;
        xfamClear:
          begin
            FreeAndNil(FPrepayments);
            result := nil;
          end;
      else
        result := FPrepayments;
      end;
    ord(xpfOverpayment):
      case Access of
        xfamWrite: result := rOverpayment;
        xfamClear:
          begin
            FreeAndNil(FOverpayment);
            result := nil;
          end;
      else
        result := FOverpayment;
      end;
  else
    result := inherited GetObject(field, Access);
  end
end;

procedure TXEROPayment.BeforeDestruction;
begin
  FreeAndNil(FAccount);
  FreeAndNil(FInvoice);
  FreeAndNil(FCreditNote);
  FreeAndNil(FPrepayments);
  FreeAndNil(FOverpayment);
  inherited BeforeDestruction;
end;


class function TXEROPaymentList.PropListName : String;
begin
  result :=  'Payments';
end;


// TXEROPrepayment
//

class function TXEROPrepayment.PropObjectName : String;
begin
  result := 'Prepayment';
end;
// TXEROPrepaymentList
//

// public definitions

class function TXEROPrepaymentList.PropListName : String;
begin
  result := 'Prepayments';
end;
// TXEROOverpayment
//

// public definitions

class function TXEROOverpayment.PropObjectName : String;
begin
  result := 'Overpayment';
end;

// TXEROOverpaymentList
//

// public definitions

class function TXEROOverpaymentList.PropListName : String;
begin
  result := 'Overpayments';
end;

// TXEROItemPriceDetail

class function TXEROItemPriceDetail.PropObjectName : string;
begin
  result := 'ItemPriceDetail';
end;

class function TXEROItemPriceDetail.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_ItemPriceDetailMap.Map) then
    Raise Exception.CreateFmt('Invalid Item Price Detail Field %d', [AField]);
  result := G_ItemPriceDetailMap.Map[AField];
end;

class function TXEROItemPriceDetail.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_ItemPriceDetailMap.Map) then
    Raise Exception.CreateFmt('Invalid Item Price Detail Field %d', [AField]);
  result := CItemPriceDetailProperties[AField];
end;

class function TXEROItemPriceDetail.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_ItemPriceDetailMap.Count[APropType];
end;

class function TXEROItemPriceDetail.PropFieldID( AFieldName : String) : integer;
begin
  result := G_ItemPriceDetailMap.NameToFieldID(AFieldName);
end;

class function TXEROItemPriceDetail.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  result := inherited PropSpecialFieldID(field);
end;

function TXEROItem.rPurchaseDetails : TXEROPurchaseDetails;
begin
  if not assigned(FPurchaseDetails) then
    FPurchaseDetails := TXEROPurchaseDetails.Create;
  result := FPurchaseDetails
end;
procedure TXEROItem.wPurchaseDetails(ANewVal : TXEROPurchaseDetails);
begin
  if assigned(ANewVal) then
    rPurchaseDetails.Assign(ANewVal)
  else if Assigned(FPurchaseDetails) then
    FPurchaseDetails.Clear;
end;
function TXEROItem.rSalesDetails : TXEROSalesDetails;
begin
  if not assigned(FSalesDetails) then
    FSalesDetails := TXEROSalesDetails.Create;
  result := FSalesDetails
end;
procedure TXEROItem.wSalesDetails(ANewVal : TXEROSalesDetails);
begin
  if assigned(ANewVal) then
    rSalesDetails.Assign(ANewVal)
  else if Assigned(FSalesDetails) then
    FSalesDetails.Clear;
end;
class function TXEROItem.PropObjectName : string;
begin
  result := 'Item';
end;
class function TXEROItem.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_ItemMap.Map) then
    Raise Exception.CreateFmt('Invalid Item Field %d', [AField]);
  result := G_ItemMap.Map[AField];
end;

class function TXEROItem.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_ItemMap.Map) then
    Raise Exception.CreateFmt('Invalid Item Field %d', [AField]);
  result := CItemProperties[AField];
end;

class function TXEROItem.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_ItemMap.Count[APropType];
end;

class function TXEROItem.PropFieldID( AFieldName : String) : integer;
begin
  result := G_ItemMap.NameToFieldID(AFieldName);
end;

class function TXEROItem.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  case field of
    xsfUID: result := ord(xifItemID);
    xsfName:result := ord(xifCode);
  else      result := inherited PropSpecialFieldID(field);
  end;
end;
function TXEROItem.GetObject(field : Integer; Access : TXEROFieldAccessMode) :TXEROObject;
begin
  case field of
    ord(xifPurchaseDetails):
      case Access of
        xfamWrite: result := rPurchaseDetails;
        xfamClear:
          begin
            FreeAndNil(FPurchaseDetails);
            result := nil;
          end;
      else
        result := FPurchaseDetails;
      end;
    ord(xifSalesDetails):
      case Access of
        xfamWrite: result := rSalesDetails;
        xfamClear:
          begin
            FreeAndNil(FSalesDetails);
            result := nil;
          end;
      else
        result := FSalesDetails;
      end;
  else
    result := inherited GetObject(field, Access);
  end
end;

function TXEROItem.CanOutField( Field : integer; mode : TXEROSerialiseOutputMode) : boolean;
begin
  result := inherited CanOutField(Field, mode);

  if result and (mode = xsomUpdate) then
  begin
    case Field of
      ord(xifPurchaseDetails): result := IsPurchased;
      ord(xifSalesDetails):    result := IsSold;
    end;
  end;
end;

procedure TXEROItem.BeforeDestruction;
begin
  FreeAndNil(FPurchaseDetails);
  FreeAndNil(FSalesDetails);
  inherited BeforeDestruction;
end;


class function TXEROItemList.PropListName : String;
begin
  result :=  'Items'; // TODO Check
end;

// TXEROCreditNote
//

  function TXEROCreditNote.rTypeEnum(AField : integer) : TXEROInvoicesType;
var
  idx : integer;
begin
  idx := rIntegerVal(AField);
  if (idx < 0) or (idx > ord(high(TXEROInvoicesType))) then
    idx := 0;
  result := TXEROInvoicesType(idx);
end;
procedure TXEROCreditNote.wTypeEnum(AField : integer; ANewVal : TXEROInvoicesType);
begin
  wIntegerVal(AField, ord(ANewVal));
end;
function TXEROCreditNote.rContact : TXEROContact;
begin
  if not assigned(FContact) then
    FContact := TXEROContact.Create;
  result := FContact
end;
procedure TXEROCreditNote.wContact(ANewVal : TXEROContact);
begin
  if assigned(ANewVal) then
    rContact.Assign(ANewVal)
  else if Assigned(FContact) then
    FContact.Clear;
end;
function TXEROCreditNote.rStatusEnum(AField : integer) : TXEROCreditNoteStatus;
var
  idx : integer;
begin
  idx := rIntegerVal(AField);
  if (idx < 0) or (idx > ord(high(TXEROCreditNoteStatus))) then
    idx := 0;
  result := TXEROCreditNoteStatus(idx);
end;
procedure TXEROCreditNote.wStatusEnum(AField : integer; ANewVal : TXEROCreditNoteStatus);
begin
  wIntegerVal(AField, ord(ANewVal));
end;
function TXEROCreditNote.rLineAmountTypesEnum(AField : integer) : TXEROLineAmountTypes;
var
  idx : integer;
begin
  idx := rIntegerVal(AField);
  if (idx < 0) or (idx > ord(high(TXEROLineAmountTypes))) then
    idx := 0;
  result := TXEROLineAmountTypes(idx);
end;
procedure TXEROCreditNote.wLineAmountTypesEnum(AField : integer; ANewVal : TXEROLineAmountTypes);
begin
  wIntegerVal(AField, ord(ANewVal));
end;
  function TXEROCreditNote.rLineItemsList : TXEROInvoiceLineItemList;
begin
  if not assigned(FLineItemsList) then
    FLineItemsList := TXEROInvoiceLineItemList.Create;
  result := FLineItemsList
end;

procedure TXEROCreditNote.wLineItemsList(ANewVal : TXEROInvoiceLineItemList);
begin
  if assigned(ANewVal) then
    rLineItemsList.Assign(ANewVal)
  else if Assigned(FLineItemsList) then
    FLineItemsList.Clear;
end;
class function TXEROCreditNote.PropObjectName : string;
begin
  result := 'CreditNote';
end;
class function TXEROCreditNote.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;
begin
  if AField > high(G_CreditNoteMap.Map) then
    Raise Exception.CreateFmt('Invalid Credit Note Field %d', [AField]);
  result := G_CreditNoteMap.Map[AField];
end;

class function TXEROCreditNote.PropInfo( AField : Word ) : TXEROPropertyEntry;
begin
  if AField > high(G_CreditNoteMap.Map) then
    Raise Exception.CreateFmt('Invalid Credit Note Field %d', [AField]);
  result := CCreditNoteProperties[AField];
end;

class function TXEROCreditNote.PropTypeCount( APropType : TXEROPropertyType) : integer;
begin
  result := G_CreditNoteMap.Count[APropType];
end;

class function TXEROCreditNote.PropFieldID( AFieldName : String) : integer;
begin
  result := G_CreditNoteMap.NameToFieldID(AFieldName);
end;

class function TXEROCreditNote.PropSpecialFieldID( Field : TXEROSpecialField) : integer;
begin
  case field of
    xsfUID: result := ord(xcnfCreditNoteID);
    xsfName:result := ord(xcnfCreditNoteNumber);
  else      result := inherited PropSpecialFieldID(field);
  end;
end;
class function TXEROCreditNote.PropStringAsEnum( AField : Word; const StgVal : String) : integer;
begin
  case AField of
    ord(xcnfType): result := ord(XEROCreditNoteTypeAsEnum(StgVal));
    ord(xcnfStatus): result := ord(XEROCreditNoteStatusAsEnum(StgVal));
    ord(xcnfLineAmountTypes): result := ord(XEROLineAmountTypesAsEnum(StgVal));
  else result := inherited PropStringAsEnum(AField, StgVal);
  end;
end;

class function TXEROCreditNote.PropEnumAsString( AField : Word; IntVal : Integer) : string;
begin
  case AField of
    ord(xcnfType): result := XEROCreditNoteTypeAsString(intVal);
    ord(xcnfStatus): result := XEROCreditNoteStatusAsString(intVal);
    ord(xcnfLineAmountTypes): result := XEROLineAmountTypesAsString(intVal);
  else result := inherited PropEnumAsString(AField, IntVal);
  end;
end;

function TXEROCreditNote.GetObject(field : Integer; Access : TXEROFieldAccessMode) :TXEROObject;
begin
  case field of
    ord(xcnfContact):
      case Access of
        xfamWrite: result := rContact;
        xfamClear:
          begin
            FreeAndNil(FContact);
            result := nil;
          end;
      else
        result := FContact;
      end;
  else
    result := inherited GetObject(field, Access);
  end
end;

function TXEROCreditNote.GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase;
begin
  case field of
    ord(xcnfLineItems):
      case access of
        xfamWrite: result := rLineItemsList;
        xfamClear:
          begin
            FreeAndNil(FLineItemsList);
            result := nil
          end;
      else result := FLineItemsList;
      end;
  else
    result := inherited GetListObject(field, Access);
  end
end;

procedure TXEROCreditNote.BeforeDestruction;
begin
  FreeAndNil(FContact);
  FreeAndNil(FLineItemsList);
  inherited BeforeDestruction;
end;

class function TXEROCreditNoteList.PropListName : String;
begin
  result :=  'CreditNotes';
end;

// TXEROObjectListBase

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

function TXEROObjectList<XOBJ>.ExtractIndex( AIndex : integer) : XOBJ;
begin
  result := FList[AIndex];
  try
    FList.OwnsObjects := false;
    FList.Delete(AIndex);
  finally
    FList.OwnsObjects := true;
  end;
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
  LoadTypeInfoMap( G_TaxComponentMap,     @CTaxComponentProperties[0], length(CTaxComponentProperties));
  LoadTypeInfoMap( G_TaxRateMap,          @CTaxRateProperties[0], length(CTaxRateProperties));

  LoadTypeInfoMap( G_InvoicesMap,         @CInvoicesProperties[0], length(CInvoicesProperties));
  LoadTypeInfoMap( G_InvoiceLineItemMap,  @CInvoiceLineItemProperties[0], length(CInvoiceLineItemProperties));
  LoadTypeInfoMap( G_BrandingThemeMap,    @CBrandingThemeProperties[0], length(CBrandingThemeProperties));

  LoadTypeInfoMap( G_ContactMap,          @CContactProperties[0], length(CContactProperties));
  LoadTypeInfoMap( G_ContactPersonMap,    @CContactPersonProperties[0], length(CContactPersonProperties));
  LoadTypeInfoMap( G_ItemMap,             @CItemProperties[0], length(CItemProperties));
  LoadTypeInfoMap( G_ItemPriceDetailMap,  @CItemPriceDetailProperties[0], length(CItemPriceDetailProperties));
  LoadTypeInfoMap( G_PaymentMap,          @CPaymentProperties[0], length(CPaymentProperties));
  LoadTypeInfoMap( G_CreditNoteMap,       @CCreditNoteProperties[0], length(CCreditNoteProperties));
  LoadTypeInfoMap( G_AddressMap,          @CAddressProperties[0], length(CAddressProperties));
finalization
  G_Comparer := nil;
end.
