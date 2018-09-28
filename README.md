# delphiXERO
XERO accounting API for Delphi

[http://developer.xero.com/ ](http://developer.xero.com/ )

Supports Delphi XE6 upwards. *(Currently only tested in Tokyo)*

> This is a work in progress so design and requirements may change.

# Sources #

This component merges code from different sources to create an API for Delphi.  

## Initial XERO RSA/OAuth code (Flow Software) ##
[ftp://ftp.flow.net.nz/release/code/OAuthWithXero.zip](ftp://ftp.flow.net.nz/release/code/OAuthWithXero.zip)

DCPcrypt Cryptographic Component Library
http://sourceforge.net/projects/dcpcrypt/

## Fundamentals 4.00 ##
[https://code.google.com/p/fundamentals/ ](https://code.google.com/p/fundamentals/)

# License #

> Use this source code in open source or commercial software. You do not need to provide any credit. However please provide any fixes or enhancements to keep the component alive and helpful to everyone

[Donations are appreciated](https://www.paypal.com/donate/?token=IS7CpOIQT6YEqUiUwICKTLsYdqFBDoFnTE894RdGA-vgWExlMo08xMSMr0SO-W64yDpkWW&country.x=AU&locale.x=AU)

# Thanks #
- Michael ([https://github.com/frogonwheels](https://github.com/frogonwheels))



----------
#Components#

## TXEROAppDetails ##

Store XERO credentials

      FXEROAppDetails.Privatekey.Text := 'MIICXgIBAAKBgQ....';
      FXEROAppDetails.ConsumerKey := 'A12345';
      FXEROAppDetails.ConsumerSecret := 'A54321';


## TXEROApiJSON ##

Provide a simple interface for calling the XERO API

## Get ##
    var
     LXEROAPI: TXEROApiJSON;
    begin
      LXEROAPI := TXEROApiJSON.Create(nil);
      try
    	LXEROAPI.XEROAppDetails := FXEROAppDetails;
    	LJSON := LXEROAPI.Get('Contacts','page=1');
      finally
    	FreeAndNil(LXEROAPI);
      end;
    end;

## Post ##
    var
     LXEROAPI: TXEROApiJSON;
    begin
      LXEROAPI := TXEROApiJSON.Create(nil);
      try
    	LXEROAPI.XEROAppDetails := FXEROAppDetails;
    	LJSON := LXEROAPI.Post('Contacts','[JSON data]');
      finally
    	FreeAndNil(LXEROAPI);
      end;
    end;


## TXERO{Module} ## (eg TXEROContacts)

A complete list is still in progress but helper objects for different XERO modules exists

 
    var
      LAPI: TXEROContacts;
      LData: TXEROContactResponse;
    begin
      LAPI := TXEROContacts.Create(nil);
      try
    	LAPI.XEROAppDetails := FXEROAppDetails;
		// Search(Page, OrderBy, ContactID, ContactNumber, IncludeArchived
    	LData := LAPI.Search(1, '', '','', '',false);
    	if LData.Contacts.Count > 0 then
		begin
			ShowMessage(LData.Contacts.Item[0].Name);
		end;
		// AsJSON(Formatted)
		LData.Contacts.AsJSON(true);
      finally
    	FreeAndNil(LAPI);
      end;
    end;

## TXEROModel ##

Models of the type TXM{Type} exist for population (eg TXMContact)

## TXEROModelList ##

A list of TXEROModels (eg TXMContacts)

## TXEROModelDataset<T: TXEROModel>

Allows the conversion of models to a dataset

    var
      LAPI: TXEROContacts;
      LData: TXEROContactResponse;
	  LContactDataset : TXEROContactDataset;
    begin
      LAPI := TXEROContacts.Create(nil);
	  LContactDataset := TXEROContactDataset.Create;
      try
    	LAPI.XEROAppDetails := FXEROAppDetails;
		// Search(Page, OrderBy, ContactID, ContactNumber, IncludeArchived
    	LData := LAPI.Search(1, '', '','', '',false);
    	LContactDataset.StoreModelList(LData.Contacts);
		While not LContactDataset.Dataset.Eof do
			begin
				ShowMessage(LContactDataset.Dataset.FieldByName('Name').AsString);	
				LContactDataset.Dataset.Next;
			end;
      finally
    	FreeAndNil(LAPI);
	    FreeAndNil(LContactDataset);
      end;
    end;