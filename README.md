# delphiXERO
XERO accounting API for Delphi

[http://developer.xero.com/ ](http://developer.xero.com/ )

Supports Delphi Sydney

* Older versions may work but this has not been tested.

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


# ToDo #
----------
[ ] Access Token JWT decode (delphi-jose-jwt?)


----------
#Components#

## TXEROAppDetails ##

Store XERO App Details

XEROAppDetails.ClientID := '4D914DECC5F34C4D882F76F0....';

## TXEROAuthenticatorPKCE ##

OAuth2 PKCE Authentication ([https://developer.xero.com/documentation/oauth2/pkce-flow])

This component will launch the authentication URL and will start a http:localhost:{port} server for authentication respons

By default the HTTP Server attempts to start on port 58985, 58986 or 58987 
The default scope is 'openid profile email accounting.transactions accounting.contacts accounting.settings'
Use OnXEROAuthenticationURL to launch default browser or show within a browser in your application (NOTE: TWebBrowser IE Mode does not work)

XEROAuthenticatorPKCE.OnXEROAuthenticationURL :=  { example OnXEROAuthenticationURL(ASender: TObject; AURL: string) }
XEROAuthenticatorPKCE.OnXEROAuthenticationComplete :=  { example OnXEROAuthenticationComplete(ASender: TObject; ASuccess: Boolean) }
XEROAuthenticatorPKCE.Scope := 'openid profile email accounting.transactions accounting.contacts accounting.settings';
XEROAuthenticatorPKCE.Login;

Upon login FXEROAuthenticatorPKCE.AuthToken and XEROAuthenticatorPKCE.Tenants will be populated


## TXEROApiJSON ##

Provide a simple interface for calling the XERO API

## Get ##
    var
     LAPI: TXEROApiJSON;
    begin
      LAPI := TXEROApiJSON.Create(nil);
      try
    	LAPI.XEROAppDetails := FXEROAppDetails;
		LAPI.TenantID := '06c6ccf8-bd57-4d2f-a36e-396b2af59f24';
    	LJSON := LAPI.Get('Contacts','page=1');
      finally
    	FreeAndNil(LAPI);
      end;
    end;

## Post ##
    var
     LAPI: TXEROApiJSON;
    begin
      LAPI := TXEROApiJSON.Create(nil);
      try
    	LAPI.XEROAppDetails := FXEROAppDetails;
		LAPI.TenantID := '06c6ccf8-bd57-4d2f-a36e-396b2af59f24';
    	LJSON := LAPI.Post('Contacts','[JSON data]');
      finally
    	FreeAndNil(LAPI);
      end;
    end;



