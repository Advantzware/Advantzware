/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : WebUtilities
    Purpose     : Wrapper to web-util methods (WebSpeed API)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 25 22:25:55 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*             FROM PROPATH . 
USING Consultingwerk.Web.*         FROM PROPATH .  
USING Consultingwerk.Web.Widgets.* FROM PROPATH .  
USING Progress.Lang.*              FROM PROPATH .

CLASS Consultingwerk.Web.WebUtilities: 

    /*------------------------------------------------------------------------------
        Purpose: Static constructor for the WebUtilities class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR STATIC WebUtilities ():

        IF SESSION:CLIENT-TYPE <> ClientType:WEBCLIENT AND NOT VALID-HANDLE (WebContext:web-utilities-hdl) THEN DO:
            
            RUN Consultingwerk/Web/init-web-utilities.p . 
        END.		
		
	END CONSTRUCTOR.


    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation          
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE WebUtilities ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Given a cookie name, returns one or more matching values.  
        Notes:   If more than one value matches, all are returned delimited by the 
                 value of SelDelim, normally a comma.  If ? is specified for the 
                 name, the output will be a list of all the cookie names.
        @param pcCookieName The name of the cookie
        @return The cookie value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetCookie (pcCookieName AS CHARACTER):
        
        RETURN DYNAMIC-FUNCTION ("get-cookie":U IN WebContext:web-utilities-hdl, 
                                 pcCookieName) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retrieves the associated value for the specified form field.
        Notes:   
        @param pcFieldName Name of field or ?
        @return Value of field or blank if invalid field name. If ? was specified for the name, the list of fields is returned.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetField (pcFieldName AS CHARACTER):
        
        RETURN DYNAMIC-FUNCTION ("get-field":U IN WebContext:web-utilities-hdl, 
                                 pcFieldName) .
                                 
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retrieves the associated value for the specified user field
                 that was set with set-user-field().
        Notes:   
        @param pcFieldName Name of user field or ?
        @return Value of user field or blank if invalid name.  If ? was specified for the name, the list of user fields is returned.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetUserField (pcFieldName AS CHARACTER):
        
        RETURN DYNAMIC-FUNCTION ("get-user-field":U IN WebContext:web-utilities-hdl, pcFieldName).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retrieves the first available value for a user field, field or cookie
        Notes:   
        @param pcFieldName Name of user field or ?
        @return Value of user field, form field or Cookie in that order or blank if an invalid name.  If ? was specified for the name, a comma separated list of all user fields, fields and cookies is returned.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetValue (pcFieldName AS CHARACTER):
        
        RETURN DYNAMIC-FUNCTION ("get-value":U IN WebContext:web-utilities-hdl, pcFieldName).

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Writes a BLOB to the WebStream 
        Notes:   
        @param pmObject The MEMPTR for the WebStream to export
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID OutLong (pmObject AS MEMPTR):
		
		EXPORT STREAM-HANDLE WebContext:WebStream pmObject . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Outputs a single meta http-equiv tag (usually in the html header)                                                                      
        Notes:                                                                  
        @param poNameValue The pair of name and the value to output       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID OutputMetaHttpEquiv (poNameValue AS NameValuePair):
        
        PUT STREAM-HANDLE WebContext:WebStream UNFORMATTED 
              SUBSTITUTE ('<meta http-equiv="&1" content="&2">':U,
                          poNameValue:Name,
                          poNameValue:Value) SKIP . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Outputs a list of meta http-equiv tags (usually in the html header)                                                                     
        Notes:                                                                  
        @param poNameValues A list of pairs of name and the value to output       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID OutputMetaHttpEquiv (poNameValues AS ListNameValuePair):
        
        {Consultingwerk/foreachABL.i NameValuePair oPair in poNameValues}
             OutputMetaHttpEquiv (oPair) .
        END.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Runs an object oriented web object
        Notes:   
        @param pcPageObject The web page object to run (Consultingwerk.Web.Widgets.IPage)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID RunPageObject (pcPageObject AS CHARACTER):
        
        DYNAMIC-FUNCTION ("set-user-field":U IN WebContext:web-utilities-hdl, 
                          "SmartWeb.Page":U, 
                          pcPageObject) .

        WebUtilities:RunWebObject ("Consultingwerk/Web/page-handler.w":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Runs a procedural web object
        Notes:   
        @param pcWebObject The web object to run (procedure)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID RunWebObject (pcWebObject AS CHARACTER):
        
        RUN run-web-object IN WebContext:web-utilities-hdl (pcWebObject) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Outputs an HTTP Set-Cookie header with specified options
        Notes:   Cookie will be send to web-stream and returned by this method
        @param pcName The Cookie Name, must be specified
        @param pcValue The Cookie Value, can be blank which is typically used when deleting a cookie
        @param pdDate Expires date is an optional parameter expression evaluating to a DATE data type.  This is used to set an expiration time on a cookie. For instance, TODAY + 1 = tomorrow; TODAY + 365 = same day next year; TODAY - 1 = yesterday. If the cookie should only persist for the duration of the browser session, specify ?  
        @param piTime Expires time is an optional parameter expression evaluating to an INTEGER data type to the number of seconds since midnight.  This is only meaningful if an expression other than ? is specified for the Expires date.  The expression can be a very large or small number (greater or less than the number of seconds in a day) which will be normalized to fit within a day.  In this case, the Expires date will be incremented or decremented as appropriate. Specifying small numbers into the future e.g. TIME + 5 * 60 (five minutes from now) may not expire when desired due to differences in  the clock of the user's machine running the browser vs. the clock of the machine running the application. Specify ? if setting an expiration time is not desired.
        @param pcPath Path is the URL path below which the Cookie will be sent.  If ? is specified, the default is the value of the AppURL global variable unless the Cookie Path configuration option is set in which case it will be used instead. which is the URL common to all programs within an application. To delete a cookie, this value must be the same as it was when the cookie was set.
        @param pcDomain Domain is an optional parameter with the internet domain, i.e. ".progress.com".  If ? is specified, the domain will be the current host or if specified, the value of the Cookie Domain configuration option. To delete a cookie, this value must be the same as it was when the cookie was set.
        @param pcOptions Comma-delimited options:  "SECURE" - If specified, the web browser will only send the Cookie back when on a secure (SSL) connection (using https). "UTC" - Assume date and time are UTC based elimitating any conversion that would otherwise be required from local time to UTC time. "LOCAL" - (Default) Assume date and time are based on local time and need conversion to UTC. The "SECURE" option can be combined with either the "UTC" or "LOCAL" options, i.e. "SECURE,UTC".
        @return The cookie value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER SetCookie (pcName    AS CHARACTER,
                                              pcValue   AS CHARACTER,
                                              pdDate    AS DATE,
                                              piTime    AS INTEGER,
                                              pcPath    AS CHARACTER,
                                              pcDomain  AS CHARACTER,
                                              pcOptions AS CHARACTER):
        
      RETURN DYNAMIC-FUNCTION ("set-cookie":U IN WebContext:web-utilities-hdl, 
                               pcName,
                               pcValue,
                               pdDate,
                               piTime,
                               pcPath,
                               pcDomain,
                               pcOptions) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Set's a value for a user field
        Notes:   
        @param pcName The name of the user field to set
        @param pcValue The value for the user field 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SetUserField (pcName AS CHARACTER,
                                            pcValue AS CHARACTER):
        
      DYNAMIC-FUNCTION ("set-user-field":U IN WebContext:web-utilities-hdl, 
                        pcName, 
                        pcValue) .
                        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Encodes unsafe characters in a URL as per RFC 1738 section 2.2.
                 http://ds.internic.net/rfc/rfc1738.txt, 2.2
        Notes:   
        @param pcValue Character string to encode
        @param poEncoding Encoding option where "query", "cookie", "default" or any specified string of characters are valid.
        @return The encoded character string
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER UrlEncode (pcValue AS CHARACTER,    
                                              poEncoding AS UrlEncodeTypeEnum):
        
      RETURN DYNAMIC-FUNCTION ("url-encode":U IN WebContext:web-utilities-hdl, 
                               pcValue, 
                               poEncoding:ToString()) .
                        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns an absolute URL to a json data source 
        Notes:   
        @param poClass The reference to the Class of the web object who's URL is required
        @return The absolute URL to the web object 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER UrlToDataObject (poClass AS Progress.Lang.Class):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poClass} .
        
        RETURN SUBSTITUTE ("&1/JsonData/&2":U,
                           WebContext:AppUrl,
                           REPLACE (poClass:TypeName, ".":U, "/":U)) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns an absolute URL to a Page object
        Notes:   
        @param poPage The reference to the Page object who's URL is required
        @return The absolute URL to the Page object 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER UrlToPageObject (poPage AS Page):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poPage} .
        
        RETURN UrlToPageObject (poPage:GetClass ()).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns an absolute URL to a web object
        Notes:   
        @param poClass The reference to the Class of the web object who's URL is required
        @return The absolute URL to the web object 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER UrlToPageObject (poClass AS Progress.Lang.Class):
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poClass} .
        
        RETURN SUBSTITUTE ("&1/SmartWeb/&2":U,
                           WebContext:AppUrl,
                           REPLACE (poClass:TypeName, ".":U, "/":U)) .

    END METHOD .
    
END CLASS.
