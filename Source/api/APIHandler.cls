 
 /*------------------------------------------------------------------------
    File        : APIHandler
    Purpose     : Handler to request API calls
    Syntax      : 
    Description : 
    Author(s)   : DEVA$!
    Created     : Tue Jun 01 05:00:34 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING System.*.
USING System.Text.*.
USING System.Net.*.
USING System.Net.Http.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS API.APIHandler: 
    /* Saves form-data and x-www-form-unrlencoded key value pairs  */
    DEFINE VARIABLE oURLEncodedData AS "System.Collections.Generic.Dictionary<String, String>" NO-UNDO.
    
    /* Stores the authentication type. Currently supports only basic and bearer token authentications */
	DEFINE PUBLIC PROPERTY AuthenticationType AS CHARACTER NO-UNDO 
	GET.
	SET(INPUT ipcAuthenticationType AS CHARACTER):
        IF LOOKUP(ipcAuthenticationType, "basic,bearer") EQ 0 THEN
            AuthenticationType = "none".           
        ELSE
            AuthenticationType = ipcAuthenticationType.   
    END SET.

	DEFINE PUBLIC PROPERTY BearerToken AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /* Stores the content type information. Set will internally convert to Rest specific content type */
	DEFINE PUBLIC PROPERTY ContentType AS CHARACTER NO-UNDO INITIAL "text/plain" 
	GET.
	SET(INPUT ipcContentType AS CHARACTER):
        IF INDEX(ipcContentType, "application") EQ 0 THEN
            ContentType = "application/" + ipcContentType. 				
	END SET.	 

	DEFINE PRIVATE PROPERTY Password AS CHARACTER NO-UNDO 
	GET.
	SET. 

	DEFINE PRIVATE PROPERTY UserName AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /* Stores all the information related to the response from server */
    DEFINE PRIVATE PROPERTY ResponseMessage AS HttpResponseMessage  NO-UNDO
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	CONSTRUCTOR PUBLIC APIHandler (  ):
        DEFINE VARIABLE oProtocolType    AS SecurityProtocolType NO-UNDO.    
	    
		oURLEncodedData = NEW "System.Collections.Generic.Dictionary<String, String>"().	

        oProtocolType = CAST(Progress.Util.EnumHelper:Or(SecurityProtocolType:Tls12, SecurityProtocolType:Tls11), SecurityProtocolType).
        oProtocolType = CAST(Progress.Util.EnumHelper:Or(oProtocolType, SecurityProtocolType:Tls), SecurityProtocolType).
    
        /* Set the supported security protocal types */
        ServicePointManager:SecurityProtocol = oProtocolType.
	END CONSTRUCTOR.
    
	/*------------------------------------------------------------------------------
	 Purpose: Method to add url-encoded data
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID AddURLEncodeData( ipcKey AS CHARACTER, iplcValue AS LONGCHAR ):
        oURLEncodedData:ADD(ipcKey, iplcValue).
	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Method for GET request types
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR Get( ipcURL AS CHARACTER ):
        RETURN pGet(ipcURL).
    END METHOD.
    
	/*------------------------------------------------------------------------------
	 Purpose: Returns the Status code returned by server
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC CHARACTER GetResponseStatusCode(  ):
        IF VALID-OBJECT (ResponseMessage) THEN
            RETURN STRING(ResponseMessage:StatusCode).
        ELSE
            RETURN "".
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: Returns if a successfull response is received
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC LOGICAL IsRequestSuccessful(  ):
		IF VALID-OBJECT (ResponseMessage) THEN
		    RETURN ResponseMessage:IsSuccessStatusCode.
		ELSE
		    RETURN FALSE.
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: Private method for GET request type
	 Notes:
	------------------------------------------------------------------------------*/

    METHOD PRIVATE LONGCHAR pGet( ipcURL AS CHARACTER ):
        DEFINE VARIABLE oClient          AS HttpClient           NO-UNDO.
        DEFINE VARIABLE oContent         AS HttpContent          NO-UNDO.
        DEFINE VARIABLE oCredentials     AS "System.Byte[]"      NO-UNDO.
        
        DEFINE VARIABLE lcResponseData AS LONGCHAR NO-UNDO. 
        
        oClient = NEW HttpClient().
        oClient:BaseAddress = NEW System.Uri(ipcURL).
        oClient:DefaultRequestHeaders:Accept:Add(NEW System.Net.Http.Headers.MediaTypeWithQualityHeaderValue(ContentType)).
        
        IF AuthenticationType EQ "Basic" THEN DO:
            oCredentials = UTF8Encoding:UTF8:GetBytes(UserName + ":" + Password).       
            oClient:DefaultRequestHeaders:Authorization = NEW System.Net.Http.Headers.AuthenticationHeaderValue("Basic", System.Convert:ToBase64String(oCredentials)).      
        END.
        ELSE IF AuthenticationType EQ "Bearer" THEN DO:
            oClient:DefaultRequestHeaders:Authorization = NEW System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", BearerToken).
        END.

        ResponseMessage = oClient:GetAsync(ipcURL):RESULT NO-ERROR.          
        
        FIX-CODEPAGE(lcResponseData) = 'utf-8'.
        
        IF VALID-OBJECT(ResponseMessage) THEN
            lcResponseData = ResponseMessage:Content:ReadAsStringAsync():RESULT.

        oClient:Dispose().
        
        RETURN lcResponseData.
    END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: Method for POST request type
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC LONGCHAR Post( ipcURL AS CHARACTER, iplcRequestData AS LONGCHAR ):
        RETURN pPost(ipcURL, iplcRequestData).
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: Private method for POST request type
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE LONGCHAR pPost( ipcURL AS CHARACTER, iplcRequestData AS LONGCHAR ):
        DEFINE VARIABLE oClient          AS HttpClient           NO-UNDO.
        DEFINE VARIABLE oContent         AS HttpContent          NO-UNDO.
        DEFINE VARIABLE oCredentials     AS "System.Byte[]"      NO-UNDO.
        
		DEFINE VARIABLE lcResponseData AS LONGCHAR NO-UNDO.	
		
		oClient = NEW HttpClient().
		oClient:BaseAddress = NEW System.Uri(ipcURL).
		oClient:DefaultRequestHeaders:Accept:Add(NEW System.Net.Http.Headers.MediaTypeWithQualityHeaderValue(ContentType)).
        
        IF AuthenticationType EQ "Basic" THEN DO:
            oCredentials = UTF8Encoding:UTF8:GetBytes(UserName + ":" + Password). 		
            oClient:DefaultRequestHeaders:Authorization = NEW System.Net.Http.Headers.AuthenticationHeaderValue("Basic", System.Convert:ToBase64String(oCredentials)).		
        END.
        ELSE IF AuthenticationType EQ "Bearer" THEN DO:
            oClient:DefaultRequestHeaders:Authorization = NEW System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", BearerToken).
        END.

        IF ContentType EQ "application/x-www-form-urlencoded" AND iplcRequestData EQ "" THEN DO:
            ResponseMessage = oClient:PostAsync(ipcURL, NEW FormUrlEncodedContent(oURLEncodedData)):RESULT NO-ERROR.
        END.
        ELSE DO: 
            oContent = NEW StringContent(iplcRequestData, System.Text.UTF8Encoding:UTF8, ContentType).
            
            oContent:Headers:ContentType:CharSet = "".
            
            ResponseMessage = oClient:PostAsync(ipcURL, oContent):RESULT NO-ERROR.          
        END.
        
        FIX-CODEPAGE(lcResponseData) = 'utf-8'.
        
        IF VALID-OBJECT(ResponseMessage) THEN
            lcResponseData = ResponseMessage:Content:ReadAsStringAsync():RESULT.
        
		oClient:Dispose().
		
		RETURN lcResponseData.
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: Method to set the current authentication type to basic and set username and password
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID SetBasicAuthentication(ipcUserName AS CHARACTER, ipcPassword AS CHARACTER ):
		
		ASSIGN
		    AuthenticationType = "Basic"
		    UserName           = ipcUserName
		    Password           = ipcPassword
		    .

	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Method to set the current authentication type to bearer and set bearer token
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID SetBearerAuthentication(ipcBearerToke AS CHARACTER ):
        
        ASSIGN
            AuthenticationType = "Bearer"
            BearerToken        = ipcBearerToke
            .

    END METHOD.
END CLASS.