 
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
USING System.Text.RegularExpressions.Regex FROM ASSEMBLY.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS API.APIHandler: 
    DEFINE VARIABLE oClient AS HttpClient NO-UNDO.
    
    /* Saves form-data and x-www-form-unrlencoded key value pairs  */
    DEFINE VARIABLE oURLEncodedData AS "System.Collections.Generic.Dictionary<String, String>" NO-UNDO.   

    /* Stores the content type information. Set will internally convert to Rest specific content type */
	DEFINE PUBLIC PROPERTY ContentType AS CHARACTER NO-UNDO INITIAL "text/plain" 
	GET.
	SET(INPUT ipcContentType AS CHARACTER):
        IF INDEX(ipcContentType, "application") EQ 0 THEN
            ContentType = "application/" + ipcContentType. 				
	END SET.	 

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

        oClient = NEW HttpClient().
        
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
     Purpose: Method to add header data
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID AddHeaderData( ipcKey AS CHARACTER, ipcValue AS CHARACTER ):
        oClient:DefaultRequestHeaders:Add(ipcKey, ipcValue).
    END METHOD.
    
    /*------------------------------------------------------------------------------
     Purpose: Method for GET request types
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR Get( ipcURL AS CHARACTER ):
        RETURN pGet(ipcURL).
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Method for DELETE request type
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR Delete( ipcURL AS CHARACTER ):
        RETURN pDelete(ipcURL).
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
	 Purpose: Method to process DELETE request
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC LONGCHAR pDelete( ipcURL AS CHARACTER ):
        DEFINE VARIABLE oContent AS HttpContent NO-UNDO.
                
        DEFINE VARIABLE lcResponseData AS LONGCHAR NO-UNDO. 
        
        oClient:BaseAddress = NEW System.Uri(ipcURL).
        oClient:DefaultRequestHeaders:Accept:Add(NEW System.Net.Http.Headers.MediaTypeWithQualityHeaderValue(ContentType)).

        ResponseMessage = oClient:DeleteAsync(ipcURL):RESULT NO-ERROR.          
        
        FIX-CODEPAGE(lcResponseData) = 'utf-8'.
        
        IF VALID-OBJECT(ResponseMessage) THEN
            lcResponseData = ResponseMessage:Content:ReadAsStringAsync():RESULT.
        
        RETURN lcResponseData.	
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: Private method for GET request type
	 Notes:
	------------------------------------------------------------------------------*/

    METHOD PRIVATE LONGCHAR pGet( ipcURL AS CHARACTER ):
        DEFINE VARIABLE oContent AS HttpContent NO-UNDO.
                
        DEFINE VARIABLE lcResponseData AS LONGCHAR NO-UNDO. 
        
        oClient:BaseAddress = NEW System.Uri(ipcURL).

        ResponseMessage = oClient:GetAsync(ipcURL):RESULT NO-ERROR.          
        
        FIX-CODEPAGE(lcResponseData) = 'utf-8'.
        
        IF VALID-OBJECT(ResponseMessage) THEN
            lcResponseData = ResponseMessage:Content:ReadAsStringAsync():RESULT.
        
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
        DEFINE VARIABLE oContent         AS HttpContent          NO-UNDO.
        
        DEFINE VARIABLE oBytes  AS "System.Byte[]":U NO-UNDO.
        
		DEFINE VARIABLE lcResponseData AS LONGCHAR NO-UNDO.	
		
		oClient:BaseAddress = NEW System.Uri(ipcURL).
		oClient:DefaultRequestHeaders:Accept:Add(NEW System.Net.Http.Headers.MediaTypeWithQualityHeaderValue(ContentType)).       
        
        IF ContentType EQ "application/x-www-form-urlencoded" AND iplcRequestData EQ "" THEN DO:
            oContent = NEW FormUrlEncodedContent(oURLEncodedData).
        END.
        ELSE DO: 
            oContent = NEW StringContent(iplcRequestData, System.Text.UTF8Encoding:UTF8, ContentType).            
        END.

        oContent:Headers:ContentType:CharSet = "".
        
        ResponseMessage = oClient:PostAsync(ipcURL, oContent):RESULT NO-ERROR.          
        
        FIX-CODEPAGE(lcResponseData) = 'utf-8'.
        
        IF VALID-OBJECT(ResponseMessage) THEN DO:
            /* Looks like ReadAsStringAsync returns data in UTF-8 encoded format and replaces right/left double quotations with normal double quotation.
               As these are not escaped by server, JSON parsing throws an error. Reading as byte array and later converting to ISO8859-1 fixed the issue */
            /* The current encoding for INPUT/OUTPUT files is ISO8859-1 and hence we didn't have this error when API is called through cURL */
            oBytes = ResponseMessage:Content:ReadAsByteArrayAsync():RESULT.

            lcResponseData = Encoding:GetEncoding("ISO8859-1"):GetString(oBytes).
        END.
        
		RETURN lcResponseData.
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: Method to set the current authentication type to basic and set username and password
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID SetBasicAuthentication(ipcUserName AS CHARACTER, ipcPassword AS CHARACTER ):		
        DEFINE VARIABLE oCredentials AS "System.Byte[]" NO-UNDO.
        
        oCredentials = UTF8Encoding:UTF8:GetBytes(ipcUserName + ":" + ipcPassword).       
        oClient:DefaultRequestHeaders:Authorization = NEW System.Net.Http.Headers.AuthenticationHeaderValue("Basic", System.Convert:ToBase64String(oCredentials)).      
	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Method to set the current authentication type to bearer and set bearer token
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID SetBearerAuthentication(ipcBearerToken AS CHARACTER ):
        oClient:DefaultRequestHeaders:Authorization = NEW System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", ipcBearerToken).
    END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC APIHandler ( ):
        oClient:Dispose().        
	END DESTRUCTOR.


END CLASS.