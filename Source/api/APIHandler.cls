 
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
	 Purpose: Converts the mutli byte and a few problematic (smart quotes) utf-8 
	          characters to ISO-8859-1 character set 
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC LONGCHAR ConvertUTF8ToISO8859-1( iplcRequestData AS LONGCHAR, ipcContentType AS CHARACTER ):
        DEFINE VARIABLE lcData AS LONGCHAR NO-UNDO.
        
        lcData = iplcRequestData.
        
        ASSIGN
            lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(147), '-')                //  en  dash    
            lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(148), '-')                //  em  dash    
            lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(149), '-')                //  horizontal  bar 
            lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(151), '_')                //  double  low line
            lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(152), "'")                //  left    single  quotation
            lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(153), "'")                //  right   single  quotation
            lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(154), ',')                //  single  low-9   quotation
            lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(155), "'")                //  single  high-reversed-9 quotation
            lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(166), '...')              //  horizontal  ellipsis    
            lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(178), "'")                //  prime       
            .
        
        /* Escape the double quotes for json */
        IF ipcContentType EQ "application/json" THEN
            ASSIGN        
                lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(156), '\"')               //  left    double  quotation
                lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(157), '\"')               //  right   double  quotation
                lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(158), '\"')               //  double  low-9   quotation
                lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(179), '\"')               //  double  prime   
                .
        ELSE
            ASSIGN        
                lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(156), '"')               //  left    double  quotation
                lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(157), '"')               //  right   double  quotation
                lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(158), '"')               //  double  low-9   quotation
                lcData = REPLACE(lcData, CHR(226) + CHR(128) + CHR(179), '"')               //  double  prime   
                .
        
        /* Replacing additional multi byte characters */
        ASSIGN
            lcData = REPLACE(lcData, CHR(194) + CHR(160),  ' ')                  // Non-breaking space (&nbsp;). Type Alt+0160
            lcData = REPLACE(lcData, CHR(194) + CHR(161),  '¡')                  // Inverted exclamation
            lcData = REPLACE(lcData, CHR(194) + CHR(162),  '¢')                  // Cent sign
            lcData = REPLACE(lcData, CHR(194) + CHR(163),  '£')                  // Pound sterling
            lcData = REPLACE(lcData, CHR(194) + CHR(164),  '¤')                  // General currency sign
            lcData = REPLACE(lcData, CHR(194) + CHR(165),  '¥')                  // Yen sign
            lcData = REPLACE(lcData, CHR(194) + CHR(166),  '¦')                  // Broken vertical bar
            lcData = REPLACE(lcData, CHR(194) + CHR(167),  '§')                  // Section sign
            lcData = REPLACE(lcData, CHR(194) + CHR(168),  '¨')                  // Umlaut (dieresis)
            lcData = REPLACE(lcData, CHR(194) + CHR(169),  '©')                  // Copyright
            lcData = REPLACE(lcData, CHR(194) + CHR(170),  'ª')                  // Feminine ordinal
            lcData = REPLACE(lcData, CHR(194) + CHR(171),  '«')                  // Left angle quote, guillemotleft
            lcData = REPLACE(lcData, CHR(194) + CHR(172),  '¬')                  // Not sign
            lcData = REPLACE(lcData, CHR(194) + CHR(173),  '­')                  // Soft hyphen
            lcData = REPLACE(lcData, CHR(194) + CHR(174),  '®')                  // Registered trademark
            lcData = REPLACE(lcData, CHR(194) + CHR(175),  '¯')                  // Macron accent
            lcData = REPLACE(lcData, CHR(194) + CHR(176),  '°')                  // Degree sign
            lcData = REPLACE(lcData, CHR(194) + CHR(177),  '±')                  // Plus or minus
            lcData = REPLACE(lcData, CHR(194) + CHR(178),  '²')                  // Superscript two
            lcData = REPLACE(lcData, CHR(194) + CHR(179),  '³')                  // Superscript three
            lcData = REPLACE(lcData, CHR(194) + CHR(180),  '´')                  // Acute accent
            lcData = REPLACE(lcData, CHR(194) + CHR(181),  'µ')                  // Micro sign
            lcData = REPLACE(lcData, CHR(194) + CHR(182),  '¶')                  // Paragraph sign
            lcData = REPLACE(lcData, CHR(194) + CHR(183),  '·')                  // Middle dot
            lcData = REPLACE(lcData, CHR(194) + CHR(184),  '¸')                  // Cedilla
            lcData = REPLACE(lcData, CHR(194) + CHR(185),  '¹')                  // Superscript one
            lcData = REPLACE(lcData, CHR(194) + CHR(186),  'º')                  // Masculine ordinal
            lcData = REPLACE(lcData, CHR(194) + CHR(187),  '»')                  // Right angle quote, guillemotright
            lcData = REPLACE(lcData, CHR(194) + CHR(188),  '¼')                  // Fraction one-fourth
            lcData = REPLACE(lcData, CHR(194) + CHR(189),  '½')                  // Fraction one-half
            lcData = REPLACE(lcData, CHR(194) + CHR(190),  '¾')                  // Fraction three-fourths
            lcData = REPLACE(lcData, CHR(194) + CHR(191),  '¿')                  // Inverted question mark
            lcData = REPLACE(lcData, CHR(195) + CHR(128),  'À')                  // Capital A, grave accent
            lcData = REPLACE(lcData, CHR(195) + CHR(129),  'Á')                  // Capital A, acute accent
            lcData = REPLACE(lcData, CHR(195) + CHR(130),  'Â')                  // Capital A, circumflex accent
            lcData = REPLACE(lcData, CHR(195) + CHR(131),  'Ã')                  // Capital A, tilde
            lcData = REPLACE(lcData, CHR(195) + CHR(132),  'Ä')                  // Capital A, dieresis or umlaut mark
            lcData = REPLACE(lcData, CHR(195) + CHR(133),  'Å')                  // Capital A, ring
            lcData = REPLACE(lcData, CHR(195) + CHR(134),  'Æ')                  // Capital AE dipthong (ligature)
            lcData = REPLACE(lcData, CHR(195) + CHR(135),  'Ç')                  // Capital C, cedilla
            lcData = REPLACE(lcData, CHR(195) + CHR(136),  'È')                  // Capital E, grave accent
            lcData = REPLACE(lcData, CHR(195) + CHR(137),  'É')                  // Capital E, acute accent
            lcData = REPLACE(lcData, CHR(195) + CHR(138),  'Ê')                  // Capital E, circumflex accent
            lcData = REPLACE(lcData, CHR(195) + CHR(139),  'Ë')                  // Capital E, dieresis or umlaut mark
            lcData = REPLACE(lcData, CHR(195) + CHR(140),  'Ì')                  // Capital I, grave accent
            lcData = REPLACE(lcData, CHR(195) + CHR(141),  'Í')                  // Capital I, acute accent
            lcData = REPLACE(lcData, CHR(195) + CHR(142),  'Î')                  // Capital I, circumflex accent
            lcData = REPLACE(lcData, CHR(195) + CHR(143),  'Ï')                  // Capital I, dieresis or umlaut mark
            lcData = REPLACE(lcData, CHR(195) + CHR(144),  'Ð')                  // Capital Eth, Icelandic
            lcData = REPLACE(lcData, CHR(195) + CHR(145),  'Ñ')                  // Capital N, tilde
            lcData = REPLACE(lcData, CHR(195) + CHR(146),  'Ò')                  // Capital O, grave accent
            lcData = REPLACE(lcData, CHR(195) + CHR(147),  'Ó')                  // Capital O, acute accent
            lcData = REPLACE(lcData, CHR(195) + CHR(148),  'Ô')                  // Capital O, circumflex accent
            lcData = REPLACE(lcData, CHR(195) + CHR(149),  'Õ')                  // Capital O, tilde
            lcData = REPLACE(lcData, CHR(195) + CHR(150),  'Ö')                  // Capital O, dieresis or umlaut mark
            lcData = REPLACE(lcData, CHR(195) + CHR(151),  '×')                  // Multiply sign
            lcData = REPLACE(lcData, CHR(195) + CHR(152),  'Ø')                  // Capital O, slash
            lcData = REPLACE(lcData, CHR(195) + CHR(153),  'Ù')                  // Capital U, grave accent
            lcData = REPLACE(lcData, CHR(195) + CHR(154),  'Ú')                  // Capital U, acute accent
            lcData = REPLACE(lcData, CHR(195) + CHR(155),  'Û')                  // Capital U, circumflex accent
            lcData = REPLACE(lcData, CHR(195) + CHR(156),  'Ü')                  // Capital U, dieresis or umlaut mark
            lcData = REPLACE(lcData, CHR(195) + CHR(157),  'Ý')                  // Capital Y, acute accent
            lcData = REPLACE(lcData, CHR(195) + CHR(158),  'Þ')                  // Capital THORN, Icelandic
            lcData = REPLACE(lcData, CHR(195) + CHR(159),  'ß')                  // Small sharp s, German (sz ligature)
            lcData = REPLACE(lcData, CHR(195) + CHR(160),  'à')                  // Small a, grave accent
            lcData = REPLACE(lcData, CHR(195) + CHR(161),  'á')                  // Small a, acute accent
            lcData = REPLACE(lcData, CHR(195) + CHR(162),  'â')                  // Small a, circumflex accent
            lcData = REPLACE(lcData, CHR(195) + CHR(163),  'ã')                  // Small a, tilde
            lcData = REPLACE(lcData, CHR(195) + CHR(164),  'ä')                  // Small a, dieresis or umlaut mark
            lcData = REPLACE(lcData, CHR(195) + CHR(165),  'å')                  // Small a, ring
            lcData = REPLACE(lcData, CHR(195) + CHR(166),  'æ')                  // Small ae dipthong (ligature)
            lcData = REPLACE(lcData, CHR(195) + CHR(167),  'ç')                  // Small c, cedilla
            lcData = REPLACE(lcData, CHR(195) + CHR(168),  'è')                  // Small e, grave accent
            lcData = REPLACE(lcData, CHR(195) + CHR(169),  'é')                  // Small e, acute accent
            lcData = REPLACE(lcData, CHR(195) + CHR(170),  'ê')                  // Small e, circumflex accent
            lcData = REPLACE(lcData, CHR(195) + CHR(171),  'ë')                  // Small e, dieresis or umlaut mark
            lcData = REPLACE(lcData, CHR(195) + CHR(172),  'ì')                  // Small i, grave accent
            lcData = REPLACE(lcData, CHR(195) + CHR(173),  'í')                  // Small i, acute accent
            lcData = REPLACE(lcData, CHR(195) + CHR(174),  'î')                  // Small i, circumflex accent
            lcData = REPLACE(lcData, CHR(195) + CHR(175),  'ï')                  // Small i, dieresis or umlaut mark
            lcData = REPLACE(lcData, CHR(195) + CHR(176),  'ð')                  // Small eth, Icelandic
            lcData = REPLACE(lcData, CHR(195) + CHR(177),  'ñ')                  // Small n, tilde
            lcData = REPLACE(lcData, CHR(195) + CHR(178),  'ò')                  // Small o, grave accent
            lcData = REPLACE(lcData, CHR(195) + CHR(179),  'ó')                  // Small o, acute accent
            lcData = REPLACE(lcData, CHR(195) + CHR(180),  'ô')                  // Small o, circumflex accent
            lcData = REPLACE(lcData, CHR(195) + CHR(181),  'õ')                  // Small o, tilde
            lcData = REPLACE(lcData, CHR(195) + CHR(182),  'ö')                  // Small o, dieresis or umlaut mark
            lcData = REPLACE(lcData, CHR(195) + CHR(183),  '÷')                  // Division sign
            lcData = REPLACE(lcData, CHR(195) + CHR(184),  'ø')                  // Small o, slash
            lcData = REPLACE(lcData, CHR(195) + CHR(185),  'ù')                  // Small u, grave accent
            lcData = REPLACE(lcData, CHR(195) + CHR(186),  'ú')                  // Small u, acute accent
            lcData = REPLACE(lcData, CHR(195) + CHR(187),  'û')                  // Small u, circumflex accent
            lcData = REPLACE(lcData, CHR(195) + CHR(188),  'ü')                  // Small u, dieresis or umlaut mark
            lcData = REPLACE(lcData, CHR(195) + CHR(189),  'ý')                  // Small y, acute accent
            lcData = REPLACE(lcData, CHR(195) + CHR(190),  'þ')                  // Small thorn, Icelandic
            lcData = REPLACE(lcData, CHR(195) + CHR(191),  'ÿ')                  // Small y, dieresis or umlaut mark
            .
                    
        RETURN lcData.
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: Method to process DELETE request
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC LONGCHAR pDelete( ipcURL AS CHARACTER ):
        DEFINE VARIABLE oContent AS HttpContent NO-UNDO.
                
        DEFINE VARIABLE lcResponseData AS LONGCHAR NO-UNDO. 
        
        ipcURL = pReplaceExceptionalCharacters(ipcURL).
        
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
        
        ipcURL = pReplaceExceptionalCharacters(ipcURL).
        
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
        
		DEFINE VARIABLE lcResponseData       AS LONGCHAR  NO-UNDO.	
		DEFINE VARIABLE cResponseContentType AS CHARACTER NO-UNDO.
		
		ipcURL = pReplaceExceptionalCharacters(ipcURL).
		
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
               As these are not escaped by server JSON parsing throws an error. Reading as byte array and later converting to ISO8859-1 fixed the issue */
            /* The current encoding for INPUT/OUTPUT files is ISO8859-1, hence we didn't have this error when API request went through cURL */
            oBytes = ResponseMessage:Content:ReadAsByteArrayAsync():RESULT.
             
            cResponseContentType = ResponseMessage:Content:Headers:ContentType:ToString() NO-ERROR.

            lcResponseData = Encoding:GetEncoding("iso8859-1"):GetString(oBytes).
        END.
        
        lcResponseData = ConvertUTF8ToISO8859-1(lcResponseData, cResponseContentType).
        
		RETURN lcResponseData.
	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Method for PATCH request type
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR Patch( ipcURL AS CHARACTER, iplcRequestData AS LONGCHAR ):
        RETURN pPatch(ipcURL, iplcRequestData).
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Private method for PATCH request type
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PRIVATE LONGCHAR pPatch( ipcURL AS CHARACTER, iplcRequestData AS LONGCHAR ):
        DEFINE VARIABLE oContent            AS HttpContent        NO-UNDO.
        DEFINE VARIABLE oHttpMethod         AS HttpMethod         NO-UNDO.
        DEFINE VARIABLE oHttpRequestMessage AS HttpRequestMessage NO-UNDO.
        
        DEFINE VARIABLE oBytes  AS "System.Byte[]":U NO-UNDO.
        
        DEFINE VARIABLE lcResponseData       AS LONGCHAR  NO-UNDO.  
        DEFINE VARIABLE cResponseContentType AS CHARACTER NO-UNDO.
        
        ipcURL = pReplaceExceptionalCharacters(ipcURL).
        
        oClient:BaseAddress = NEW System.Uri(ipcURL).
        
        oClient:DefaultRequestHeaders:Accept:Add(NEW System.Net.Http.Headers.MediaTypeWithQualityHeaderValue(ContentType)).       
        
        oHttpMethod = NEW HttpMethod("PATCH").
        
        oHttpRequestMessage = NEW HttpRequestMessage(oHttpMethod, ipcURL).
        
        IF ContentType EQ "application/x-www-form-urlencoded" AND iplcRequestData EQ "" THEN DO:
            oContent = NEW FormUrlEncodedContent(oURLEncodedData).
        END.
        ELSE DO: 
            oContent = NEW StringContent(iplcRequestData, System.Text.UTF8Encoding:UTF8, ContentType).            
        END.

        oContent:Headers:ContentType:CharSet = "".

        oHttpRequestMessage:Content = oContent.
        
        ResponseMessage = oClient:SendAsync(oHttpRequestMessage):RESULT NO-ERROR.          
        
        FIX-CODEPAGE(lcResponseData) = 'utf-8'.
        
        IF VALID-OBJECT(ResponseMessage) THEN DO:
            /* Looks like ReadAsStringAsync returns data in UTF-8 encoded format and replaces right/left double quotations with normal double quotation.
               As these are not escaped by server JSON parsing throws an error. Reading as byte array and later converting to ISO8859-1 fixed the issue */
            /* The current encoding for INPUT/OUTPUT files is ISO8859-1, hence we didn't have this error when API request went through cURL */
            oBytes = ResponseMessage:Content:ReadAsByteArrayAsync():RESULT.
             
            cResponseContentType = ResponseMessage:Content:Headers:ContentType:ToString() NO-ERROR.

            lcResponseData = Encoding:GetEncoding("iso8859-1"):GetString(oBytes).
        END.
        
        lcResponseData = ConvertUTF8ToISO8859-1(lcResponseData, cResponseContentType).
        
        RETURN lcResponseData.
    END METHOD.
    
	/*------------------------------------------------------------------------------
	 Purpose: cURL fails when there are & in URL. Escaping with ^ can fix this issue. But fails with HttpClient method. 
	          Removing carats in URL should fix this. 
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE CHARACTER pReplaceExceptionalCharacters( ipcURL AS CHARACTER ):
		
		RETURN REPLACE(ipcURL, "^&", "&").

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