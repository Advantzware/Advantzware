/*------------------------------------------------------------------------
    File        : api/CallOutboundAPI.p
    Purpose     : Triggers the outbound APIs

    Syntax      :

    Description : Triggers the outbound APIs

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER ipcAPIID        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplcRequestData AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess      AS LOGICAL   NO-UNDO.

DEFINE VARIABLE gcEndPoint         AS CHARACTER NO-UNDO.
DEFINE VARIABLE glIsSSLEnabled     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcAuthType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcUserName         AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcPassword         AS CHARACTER NO-UNDO.
DEFINE VARIABLE glcRequestData     AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE glcReponseData     AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE gcRequestDataType  AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcResponseDataType AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcResponseHandler  AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcRequestVerb      AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcClientID         AS CHARACTER NO-UNDO.

DEFINE VARIABLE gcCommandResult   AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCommand         AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcRequestFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcResponseFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcDateTime        AS CHARACTER NO-UNDO.
DEFINE VARIABLE gdDateTime        AS DATETIME  NO-UNDO.
DEFINE VARIABLE gcSuccess         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glAPIConfigFound  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcParentProgram   AS CHARACTER NO-UNDO.

ASSIGN
    gcParentProgram = PROGRAM-NAME(2)
    glcRequestData  = iplcRequestData
    gdDateTime      = NOW
    .

FOR FIRST APIOutbound
    WHERE APIOutbound.APIID = ipcAPIID 
      AND APIOutbound.isActive  NO-LOCK:
    ASSIGN
        gcUserName         = APIOutbound.username
        gcPassword         = APIOutbound.password
        gcRequestDataType  = APIOutbound.requestDataType
        gcResponseDataType = gcRequestDataType 
        glIsSSLEnabled     = APIOutbound.isSSLEnabled
        gcRequestVerb      = APIOutbound.requestVerb
        gcClientID         = APIOutbound.clientID
        gcAuthType         = APIOutbound.authType
        gcEndPoint         = APIOutbound.endPoint
        gcResponseHandler  = APIOutbound.responseHandler
        ipcAPIID           = APIOutbound.APIID
        glAPIConfigFound   = YES
        .
END.

IF NOT glAPIConfigFound THEN DO:
    ASSIGN 
        opcMessage = "Config for API ID " + ipcAPIID + " not available or inactive in APIOutbound table"
        oplSuccess = NO
        .
        
    /* add a record in APIOutboundEvent table here*/
    RUN pCreateAPIOutboundEvent (
        INPUT ipcAPIID,
        INPUT glcRequestData,
        INPUT glcReponseData,
        INPUT gcParentProgram,
        INPUT oplSuccess,
        INPUT opcMessage,
        INPUT gdDateTime
        ).
        
    RETURN.
END.

ASSIGN
    gcDateTime     = REPLACE(STRING(gdDateTime, "99/99/9999 HH:MM:SS.sss"), "/", "")
    gcDateTime     = REPLACE(gcDateTime, ":", "")
    gcDateTime     = REPLACE(gcDateTime, " ", "")
    gcDateTime     = REPLACE(gcDateTime, ".", "")
    gcRequestFile  = "request"     + "_"
                   + ipcAPIID      + "_"      /* API ID    */
                   + gcClientID    + "_"      /* Client ID */
                   + gcRequestVerb + "_"      /* i.e. GET, POST, PUT? */
                   + gcDateTime               /* Date and Time */
                   + "." + lc(gcRequestDataType). /* File Extentions */
    gcResponseFile = "response"    + "_"
                   + ipcAPIID      + "_"      /* API ID    */
                   + gcClientID    + "_"      /* Client ID */
                   + gcRequestVerb + "_"      /* i.e. GET, POST, PUT? */
                   + gcDateTime               /* Date and Time */
                   + "." + lc(gcRequestDataType) /* File Extentions */
    NO-ERROR.
    
IF ERROR-STATUS:ERROR THEN DO:
    ASSIGN 
        opcMessage = "ERROR: " + ERROR-STATUS:GET-MESSAGE(1) + "~nAPIID [ " + ipcAPIID + " ]".
        oplSuccess = NO
        .
        
    /* add a record in APIOutboundEvent table here*/
    RUN pCreateAPIOutboundEvent (
        INPUT ipcAPIID,
        INPUT glcRequestData,
        INPUT glcReponseData,
        INPUT gcParentProgram,
        INPUT oplSuccess,
        INPUT opcMessage,
        INPUT gdDateTime
        ).
        
    RETURN.
END.

IF gcAuthType = "basic" THEN
   gcCommand = 'curl --user '
              + gcUserName + ':' + gcPassword + ' '
              + (IF NOT glIsSSLEnabled THEN '--insecure' ELSE '') + ' '
              + '-H "Content-Type: application/' +  lc(gcRequestDataType + '"') /* handles XML or JSON only - not RAW */
              + (IF gcRequestVerb NE 'get' THEN ' -d "@' + gcRequestFile + '" ' ELSE '')
              + (IF gcRequestVerb NE 'get' THEN ' -X ' + gcRequestVerb ELSE '')  + ' '
              + gcEndPoint
              + ' > ' + gcResponseFile.

IF gcCommand = '' THEN DO:
    ASSIGN 
        opcMessage = "Invalid Authentication Type [ " + gcAuthType + " ] found in config in APIOutbound table for APIID [ " + ipcAPIID  + " ]".
        oplSuccess = NO
        .
        
        
    /* add a record in APIOutboundEvent table here*/
    RUN pCreateAPIOutboundEvent (
        INPUT ipcAPIID,
        INPUT glcRequestData,
        INPUT glcReponseData,
        INPUT gcParentProgram,
        INPUT oplSuccess,
        INPUT opcMessage,
        INPUT gdDateTime
        ).
        
    RETURN. 
END.

/* Put Request Data from a variable into a Temporary file */
COPY-LOB glcRequestData TO FILE gcRequestFile.

/* execute CURL command with required parameters to call the API */
OS-COMMAND SILENT VALUE(gcCommand).

/* Put Response Data from Temporary file into a variable */
COPY-LOB FILE gcResponseFile TO glcReponseData.

/* read Response  */
RUN pReadResponse (
    INPUT  glcReponseData,
    INPUT  gcResponseDataType,
    OUTPUT opcMessage,
    OUTPUT oplSuccess
    ).

/* add a record in APIOutboundEvent table here*/
RUN pCreateAPIOutboundEvent (
    INPUT ipcAPIID,
    INPUT glcRequestData,
    INPUT glcReponseData,
    INPUT gcParentProgram,
    INPUT oplSuccess,
    INPUT opcMessage,
    INPUT gdDateTime
    ).

/* delete temporary files */
OS-DELETE VALUE(gcRequestFile).
OS-DELETE VALUE(gcResponseFile).


PROCEDURE pReadResponse PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Reads outbound response data based on data type
    Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE INPUT  PARAMETER iplcReponseData     AS LONGCHAR  NO-UNDO.
    DEFINE INPUT  PARAMETER ipcReponseDataType  AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReadMode   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRetValue   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE hdttJSON    AS HANDLE    NO-UNDO.

    CASE ipcReponseDataType:
        WHEN "JSON" THEN DO:
            /* JSON processing goes here */
            CREATE TEMP-TABLE hdttJSON.
                
            ASSIGN
                cSourceType = "longchar"
                cReadMode   = "empty"
                oplSuccess  = NO
                opcMessage  = "bad JSON"
                .
            
            lRetValue = hdttJSON:READ-JSON(cSourceType, iplcReponseData, cReadMode) NO-ERROR.
            IF NOT lRetValue THEN
                RETURN.

            RUN VALUE(gcResponseHandler) (
                hdttJSON,
                OUTPUT opcMessage,
                OUTPUT oplSuccess
                ).  

        END.
        WHEN "XML" THEN DO:
            /* XML processing goes here */
        END.
        OTHERWISE DO:
            ASSIGN 
                opcMessage = "Invalid Response Data Type [ " + ipcReponseDataType + " ] found in config in APIOutbound table for APIID [ " + ipcAPIID  + " ]".
                oplSuccess = NO
		  .
        END.
    END.

    IF VALID-HANDLE(hdttJSON) THEN
        DELETE OBJECT hdttJSON.

END PROCEDURE.


PROCEDURE pCreateAPIOutboundEvent PRIVATE:
   /*------------------------------------------------------------------------------
   Purpose: Loads data into CreateAPIOutboundEvent 
   Notes:
   ------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ipcAPIID        AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iplcRequestData AS LONGCHAR  NO-UNDO.
   DEFINE INPUT PARAMETER iplcReponseData AS LONGCHAR  NO-UNDO.
   DEFINE INPUT PARAMETER ipcProgramName  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iplSuccess      AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER ipcMessage      AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ipcDateTime     AS DATETIME  NO-UNDO.

   RUN api/CreateAPIOutboundEvent.p (
       INPUT ipcAPIID,
       INPUT glcRequestData,
       INPUT glcReponseData,
       INPUT gcParentProgram,
       INPUT oplSuccess,
       INPUT opcMessage,
       INPUT gdDateTime
       ).

END PROCEDURE.
