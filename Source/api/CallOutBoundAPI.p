/*------------------------------------------------------------------------
    File        : api/CallOutboundAPI.p
    Purpose     : Triggers the outbound APIs

    Syntax      :

    Description : Triggers the outbound APIs

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER ipcAPIOutboundID AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER iplcRequestData  AS LONGCHAR  NO-UNDO.
DEFINE INPUT  PARAMETER ipcParentProgram AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcPrimaryID     AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplcResponseData AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.

DEFINE VARIABLE gcEndPoint         AS CHARACTER NO-UNDO.
DEFINE VARIABLE glIsSSLEnabled     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcAuthType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcUserName         AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcPassword         AS CHARACTER NO-UNDO.
DEFINE VARIABLE glcRequestData     AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE glcResponseData    AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE gcRequestDataType  AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcRequestType      AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcResponseDataType AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcResponseHandler  AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcRequestVerb      AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcClientID         AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcAPIID            AS CHARACTER NO-UNDO.
DEFINE VARIABLE glSaveFile         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcSaveFileFolder   AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcHostSSHKey       AS CHARACTER NO-UNDO.

DEFINE VARIABLE gcCommandResult   AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCommand         AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcRequestFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcResponseFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcDateTime        AS CHARACTER NO-UNDO.
DEFINE VARIABLE gdDateTime        AS DATETIME  NO-UNDO.
DEFINE VARIABLE gcSuccess         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glAPIConfigFound  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcParentProgram   AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdFTPProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE lSuccess   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.

ASSIGN
    gcParentProgram = ipcParentProgram
    glcRequestData  = iplcRequestData
    gdDateTime      = NOW
    .

FOR FIRST APIOutbound NO-LOCK
    WHERE APIOutbound.apiOutboundID EQ ipcAPIOutboundID 
      AND APIOutbound.Inactive      EQ FALSE:
    ASSIGN
        gcUserName         = APIOutbound.username
        gcPassword         = APIOutbound.password
        gcRequestDataType  = APIOutbound.requestDataType
        gcResponseDataType = gcRequestDataType 
        gcRequestType      = APIOutbound.requestType
        glIsSSLEnabled     = APIOutbound.isSSLEnabled
        gcRequestVerb      = APIOutbound.requestVerb
        gcClientID         = APIOutbound.clientID
        gcAuthType         = APIOutbound.authType
        gcEndPoint         = APIOutbound.endPoint
        gcResponseHandler  = APIOutbound.responseHandler
        gcAPIID            = APIOutbound.apiID
        gcSaveFileFolder   = APIOutbound.saveFileFolder
        glSaveFile         = APIOutbound.saveFile
        gcHostSSHKey       = APIOutbound.hostSSHKey
        glAPIConfigFound   = YES
        .
END.

IF NOT glAPIConfigFound THEN DO:
    ASSIGN 
        opcMessage = "Config for Outbound API Sequence ID [" 
                   + STRING(ipcAPIOutboundID) 
                   + "] not available or inactive in APIOutbound table"
        oplSuccess = NO
        .
                
    RETURN.
END.

ASSIGN
    gcDateTime = REPLACE(STRING(gdDateTime, "99/99/9999 HH:MM:SS.sss"), "/", "")
    gcDateTime = REPLACE(gcDateTime, ":", "")
    gcDateTime = REPLACE(gcDateTime, " ", "")
    gcDateTime = REPLACE(gcDateTime, ".", "")
    NO-ERROR.
    
IF ERROR-STATUS:ERROR THEN DO:
    ASSIGN 
        opcMessage = "ERROR: " + ERROR-STATUS:GET-MESSAGE(1) + "~nAPIID [ " + gcAPIID + " ]".
        oplSuccess = NO
        .
                
    RETURN.
END.

IF SEARCH("curl.exe") EQ ? THEN DO:
    ASSIGN 
        opcMessage = "curl not found!".
        oplSuccess = NO
        .
             
    RETURN. 
END.

IF (gcRequestType EQ "FTP" OR gcRequestType EQ "SFTP" OR gcRequestType EQ "SAVE") AND NOT glSaveFile THEN DO:
    ASSIGN
        oplSuccess = FALSE
        opcMessage = "File saving option is not enabled for [" + gcAPIID + "] API Oubound"
        .
    RETURN.
END.

IF glSaveFile THEN DO:
    RUN FileSys_CreateDirectory (
        INPUT  gcSaveFileFolder,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
    IF NOT oplSuccess THEN
        RETURN.

    RUN FileSys_GetFilePath (
        INPUT  gcSaveFileFolder,
        OUTPUT gcSaveFileFolder,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).    
    IF NOT oplSuccess THEN
        RETURN.

    ASSIGN
        gcRequestFile = RIGHT-TRIM(gcSaveFileFolder, "/")
        gcRequestFile = RIGHT-TRIM(gcSaveFileFolder, "\")
        .

    ASSIGN
        gcResponseFile = gcRequestFile + "\"      /* Save Folder */
                       + gcAPIID       + "_"      /* API ID    */
                       + gcClientID    + "_"      /* Client ID */
                       + ipcPrimaryID  + "_"      /* i.e. GET, POST, PUT? */
                       + gcDateTime               /* Date and Time */
                       + "." + "log"
        gcRequestFile  = gcRequestFile + "\"      /* Save Folder */
                       + gcAPIID       + "_"      /* API ID    */
                       + gcClientID    + "_"      /* Client ID */
                       + ipcPrimaryID  + "_"      /* i.e. GET, POST, PUT? */
                       + gcDateTime               /* Date and Time */
                       + "." + lc(gcRequestDataType). /* File Extentions */
        .

    COPY-LOB iplcRequestData TO FILE gcRequestFile.
    OS-COPY VALUE (gcRequestFile) VALUE (gcSaveFileFolder).    
END.

/* Return as file would have been already saved */
IF gcRequestType EQ "SAVE" THEN DO:
    oplcResponseData = "File saved to " + gcRequestFile.
    
    RETURN.
END.

/* Request for FTP transfer of the request data */
IF gcRequestType EQ "FTP" OR gcRequestType EQ "SFTP" THEN DO:
    RUN system/ftpProcs.p PERSISTENT SET hdFTPProcs.
    
    RUN FTP_SendFileWithCurl IN hdFTPProcs (
        INPUT  gcEndPoint,
        INPUT  gcRequestType,
        INPUT  gcUserName,
        INPUT  gcPassword,
        INPUT  gcRequestFile,
        INPUT  gcResponseFile,
        INPUT  glIsSSLEnabled,
        INPUT  gcHostSSHKey,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).

    DELETE PROCEDURE hdFTPProcs.
    
    oplcResponseData = "FTP transfer status is saved to file " + gcResponseFile.
     
    RETURN.
END.

ASSIGN
    gcRequestFile  = "request"     + "_"
                   + gcAPIID       + "_"      /* API ID    */
                   + gcClientID    + "_"      /* Client ID */
                   + gcRequestVerb + "_"      /* i.e. GET, POST, PUT? */
                   + gcDateTime               /* Date and Time */
                   + "." + lc(gcRequestDataType). /* File Extentions */
    gcResponseFile = "response"    + "_"
                   + gcAPIID       + "_"      /* API ID    */
                   + gcClientID    + "_"      /* Client ID */
                   + gcRequestVerb + "_"      /* i.e. GET, POST, PUT? */
                   + gcDateTime               /* Date and Time */
                   + "." + lc(gcRequestDataType) /* File Extentions */
    .

gcCommand = SEARCH("curl.exe") 
          + (IF gcAuthType = "basic" THEN ' --user ' + gcUserName + ':' + gcPassword 
             ELSE IF gcAuthType = "bearer" THEN ' -H "Authorization: Bearer ' + gcPassword + '"' 
             ELSE "") + ' ' 
          + (IF NOT glIsSSLEnabled THEN '--insecure' ELSE '') + ' '
          + '-H "Content-Type: application/' +  lc(gcRequestDataType + '"') /* handles XML or JSON only - not RAW */
          + (IF gcRequestVerb NE 'get' THEN ' -d "@' + gcRequestFile + '" ' ELSE '')
          + (IF gcRequestVerb NE 'get' THEN ' -X ' + gcRequestVerb ELSE '')  + ' '
          + gcEndPoint.

/* Put Request Data from a variable into a Temporary file */
COPY-LOB glcRequestData TO FILE gcRequestFile.
   
/* execute CURL command with requiredif  parameters to call the API */
RUN OS_RunCommand (
    INPUT  gcCommand,             /* Command string to run */
    INPUT  gcResponseFile,        /* File name to write the command output */
    INPUT  TRUE,                  /* Run with SILENT option */
    INPUT  FALSE,                 /* Run with NO-WAIT option */
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ) NO-ERROR.
IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN DO:
    ASSIGN
        oplSuccess = FALSE
        opcMessage = "Error excuting curl command"
        .

    /* delete temporary files */
    OS-DELETE VALUE(gcRequestFile).
    OS-DELETE VALUE(gcResponseFile).
        
    RETURN.
END.
    
/* Put Response Data from Temporary file into a variable */
COPY-LOB FILE gcResponseFile TO glcResponseData.

oplcResponseData = glcResponseData.

/* Read Response  */
RUN pReadResponse (
    INPUT  glcResponseData,
    INPUT  gcResponseDataType,
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ).

/* delete temporary files */
OS-DELETE VALUE(gcRequestFile).
OS-DELETE VALUE(gcResponseFile).

PROCEDURE pReadResponse PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Reads outbound response data based on data type
    Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE INPUT  PARAMETER iplcResponseData    AS LONGCHAR  NO-UNDO.
    DEFINE INPUT  PARAMETER ipcReponseDataType  AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

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
                opcMessage  = "Could not get any response"
                .
            
            IF iplcResponseData EQ "" THEN
                RETURN.

            RUN VALUE(gcResponseHandler) (
                INPUT  iplcResponseData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).  

        END.
        WHEN "XML" THEN DO:
            /* XML processing goes here */
            RUN VALUE(gcResponseHandler) (
                INPUT  iplcResponseData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).  
        END.
        OTHERWISE DO:
            ASSIGN 
                opcMessage = "Invalid Response Data Type [ " 
                           + ipcReponseDataType 
                           + " ] found in config in APIOutbound table for APIID [ " 
                           + gcAPIID  + " ]".
                oplSuccess = NO
		  .
        END.
    END.

    IF VALID-HANDLE(hdttJSON) THEN
        DELETE OBJECT hdttJSON.

END PROCEDURE.
