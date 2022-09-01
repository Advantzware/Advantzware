/*------------------------------------------------------------------------
    File        : api/CallOutboundAPI.p
    Purpose     : Triggers the outbound APIs

    Syntax      :

    Description : Triggers the outbound APIs

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
USING System.SharedConfig.

DEFINE INPUT  PARAMETER ipiAPIOutboundID AS INTEGER   NO-UNDO.
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

DEFINE VARIABLE lSuccess             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAPIOutboundTestMode AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cRequestFile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResponseFile        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAPIRequestMethod    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFTPRequestMethod    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cHeadersData         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUrlEncodedData      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResponseCode        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilePath            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRequestDateTime     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lError               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cBearerToken         AS CHARACTER NO-UNDO.

DEFINE VARIABLE oAPIHandler          AS API.APIHandler NO-UNDO. 
DEFINE VARIABLE scInstance           AS CLASS System.SharedConfig NO-UNDO. 
DEFINE VARIABLE hdFTPProcs           AS HANDLE    NO-UNDO.
DEFINE VARIABLE mmptrRequestData     AS MEMPTR    NO-UNDO.      

DEFINE BUFFER bf-APIOutboundContent FOR APIOutboundContent.

RUN spGetSessionParam(
    INPUT  "Company",
    OUTPUT cCompany
    ).
    
ASSIGN 
    scInstance           = SharedConfig:instance
    lAPIOutboundTestMode = LOGICAL(scInstance:GetValue("APIOutboundTestMode"))
    cRequestDateTime     = scInstance:GetValue("APIVariable_RequestDateTime")
    NO-ERROR.

IF cRequestDateTime = "" THEN
    cRequestDateTime = STRING(NOW).
        
ASSIGN
    gcParentProgram = ipcParentProgram
    glcRequestData  = iplcRequestData
    gdDateTime      = NOW
    .

FOR FIRST APIOutbound NO-LOCK
    WHERE APIOutbound.apiOutboundID EQ ipiAPIOutboundID 
      AND (lAPIOutboundTestMode OR 
           APIOutbound.Inactive EQ FALSE):
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

/* EndPoint Variable replacement using SharedConfig values */
gcEndPoint = scInstance:ReplaceEndPointVariables(gcEndPoint, gcAPIID).

RUN pReplaceUserContentVariables(INPUT-OUTPUT gcEndPoint, INPUT ipiAPIOutboundID).

IF NOT glAPIConfigFound THEN DO:
    ASSIGN 
        opcMessage = "Config for Outbound API Sequence ID [" 
                   + STRING(ipiAPIOutboundID) 
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
    cFilePath = gcSaveFileFolder.
    
    IF INDEX (gcSaveFileFolder, "$") GT 0 THEN DO:
        RUN Format_UpdateRequestData (INPUT-OUTPUT gcSaveFileFolder, "CompanyID", cCompany, "").
        RUN Format_UpdateRequestData (INPUT-OUTPUT gcSaveFileFolder, "APIID", gcAPIID, "").
        RUN Format_UpdateRequestData (INPUT-OUTPUT gcSaveFileFolder, "ClientID", gcClientID, "").
        RUN Format_UpdateRequestData (INPUT-OUTPUT gcSaveFileFolder, "PrimaryID", ipcPrimaryID, "").   
        RUN Format_UpdateRequestData (INPUT-OUTPUT gcSaveFileFolder, "RequestDataType", gcRequestDataType, ""). 
        RUN Format_UpdateRequestData (INPUT-OUTPUT gcSaveFileFolder, "RequestVerb", gcRequestVerb, "").
        RUN Format_UpdateRequestData (INPUT-OUTPUT gcSaveFileFolder, "CurrentDate", TODAY, "").
        RUN Format_UpdateRequestData (INPUT-OUTPUT gcSaveFileFolder, "CurrentTime", TIME, "").
        RUN Format_UpdateRequestData (INPUT-OUTPUT gcSaveFileFolder, "CurrentDateTime", NOW, "").
        RUN Format_UpdateRequestData (INPUT-OUTPUT gcSaveFileFolder, "RequestDateTime", cRequestDateTime, "").

        RUN pReplaceUserContentVariables(INPUT-OUTPUT gcSaveFileFolder, INPUT ipiAPIOutboundID).
        
        /* API Variable replacement using SharedConfig values */
        gcSaveFileFolder = scInstance:ReplaceAPIVariables(gcSaveFileFolder, gcAPIID).

        ASSIGN 
            cFileName = ENTRY(NUM-ENTRIES(gcSaveFileFolder, "\"), gcSaveFileFolder, "\") 
            cFilePath = SUBSTRING(gcSaveFileFolder, 1, R-INDEX(gcSaveFileFolder, "\") - 1)
            NO-ERROR.
    END.
    
    RUN FileSys_CreateDirectory (
        INPUT  cFilePath,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
    IF NOT oplSuccess THEN
        RETURN.

    RUN FileSys_GetFilePath (
        INPUT  cFilePath,
        OUTPUT cFilePath,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).    
    IF NOT oplSuccess THEN
        RETURN.

    ASSIGN
        gcRequestFile = RIGHT-TRIM(cFilePath, "/")
        gcRequestFile = RIGHT-TRIM(cFilePath, "\")
        .

    ASSIGN
        cResponseFile  = gcAPIID       + "_"      /* API ID    */
                       + gcClientID    + "_"      /* Client ID */
                       + ipcPrimaryID  + "_"      /* i.e. GET, POST, PUT? */
                       + gcDateTime               /* Date and Time */
                       + "." + "log"
        cRequestFile   = IF cFileName NE "" THEN
                             cFileName
                         ELSE 
                             (gcAPIID       + "_"      /* API ID    */
                            + gcClientID    + "_"      /* Client ID */
                            + ipcPrimaryID  + "_"      /* i.e. GET, POST, PUT? */
                            + gcDateTime               /* Date and Time */
                            + "." + lc(gcRequestDataType)). /* File Extentions */
        .

    RUN FileSys_FileNameCleanup (
        INPUT-OUTPUT cResponseFile
        ).

    RUN FileSys_FileNameCleanup (
        INPUT-OUTPUT cRequestFile
        ).

    ASSIGN
        gcResponseFile = gcRequestFile + "\"      /* Save Folder */
                       + cResponseFile
        gcRequestFile  = gcRequestFile + "\"      /* Save Folder */
                       + cRequestFile
        .
        
    COPY-LOB iplcRequestData TO FILE gcRequestFile.
END.

/* Return as file would have been already saved */
IF gcRequestType EQ "SAVE" THEN DO:
    oplcResponseData = "File saved to " + gcRequestFile.
    
    RETURN.
END.

RUN spGetSettingByName ("FTPRequestMethod", OUTPUT cFTPRequestMethod).
IF cFTPRequestMethod EQ ? OR cFTPRequestMethod EQ "" THEN
    cFTPRequestMethod = "cURL".
    
/* Request for FTP transfer of the request data */
IF gcRequestType EQ "FTP" OR gcRequestType EQ "SFTP" THEN DO:
    RUN system/ftpProcs.p PERSISTENT SET hdFTPProcs.
    
    IF cFTPRequestMethod EQ "Internal" THEN DO:
        IF gcRequestType EQ "FTP" THEN
            RUN FTP_UploadFileUsingFTP in hdFTPProcs (
                INPUT  gcEndPoint, 
                INPUT  gcRequestFile, 
                INPUT  gcUserName, 
                INPUT  gcPassword, 
                OUTPUT lError, 
                OUTPUT opcMessage
                ).  
        ELSE IF gcRequestType EQ "SFTP" THEN
            RUN FTP_UploadFileUsingSFTP in hdFTPProcs (
                INPUT  gcEndPoint, 
                INPUT  gcRequestFile, 
                INPUT  gcUserName, 
                INPUT  gcPassword,
                INPUT  glIsSSLEnabled,
                INPUT  gcHostSSHKey, 
                OUTPUT lError, 
                OUTPUT opcMessage
                ).  

        oplSuccess = NOT lError.
        
        oplcResponseData = opcMessage.
    END.
    ELSE DO:
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
        
        oplcResponseData = "FTP transfer status is saved to file " + gcResponseFile.
    END.
    
    DELETE PROCEDURE hdFTPProcs.
    
    RETURN.
END.

RUN sys/ref/nk1look.p (
    INPUT cCompany, 
    INPUT "APIRequestMethod", 
    INPUT "C", 
    INPUT NO, 
    INPUT NO, 
    INPUT "", 
    INPUT "",
    OUTPUT cAPIRequestMethod, 
    OUTPUT lFound
    ).

IF cAPIRequestMethod EQ "" THEN
    cAPIRequestMethod = "cURL".
    
ASSIGN
    FIX-CODEPAGE(oplcResponseData) = 'utf-8'
    FIX-CODEPAGE(glcResponseData)  = 'utf-8'.
    .

IF gcAuthType EQ "Bearer" THEN DO:
    cBearerToken = scInstance:GetValue (gcAPIID + "_" + gcClientID + "_BearerToken").
    IF cBearerToken NE "" THEN
        gcPassword = cBearerToken.    
END.
        
IF cAPIRequestMethod EQ "cURL" THEN DO:
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

    FOR EACH bf-APIOutboundContent NO-LOCK
        WHERE bf-APIOutboundContent.apiOutboundID EQ ipiAPIOutboundID:
        IF bf-APIOutboundContent.contentType EQ "Headers" THEN
            cHeadersData = cHeadersData
                         + ' -H "' + bf-APIOutboundContent.contentKey + ':'
                         + bf-APIOutboundContent.contentValue + '" '.
        ELSE IF bf-APIOutboundContent.contentType EQ "x-www-form-urlencoded" THEN
            cUrlEncodedData = cUrlEncodedData
                            + ' --data-urlencode "' + bf-APIOutboundContent.contentKey + '='
                            + bf-APIOutboundContent.contentValue + '" '.
        
    END.

    gcCommand = SEARCH("curl.exe") 
              + (IF gcAuthType = "basic" THEN ' --user ' + gcUserName + ':' + gcPassword 
                 ELSE IF gcAuthType = "bearer" THEN ' -H "Authorization: Bearer ' + gcPassword + '"' 
                 ELSE "") + ' ' 
              + (IF NOT glIsSSLEnabled THEN '--insecure' ELSE '') + ' '
              + '-H "Content-Type: application/' +  lc(gcRequestDataType + '"') /* handles XML or JSON only - not RAW */
              + cHeadersData
              + cUrlEncodedData
              + (IF gcRequestVerb NE 'get' THEN ' -d "@' + gcRequestFile + '" ' ELSE '')
              + (IF gcRequestVerb NE 'get' THEN ' -X ' + gcRequestVerb ELSE '')  + ' '
              + '"' + gcEndPoint + '"'.
    
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
            
        RETURN.
    END.
        
    /* Put Response Data from Temporary file into a variable */
    COPY-LOB FILE gcResponseFile TO glcResponseData NO-ERROR.
    
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Error reading the response"
            .
            
        RETURN.
    END.
        
    oplcResponseData = glcResponseData.
END.
ELSE IF cAPIRequestMethod EQ "Internal" THEN DO:
    oAPIHandler = NEW API.APIHandler().
    
    oAPIHandler:ContentType = gcRequestDataType.

    FOR EACH bf-APIOutboundContent NO-LOCK
        WHERE bf-APIOutboundContent.apiOutboundID EQ ipiAPIOutboundID:
        IF bf-APIOutboundContent.contentType EQ "Headers" THEN
            oAPIHandler:AddHeaderData(bf-APIOutboundContent.contentKey, bf-APIOutboundContent.contentValue).
        ELSE IF bf-APIOutboundContent.contentType EQ "x-www-form-urlencoded" THEN
            oAPIHandler:AddURLEncodeData(bf-APIOutboundContent.contentKey, bf-APIOutboundContent.contentValue).        
    END.
        
    IF gcAuthType EQ "Basic" THEN
        oAPIHandler:SetBasicAuthentication(gcUserName, gcPassword).
    ELSE IF gcAuthType EQ "Bearer" THEN
        oAPIHandler:SetBearerAuthentication(gcPassword).

    IF gcRequestVerb EQ "POST" THEN        
        oplcResponseData = oAPIHandler:Post(gcEndPoint, iplcRequestData).
    ELSE IF gcRequestVerb EQ "GET" THEN 
        oplcResponseData = oAPIHandler:Get(gcEndPoint).
    ELSE IF gcRequestVerb EQ "DELETE" THEN 
        oplcResponseData = oAPIHandler:Delete(gcEndPoint).
    ELSE IF gcRequestVerb EQ "PATCH" THEN
        oplcResponseData = oAPIHandler:Patch(gcEndPoint, iplcRequestData).
        
    cResponseCode = oAPIHandler:GetResponseStatusCode().
                            
    IF VALID-OBJECT(oAPIHandler) THEN
        DELETE OBJECT oAPIHandler.    
END.

/* Read Response  */
RUN pReadResponse (
    INPUT  oplcResponseData,
    INPUT  cResponseCode,
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ).

FINALLY:
    /* delete temporary files */
    IF gcRequestType EQ "API" AND NOT glSaveFile THEN
        OS-DELETE VALUE(gcRequestFile).
    
    IF gcRequestType NE "FTP" AND gcRequestType NE "SFTP" THEN
        OS-DELETE VALUE(gcResponseFile).
END FINALLY.



/* **********************  Internal Procedures  *********************** */


PROCEDURE pReadResponse PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Reads outbound response data based on data type
    Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE INPUT  PARAMETER iplcResponseData AS LONGCHAR  NO-UNDO.
    DEFINE INPUT  PARAMETER ipcResponseCode  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cResponseHandler AS CHARACTER NO-UNDO.
                
    IF iplcResponseData EQ "" THEN DO:
        ASSIGN
            oplSuccess  = NO
            opcMessage  = IF ipcResponseCode NE "" THEN
                              ipcResponseCode
                          ELSE
                              "Could not get any response"
            .

        RETURN.
    END.

    IF gcResponseHandler EQ "" THEN DO:
        ASSIGN
            oplSuccess  = NO
            opcMessage  = "No response handler available in the API configuration"
            .

        RETURN.    
    END.

    ASSIGN
        cResponseHandler = gcResponseHandler
        cResponseHandler = ENTRY(1, cResponseHandler, ".")
        .
       
    IF SEARCH(cResponseHandler + ".r") EQ ? AND SEARCH(cResponseHandler + ".p") EQ ? THEN DO:
        ASSIGN
            oplSuccess  = NO
            opcMessage  = "Missing or invalid response handler in the API configuration"
            .

        RETURN.    
    END.
     
    RUN VALUE(gcResponseHandler) (
        INPUT  iplcResponseData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).
END PROCEDURE.

PROCEDURE pReplaceUserContentVariables:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopcRequestData  AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundID AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-APIOutboundContent FOR APIOutboundContent.
    
    FOR EACH bf-APIOutboundContent NO-LOCK
        WHERE bf-APIOutboundContent.apiOutboundID EQ ipiAPIOutboundID
          AND bf-APIOutboundContent.contentType   EQ "User":
        RUN Format_UpdateRequestData (INPUT-OUTPUT iopcRequestData, bf-APIOutboundContent.contentKey, bf-APIOutboundContent.contentValue, "").        
    END.    
END PROCEDURE.
