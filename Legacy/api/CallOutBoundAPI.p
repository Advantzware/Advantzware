/*------------------------------------------------------------------------
    File        : api/CallOutboundAPI.p
    Purpose     : Triggers the outbound APIs

    Syntax      :

    Description : Triggers the outbound APIs

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER ipcAPIID   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.

DEFINE VARIABLE gcEndPoint        AS CHARACTER NO-UNDO.
DEFINE VARIABLE glIsSSLEnabled    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcAuthType        AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcUserName        AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcPassword        AS CHARACTER NO-UNDO.
DEFINE VARIABLE glcRequestData    AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE glcReponseData    AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE gcRequestDataType AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcRequestVerb     AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcClientID        AS CHARACTER NO-UNDO.

DEFINE VARIABLE gcCommandResult   AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCommand         AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcRequestFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcResponseFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcDateTime        AS CHARACTER NO-UNDO.
DEFINE VARIABLE gdDateTime        AS DATETIME  NO-UNDO.
DEFINE VARIABLE gcSuccess         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glAPIConfigFound  AS LOGICAL   NO-UNDO.

FOR FIRST APIOutbound
    WHERE APIOutbound.APIID = ipcAPIID NO-LOCK:
    ASSIGN
        gcUserName        = APIOutbound.username
        gcPassword        = APIOutbound.password
        gcRequestDataType = APIOutbound.requestDataType
        glIsSSLEnabled    = APIOutbound.isSSLEnabled
        gcRequestVerb     = APIOutbound.requestVerb
        gcClientID        = APIOutbound.clientID
        gcAuthType        = APIOutbound.authType
        glcRequestData    = APIOutbound.requestData
        gcEndPoint        = APIOutbound.endPoint
        ipcAPIID          = APIOutbound.APIID.
        glAPIConfigFound  = YES
        .
END.

IF NOT glAPIConfigFound THEN DO:
    MESSAGE ipcAPIID " config  not available" VIEW-AS ALERT-BOX.
    RETURN.
END.

ASSIGN
    gdDateTime     = NOW
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
    .

IF gcAuthType = "basic" THEN
    gcCommand = 'curl --user '
              + gcUserName + ':' + gcPassword + ' '
              + (IF glIsSSLEnabled THEN '' ELSE '--insecure') + ' '
              + '-H "Content-Type: application/' +  lc(gcRequestDataType + '"') /* handles XML or JSON only - not RAW */
              + (IF gcRequestVerb NE 'get' THEN ' -d "@' + gcRequestFile + '" ' ELSE '')
              + (IF gcRequestVerb NE 'get' THEN ' -X ' + gcRequestVerb ELSE '')  + ' '
              + gcEndPoint
              + ' > ' + gcResponseFile.

IF gcCommand NE '' THEN DO:
    RUN ipPrepareRequest (
        ipcAPIID, INPUT-OUTPUT glcRequestData
        ).

    COPY-LOB glcRequestData TO FILE gcRequestFile.
    OS-COMMAND SILENT VALUE(gcCommand).
    COPY-LOB FILE gcResponseFile TO glcReponseData.

    /* read JSON Response  */
    RUN ReadJSONResponse (
        INPUT  glcReponseData,
        OUTPUT opcMessage,
        OUTPUT oplSuccess
        ).

    /* add a record in APIOutboundEvent table here*/
    RUN ipCreateAPIOutboundEvent (
        INPUT ipcAPIID,
        INPUT glcRequestData,
        INPUT glcReponseData,
        INPUT PROGRAM-NAME(2),
        INPUT oplSuccess,
        INPUT gdDateTime
        ).

    OS-DELETE VALUE(gcRequestFile).
    OS-DELETE VALUE(gcResponseFile).

END.

PROCEDURE ReadJSONResponse:
    DEFINE INPUT  PARAMETER iplcJSON   AS LONGCHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER oplMessage AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL  NO-UNDO.

    DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReadMode   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRetValue   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE httJSON     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE httBuffer   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE httQuery    AS HANDLE    NO-UNDO.

    CREATE TEMP-TABLE httJSON.
    CREATE QUERY httQuery.

    ASSIGN
        cSourceType = "longchar"
        cReadMode   = "empty"
        oplSuccess  = NO
        oplMessage  = "bad JSON"
        .

    lRetValue = httJSON:READ-JSON(cSourceType, iplcJSON, cReadMode) NO-ERROR.

    IF NOT VALID-HANDLE(httJSON) THEN
        RETURN.

    httBuffer = httJSON:DEFAULT-BUFFER-HANDLE.
    httQuery:SET-BUFFERS(httBuffer).
    httQuery:QUERY-PREPARE("FOR EACH NewTable").
    httQuery:QUERY-OPEN.
    httQuery:GET-FIRST.
    IF httBuffer:AVAILABLE THEN
       ASSIGN
           oplSuccess = (httBuffer:BUFFER-FIELD(1):buffer-value = 1)
           oplMessage = httBuffer:BUFFER-FIELD(2):buffer-value
           .

   httQuery:QUERY-CLOSE.

   DELETE OBJECT httJSON.
   DELETE OBJECT httQuery.

END PROCEDURE.


PROCEDURE ipCreateAPIOutboundEvent:
   DEFINE INPUT PARAMETER ipcAPIID        AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iplcRequestData AS LONGCHAR  NO-UNDO.
   DEFINE INPUT PARAMETER iplcReponseData AS LONGCHAR  NO-UNDO.
   DEFINE INPUT PARAMETER ipcProgramName  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iplSuccess      AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER ipcDateTime     AS DATETIME  NO-UNDO.

   CREATE APIOutboundEvent.
   ASSIGN
       APIOutboundEvent.apiID           = ipcAPIID
       APIOutboundEvent.requestData     = iplcRequestData
       APIOutboundEvent.responseData    = iplcReponseData
       APIOutboundEvent.callingProgram  = ipcProgramName
       APIOutboundEvent.success         = iplSuccess
       APIOutboundEvent.requestDateTime = ipcDateTime
       .

END PROCEDURE.


PROCEDURE ipPrepareRequest:
   DEFINE INPUT         PARAMETER ipcAPIID         AS CHARACTER NO-UNDO.
   DEFINE INPUT-OUTPUT  PARAMETER oplcRequestData AS LONGCHAR  NO-UNDO.

   CASE ipcAPIID:
       WHEN "AddCustomer" THEN
           RUN api/AddCustomer.p (
               INPUT-OUTPUT oplcRequestData
               ).
        WHEN "AddProduct" THEN
            RUN api/AddProduct.p (
                INPUT-OUTPUT oplcRequestData
                ).
        WHEN "AddVendor" THEN
            RUN api/AddVendor.p (
                INPUT-OUTPUT oplcRequestData
                ).
        WHEN "AddPurchaseOrder" THEN
            RUN api/AddPurchaseOrder.p (
                INPUT ipcAPIID,
                INPUT-OUTPUT oplcRequestData
                ).
        WHEN "AddPicklist" THEN
            RUN api/AddPicklist.p (
                INPUT ipcAPIID,
                INPUT-OUTPUT oplcRequestData
                ).
   END CASE.
END PROCEDURE.
