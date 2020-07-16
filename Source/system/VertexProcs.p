
/*------------------------------------------------------------------------
    File        : VertexProcs.p
    Purpose     : 

    Syntax      :

    Description : Procedures to handle vertex cloud integration

    Author(s)   : Mithun Porandla
    Created     : Tue Jun 23 02:36:16 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
USING Progress.Json.ObjectModel.*.

DEFINE VARIABLE oModelParser    AS ObjectModelParser NO-UNDO.
DEFINE VARIABLE oObject         AS JsonObject        NO-UNDO.
DEFINE VARIABLE hdOutboundProcs AS HANDLE            NO-UNDO.
DEFINE VARIABLE cTempDir        AS CHARACTER         NO-UNDO.

{api/ttAPIOutboundEvent.i}

RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

RUN FileSys_GetTempDirectory (
    OUTPUT cTempDir
    ).
    
oModelParser = NEW ObjectModelParser().


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE pCallOutboundAPI PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Procedure to call the Outbound API
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAPIID            AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcScopeID          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScopeType        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTableList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcROWIDList        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPrimaryID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEventDescription AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER TABLE FOR ttAPIOutboundEvent.
        
    RUN pUpdateAccessToken (
        INPUT  ipcCompany,
        OUTPUT oplSuccess,
        OUTPUT opcMessage    
        ).
    IF NOT oplSuccess THEN
        RETURN.
        
    RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs (
        INPUT  ipcCompany,                      /* Company Code (Mandatory) */
        INPUT  ipcLocation,                     /* Location Code (Mandatory) */
        INPUT  ipcAPIID,                        /* API ID (Mandatory) */
        INPUT  ipcScopeID,                      /* Scope ID */
        INPUT  ipcScopeType,                    /* Scope Type */
        INPUT  ipcTriggerID,                    /* Trigger ID (Mandatory) */
        INPUT  ipcTableList,                    /* Comma separated list of table names for which data being sent (Mandatory) */
        INPUT  ipcROWIDList,                    /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
        INPUT  ipcPrimaryID,                    /* Primary ID for which API is called for (Mandatory) */   
        INPUT  ipcEventDescription,             /* Event's description (Optional) */
        OUTPUT oplSuccess,                      /* Success/Failure flag */
        OUTPUT opcMessage                       /* Status message */
        ) NO-ERROR.
    
    IF oplSuccess THEN DO:
        RUN Outbound_GetEvents IN hdOutboundProcs (
            OUTPUT TABLE ttAPIOutboundEvent
            ).
    END.
    
    RUN Outbound_ResetContext IN hdOutboundProcs.
END PROCEDURE.

PROCEDURE pUpdateAccessToken PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Updates the access token in sys-ctrl and Outbound API 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cClientID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cClientSecret   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAPIKey         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAPIPassword    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCommand        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cGrantType      AS CHARACTER NO-UNDO INITIAL "password". /* Grant Type */
    DEFINE VARIABLE cAccessTokenURL AS CHARACTER NO-UNDO INITIAL "https://auth.vertexsmb.com/identity/connect/token". /* API URL */
    DEFINE VARIABLE cScope          AS CHARACTER NO-UNDO INITIAL "calc-rest-api".
    DEFINE VARIABLE cResponseFile   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcResponse      AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cAccessToken    AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-sys-ctrl FOR sys-ctrl.
    DEFINE BUFFER bf-APIOutbound FOR APIOutbound.
    
    FIND FIRST bf-sys-ctrl NO-LOCK
         WHERE bf-sys-ctrl.company EQ ipcCompany
           AND bf-sys-ctrl.name    EQ "VertexAccessToken"
         NO-ERROR.
    IF AVAILABLE bf-sys-ctrl THEN DO:
        /* 1200 seconds or 20 minutes is the access token expiry time. 
           Added a buffer of additionl 20 seconds to refresh access token */
        IF bf-sys-ctrl.date-fld EQ TODAY AND TIME - bf-sys-ctrl.int-fld LT 1180 THEN DO:
            ASSIGN
                oplSuccess     = TRUE
                opcMessage     = "Success"
                .
            RETURN.
        END.
    END.
    
    RUN pGetClientID (
        INPUT  ipcCompany,
        OUTPUT cClientID
        ).

    RUN pGetClientSecret (
        INPUT  ipcCompany,
        OUTPUT cClientSecret
        ).

    RUN pGetAPIKey (
        INPUT  ipcCompany,
        OUTPUT cAPIKey
        ).

    RUN pGetAPIPassword (
        INPUT  ipcCompany,
        OUTPUT cAPIPassword
        ).

    FIND FIRST bf-APIOutbound NO-LOCK
         WHERE bf-APIOutbound.apiID EQ "CalculateTax"
           AND NOT bf-APIOutbound.clientID BEGINS "_default"
         NO-ERROR. 
    IF NOT AVAILABLE bf-APIOutbound THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "No Outbound API is configured to save the access token"
            .
        RETURN.        
    END.
    
    ASSIGN
        cResponseFile            = cTempDir + "\vertex_access_token" + STRING(MTIME) + ".txt"
        FIX-CODEPAGE(lcResponse) = 'utf-8'
        cCommand                 = SEARCH("curl.exe") 
                                 + (IF bf-APIOutbound.isSSLEnabled THEN
                                        ""
                                    ELSE
                                        " --insecure ")
                                 + ' -X POST "' 
                                 + cAccessTokenURL + '" '
                                 + '-H "Content-Type:application/x-www-form-urlencoded" '
                                 + '--data-urlencode "client_id=' + cClientID + '" '
                                 + '--data-urlencode "client_secret=' + cClientSecret + '" '
                                 + '--data-urlencode "username=' + cAPIKey + '" '
                                 + '--data-urlencode "password=' + cAPIPassword + '" '
                                 + '--data-urlencode "grant_type=' + cGrantType + '" '
                                 + '--data-urlencode "scope=' + cScope + '"'
        .

    /* execute CURL command with required parameters to call the API */
    RUN OS_RunCommand (
        INPUT  cCommand,             /* Command string to run */
        INPUT  cResponseFile,        /* File name to write the command output */
        INPUT  TRUE,                 /* Run with SILENT option */
        INPUT  FALSE,                /* Run with NO-WAIT option */
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
    IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Error excuting curl command"
            . 
            
       OS-DELETE VALUE(cResponseFile).             
       RETURN.
    END.
    
    COPY-LOB FILE cResponseFile TO lcResponse.

    IF lcResponse EQ "" THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Empty response"
            .
        RETURN.
    END.
    
    ASSIGN
        oObject      = CAST(oModelParser:Parse(INPUT lcResponse),JsonObject).
        cAccessToken = oObject:GetJsonText('access_token')
        NO-ERROR.

    IF cAccessToken EQ "" THEN DO:
        ASSIGN
            opcMessage = "Error while generating access token"
            oplSuccess = FALSE
            .
        RETURN.
    END.

    FIND FIRST bf-sys-ctrl EXCLUSIVE-LOCK
         WHERE bf-sys-ctrl.company EQ ipcCompany
           AND bf-sys-ctrl.name    EQ "VertexAccessToken"
         NO-ERROR.
    IF AVAILABLE bf-sys-ctrl THEN
        ASSIGN
            bf-sys-ctrl.char-fld = cAccessToken
            bf-sys-ctrl.date-fld = TODAY
            bf-sys-ctrl.int-fld  = TIME
            opcMessage           = "Success"
            oplSuccess           = TRUE
            .    
        
    FIND CURRENT bf-APIOutbound EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE bf-APIOutbound THEN
        bf-APIOutbound.password = cAccessToken.
    
    OS-DELETE VALUE(cResponseFile).            
END PROCEDURE.

PROCEDURE pGetClientID PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Returns the client ID from sys-ctrl 
 Notes: 
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcClientID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.

    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "VertexClientID", 
        INPUT "C", 
        INPUT NO, 
        INPUT NO, 
        INPUT "", 
        INPUT "",
        OUTPUT opcClientID, 
        OUTPUT lFound
        ).
END PROCEDURE.

PROCEDURE pGetClientSecret PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Returns the client secret from sys-ctrl
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcClientSecret AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.

    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "VertexClientSecret", 
        INPUT "C", 
        INPUT NO, 
        INPUT NO, 
        INPUT "", 
        INPUT "",
        OUTPUT opcClientSecret, 
        OUTPUT lFound
        ).
END PROCEDURE.

PROCEDURE pGetAPIKey PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Returns the api key from sys-ctrl
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAPIKey  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.

    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "VertexAPIKey", 
        INPUT "C", 
        INPUT NO, 
        INPUT NO, 
        INPUT "", 
        INPUT "",
        OUTPUT opcAPIKey, 
        OUTPUT lFound
        ).
END PROCEDURE.

PROCEDURE pGetAPIPassword PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Returns the api password from sys-ctrl
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAPIPassword AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.

    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "VertexAPIPassword", 
        INPUT "C", 
        INPUT NO, 
        INPUT NO, 
        INPUT "", 
        INPUT "",
        OUTPUT opcAPIPassword, 
        OUTPUT lFound
        ).
END PROCEDURE.

PROCEDURE pGetTaxAmounts PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Reads the response and returns the tax amounts 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplcResponseData   AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInvoiceTotal    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInvoiceSubTotal AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTaxTotal        AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage         AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE jaLineItems      AS JsonArray  NO-UNDO.
    DEFINE VARIABLE jaErrors         AS JsonArray  NO-UNDO.
    DEFINE VARIABLE joErrors         AS JsonObject NO-UNDO.
    DEFINE VARIABLE iLengthProperty  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iCount           AS INTEGER    NO-UNDO.    
    
    ASSIGN
        oObject = CAST(oModelParser:Parse(INPUT iplcResponseData),JsonObject)
        oObject = CAST(oModelParser:PARSE(INPUT oObject:GetJsonText('data')),JsonObject)                
        NO-ERROR.

    ASSIGN
        jaErrors        = oObject:GetJsonArray("errors")
        iLengthProperty = jaErrors:LENGTH        
        NO-ERROR.

    IF NOT ERROR-STATUS:ERROR AND iLengthProperty GT 0 THEN DO:
        DO iCount = 1 TO iLengthProperty:
            ASSIGN
                joErrors   = jaErrors:GetJsonObject(iCount)
                opcMessage = joErrors:GetJsonText("detail") 
                NO-ERROR.            
        END.
        oplSuccess = FALSE.
        RETURN.        
    END. 
    
    ASSIGN
        opdInvoiceTotal    = DECIMAL(oObject:GetJsonText('total'))
        opdInvoiceSubTotal = DECIMAL(oObject:GetJsonText('subTotal'))
        opdTaxTotal        = DECIMAL(oObject:GetJsonText('totalTax'))
/*        jaLineItems        = oObject:GetJsonArray("lineItems")*/
/*        iLengthProperty    = jaLineItems:LENGTH               */
        NO-ERROR.
        
    ASSIGN
        oplSuccess = TRUE 
        opcMessage = "Success"
        .    

END PROCEDURE.

PROCEDURE Vertex_CalculateTaxForInvHead:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriInvHead        AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMessageType     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplPostToJournal   AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID       AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER opdInvoiceTotal    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInvoiceSubTotal AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTaxTotal        AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage         AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lcResponseData   AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cInputListValues AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInputListKeys   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-inv-head FOR inv-head.
    DEFINE BUFFER bf-APIOutboundEvent FOR APIOutboundEvent.

    FIX-CODEPAGE(lcResponseData) = 'utf-8'.
    
    FIND FIRST bf-inv-head NO-LOCK
         WHERE ROWID(bf-inv-head) EQ ipriInvHead
         NO-ERROR.
    IF NOT AVAILABLE bf-inv-head THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid invoice header row id "
            .
        RETURN.
    END.
    
    ASSIGN
        cInputListKeys   = "inv-head" + "," + "MessageType" + "," + "PostToJournal"
        cInputListValues = STRING(ROWID(bf-inv-head)) + "," + "INVOICE" + "," + STRING(iplPostToJournal,"true/false")
        .

    RUN pCallOutboundAPI (
        INPUT  bf-inv-head.company,           /* Company Code (Mandatory) */
        INPUT  ipcLocation,                   /* Location Code (Mandatory) */
        INPUT  "Calculatetax",                /* API ID (Mandatory) */
        INPUT  bf-inv-head.cust-no,           /* Scope ID */
        INPUT  "Customer",                    /* Scope Type */
        INPUT  ipcTriggerID,                  /* Trigger ID (Mandatory) */
        INPUT  cInputListKeys,                /* Comma separated list of table names for which data being sent (Mandatory) */
        INPUT  cInputListValues,              /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
        INPUT  bf-inv-head.inv-no,            /* Primary ID for which API is called for (Mandatory) */   
        INPUT  "Called from Vertex Procs",    /* Event's description (Optional) */
        OUTPUT oplSuccess,
        OUTPUT opcMessage,
        OUTPUT TABLE ttAPIOutboundEvent
        ) NO-ERROR.    
    
    IF NOT oplSuccess THEN
        RETURN.

    IF NOT TEMP-TABLE ttAPIOutboundEvent:HAS-RECORDS THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Failure in calling the api"
            .
        RETURN.
    END.

    FOR FIRST ttAPIOutboundEvent:
        FIND FIRST bf-APIOutboundEvent NO-LOCK
             WHERE bf-APIOutboundEvent.apiOutboundEventID EQ ttAPIOutboundEvent.apiOutboundEventID
             NO-ERROR.
        IF AVAILABLE bf-APIOutboundEvent THEN DO:
            lcResponseData = bf-APIOutboundEvent.responseData.

            RUN pGetTaxAmounts (
                INPUT  lcResponseData,
                OUTPUT opdInvoiceTotal,
                OUTPUT opdInvoiceSubTotal,
                OUTPUT opdTaxTotal,
                OUTPUT oplSuccess,
                OUTPUT opcMessage               
                ).
        END.
    END.
END PROCEDURE.

PROCEDURE Vertex_CalculateTaxForArInv:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriArInv          AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMessageType     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplPostToJournal   AS LOGICAL   NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcTriggerID       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInvoiceTotal    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInvoiceSubTotal AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTaxTotal        AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage         AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lcResponseData   AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cInputListValues AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInputListKeys   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ar-inv FOR ar-inv.
    DEFINE BUFFER bf-APIOutboundEvent FOR APIOutboundEvent.

    FIX-CODEPAGE(lcResponseData) = 'utf-8'.
    
    FIND FIRST bf-ar-inv NO-LOCK
         WHERE ROWID(bf-ar-inv) EQ ipriArInv
         NO-ERROR.
    IF NOT AVAILABLE bf-ar-inv THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid ar-inv row id "
            .
        RETURN.
    END.

    ASSIGN
        cInputListKeys   = "ar-inv" + "," + "MessageType" + "," + "PostToJournal"
        cInputListValues = STRING(ROWID(bf-ar-inv)) + "," + "INVOICE" + "," + STRING(iplPostToJournal,"true/false")
        .
    
    RUN pCallOutboundAPI (
        INPUT  bf-ar-inv.company,             /* Company Code (Mandatory) */
        INPUT  ipcLocation,                   /* Location Code (Mandatory) */
        INPUT  "Calculatetax",                /* API ID (Mandatory) */
        INPUT  bf-ar-inv.cust-no,             /* Scope ID */
        INPUT  "Customer",                    /* Scope Type */
        INPUT  ipcTriggerID,                  /* Trigger ID (Mandatory) */
        INPUT  cInputListKeys,                /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
        INPUT  cInputListValues,              /* Primary ID for which API is called for (Mandatory) */   
        INPUT  bf-ar-inv.inv-no,              /* Primary ID for which API is called for (Mandatory) */   
        INPUT  "Called from Vertex Procs",    /* Event's description (Optional) */
        OUTPUT oplSuccess,
        OUTPUT opcMessage,
        OUTPUT TABLE ttAPIOutboundEvent
        ) NO-ERROR.    
    
    IF NOT oplSuccess THEN
        RETURN.

    IF NOT TEMP-TABLE ttAPIOutboundEvent:HAS-RECORDS THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Failure in calling the api"
            .
        RETURN.
    END.

    FOR FIRST ttAPIOutboundEvent:
        FIND FIRST bf-APIOutboundEvent NO-LOCK
             WHERE bf-APIOutboundEvent.apiOutboundEventID EQ ttAPIOutboundEvent.apiOutboundEventID
             NO-ERROR.
        IF AVAILABLE bf-APIOutboundEvent THEN DO:
            lcResponseData = bf-APIOutboundEvent.responseData.

            RUN pGetTaxAmounts (
                INPUT  lcResponseData,
                OUTPUT opdInvoiceTotal,
                OUTPUT opdInvoiceSubTotal,
                OUTPUT opdTaxTotal,
                OUTPUT oplSuccess,
                OUTPUT opcMessage               
                ).
        END.
    END.
END PROCEDURE.
