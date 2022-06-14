
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
DEFINE VARIABLE cTempDir        AS CHARACTER         NO-UNDO.

{system/TaxProcs.i}
{api/ttAPIOutboundEvent.i}

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

    DEFINE VARIABLE hdOutboundProcs AS HANDLE NO-UNDO.
        
    RUN pUpdateAccessToken (
        INPUT  ipcCompany,
        OUTPUT oplSuccess,
        OUTPUT opcMessage    
        ).
    IF NOT oplSuccess THEN
        RETURN.

    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.
    
    EMPTY TEMP-TABLE ttAPIOutboundEvent.
        
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
            OUTPUT TABLE ttAPIOutboundEvent BY-REFERENCE
            ).
    END.
    
    RUN Outbound_ResetContext IN hdOutboundProcs.
    
    DELETE PROCEDURE hdOutboundProcs.
END PROCEDURE.

PROCEDURE pUpdateAccessToken PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Updates the access token in sys-ctrl and Outbound API 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cAccessToken    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdOutboundProcs AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lcResponse      AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cSettingValue   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dttzServerDateTimeTZ              AS DATETIME-TZ NO-UNDO.    
    DEFINE VARIABLE dttzCurrentGMTDateTimeTZ          AS DATETIME-TZ NO-UNDO.
    DEFINE VARIABLE dtVertexAccessTokenDateTimeTZ     AS DATETIME-TZ NO-UNDO.
    DEFINE VARIABLE iVertexAccessTokenRefreshInterval AS INTEGER     NO-UNDO.
    
    DEFINE BUFFER bf-sys-ctrl FOR sys-ctrl.
    DEFINE BUFFER bf-APIOutbound      FOR APIOutbound.
    DEFINE BUFFER bf-APIOutboundEvent FOR APIOutboundEvent.
        
    /* Code to find and extract the GMT Date and time from current date amnd time */
    RUN spCommon_GetServerTime (
        OUTPUT dttzServerDateTimeTZ
        ).
        
    RUN spCommon_GetGMTTime (
        INPUT  dttzServerDateTimeTZ,
        OUTPUT dttzCurrentGMTDateTimeTZ
        ).        

    RUN spGetSettingByName ("VertexAccessTokenDateTime", OUTPUT cSettingValue).
    dtVertexAccessTokenDateTimeTZ = DATETIME-TZ(cSettingValue) NO-ERROR.
    
    RUN spGetSettingByName ("VertexAccessTokenRefreshInterval", OUTPUT cSettingValue).
    iVertexAccessTokenRefreshInterval = INTEGER (cSettingValue) NO-ERROR.
             
    IF INTERVAL(dttzCurrentGMTDateTimeTZ, dtVertexAccessTokenDateTimeTZ, "seconds") LT iVertexAccessTokenRefreshInterval THEN DO:
        ASSIGN
            oplSuccess     = TRUE
            opcMessage     = "Success"
            .
        RETURN.
    END.
            
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

    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.
        
    RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs (
        INPUT  ipcCompany,                      /* Company Code (Mandatory) */
        INPUT  "",                              /* Location Code (Mandatory) */
        INPUT  "CalculateTaxToken",             /* API ID (Mandatory) */
        INPUT  "",                              /* Scope ID */
        INPUT  "",                              /* Scope Type */
        INPUT  "GetRefreshToken",               /* Trigger ID (Mandatory) */
        INPUT  "Tax",                           /* Comma separated list of table names for which data being sent (Mandatory) */
        INPUT  "Tax",                           /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
        INPUT  USERID("ASI"),                   /* Primary ID for which API is called for (Mandatory) */   
        INPUT  "Vertex Refresh Token",          /* Event's description (Optional) */
        OUTPUT oplSuccess,                      /* Success/Failure flag */
        OUTPUT opcMessage                       /* Status message */
        ) NO-ERROR.
    
    IF oplSuccess THEN DO:
        RUN Outbound_GetEvents IN hdOutboundProcs (
            OUTPUT TABLE ttAPIOutboundEvent BY-REFERENCE
            ).
    END.
    
    DELETE PROCEDURE hdOutboundProcs.

    FIND FIRST ttAPIOutboundEvent NO-ERROR.
    IF NOT AVAILABLE ttAPIOutboundEvent THEN DO:
        ASSIGN
            opcMessage = "Error while generating access token"
            oplSuccess = FALSE
            .
        RETURN.    
    END.
    
    FIND FIRST bf-APIOutboundEvent NO-LOCK
         WHERE bf-APIOutboundEvent.apiOutboundEventID EQ ttAPIOutboundEvent.apiOutboundEventID
         NO-ERROR.
    IF NOT AVAILABLE bf-APIOutboundEvent OR (AVAILABLE bf-APIOutboundEvent AND NOT bf-APIOutboundEvent.success) THEN DO:
        ASSIGN
            opcMessage = "Error while generating access token"
            oplSuccess = FALSE
            .
        RETURN.    
    END.
    
    FIX-CODEPAGE(lcResponse) = "utf-8".
    
    lcResponse = bf-APIOutboundEvent.responseData.

    IF lcResponse EQ "" OR lcResponse EQ ? THEN DO:
        ASSIGN
            opcMessage = "Error while generating access token"
            oplSuccess = FALSE
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

    RUN spCommon_GetServerTime (
        OUTPUT dttzServerDateTimeTZ
        ).

    /* Code to find and extract the GMT Date and time from current date amnd time */        
    RUN spCommon_GetGMTTime (
        INPUT  dttzServerDateTimeTZ,
        OUTPUT dttzCurrentGMTDateTimeTZ
        ).        
    
    RUN spSetSettingByName ("VertexAccessToken", cAccessToken).
    
    opcMessage = RETURN-VALUE.
    IF opcMessage NE "" THEN DO:
        ASSIGN
            opcMessage = "Error while updating VertexAccessToken setting. '" + opcMessage + "'"
            oplSuccess = FALSE
            .
    END.
    
    RUN spSetSettingByName ("VertexAccessTokenDateTime", STRING(dttzCurrentGMTDateTimeTZ)).
    
    opcMessage = RETURN-VALUE.
    
    IF opcMessage NE "" THEN DO:
        ASSIGN
            opcMessage = "Error while updating VertexAccessTokenDateTime setting. '" + opcMessage + "'"
            oplSuccess = FALSE
            .
    END.      
        
    FIND CURRENT bf-APIOutbound EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE bf-APIOutbound THEN
        bf-APIOutbound.password = cAccessToken.

END PROCEDURE.

PROCEDURE pGetTaxAmounts PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Reads the response and returns the tax amounts 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTaxCode         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplcResponseData   AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInvoiceTotal    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInvoiceSubTotal AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTaxTotal        AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE              FOR ttTaxDetail.
    DEFINE OUTPUT PARAMETER oplSuccess         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage         AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE jaErrors         AS JsonArray  NO-UNDO.
    DEFINE VARIABLE joErrors         AS JsonObject NO-UNDO.
    DEFINE VARIABLE jaLineItems      AS JsonArray  NO-UNDO.
    DEFINE VARIABLE joLineItem       AS JsonObject NO-UNDO.
    DEFINE VARIABLE jaLineTaxes      AS JsonArray  NO-UNDO.
    DEFINE VARIABLE joLineTax        AS JsonObject NO-UNDO.
    DEFINE VARIABLE joFlexFields     AS JsonObject NO-UNDO.
    DEFINE VARIABLE jaFlexCodeFields AS JsonArray  NO-UNDO.
    DEFINE VARIABLE joFlexCodeField  AS JsonObject NO-UNDO.
    
    DEFINE VARIABLE iLengthProperty    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCount1            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCount2            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCount3            AS INTEGER   NO-UNDO.    
    DEFINE VARIABLE iNumLines          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNumTaxes          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNumFlexCodeFields AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cInvoiceNo         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLineNo            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dLineTax           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCalculatedTax     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dEffectiveRate     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTaxable           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cLineType          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLineRecKey        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE isFreight          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE isFreightTaxable   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cFieldID           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldValue        AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-stax   FOR stax.
    
    EMPTY TEMP-TABLE tttaxDetail.
    
    IF iplcResponseData EQ "" THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Empty response data"
            .    
        RETURN.
    END.
    
    ASSIGN
        oObject = CAST(oModelParser:Parse(INPUT iplcResponseData),JsonObject)
        oObject = CAST(oModelParser:PARSE(INPUT oObject:GetJsonText('data')),JsonObject)                
        NO-ERROR.

    ASSIGN
        jaErrors        = oObject:GetJsonArray("errors")
        iLengthProperty = jaErrors:LENGTH        
        NO-ERROR.

    IF NOT ERROR-STATUS:ERROR AND iLengthProperty GT 0 THEN DO:
        DO iCount1 = 1 TO iLengthProperty:
            ASSIGN
                joErrors   = jaErrors:GetJsonObject(iCount1)
                opcMessage = joErrors:GetJsonText("detail") 
                NO-ERROR.            
        END.
        oplSuccess = FALSE.
        RETURN.        
    END. 
    
    RUN pSetStaxBuffer(
        INPUT  ipcCompany, 
        INPUT  ipcTaxCode, 
        BUFFER bf-stax
        ).
    
    ASSIGN
        opdInvoiceTotal    = DECIMAL(oObject:GetJsonText('total'))
        opdInvoiceSubTotal = DECIMAL(oObject:GetJsonText('subTotal'))
        opdTaxTotal        = DECIMAL(oObject:GetJsonText('totalTax'))
        cInvoiceNo         = oObject:GetJsonText('documentNumber')
        jaLineItems        = oObject:GetJsonArray("lineItems")
        iNumLines          = jaLineItems:LENGTH
        NO-ERROR.

    IF NOT ERROR-STATUS:ERROR AND iNumLines GT 0 THEN DO:
        DO iCount1 = 1 TO iNumLines:            
            ASSIGN
                cLineType          = ""
                cLineRecKey        = ""
                isFreight          = FALSE
                isFreightTaxable   = FALSE
                cLineType          = ""
                cLineRecKey        = ""
                joLineItem         = jaLineItems:GetJsonObject(iCount1)
                iLineNo            = INTEGER(joLineItem:GetJsonText("lineItemNumber")) 
                dLineTax           = DECIMAL(joLineItem:GetJsonText("totalTax")) 
                jaLineTaxes        = joLineItem:GetJsonArray("taxes")
                joFlexFields       = joLineItem:GetJsonObject("flexibleFields")
                jaFlexCodeFields   = joFlexFields:GetJsonArray("flexibleCodeFields")
                iNumFlexCodeFields = jaFlexCodeFields:LENGTH
                iNumTaxes          = jaLineTaxes:LENGTH
                NO-ERROR.

            DO iCount2 = 1 TO iNumFlexCodeFields:
                joFlexCodeField = jaFlexCodeFields:GetjsonObject(iCount2) NO-ERROR.
                
                ASSIGN
                    cFieldID    = joFlexCodeField:GetJsonText("fieldId")
                    cFieldValue = joFlexCodeField:GetJsonText("value")
                    NO-ERROR.

                IF cFieldID EQ "6" THEN
                    isFreight = LOGICAL(joFlexCodeField:GetJsonText("value"),"FREIGHT/").

                IF cFieldID EQ "7" THEN
                    isFreightTaxable = LOGICAL(joFlexCodeField:GetJsonText("value"),"FREIGHTTAXABLE/").
                
                /* We are sending line type (ar-inv(ARINV), inv-line(INVLINE) or inv-misc(INVMISC) to identify the 
                   returned value */
                IF cFieldID EQ "8" THEN
                    cLineType = joFlexCodeField:GetJsonText("value").
                
                /* Reckey of the line record that is calculated */
                IF cFieldID EQ "9" THEN
                    cLineRecKey = joFlexCodeField:GetJsonText("value"). 
            END.
            
            DO iCount3 = 1 TO iNumTaxes:
                ASSIGN
                    joLineTax      = jaLinetaxes:GetJsonObject(iCount3)
                    dCalculatedTax = DECIMAL(joLineTax:GetJsonText("calculatedTax"))
                    dEffectiveRate = DECIMAL(joLineTax:GetJsonText("effectiveRate"))
                    dTaxable       = DECIMAL(joLineTax:GetJsonText("taxable"))
                    NO-ERROR.
                
                CREATE ttTaxDetail.
                ASSIGN
                    ttTaxDetail.company                = ipcCompany
                    ttTaxDetail.invoiceNo              = INTEGER(cInvoiceNo)
                    ttTaxDetail.invoiceLineType        = cLineType
                    ttTaxDetail.invoiceLineRecKey      = cLineRecKey
                    ttTaxDetail.taxLine                = iLineNo
                    ttTaxDetail.taxGroup               = IF AVAIL bf-stax THEN bf-stax.tax-group ELSE ""
                    ttTaxDetail.taxGroupLine           = iCount3
                    ttTaxDetail.isFreight              = isFreight
                    ttTaxDetail.isTaxOnFreight         = isFreightTaxable                    
                    ttTaxDetail.taxGroupTaxAmountLimit = 0
                    ttTaxDetail.taxCode                = IF AVAIL bf-stax THEN bf-stax.tax-code1[1] ELSE ""
                    ttTaxDetail.taxCodeDescription     = IF AVAIL bf-stax THEN bf-stax.tax-dscr1[1] ELSE ""
                    ttTaxDetail.taxCodeRate            = dEffectiveRate
                    ttTaxDetail.taxCodeAccount         = IF AVAIL bf-stax THEN bf-stax.tax-acc1[1] ELSE ""
                    ttTaxDetail.taxCodeTaxAmount       = dCalculatedTax
                    ttTaxDetail.taxCodeTaxableAmount   = dTaxable
                    .
            END.
        END.
    END. 

    ASSIGN
        oplSuccess = TRUE 
        opcMessage = "Success"
        .    

END PROCEDURE.

PROCEDURE pSetStaxBuffer PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Gets the Stax buffer given company and tax group
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTaxGroup AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-stax FOR stax.

    FIND FIRST ipbf-stax NO-LOCK 
        WHERE ipbf-stax.company EQ ipcCompany
        AND ipbf-stax.tax-group EQ ipcTaxGroup
        NO-ERROR.

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
    DEFINE OUTPUT PARAMETER TABLE              FOR ttTaxDetail.
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
        cInputListValues = STRING(ROWID(bf-inv-head)) + "," + ipcMessageType + "," + STRING(iplPostToJournal,"true/false")
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
                INPUT  bf-inv-head.company,
                INPUT  bf-inv-head.tax-gr,
                INPUT  lcResponseData,
                OUTPUT opdInvoiceTotal,
                OUTPUT opdInvoiceSubTotal,
                OUTPUT opdTaxTotal,
                OUTPUT TABLE ttTaxDetail,
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
    DEFINE OUTPUT PARAMETER TABLE              FOR ttTaxDetail.
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
        cInputListValues = STRING(ROWID(bf-ar-inv)) + "," + ipcMessageType + "," + STRING(iplPostToJournal,"true/false")
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
                INPUT  bf-ar-inv.company, 
                INPUT  bf-ar-inv.tax-code,
                INPUT  lcResponseData,
                OUTPUT opdInvoiceTotal,
                OUTPUT opdInvoiceSubTotal,
                OUTPUT opdTaxTotal,
                OUTPUT TABLE ttTaxDetail,
                OUTPUT oplSuccess,
                OUTPUT opcMessage               
                ).
        END.
    END.
END PROCEDURE.
