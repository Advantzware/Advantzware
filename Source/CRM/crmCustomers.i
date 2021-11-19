/* crmCustomers.i */

{XMLOutput/ttNodes.i NEW}
    
{CRM/crmProcs.i}

{api/ttAPIOutboundEvent.i}

/* **********************  Internal Procedures  *********************** */


PROCEDURE pApplyCRM:
    DEFINE VARIABLE cPhone AS CHARACTER NO-UNDO.

    FOR EACH ttCRMCustomers:
        IF ttCRMCustomers.xxApplyAction EQ NO THEN NEXT.
        IF ttCRMCustomers.action EQ "" THEN NEXT.
        ASSIGN
            cPhone = REPLACE(ttCRMCustomers.crmPhone," ","")
            cPhone = REPLACE(cPhone,"+","")
            cPhone = REPLACE(cPhone,"-","")
            cPhone = REPLACE(cPhone,"(","")
            cPhone = REPLACE(cPhone,")","")
            cPhone = REPLACE(cPhone,"x","")
            cPhone = REPLACE(cPhone,".","")
            ttCRMCustomers.custName      = ttCRMCustomers.crmName
            ttCRMCustomers.custAreaCode  = SUBSTR(cPhone,1,3)
            ttCRMCustomers.custPhone     = SUBSTR(cPhone,4,7)
            ttCRMCustomers.custStreet    = ttCRMCustomers.crmStreet
            ttCRMCustomers.custStreet2   = ttCRMCustomers.crmStreet2
            ttCRMCustomers.custCity      = ttCRMCustomers.crmCity
            ttCRMCustomers.custState     = ttCRMCustomers.crmState
            ttCRMCustomers.custCode      = ttCRMCustomers.crmCode
            ttCRMCustomers.saveAction    = ttCRMCustomers.action
            ttCRMCustomers.action        = ""
            ttCRMCustomers.xxApplyAction = NO
            .
    END. /* each ttCRMCustomers */
END PROCEDURE.

PROCEDURE pBuildCustomersHubSpot PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplcResponseData  AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNextPageLinkID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER lError            AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage        AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE oModelParser   AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE oObject        AS JsonObject        NO-UNDO.
    DEFINE VARIABLE jaResult       AS JsonArray         NO-UNDO.
    DEFINE VARIABLE joCustomer     AS JsonObject        NO-UNDO.
    DEFINE VARIABLE joProperties   AS JsonObject        NO-UNDO.
    DEFINE VARIABLE joPaging       AS JsonObject        NO-UNDO.
    DEFINE VARIABLE joNext         AS JsonObject        NO-UNDO.
    DEFINE VARIABLE iResultLength  AS INTEGER           NO-UNDO.
    DEFINE VARIABLE iCount         AS INTEGER           NO-UNDO.
    DEFINE VARIABLE lcResponseData AS LONGCHAR          NO-UNDO.
    DEFINE VARIABLE cStatus        AS CHARACTER         NO-UNDO.
        
    oModelParser = NEW ObjectModelParser().

    FIX-CODEPAGE(lcResponseData) = 'utf-8'.
        
    ASSIGN
        lcResponseData = iplcResponseData
        oObject        = CAST(oModelParser:Parse(INPUT lcResponseData),JsonObject)
        NO-ERROR.
    
    ASSIGN
        jaResult      = oObject:GetJsonArray("results")
        iResultLength = jaResult:LENGTH
        NO-ERROR.
    
    joPaging = oObject:GetJsonObject("paging") NO-ERROR.
    IF VALID-OBJECT(joPaging) THEN
        joNext = joPaging:GetJsonObject("next") NO-ERROR.
    
    IF VALID-OBJECT(joNext) THEN
        opcNextPageLinkID = joNext:GetJsonText("after") NO-ERROR.
        
    IF VALID-OBJECT(jaResult) THEN DO:
        DO iCount = 1 TO iResultLength:
            joCustomer = jaResult:GetJsonObject(iCount) NO-ERROR.
            
            IF NOT VALID-OBJECT(joCustomer) THEN
                NEXT.
                
            joProperties = joCustomer:GetJsonObject("properties") NO-ERROR.
            
            IF NOT VALID-OBJECT(joProperties) THEN
                NEXT.
            
            CREATE ttCRMCustomers.
            ttCRMCustomers.crmCity    = joProperties:GetJsonText("city") NO-ERROR.
            ttCRMCustomers.crmState   = joProperties:GetJsonText("state") NO-ERROR.
            ttCRMCustomers.crmName    = joProperties:GetJsonText("name") NO-ERROR.
            ttCRMCustomers.crmPhone   = joProperties:GetJsonText("phone") NO-ERROR.
            ttCRMCustomers.crmStreet  = joProperties:GetJsonText("address") NO-ERROR.
            ttCRMCustomers.crmStreet2 = joProperties:GetJsonText("address2") NO-ERROR.
            ttCRMCustomers.tickerSymbol = joProperties:GetJsonText("customer_number") NO-ERROR.

            RELEASE ttCRMCustomers. 
        END. 
    END.
    
END PROCEDURE.

PROCEDURE pHubspotCRM PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiRows    AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdOutboundProcs  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUser            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcResponseData   AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cNextpageLinkID  AS CHARACTER NO-UNDO INITIAL "0".
    
    DEFINE BUFFER bf-APIOutboundEvent FOR APIOutboundEvent.
    
    RUN spGetSessionParam ("UserID", OUTPUT cUser).
    
    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.
    
    REPEAT:        
        RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs (
            INPUT  ipcCompany,                             /* Company Code (Mandatory) */
            INPUT  "",                                     /* Location Code (Mandatory) */
            INPUT  "GetCustomers",                         /* API ID (Mandatory) */
            INPUT  "",                                     /* Scope ID */
            INPUT  "",                                     /* Scope Type */
            INPUT  "GetAll",                               /* Trigger ID (Mandatory) */
            INPUT  "HubSpotNextPageLinkID",                /* Comma separated list of table names for which data being sent (Mandatory) */
            INPUT  cNextpageLinkID,                        /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
            INPUT  cUser,                                  /* Primary ID for which API is called for (Mandatory) */   
            INPUT  "Get Hubspot customer list",            /* Event's description (Optional) */
            OUTPUT lSuccess,                               /* Success/Failure flag */
            OUTPUT opcMessage                              /* Status message */
            ) NO-ERROR.
        
        RUN Outbound_GetEvents IN hdOutboundProcs (
            OUTPUT TABLE ttAPIOutboundEvent BY-REFERENCE
            ).
        
        RUN Outbound_ResetContext IN hdOutboundProcs.
        
        IF NOT lSuccess THEN DO:
            oplError = TRUE.
            LEAVE.
        END.
        
        IF NOT TEMP-TABLE ttAPIOutboundEvent:HAS-RECORDS THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Failure in calling the api"
                .
            LEAVE.
        END.
    
        FIND FIRST ttAPIOutboundEvent NO-ERROR.
        IF AVAILABLE ttAPIOutboundEvent THEN DO:
            FIND FIRST bf-APIOutboundEvent NO-LOCK
                 WHERE bf-APIOutboundEvent.apiOutboundEventID EQ ttAPIOutboundEvent.apiOutboundEventID
                 NO-ERROR.
            IF AVAILABLE bf-APIOutboundEvent THEN DO:
                IF NOT bf-APIOutboundEvent.success THEN DO:
                    ASSIGN
                        oplError   = TRUE
                        opcMessage = "API request failed. View Outbound events for more information"
                        .
                    LEAVE.
                END.
                lcResponseData = bf-APIOutboundEvent.responseData.
    
                RUN pBuildCustomersHubSpot (
                    INPUT  lcResponseData,
                    OUTPUT cNextpageLinkID,
                    OUTPUT oplError,
                    OUTPUT opcMessage               
                    ).
            END.
        END.

        IF cNextpageLinkID EQ "" OR cNextPageLinkID EQ "0" OR cNextpageLinkID EQ ? THEN
            LEAVE.
    END.
    
    DELETE PROCEDURE hdOutboundProcs. 

    FOR EACH ttCRMCustomers:
        opiRows = opiRows + 1.
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ ipcCompany
               AND cust.cust-no EQ ttCRMCustomers.tickerSymbol
             NO-ERROR.
        IF AVAILABLE cust THEN DO:
            ASSIGN
                ttCRMCustomers.custName      = cust.name
                ttCRMCustomers.custAreaCode  = cust.area-code
                ttCRMCustomers.custPhone     = cust.phone
                ttCRMCustomers.custStreet    = cust.addr[1]
                ttCRMCustomers.custStreet2   = cust.addr[2]
                ttCRMCustomers.custCity      = cust.city
                ttCRMCustomers.custState     = cust.state
                ttCRMCustomers.custCode      = cust.zip
                ttCRMCustomers.action        = "Update"
                ttCRMCustomers.origName      = cust.name
                ttCRMCustomers.origAreaCode  = cust.area-code
                ttCRMCustomers.origPhone     = cust.phone
                ttCRMCustomers.origStreet    = cust.addr[1]
                ttCRMCustomers.origStreet2   = cust.addr[2]
                ttCRMCustomers.origCity      = cust.city
                ttCRMCustomers.origState     = cust.state
                ttCRMCustomers.origCode      = cust.zip
                ttCRMCustomers.xxCustRowID   = ROWID(cust)
                ttCRMCustomers.xxApplyAction = YES
                .
        END. /* avail cust */
    END. /* each ttCRMCustomers */
    
END PROCEDURE.

PROCEDURE pSave:
    FOR EACH ttCRMCustomers:
        IF ttCRMCustomers.saveAction EQ "" THEN NEXT.
        IF ttCRMCustomers.saveAction EQ "Update" THEN
        FIND cust EXCLUSIVE-LOCK WHERE ROWID(cust) EQ ttCRMCustomers.xxCustRowID.
        ELSE DO:
            CREATE cust.
            cust.cust-no = ttCRMCustomers.TickerSymbol.
        END. /* add */
        ASSIGN
            cust.name      = ttCRMCustomers.custName
            cust.area-code = ttCRMCustomers.custAreaCode
            cust.phone     = ttCRMCustomers.custPhone
            cust.addr[1]   = ttCRMCustomers.custStreet
            cust.addr[2]   = ttCRMCustomers.custStreet2
            cust.city      = ttCRMCustomers.custCity
            cust.state     = ttCRMCustomers.custState
            cust.zip       = ttCRMCustomers.custCode
            .
        RELEASE cust.
    END. /* each ttCRMCustomers */
END PROCEDURE.

PROCEDURE pXML:
    DEFINE INPUT PARAMETER ipcXMLFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType    AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValue  AS CHARACTER NO-UNDO.

    RUN XMLOutput/XMLParser.p (ipcXMLFile).
    FOR EACH ttNodes:
        ASSIGN
            ttNodes.nodeName   = TRIM(LEFT-TRIM(ttNodes.nodeName))
            ttNodes.nodeValue  = TRIM(LEFT-TRIM(ttNodes.nodeValue))
            ttNodes.parentName = TRIM(LEFT-TRIM(ttNodes.parentName))
            .
        IF ttNodes.nodeName   EQ "no"  AND
           ttNodes.parentName EQ "row" AND
           ttNodes.level      EQ 5     THEN
        CREATE ttCRMCustomers.

        IF ttNodes.nodeName   EQ "val" AND
           ttNodes.parentName EQ "FL"  AND
           ttNodes.level      EQ 6     THEN
        CASE ttNodes.nodeValue:
            WHEN "Account Name" THEN
            ttCRMCustomers.crmName = cValue.
            WHEN "Billing City" THEN
            ttCRMCustomers.crmCity = cValue.
            WHEN "Billing State" THEN
            ttCRMCustomers.crmState = cValue.
            WHEN "Billing Code" THEN
            ttCRMCustomers.crmCode = cValue.
            WHEN "Billing Street" THEN
            ttCRMCustomers.crmStreet = cValue.
            WHEN "Billing Street 2" THEN
            ttCRMCustomers.crmStreet2 = cValue.
            WHEN "Phone" THEN
            ttCRMCustomers.crmPhone = cValue.
            WHEN "Ticker Symbol" THEN
            ttCRMCustomers.tickerSymbol = cValue.
        END CASE.

        IF ttNodes.nodeName   EQ "FL"  AND
           ttNodes.parentName EQ "row" AND
           ttNodes.level      EQ 5     THEN
        cValue = ttNodes.nodeValue.
    END. /* each ttnodes */
END PROCEDURE.

PROCEDURE pZohoCRM:
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiRows    AS INTEGER   NO-UNDO.

    DEFINE VARIABLE lcAccounts    AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE iCnt          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hdZohoProcs   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cRefreshToken AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cClientID     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cClientSecret AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAccessToken  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.
    
    lSuccess = YES.
    
    RUN CRM\ZohoProcs.p PERSISTENT SET hdZohoProcs.
    
    RUN Zoho_GetRefreshToken IN hdZohoProcs (
        INPUT  ipcCompany,
        OUTPUT cRefreshToken
        ).
        
    IF cRefreshToken EQ "" THEN
        RETURN "Refresh Token value is blank. Please update the refresh token in NK1 configuration 'ZohoRefreshToken'".
        
    RUN Zoho_GetClientID IN hdZohoProcs (
        INPUT  ipcCompany,
        OUTPUT cClientID
        ).
        
    IF cClientID EQ "" THEN
        RETURN "ClientID value is blank. Please update the client id in NK1 configuration 'ZohoClientID'".

    RUN Zoho_GetClientSecret IN hdZohoProcs (
        INPUT  ipcCompany,
        OUTPUT cClientSecret
        ).
        
    IF cClientSecret EQ "" THEN
        RETURN "ClientSecret value is blank. Please update the client secret in NK1 configuration 'ZohoClientSecret'".
    
     RUN Zoho_GetAccessToken IN hdZohoProcs (
         INPUT  cRefreshToken,
         INPUT  cClientID,
         INPUT  cClientSecret,
         OUTPUT cAccessToken,
         OUTPUT lSuccess,
         OUTPUT cMessage
         ).
     
    IF cAccessToken EQ "" THEN
        RETURN "AccessToken Value is Blank".
 
    RUN Zoho_GetCustomers IN hdZohoProcs (
        INPUT        cAccessToken,
        OUTPUT TABLE ttCRMCustomers,
        OUTPUT       lSuccess,
        OUTPUT       cMessage
        ) NO-ERROR.
    
    IF NOT lSuccess THEN
        RETURN cMessage.                                                                                                                                                   
    
    FOR EACH ttCRMCustomers
        WHERE ttCRMCustomers.tickerSymbol EQ "" 
           OR ttCRMCustomers.tickerSymbol EQ "null":
   
        DELETE ttCRMCustomers.
        
    END. /* each ttCRMCustomers */

    FOR EACH ttCRMCustomers:
        opiRows = opiRows + 1.
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ ipcCompany
               AND cust.cust-no EQ ttCRMCustomers.tickerSymbol
             NO-ERROR.
        IF AVAILABLE cust THEN DO:
            ASSIGN
                ttCRMCustomers.custName      = cust.name
                ttCRMCustomers.custAreaCode  = cust.area-code
                ttCRMCustomers.custPhone     = cust.phone
                ttCRMCustomers.custStreet    = cust.addr[1]
                ttCRMCustomers.custStreet2   = cust.addr[2]
                ttCRMCustomers.custCity      = cust.city
                ttCRMCustomers.custState     = cust.state
                ttCRMCustomers.custCode      = cust.zip
                ttCRMCustomers.action        = "Update"
                ttCRMCustomers.origName      = cust.name
                ttCRMCustomers.origAreaCode  = cust.area-code
                ttCRMCustomers.origPhone     = cust.phone
                ttCRMCustomers.origStreet    = cust.addr[1]
                ttCRMCustomers.origStreet2   = cust.addr[2]
                ttCRMCustomers.origCity      = cust.city
                ttCRMCustomers.origState     = cust.state
                ttCRMCustomers.origCode      = cust.zip
                ttCRMCustomers.xxCustRowID   = ROWID(cust)
                ttCRMCustomers.xxApplyAction = YES
                .
        END. /* avail cust */
    END. /* each ttCRMCustomers */

    RETURN.
END PROCEDURE.
