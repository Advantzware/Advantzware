/* crmCustomers.i */

{XMLOutput/ttNodes.i NEW}
    
{CRM/crmProcs.i}

{api/ttAPIOutboundEvent.i}

/* **********************  Internal Procedures  *********************** */


PROCEDURE pApplyCRM:
    DEFINE VARIABLE cPhone    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAreaCode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPhoneNo  AS CHARACTER NO-UNDO.
    
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
            cAreaCode = IF ttCRMCustomers.crmPhone EQ "null" THEN "" ELSE SUBSTR(cPhone,1,3)
            cPhoneNo  = IF ttCRMCustomers.crmPhone EQ "null" THEN "" ELSE SUBSTR(cPhone,4,7)
            .
        
        ASSIGN
            ttCRMCustomers.custName      = IF ttCRMCustomers.crmName    EQ "null" THEN "" ELSE ttCRMCustomers.crmName
            ttCRMCustomers.custAreaCode  = IF cAreaCode                 EQ "null" THEN "" ELSE cAreaCode
            ttCRMCustomers.custPhone     = IF cPhoneNo                  EQ "null" THEN "" ELSE cPhoneNo
            ttCRMCustomers.custStreet    = IF ttCRMCustomers.crmStreet  EQ "null" THEN "" ELSE ttCRMCustomers.crmStreet
            ttCRMCustomers.custStreet2   = IF ttCRMCustomers.crmStreet2 EQ "null" THEN "" ELSE ttCRMCustomers.crmStreet2
            ttCRMCustomers.custCity      = IF ttCRMCustomers.crmCity    EQ "null" THEN "" ELSE ttCRMCustomers.crmCity
            ttCRMCustomers.custState     = IF ttCRMCustomers.crmState   EQ "null" THEN "" ELSE ttCRMCustomers.crmState
            ttCRMCustomers.custCode      = IF ttCRMCustomers.crmCode    EQ "null" THEN "" ELSE ttCRMCustomers.crmCode
            ttCRMCustomers.saveAction    = IF ttCRMCustomers.action     EQ "null" THEN "" ELSE ttCRMCustomers.action
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
            
            IF joProperties:GetJsonText("customer_number") EQ "null" THEN
                NEXT.
            
            CREATE ttCRMCustomers.
            ttCRMCustomers.crmCity    = joProperties:GetJsonText("city") NO-ERROR.
            ttCRMCustomers.crmState   = joProperties:GetJsonText("state") NO-ERROR.
            ttCRMCustomers.crmName    = joProperties:GetJsonText("name") NO-ERROR.
            ttCRMCustomers.crmPhone   = joProperties:GetJsonText("phone") NO-ERROR.
            ttCRMCustomers.crmStreet  = joProperties:GetJsonText("address") NO-ERROR.
            ttCRMCustomers.crmStreet2 = joProperties:GetJsonText("address2") NO-ERROR.
            ttCRMCustomers.crmCode    = joProperties:GetJsonText("zip") NO-ERROR.
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
    DEFINE VARIABLE cPhone           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAreaCode        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPhoneNo         AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cTempName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTempAreaCode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTempPhoneNo  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTempAddress1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTempAddress2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTempCity     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTempState    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTempZip      AS CHARACTER NO-UNDO.
    
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
                ttCRMCustomers.origName      = cust.name
                ttCRMCustomers.origAreaCode  = cust.area-code
                ttCRMCustomers.origPhone     = cust.phone
                ttCRMCustomers.origStreet    = cust.addr[1]
                ttCRMCustomers.origStreet2   = cust.addr[2]
                ttCRMCustomers.origCity      = cust.city
                ttCRMCustomers.origState     = cust.state
                ttCRMCustomers.origCode      = cust.zip
                ttCRMCustomers.xxCustRowID   = ROWID(cust)
                ttCRMCustomers.xxApplyAction = NO
                .
            
            ASSIGN
                cPhone    = REPLACE(ttCRMCustomers.crmPhone," ","")
                cPhone    = REPLACE(cPhone,"+","")
                cPhone    = REPLACE(cPhone,"-","")
                cPhone    = REPLACE(cPhone,"(","")
                cPhone    = REPLACE(cPhone,")","")
                cPhone    = REPLACE(cPhone,"x","")
                cPhone    = REPLACE(cPhone,".","")
                cAreaCode = IF ttCRMCustomers.crmPhone EQ "null" THEN "null" ELSE SUBSTR(cPhone,1,3)
                cPhoneNo  = IF ttCRMCustomers.crmPhone EQ "null" THEN "null" ELSE SUBSTR(cPhone,4,7)  
                .
            
            ASSIGN
                cTempName     = IF ttCRMCustomers.crmName    EQ "null" THEN "" ELSE ttCRMCustomers.crmName
                cTempAreaCode = IF cAreaCode                 EQ "null" THEN "" ELSE cAreaCode
                cTempPhoneNo  = IF cPhoneNo                  EQ "null" THEN "" ELSE cPhoneNo
                cTempAddress1 = IF ttCRMCustomers.crmStreet  EQ "null" THEN "" ELSE ttCRMCustomers.crmStreet
                cTempAddress2 = IF ttCRMCustomers.crmStreet2 EQ "null" THEN "" ELSE ttCRMCustomers.crmStreet2
                cTempCity     = IF ttCRMCustomers.crmCity    EQ "null" THEN "" ELSE ttCRMCustomers.crmCity
                cTempState    = IF ttCRMCustomers.crmState   EQ "null" THEN "" ELSE ttCRMCustomers.crmState
                cTempZip      = IF ttCRMCustomers.crmCode    EQ "null" THEN "" ELSE ttCRMCustomers.crmCode
                .
                                                                                                
            IF cust.name      NE cTempName     OR
               cust.area-code NE cTempAreaCode OR
               cust.phone     NE cTempPhoneNo  OR
               cust.addr[1]   NE cTempAddress1 OR
               cust.addr[2]   NE cTempAddress2 OR
               cust.city      NE cTempCity     OR
               cust.state     NE cTempState    OR
               cust.zip       NE cTempZip THEN DO:
                ttCRMCustomers.action = "Update".

                IF ttCRMCustomers.crmName    EQ "null" OR
                   ttCRMCustomers.crmPhone   EQ "null" OR
                   ttCRMCustomers.crmStreet  EQ "null" OR
                   ttCRMCustomers.crmStreet2 EQ "null" OR
                   ttCRMCustomers.crmCity    EQ "null" OR
                   ttCRMCustomers.crmState   EQ "null" OR
                   ttCRMCustomers.crmCode    EQ "null" THEN                
                    ttCRMCustomers.xxApplyAction = NO.
                ELSE 
                    ttCRMCustomers.xxApplyAction = YES.
            END.
            ELSE
                DELETE ttCRMCustomers.      
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
            cust.name      = IF ttCRMCustomers.custName     EQ 'null' THEN '' ELSE ttCRMCustomers.custName     
            cust.area-code = IF ttCRMCustomers.custAreaCode EQ 'null' THEN '' ELSE ttCRMCustomers.custAreaCode 
            cust.phone     = IF ttCRMCustomers.custPhone    EQ 'null' THEN '' ELSE ttCRMCustomers.custPhone    
            cust.addr[1]   = IF ttCRMCustomers.custStreet   EQ 'null' THEN '' ELSE ttCRMCustomers.custStreet   
            cust.addr[2]   = IF ttCRMCustomers.custStreet2  EQ 'null' THEN '' ELSE ttCRMCustomers.custStreet2  
            cust.city      = IF ttCRMCustomers.custCity     EQ 'null' THEN '' ELSE ttCRMCustomers.custCity     
            cust.state     = IF ttCRMCustomers.custState    EQ 'null' THEN '' ELSE ttCRMCustomers.custState    
            cust.zip       = IF ttCRMCustomers.custCode     EQ 'null' THEN '' ELSE ttCRMCustomers.custCode     
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
    DEFINE VARIABLE cZohoCRM      AS CHARACTER NO-UNDO.
    
    RUN spGetSettingByName ("ZohoCRM", OUTPUT cZohoCRM).
    IF cZohoCRM EQ "NO" THEN DO:
        MESSAGE "Please active setting 'ZohoCRM' from NK6"
        VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    lSuccess = YES.
    
    RUN CRM\ZohoProcs.p PERSISTENT SET hdZohoProcs.
    
    RUN Zoho_GetRefreshToken IN hdZohoProcs (OUTPUT cRefreshToken).
        
    IF cRefreshToken EQ "" THEN
        RETURN "Refresh Token value is blank. Please update the refresh token in NK6 configuration 'ZohoRefreshToken'".
        
    RUN Zoho_GetClientID IN hdZohoProcs (OUTPUT cClientID).
        
    IF cClientID EQ "" THEN
        RETURN "ClientID value is blank. Please update the client id in NK6 configuration 'ZohoClientID'".

    RUN Zoho_GetClientSecret IN hdZohoProcs (OUTPUT cClientSecret).
        
    IF cClientSecret EQ "" THEN
        RETURN "ClientSecret value is blank. Please update the client secret in NK6 configuration 'ZohoClientSecret'".
    
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
