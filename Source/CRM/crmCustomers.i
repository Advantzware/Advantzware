/* crmCustomers.i */

{XMLOutput/ttNodes.i NEW}
    
{CRM/crmProcs.i}

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
