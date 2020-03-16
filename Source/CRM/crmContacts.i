/* crmContacts.i */

{XMLOutput/ttNodes.i NEW}
    
{CRM/crmProcs.i}

PROCEDURE pApplyCRM:
    DEFINE VARIABLE cPhone AS CHARACTER NO-UNDO.

    FOR EACH ttCRMContacts:
        IF ttCRMContacts.xxApplyAction EQ NO THEN NEXT.
        IF ttCRMContacts.action EQ "" THEN NEXT.
        ASSIGN
            cPhone = REPLACE(ttCRMContacts.crmPhone," ","")
            cPhone = REPLACE(cPhone,"+","")
            cPhone = REPLACE(cPhone,"-","")
            cPhone = REPLACE(cPhone,"(","")
            cPhone = REPLACE(cPhone,")","")
            cPhone = REPLACE(cPhone,"x","")
            cPhone = REPLACE(cPhone,".","")
            ttCRMContacts.phoneAttention = ttCRMContacts.crmFirstName + " " + ttCRMContacts.crmLastName
            ttCRMContacts.phoneCityCode  = SUBSTR(cPhone,1,3)
            ttCRMContacts.phonePhone     = SUBSTR(cPhone,4,7)
            ttCRMContacts.phoneExt       = SUBSTR(cPhone,11)
            ttCRMContacts.phoneEmail     = ttCRMContacts.crmEmail
            ttCRMContacts.saveAction     = ttCRMContacts.action
            ttCRMContacts.action         = ""
            ttCRMContacts.xxApplyAction  = NO
            .
    END. /* each ttCRMContacts */
END PROCEDURE.

PROCEDURE pSave:
    FOR EACH ttCRMContacts:
        IF ttCRMContacts.saveAction EQ "" THEN NEXT.
        IF ttCRMContacts.saveAction EQ "Update" THEN
        FIND phone EXCLUSIVE-LOCK WHERE ROWID(phone) EQ ttCRMContacts.xxPhoneRowID.
        ELSE DO:
            CREATE phone.
            phone.table_rec_key   = ttCRMContacts.xxTableRecKey.
        END. /* add */
        ASSIGN
            phone.attention       = ttCRMContacts.phoneAttention
            phone.phone_city_code = ttCRMContacts.phoneCityCode
            phone.phone           = ttCRMContacts.phonePhone
            phone.phone_ext       = ttCRMContacts.phoneExt
            phone.e_mail          = ttCRMContacts.phoneEmail
            .
        RELEASE phone.
    END. /* each ttCRMContacts */
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
           ttNodes.level      EQ 5     THEN DO:
            IF ipcType EQ "Accounts" THEN
            CREATE ttAccounts.
            ELSE IF ipcType EQ "Contacts" THEN
            CREATE ttCRMContacts.
        END.

        IF ttNodes.nodeName   EQ "val" AND
           ttNodes.parentName EQ "FL"  AND
           ttNodes.level      EQ 6     THEN DO:
            IF ipcType EQ "Accounts" THEN
            CASE ttNodes.nodeValue:
                WHEN "Account Name" THEN
                ttAccounts.accountName = cValue.
                WHEN "Ticker Symbol" THEN
                ttAccounts.tickerSymbol = cValue.
            END CASE.
            ELSE IF ipcType EQ "Contacts" THEN
            CASE ttNodes.nodeValue:
                WHEN "First Name" THEN
                ttCRMContacts.crmFirstName = cValue.
                WHEN "Last Name" THEN
                ttCRMContacts.crmLastName = cValue.
                WHEN "Phone" THEN
                ttCRMContacts.crmPhone = cValue.
                WHEN "Email" THEN
                ttCRMContacts.crmEmail = cValue.
            END CASE.
        END.

        IF ttNodes.nodeName   EQ "FL"  AND
           ttNodes.parentName EQ "row" AND
           ttNodes.level      EQ 5     THEN
        cValue = ttNodes.nodeValue.
    END. /* each ttnodes */
END PROCEDURE.

PROCEDURE pZohoCRM:
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcRecKey  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiRows    AS INTEGER   NO-UNDO.

    DEFINE VARIABLE lcAccounts    AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcContacts    AS LONGCHAR  NO-UNDO.
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
    IF NOT lSuccess THEN
        RETURN cMessage.     
    IF cAccessToken EQ "" THEN
        RETURN "AccessToken Value is Blank".
    
    IF ipcRecKey NE "" THEN DO:
        FIND FIRST cust NO-LOCK
             WHERE cust.rec_key EQ ipcRecKey
             NO-ERROR.
             
        IF NOT AVAILABLE cust THEN
            RETURN "Customer Record Not Available".

        RUN Zoho_GetAccounts IN hdZohoProcs (
            INPUT        cAccessToken,
            INPUT        cust.cust-no,
            OUTPUT TABLE ttAccounts,
            OUTPUT       lSuccess,
            OUTPUT       cMessage
            ).
        IF NOT lSuccess THEN
            RETURN cMessage.

    END. /* if ipcreckey */
    ELSE DO:
        RUN Zoho_GetAccounts IN hdZohoProcs (
            INPUT        cAccessToken,
            INPUT        "",
            OUTPUT TABLE ttAccounts,
            OUTPUT       lSuccess,
            OUTPUT       cMessage
            ).
        
        IF NOT lSuccess THEN
            RETURN cMessage. 
    END.   
    FOR EACH ttAccounts
        WHERE ttAccounts.tickerSymbol NE ""
          AND ttAccounts.tickerSymbol NE "null":
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ ipcCompany
               AND cust.cust-no EQ ttAccounts.tickerSymbol
             NO-ERROR.
        IF AVAILABLE cust THEN DO:
            RUN Zoho_GetContacts IN hdZohoProcs (
                INPUT        cAccessToken,
                INPUT        ttAccounts.accountName,
                OUTPUT TABLE ttCRMContacts,
                OUTPUT       lSuccess,
                OUTPUT       cMessage
                ).

            FOR EACH ttCRMContacts
                WHERE ttCRMContacts.tickerSymbol EQ ""
                  AND ttCRMContacts.xxRow        EQ 0
                :

                ASSIGN
                    ttCRMContacts.tickerSymbol  = ttAccounts.tickerSymbol
                    opiRows                     = opiRows + 1
                    ttCRMContacts.xxRow         = opiRows
                    ttCRMContacts.xxTableRecKey = cust.rec_key
                    .
                IF ttCRMContacts.crmEmail NE "" THEN
                ttCRMContacts.xxApplyAction = YES.
            END.
    
            FOR EACH phone NO-LOCK
                WHERE phone.table_rec_key EQ cust.rec_key
                :
                RELEASE ttCRMContacts.
                IF phone.e_mail NE "" THEN
                FIND FIRST ttCRMContacts
                     WHERE ttCRMContacts.crmEmail EQ phone.e_mail
                     NO-ERROR.
                IF NOT AVAILABLE ttCRMContacts THEN DO:
                    CREATE ttCRMContacts.
                    ASSIGN
                        ttCRMContacts.tickerSymbol  = ttAccounts.tickerSymbol
                        opiRows                     = opiRows + 1
                        ttCRMContacts.xxRow         = opiRows
                        ttCRMContacts.xxTableRecKey = phone.table_rec_key
                        ttCRMContacts.action        = ""
                        .
                END.
                ELSE
                    ASSIGN
                        ttCRMContacts.action        = "Update"
                        ttCRMContacts.xxApplyAction = YES
                        .
                ASSIGN
                    ttCRMContacts.phoneAttention = phone.attention
                    ttCRMContacts.phoneCityCode  = phone.phone_city_code
                    ttCRMContacts.phonePhone     = phone.phone
                    ttCRMContacts.phoneExt       = phone.phone_ext
                    ttCRMContacts.phoneEmail     = phone.e_mail
                    ttCRMContacts.origAttention  = phone.attention
                    ttCRMContacts.origCityCode   = phone.phone_city_code
                    ttCRMContacts.origPhone      = phone.phone
                    ttCRMContacts.origExt        = phone.phone_ext
                    ttCRMContacts.origEmail      = phone.e_mail
                    ttCRMContacts.xxPhoneRowID   = ROWID(phone)
                    .
            END. /* each phone */
        END. /* avail cust */
    END. /* each ttAccounts */

    RETURN.
END PROCEDURE.
