
/*------------------------------------------------------------------------
    File        : CRM/ZohoProcs
    Purpose     : Procedures related to calling Zoho CRM APIs

    Syntax      :

    Description : Procedures related to calling Zoho CRM APIs

    Author(s)   : Vishnu Vellanki
    Created     : Tue Mar 10 05:54:49 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Json.ObjectModel.*.

{CRM/ttCRMContacts.i}
{CRM/ttCRMCustomers.i}
{api/ttAPIOutboundEvent.i}

DEFINE VARIABLE oModelParser   AS ObjectModelParser NO-UNDO.
DEFINE VARIABLE oObject        AS JsonObject        NO-UNDO.
DEFINE VARIABLE cTempDir       AS CHARACTER         NO-UNDO.
DEFINE VARIABLE oAccount       AS JsonObject        NO-UNDO.

RUN FileSys_GetTempDirectory(
    OUTPUT cTempDir
    ).
    
oModelParser = NEW ObjectModelParser().
               


/* **********************  Internal Procedures  *********************** */


PROCEDURE Zoho_CreateDeskTicket:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcSubject     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdOutboundProcs  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcRequestData    AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cSubject         AS CHARACTER NO-UNDO.
    
    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.
    
    RUN Outbound_PrepareAndExecute IN hdOutboundProcs (
        INPUT  "",                                       /* Company Code (Mandatory) */
        INPUT  "",                                       /* Location Code (Mandatory) */
        INPUT  "CreateZohoDeskTicket",                   /* API ID (Mandatory) */
        INPUT  "_default",                               /* ClientID */
        INPUT  "CreateTicket",                           /* Trigger ID (Mandatory) */
        INPUT  "Subject,Description",                    /* Comma separated list of table names for which data being sent (Mandatory) */
        INPUT  ipcSubject + "," + ipcDescription,        /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
        INPUT  USERID("ASI"),                            /* Primary ID for which API is called for (Mandatory) */   
        INPUT  "Creating zoho desk ticket",              /* Event's description (Optional) */
        OUTPUT lSuccess,                                 /* Success/Failure flag */
        OUTPUT cMessage                                  /* Status message */
        ) NO-ERROR.
    
    RUN Outbound_GetEvents IN hdOutboundProcs (OUTPUT TABLE ttAPIOutboundEvent).
    
    FIX-CODEPAGE(lcRequestData) = 'utf-8'.
    
    lcRequestData = "".
    
    FIND FIRST ttAPIOutboundEvent NO-LOCK NO-ERROR.
    IF AVAILABLE ttAPIOutboundEvent THEN DO:
        FIND FIRST apiOutboundEvent NO-LOCK
             WHERE apiOutboundEvent.apiOutboundEventID EQ ttAPIOutboundEvent.APIOutboundEventID
             NO-ERROR.
        IF AVAILABLE apiOutboundEvent THEN
            lcRequestData = apiOutboundEvent.responseData.
    END.    

    IF lcRequestData EQ "" THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Unable to fetch response from Zoho"
            . 
        
        RETURN.
    END.

    ASSIGN
        oObject  = CAST(oModelParser:Parse(INPUT lcRequestData),JsonObject)
        cSubject = oObject:GetJsonText('subject')
        NO-ERROR.
    
    IF cSubject EQ "" THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Error while creating zoho ticket"
            . 
        
        RETURN.        
    END.
    
    FINALLY:
        IF VALID-HANDLE(hdOutboundProcs) THEN
            DELETE OBJECT hdOutboundProcs.  
    END FINALLY.
END PROCEDURE.


PROCEDURE pUpdateDeskAccount PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAttribute AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcValue     AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdOutboundProcs    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lSuccess           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcRequestData      AS LONGCHAR  NO-UNDO.
    
    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.
    
    RUN Outbound_PrepareAndExecute IN hdOutboundProcs (
        INPUT  "",                                       /* Company Code (Mandatory) */
        INPUT  "",                                       /* Location Code (Mandatory) */
        INPUT  "UpdateZohoDeskAccount",                  /* API ID (Mandatory) */
        INPUT  "_default",                               /* ClientID */
        INPUT  "UpdateAccount",                          /* Trigger ID (Mandatory) */
        INPUT  ipcAttribute,                             /* Comma separated list of table names for which data being sent (Mandatory) */
        INPUT  ipcValue,                                 /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
        INPUT  USERID("ASI"),                            /* Primary ID for which API is called for (Mandatory) */   
        INPUT  "Updating account details",               /* Event's description (Optional) */
        OUTPUT lSuccess,                                 /* Success/Failure flag */
        OUTPUT cMessage                                  /* Status message */
        ) NO-ERROR.
    
    RUN Outbound_GetEvents IN hdOutboundProcs (OUTPUT TABLE ttAPIOutboundEvent).
    
    FIX-CODEPAGE(lcRequestData) = 'utf-8'.
    
    lcRequestData = "".
    
    FIND FIRST ttAPIOutboundEvent NO-LOCK NO-ERROR.
    IF AVAILABLE ttAPIOutboundEvent THEN DO:
        FIND FIRST apiOutboundEvent NO-LOCK
             WHERE apiOutboundEvent.apiOutboundEventID EQ ttAPIOutboundEvent.APIOutboundEventID
             NO-ERROR.
        IF AVAILABLE apiOutboundEvent THEN
            lcRequestData = apiOutboundEvent.responseData.
    END.    

    IF lcRequestData EQ "" THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Unable to fetch response from Zoho"
            . 
        
        RETURN.
    END.

    ASSIGN
        oObject            = CAST(oModelParser:Parse(INPUT lcRequestData),JsonObject)
        NO-ERROR.
    
    FINALLY:
        IF VALID-HANDLE(hdOutboundProcs) THEN
            DELETE OBJECT hdOutboundProcs.  
    END FINALLY.
END PROCEDURE.

PROCEDURE Zoho_GetAccountLicenseExpiryDate:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opdtLicenseExpiryDate AS DATE NO-UNDO.
    
    DEFINE VARIABLE lError             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLicenseExpiryDate AS CHARACTER NO-UNDO.
    
    IF NOT VALID-OBJECT(oAccount) THEN DO:
        RUN Zoho_GetDeskAccount (OUTPUT lError, OUTPUT cMessage).
        IF lError THEN
            RETURN.
    END. 
    
    IF NOT VALID-OBJECT(oAccount) THEN
        RETURN.
        
    cLicenseExpiryDate = oAccount:GetJsonObject("cf"):GetJsonText("cf_license_time_out") NO-ERROR.
    
    IF cLicenseExpiryDate EQ "null" OR cLicenseExpiryDate EQ "" THEN
        opdtLicenseExpiryDate = ?.
    ELSE
        opdtLicenseExpiryDate = DATE(INTEGER(SUBSTRING(cLicenseExpiryDate, 6, 2)), INTEGER(SUBSTRING(cLicenseExpiryDate, 9, 2)), INTEGER(SUBSTRING(cLicenseExpiryDate, 1, 4))) NO-ERROR. 
END PROCEDURE.

PROCEDURE Zoho_GetAccountLicensedUserCount:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiUserCount AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    IF NOT VALID-OBJECT(oAccount) THEN DO:
        RUN Zoho_GetDeskAccount (OUTPUT lError, OUTPUT cMessage).
        IF lError THEN
            RETURN.
    END. 
    
    IF NOT VALID-OBJECT(oAccount) THEN
        RETURN.
        
    opiUserCount = INTEGER(oAccount:GetJsonObject("cf"):GetJsonText("cf_advantzware_user_count")) NO-ERROR.
END PROCEDURE.

PROCEDURE Zoho_GetAccountBillableUserCount:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiUserCount AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    IF NOT VALID-OBJECT(oAccount) THEN DO:
        RUN Zoho_GetDeskAccount (OUTPUT lError, OUTPUT cMessage).
        IF lError THEN
            RETURN.
    END. 
    
    IF NOT VALID-OBJECT(oAccount) THEN
        RETURN.
        
    opiUserCount = INTEGER(oAccount:GetJsonObject("cf"):GetJsonText("cf_billable_users")) NO-ERROR.
END PROCEDURE.

PROCEDURE Zoho_GetDeskAccount:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplError   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdOutboundProcs    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lSuccess           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcRequestData      AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cLicenseExpiryDate AS CHARACTER NO-UNDO.
    
    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.
    
    RUN Outbound_PrepareAndExecute IN hdOutboundProcs (
        INPUT  "",                                       /* Company Code (Mandatory) */
        INPUT  "",                                       /* Location Code (Mandatory) */
        INPUT  "GetZohoDeskAccount",                     /* API ID (Mandatory) */
        INPUT  "_default",                               /* ClientID */
        INPUT  "GetAccount",                             /* Trigger ID (Mandatory) */
        INPUT  "GetAccount",                             /* Comma separated list of table names for which data being sent (Mandatory) */
        INPUT  "GetAccount",                             /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
        INPUT  USERID("ASI"),                            /* Primary ID for which API is called for (Mandatory) */   
        INPUT  "Fetching account details",               /* Event's description (Optional) */
        OUTPUT lSuccess,                                 /* Success/Failure flag */
        OUTPUT cMessage                                  /* Status message */
        ) NO-ERROR.
    
    RUN Outbound_GetEvents IN hdOutboundProcs (OUTPUT TABLE ttAPIOutboundEvent).
    
    FIX-CODEPAGE(lcRequestData) = 'utf-8'.
    
    lcRequestData = "".
    
    FIND FIRST ttAPIOutboundEvent NO-LOCK NO-ERROR.
    IF AVAILABLE ttAPIOutboundEvent THEN DO:
        FIND FIRST apiOutboundEvent NO-LOCK
             WHERE apiOutboundEvent.apiOutboundEventID EQ ttAPIOutboundEvent.APIOutboundEventID
             NO-ERROR.
        IF AVAILABLE apiOutboundEvent THEN
            lcRequestData = apiOutboundEvent.responseData.
    END.    

    IF lcRequestData EQ "" THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Unable to fetch response from Zoho"
            . 
        
        RETURN.
    END.

    oAccount = CAST(oModelParser:Parse(INPUT lcRequestData),JsonObject) NO-ERROR.
    
    FINALLY:
        IF VALID-HANDLE(hdOutboundProcs) THEN
            DELETE OBJECT hdOutboundProcs.  
    END FINALLY.
END PROCEDURE.

PROCEDURE Zoho_UpdateDeskAccessToken:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplError       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdOutboundProcs  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcRequestData    AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE iIndex           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cAccessToken     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cZohoDeskAPIList AS CHARACTER NO-UNDO INITIAL "CreateZohoDeskTicket,GetZohoDeskAccount,UpdateZohoDeskAccount".
    
    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.
    
    RUN Outbound_PrepareAndExecute IN hdOutboundProcs (
        INPUT  "",                                       /* Company Code (Mandatory) */
        INPUT  "",                                       /* Location Code (Mandatory) */
        INPUT  "GetZohoDeskAccessToken",                 /* API ID (Mandatory) */
        INPUT  "_default",                               /* ClientID */
        INPUT  "GetAccessToken",                         /* Trigger ID (Mandatory) */
        INPUT  "AccessToken",                            /* Comma separated list of table names for which data being sent (Mandatory) */
        INPUT  "AccessToken",                            /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
        INPUT  USERID("ASI"),                            /* Primary ID for which API is called for (Mandatory) */   
        INPUT  "Fetching Access Token",                  /* Event's description (Optional) */
        OUTPUT lSuccess,                                 /* Success/Failure flag */
        OUTPUT cMessage                                  /* Status message */
        ) NO-ERROR.
    
    RUN Outbound_GetEvents IN hdOutboundProcs (OUTPUT TABLE ttAPIOutboundEvent).
    
    FIX-CODEPAGE(lcRequestData) = 'utf-8'.
    
    lcRequestData = "".
    
    FIND FIRST ttAPIOutboundEvent NO-LOCK NO-ERROR.
    IF AVAILABLE ttAPIOutboundEvent THEN DO:
        FIND FIRST apiOutboundEvent NO-LOCK
             WHERE apiOutboundEvent.apiOutboundEventID EQ ttAPIOutboundEvent.APIOutboundEventID
             NO-ERROR.
        IF AVAILABLE apiOutboundEvent THEN
            lcRequestData = apiOutboundEvent.responseData.
    END.    

    IF lcRequestData EQ "" THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Unable to fetch response from Zoho"
            . 
        
        RETURN.
    END.

    ASSIGN
        oObject      = CAST(oModelParser:Parse(INPUT lcRequestData),JsonObject)
        cAccessToken = oObject:GetJsonText('access_token')
        NO-ERROR.

    IF cAccessToken EQ "" THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Error while fetching refresh token"
            . 
        
        RETURN.    
    END.
    
    DO iIndex = 1 TO NUM-ENTRIES(cZohoDeskAPIList):
        FOR EACH apiOutbound NO-LOCK
            WHERE apiOutbound.company  EQ ""
              AND apiOutbound.apiID    EQ ENTRY(iIndex, cZohoDeskAPIList)
              AND apiOutbound.clientID EQ "_default":
            FIND FIRST apiOutboundContent EXCLUSIVE-LOCK
                 WHERE apiOutboundContent.apiOutboundID EQ apiOutbound.apiOutboundID
                   AND apiOutboundContent.contentKey    EQ "Authorization"
                 NO-ERROR.
            IF AVAILABLE apiOutboundContent THEN
                apiOutboundContent.contentValue = "Bearer " + cAccessToken.
        END.
    END.
    
    FINALLY:
        IF VALID-HANDLE(hdOutboundProcs) THEN
            DELETE OBJECT hdOutboundProcs.	
    END FINALLY.
END PROCEDURE.

PROCEDURE Zoho_UpdateDeskAccountUserCount:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiUserCount AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cAttributeName AS CHARACTER NO-UNDO.
    
    RUN spGetSettingByName ("ZohoUserCountAttribute", OUTPUT cAttributeName).
    
    RUN pUpdateDeskAccount(cAttributeName, ipiUserCount, OUTPUT oplError, OUTPUT opcMessage).

END PROCEDURE.

PROCEDURE Zoho_UpdateRefreshToken:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcGrantToken   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientSecret AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRefreshToken AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess      AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cCommand       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnURI     AS CHARACTER NO-UNDO INITIAL "https://www.dummy.com". /* dummy URL */
    DEFINE VARIABLE cGrantType     AS CHARACTER NO-UNDO INITIAL "authorization_code". /* Grant Type */
    DEFINE VARIABLE cAPIURL        AS CHARACTER NO-UNDO INITIAL "https://accounts.zoho.com/oauth/v2/token". /* API URL */
    DEFINE VARIABLE cResponse      AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cResponseFile  AS CHARACTER NO-UNDO.
    
    ASSIGN
        cResponseFile            = cTempDir + "\zoho_refresh" + STRING(MTIME) + ".txt"
        FIX-CODEPAGE (cResponse) = 'utf-8'
        cCommand                 = SEARCH("curl.exe") + ' --insecure -X POST "'
                                 + cAPIURL + '?code=' + ipcGrantToken 
                                 + '^&client_id=' + ipcClientID 
                                 + '^&client_secret=' + ipcClientSecret 
                                 + '^&grant_type=' + cGrantType
                                 + '^&redirect_uri=' + cReturnURI  + '"' 
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
    
    COPY-LOB FILE cResponseFile TO cResponse.

    IF cResponse EQ "" THEN 
        RETURN.
   
    ASSIGN
        oObject         = CAST(oModelParser:Parse(INPUT cResponse),JsonObject).
        opcRefreshToken = oObject:GetJsonText('refresh_token')
        NO-ERROR.

    IF opcRefreshToken NE "" THEN DO:
        RUN spSetSettingByName ("ZohoRefreshToken", opcRefreshToken).

        oplSuccess = RETURN-VALUE EQ "".  
    END.
    ELSE
        ASSIGN
            opcMessage = "Error while generating refresh token"
            oplSuccess = FALSE
            .    
        
    OS-DELETE VALUE(cResponseFile).   
END PROCEDURE.

PROCEDURE Zoho_GetRefreshToken:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  output refresh token
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcRefreshToken AS CHARACTER NO-UNDO.

    RUN spGetSettingByName ("ZohoRefreshToken", OUTPUT opcRefreshToken).
END.

PROCEDURE Zoho_GetClientID:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  output clientid
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcClientID AS CHARACTER NO-UNDO.

    RUN spGetSettingByName ("ZohoClientID", OUTPUT opcClientID).
END.

PROCEDURE Zoho_GetClientSecret:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  input company, output clientsecret
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcClientSecret AS CHARACTER NO-UNDO.

    RUN spGetSettingByName ("ZohoClientSecret", OUTPUT opcClientSecret).
END.

PROCEDURE Zoho_GetAccessToken:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  input refresh token,clientid and clientsecret output access token
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcRefreshToken AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientID     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcClientSecret AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAccessToken  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess      AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.
        
    DEFINE VARIABLE cCommand       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnURI     AS CHARACTER NO-UNDO INITIAL "https://www.dummy.com". /* dummy URL */
    DEFINE VARIABLE cScope         AS CHARACTER NO-UNDO INITIAL "ZohoCRM.modules.ALL". /* Scope */
    DEFINE VARIABLE cGrantType     AS CHARACTER NO-UNDO INITIAL "refresh_token". /* Grant Type */
    DEFINE VARIABLE cAPIURL        AS CHARACTER NO-UNDO INITIAL "https://accounts.zoho.com/oauth/v2/token". /* API URL */
    DEFINE VARIABLE cResponse      AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cResponseFile  AS CHARACTER NO-UNDO.
    
    ASSIGN
        oplSuccess               = YES
        cResponseFile            = cTempDir + "\zoho_access" + STRING(MTIME) + ".txt"
        FIX-CODEPAGE (cResponse) = 'utf-8'
        cCommand                 = SEARCH("curl.exe") + ' --insecure -X POST "' 
                                 + cAPIURL + '?refresh_token=' + ipcRefreshToken 
                                 + '^&client_id=' + ipcClientID 
                                 + '^&client_secret=' + ipcClientSecret 
                                 + '^&scope=' + cScope 
                                 + '^&redirect_uri=' + cReturnURI 
                                 + '^&grant_type=' + cGrantType + '"'
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
    
    COPY-LOB FILE cResponseFile TO cResponse.
    
    IF cResponse EQ "" THEN 
        RETURN.
   
    ASSIGN
        oObject        = CAST(oModelParser:Parse(INPUT cResponse),JsonObject).
        opcAccessToken = oObject:GetJsonText('access_token')
        NO-ERROR.
        
    OS-DELETE VALUE(cResponseFile).
END PROCEDURE.

PROCEDURE Zoho_GetCustomers:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  input access token output contacts
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAccessToken AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttCRMCustomers.
    DEFINE OUTPUT PARAMETER oplSuccess     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cCommand               AS CHARACTER  NO-UNDO.  
    DEFINE VARIABLE cResponseCustomersFile AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cResponseCustomers     AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE cAPIURL                AS CHARACTER  NO-UNDO INITIAL "https://www.zohoapis.com/crm/v2/Accounts". /* API URL */
    DEFINE VARIABLE jsonData               AS JsonArray  NO-UNDO.
    DEFINE VARIABLE CustomersPropertyNames AS CHARACTER  NO-UNDO EXTENT.
    DEFINE VARIABLE iLengthProperty        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE CustomerObj            AS JsonObject NO-UNDO.
    DEFINE VARIABLE iCount                 AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iCount1                AS INTEGER    NO-UNDO.
    DEFINE VARIABLE CustomerPropertyName   AS CHARACTER  NO-UNDO.
    
    ASSIGN
        oplSuccess                       = YES
        FIX-CODEPAGE (cResponseCustomers)= 'utf-8'
        cResponseCustomersFile           = cTempDir + "\zoho_customers" + STRING(MTIME) + ".txt"
        cCommand                         = SEARCH("curl.exe") + ' "' 
                                         + cAPIURL + '" --insecure -X GET -H "Authorization: Zoho-oauthtoken ' 
                                         + ipcAccessToken + '"'
        .

    /* execute CURL command with required parameters to call the API */
    RUN OS_RunCommand (
        INPUT  cCommand,             /* Command string to run */
        INPUT  cResponseCustomersFile, /* File name to write the command output */
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
        OS-DELETE VALUE(cResponseCustomersFile). 
        RETURN.
    END.
    
    COPY-LOB FILE cResponseCustomersFile TO cResponseCustomers.
    
    IF cResponseCustomers EQ "" THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "No Data Returned"
            .  
        RETURN.
    END.        
    ASSIGN
        oObject         = CAST(oModelParser:Parse(INPUT cResponseCustomers),JsonObject)
        jsonData        = oObject:GetJsonArray("data")
        iLengthProperty = jsonData:LENGTH
        NO-ERROR.

    DO iCount = 1 TO iLengthProperty:
        ASSIGN
            CustomerObj            = jsonData:GetJsonObject(iCount)
            CustomersPropertyNames = CustomerObj:getNames()
            NO-ERROR.

        CREATE ttCRMCustomers.
        DO iCount1 = 1 TO EXTENT(CustomersPropertyNames):
            CustomerPropertyName=CustomersPropertyNames[iCount1].
            CASE CustomerPropertyName:
                WHEN "Account_Name" THEN
                    ttCRMCustomers.crmName = STRING(CustomerObj:GetJsonText(CustomerPropertyName)) NO-ERROR.
                WHEN "Billing_City" THEN
                    ttCRMCustomers.crmCity = STRING(CustomerObj:GetJsonText(CustomerPropertyName)) NO-ERROR.
                WHEN "Billing_State" THEN
                    ttCRMCustomers.crmState = STRING(CustomerObj:GetJsonText(CustomerPropertyName)) NO-ERROR.
                WHEN "Billing_Code" THEN
                    ttCRMCustomers.crmCode = STRING(CustomerObj:GetJsonText(CustomerPropertyName)) NO-ERROR.
                WHEN "Billing_Street" THEN
                    ttCRMCustomers.crmStreet = STRING(CustomerObj:GetJsonText(CustomerPropertyName)) NO-ERROR.
                WHEN "Billing Street 2" THEN
                    ttCRMCustomers.crmStreet2 = STRING(CustomerObj:GetJsonText(CustomerPropertyName)) NO-ERROR.
                WHEN "Phone" THEN
                    ttCRMCustomers.crmPhone = STRING(CustomerObj:GetJsonText(CustomerPropertyName)) NO-ERROR.
                WHEN "Ticker_Symbol" THEN
                    ttCRMCustomers.tickerSymbol = STRING(CustomerObj:GetJsonText(CustomerPropertyName)) NO-ERROR.
            END CASE.

        END.

    END.
    OS-DELETE VALUE(cResponseCustomersFile). 
END PROCEDURE.

PROCEDURE Zoho_GetContacts:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  input access token output contacts
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAccessToken AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcAccountName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttCRMContacts.
    DEFINE OUTPUT PARAMETER oplSuccess     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cCommand              AS CHARACTER  NO-UNDO.  
    DEFINE VARIABLE cResponseContactsFile AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cResponseContacts     AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE cAPIURL               AS CHARACTER  NO-UNDO INITIAL "https://www.zohoapis.com/crm/v2/Contacts". /* API URL */
    DEFINE VARIABLE jsonData              AS JsonArray  NO-UNDO.
    DEFINE VARIABLE ContactsPropertyNames AS CHARACTER  NO-UNDO EXTENT.
    DEFINE VARIABLE iLengthProperty       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE ContactObj            AS JsonObject NO-UNDO.
    DEFINE VARIABLE iCount                AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iCount1               AS INTEGER    NO-UNDO.
    DEFINE VARIABLE ContactPropertyName   AS CHARACTER  NO-UNDO.
    
    ASSIGN
        oplSuccess                       = YES
        FIX-CODEPAGE (cResponseContacts) = 'utf-8'
        cResponseContactsFile            = cTempDir + "\zoho_contacts" + STRING(MTIME) + ".txt"
        ipcAccountName                   = REPLACE(ipcAccountName," ","%20")
        ipcAccountName                   = REPLACE(ipcAccountName,"(","")
        ipcAccountName                   = REPLACE(ipcAccountName,")","")
        cCommand                         = SEARCH("curl.exe") + ' "' 
                                         + cAPIURL + '/search?criteria=Account_Name:equals:' + ipcAccountName + '"' 
                                         + ' --insecure -X GET -H "Authorization: Zoho-oauthtoken ' 
                                         + ipcAccessToken + '"'
        .

    /* execute CURL command with required parameters to call the API */
    RUN OS_RunCommand (
        INPUT  cCommand,             /* Command string to run */
        INPUT  cResponseContactsFile, /* File name to write the command output */
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
        OS-DELETE VALUE(cResponseContactsFile).   
        RETURN.
    END.
    
    COPY-LOB FILE cResponseContactsFile TO cResponseContacts.
    
    IF cResponseContacts EQ "" THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "No Data Returned"
            .  
        RETURN.
    END.

    ASSIGN
        oObject         = CAST(oModelParser:Parse(INPUT cResponseContacts),JsonObject)
        jsonData        = oObject:GetJsonArray("data")
        iLengthProperty = jsonData:LENGTH
        NO-ERROR.
        
    DO iCount = 1 TO iLengthProperty:
        ASSIGN
            ContactObj            = jsonData:GetJsonObject(iCount)
            ContactsPropertyNames = ContactObj:getNames()
            NO-ERROR.
            
        CREATE ttCRMContacts.
        DO iCount1 = 1 TO EXTENT(ContactsPropertyNames):
            ContactPropertyName=ContactsPropertyNames[iCount1].
            CASE ContactPropertyName:
                WHEN "First_Name" THEN 
                    ttCRMContacts.crmFirstName = STRING(ContactObj:GetJsonText(ContactPropertyName)) NO-ERROR.
                WHEN "Last_Name" THEN
                    ttCRMContacts.crmLastName = STRING(ContactObj:GetJsonText(ContactPropertyName)) NO-ERROR.
                WHEN "phone" THEN
                    ttCRMContacts.crmPhone = STRING(ContactObj:GetJsonText(ContactPropertyName)) NO-ERROR.
                WHEN "Email" THEN
                    ttCRMContacts.crmEmail = STRING(ContactObj:GetJsonText(ContactPropertyName)) NO-ERROR.
            END CASE.
                
        END.

    END.
    OS-DELETE VALUE(cResponseContactsFile). 
END PROCEDURE.

PROCEDURE Zoho_GetAccounts:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  input access token,customer number output accounts
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAccessToken AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustNo      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttAccounts.
    DEFINE OUTPUT PARAMETER oplSuccess     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cCommand              AS CHARACTER  NO-UNDO.  
    DEFINE VARIABLE cResponseAccountsFile AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cResponseAccounts     AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE cAPIURL               AS CHARACTER  NO-UNDO INITIAL "https://www.zohoapis.com/crm/v2/Accounts". /* API URL */
    DEFINE VARIABLE jsonData              AS JsonArray  NO-UNDO.
    DEFINE VARIABLE AccountsPropertyNames AS CHARACTER  NO-UNDO EXTENT.
    DEFINE VARIABLE iLengthProperty       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE AccountObj            AS JsonObject NO-UNDO.
    DEFINE VARIABLE iCount                AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iCount1               AS INTEGER    NO-UNDO.
    DEFINE VARIABLE AccountPropertyName   AS CHARACTER  NO-UNDO.
    
    ASSIGN
        oplSuccess                       = YES
        FIX-CODEPAGE (cResponseAccounts) = 'utf-8'
        cResponseAccountsFile            = cTempDir + "\zoho_accounts" + STRING(MTIME) + ".txt"
        .
    IF ipcCustNo NE "" THEN
        cCommand = SEARCH("curl.exe") + ' "' 
                 + cAPIURL + '/search?criteria=Ticker_Symbol:equals:' + ipcCustNo + '"' 
                 + ' --insecure -X GET -H "Authorization: Zoho-oauthtoken ' + ipcAccessToken + '"'.
    ELSE
        cCommand = SEARCH("curl.exe") + ' "' 
                 + cAPIURL + '" --insecure -X GET -H "Authorization: Zoho-oauthtoken ' 
                 + ipcAccessToken + '"'.
        
    /* execute CURL command with required parameters to call the API */
    RUN OS_RunCommand (
        INPUT  cCommand,             /* Command string to run */
        INPUT  cResponseAccountsFile, /* File name to write the command output */
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
       OS-DELETE VALUE(cResponseAccountsFile).  
       RETURN.
    END.
    
    COPY-LOB FILE cResponseAccountsFile TO cResponseAccounts.
    
    IF cResponseAccounts EQ "" THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "No Data Returned"
            .  
       RETURN.
    END.
        
    ASSIGN
        oObject         = CAST(oModelParser:Parse(INPUT cResponseAccounts),JsonObject)
        jsonData        = oObject:GetJsonArray("data")
        iLengthProperty = jsonData:LENGTH
        NO-ERROR.
        
    DO iCount = 1 TO iLengthProperty:
        ASSIGN
            AccountObj            = jsonData:GetJsonObject(iCount)
            AccountsPropertyNames = AccountObj:getNames()
            NO-ERROR.
            
        CREATE ttAccounts.
        DO iCount1 = 1 TO EXTENT(AccountsPropertyNames):
            AccountPropertyName=AccountsPropertyNames[iCount1].
            CASE AccountPropertyName:
                WHEN "Account_Name" THEN 
                    ttAccounts.accountName = STRING(AccountObj:GetJsonText(AccountPropertyName)) NO-ERROR.
                WHEN "Ticker_Symbol" THEN
                    ttAccounts.tickerSymbol = STRING(AccountObj:GetJsonText(AccountPropertyName)) NO-ERROR.
            END CASE.
                
        END.

    END.
    OS-DELETE VALUE(cResponseAccountsFile). 
END PROCEDURE.

