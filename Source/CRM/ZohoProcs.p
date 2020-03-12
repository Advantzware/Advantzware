
/*------------------------------------------------------------------------
    File        : ZohoProcs
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

DEFINE VARIABLE hdOSProcs      AS HANDLE            NO-UNDO.
DEFINE VARIABLE oModelParser   AS ObjectModelParser NO-UNDO.
DEFINE VARIABLE oObject        AS JsonObject        NO-UNDO.
DEFINE VARIABLE hdFileSysProcs AS HANDLE            NO-UNDO.
DEFINE VARIABLE cTempDir       AS CHARACTER         NO-UNDO.

RUN system/OSProcs.p PERSISTENT SET hdOSProcs.
RUN system/FileSysProcs.p PERSISTENT SET hdFileSysProcs.

RUN FileSys_GetTempDirectory IN hdFileSysProcs(
    OUTPUT cTempDir
    ).
    
oModelParser = NEW ObjectModelParser().
               
PROCEDURE pGetRefreshToken:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  input company, output refresh token
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRefreshToken AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.

    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "ZohoRefreshToken", 
        INPUT "C", 
        INPUT NO, 
        INPUT NO, 
        INPUT "", 
        INPUT "",
        OUTPUT opcRefreshToken, 
        OUTPUT lFound
        ).
    RETURN opcRefreshToken.
END.

PROCEDURE pGetClientID:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  input company, output clientid
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcClientID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.

    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "ZohoClientID", 
        INPUT "C", 
        INPUT NO, 
        INPUT NO, 
        INPUT "", 
        INPUT "",
        OUTPUT opcClientID, 
        OUTPUT lFound
        ).
    RETURN opcClientID.
END.

PROCEDURE pGetClientSecret:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  input company, output clientsecret
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcClientSecret AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.

    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "ZohoClientSecret", 
        INPUT "C", 
        INPUT NO, 
        INPUT NO, 
        INPUT "", 
        INPUT "",
        OUTPUT opcClientSecret, 
        OUTPUT lFound
        ).
        
    RETURN opcClientSecret.
END.

PROCEDURE pGetAccessToken:
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
        cResponseFile            = cTempDir + "\output.txt"
        FIX-CODEPAGE (cResponse) = 'utf-8'
        cCommand                 = SEARCH("curl.exe") + ' -X POST "' + 
                                   cAPIURL + '?refresh_token=' + ipcRefreshToken + 
                                   '^&client_id=' + ipcClientID + 
                                   '^&client_secret=' + ipcClientSecret + 
                                   '^&scope=' + cScope + 
                                   '^&redirect_uri=' + cReturnURI + 
                                   '^&grant_type=' + cGrantType + '"'
        .

    /* execute CURL command with required parameters to call the API */
    RUN OS_RunCommand IN hdOSProcs (
        INPUT  cCommand,             /* Command string to run */
        INPUT  cResponseFile,        /* File name to write the command output */
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
            
       OS-DELETE VALUE(cResponseFile).             
       RETURN.
    END.
    
    COPY-LOB FILE cResponseFile TO cResponse.
    
    IF cResponse EQ "" THEN 
        RETURN.
   
    ASSIGN
        oObject        = CAST(oModelParser:Parse(INPUT cResponse),JsonObject).
        opcAccessToken = oObject:GetJsonText('access_token')
        .
        
    OS-DELETE VALUE(cResponseFile).
END PROCEDURE.

PROCEDURE pGetCustomers:
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
        cResponseCustomersFile           = cTempDir + "\outputcustomers.txt"
        cCommand                         = SEARCH("curl.exe") + ' "' + 
                                           cAPIURL + '" -X GET -H "Authorization: Zoho-oauthtoken ' + 
                                           ipcAccessToken + '"'
        .

    /* execute CURL command with required parameters to call the API */
    RUN OS_RunCommand IN hdOSProcs (
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
        .

    DO iCount = 1 TO iLengthProperty:
        ASSIGN
            CustomerObj            = jsonData:GetJsonObject(iCount)
            CustomersPropertyNames = CustomerObj:getNames()
            .

        CREATE ttCRMCustomers.
        DO iCount1 = 1 TO EXTENT(CustomersPropertyNames):
            CustomerPropertyName=CustomersPropertyNames[iCount1].
            CASE CustomerPropertyName:
                WHEN "Account_Name" THEN
                    ttCRMCustomers.crmName = STRING(CustomerObj:GetJsonText(CustomerPropertyName)).
                WHEN "Billing_City" THEN
                    ttCRMCustomers.crmCity = STRING(CustomerObj:GetJsonText(CustomerPropertyName)).
                WHEN "Billing_State" THEN
                    ttCRMCustomers.crmState = STRING(CustomerObj:GetJsonText(CustomerPropertyName)).
                WHEN "Billing_Code" THEN
                    ttCRMCustomers.crmCode = STRING(CustomerObj:GetJsonText(CustomerPropertyName)).
                WHEN "Billing_Street" THEN
                    ttCRMCustomers.crmStreet = STRING(CustomerObj:GetJsonText(CustomerPropertyName)).
                WHEN "Billing Street 2" THEN
                    ttCRMCustomers.crmStreet2 = STRING(CustomerObj:GetJsonText(CustomerPropertyName)).
                WHEN "Phone" THEN
                    ttCRMCustomers.crmPhone = STRING(CustomerObj:GetJsonText(CustomerPropertyName)).
                WHEN "Ticker_Symbol" THEN
                    ttCRMCustomers.tickerSymbol = STRING(CustomerObj:GetJsonText(CustomerPropertyName)).
            END CASE.

        END.

    END.
    OS-DELETE VALUE(cResponseCustomersFile). 
END PROCEDURE.

PROCEDURE pGetContacts:
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
        cResponseContactsFile            = cTempDir + "\outputcontacts.txt"
        ipcAccountName                   = REPLACE(ipcAccountName," ","%20")
        ipcAccountName                   = REPLACE(ipcAccountName,"(","")
        ipcAccountName                   = REPLACE(ipcAccountName,")","")
        cCommand                         = SEARCH("curl.exe") + ' "' +
                                           cAPIURL + '/search?criteria=Account_Name:equals:' + ipcAccountName + '"' +
                                           ' -X GET -H "Authorization: Zoho-oauthtoken ' + 
                                           ipcAccessToken + '"'
        .

    /* execute CURL command with required parameters to call the API */
    RUN OS_RunCommand IN hdOSProcs (
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
        .
        
    DO iCount = 1 TO iLengthProperty:
        ASSIGN
            ContactObj            = jsonData:GetJsonObject(iCount)
            ContactsPropertyNames = ContactObj:getNames()
            .
            
        CREATE ttCRMContacts.
        DO iCount1 = 1 TO EXTENT(ContactsPropertyNames):
            ContactPropertyName=ContactsPropertyNames[iCount1].
            CASE ContactPropertyName:
                WHEN "First_Name" THEN 
                    ttCRMContacts.crmFirstName = STRING(ContactObj:GetJsonText(ContactPropertyName)).
                WHEN "Last_Name" THEN
                    ttCRMContacts.crmLastName = STRING(ContactObj:GetJsonText(ContactPropertyName)).
                WHEN "phone" THEN
                    ttCRMContacts.crmPhone = STRING(ContactObj:GetJsonText(ContactPropertyName)).
                WHEN "Email" THEN
                    ttCRMContacts.crmEmail = STRING(ContactObj:GetJsonText(ContactPropertyName)).
            END CASE.
                
        END.

    END.
    OS-DELETE VALUE(cResponseContactsFile). 
END PROCEDURE.

PROCEDURE pGetAccounts:
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
        cResponseAccountsFile            = cTempDir + "\outputaccounts.txt"
        .
    IF ipcCustNo NE "" THEN
        cCommand = SEARCH("curl.exe") + ' "' + 
                   cAPIURL + '/search?criteria=Ticker_Symbol:equals:' + ipcCustNo + '"' + 
                   ' -X GET -H "Authorization: Zoho-oauthtoken ' + ipcAccessToken + '"'.
    ELSE
        cCommand = SEARCH("curl.exe") + ' "' + 
                   cAPIURL + '" -X GET -H "Authorization: Zoho-oauthtoken ' + 
                   ipcAccessToken + '"'.
        
    /* execute CURL command with required parameters to call the API */
    RUN OS_RunCommand IN hdOSProcs (
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
        .
        
    DO iCount = 1 TO iLengthProperty:
        ASSIGN
            AccountObj            = jsonData:GetJsonObject(iCount)
            AccountsPropertyNames = AccountObj:getNames()
            .
            
        CREATE ttAccounts.
        DO iCount1 = 1 TO EXTENT(AccountsPropertyNames):
            AccountPropertyName=AccountsPropertyNames[iCount1].
            CASE AccountPropertyName:
                WHEN "Account_Name" THEN 
                    ttAccounts.accountName = STRING(AccountObj:GetJsonText(AccountPropertyName)).
                WHEN "Ticker_Symbol" THEN
                    ttAccounts.tickerSymbol = STRING(AccountObj:GetJsonText(AccountPropertyName)).
            END CASE.
                
        END.

    END.
    OS-DELETE VALUE(cResponseAccountsFile). 
END PROCEDURE.




    








