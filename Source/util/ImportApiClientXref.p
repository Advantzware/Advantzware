
/*------------------------------------------------------------------------
    File        : ImportapiClientXref.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Prep and Die	

    Author(s)   : Sewa Singh
    Created     : Tue April 21 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportapiClientXref
    FIELD Company                 AS CHARACTER 
    FIELD Location                AS CHARACTER 
    FIELD apicompany              AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Company" HELP "Required - Size:3" 
    FIELD scopeType               AS CHARACTER FORMAT "x(12)" COLUMN-LABEL "Scope Type" HELP "Required - Size:12"
    FIELD scopeID                 AS CHARACTER FORMAT "x(12)" COLUMN-LABEL "Scope ID" HELP "Required -  Size:12"
    FIELD clientID                AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Client ID" HELP "Optional - Size:8"
    
    
    .
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data*/
 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
 /*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportapiClientXref"}


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportapiClientXref FOR ttImportapiClientXref.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE riNote AS ROWID NO-UNDO.
    DEFINE BUFFER bf-apiClientXref FOR apiClientXref.

    FIND FIRST bf-apiClientXref EXCLUSIVE-LOCK 
        WHERE bf-apiClientXref.company EQ ipbf-ttImportapiClientXref.apicompany
        AND bf-apiClientXref.scopeType EQ ipbf-ttImportapiClientXref.scopeType
        AND bf-apiClientXref.scopeID EQ ipbf-ttImportapiClientXref.scopeID
        NO-ERROR.

    IF NOT AVAILABLE bf-apiClientXref THEN 
    DO:
        ASSIGN 
            iopiAdded = iopiAdded + 1.
        CREATE bf-apiClientXref.
        /*{custom/rec_key.i bf-apiClientXref}*/
        bf-apiClientXref.rec_key = DYNAMIC-FUNCTION("sfGetNextRecKey").
        ASSIGN 
            bf-apiClientXref.company   = ipbf-ttImportapiClientXref.apicompany
             .
    END.
                                                                                                                                     
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */                                        
    RUN pAssignValueC (ipbf-ttImportapiClientXref.scopeType, iplIgnoreBlanks, INPUT-OUTPUT bf-apiClientXref.scopeType).                                                   
    RUN pAssignValueC (ipbf-ttImportapiClientXref.scopeID, iplIgnoreBlanks, INPUT-OUTPUT bf-apiClientXref.scopeID).                                                   
    RUN pAssignValueC (ipbf-ttImportapiClientXref.clientID, iplIgnoreBlanks, INPUT-OUTPUT bf-apiClientXref.clientID).                               
    
   RELEASE bf-apiClientXref .
                                                                                                                               
                                                                                                                               
END PROCEDURE.                                                                                                                 
                                                                                                                               
PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportapiClientXref FOR ttImportapiClientXref.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportapiClientXref FOR ttImportapiClientXref.


    oplValid = YES.
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportapiClientXref.scopeType EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Scope Type is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportapiClientXref.scopeID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Scope Id is Blank".
    END.
    
    /*Check for Duplicate Import Record and Ignore It*/ 
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportapiClientXref NO-LOCK 
            WHERE bf-ttImportapiClientXref.Company EQ ipbf-ttImportapiClientXref.apicompany
            AND bf-ttImportapiClientXref.scopeType EQ ipbf-ttImportapiClientXref.scopeType
            AND bf-ttImportapiClientXref.scopeID EQ ipbf-ttImportapiClientXref.scopeID
            AND ROWID(bf-ttImportapiClientXref) NE ROWID(ipbf-ttImportapiClientXref)
            NO-ERROR.
        IF AVAILABLE bf-ttImportapiClientXref THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    /*Determine if Add or Update*/
    IF oplValid THEN 
    DO:
        FIND FIRST apiClientXref NO-LOCK 
            WHERE apiClientXref.company EQ ipbf-ttImportapiClientXref.apicompany
            AND apiClientXref.scopeType EQ ipbf-ttImportapiClientXref.scopeType
            AND apiClientXref.scopeID EQ ipbf-ttImportapiClientXref.scopeID
            NO-ERROR .
        IF AVAIL apiClientXref THEN
        DO: 
            IF NOT iplUpdateDuplicates THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Duplicate record exists"
                    .
            ELSE
                ASSIGN 
                    oplValid = YES
                    opcNote = "Update existing record"
                    .        
        END.
        ELSE 
            ASSIGN 
                oplValid = YES
                opcNote = "Add record"
                .
        
    END.
    
    /*Field Level Validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:
        
        IF oplValid AND ipbf-ttImportapiClientXref.scopeType NE "" THEN 
            RUN pIsValidFromList ("Scope Type", ipbf-ttImportapiClientXref.scopeType, "Customer,ShipTo,Vendor", OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportapiClientXref.scopeType EQ "Vendor" AND ipbf-ttImportapiClientXref.scopeID NE "" THEN 
            RUN pIsValidVendor (ipbf-ttImportapiClientXref.scopeID, NO, ipbf-ttImportapiClientXref.apicompany, OUTPUT oplValid, OUTPUT cValidNote).
                     
        IF oplValid AND ipbf-ttImportapiClientXref.scopeType EQ "Customer" AND ipbf-ttImportapiClientXref.scopeID NE "" THEN 
            RUN pIsValidCustomerID (ipbf-ttImportapiClientXref.scopeID, NO, ipbf-ttImportapiClientXref.apicompany, OUTPUT oplValid, OUTPUT cValidNote).
        
        
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
END PROCEDURE.

