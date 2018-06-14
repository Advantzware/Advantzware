
/*------------------------------------------------------------------------
    File        : ImportGL.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for GL Accounts	

    Author(s)   : BV
    Created     : Fri Nov 24 16:18:38 EST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportGL
    FIELD Company             AS CHARACTER 
    FIELD Location            AS CHARACTER 
    FIELD AccountNo           AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Account #" HELP "Required - Size:30"
    FIELD AccountDesc         AS CHARACTER FORMAT "x(50)" COLUMN-LABEL "Description" HELP "Optional - Size:50"
    FIELD AccountType         AS CHARACTER FORMAT "x(1)" COLUMN-LABEL "Type" HELP "Required - Size:1"
        .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportGL"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportGL FOR ttImportGL.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportGL FOR ttImportGL.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    
    ipbf-ttImportGL.AccountType = SUBSTRING(TRIM(ipbf-ttImportGL.AccountType),1,1).
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGL.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGL.AccountNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Account #".
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportGL NO-LOCK 
            WHERE bf-ttImportGL.Company EQ ipbf-ttImportGL.Company
            AND bf-ttImportGL.AccountNo EQ ipbf-ttImportGL.AccountNo
            AND ROWID(bf-ttImportGL) NE ROWID(ipbf-ttImportGL)
            NO-ERROR.
        IF AVAILABLE bf-ttImportGL THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST account NO-LOCK 
            WHERE account.company EQ ipbf-ttImportGL.Company
            AND account.actnum EQ ipbf-ttImportGL.AccountNo
            NO-ERROR .
        IF AVAILABLE account THEN 
        DO:
            IF NOT iplUpdateDuplicates THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Duplicate Exists:  Will be skipped"
                    .
            ELSE
                ASSIGN 
                    opcNote = "Update record - All fields to be overwritten"
                    .        
        END.
        ELSE 
            ASSIGN 
                opcNote = "Add record"
                .
        
    END.
    IF oplValid AND iplFieldValidation THEN 
        RUN pIsValidFromList IN hdValidator ("Account Type", ipbf-ttImportGL.AccountType, "A,C,E,L,R,T", OUTPUT oplValid, OUTPUT cValidNote).
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportGL FOR ttImportGL.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.

    FIND FIRST account EXCLUSIVE-LOCK 
        WHERE account.company EQ ipbf-ttImportGL.Company
        AND account.actnum EQ ipbf-ttImportGL.AccountNo
        NO-ERROR.
    IF NOT AVAILABLE account THEN DO:
        iopiAdded = iopiAdded + 1.
        CREATE account.
        ASSIGN
            account.company = ipbf-ttImportGL.Company
            account.actnum = ipbf-ttImportGL.AccountNo
            account.type = "A". 
    END.
    RUN pAssignValueC (ipbf-ttImportGL.AccountDesc, iplIgnoreBlanks, INPUT-OUTPUT account.dscr).
    RUN pAssignValueC (ipbf-ttImportGL.AccountType, YES, INPUT-OUTPUT account.type).    

END PROCEDURE.

