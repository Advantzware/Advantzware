/*------------------------------------------------------------------------
    File        : ImportGlTrans.p
    Purpose     : 
    Syntax      :
    Description : Import Program (Persistent) for Configuring and Processing the Import for Carrier	
    Author(s)   : Sewa Singh
    Created     : Fri Jan 21:18:38 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportGlTrans
    FIELD Company       AS CHARACTER 
    FIELD Location      AS CHARACTER
    FIELD trnum         AS INTEGER FORMAT ">>>>>" COLUMN-LABEL "Run#" HELP "Required - Integer"
    FIELD tr-date       AS CHARACTER FORMAT "X(12)" COLUMN-LABEL "Date" HELP "Required - Date" 
    FIELD actnum        AS CHARACTER FORMAT "X(25)" COLUMN-LABEL "Account Number" HELP "Required - Size:25" 
    FIELD tr-dscr       AS CHARACTER FORMAT "x(35)" COLUMN-LABEL "Dscription" HELP "Optional - Size:35" 
    FIELD jrnl          AS CHARACTER FORMAT "X(25)" COLUMN-LABEL "Journal" HELP "Required - Size:8"
    FIELD yr            AS INTEGER FORMAT "9999" COLUMN-LABEL "Year" HELP "Required - Integer"
    FIELD period        AS INTEGER FORMAT "99" COLUMN-LABEL "Period" HELP "Required - Integer"    
    FIELD tr-amt        AS DECIMAL FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Amount" HELP "Required - Decimal"
    
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/
DEFINE VARIABLE iCheckJournal AS INTEGER NO-UNDO .
DEFINE VARIABLE iJournalNo AS INTEGER NO-UNDO .
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportGlTrans"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportGlTrans FOR ttImportGlTrans.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportGlTrans FOR ttImportGlTrans.
    DEFINE BUFFER bf-gltrans FOR gltrans .
    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGlTrans.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGlTrans.jrnl EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Journal".
    END.
    
    
    IF oplValid THEN 
    DO:
        IF date(ipbf-ttImportGlTrans.tr-date) EQ ? THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Invalid Date".
    END.
  
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGlTrans.actnum EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Account Number.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGlTrans.tr-amt EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Zero: Amount.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGlTrans.trnum LE 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Zero: run.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGlTrans.period LE 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Zero: Period.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGlTrans.yr LE 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Zero: Year.".
    END.
        /*Field level validation*/
    /*Check for Duplicate Import Record and Ignore It*/ 
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportGlTrans NO-LOCK 
            WHERE bf-ttImportGlTrans.Company EQ ipbf-ttImportGlTrans.Company
            AND bf-ttImportGlTrans.trnum EQ ipbf-ttImportGlTrans.trnum
            AND ROWID(bf-ttImportGlTrans) NE ROWID(ipbf-ttImportGlTrans)
            NO-ERROR.
        IF AVAILABLE bf-ttImportGlTrans THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST bf-gltrans NO-LOCK 
            WHERE bf-gltrans.Company EQ ipbf-ttImportGlTrans.Company
            AND bf-gltrans.trnum EQ ipbf-ttImportGlTrans.trnum
            NO-ERROR.
        IF NOT AVAILABLE bf-gltrans THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Run Tranaction Record Not Found"
                .
        ELSE
            ASSIGN 
                oplValid = YES
                opcNote = "Update existing record".

    END.

    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportGlTrans.actnum NE "" THEN 
            RUN pIsValidGLAccount  (ipbf-ttImportGlTrans.actnum, NO, ipbf-ttImportGlTrans.Company, OUTPUT oplValid, OUTPUT cValidNote).
      
       
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
   
END PROCEDURE.
  
PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportGlTrans FOR ttImportGlTrans.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    
     FIND FIRST gltrans EXCLUSIVE-LOCK 
            WHERE gltrans.Company EQ ipbf-ttImportGlTrans.Company
            AND gltrans.trnum EQ ipbf-ttImportGlTrans.trnum
            NO-ERROR.
     IF AVAIL gltrans THEN do: 
           RUN pAssignValueCToDt (ipbf-ttImportGlTrans.tr-date, iplIgnoreBlanks, INPUT-OUTPUT gltrans.tr-date).
           RUN pAssignValueC (ipbf-ttImportGlTrans.actnum, iplIgnoreBlanks, INPUT-OUTPUT gltrans.actnum).
           RUN pAssignValueC (ipbf-ttImportGlTrans.tr-dscr, iplIgnoreBlanks, INPUT-OUTPUT gltrans.tr-dscr).
           RUN pAssignValueC (ipbf-ttImportGlTrans.jrnl, iplIgnoreBlanks, INPUT-OUTPUT gltrans.jrnl).
           RUN pAssignValueI (ipbf-ttImportGlTrans.yr, iplIgnoreBlanks, INPUT-OUTPUT gltrans.yr).
           RUN pAssignValueI (ipbf-ttImportGlTrans.period, iplIgnoreBlanks, INPUT-OUTPUT gltrans.period).
           RUN pAssignValueD (ipbf-ttImportGlTrans.tr-amt, iplIgnoreBlanks, INPUT-OUTPUT gltrans.tr-amt).
     END.
    
END PROCEDURE.


