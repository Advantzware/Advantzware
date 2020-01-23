/*------------------------------------------------------------------------
    File        : ImportGlJrn.p
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

DEFINE TEMP-TABLE ttImportGlJrn
    FIELD Company       AS CHARACTER 
    FIELD Location      AS CHARACTER
    FIELD journal       AS INTEGER FORMAT ">>>>>>>>" COLUMN-LABEL "Id" HELP "Required - Integer"
    FIELD tr-date       AS CHARACTER FORMAT "X(12)" COLUMN-LABEL "Date" HELP "Required - Date" 
    FIELD reverse       AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Location" HELP "Optional - Yes or No (Blank No)" 
    FIELD iline         AS INTEGER FORMAT ">>>" COLUMN-LABEL "Line" HELP "Required - Integer" 
    FIELD actnum        AS CHARACTER FORMAT "X(25)" COLUMN-LABEL "Account Number" HELP "Required - Size:25"
    FIELD dscr          AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Dscription" HELP "Optional - Size:30"
    FIELD tr-amt        AS DECIMAL FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Amount" HELP "Required - Decimal"
    
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/
DEFINE VARIABLE iCheckJournal AS INTEGER NO-UNDO .
DEFINE VARIABLE iJournalNo AS INTEGER NO-UNDO .
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportGlJrn"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportGlJrn FOR ttImportGlJrn.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportGlJrn FOR ttImportGlJrn.
    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGlJrn.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGlJrn.journal EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Journal".
    END.
    
    
    IF oplValid THEN 
    DO:
        IF date(ipbf-ttImportGlJrn.tr-date) EQ ? THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Invalid Date".
    END.
  
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGlJrn.actnum EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Account Number.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGlJrn.tr-amt EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Zero: Amount.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGlJrn.iLine LE 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Zero: Line.".
    END.
        /*Field level validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportGlJrn.actnum NE "" THEN 
            RUN pIsValidGLAccount  (ipbf-ttImportGlJrn.actnum, NO, ipbf-ttImportGlJrn.Company, OUTPUT oplValid, OUTPUT cValidNote).
      
       
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
   
END PROCEDURE.
  
PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportGlJrn FOR ttImportGlJrn.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    
     IF iCheckJournal EQ 0 THEN
         iCheckJournal = ipbf-ttImportGlJrn.journal.

    IF iCheckJournal NE ipbf-ttImportGlJrn.journal OR iJournalNo EQ 0 THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE gl-jrn.
        ASSIGN 
            gl-jrn.reverse = IF  ipbf-ttImportGlJrn.reverse EQ "Yes" THEN YES ELSE NO .
            gl-jrn.tr-date = date(ipbf-ttImportGlJrn.tr-date)                         .
            gl-jrn.company = ipbf-ttImportGlJrn.company                               .
            gl-jrn.period  = month(date(ipbf-ttImportGlJrn.tr-date))                   .
            gl-jrn.yr  = YEAR(date(ipbf-ttImportGlJrn.tr-date))                             .
            gl-jrn.recur   = NO                                                       .
            gl-jrn.from-reverse = NO                                                  .
            iJournalNo = gl-jrn.j-no                                                  .
            iCheckJournal = ipbf-ttImportGlJrn.journal.
    END.

    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */

    CREATE gl-jrnl.
    ASSIGN gl-jrnl.j-no = iJournalNo             .
           RUN pAssignValueI (ipbf-ttImportGlJrn.iLINE, iplIgnoreBlanks, INPUT-OUTPUT gl-jrnl.line).
           RUN pAssignValueC (ipbf-ttImportGlJrn.actnum, iplIgnoreBlanks, INPUT-OUTPUT gl-jrnl.actnum).
           RUN pAssignValueC (ipbf-ttImportGlJrn.dscr, iplIgnoreBlanks, INPUT-OUTPUT gl-jrnl.dscr).
           RUN pAssignValueD (ipbf-ttImportGlJrn.tr-amt, iplIgnoreBlanks, INPUT-OUTPUT gl-jrnl.tr-amt).

           IF gl-jrnl.tr-amt GT 0 THEN
               gl-jrn.tdeb = gl-jrn.tdeb  + gl-jrnl.tr-amt.
           ELSE
               gl-jrn.tcred = gl-jrn.tcred + gl-jrnl.tr-amt.

           gl-jrn.tr-amt = gl-jrn.tdeb + gl-jrn.tcred.
    
END PROCEDURE.
