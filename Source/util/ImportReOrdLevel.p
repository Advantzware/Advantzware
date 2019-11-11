/*------------------------------------------------------------------------
    File        : ImportCust.p
    Purpose     : 
    Syntax      :
    Description : Import Program (Persistent) for Configuring and Processing the Import for Customer	
    Author(s)   : BV
    Created     : Sun Jan 21:18:38 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportReOrdLevel
    FIELD Company       AS CHARACTER 
    FIELD Location      AS CHARACTER 
    FIELD cFGItem        AS CHARACTER FORMAT "X(15)" COLUMN-LABEL "FG Item" HELP "Required - Size:15" 
    FIELD cLoc           AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Location" HELP "Required - Size:8" 
    FIELD cLocDesc       AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Loc Dscr" HELP "Optional - Size:30"
    FIELD iReorderLevel  AS INTEGER FORMAT ">>>,>>>,>>>.<<<" COLUMN-LABEL "Reorder Level" HELP "Optional - Integer"
    FIELD iOrderMinimum  AS INTEGER FORMAT ">>>,>>>,>>>.<<<" COLUMN-LABEL "Order Minimum" HELP "Optional - Integer"
    FIELD iOrderMaximum  AS INTEGER FORMAT ">>>,>>>,>>>.<<" COLUMN-LABEL "Order Maximum" HELP "Optional - Integer"
    FIELD iLeadTime      AS INTEGER FORMAT ">>>" COLUMN-LABEL "Lead Time (Days)" HELP "Optional - Integer"
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportReOrdLevel"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportReOrdLevel FOR ttImportReOrdLevel.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportReOrdLevel FOR ttImportReOrdLevel.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportReOrdLevel.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportReOrdLevel.cFGItem EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: FG Item".
    END.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportReOrdLevel.cloc EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Location".
    END.
    
    /*IF oplValid THEN 
    DO:
        IF ipbf-ttImportReOrdLevel.dGraceDolr LT 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Grace $ can not be Negative.".
    END.*/
    
    
    IF oplValid THEN 
    DO:
        FIND FIRST itemfg-loc NO-LOCK 
            WHERE itemfg-loc.company EQ ipbf-ttImportReOrdLevel.Company
            AND itemfg-loc.i-no EQ ipbf-ttImportReOrdLevel.cFGItem
            AND itemfg-loc.loc EQ ipbf-ttImportReOrdLevel.cLoc
            NO-ERROR .
        IF AVAILABLE itemfg-loc THEN 
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
    
    /*Field level validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportReOrdLevel.cFGItem NE "" THEN 
            RUN pIsValidFGITemID IN hdValidator (ipbf-ttImportReOrdLevel.cFGItem,YES,ipbf-ttImportReOrdLevel.Company, OUTPUT oplValid, OUTPUT cValidNote).

        
        IF oplValid AND ipbf-ttImportReOrdLevel.Loc NE "" THEN 
            RUN pIsValidWarehouse IN hdValidator (ipbf-ttImportReOrdLevel.cLoc, YES, ipbf-ttImportReOrdLevel.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
    END.
    IF VALID-HANDLE(hdValidator) THEN 
        DELETE OBJECT hdValidator.
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportReOrdLevel FOR ttImportReOrdLevel.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
     
    FIND FIRST itemfg-loc EXCLUSIVE-LOCK
        WHERE itemfg-loc.company EQ ipbf-ttImportReOrdLevel.Company
        AND itemfg-loc.i-no EQ ipbf-ttImportReOrdLevel.cFGItem
        AND itemfg-loc.loc EQ ipbf-ttImportReOrdLevel.cLoc
        NO-ERROR.  
    IF NOT AVAILABLE itemfg-loc THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE itemfg-loc.
        ASSIGN 
            itemfg-loc.company = ipbf-ttImportReOrdLevel.Company
            itemfg-loc.i-no = ipbf-ttImportReOrdLevel.cFGItem
            itemfg-loc.loc = ipbf-ttImportReOrdLevel.cLoc
            .
    END.

    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */
    RUN pAssignValueI (ipbf-ttImportReOrdLevel.iReorderLevel, YES, INPUT-OUTPUT itemfg-loc.ord-level).
    RUN pAssignValueI (ipbf-ttImportReOrdLevel.iOrderMinimum, YES, INPUT-OUTPUT itemfg-loc.ord-min).
    RUN pAssignValueI (ipbf-ttImportReOrdLevel.iOrderMaximum, YES, INPUT-OUTPUT itemfg-loc.ord-max).
    RUN pAssignValueI (ipbf-ttImportReOrdLevel.iLeadTime, YES, INPUT-OUTPUT itemfg-loc.lead-days).
     
    RELEASE itemfg-loc.
    
    
END PROCEDURE.

