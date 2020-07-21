/*------------------------------------------------------------------------
    File        : ImportCarrierMtx.p
    Purpose     : 
    Syntax      :
    Description : Import Program (Persistent) for Configuring and Processing the Import for Carrier	
    Author(s)   : BV
    Created     : Sun Jan 21:18:38 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportCarrierMtx
    FIELD Company      AS CHARACTER 
    FIELD Location     AS CHARACTER
    FIELD carrier      AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Carrier" HELP "Required - Size:8"
    FIELD loc          AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Location" HELP "Required - Size:8"
    FIELD del-zone     AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Zone" HELP "Required - Size:8"
    FIELD del-dscr     AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Description" HELP "Optional - Size:30" 
    FIELD del-zip      AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Zip Code" HELP "Optional - Size:10" 
    FIELD min-rate     AS DECIMAL FORMAT ">>>>9.999" COLUMN-LABEL "Min Rate" HELP "Optional - Decimal" 
    FIELD weight1      AS INTEGER FORMAT ">>>>>>>" COLUMN-LABEL "Qty Up To 1" HELP "Optional - Integer"
    FIELD weight2      AS INTEGER FORMAT ">>>>>>>" COLUMN-LABEL "Qty Up To 2" HELP "Optional - Integer"
    FIELD weight3      AS INTEGER FORMAT ">>>>>>>" COLUMN-LABEL "Qty Up To 3" HELP "Optional - Integer"
    FIELD weight4      AS INTEGER FORMAT ">>>>>>>" COLUMN-LABEL "Qty Up To 4" HELP "Optional - Integer"
    FIELD weight5      AS INTEGER FORMAT ">>>>>>>" COLUMN-LABEL "Qty Up To 5" HELP "Optional - Integer"
    FIELD weight6      AS INTEGER FORMAT ">>>>>>>" COLUMN-LABEL "Qty Up To 6" HELP "Optional - Integer"
    FIELD weight7      AS INTEGER FORMAT ">>>>>>>" COLUMN-LABEL "Qty Up To 7" HELP "Optional - Integer"
    FIELD weight8      AS INTEGER FORMAT ">>>>>>>" COLUMN-LABEL "Qty Up To 8" HELP "Optional - Integer"
    FIELD weight9      AS INTEGER FORMAT ">>>>>>>" COLUMN-LABEL "Qty Up To 9" HELP "Optional - Integer"
    FIELD weight10     AS INTEGER FORMAT ">>>>>>>" COLUMN-LABEL "Qty Up To 10" HELP "Optional - Integer"
    FIELD rate1        AS DECIMAL FORMAT ">>>9.999" COLUMN-LABEL "Rate/Qty 1" HELP "Optional - Decimal"
    FIELD rate2        AS DECIMAL FORMAT ">>>9.999" COLUMN-LABEL "Rate/Qty 2" HELP "Optional - Decimal"
    FIELD rate3        AS DECIMAL FORMAT ">>>9.999" COLUMN-LABEL "Rate/Qty 3" HELP "Optional - Decimal"
    FIELD rate4        AS DECIMAL FORMAT ">>>9.999" COLUMN-LABEL "Rate/Qty 4" HELP "Optional - Decimal"
    FIELD rate5        AS DECIMAL FORMAT ">>>9.999" COLUMN-LABEL "Rate/Qty 5" HELP "Optional - Decimal"
    FIELD rate6        AS DECIMAL FORMAT ">>>9.999" COLUMN-LABEL "Rate/Qty 6" HELP "Optional - Decimal"
    FIELD rate7        AS DECIMAL FORMAT ">>>9.999" COLUMN-LABEL "Rate/Qty 7" HELP "Optional - Decimal"
    FIELD rate8        AS DECIMAL FORMAT ">>>9.999" COLUMN-LABEL "Rate/Qty 8" HELP "Optional - Decimal"
    FIELD rate9        AS DECIMAL FORMAT ">>>9.999" COLUMN-LABEL "Rate/Qty 9" HELP "Optional - Decimal"
    FIELD rate10       AS DECIMAL FORMAT ">>>9.999" COLUMN-LABEL "Rate/Qty 10" HELP "Optional - Decimal"
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportCarrierMtx"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportCarrierMtx FOR ttImportCarrierMtx.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportCarrierMtx FOR ttImportCarrierMtx.
    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCarrierMtx.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCarrierMtx.Carrier EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Carrier".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCarrierMtx.del-zone EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Zone".
    END.
    
            /*Field level validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportCarrierMtx.carrier NE "" THEN 
            RUN pIsValidCarrier (ipbf-ttImportCarrierMtx.carrier, YES, ipbf-ttImportCarrierMtx.company, OUTPUT oplValid, OUTPUT cValidNote).
        IF oplValid THEN 
            RUN pIsValidWarehouse(
                INPUT ipbf-ttImportCarrierMtx.loc,
                INPUT YES,
                INPUT ipbf-ttImportCarrierMtx.company,
                OUTPUT oplvalid,
                OUTPUT cValidNote
                ). 
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    
    IF oplValid THEN DO:
        FIND FIRST carr-mtx NO-LOCK
             WHERE carr-mtx.company  EQ ttImportCarrierMtx.Company
               AND carr-mtx.carrier  EQ ttImportCarrierMtx.carrier
               AND carr-mtx.del-zone EQ ttImportCarrierMtx.del-zone
             NO-ERROR. 
        IF AVAILABLE carr-mtx THEN DO:
            IF NOT iplUpdateDuplicates THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote = "Duplicate Exists - Will be skipped"
                    .
            ELSE 
                opcNote = "Update record - All fields to be overwritten".        
        END. 
        ELSE 
            opcNote = "Add Record".             
    END.    
        
END PROCEDURE.
  
PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportCarrierMtx FOR ttImportCarrierMtx.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-carr-mtx FOR carr-mtx.
     
    FIND FIRST bf-carr-mtx EXCLUSIVE-LOCK
        WHERE bf-carr-mtx.company EQ ipbf-ttImportCarrierMtx.Company
        AND bf-carr-mtx.carrier EQ ipbf-ttImportCarrierMtx.carrier
        AND bf-carr-mtx.del-zone EQ ipbf-ttImportCarrierMtx.del-zone
        NO-ERROR.  
    IF NOT AVAILABLE bf-carr-mtx THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE bf-carr-mtx.
        ASSIGN 
            bf-carr-mtx.company = ipbf-ttImportCarrierMtx.Company
            bf-carr-mtx.carrier = ipbf-ttImportCarrierMtx.carrier
            bf-carr-mtx.del-zone = ipbf-ttImportCarrierMtx.del-zone .
    END.
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */
    RUN pAssignValueC (ipbf-ttImportCarrierMtx.carrier, iplIgnoreBlanks, INPUT-OUTPUT bf-carr-mtx.carrier).
    RUN pAssignValueC (ipbf-ttImportCarrierMtx.del-zone, iplIgnoreBlanks, INPUT-OUTPUT bf-carr-mtx.del-zone).
    RUN pAssignValueC (ipbf-ttImportCarrierMtx.del-dscr, iplIgnoreBlanks, INPUT-OUTPUT bf-carr-mtx.del-dscr).
    RUN pAssignValueC (ipbf-ttImportCarrierMtx.del-zip, iplIgnoreBlanks, INPUT-OUTPUT bf-carr-mtx.del-zip).
    RUN pAssignValueD (ipbf-ttImportCarrierMtx.min-rate, YES, INPUT-OUTPUT bf-carr-mtx.min-rate).

    RUN pAssignValueI (ipbf-ttImportCarrierMtx.weight1, YES, INPUT-OUTPUT bf-carr-mtx.weight[1]).
    RUN pAssignValueI (ipbf-ttImportCarrierMtx.weight2, YES, INPUT-OUTPUT bf-carr-mtx.weight[2]).
    RUN pAssignValueI (ipbf-ttImportCarrierMtx.weight3, YES, INPUT-OUTPUT bf-carr-mtx.weight[3]).
    RUN pAssignValueI (ipbf-ttImportCarrierMtx.weight4, YES, INPUT-OUTPUT bf-carr-mtx.weight[4]).
    RUN pAssignValueI (ipbf-ttImportCarrierMtx.weight5, YES, INPUT-OUTPUT bf-carr-mtx.weight[5]).
    RUN pAssignValueI (ipbf-ttImportCarrierMtx.weight6, YES, INPUT-OUTPUT bf-carr-mtx.weight[6]).
    RUN pAssignValueI (ipbf-ttImportCarrierMtx.weight7, YES, INPUT-OUTPUT bf-carr-mtx.weight[7]).
    RUN pAssignValueI (ipbf-ttImportCarrierMtx.weight8, YES, INPUT-OUTPUT bf-carr-mtx.weight[8]).
    RUN pAssignValueI (ipbf-ttImportCarrierMtx.weight9, YES, INPUT-OUTPUT bf-carr-mtx.weight[9]).
    RUN pAssignValueI (ipbf-ttImportCarrierMtx.weight10, YES, INPUT-OUTPUT bf-carr-mtx.weight[10]).

    RUN pAssignValueD (ipbf-ttImportCarrierMtx.rate1, YES, INPUT-OUTPUT bf-carr-mtx.rate[1]).
    RUN pAssignValueD (ipbf-ttImportCarrierMtx.rate2, YES, INPUT-OUTPUT bf-carr-mtx.rate[2]).
    RUN pAssignValueD (ipbf-ttImportCarrierMtx.rate3, YES, INPUT-OUTPUT bf-carr-mtx.rate[3]).
    RUN pAssignValueD (ipbf-ttImportCarrierMtx.rate4, YES, INPUT-OUTPUT bf-carr-mtx.rate[4]).
    RUN pAssignValueD (ipbf-ttImportCarrierMtx.rate5, YES, INPUT-OUTPUT bf-carr-mtx.rate[5]).
    RUN pAssignValueD (ipbf-ttImportCarrierMtx.rate6, YES, INPUT-OUTPUT bf-carr-mtx.rate[6]).
    RUN pAssignValueD (ipbf-ttImportCarrierMtx.rate7, YES, INPUT-OUTPUT bf-carr-mtx.rate[7]).
    RUN pAssignValueD (ipbf-ttImportCarrierMtx.rate8, YES, INPUT-OUTPUT bf-carr-mtx.rate[8]).
    RUN pAssignValueD (ipbf-ttImportCarrierMtx.rate9, YES, INPUT-OUTPUT bf-carr-mtx.rate[9]).
    RUN pAssignValueD (ipbf-ttImportCarrierMtx.rate10, YES, INPUT-OUTPUT bf-carr-mtx.rate[10]).
    
    RELEASE bf-carr-mtx.
    
END PROCEDURE.
