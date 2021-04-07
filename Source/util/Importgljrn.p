
/*------------------------------------------------------------------------
    File        : Importgljrn.p
    Purpose     : 

    Syntax      :

    Description : Import program for Genreal ledger

    Author(s)   : Goutam Sharma
    Created     : Tue Mar 09 08:33:24 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/
 
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportgljrn NO-UNDO
    FIELD Company         AS CHARACTER FORMAT "x(3)"
    FIELD Location        AS CHARACTER FORMAT "x(5)"
    FIELD identifier      AS CHARACTER FORMAT "x(10)" LABEL "Identifier"
    FIELD Date            AS DATE                     LABEL "Date"
    FIELD actnum          AS CHARACTER                LABEL "Account Number"
    FIELD dscr            AS CHARACTER                LABEL "Description"
    FIELD tr-amt          AS CHARACTER                LABEL "Transaction Amount"
    FIELD reverse         AS LOGICAL                  LABEL "Reverse"
    FIELD j-no            AS INTEGER                  LABEL "Journal Number"
    FIELD period          AS INTEGER                  LABEL "Period"
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/
DEFINE VARIABLE iPeriod       AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportgljrn"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportgljrn FOR ttImportgljrn.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportgljrn FOR ttImportgljrn.

    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportgljrn.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportgljrn.Date EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Date ".
    END.
    
    IF oplValid THEN
    DO:
        IF CAN-FIND(FIRST account 
                    WHERE account.company EQ ipbf-ttImportgljrn.Company 
                    AND account.type    NE "T" 
                    AND account.actnum  EQ ipbf-ttImportgljrn.actnum) THEN
    
        ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
        
    END.
    
    IF oplValid THEN
    DO:
        FIND FIRST period 
             WHERE period.company  =  ipbf-ttImportgljrn.Company  
               AND period.pst  <= ipbf-ttImportgljrn.Date 
               AND period.pend >= ipbf-ttImportgljrn.Date
               NO-LOCK NO-ERROR.
        IF NOT (AVAIL period AND period.pstat) THEN
        DO:
            ASSIGN 
                oplValid = NO
                opcNote  = "Period is not OPEN !".
        END.
        
    END.
    
END PROCEDURE.

PROCEDURE pCreateNewCashHeader:
    /*------------------------------------------------------------------------------
     Purpose: Creates a new AR Cash Header, setting defaults based on key values
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdDate        AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipiperiod      AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iplreverse     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oprigljrn     AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER opiJournalNo  AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE iNextCNo AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-gl-jrn FOR gl-jrn.
    
    CREATE gl-jrn.
    ASSIGN
        gl-jrn.reverse = iplreverse
        gl-jrn.tr-date = ipdDate
        gl-jrn.company = ipcCompany
        gl-jrn.period  = ipiperiod
        gl-jrn.recur   = NO
        gl-jrn.from-reverse = NO
        oprigljrn    = ROWID(gl-jrn)  
        opiJournalNo = gl-jrn.j-no.
        .
   
    RELEASE gl-jrn.

END PROCEDURE.

PROCEDURE pCreateNewCashLine:
    /*------------------------------------------------------------------------------
        Purpose: Creates a new line, setting defaults based on key values
        Notes:
       ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprigljrn         AS ROWID.
    DEFINE INPUT  PARAMETER ipiJournal-no     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcActNum         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDescription    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdTr-Amount      AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oprigljrnl        AS ROWID.
    
    DEFINE VARIABLE iNextLine AS INTEGER   NO-UNDO.
    DEFINE BUFFER bf-gl-jrnl FOR gl-jrnl.
    
    FIND gl-jrn NO-LOCK 
        WHERE ROWID(gl-jrn) EQ iprigljrn
        NO-ERROR.
    IF NOT AVAILABLE gl-jrn THEN RETURN.
    
    iNextLine = 1.
    FOR EACH bf-gl-jrnl OF gl-jrn NO-LOCK BY LINE DESCENDING:
        iNextLine = bf-gl-jrnl.line + 1.
        LEAVE.
    END.
                    
    CREATE gl-jrnl.
    ASSIGN gl-jrnl.j-no    = ipiJournal-no
           gl-jrnl.line    = iNextLine
           gl-jrnl.actnum  = ipcActNum
           gl-jrnl.dscr    = ipcDescription
           gl-jrnl.tr-amt  = ipdTr-Amount.

    IF gl-jrnl.tr-amt <> 0 THEN
    DO:
        FIND gl-jrn EXCLUSIVE-LOCK 
            WHERE ROWID(gl-jrn) EQ iprigljrn
            NO-ERROR NO-WAIT.
        IF AVAIL(gl-jrn) THEN
        DO:
            IF gl-jrnl.tr-amt GT 0 THEN
                gl-jrn.tdeb = gl-jrn.tdeb  + gl-jrnl.tr-amt.
            ELSE
                gl-jrn.tcred = gl-jrn.tcred + gl-jrnl.tr-amt.

            gl-jrn.tr-amt = gl-jrn.tdeb + gl-jrn.tcred.
        END.
    END.
    
        oprigljrnl = ROWID(gl-jrnl).
        
    RELEASE gl-jrnl.
    RELEASE gl-jrn.

END PROCEDURE.


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportgljrn FOR ttImportgljrn.
    DEFINE INPUT        PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded       AS INTEGER NO-UNDO.

    DEFINE VARIABLE rigljrn    AS ROWID   NO-UNDO. 
    DEFINE VARIABLE rigljrnl   AS ROWID   NO-UNDO.
    DEFINE VARIABLE iJournalNo AS INTEGER NO-UNDO.
      
    DEFINE BUFFER bf-gl-jrn  FOR gl-jrnl.
    DEFINE BUFFER bf-gl-jrnl FOR gl-jrnl.
    
    iopiAdded = iopiAdded + 1.
        
    /*if found, add another line to existing header - otherwise, create a new header*/
    FIND FIRST bf-gl-jrn NO-LOCK
        WHERE bf-gl-jrn.j-no EQ ipbf-ttImportgljrn.j-no
        NO-ERROR.
    IF NOT AVAILABLE bf-gl-jrn THEN /*create a new one*/
    DO:
        RUN pCreateNewGeneralHeader(
            ipbf-ttImportgljrn.Company, 
            ipbf-ttImportgljrn.Date, 
            ipbf-ttImportgljrn.period, 
            ipbf-ttImportgljrn.reverse,
            OUTPUT rigljrn,
            OUTPUT iJournalNo).
        
       
    END. /*not available bf-gl-jrn*/
    
    RUN pCreateNewGeneralLine(
        ROWID(bf-gl-jrn),
        iJournalNo,
        ipbf-ttImportgljrn.actnum,
        ipbf-ttImportgljrn.dscr,
        ipbf-ttImportgljrn.tr-amt,
        OUTPUT rigljrnl).
        
                                         
END PROCEDURE.
    