
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

DEFINE TEMP-TABLE ttImportGLjrn   
    FIELD Location        AS CHARACTER                       COLUMN-LABEL "Location" 
    FIELD Company         AS CHARACTER FORMAT "x(3)"         COLUMN-LABEL "Company"
    FIELD j-no            AS INTEGER FORMAT ">>>>>>9"        COLUMN-LABEL "Internal Journal#"
    FIELD journal         AS INTEGER FORMAT ">>>>>>9"        COLUMN-LABEL "Journal #" 
    FIELD tr-date         AS DATE    FORMAT "99/99/9999"     COLUMN-LABEL "Trx Date" 
    FIELD tr-amt          AS DECIMAL FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Amount" 
    FIELD tcred           AS DECIMAL FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Credits" 
    FIELD tdeb            AS DECIMAL FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Debits" 
    FIELD period          AS INTEGER FORMAT "99"             COLUMN-LABEL "Period" 
    FIELD posted          AS LOGICAL FORMAT "Y/N"            COLUMN-LABEL "Posted" 
         .
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 1. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportGLjrn"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportGLjrn FOR ttImportGLjrn.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportGLjrn FOR ttImportGLjrn.
    
    oplValid = YES.

    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGLjrn.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportGLjrn NO-LOCK 
            WHERE bf-ttImportGLjrn.Company EQ bf-ttImportGLjrn.Company
            AND bf-ttImportGLjrn.posted EQ bf-ttImportGLjrn.posted
            AND bf-ttImportGLjrn.journal EQ bf-ttImportGLjrn.journal
            AND bf-ttImportGLjrn.tr-date EQ bf-ttImportGLjrn.tr-Date           
            AND ROWID(bf-ttImportGLjrn) NE ROWID(bf-ttImportGLjrn)
            NO-ERROR.
        IF AVAILABLE bf-ttImportGLjrn THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    
    FIND FIRST period                   
        WHERE period.company EQ bf-ttImportGLjrn.Company
        AND period.pst     LE bf-ttImportGLjrn.tr-date
        AND period.pend    GE bf-ttImportGLjrn.tr-date
        NO-LOCK NO-ERROR.
    IF NOT AVAIL period THEN 
        ASSIGN 
            oplValid = NO 
            opcNote  = "No Defined Period Exists for" + string(ipbf-ttImportGLjrn.tr-date)
            .


END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportGLjrn FOR ttImportGLjrn.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-gl-jrn FOR gl-jrn.
    DEFINE VARIABLE tran-period AS INTEGER NO-UNDO.

    FIND FIRST period                   
        WHERE period.company EQ ipbf-ttImportGLjrn.Company
        AND period.pst      LE ipbf-ttImportGLjrn.tr-date
        AND period.pend     GE ipbf-ttImportGLjrn.tr-date
        NO-LOCK NO-ERROR.
    IF AVAIL period THEN tran-period = period.pnum.


    FIND FIRST bf-gl-jrn EXCLUSIVE-LOCK 
        WHERE bf-gl-jrn.Company EQ ipbf-ttImportGLjrn.Company
        AND bf-gl-jrn.posted    EQ ipbf-ttImportGLjrn.posted
        AND bf-gl-jrn.journal   EQ ipbf-ttImportGLjrn.journal
        AND bf-gl-jrn.tr-date   EQ ipbf-ttImportGLjrn.tr-Date  
        NO-ERROR.
    IF NOT AVAILABLE bf-gl-jrn THEN DO:
        iopiAdded = iopiAdded + 1.
        CREATE bf-gl-jrn.
        ASSIGN
        bf-gl-jrn.company   = ipbf-ttImportGLjrn.Company
        bf-gl-jrn.j-no      = ipbf-ttImportGLjrn.j-no  
        bf-gl-jrn.journal   = ipbf-ttImportGLjrn.journal 
        bf-gl-jrn.tr-date   = ipbf-ttImportGLjrn.tr-date
        bf-gl-jrn.tr-amt    = ipbf-ttImportGLjrn.tr-amt
        bf-gl-jrn.tcred     = ipbf-ttImportGLjrn.tcred
        bf-gl-jrn.tdeb      = ipbf-ttImportGLjrn.tdeb
        bf-gl-jrn.period    = tran-period
        bf-gl-jrn.posted    = ipbf-ttImportGLjrn.posted. 
    END.
    
    RELEASE bf-gl-jrn.

END PROCEDURE.

