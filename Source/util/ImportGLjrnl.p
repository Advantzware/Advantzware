
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
{methods/defines/hndldefs.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{sys/inc/VAR.i new shared}

ASSIGN
    cocode = gcompany.
        
DEFINE TEMP-TABLE ttImportGLjrnl
    FIELD j-no             AS INTEGER   FORMAT ">>>>>>9"        COLUMN-LABEL "Internal Journal#" 
    FIELD line             AS INTEGER   FORMAT "999"  
    FIELD actnum           AS CHARACTER FORMAT "x(25)"          COLUMN-LABEL "Account" 
    FIELD dscr             AS CHARACTER FORMAT "x(30)"          COLUMN-LABEL "Description" 
    FIELD tr-amt           AS DECIMAL   FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Amount" 
        .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportGLjrnl"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportGLjrnl FOR ttImportGLjrnl.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportGLjrnl FOR ttImportGLjrnl.

    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGLjrnl.j-no EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Journal #".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGLjrnl.line EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Line #".
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportGLjrnl NO-LOCK 
            WHERE bf-ttImportGLjrnl.j-no EQ ipbf-ttImportGLjrnl.j-no
            AND bf-ttImportGLjrnl.line EQ ipbf-ttImportGLjrnl.line
            AND ROWID(bf-ttImportGLjrnl) NE ROWID(ipbf-ttImportGLjrnl)
            NO-ERROR.
        IF AVAILABLE bf-ttImportGLjrnl THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST GL-jrnl NO-LOCK 
            WHERE GL-jrnl.j-no EQ ipbf-ttImportGLjrnl.j-no
            AND GL-jrnl.line EQ ipbf-ttImportGLjrnl.line
            NO-ERROR .
        IF AVAILABLE GL-jrnl THEN 
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

END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportGLjrnl FOR ttImportGLjrnl.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-gl-jrnl FOR gl-jrnl.
    DEFINE VARIABLE xtrnum AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-payroll-system AS CHARACTER NO-UNDO.
    DEFINE VARIABLE  save_id AS RECID.
    DEFINE VARIABLE v-line AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-amount AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-acct AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-tmp AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-start AS INTEGER NO-UNDO. 

    FIND FIRST sys-ctrl 
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "PAYVEND"
        NO-LOCK NO-ERROR.
    IF NOT AVAIL sys-ctrl THEN
        v-payroll-system = sys-ctrl.char-fld.
     /*   IF v-payroll-system = "WESTIND" THEN
        DO:
            ASSIGN
                v-date        = tran-date
                v-jsc         = "PR"
                v-seq         = "000"
                v-source      = "PR".
        END.  /* WESTIND */
        ELSE
        DO:
            qtr-file-name = in-file-name + ".q".

            cmdline = cQuoterFullPathName + " -d , " + in-file-name + " >" + qtr-file-name.

            IF OPSYS = "unix" THEN
                UNIX SILENT VALUE(cmdline).
            ELSE
                IF OPSYS = "msdos" OR OPSYS BEGINS "win" THEN
                    DOS SILENT VALUE(cmdline).

        END.  /* else do */*/
        
        FIND FIRST gl-ctrl EXCLUSIVE-LOCK
            WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
        IF AVAIL gl-ctrl THEN 
        DO:
            ASSIGN 
                xtrnum        = gl-ctrl.trnum + 1
                gl-ctrl.trnum = xtrnum.
            FIND CURRENT gl-ctrl NO-LOCK.
            RELEASE gl-ctrl.
            LEAVE.
        END. /* IF AVAIL gl-ctrl */
        /* gdm - 11050906 */

        CREATE gl-jrn.

        ASSIGN
            gl-jrn.company = cocode
            gl-jrn.j-no    = xtrnum
            gl-jrn.journal = xtrnum
         //   gl-jrn.tr-date = tran-date
            gl-jrn.tr-amt  = 0
            gl-jrn.tcred   = 0
            gl-jrn.tdeb    = 0
      //      gl-jrn.period  = tran-period
            gl-jrn.posted  = FALSE
            save_id        = RECID(gl-jrn).

        v-line = 0.

        ASSIGN
            v-amount = 0
            v-acct   = "".

        IF v-payroll-system = "WESTIND"
            THEN
        DO:
          //  IMPORT STREAM s-input
          //      v-acct v-amount.
            v-acct = ipbf-ttImportGLjrnl.actnum.
            v-amount = ipbf-ttImportGLjrnl.tr-amt.
                
        END.
        ELSE
            IF v-payroll-system = "MAS90"
                OR v-payroll-system = "ASI"
                THEN
            DO:
               // IMPORT STREAM s-input
                 //   v-date v-jsc v-acct v-seq v-source v-amount.
                                    
                v-acct = ipbf-ttImportGLjrnl.actnum.
                v-amount = ipbf-ttImportGLjrnl.tr-amt.
                /* Code to import correct account number from MAS90 */
                FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
                IF AVAIL company THEN
                DO: 
                    ASSIGN 
                        v-tmp = "".
                    DO i = 1 TO company.acc-level:
                        ASSIGN 
                            v-start = 1.
                        IF i NE 1 THEN
                        DO x = 1 TO i - 1:
                            v-start = v-start + company.acc-dig[x].
                        END.
                        ASSIGN 
                            v-tmp = v-tmp + substring(v-acct,v-start,company.acc-dig[i]).
                        IF company.acc-level GT 1 AND i NE company.acc-level THEN
                            v-tmp = v-tmp + "-".
                    END.
                END.
                ASSIGN 
                    v-acct = v-tmp.

            END.

            IF v-amount <> 0 THEN
            DO:

                FIND account NO-LOCK WHERE
                    account.company = cocode AND
                    account.actnum = v-acct NO-ERROR.

                IF NOT AVAILABLE account THEN
                    MESSAGE "WARNING: Account "
                        v-acct " was not found! ".

                FIND gl-jrn WHERE RECID(gl-jrn) = save_id EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAIL gl-jrn THEN
                DO:
                    BELL.
                    MESSAGE "Could not locate header when creating detail" v-acct.
                    PAUSE.
                    RETURN.
                END.
                
                FOR FIRST bf-gl-jrnl NO-LOCK 
                    WHERE bf-gl-jrnl.j-no EQ ipbf-ttImportGLjrnl.j-no
                    BY bf-gl-jrnl.line DESCENDING 
                    :
                    v-line = bf-gl-jrnl.line.
                END.  
                
                FIND FIRST bf-gl-jrnl EXCLUSIVE-LOCK 
                    WHERE bf-gl-jrnl.j-no EQ ipbf-ttImportGLjrnl.j-no
                    AND bf-gl-jrnl.line EQ v-line
                    NO-ERROR.
                IF NOT AVAILABLE bf-gl-jrnl THEN 
                DO:
                    iopiAdded = iopiAdded + 1.
                    CREATE bf-gl-jrnl.
                    ASSIGN
                        v-line         = v-line + 1
                        bf-gl-jrnl.j-no   = ipbf-ttImportGLjrnl.j-no
                        bf-gl-jrnl.line   = v-line
                        bf-gl-jrnl.actnum = v-acct
                        bf-gl-jrnl.dscr   = "Payroll G/L Entry Import"
                        bf-gl-jrnl.tr-amt = v-amount
                        bf-gl-jrnl.tr-amt  = ipbf-ttImportGLjrnl.tr-amt + ipbf-ttImportGLjrnl.tr-amt
                        gl-jrn.tcred   = gl-jrn.tcred +
                                    (IF ipbf-ttImportGLjrnl.tr-amt < 0 THEN ipbf-ttImportGLjrnl.tr-amt ELSE 0)
                        gl-jrn.tdeb    = gl-jrn.tdeb +
                                    (IF ipbf-ttImportGLjrnl.tr-amt > 0 THEN ipbf-ttImportGLjrnl.tr-amt ELSE 0)
                        .
                END.  

            END.    

    RELEASE bf-gl-jrnl.

END PROCEDURE.

