
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
        
DEFINE TEMP-TABLE ttImportGLjrnl
    FIELD Company          AS CHARACTER FORMAT "x(3)"           COLUMN-LABEL "Company"              HELP "Required - Size:3" 
    FIELD Location         AS CHARACTER                         COLUMN-LABEL "Location"             HELP "Optional - Size:8" 
    FIELD j-no             AS INTEGER   FORMAT ">>>>>>9"        COLUMN-LABEL "Internal Journal#"    HELP "Required - Integer" 
    FIELD line             AS INTEGER   FORMAT "999"            COLUMN-LABEL "Line#"                HELP "Optional - Integer" 
    FIELD actnum           AS CHARACTER FORMAT "x(25)"          COLUMN-LABEL "Account"              HELP "Required - Size:25" 
    FIELD dscr             AS CHARACTER FORMAT "x(30)"          COLUMN-LABEL "Description"          HELP "Optional - Size:30" 
    FIELD tr-amt           AS DECIMAL   FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Amount"               HELP "Optional - Decimal" 
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
    DEFINE VARIABLE v-payroll-system AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-acct           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-tmp            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-start          AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE i                AS INTEGER   NO-UNDO.
    DEFINE VARIABLE x                AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-ttImportGLjrnl FOR ttImportGLjrnl.
    DEFINE BUFFER bf-gl-jrn FOR gl-jrn.
    
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
    
    IF oplValid THEN 
    DO:
        FIND FIRST bf-gl-jrn NO-LOCK 
            WHERE bf-gl-jrn.j-no EQ ipbf-ttImportGLjrnl.j-no
            NO-ERROR.
        IF NOT AVAILABLE bf-gl-jrn THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Could not locate header when creating detail."
                .
    END.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportGLjrnl.tr-amt <> 0 THEN
        DO:
            FIND FIRST sys-ctrl 
                WHERE sys-ctrl.company EQ ipbf-ttImportGLjrnl.Company
                AND sys-ctrl.name    EQ "PAYVEND"
                NO-LOCK NO-ERROR.
            IF NOT AVAIL sys-ctrl THEN
                v-payroll-system = sys-ctrl.char-fld.
    
            ASSIGN
                v-acct   = "".
    
            IF v-payroll-system = "WESTIND"
                THEN
                v-acct = ipbf-ttImportGLjrnl.actnum.
            ELSE
                IF v-payroll-system = "MAS90"
                    OR v-payroll-system = "ASI"
                    THEN
                DO:                                    
                    v-acct = ipbf-ttImportGLjrnl.actnum.
                    /* Code to import correct account number from MAS90 */
                    FIND FIRST company WHERE company.company EQ ipbf-ttImportGLjrnl.Company NO-LOCK NO-ERROR.
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
    
            FIND account NO-LOCK WHERE
                account.company = ipbf-ttImportGLjrnl.Company AND
                account.actnum = v-acct NO-ERROR.
    
            IF NOT AVAILABLE account THEN
                ASSIGN 
                    oplValid = YES 
                    opcNote  = "WARNING: Account " + v-acct + " was not found! "
                    .
        END.
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
    DEFINE BUFFER bf-gl-jrn  FOR gl-jrn.
    
    DEFINE VARIABLE v-payroll-system    AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE v-acct              AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE v-tmp               AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE v-start             AS INTEGER      NO-UNDO. 
    DEFINE VARIABLE v-line              AS INTEGER      NO-UNDO.   
    DEFINE VARIABLE i                   AS INTEGER      NO-UNDO.
    DEFINE VARIABLE x                   AS INTEGER      NO-UNDO. 

    FIND FIRST sys-ctrl 
        WHERE sys-ctrl.company EQ ipbf-ttImportGLjrnl.Company
        AND sys-ctrl.name    EQ "PAYVEND"
        NO-LOCK NO-ERROR.
    IF AVAILABLE sys-ctrl THEN
        v-payroll-system = sys-ctrl.char-fld.

        ASSIGN
            v-acct   = "".
        IF v-payroll-system = "WESTIND"
            THEN
        ASSIGN 
            v-acct = ipbf-ttImportGLjrnl.actnum
            .            
        ELSE
            IF v-payroll-system = "MAS90"
                OR v-payroll-system = "ASI"
                THEN
            DO:                                   
                v-acct = ipbf-ttImportGLjrnl.actnum.
                /* Code to import correct account number from MAS90 */
                FIND FIRST company WHERE company.company EQ ipbf-ttImportGLjrnl.Company NO-LOCK NO-ERROR.
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

            IF ipbf-ttImportGLjrnl.tr-amt <> 0 THEN
            DO: 
                v-line = ipbf-ttImportGLjrnl.line.   
                IF ipbf-ttImportGLjrnl.line = 0 THEN 
                    LINE-NUM-BLOCK:
                    FOR EACH bf-gl-jrnl NO-LOCK 
                        WHERE bf-gl-jrnl.j-no EQ ipbf-ttImportGLjrnl.j-no
                        BY bf-gl-jrnl.line DESCENDING 
                        :
                        v-line = bf-gl-jrnl.line + 1.
                        LEAVE LINE-NUM-BLOCK.
                    END.         
                FIND FIRST bf-gl-jrnl EXCLUSIVE-LOCK 
                    WHERE bf-gl-jrnl.j-no EQ ipbf-ttImportGLjrnl.j-no
                    AND bf-gl-jrnl.line EQ ipbf-ttImportGLjrnl.line
                    NO-ERROR.
                IF NOT AVAILABLE bf-gl-jrnl THEN 
                DO:
                    iopiAdded = iopiAdded + 1.
                    CREATE bf-gl-jrnl.
                    ASSIGN
                        bf-gl-jrnl.j-no     = ipbf-ttImportGLjrnl.j-no
                        bf-gl-jrnl.line     = v-line 
                        bf-gl-jrnl.actnum   = v-acct
                        bf-gl-jrnl.dscr     = ipbf-ttImportGLjrnl.dscr
                        bf-gl-jrnl.tr-amt   = ipbf-ttImportGLjrnl.tr-amt.

                    FIND FIRST bf-gl-jrn EXCLUSIVE-LOCK 
                        WHERE bf-gl-jrn.j-no EQ ipbf-ttImportGLjrnl.j-no
                        NO-ERROR.
                    IF AVAILABLE bf-gl-jrn THEN  
                    DO:
                    ASSIGN      
                        bf-gl-jrn.tr-amt   = bf-gl-jrn.tr-amt + ipbf-ttImportGLjrnl.tr-amt
                        bf-gl-jrn.tcred    = bf-gl-jrn.tcred +
                                                (IF ipbf-ttImportGLjrnl.tr-amt < 0 THEN ipbf-ttImportGLjrnl.tr-amt ELSE 0)
                        bf-gl-jrn.tdeb     = bf-gl-jrn.tdeb +
                                                (IF ipbf-ttImportGLjrnl.tr-amt > 0 THEN ipbf-ttImportGLjrnl.tr-amt ELSE 0)
                        .
                        END.
                END.  
            END.    

    RELEASE bf-gl-jrnl.

END PROCEDURE.

