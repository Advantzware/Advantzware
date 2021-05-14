
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

    FIND FIRST bf-gl-jrnl EXCLUSIVE-LOCK 
        WHERE bf-gl-jrnl.j-no EQ ipbf-ttImportGLjrnl.j-no
        AND bf-gl-jrnl.line EQ ipbf-ttImportGLjrnl.line
        NO-ERROR.
    IF NOT AVAILABLE bf-gl-jrnl THEN DO:
        iopiAdded = iopiAdded + 1.
        CREATE bf-gl-jrnl.
        ASSIGN
        //   bf-gl-jrnl.j-no = gl-jrn.j-no
        //    bf-gl-jrnl.line = v-line
        //    bf-gl-jrnl.actnum = v-acct
            bf-gl-jrnl.dscr = "Payroll G/L Entry Import".
       //     bf-gl-jrnl.tr-amt = v-amount
            bf-gl-jrnl.tr-amt = bf-gl-jrnl.tr-amt + bf-gl-jrnl.tr-amt.
      //      bf-gl-jrnl.tcred = bf-gl-jrn.tcred +
        //        (IF bf-gl-jrnl.tr-amt < 0 THEN bf-gl-jrnl.tr-amt ELSE 0).
//            bf-gl-jrnl.tdeb = bf-gl-jrnl.tdeb +
  //              (IF bf-gl-jrnl.tr-amt > 0 THEN bf-gl-jrnl.tr-amt ELSE 0). 
    END.
   // RUN pAssignValueC (ipbf-ttImportGLjrnl.AccountDesc, iplIgnoreBlanks, INPUT-OUTPUT bf-account.dscr).
   // RUN pAssignValueC (ipbf-ttImportGLjrnl.AccountType, YES, INPUT-OUTPUT bf-account.type).   
   // RUN pAssignValueCToL (ipbf-ttImportGLjrnl.Inactive, "Yes", iplIgnoreBlanks, INPUT-OUTPUT bf-account.Inactive).
    
    RELEASE bf-gl-jrnl.

END PROCEDURE.

