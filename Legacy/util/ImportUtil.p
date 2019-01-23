/*------------------------------------------------------------------------
    File        : ImportUtil.p
    Purpose     : 
    Syntax      :
    Description : Import Program (Persistent) for the Import for Utilities
    Author(s)   : BV
    Created     : MOn Nov 5 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportUtil
    FIELD Company      AS CHARACTER 
    FIELD Location     AS CHARACTER 
    FIELD cProgramName AS CHARACTER FORMAT "X(16)" COLUMN-LABEL "Program Name" HELP "Required - Size:16" 
    FIELD cModule      AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Module" HELP "Optional - Size:3"
    FIELD cHotKey      AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Hotkey" HELP "Optional - Size:3"
    FIELD cDesc        AS CHARACTER FORMAT "X(48)" COLUMN-LABEL "Description" HELP "Optional - Size:48"
    FIELD cNotes       AS CHARACTER FORMAT "X(1000)" COLUMN-LABEL "Notes" HELP "Optional - Size:1000" 
    FIELD iSecLev      AS INTEGER   FORMAT ">>>9" COLUMN-LABEL "Security Level" HELP "Optional - Integer"
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportUtil"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportUtil FOR ttImportUtil.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuperAdmin   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE hPgmMstrSecur AS HANDLE    NO-UNDO.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportUtil.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportUtil.cProgramName EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Program Name".
    END.
    
    FIND FIRST users NO-LOCK
        WHERE  users.user_id EQ USERID(LDBNAME(1)) 
        NO-ERROR.
    
     IF AVAILABLE users THEN 
     DO: 
        IF ipbf-ttImportUtil.iSecLev GT users.securityLevel THEN 
        DO:
            ASSIGN 
                oplValid = NO
                opcNote  = "security level can not be greater than " + string(users.securityLevel) .
        END.
     END.
   

    IF oplValid THEN 
    DO:
        FIND FIRST utilities NO-LOCK 
            WHERE utilities.programName EQ ipbf-ttImportUtil.cProgramName
            NO-ERROR .
        IF AVAILABLE utilities THEN 
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
        
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportUtil FOR ttImportUtil.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
     
    FIND FIRST utilities EXCLUSIVE-LOCK
        WHERE utilities.programName EQ ipbf-ttImportUtil.cProgramName
        NO-ERROR.  
    IF NOT AVAILABLE utilities THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE utilities.
        ASSIGN 
            utilities.programName = ipbf-ttImportUtil.cProgramName
            .
    END.

    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */
    RUN pAssignValueC (ipbf-ttImportUtil.cModule, iplIgnoreBlanks, INPUT-OUTPUT utilities.module).
    RUN pAssignValueC (ipbf-ttImportUtil.cHotKey, iplIgnoreBlanks, INPUT-OUTPUT utilities.hotkey).
    RUN pAssignValueC (ipbf-ttImportUtil.cDesc, iplIgnoreBlanks, INPUT-OUTPUT utilities.description).
    RUN pAssignValueC (ipbf-ttImportUtil.cNotes, iplIgnoreBlanks, INPUT-OUTPUT utilities.notes).
    RUN pAssignValueI (ipbf-ttImportUtil.iSecLev, iplIgnoreBlanks, INPUT-OUTPUT utilities.securityLevel).

END PROCEDURE.
