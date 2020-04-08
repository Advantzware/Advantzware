
/*------------------------------------------------------------------------
    File        : ImportMessage.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Message	

    Author(s)   : Sewa Singh
    Created     : Tue March 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportMessage
    FIELD Company                 AS CHARACTER 
    FIELD Location                AS CHARACTER 
    FIELD msgID                   AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Message ID" HELP "Required - Size:5" 
    FIELD msgName                 AS CHARACTER FORMAT "x(48)" COLUMN-LABEL "Message Name" HELP "Optional - Size:48"
    FIELD msgType                 AS CHARACTER FORMAT "x(12)" COLUMN-LABEL "Msg Type" HELP "Optional - - Size:12"
    FIELD module                  AS CHARACTER FORMAT "x(4)" COLUMN-LABEL "Module" HELP "Optional - Size:4"
    FIELD hotKey                  AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Hot Key" HELP "Optional - Size:3"
    FIELD currSecLevel            AS INTEGER   FORMAT ">>>9" COLUMN-LABEL "Security Level" HELP "Optional - Integer"
    FIELD defaultTitle            AS CHARACTER FORMAT "x(40)" COLUMN-LABEL "Default Title" HELP "Optional - Size:40"
    FIELD defaultMsg              AS CHARACTER FORMAT "x(120)" COLUMN-LABEL "Default Message" HELP "Optional - Size:120"
    FIELD currentTitle            AS CHARACTER FORMAT "X(40)" COLUMN-LABEL "Custom Title" HELP "Optional - Size:40"
    FIELD currMessage             AS CHARACTER FORMAT "x(120)" COLUMN-LABEL "Custom Message" HELP "Optional - Size:120"
    FIELD userSuppress            AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Suppress Display" HELP "Optional - Yes or No (blank=No)"
    FIELD rtnValue                AS CHARACTER FORMAT "x(24)" COLUMN-LABEL "Default Answer" HELP "Optional - Size:24"
        
    .
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data*/
 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
 /*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportMessage"}


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportMessage FOR ttImportMessage.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE riNote AS ROWID NO-UNDO.
    DEFINE BUFFER bf-zMessage FOR zMessage.
    
    FIND FIRST users NO-LOCK
        WHERE  users.user_id EQ USERID(LDBNAME(1)) 
        NO-ERROR.

    FIND FIRST bf-zMessage EXCLUSIVE-LOCK 
        WHERE bf-zMessage.msgID EQ ipbf-ttImportMessage.msgID        
        NO-ERROR.

    IF NOT AVAILABLE bf-zMessage THEN 
    DO:
        ASSIGN 
            iopiAdded = iopiAdded + 1.
        CREATE bf-zMessage.
        ASSIGN 
            bf-zMessage.msgID   = ipbf-ttImportMessage.msgID .            
    END.
                                                                                                                                     
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */
    IF users.securityLevel GE 1000 THEN
    DO:
        RUN pAssignValueC (ipbf-ttImportMessage.msgName, iplIgnoreBlanks, INPUT-OUTPUT bf-zMessage.msgName).                                                   
        RUN pAssignValueC (ipbf-ttImportMessage.msgType, iplIgnoreBlanks, INPUT-OUTPUT bf-zMessage.msgType).                                                   
        RUN pAssignValueC (ipbf-ttImportMessage.module, iplIgnoreBlanks, INPUT-OUTPUT bf-zMessage.module).                               
        RUN pAssignValueC (ipbf-ttImportMessage.hotKey, iplIgnoreBlanks, INPUT-OUTPUT bf-zMessage.hotKey).                                                   
        RUN pAssignValueI (ipbf-ttImportMessage.currSecLevel, iplIgnoreBlanks, INPUT-OUTPUT bf-zMessage.currSecLevel).                                                   
        RUN pAssignValueC (ipbf-ttImportMessage.defaultTitle, iplIgnoreBlanks, INPUT-OUTPUT bf-zMessage.defaultTitle).                                       
        RUN pAssignValueC (ipbf-ttImportMessage.defaultMsg, iplIgnoreBlanks, INPUT-OUTPUT bf-zMessage.defaultMsg).                                               
        RUN pAssignValueC (ipbf-ttImportMessage.rtnValue, iplIgnoreBlanks, INPUT-OUTPUT bf-zMessage.rtnValue).      
    END.
    RUN pAssignValueC (ipbf-ttImportMessage.currentTitle, iplIgnoreBlanks, INPUT-OUTPUT bf-zMessage.currentTitle).                      
    RUN pAssignValueC (ipbf-ttImportMessage.currMessage, iplIgnoreBlanks, INPUT-OUTPUT bf-zMessage.currMessage).                                 
    RUN pAssignValueCToL (ipbf-ttImportMessage.userSuppress,"Yes", iplIgnoreBlanks, INPUT-OUTPUT bf-zMessage.userSuppress). 

   RELEASE bf-zMessage .
                                                                                                                               
                                                                                                                               
END PROCEDURE.                                                                                                                 
                                                                                                                               
PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportMessage FOR ttImportMessage.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportMessage FOR ttImportMessage.

    FIND FIRST users NO-LOCK
        WHERE  users.user_id EQ USERID(LDBNAME(1)) 
        NO-ERROR.
        
    oplValid = YES.
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportMessage.msgID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "MessageID is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF NOT(ipbf-ttImportMessage.userSuppress EQ "Yes" OR ipbf-ttImportMessage.userSuppress EQ "No")  THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Suppress Display is wrong".
    END.    
    
    /*Check for Duplicate Import Record and Ignore It*/ 
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportMessage NO-LOCK 
            WHERE bf-ttImportMessage.msgID EQ ipbf-ttImportMessage.msgID             
            AND ROWID(bf-ttImportMessage) NE ROWID(ipbf-ttImportMessage)
            NO-ERROR.
        IF AVAILABLE bf-ttImportMessage THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    /*Determine if Add or Update*/
    IF oplValid THEN 
    DO:
        FIND FIRST zMessage NO-LOCK 
            WHERE zMessage.msgID EQ ipbf-ttImportMessage.msgID            
            NO-ERROR .
        IF AVAIL zMessage THEN
        DO:             
            IF users.securityLevel LT 900 THEN
                ASSIGN
                    oplValid = NO
                    opcNote  = "Security level not allow update"
                    .
            IF oplValid AND NOT iplUpdateDuplicates THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Duplicate record exists"
                    .
            ELSE  IF oplValid THEN
                ASSIGN 
                    oplValid = YES
                    opcNote = "Update existing record"
                    .        
        END.
        ELSE  IF users.securityLevel GE 1000 THEN
            ASSIGN 
                oplValid = YES
                opcNote = "Add record"
                .
        ELSE 
           ASSIGN 
                oplValid = NO
                opcNote = "Not allow to add record"
                .
    END.
    
    /*Field Level Validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:
        
        IF oplValid AND ipbf-ttImportMessage.msgType NE "" THEN 
             RUN pIsValidFromList ("Msg Type" ,ipbf-ttImportMessage.msgType, ",ERROR,INFO,MESSAGE,WARNING,QUESTION-YN", OUTPUT oplValid, OUTPUT cValidNote).
        
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
END PROCEDURE.

