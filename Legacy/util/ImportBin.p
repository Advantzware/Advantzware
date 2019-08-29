/*------------------------------------------------------------------------
    File        : ImportBin.p
    Purpose     : 
    Syntax      :
    Description : Import Program (Persistent) for Configuring and Processing the Import for Bin	
    Author(s)   : BV
    Created     : Sun Jan 21:18:38 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportBin
    FIELD Company   AS CHARACTER 
    FIELD Location  AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Location" HELP "Required. Must be valid - Size:5"
    FIELD BinType   AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Type" HELP "Required. Must be FG  RM or WIP"
    FIELD GLCode    AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "GL Code" HELP "Optional. Any text value"
    FIELD BinLoc    AS CHARACTER FORMAT "X(8)" COLUMN-LABEL "Primary Bin Loc" HELP "Required - Size:8"
    FIELD BinAct    AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "FG Bin Active" HELP "Optional. Yes or No - Default Yes"
    .

DEFINE VARIABLE giIndexOffset AS INTEGER   NO-UNDO INIT 1. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportBin"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportBin FOR ttImportBin.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportBin FOR ttImportBin.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportBin.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportBin.Location EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Location".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportBin.BinType EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Type".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportBin.BinLoc EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Primary Bin Loc".
    END.
    
    IF oplValid THEN DO:
        IF ipbf-ttImportBin.BinType = "FG" THEN DO:
            FIND FIRST fg-bin NO-LOCK 
                WHERE fg-bin.company EQ ipbf-ttImportBin.Company
                AND fg-bin.loc EQ ipbf-ttImportBin.Location
                AND fg-bin.loc-bin EQ ipbf-ttImportBin.BinLoc
                NO-ERROR .
            IF AVAILABLE fg-bin THEN DO:
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
                    opcNote = "Add record".
        END.
        ELSE IF ipbf-ttImportBin.BinType = "RM" THEN DO:
            FIND FIRST rm-bin NO-LOCK 
                WHERE rm-bin.company EQ ipbf-ttImportBin.Company
                AND rm-bin.loc EQ ipbf-ttImportBin.Location
                AND rm-bin.loc-bin EQ ipbf-ttImportBin.BinLoc
                NO-ERROR .
            IF AVAILABLE rm-bin THEN DO:
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
                    opcNote = "Add record".
        END.
        ELSE DO:
            FIND FIRST wip-bin NO-LOCK 
                WHERE wip-bin.company EQ ipbf-ttImportBin.Company
                AND wip-bin.loc EQ ipbf-ttImportBin.Location
                AND wip-bin.loc-bin EQ ipbf-ttImportBin.BinLoc
                NO-ERROR .
            IF AVAILABLE wip-bin THEN DO:
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
                    opcNote = "Add record".
        END.
    END.

    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportBin.BinType NE "" THEN 
           RUN pIsValidFromList IN hdValidator ("Type", ipbf-ttImportBin.BinType, "FG,RM,WIP", OUTPUT oplValid, OUTPUT cValidNote).

    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    IF ipbf-ttImportBin.BinAct EQ "" THEN
        ipbf-ttImportBin.BinAct = "Yes" .
    
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportBin FOR ttImportBin.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
     
    IF AVAILABLE ipbf-ttImportBin THEN DO: 

        IF ipbf-ttImportBin.BinType = "FG" THEN DO:
            FIND FIRST fg-bin EXCLUSIVE-LOCK
                WHERE fg-bin.company EQ ipbf-ttImportBin.company
                AND fg-bin.loc EQ ipbf-ttImportBin.Location
                AND fg-bin.loc-bin EQ ipbf-ttImportBin.BinLoc
                NO-ERROR.  
            IF NOT AVAILABLE fg-bin THEN 
            DO:
                iopiAdded = iopiAdded + 1.
                CREATE fg-bin.
            END.
            ASSIGN
            fg-bin.company     = ipbf-ttImportBin.company
            fg-bin.loc         = ipbf-ttImportBin.Location
            fg-bin.loc-bin     = ipbf-ttImportBin.BinLoc 
            fg-bin.ACTIVE      = LOGICAL(ipbf-ttImportBin.BinAct)      .
        END.
        ELSE IF ipbf-ttImportBin.BinType = "RM" THEN DO:
            FIND FIRST rm-bin EXCLUSIVE-LOCK
                WHERE rm-bin.company EQ ipbf-ttImportBin.company
                AND rm-bin.loc EQ ipbf-ttImportBin.Location
                AND rm-bin.loc-bin EQ ipbf-ttImportBin.BinLoc
                NO-ERROR.  
            IF NOT AVAILABLE rm-bin THEN 
            DO:
                iopiAdded = iopiAdded + 1.
                CREATE rm-bin.
            END.
            ASSIGN
            rm-bin.company     = ipbf-ttImportBin.company
            rm-bin.loc         = ipbf-ttImportBin.Location
            rm-bin.loc-bin     = ipbf-ttImportBin.BinLoc.
        END.
        ELSE DO:
            FIND FIRST wip-bin EXCLUSIVE-LOCK
                WHERE wip-bin.company EQ ipbf-ttImportBin.company
                AND wip-bin.loc EQ ipbf-ttImportBin.Location
                AND wip-bin.loc-bin EQ ipbf-ttImportBin.BinLoc
                NO-ERROR.  
            IF NOT AVAILABLE wip-bin THEN 
            DO:
                iopiAdded = iopiAdded + 1.
                CREATE wip-bin.
            END.
            ASSIGN
            wip-bin.company     = ipbf-ttImportBin.company
            wip-bin.loc         = ipbf-ttImportBin.Location
            wip-bin.loc-bin     = ipbf-ttImportBin.BinLoc.
        END.
        RELEASE loc .
        RELEASE fg-bin .
        RELEASE rm-bin .
        RELEASE wip-bin .
    END.
    
END PROCEDURE.
