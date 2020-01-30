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

    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportBin FOR ttImportBin.
    
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
           RUN pIsValidFromList ("Type", ipbf-ttImportBin.BinType, "FG,RM,WIP", OUTPUT oplValid, OUTPUT cValidNote).

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
    DEFINE BUFFER bf-fg-bin FOR fg-bin .
    DEFINE BUFFER bf-rm-bin FOR rm-bin .
    DEFINE BUFFER bf-wip-bin FOR wip-bin .
     
    IF AVAILABLE ipbf-ttImportBin THEN DO: 

        IF ipbf-ttImportBin.BinType = "FG" THEN DO:
            FIND FIRST bf-fg-bin EXCLUSIVE-LOCK
                WHERE bf-fg-bin.company EQ ipbf-ttImportBin.company
                AND bf-fg-bin.loc EQ ipbf-ttImportBin.Location
                AND bf-fg-bin.loc-bin EQ ipbf-ttImportBin.BinLoc
                NO-ERROR.  
            IF NOT AVAILABLE bf-fg-bin THEN 
            DO:
                iopiAdded = iopiAdded + 1.
                CREATE bf-fg-bin.
            END.
            ASSIGN
            bf-fg-bin.company     = ipbf-ttImportBin.company
            bf-fg-bin.loc         = ipbf-ttImportBin.Location
            bf-fg-bin.loc-bin     = ipbf-ttImportBin.BinLoc 
            bf-fg-bin.ACTIVE      = LOGICAL(ipbf-ttImportBin.BinAct)      .
        END.
        ELSE IF ipbf-ttImportBin.BinType = "RM" THEN DO:
            FIND FIRST bf-rm-bin EXCLUSIVE-LOCK
                WHERE bf-rm-bin.company EQ ipbf-ttImportBin.company
                AND bf-rm-bin.loc EQ ipbf-ttImportBin.Location
                AND bf-rm-bin.loc-bin EQ ipbf-ttImportBin.BinLoc
                NO-ERROR.  
            IF NOT AVAILABLE bf-rm-bin THEN 
            DO:
                iopiAdded = iopiAdded + 1.
                CREATE bf-rm-bin.
            END.
            ASSIGN
            bf-rm-bin.company     = ipbf-ttImportBin.company
            bf-rm-bin.loc         = ipbf-ttImportBin.Location
            bf-rm-bin.loc-bin     = ipbf-ttImportBin.BinLoc.
        END.
        ELSE DO:
            FIND FIRST bf-wip-bin EXCLUSIVE-LOCK
                WHERE bf-wip-bin.company EQ ipbf-ttImportBin.company
                AND bf-wip-bin.loc EQ ipbf-ttImportBin.Location
                AND bf-wip-bin.loc-bin EQ ipbf-ttImportBin.BinLoc
                NO-ERROR.  
            IF NOT AVAILABLE bf-wip-bin THEN 
            DO:
                iopiAdded = iopiAdded + 1.
                CREATE bf-wip-bin.
            END.
            ASSIGN
            bf-wip-bin.company     = ipbf-ttImportBin.company
            bf-wip-bin.loc         = ipbf-ttImportBin.Location
            bf-wip-bin.loc-bin     = ipbf-ttImportBin.BinLoc.
        END.
        RELEASE loc .
        RELEASE bf-fg-bin .
        RELEASE bf-rm-bin .
        RELEASE bf-wip-bin .
    END.
    
END PROCEDURE.
