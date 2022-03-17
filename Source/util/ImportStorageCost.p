/*------------------------------------------------------------------------
    File        : ImportStorageCost.p
    Purpose     : 
    Syntax      :
    Description : Import Program (Persistent) for Configuring and Processing the Import for Storage Cost	
    Author(s)   : Sachin Chahal
    Created     : Wed 03/16/2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportStorageCost
    FIELD Company       AS CHARACTER 
    FIELD Location      AS CHARACTER FORMAT "x(5)"       COLUMN-LABEL "Location"     HELP "Required. Must be valid - Size:5"
    FIELD positions     AS INTEGER   FORMAT ">>>>>9"     COLUMN-LABEL "Positions"    HELP "Required. Integer"
    FIELD handlingFee   AS DECIMAL   FORMAT "->>,>>9.99" COLUMN-LABEL "Handling Fee" HELP "Optional. Decimal"
    FIELD stack1High    AS DECIMAL   FORMAT "->>,>>9.99" COLUMN-LABEL "Stack 1 High" HELP "Optional. Decimal"
    FIELD stack2High    AS DECIMAL   FORMAT "->>,>>9.99" COLUMN-LABEL "Stack 2 High" HELP "Optional. Decimal"
    FIELD stack3High    AS DECIMAL   FORMAT "->>,>>9.99" COLUMN-LABEL "Stack 3 High" HELP "Optional. Decimal"
    FIELD stack4High    AS DECIMAL   FORMAT "->>,>>9.99" COLUMN-LABEL "Stack 4 High" HELP "Optional. Decimal"
    FIELD FromWidth     AS DECIMAL   FORMAT ">,>>9.99"   COLUMN-LABEL "From Width"   HELP "Optional. Decimal"
    FIELD upToWidth     AS DECIMAL   FORMAT ">,>>9.99"   COLUMN-LABEL "UpTo Width"   HELP "Optional. Decimal"
    FIELD FromLength    AS DECIMAL   FORMAT ">,>>9.99"   COLUMN-LABEL "From Length"  HELP "Optional. Decimal"
    FIELD upToLength    AS DECIMAL   FORMAT ">,>>9.99"   COLUMN-LABEL "UpTo Length"  HELP "Optional. Decimal"
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 1. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportStorageCost"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportStorageCost FOR ttImportStorageCost.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dValidWidthRangeBegin  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dValidWidthRangeEnd    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dValidLengthRangeBegin AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dValidLengthRangeEnd   AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-ttImportStorageCost FOR ttImportStorageCost.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportStorageCost.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportStorageCost.Location EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Location".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportStorageCost.Location EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Location".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportStorageCost.positions EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Positions".
        ELSE IF ipbf-ttImportStorageCost.positions GT 999999 THEN
            ASSIGN 
                oplValid = NO
                opcNote  = "Positions cannot be greater than 999999".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportStorageCost.upToWidth GT 9999.99 THEN
            ASSIGN 
                oplValid = NO
                opcNote  = "UpTo Width cannot be greater than 9999.99".
        IF ipbf-ttImportStorageCost.upToLength GT 9999.99 THEN
            ASSIGN 
                oplValid = NO
                opcNote  = "UpTo Length cannot be greater than 9999.99".
        /*IF ipbf-ttImportStorageCost.FromWidth GT 9999.98 THEN
            ASSIGN 
                oplValid = NO
                opcNote  = "UpTo Length cannot be greater than 9999.98".
        IF ipbf-ttImportStorageCost.FromWidth GT 9999.98 THEN
            ASSIGN 
                oplValid = NO
                opcNote  = "UpTo Length cannot be greater than 9999.98". */
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST storageCost NO-LOCK 
            WHERE storageCost.company   EQ ipbf-ttImportStorageCost.Company
              AND storageCost.location  EQ ipbf-ttImportStorageCost.Location
              AND storageCost.positions EQ ipbf-ttImportStorageCost.positions
            NO-ERROR .
        IF AVAILABLE storageCost THEN 
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
        IF oplValid AND ipbf-ttImportStorageCost.Location NE "" THEN 
            RUN pIsValidWarehouse IN hdValidator (ipbf-ttImportStorageCost.Location, NO, ipbf-ttImportStorageCost.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote. 
    
    DELETE OBJECT hdValidator.   
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportStorageCost FOR ttImportStorageCost.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-storageCost FOR storageCost.
    DEFINE BUFFER bf-palletSize FOR palletSize.

     
    FIND FIRST bf-storageCost EXCLUSIVE-LOCK
        WHERE bf-storageCost.company EQ ipbf-ttImportStorageCost.Company
        AND bf-storageCost.location EQ ipbf-ttImportStorageCost.Location
        AND bf-storageCost.positions EQ ipbf-ttImportStorageCost.positions
        NO-ERROR.  
    IF NOT AVAILABLE bf-storageCost THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE bf-storageCost.
        ASSIGN 
            bf-storageCost.company = ipbf-ttImportStorageCost.Company
            .
    END.
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */
    RUN pAssignValueC (ipbf-ttImportStorageCost.Location, iplIgnoreBlanks, INPUT-OUTPUT bf-storageCost.location).
    RUN pAssignValueI (ipbf-ttImportStorageCost.positions, iplIgnoreBlanks, INPUT-OUTPUT bf-storageCost.positions).
    RUN pAssignValueD (ipbf-ttImportStorageCost.handlingFee, iplIgnoreBlanks, INPUT-OUTPUT bf-storageCost.handlingFee).
    RUN pAssignValueD (ipbf-ttImportStorageCost.stack1High, iplIgnoreBlanks, INPUT-OUTPUT bf-storageCost.stack1High).
    RUN pAssignValueD (ipbf-ttImportStorageCost.stack2High, iplIgnoreBlanks, INPUT-OUTPUT bf-storageCost.stack2High).
    RUN pAssignValueD (ipbf-ttImportStorageCost.stack3High, iplIgnoreBlanks, INPUT-OUTPUT bf-storageCost.stack3High).
    RUN pAssignValueD (ipbf-ttImportStorageCost.stack4High, iplIgnoreBlanks, INPUT-OUTPUT bf-storageCost.stack4High).
    
    
    
    
    /*FIND FIRST bf-palletSize EXCLUSIVE-LOCK
        WHERE bf-palletSize.company EQ ipbf-ttImportStorageCost.Company
        AND bf-palletSize.location EQ ipbf-ttImportStorageCost.Location
        AND bf-palletSize.positions EQ ipbf-ttImportStorageCost.positions
        NO-ERROR.  
    IF NOT AVAILABLE bf-palletSize THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE bf-palletSize.
        ASSIGN 
            bf-palletSize.company = ipbf-ttImportStorageCost.Company
            .
    END.
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */
    
    RUN pAssignValueC (ipbf-ttImportStorageCost.Location,   iplIgnoreBlanks, INPUT-OUTPUT bf-palletSize.location).
    RUN pAssignValueI (ipbf-ttImportStorageCost.positions,  iplIgnoreBlanks, INPUT-OUTPUT bf-palletSize.positions).
    RUN pAssignValueD (ipbf-ttImportStorageCost.upToWidth,  iplIgnoreBlanks, INPUT-OUTPUT bf-palletSize.upToWidth).
    RUN pAssignValueD (ipbf-ttImportStorageCost.upToLength, iplIgnoreBlanks, INPUT-OUTPUT bf-palletSize.upToLength).    
    /* Set to round up if write blank and zero is selected */      */
        
    RELEASE bf-storageCost.
    //RELEASE bf-palletSize.
    
END PROCEDURE.
