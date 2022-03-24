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
    FIELD PoNoGroup     AS CHARACTER
    FIELD Location      AS CHARACTER FORMAT "x(5)"       COLUMN-LABEL "Location"     HELP "Required. Must be valid - Size:5"
    FIELD positions     AS CHARACTER FORMAT "x(20)"      COLUMN-LABEL "Positions"    HELP "Optional - Integer or <AUTO> to auto-number.  Use <AUTO>#### where # is a unique group number. "
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
DEFINE VARIABLE gcAutoIndicator AS CHARACTER NO-UNDO INITIAL "<AUTO>".
DEFINE VARIABLE gcLocation AS CHARACTER NO-UNDO .
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/

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
    
    DEFINE VARIABLE dValidWidthRangeBegin  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dValidWidthRangeEnd    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dValidLengthRangeBegin AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dValidLengthRangeEnd   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iCheckPositionNo       AS INTEGER   NO-UNDO.    
    DEFINE VARIABLE lAutoNumber            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cPositionNumber        AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE cPositionNoGroup       AS CHARACTER NO-UNDO.
    
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
        IF ipbf-ttImportStorageCost.upToWidth GT 9999.99 THEN
            ASSIGN 
                oplValid = NO
                opcNote  = "UpTo Width cannot be greater than 9999.99".
        IF ipbf-ttImportStorageCost.upToLength GT 9999.99 THEN
            ASSIGN 
                oplValid = NO
                opcNote  = "UpTo Length cannot be greater than 9999.99".
                
        IF ipbf-ttImportStorageCost.upToWidth LT 1 AND ipbf-ttImportStorageCost.upToWidth NE 0 THEN
            ASSIGN 
                oplValid = NO
                opcNote  = "UpTo Width cannot be less than 1".
        IF ipbf-ttImportStorageCost.upToLength LT 1 AND ipbf-ttImportStorageCost.upToLength NE 0 THEN
            ASSIGN 
                oplValid = NO
                opcNote  = "UpTo Length cannot be less than 1".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportStorageCost.positions BEGINS gcAutoIndicator THEN DO:
            opcNote = "Add Record - Auto Increment Position#"
            .
        END. 
        ELSE do:
            FIND FIRST storageCost NO-LOCK 
                WHERE storageCost.company   EQ ipbf-ttImportStorageCost.Company
                  AND storageCost.location  EQ ipbf-ttImportStorageCost.Location
                  AND storageCost.positions EQ integer(ipbf-ttImportStorageCost.positions)
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
    END.
    
    /*Field level validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportStorageCost.Location NE "" THEN 
            RUN pIsValidWarehouse IN hdValidator (ipbf-ttImportStorageCost.Location, NO, ipbf-ttImportStorageCost.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
    END.
    
    IF oplValid THEN
    DO:   
        ASSIGN 
        cPositionNoGroup = ""
        lAutoNumber = NO
        cPositionNumber = ipbf-ttImportStorageCost.positions
        .
        
        IF cPositionNumber BEGINS gcAutoIndicator THEN DO:
        /*Auto numbering logic*/
                        
            /*Get the PoNoGroup as string to the right of the indicator*/
            IF LENGTH(cPositionNumber) NE LENGTH(gcAutoIndicator) THEN 
                cPositionNoGroup = SUBSTRING(cPositionNumber,LENGTH(gcAutoIndicator) + 1, LENGTH(cPositionNumber) - LENGTH(gcAutoIndicator)).
            IF cPositionNoGroup NE "" THEN 
                FIND FIRST bf-ttImportStorageCost NO-LOCK
                     WHERE bf-ttImportStorageCost.PoNoGroup EQ cPositionNoGroup
                    NO-ERROR.
            IF AVAILABLE bf-ttImportStorageCost THEN
                ipbf-ttImportStorageCost.positions = bf-ttImportStorageCost.positions.
            ELSE 
                lAutoNumber = YES.
        END.  
        
        IF lAutoNumber OR ipbf-ttImportStorageCost.positions EQ "" THEN
        DO:
           RUN pGetNextPositionFromTT(INPUT ipbf-ttImportStorageCost.Company, ipbf-ttImportStorageCost.Location, OUTPUT iCheckPositionNo).
           ipbf-ttImportStorageCost.positions = STRING(iCheckPositionNo).
        END.
        IF lAutoNumber AND cPositionNoGroup NE "" THEN
        ipbf-ttImportStorageCost.PoNoGroup = cPositionNoGroup.
        
        IF INTEGER(ipbf-ttImportStorageCost.positions) GT 9 THEN
        DO:
            ASSIGN 
                 oplValid = NO
                 opcNote  = "Maximum positions is 9, entry rejected" .            
        END.
        
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
    
    DEFINE VARIABLE cPositionNumber AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPositionNoGroup AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAutoNumber AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lNewGroup AS LOGICAL NO-UNDO.     
    DEFINE VARIABLE iCheckPositionNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE lDeleteData AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-storageCost FOR storageCost.
    DEFINE BUFFER bf-palletSize FOR palletSize.
    DEFINE BUFFER bf-ttImportStorageCost FOR ttImportStorageCost.
        
    IF lookup(ipbf-ttImportStorageCost.Location,gcLocation) EQ 0 THEN
    ASSIGN
        gcLocation = gcLocation + ipbf-ttImportStorageCost.Location + ","
        lDeleteData = YES.
    ELSE lDeleteData = NO.  
    
    IF lDeleteData THEN
    DO:
        FOR EACH bf-storageCost EXCLUSIVE-LOCK
            WHERE bf-storageCost.company EQ ipbf-ttImportStorageCost.Company
              AND bf-storageCost.location EQ ipbf-ttImportStorageCost.Location:
              
              FOR EACH bf-palletSize EXCLUSIVE-LOCK
                  WHERE bf-palletSize.company EQ ipbf-ttImportStorageCost.Company
                    AND bf-palletSize.location EQ ipbf-ttImportStorageCost.Location
                    AND bf-palletSize.positions EQ bf-storageCost.positions:
                    DELETE  bf-palletSize.
              END.
              DELETE bf-storageCost.
        END.         
    END.
    
    ASSIGN 
        cPositionNoGroup = ""
        lAutoNumber = NO
        cPositionNumber = ipbf-ttImportStorageCost.positions
        .
     iCheckPositionNo = INT(ipbf-ttImportStorageCost.positions) NO-ERROR.    
        
    IF cPositionNumber BEGINS gcAutoIndicator THEN DO:
        /*Auto numbering logic*/
        
        /*Get the PoNoGroup as string to the right of the indicator*/
        IF LENGTH(cPositionNumber) NE LENGTH(gcAutoIndicator) THEN 
            cPositionNoGroup = SUBSTRING(cPositionNumber,LENGTH(gcAutoIndicator) + 1, LENGTH(cPositionNumber) - LENGTH(gcAutoIndicator)).
        IF cPositionNoGroup NE "" THEN 
            FIND FIRST bf-ttImportStorageCost NO-LOCK
                 WHERE bf-ttImportStorageCost.PoNoGroup EQ cPositionNoGroup
                NO-ERROR.
        IF AVAILABLE bf-ttImportStorageCost THEN
            cPositionNumber = bf-ttImportStorageCost.positions.
        ELSE 
            lAutoNumber = YES.
    END.       
        
    FIND FIRST bf-storageCost EXCLUSIVE-LOCK
        WHERE bf-storageCost.company EQ ipbf-ttImportStorageCost.Company
        AND bf-storageCost.location EQ ipbf-ttImportStorageCost.Location
        AND bf-storageCost.positions EQ integer(cPositionNumber)
        NO-ERROR.  
    IF NOT AVAILABLE bf-storageCost THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        IF iCheckPositionNo EQ 0 OR iCheckPositionNo EQ ? THEN
        RUN pGetNextPosition(INPUT ipbf-ttImportStorageCost.Company, ipbf-ttImportStorageCost.Location, OUTPUT iCheckPositionNo).
        
        CREATE bf-storageCost.
        ASSIGN 
            bf-storageCost.company = ipbf-ttImportStorageCost.Company
            bf-storageCost.positions = iCheckPositionNo  
            bf-storageCost.location  = ipbf-ttImportStorageCost.Location
            .
        IF lAutoNumber AND cPositionNoGroup NE "" THEN DO:
           FIND CURRENT ipbf-ttImportStorageCost EXCLUSIVE-LOCK.
             ASSIGN 
                ipbf-ttImportStorageCost.PoNoGroup = cPositionNoGroup
                ipbf-ttImportStorageCost.positions = STRING(bf-storageCost.positions)                 
                .
           FIND CURRENT ipbf-ttImportStorageCost NO-LOCK.    
        END.    
    END.     
    RUN pAssignValueD (ipbf-ttImportStorageCost.handlingFee, iplIgnoreBlanks, INPUT-OUTPUT bf-storageCost.handlingFee).
    RUN pAssignValueD (ipbf-ttImportStorageCost.stack1High, iplIgnoreBlanks, INPUT-OUTPUT bf-storageCost.stack1High).
    RUN pAssignValueD (ipbf-ttImportStorageCost.stack2High, iplIgnoreBlanks, INPUT-OUTPUT bf-storageCost.stack2High).
    RUN pAssignValueD (ipbf-ttImportStorageCost.stack3High, iplIgnoreBlanks, INPUT-OUTPUT bf-storageCost.stack3High).
    RUN pAssignValueD (ipbf-ttImportStorageCost.stack4High, iplIgnoreBlanks, INPUT-OUTPUT bf-storageCost.stack4High).
       
    IF ipbf-ttImportStorageCost.upToWidth NE 0 OR ipbf-ttImportStorageCost.upToLength NE 0 THEN 
    DO:
        
        CREATE bf-palletSize.
        ASSIGN 
            bf-palletSize.company = ipbf-ttImportStorageCost.Company
            bf-palletSize.location = ipbf-ttImportStorageCost.Location
            bf-palletSize.positions = bf-storageCost.positions
            .
            RUN pAssignValueD (ipbf-ttImportStorageCost.upToWidth,  iplIgnoreBlanks, INPUT-OUTPUT bf-palletSize.upToWidth).
            RUN pAssignValueD (ipbf-ttImportStorageCost.upToLength, iplIgnoreBlanks, INPUT-OUTPUT bf-palletSize.upToLength).
    END.
            
    RELEASE bf-storageCost.
    RELEASE bf-palletSize.    
END PROCEDURE.


PROCEDURE pGetNextPosition PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiPosition AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPosition AS INTEGER NO-UNDO.
    
    FOR EACH storageCost NO-LOCK
        WHERE storageCost.company EQ ipcCompany
        AND storageCost.location EQ ipcLocation        
        BY storageCost.positions DESC:
        iPosition = storageCost.positions.
        LEAVE.
    END.
    opiPosition = iPosition + 1.    
    
END PROCEDURE.    

PROCEDURE pGetNextPositionFromTT PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiPosition AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPosition AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-ttImportStorageCost FOR ttImportStorageCost.
    Main-loop:
    FOR EACH bf-ttImportStorageCost NO-LOCK
        WHERE bf-ttImportStorageCost.company EQ ipcCompany
        AND bf-ttImportStorageCost.location EQ ipcLocation          
        BY bf-ttImportStorageCost.positions DESC:
        IF bf-ttImportStorageCost.positions BEGINS gcAutoIndicator OR bf-ttImportStorageCost.positions EQ "" THEN NEXT Main-loop.
        iPosition = integer(bf-ttImportStorageCost.positions) NO-ERROR.
        LEAVE.
    END.
    opiPosition = iPosition + 1.    
    
END PROCEDURE.    
