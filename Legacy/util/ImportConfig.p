/*------------------------------------------------------------------------
    File        : ImportConfig.p
    Purpose     : 
    Syntax      :
    Description : Import Program (Persistent) for Configuring and Processing the Import for Configuration Settings and NK1s	
    Author(s)   : BV
    Created     : Sun July 29 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportConfig
    FIELD Company         AS CHARACTER 
    FIELD Location        AS CHARACTER 
    FIELD cConfigName     AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Config Name" HELP "Required - Size:30" 
    FIELD cConfigDesc     AS CHARACTER FORMAT "X(40)" COLUMN-LABEL "Config Description" HELP "Optional - Size:40"
    FIELD cCategory       AS CHARACTER FORMAT "X(16)" COLUMN-LABEL "Category" HELP "Optional - Size:16" 
    FIELD cCategorySub    AS CHARACTER FORMAT "X(16)" COLUMN-LABEL "Sub-category" HELP "Optional - Size:16"
    FIELD cModule         AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Module" HELP "Optional - Size:30"
    FIELD cAllowsContext  AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Allows Context?" HELP "Optional - Yes or No"
    FIELD cCharVal        AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Character Value" HELP "Optional - Size:30"
    FIELD cCharValDef     AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Character Value - Default" HELP "Optional - Size:30"
    FIELD cCharValDesc    AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Character Value - Description" HELP "Optional - Size:30"
    FIELD dtDateVal       AS DATE FORMAT "99/99/9999" COLUMN-LABEL "Date Value" HELP "Optional - Date"
    FIELD dtDateValDef    AS DATE FORMAT "99/99/9999" COLUMN-LABEL "Date Value - Default" HELP "Optional - Date"
    FIELD cDateValDesc    AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Date Value - Description" HELP "Optional - Size:30"
    FIELD dDecimalVal     AS DECIMAL   FORMAT ">,>>>,>>9.99" COLUMN-LABEL "Decimal Value" HELP "Optional - Decimal"
    FIELD dDecimalValDef  AS DECIMAL   FORMAT ">,>>>,>>9.99" COLUMN-LABEL "Decimal Value - Default " HELP "Optional - Decimal"
    FIELD cDecimalValDesc AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Decimal Value - Description" HELP "Optional - Size:30"
    FIELD iIntegerVal     AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Integer Value" HELP "Optional - Integer"
    FIELD iIntegerValDef  AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Integer Value - Default" HELP "Optional - Integer"
    FIELD cIntegerValDesc AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Integer Value - Description" HELP "Optional - Size:30"
    FIELD cLogicalVal     AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Logical Value" HELP "Optional - Size:3"
    FIELD cLogicalValDef  AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Logical Value - Default" HELP "Optional - Size:3"
    FIELD cLogicalValDesc AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Logical Value - Description" HELP "Optional - Size:30"
    
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportConfig"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportConfig FOR ttImportConfig.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportConfig.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportConfig.cConfigName EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Config Name".
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST sys-ctrl NO-LOCK 
            WHERE sys-ctrl.company EQ ipbf-ttImportConfig.Company
            AND sys-ctrl.name EQ ipbf-ttImportConfig.cConfigName
            NO-ERROR .
        IF AVAILABLE sys-ctrl THEN 
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
    DEFINE PARAMETER BUFFER ipbf-ttImportConfig FOR ttImportConfig.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
     
    FIND FIRST sys-ctrl EXCLUSIVE-LOCK
        WHERE sys-ctrl.company EQ ipbf-ttImportConfig.Company
        AND sys-ctrl.name EQ ipbf-ttImportConfig.cConfigName
        NO-ERROR.  
    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        CREATE sys-ctrl.
        ASSIGN 
            sys-ctrl.company = ipbf-ttImportConfig.Company
            sys-ctrl.name = ipbf-ttImportConfig.cConfigName
            .
    END.
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */
    RUN pAssignValueC (ipbf-ttImportConfig.cConfigDesc, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.descrip).
    RUN pAssignValueC (ipbf-ttImportConfig.cCategory, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.category).
    RUN pAssignValueC (ipbf-ttImportConfig.cCategorySub, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.subcategory). 
    RUN pAssignValueC (ipbf-ttImportConfig.cModule, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.module).
    RUN pAssignValueCToL (ipbf-ttImportConfig.cAllowsContext, "YES", iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.allowsContext).
    RUN pAssignValueC (ipbf-ttImportConfig.cCharVal, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.char-fld).
    RUN pAssignValueC (ipbf-ttImportConfig.cCharValDef, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.char_field_default).
    RUN pAssignValueC (ipbf-ttImportConfig.cCharValDesc, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.char-fld_descrip).
    RUN pAssignValueDate (ipbf-ttImportConfig.dtDateVal, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.date-fld).
    RUN pAssignValueDate (ipbf-ttImportConfig.dtDateValDef, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.date-fld_default).
    RUN pAssignValueC (ipbf-ttImportConfig.cDateValDesc, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.date-fld_descrip).
    RUN pAssignValueD (ipbf-ttImportConfig.dDecimalVal, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.dec-fld).
    RUN pAssignValueD (ipbf-ttImportConfig.dDecimalValDef, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.dec-fld_default).
    RUN pAssignValueC (ipbf-ttImportConfig.cDecimalValDesc, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.dec-fld_descrip).
    RUN pAssignValueI (ipbf-ttImportConfig.iIntegerVal, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.int-fld).
    RUN pAssignValueI (ipbf-ttImportConfig.iIntegerValDef, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.int-fld_default).
    RUN pAssignValueC (ipbf-ttImportConfig.cIntegerValDesc, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.int-fld_descrip).
    RUN pAssignValueCToL (ipbf-ttImportConfig.cLogicalVal, "YES", iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.log-fld).
    RUN pAssignValueCToL (ipbf-ttImportConfig.cLogicalValDef, "YES", iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.log-fld_default).
    RUN pAssignValueC (ipbf-ttImportConfig.cLogicalValDesc, iplIgnoreBlanks, INPUT-OUTPUT sys-ctrl.log-fld_descrip).
    .

END PROCEDURE.