
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

DEFINE TEMP-TABLE ttImportGL
    FIELD Company             AS CHARACTER 
    FIELD AccountNo           AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Account #"
    FIELD AccountDesc         AS CHARACTER FORMAT "x(50)" COLUMN-LABEL "Description"
    FIELD AccountType         AS CHARACTER FORMAT "x(1)" COLUMN-LABEL "Type"
        .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 1. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */

PROCEDURE pAddRecord:
    /*------------------------------------------------------------------------------
     Purpose: Accepts a Data Array, validates it and adds a temp-table record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcData AS CHARACTER NO-UNDO EXTENT.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdTempTableBuffer AS HANDLE.
    DEFINE VARIABLE cData AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportGL FOR ttImportGL.

    oplValid = YES.
    CREATE ttImportGL.
    ASSIGN 
        ttImportGL.Company = ipcCompany.
    FOR EACH ttImportMap
        WHERE ttImportMap.cType EQ 'GL':
        cData = ipcData[ttImportMap.iImportIndex].
        hdTempTableBuffer = TEMP-TABLE ttImportGL:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(ttImportMap.iIndex + giIndexOffset):HANDLE.
        CASE ttImportMap.cDataType:
            WHEN "integer" THEN 
                ASSIGN hdTempTableBuffer:BUFFER-VALUE = INT(cData).
            WHEN "logical" THEN 
                ASSIGN hdTempTableBuffer:BUFFER-VALUE = cData BEGINS "Y".
            WHEN "decimal" THEN 
                ASSIGN hdTempTableBuffer:BUFFER-VALUE = DEC(cDaTa).
            WHEN "date" THEN 
                ASSIGN hdTempTableBuffer:BUFFER-VALUE = DATE(cData). 
            OTHERWISE 
                ASSIGN hdTempTableBuffer:BUFFER-VALUE = cData.
        END CASE.              
    END.
    ttImportGL.AccountType = SUBSTRING(TRIM(ttImportGL.AccountType),1,1).
    IF oplValid THEN 
    DO:
        IF ttImportGL.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ttImportGL.AccountNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Account #".
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportGL NO-LOCK 
            WHERE bf-ttImportGL.Company EQ ttImportGL.Company
            AND bf-ttImportGL.AccountNo EQ ttImportGL.AccountNo
            AND ROWID(bf-ttImportGL) NE ROWID(ttImportGL)
            NO-ERROR.
        IF AVAILABLE bf-ttImportGL THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST account NO-LOCK 
            WHERE account.company EQ ttImportGL.Company
            AND account.actnum EQ ttImportGL.AccountNo
            NO-ERROR .
        IF AVAILABLE account THEN 
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
    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF LOOKUP(ttImportGL.AccountType,"A,C,E,L,R,T") EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote = "Invalid Account Type"
                .
    END.
    IF NOT oplValid THEN DELETE ttImportGL.
    
END PROCEDURE.

PROCEDURE pExportData:
/*------------------------------------------------------------------------------
 Purpose:  Runs the Export Data Program for ShipTo
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

PROCEDURE pInitialize:
    /*------------------------------------------------------------------------------
     Purpose: Initializes the specific Column Mapping for ShipTos   
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLoadFile AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFields    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLabels    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDataTypes AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWidths    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormats   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    DEFINE VARIABLE iIndexStart AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttImportGL.
    EMPTY TEMP-TABLE ttImportMap.
    
    iIndexStart = 1 + giIndexOffset.
    cWidths    = "80,150,60"
                  .

    IF ipcLoadFile EQ '' THEN 
    DO:
        ASSIGN 
            cFields = ""
            cDataTypes = ""
            cFormats = ""
            cLabels = ""
            .
        DO iIndex = iIndexStart TO TEMP-TABLE ttImportGL:DEFAULT-BUFFER-HANDLE:NUM-FIELDS:
            ASSIGN 
                cFields = cFields + TEMP-TABLE ttImportGL:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):NAME + ","
                cDataTypes = cDataTypes + TEMP-TABLE ttImportGL:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):DATA-TYPE + ","
                cFormats = cFormats + TEMP-TABLE ttImportGL:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):FORMAT + ","
                cLabels = cLabels + TEMP-TABLE ttImportGL:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):COLUMN-LABEL + ","
                .
            
        
        END.
        ASSIGN 
            cFields = TRIM(cFields,",")
            cDataTypes = TRIM(cDataTypes,",")
            cFormats = TRIM(cFormats,",")
            cLabels = TRIM(cLabels,",")
            .
        DO iIndex = 1 TO NUM-ENTRIES(cFields):
            CREATE ttImportMap.
            ASSIGN 
                ttImportMap.cType        = "GL"
                ttImportMap.cLabel       = ENTRY(iIndex,cFields)
                ttImportMap.iIndex       = iIndex
                ttImportMap.iImportIndex = iIndex
                ttImportMap.cDataType    = ENTRY(iIndex,cDataTypes)
                ttImportMap.cColumnLabel = ENTRY(iIndex,cLabels)
                ttImportMap.cColumnFormat = ENTRY(iIndex,cFormats)
                .
                IF iIndex LE NUM-ENTRIES(cWidths)  THEN 
                    ttImportMap.iColumnWidth = INT(ENTRY(iIndex,cWidths)).
        END. 
    
    END.
    ELSE 
    DO:
    /*Load from Config File provided*/
    END.

END PROCEDURE.

PROCEDURE pProcessImport:
/*------------------------------------------------------------------------------
 Purpose: Processes the temp-table already loaded and returns counts
 Notes:
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER opiUpdated AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opiAdded AS INTEGER NO-UNDO.

FOR EACH ttImportGL NO-LOCK:
    IF ttImportGL.AccountNo EQ "" THEN NEXT.
    FIND FIRST account EXCLUSIVE-LOCK 
        WHERE account.company EQ ttImportGL.Company
        AND account.actnum EQ ttImportGL.AccountNo
        NO-ERROR.
    IF NOT AVAILABLE account THEN DO:
        opiAdded = opiAdded + 1.
        CREATE account.
        ASSIGN
            account.company = ttImportGL.Company
            account.actnum = ttImportGL.AccountNo
            . 
    END.
    opiUpdated = opiUpdated + 1.
    ASSIGN 
        account.dscr = ttImportGL.AccountDesc
        account.type = ttImportGL.AccountType
        .
    
END.
opiUpdated = opiUpdated - opiAdded.

END PROCEDURE.

