
/*------------------------------------------------------------------------
    File        : ImportProcs.i
    Purpose     : 

    Syntax      :

    Description : Common code to build the ImportMap file for a supplied ImportType.
Import temp-table is primary argument

    Author(s)   : BV
    Created     : Tue Mar 13 22:48:01 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

PROCEDURE pInitialize:
    /*------------------------------------------------------------------------------
     Purpose: Initializes the specific Column Mapping for Estimates   
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLoadFile AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFields     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLabels     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDataTypes  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormats    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHelps      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndexStart AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE {&ImportTempTable}.
    EMPTY TEMP-TABLE ttImportMap.
    
    iIndexStart = 1 + giIndexOffset.
    
    IF ipcLoadFile EQ '' THEN 
    DO:
        ASSIGN 
            cFields    = ""
            cDataTypes = ""
            cFormats   = ""
            cLabels    = ""
            cHelps     = ""
            .
        DO iIndex = iIndexStart TO TEMP-TABLE {&ImportTempTable}:DEFAULT-BUFFER-HANDLE:NUM-FIELDS:
            ASSIGN 
                cFields    = cFields + TEMP-TABLE {&ImportTempTable}:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):NAME + ","
                cDataTypes = cDataTypes + TEMP-TABLE {&ImportTempTable}:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):DATA-TYPE + ","
                cFormats   = cFormats + TEMP-TABLE {&ImportTempTable}:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):FORMAT + ","
                cLabels    = cLabels + TEMP-TABLE {&ImportTempTable}:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):COLUMN-LABEL + ","
                cHelps     = cHelps + TEMP-TABLE {&ImportTempTable}:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):HELP + ",".
        END.
        ASSIGN 
            cFields    = TRIM(cFields,",")
            cDataTypes = TRIM(cDataTypes,",")
            cFormats   = TRIM(cFormats,",")
            cLabels    = TRIM(cLabels,",")
            cHelps     = TRIM(cHelps,",")
            .
        DO iIndex = 1 TO NUM-ENTRIES(cFields):
            CREATE ttImportMap.
            ASSIGN 
                ttImportMap.cType         = "{&ImportTempTable}"
                ttImportMap.cLabel        = ENTRY(iIndex,cFields)
                ttImportMap.iIndex        = iIndex
                ttImportMap.iImportIndex  = iIndex
                ttImportMap.cDataType     = ENTRY(iIndex,cDataTypes)
                ttImportMap.cColumnLabel  = ENTRY(iIndex,cLabels)
                ttImportMap.cColumnFormat = ENTRY(iIndex,cFormats)
                ttImportMap.cHelp         = ENTRY(iIndex,cHelps)
                ttImportMap.iColumnWidth  = INT(ROUND(LENGTH(ttImportMap.cColumnLabel) * 6.5 , 0))
                .
        END. 
    
    END.
    ELSE 
    DO:
    /*Load from Config File provided*/
    END.

END PROCEDURE.

PROCEDURE pAddRecord:
    /*------------------------------------------------------------------------------
     Purpose: Accepts a Data Array, validates it and adds a temp-table record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcData AS CHARACTER NO-UNDO EXTENT.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO INIT YES.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdTempTableBuffer AS HANDLE.
    DEFINE VARIABLE cData             AS CHARACTER NO-UNDO.
        
    CREATE {&ImportTempTable}.
    ASSIGN 
        {&ImportTempTable}.Company = ipcCompany
        {&ImportTempTable}.Location = ipcLocation
        .
    FOR EACH ttImportMap
        WHERE ttImportMap.cType EQ  "{&ImportTempTable}":
        cData = ipcData[ttImportMap.iImportIndex].
        hdTempTableBuffer = TEMP-TABLE {&ImportTempTable}:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(ttImportMap.iIndex + giIndexOffset):HANDLE.
        CASE ttImportMap.cDataType:
            WHEN "integer" THEN DO:
                ASSIGN 
                    hdTempTableBuffer:BUFFER-VALUE = INTEGER(cData) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    ASSIGN 
                        oplValid = NO
                        opcNote = "Non Integer in "+ hdTempTableBuffer:COLUMN-LABEL
                        ERROR-STATUS:ERROR = NO
                        .
            END.
            WHEN "logical" THEN DO:
                ASSIGN 
                    hdTempTableBuffer:BUFFER-VALUE = cData BEGINS "Y" NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    ASSIGN 
                        oplValid = NO
                        opcNote = "Non Logical in "+ hdTempTableBuffer:COLUMN-LABEL
                        ERROR-STATUS:ERROR = NO
                        .
            END.
            WHEN "decimal" THEN DO: 
                ASSIGN 
                    hdTempTableBuffer:BUFFER-VALUE = DECIMAL(cDaTa) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    ASSIGN 
                        oplValid = NO
                        opcNote = "Non Decimal in "+ hdTempTableBuffer:COLUMN-LABEL
                        ERROR-STATUS:ERROR = NO
                        .
            END.
            WHEN "date" THEN DO:
                ASSIGN 
                    hdTempTableBuffer:BUFFER-VALUE = DATE(cData) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    ASSIGN 
                        oplValid = NO
                        opcNote = "Non Date in "+ hdTempTableBuffer:COLUMN-LABEL
                        ERROR-STATUS:ERROR = NO
                        .
            END. 
            OTHERWISE 
            ASSIGN 
                hdTempTableBuffer:BUFFER-VALUE = cData.
        END CASE.   
        IF NOT oplValid THEN LEAVE.           
    END.
    IF oplValid THEN  
        RUN pValidate(BUFFER {&ImportTempTable}, iplUpdateDuplicates, iplFieldValidation, OUTPUT oplValid, OUTPUT opcNote).   
    IF NOT oplValid THEN DELETE {&ImportTempTable}.
    
END PROCEDURE.

PROCEDURE pProcessImport:
    /*------------------------------------------------------------------------------
     Purpose: Processes the temp-table already loaded and returns counts
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiUpdated AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiAdded AS INTEGER NO-UNDO.
        
    FOR EACH {&ImportTempTable} NO-LOCK:
        opiUpdated = opiUpdated + 1.
        RUN pProcessRecord(BUFFER {&ImportTempTable}, INPUT-OUTPUT opiAdded).
    END.
    opiUpdated = opiUpdated - opiAdded.

END PROCEDURE.

PROCEDURE pExportData:
    /*------------------------------------------------------------------------------
     Purpose:  Runs the Export Data Program
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriContext AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iopcFile AS CHARACTER NO-UNDO.


END PROCEDURE.