
/*------------------------------------------------------------------------
    File        : ImportProcs.p
    Purpose     : 

    Syntax      :

    Description : Builds ttInputData based on valid input file	

    Author(s)   : BV
    Created     : Wed Nov 22 17:40:38 EST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE gcType               AS CHARACTER NO-UNDO.
DEFINE VARIABLE ghdImportProcForType AS HANDLE    NO-UNDO.

{util\ttImport.i SHARED}

DEFINE STREAM sImport.
DEFINE STREAM sTemplate.
DEFINE STREAM sLog.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fConvertFileName RETURNS CHARACTER 
    ( ipcFileName AS CHARACTER ) FORWARD.

FUNCTION fIsExcel RETURNS LOGICAL 
    ( ipcFileName AS CHARACTER ) FORWARD.


/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pConvertExceltoCSV:
    /*------------------------------------------------------------------------------
     Purpose: Accepts a file and converts it to .csv
     Notes: Returns the new file location
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcInputFile AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcOutputFile AS CHARACTER NO-UNDO INIT "".

    DEFINE VARIABLE chExcel     AS COMPONENT-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorkBook  AS COMPONENT-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorkSheet AS COMPONENT-HANDLE NO-UNDO.

    IF NOT fIsExcel(ipcInputFile) THEN 
    DO:
        opcOutputFile = ipcInputFile.
        RETURN.
    END.
    /* Start Excel */
    CREATE "Excel.Application" chExcel.
    ASSIGN 
        chExcel:Visible = FALSE.
  
    /* Open the file. */
    chExcel:Workbooks:Open(ipcInputFile,2,TRUE,,,,TRUE).

    /* Get the sheet. */
    ASSIGN 
        chWorkbook = chExcel:WorkBooks:Item(1).
    /*         chWorkSheet = chExcel:Sheets:Item(1).*/
  
    /* Convert the filename. */
    ASSIGN 
        opcOutputFile = fConvertFileName(ipcInputFile). 

    /* Delete if already exists. */
    OS-COMMAND SILENT DEL VALUE(opcOutputFile).
    /*  IF SEARCH(pcInFile) <> ? THEN
        OS-DELETE value(pcInfile) NO-ERROR. */

    /* Turn off alerts */
    chExcel:ScreenUpdating = FALSE.
    chExcel:DisplayAlerts = FALSE.

    /* Save the new file in csv format. */
    chWorkBook:SaveAs(opcOutputFile,6,,,,,,, TRUE).

    /* Close the workbook. */
    chWorkBook:Close().
  
    /* Release objects. */
    RELEASE OBJECT chWorkSheet NO-ERROR.
    RELEASE OBJECT chWorkBook NO-ERROR.

    /* Quit Excel */
    chExcel:QUIT.
    RELEASE OBJECT chExcel.

    RETURN.


END PROCEDURE.

PROCEDURE pGenerateLog:
    /*------------------------------------------------------------------------------
     Purpose: Generates Log for Active Import Data Set
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLogFile AS CHARACTER NO-UNDO.

    OUTPUT STREAM sLog TO VALUE(ipcLogFile).

    FOR EACH ttImportData:
        EXPORT STREAM sLog DELIMITER "," 
            ttImportData.lValid
            ttImportData.cImportNote
            ttImportData.cData
            .
    END.
    OUTPUT STREAM sLog CLOSE.

END PROCEDURE.

PROCEDURE pGenerateTemplate:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplShellOnly AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipriContext AS ROWID NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcFile AS CHARACTER NO-UNDO.



    IF iplShellOnly THEN 
    DO:
        OUTPUT STREAM sTemplate TO VALUE(iopcFile). 
        FOR EACH ttImportMap:
            PUT STREAM sTemplate UNFORMATTED ttImportMap.cColumnLabel ',' .
        END.
        PUT STREAM sTemplate SKIP.
        OUTPUT STREAM sTemplate CLOSE.
    END.
    ELSE 
    DO:   
        RUN pExportData IN ghdImportProcForType  (INPUT ipriContext, INPUT-OUTPUT iopcFile). 
    END.
    


END PROCEDURE.

PROCEDURE pInitializeType:
    /*------------------------------------------------------------------------------
     Purpose:  Initializes the ttImportMap for a given Type
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTypeToInit AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttImportMap.
    CASE ipcTypeToInit:
        WHEN "ShipTo" THEN
        RUN util/ImportShipTo.p PERSISTENT SET ghdImportProcForType.
        WHEN "Cash" THEN
        RUN util/ImportCash.p PERSISTENT SET ghdImportProcForType.
        WHEN "Cust" THEN
        RUN util/ImportCust.p PERSISTENT SET ghdImportProcForType.
        WHEN "Est" THEN
        RUN util/ImportEstimate.p PERSISTENT SET ghdImportProcForType.
        WHEN "Vend" THEN
        RUN util/ImportVend.p PERSISTENT SET ghdImportProcForType.
        WHEN "GL" THEN
        RUN util/ImportGL.p PERSISTENT SET ghdImportProcForType.
        WHEN "FG" THEN
        RUN util/ImportFG.p PERSISTENT SET ghdImportProcForType.
        WHEN "AP" THEN
        RUN util/ImportAP.p PERSISTENT SET ghdImportProcForType.
        WHEN "AR" THEN
        RUN util/ImportAR.p PERSISTENT SET ghdImportProcForType.
        
    END CASE.
    IF NOT CAN-FIND(FIRST ttImportMap WHERE ttImportMap.cType EQ ipcTypeToInit) THEN
        RUN pInitialize IN ghdImportProcForType("").


END PROCEDURE.

PROCEDURE pLoad:
    /*------------------------------------------------------------------------------
     Purpose: Processes the file and returns valid logical if successful
     Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplHeader AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE iCount AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cData  AS CHARACTER NO-UNDO EXTENT 200.

    EMPTY TEMP-TABLE ttImportData.
    oplSuccess = YES.
    IF SEARCH(ipcFile) NE ? THEN 
    DO:
        INPUT STREAM sImport FROM VALUE(ipcFile).
        REPEAT:
            iCount = iCount + 1.
            IMPORT STREAM sImport DELIMITER ','
                cData
                . 
            CREATE ttImportData.
            ASSIGN
                ttImportData.cData  = cData 
                ttImportData.lValid = YES 
                ttImportData.iCount = iCount.    
                
            IF TRIM(ttImportData.cData[1]) EQ '' THEN
                ASSIGN 
                    ttImportData.lValid      = NO 
                    ttImportData.cImportNote = 'No Data in First Column'
                    .
            ELSE 
            DO:
                IF (iplHeader AND iCount GT 1) OR NOT iplHeader THEN 
                    RUN pAddRecord IN ghdImportProcForType(ipcCompany,
                        ttImportData.cData, 
                        iplUpdateDuplicates,
                        iplFieldValidation,
                        OUTPUT ttImportData.lValid, 
                        OUTPUT ttImportDAta.cImportNote).
            END.
        END.
        OUTPUT STREAM sImport CLOSE.
    END.
    FOR EACH ttImportData
        WHERE NOT ttImportData.lValid
        AND ttImportData.cImportNote EQ '':
        
        DELETE ttImportData.
    END. 
    IF iplHeader THEN 
    DO:
        FIND FIRST ttImportData
            WHERE ttImportData.iCount EQ 1
            NO-ERROR.
        IF AVAILABLE ttImportData THEN 
            ASSIGN 
                ttImportData.lValid      = NO 
                ttImportData.cImportNote = 'Header Row'
                ttImportData.lheader     = YES. 
    END.
    
END PROCEDURE.

PROCEDURE pProcessImport:
    /*------------------------------------------------------------------------------
     Purpose: Processes the Import Data TempTable for the active type
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiUpdateCount AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiAddCount AS INTEGER NO-UNDO.


    RUN pProcessImport IN ghdImportProcForType (OUTPUT opiUpdateCount, OUTPUT opiAddCount).

END PROCEDURE.

PROCEDURE pSetType:
    /*------------------------------------------------------------------------------
     Purpose:  Assigns the Type "property" for the procedure and adds Type Records to ttImportMap
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ipcTypeToSet AS CHARACTER NO-UNDO.

    ASSIGN 
        gcType = ipcTypeToSet
        .
    RUN pInitializeType(ipcTypeToSet).
    
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fConvertFileName RETURNS CHARACTER 
    ( INPUT ipcFileName AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cBaseFileName AS CHARACTER NO-UNDO.


    ASSIGN 
        cBaseFileName = TRIM(ENTRY(1,ipcFileName,".") + ".csv").

    RETURN cBaseFileName.   /* Function return value. */
		
END FUNCTION.

FUNCTION fIsExcel RETURNS LOGICAL 
    ( ipcFileName AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Takes file name and determines if it is Excel 
     Notes: Returns YES if Excel file
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cExt AS CHARACTER NO-UNDO INIT "".

    ASSIGN 
        cExt = TRIM(ENTRY(2,ipcFileName,".")).

    IF LOOKUP(cExt, "xls,xlsx") > 0 THEN 
        RETURN YES.
    ELSE
        RETURN NO.   /* Function return value. */
		
END FUNCTION.



    