
/*------------------------------------------------------------------------
    File        : ImportProcs.p
    Purpose     : Common procedures for Import - Pass Through to Typed Importers

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

DEFINE TEMP-TABLE ttFile
    FIELD cFile AS CHARACTER 
    FIELD cType AS CHARACTER
    .
    
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

PROCEDURE pBuildFileList:
    /*------------------------------------------------------------------------------
     Purpose: Recursively processes all sub folders below 
        a given directly and builds a list of all files of a particular extension in
        that directory
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcStartingDirectory AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcExtension AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFilePath AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAttrib   AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE iExtensionLength AS INTEGER NO-UNDO. 
    DEFINE VARIABLE iCharLength AS INTEGER NO-UNDO.
    
    iExtensionLength = LENGTH(ipcExtension).
    INPUT FROM OS-DIR (ipcStartingDirectory).
    REPEAT:
        IMPORT cFileName.
        IF cFileName = '.' OR cFileName = '..' OR cFileName = ? THEN NEXT.
        FILE-INFO:FILE-NAME = ipcStartingDirectory + cFileName.
        IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN DO:
            iCharLength = LENGTH(cFileName).
            IF SUBSTRING(cFileName, iCharLength - iExtensionLength + 1, iExtensionLength) EQ ipcExtension THEN DO:
                CREATE ttFile.
                ASSIGN 
                    ttFile.cFile = FILE-INFO:FULL-PATHNAME
                    ttFile.cType = ipcExtension
                    .
            END.
        END.
        ELSE IF FILE-INFO:FULL-PATHNAME <> ? THEN 
        DO:
            RUN pBuildFileList(INPUT FILE-INFO:FULL-PATHNAME + "\", ipcExtension).
        END.
    END.


END PROCEDURE.


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
    DEFINE INPUT PARAMETER iplErrorsOnly AS LOGICAL NO-UNDO.

    OUTPUT STREAM sLog TO VALUE(ipcLogFile).

    FOR EACH ttImportData
        WHERE (NOT ttImportData.lValid AND iplErrorsOnly) OR NOT iplErrorsOnly:
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
    DEFINE INPUT PARAMETER iplIncludeHelp AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipriContext AS ROWID NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcFile AS CHARACTER NO-UNDO.



    IF iplShellOnly THEN 
    DO:
        OUTPUT STREAM sTemplate TO VALUE(iopcFile). 
        FOR EACH ttImportMap:
            PUT STREAM sTemplate UNFORMATTED ttImportMap.cColumnLabel ',' .
        END.
        PUT STREAM sTemplate SKIP.
        IF iplIncludeHelp THEN 
        DO:
            FOR EACH ttImportMap:
                PUT STREAM sTemplate UNFORMATTED ttImportMap.cHelp ',' .
            END.
            PUT STREAM sTemplate SKIP.
        END.    
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
    
    DEFINE VARIABLE cProgram AS CHARACTER NO-UNDO.
   
    EMPTY TEMP-TABLE ttImportMap.
    /*gc global variables defined in ttImport.i*/
    cProgram = gcTypeProgramsFolder + ENTRY(LOOKUP(ipcTypeToInit,gcTypeList),gcTypePrograms).
    RUN VALUE(cProgram) PERSISTENT SET ghdImportProcForType.

    IF NOT CAN-FIND(FIRST ttImportMap WHERE ttImportMap.cType EQ ipcTypeToInit) THEN
        RUN pInitialize IN ghdImportProcForType("").


END PROCEDURE.

PROCEDURE pLoad:
    /*------------------------------------------------------------------------------
     Purpose: Processes the file and returns valid logical if successful
     Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplHeader AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcFileType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplLimitReached AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiCount AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE cData  AS CHARACTER NO-UNDO EXTENT 200.

    EMPTY TEMP-TABLE ttImportData.
    oplSuccess = YES.
    
    CASE ipcFileType:
        WHEN "ARD" THEN 
            DO:
                ASSIGN 
                    iplHeader = NO 
                    .
                IF SEARCH(ipcFile) NE ? THEN 
                DO:
                    RUN pLoadARD(ipcCompany, ipcLocation, ipcFile, INPUT-OUTPUT opiCount, INPUT-OUTPUT oplSuccess).
                END.    
            END.
        WHEN "ARDFolder" THEN 
            DO:
                RUN pBuildFileList(ipcFile, ".ard").
                FOR EACH ttFile:
                    IF SEARCH(ipcFile) NE ? THEN 
                    DO:
                        RUN pLoadARD(ipcCompany, ipcLocation, ttFile.cFile, INPUT-OUTPUT opiCount, INPUT-OUTPUT oplSuccess).
                    END.    
                END.
            END.
        OTHERWISE
        DO:
            IF SEARCH(ipcFile) NE ? THEN 
            DO:
                INPUT STREAM sImport FROM VALUE(ipcFile).
                REPEAT:
                    
                    ASSIGN 
                        opiCount = opiCount + 1
                        cData  = "".
                    IMPORT STREAM sImport DELIMITER ','
                        cData
                        . 
                    CREATE ttImportData.
                    ASSIGN
                        ttImportData.cData  = cData 
                        ttImportData.lValid = YES 
                        ttImportData.iCount = opiCount.    
                
                    IF TRIM(ttImportData.cData[1]) EQ '' THEN
                        ASSIGN 
                            ttImportData.lValid      = NO 
                            ttImportData.cImportNote = 'No Data in First Column'
                            .
                    ELSE 
                    DO:
                        IF (iplHeader AND opiCount GT 1) OR NOT iplHeader THEN 
                            RUN pAddRecord IN ghdImportProcForType(ipcCompany,
                                ipcLocation,
                                ttImportData.cData, 
                                iplUpdateDuplicates,
                                iplFieldValidation,
                                OUTPUT ttImportData.lValid, 
                                OUTPUT ttImportDAta.cImportNote).
                    END.
                    IF opiCount EQ giRecordLimit THEN DO:
                        oplLimitReached = TRUE .
                        LEAVE.
                    END.
                    
                END.
                OUTPUT STREAM sImport CLOSE.
            END.
        END.     
    END CASE.
   
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

PROCEDURE pLoadArd PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Accepts a file and creates a ttImportData record for a given file
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER oplSuccess AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE hdArtiosProcs AS HANDLE.
    DEFINE VARIABLE cData LIKE ttImportData.cData.
    
    RUN est/ArtiosProcs.p PERSISTENT SET hdArtiosProcs.

    iopiCount = iopiCount + 1.
    CREATE ttImportData.
    ASSIGN
        ttImportData.lValid = YES 
        ttImportData.iCount = iopiCount
        .
    RUN pSetArtiosData IN hdArtiosProcs (ipcCompany, ipcFile, OUTPUT ttImportData.cData).
    IF TRIM(ttImportData.cData[1]) EQ '' THEN
        ASSIGN 
            ttImportData.lValid      = NO 
            ttImportData.cImportNote = 'No Data in First Column'
            .
    ELSE 
    DO:
        RUN pAddRecord IN ghdImportProcForType(ipcCompany,
            ipcLocation,
            ttImportData.cData, 
            NO,
            YES,
            OUTPUT ttImportData.lValid, 
            OUTPUT ttImportDAta.cImportNote).
    END.
    
END PROCEDURE.

PROCEDURE pProcessImport:
    /*------------------------------------------------------------------------------
     Purpose: Processes the Import Data TempTable for the active type
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiUpdateCount AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiAddCount AS INTEGER NO-UNDO.


    RUN pProcessImport IN ghdImportProcForType (iplIgnoreBlanks, OUTPUT opiUpdateCount, OUTPUT opiAddCount).

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



    