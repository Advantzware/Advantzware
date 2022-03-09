
/*------------------------------------------------------------------------
    File        : ExcelProcs.p
    Purpose     : 

    Syntax      :

    Description : Wrapper for the Excel COM Object functions

    Author(s)   : BV
    Created     : Mon Jan 31 21:36:16 EST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE gchExcelApplication AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE gchExcelWorkbook    AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE gchExcelWorksheet   AS COMPONENT-HANDLE NO-UNDO.

DEFINE VARIABLE xlDown AS INTEGER NO-UNDO INITIAL -4121.
DEFINE VARIABLE xlFormatFromLeftOrAbove AS INTEGER NO-UNDO INITIAL 0.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE Excel_Cleanup:
    /*------------------------------------------------------------------------------
     Purpose: wrapper for Excel cleanup
     Notes:
    ------------------------------------------------------------------------------*/
    RUN pCleanup.

END PROCEDURE.

PROCEDURE Excel_Initialize:
    /*------------------------------------------------------------------------------
     Purpose: wrapper for Excel Initialize 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    RUN pInitialize (NO, OUTPUT oplError, OUTPUT opcMessage). 

END PROCEDURE.

PROCEDURE Excel_InitializeTemplate:
    /*------------------------------------------------------------------------------
     Purpose: wrapper for Excel Initialize with
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTemplateFile AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    RUN pInitialize (NO, OUTPUT oplError, OUTPUT opcMessage).
    IF NOT oplError THEN 
        RUN pOpenTemplate(ipcTemplateFile, OUTPUT oplError, OUTPUT opcMessage).
        
END PROCEDURE.

PROCEDURE Excel_InsertRowAbove:
/*------------------------------------------------------------------------------
 Purpose:  Given a row number, this will insert a row above the row number and 
 copy formatting to it.
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiRowToInsertAbove AS INTEGER NO-UNDO.
    
    RUN pInsertRowAbove(ipiRowToInsertAbove).
    
END PROCEDURE.

PROCEDURE Excel_InsertRowsAbove:
/*------------------------------------------------------------------------------
 Purpose:  Given a row number, this will insert a row above the row number and 
 copy formatting to it.
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiRowToInsertAbove AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiNumRows AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
    IF ipiNumRows LE 0 THEN 
        ipiNumRows = 1.
        
    DO iCount = 1 TO ipiNumRows:
        RUN pInsertRowAbove(ipiRowToInsertAbove).
    END.
    
END PROCEDURE.
PROCEDURE Excel_SetCellValue:
/*------------------------------------------------------------------------------
 Purpose:  given cell address and value, sets the value within the active sheet
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCellAddress AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCellValue AS CHARACTER NO-UNDO.
    
    RUN pSetCellValue(ipcCellAddress, ipcCellValue).

END PROCEDURE.

PROCEDURE Excel_SetCellFormat:
/*------------------------------------------------------------------------------
 Purpose:  given cell address and value, sets the value within the active sheet
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCellAddress AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCellFormat AS CHARACTER NO-UNDO.
    
    RUN pSetCellFormat(ipcCellAddress, ipcCellFormat).

END PROCEDURE.

PROCEDURE pCleanup PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Resets Excel Status and cleans up procedure-level objects
     Notes:
    ------------------------------------------------------------------------------*/
    
    /* enable screen updating */
    gchExcelApplication:ScreenUpdating = TRUE NO-ERROR.
    
    /* RELEASE OBJECTS */
    RELEASE OBJECT gchExcelWorksheet.
    RELEASE OBJECT gchExcelWorkbook.
    RELEASE OBJECT gchExcelApplication.
    
END PROCEDURE.


PROCEDURE pInitialize PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Initializes the Excel Application Object
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplHideExcel AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
        
    /* Connect to the running Excel session. */
    CREATE "Excel.Application" gchExcelApplication CONNECT NO-ERROR.
  
    /* Start a new session of Excel. */
    IF NOT VALID-HANDLE(gchExcelApplication) THEN
        CREATE "Excel.Application" gchExcelApplication NO-ERROR.
  
    /* Check if Excel got initialized. */
    IF NOT VALID-HANDLE (gchExcelApplication) THEN
    DO :
        ASSIGN 
            opcMessage = "Unable to Start Excel"
            oplError   = YES
            .        
    END.
    ELSE 
    DO:
        
       /* Make Excel visible. */
       gchExcelApplication:VISIBLE        = NOT iplHideExcel NO-ERROR.
       /* Do not display Excel error messages. */
       gchExcelApplication:DisplayAlerts  = FALSE NO-ERROR.  
       /* Disable screen updating so it will go faster */
       gchExcelApplication:ScreenUpdating = FALSE NO-ERROR.      
    END.
    
END PROCEDURE.

PROCEDURE pInsertRowAbove PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiRow AS INTEGER NO-UNDO.
    
    gchExcelWorksheet:Rows(ipiRow):INSERT . 

END PROCEDURE.

PROCEDURE pOpenTemplate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given template file, open the template and establish procedure-level
        variables for managing the Excel file.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTemplateFile AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFullPath AS CHARACTER NO-UNDO.
    
    RUN FileSys_GetFullFilePath (ipcTemplateFile, OUTPUT cFullPath, OUTPUT oplError, OUTPUT opcMessage).
    
    IF NOT oplError THEN 
    DO:
        IF cFullPath EQ ? THEN
            ASSIGN 
                oplError   = YES
                opcMessage = "Invalid Template File Path: " + ipcTemplateFile
                .  
        ELSE 
        DO:
            gchExcelWorkbook = gchExcelApplication:Workbooks:Open(cFullPath) NO-ERROR.
            gchExcelWorkbook:WorkSheets(1):Activate NO-ERROR.
            gchExcelWorkSheet = gchExcelApplication:Sheets:item(1) NO-ERROR.
        END.
    END.
    
END PROCEDURE.

PROCEDURE pSetCellValue PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  given cell address and value, sets the value within the active sheet
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCellAddress AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCellValue AS CHARACTER NO-UNDO.

    gchExcelWorkSheet:Range(ipcCellAddress):Value = ipcCellValue.    
     
END PROCEDURE.

PROCEDURE pSetCellFormat PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  given cell address and value, sets the value within the active sheet
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCellAddress AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCellFormat AS CHARACTER NO-UNDO.

    gchExcelWorkSheet:Range(ipcCellAddress):NumberFormat = ipcCellFormat.    
     
END PROCEDURE.

