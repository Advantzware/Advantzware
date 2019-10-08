/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/ 
/*------------------------------------------------------------------------
    File        : ExcelWorksheetImporter
    Purpose     : Imports Excel Worksheets into Temp-Tables
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Feb 26 12:21:30 CET 2012
    Notes       : Based on the Excel Com Interop Assembly 
                  Microsoft.Office.Interop.Excel
  ----------------------------------------------------------------------*/

USING Consultingwerk.Utilities.Excel.*  FROM PROPATH . 
USING Microsoft.Office.Interop.Excel.*  FROM ASSEMBLY . 
USING Progress.Lang.*                   FROM PROPATH .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Utilities.Excel.ExcelWorksheetImporter: 

    /*------------------------------------------------------------------------------
        Purpose: Raised when the import has finished                                            
        Notes:          
        @param sender The instance the raised the event
        @param e The Consultingwerk.EventArgs instance for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT Completed SIGNATURE VOID (sender AS Progress.Lang.Object,  
                                                  e AS Consultingwerk.EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Returns the number of rows that have been imported                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RowsImported AS INTEGER INITIAL 0 NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Raised when the import has processed the RowsPerProgressEvent number
                 of rows                                                                        
        Notes:          
        @param sender The instance the raised the event
        @param e The Consultingwerk.EventArgs instance for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ProgressChanged SIGNATURE VOID (sender AS Progress.Lang.Object,  
                                                        e AS Consultingwerk.EventArgs).
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ExcelWorksheetImporter class                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ExcelWorksheetImporter ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Worksheet referenced by either Name or  
                 Index                                                                                      
        Notes:   The Worksheet name has preference over the index if set  
        @param poWorkbook The reference to the workbook that contains the Worksheet
        @param pcWorksheetName The name of the worksheet to return
        @param piWorksheetIndex The index (number) of the worksheet to return
        @return The reference to the Excel worksheet                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Worksheet GetWorksheet (poWorkbook AS Workbook,
                                          pcWorksheetName AS CHARACTER,
                                          piWorksheetIndex AS INTEGER):
        
        DEFINE VARIABLE oWorksheet AS Worksheet NO-UNDO . 
        DEFINE VARIABLE i          AS INTEGER   NO-UNDO.
        
        Consultingwerk.Assertion.ObjectAssert:IsValid (poWorkbook) .
        
        IF pcWorksheetName > "":U THEN DO:
            DO i = 1 TO poWorkbook:Sheets:Count:
                
                oWorksheet = CAST (poWorkbook:Sheets:Item [i],
                                   Worksheet) .
            
                IF oWorksheet:Name = pcWorksheetName THEN 
                    RETURN oWorksheet .
            END.
        END.
        ELSE 
            RETURN CAST (poWorkbook:Sheets:Item [1],
                         Worksheet) . 

        RETURN ? . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Imports from an Excel Worksheet into a Temp-Table                                                                       
        Notes:                                                      
        @param poImportWorksheetArguments A reference to an ImportWorksheetArguments instance with the parameters for this method
        @param phTempTable OUTPUT TABLE-HANDLE The temp-table to import to (note the schema for the temp-table is provided by the caller)                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Import (poImportWorksheetArguments AS ImportWorksheetArguments,
                               OUTPUT TABLE-HANDLE phTempTable):
        
        DEFINE VARIABLE oExcel     AS Microsoft.Office.Interop.Excel.Application NO-UNDO . 
        DEFINE VARIABLE oWorkbook  AS Workbook                                   NO-UNDO .  
        DEFINE VARIABLE oWorksheet AS Worksheet                                  NO-UNDO .
        
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
        
        ASSIGN cFileName = poImportWorksheetArguments:FileName . 
        
        oExcel = NEW Microsoft.Office.Interop.Excel.ApplicationClass () .
        
        oWorkbook = OpenWorkbook (oExcel, cFileName) .

        oWorksheet = THIS-OBJECT:GetWorksheet (oWorkbook, 
                                               poImportWorksheetArguments:WorksheetName,
                                               poImportWorksheetArguments:WorksheetIndex) .

        ImportRows (oWorksheet, 
                    poImportWorksheetArguments,
                    OUTPUT TABLE-HANDLE phTempTable BY-REFERENCE) .

        FINALLY:
            oExcel:Quit() .        
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Imports rows from an Excel worksheet to a Progress temp-table                                                                        
        Notes:                                                                  
        @param poWorksheet The reference to the Excel Worksheet
        @param poImportWorksheetArguments A reference to an ImportWorksheetArguments instance with the parameters for this method
        @param phTempTable OUTPUT TABLE-HANDLE The temp-table to import to (note the schema for the temp-table is provided by the caller)                 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ImportRows (poWorksheet AS Worksheet,
                                      poImportWorksheetArguments AS ImportWorksheetArguments,
                                      OUTPUT TABLE-HANDLE phTempTable):
        
        DEFINE VARIABLE iFrom      AS INTEGER NO-UNDO.
        DEFINE VARIABLE iTo        AS INTEGER NO-UNDO.
        DEFINE VARIABLE iRow       AS INTEGER NO-UNDO.
        DEFINE VARIABLE iCell      AS INTEGER NO-UNDO.
        DEFINE VARIABLE iNumFields AS INTEGER NO-UNDO.
        DEFINE VARIABLE hBuffer    AS HANDLE  NO-UNDO.
        DEFINE VARIABLE lEmptyRow  AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iEmptyRows AS INTEGER NO-UNDO INIT 0 .
        
        DEFINE VARIABLE oValues    AS "System.Object[]":U NO-UNDO .         
        
        ASSIGN hBuffer    = phTempTable:DEFAULT-BUFFER-HANDLE 
               iNumFields = hBuffer:NUM-FIELDS 
               
               iFrom      = poImportWorksheetArguments:SkipRowsFromTop  + 1
               iTo        = poWorksheet:Rows:Count.           

        oValues = {Consultingwerk/new-array.i System.Object iNumFields} .
        
        DO iRow = iFrom TO iTo:

            ASSIGN lEmptyRow = TRUE . 

            Consultingwerk.Utilities.Support.ExcelHelper:GetExcelCellValues (poWorksheet, iRow, INPUT-OUTPUT oValues) .

            emptyrowloop: DO iCell = 1 TO iNumFields:
                IF STRING (oValues:GetValue (iCell - 1)) > "":U THEN 
                DO:
                    ASSIGN lEmptyRow = FALSE .
                    LEAVE emptyrowloop .
                END. 
            END. 

            IF lEmptyRow THEN 
                iEmptyRows = iEmptyRows + 1 . 
                
            IF iEmptyRows >= poImportWorksheetArguments:StopAfterEmptyRows THEN 
                RETURN . 

            hBuffer:BUFFER-CREATE () .
            
            DO iCell = 1 TO iNumFields ON ERROR UNDO, THROW:
                hBuffer:BUFFER-FIELD (iCell):BUFFER-VALUE = UNBOX (oValues:GetValue (iCell - 1)) .
                
                CATCH err AS Progress.Lang.Error:
                    UNDO, THROW NEW ExcelImportError (err, 
                                                      hBuffer:BUFFER-FIELD (iCell):NAME,
                                                      STRING (UNBOX (oValues:GetValue (iCell - 1))),
                                                      iRow, 
                                                      iCell).    
                END CATCH.
            END.
            
            hBuffer:BUFFER-RELEASE () .

            THIS-OBJECT:RowsImported = THIS-OBJECT:RowsImported + 1 . 
            
            IF poImportWorksheetArguments:RaiseProgressChangedAfter <> ? THEN 
                IF THIS-OBJECT:RowsImported MODULO poImportWorksheetArguments:RaiseProgressChangedAfter = 0 THEN 
                    OnProgressChanged (Consultingwerk.EventArgs:Empty) .
                    
            IF poImportWorksheetArguments <> ? AND THIS-OBJECT:RowsImported >= poImportWorksheetArguments:ImportRows THEN 
                RETURN .                     
        END.

        FINALLY:
            IF poImportWorksheetArguments:RaiseProgressChangedAfter <> ? THEN 
                OnProgressChanged (Consultingwerk.EventArgs:Empty) .
    
            OnCompleted (Consultingwerk.EventArgs:Empty) .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Completed event                                                                        
        Notes:                                                                        
        @param e The Consultingwerk.EventArgs instance for the event                                                              
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnCompleted (e AS Consultingwerk.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = Consultingwerk.EventArgs:Empty .
            
        THIS-OBJECT:Completed:Publish (THIS-OBJECT, e) .
                    
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the ProgressChanged event                                                                        
        Notes:                                  
        @param e The Consultingwerk.EventArgs instance for the event                                                              
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnProgressChanged (e AS Consultingwerk.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = Consultingwerk.EventArgs:Empty . 

        THIS-OBJECT:ProgressChanged:Publish (THIS-OBJECT, e) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Opens an Excel Workbook                                                                        
        Notes:                                                                        
        @param poExcel The reference to an Excel Application instance
        @param pcFileName The file name of the Workbook to open
        @return The reference to the Workbook instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Workbook OpenWorkbook (poExcel AS Microsoft.Office.Interop.Excel.Application, 
                                         pcFileName AS CHARACTER):
        
        DEFINE VARIABLE oWorkbook AS Workbook NO-UNDO .  
        
        oWorkbook = poExcel:Workbooks:Open
                     (pcFileName,            /*[In] string Filename,                           */
                      System.Type:Missing,   /*[In, Optional] object UpdateLinks,              */
                      TRUE,                  /*[In, Optional] object ReadOnly,                 */
                      System.Type:Missing,   /*[In, Optional] object Format,                   */
                      System.Type:Missing,   /*[In, Optional] object Password,                 */
                      System.Type:Missing,   /*[In, Optional] object WriteResPassword,         */
                      System.Type:Missing,   /*[In, Optional] object IgnoreReadOnlyRecommended,*/
                      System.Type:Missing,   /*[In, Optional] object Origin,                   */
                      System.Type:Missing,   /*[In, Optional] object Delimiter,                */
                      System.Type:Missing,   /*[In, Optional] object Editable,                 */
                      System.Type:Missing,   /*[In, Optional] object Notify,                   */
                      System.Type:Missing,   /*[In, Optional] object Converter,                */
                      FALSE,                 /*[In, Optional] object AddToMru,                 */
                      System.Type:Missing,   /*[In, Optional] object Local,                    */
                      System.Type:Missing) . /*[In, Optional] object CorruptLoad               */

        RETURN oWorkbook . 

    END METHOD.

END CLASS.