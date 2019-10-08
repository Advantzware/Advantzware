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
    File        : ExcelWorksheetExporter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Sebastian Düngel / Consultingwerk Ltd.
    Created     : Thu Jun 14 11:12:32 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*              FROM PROPATH . 
USING Consultingwerk.Utilities.Excel.*   FROM PROPATH . 
USING Consultingwerk.Utilities.Support.* FROM ASSEMBLY .
USING Microsoft.Office.Interop.Excel.*   FROM ASSEMBLY . 
USING Progress.Lang.* FROM ASSEMBLY.

CLASS Consultingwerk.Utilities.Excel.ExcelWorksheetExporter
    USE-WIDGET-POOL: 
    
    /*------------------------------------------------------------------------------
        Purpose: Raised when the Export has processed the RowsPerProgressEvent number
                 of rows                                                                        
        Notes:          
        @param sender The instance the raised the event
        @param e The Consultingwerk.EventArgs instance for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ProgressChanged SIGNATURE VOID (sender AS Progress.Lang.Object,  
                                                        e AS Consultingwerk.EventArgs).
    
    /*------------------------------------------------------------------------------
        Purpose: Raised when the Export has finished                                            
        Notes:          
        @param sender The instance the raised the event
        @param e The Consultingwerk.EventArgs instance for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT Completed SIGNATURE VOID (sender AS Progress.Lang.Object,  
                                                  e AS Consultingwerk.EventArgs).
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ExcelWorksheetExporter class                                                              
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ExcelWorksheetExporter ():
        SUPER ().
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Export rows from an  Progress temp-table to a Excel worksheet                                                                     
        Notes:                                                                  
        @param poExportWorksheetArguments A reference to an ExportWorksheetArguments instance with the parameters for this method
        @param phTempTable OUTPUT TABLE-HANDLE The temp-table to Export to (note the schema for the temp-table is provided by the caller)                 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ExportExcel (poExportWorksheetArguments AS ExportWorksheetArguments,
                                    phTempTable AS HANDLE):
                                   
        DEFINE VARIABLE oExcel     AS Microsoft.Office.Interop.Excel.Application NO-UNDO . 
        DEFINE VARIABLE oWorkbook  AS Workbook                                   NO-UNDO .  
        DEFINE VARIABLE oWorksheet AS Worksheet                                  NO-UNDO .
        DEFINE VARIABLE iStartRow AS INTEGER                                     NO-UNDO .
        DEFINE VARIABLE cFileName  AS CHARACTER                                  NO-UNDO .
        
        ASSIGN cFileName  = poExportWorksheetArguments:FileName 
               iStartRow  = poExportWorksheetArguments:StartRow
               . 
        
        oExcel     = NEW Microsoft.Office.Interop.Excel.ApplicationClass () .
        oWorkbook  = OpenWorkbook (oExcel, cFileName) .
        oWorksheet = THIS-OBJECT:GetWorksheet (oWorkbook,
                                               poExportWorksheetArguments:WorksheetName,
                                               poExportWorksheetArguments:WorksheetIndex) .
        
        ASSIGN oWorksheet:EnableCalculation                 = FALSE 
               oWorksheet:EnableFormatConditionsCalculation = FALSE 
               oWorksheet:EnableOutlining                   = FALSE .
        
        ExportRows (oWorksheet,
                    poExportWorksheetArguments,
                    phTempTable,
                    iStartRow) .

        oWorksheet:Calculate () . 

        oWorkbook:Save () .

        FINALLY:
            oWorkbook:Saved = TRUE . 
            
            oExcel:Quit() .        
        END FINALLY.
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Exports rows from Progress temp-table to an Excel-Worksheet                                                                        
        Notes:                                                                  
        @param poWorksheet The reference to the Excel Worksheet
        @param poExportWorksheetArguments A reference to an ImportWorksheetArguments instance with the parameters for this method
        @param phTempTable OUTPUT TABLE-HANDLE The temp-table to import to (note the schema for the temp-table is provided by the caller)
        @param piStartRow Sets the Startrow of the Worksheet to be Exported                
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ExportRows (poWorksheet AS Worksheet,
                                      poExportWorksheetArguments AS ExportWorksheetArguments,
                                      phTempTable AS HANDLE,
                                      piStartRow AS INTEGER):
        
        DEFINE VARIABLE hBuffer    AS HANDLE                NO-UNDO.
        DEFINE VARIABLE hQuery     AS HANDLE                NO-UNDO. 
        DEFINE VARIABLE iNumFields AS INTEGER               NO-UNDO.
        DEFINE VARIABLE iField     AS INTEGER               NO-UNDO.
        DEFINE VARIABLE oValues    AS "System.Object[]":U   NO-UNDO. 
        DEFINE VARIABLE iCount     AS INTEGER               NO-UNDO.
        
        ASSIGN 
            hBuffer    = phTempTable:DEFAULT-BUFFER-HANDLE 
            iNumFields = hBuffer:NUM-FIELDS
            iCount     = piStartRow
            .
   
        hQuery = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (hBuffer).
        hQuery:GET-FIRST ().
        
        oValues = {Consultingwerk/new-array.i System.Object iNumFields}.
        
        /* Mike Fechner, Consultingwerk Ltd. 16.06.2012 *** TO-DO ***
           Handling of ARRAY-Fields while exporting */

        /* Export header */
        DO iField = 1 TO iNumFields:
            oValues:SetValue (hBuffer:BUFFER-FIELD (iField):NAME, iField - 1).  
        END.
        
        ExcelHelper:SetExcelCellValues (poWorksheet, iCount, oValues).
        iCount = iCount + 1.
        
        /* Export rows */
        DO WHILE NOT hQuery:QUERY-OFF-END:
            
            /* Mike Fechner, Consultingwerk Ltd. 16.06.2012 *** TO-DO ***
               Handling of ARRAY-Fields while exporting */

            DO iField = 1 TO iNumFields:
               oValues:SetValue (hBuffer:BUFFER-FIELD(iField):BUFFER-VALUE, iField - 1).
            END.
            
            ExcelHelper:SetExcelCellValues (poWorksheet, iCount, oValues).
            
            hQuery:GET-NEXT () .
            iCount = iCount + 1.
            
            THIS-OBJECT:RowsExported = THIS-OBJECT:RowsExported + 1 . 
            
            IF poExportWorksheetArguments:RaiseProgressChangedAfter <> ? THEN 
                IF THIS-OBJECT:RowsExported MODULO poExportWorksheetArguments:RaiseProgressChangedAfter = 0 THEN 
                    OnProgressChanged (Consultingwerk.EventArgs:Empty) .
                    
            IF poExportWorksheetArguments <> ? AND THIS-OBJECT:RowsExported >= poExportWorksheetArguments:ExportRows THEN 
                RETURN . 
        END.
 
        hQuery:QUERY-CLOSE ().
        
        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) . 
            
            IF poExportWorksheetArguments:RaiseProgressChangedAfter <> ? THEN 
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
        Purpose: Returns the number of rows that have been exported                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RowsExported AS INTEGER INITIAL 0 NO-UNDO 
    GET.
    PROTECTED SET. 
    
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
                      System.Type:Missing,   /*[In, Optional] object ReadOnly,                 */
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
