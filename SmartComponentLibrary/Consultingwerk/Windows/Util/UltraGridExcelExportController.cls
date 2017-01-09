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
    File        : UltraGridExcelExportController
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Mon Dec 10 21:52:12 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.Enum.*                 FROM PROPATH .
&IF DEFINED (SmartComponentLibrary) &THEN
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
&ENDIF 
USING Consultingwerk.Util.*                           FROM PROPATH . 
USING Consultingwerk.Windows.Util.*                   FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*             FROM PROPATH . 
USING Infragistics.Excel.*                            FROM ASSEMBLY . 
USING Infragistics.Documents.Excel.*                  FROM ASSEMBLY . 
USING Infragistics.Win.UltraWinGrid.*                 FROM ASSEMBLY . 
USING Infragistics.Win.UltraWinGrid.ExcelExport.*     FROM ASSEMBLY . 
USING Progress.Lang.*                                 FROM PROPATH .
USING System.Windows.Forms.*                          FROM ASSEMBLY . 

CLASS Consultingwerk.Windows.Util.UltraGridExcelExportController 
    IMPLEMENTS IUltraGridExcelExportController: 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the UltraGridExcelExporter used by this service                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Exporter AS UltraGridExcelExporter NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UltraGridExcelExportController class                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UltraGridExcelExportController ():
        SUPER ().
        
        THIS-OBJECT:Exporter = NEW UltraGridExcelExporter () .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Exports an UltraGrid     
        Notes:      
        @param poGrid The UltraGrid to export
        @return The filename of the exported document
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Export (poGrid AS UltraGrid):
    
        DEFINE VARIABLE cFileName        AS CHARACTER                     NO-UNDO .
        DEFINE VARIABLE oDialogResult    AS DialogResult                  NO-UNDO . 
        DEFINE VARIABLE oSaveFileDialog  AS SaveFileDialog                NO-UNDO .
        DEFINE VARIABLE oExportParameter AS UltraGridExcelExportParameter NO-UNDO . 
        DEFINE VARIABLE oForm            AS System.Windows.Forms.Form     NO-UNDO .  
        
        Consultingwerk.Assertion.ObjectAssert:IsValid (poGrid) .

        &IF DEFINED (SmartComponentLibrary) &THEN
        IF TYPE-OF (poGrid, Consultingwerk.SmartComponents.Implementation.SmartDataBrowser) THEN DO: 
            IF THIS-OBJECT:VerifyFetchMoreRows (CAST (poGrid, SmartDataBrowser)) = FALSE THEN 
                RETURN ? . 
        END.
        &ENDIF

        ASSIGN oSaveFileDialog                  = NEW SaveFileDialog () 
               oSaveFileDialog:AddExtension     = TRUE 
               oSaveFileDialog:CheckPathExists  = TRUE 
               oSaveFileDialog:DefaultExt       = "xlsx":U
               oSaveFileDialog:DereferenceLinks = TRUE
               oSaveFileDialog:Filter           = "Excel Files (*.xlsx)|*.xlsx"{&TRAN}
               oSaveFileDialog:FilterIndex      = 0 
               oSaveFileDialog:OverwritePrompt  = TRUE
               oSaveFileDialog:RestoreDirectory = TRUE
               oSaveFileDialog:Title            = "Excel Export"{&TRAN}  
              
               oForm                            = poGrid:FindForm (). 

        THIS-OBJECT:PerformWaitFor (oSaveFileDialog, oForm, OUTPUT oDialogResult) .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult ?} .

        oExportParameter = NEW UltraGridExcelExportParameter (oSaveFileDialog:FileName) .
        oExportParameter:PromptForAdditionalRows = FALSE .
        oExportParameter:PromptForOverwrite = FALSE .
        &IF PROVERSION BEGINS "10.2":U &THEN
        oExportParameter:WorkbookFormat = Infragistics.Excel.WorkbookFormat:Excel2007 .
        &ELSE
        oExportParameter:WorkbookFormat = Infragistics.Documents.Excel.WorkbookFormat:Excel2007 .
        &ENDIF
        RETURN THIS-OBJECT:Export (poGrid, oExportParameter) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oSaveFileDialog) .        
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Exports an UltraGrid                                                                     
        Notes:                                                                        
        @param poGrid The UltraGrid to export
        @param poOptions The IUltraGridExcelExportParameter for this export                                                               
        @return The filename of the exported document
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Export (poGrid AS UltraGrid,
                                    poOptions AS IUltraGridExcelExportParameter):
        
        Consultingwerk.Assertion.ObjectAssert:IsValid (poGrid) .
        
        &IF DEFINED (SmartComponentLibrary) &THEN
        IF poOptions:PromptForAdditionalRows = TRUE THEN DO:
            IF TYPE-OF (poGrid, Consultingwerk.SmartComponents.Implementation.SmartDataBrowser) THEN DO: 
                IF THIS-OBJECT:VerifyFetchMoreRows (CAST (poGrid, SmartDataBrowser)) = FALSE THEN 
                    RETURN ? . 
            END.
        END.
        &ENDIF
        
        IF poOptions:PromptForOverwrite THEN DO:
            FILE-INFORMATION:FILE-NAME = poOptions:FileName . 
              
            IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN DO:

                IF Consultingwerk.Util.MessageFormHelper:AskQuestion 
                    (SUBSTITUTE ("The file &1 does already exist. Overwrite?"{&TRAN}, 
                                 poOptions:FileName),
                     "Excel Export"{&TRAN}) = Consultingwerk.Framework.Enum.DialogResultEnum:DialogResultNo THEN 
                        
                    RETURN ?  .                     
            END .
        END.
        
        THIS-OBJECT:Exporter:Export (poGrid, poOptions:FileName) .        

        RETURN poOptions:FileName . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs the WAIT-FOR ShowDialog ()
        Notes:   Workaround for inabilit to compile code that uses a WAIT-FOR in a 
                 non void method
        @param oDialog The reference to the CommonDialog
        @param poForm The parent Form
        @param poDialogResult The OUTPUT DialogResult
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformWaitFor (oDialog AS CommonDialog, 
                                       poForm AS Form,
                                       OUTPUT poDialogResult AS DialogResult):
        
        WAIT-FOR oDialog:ShowDialog (poForm) SET poDialogResult . 

    END METHOD .

    &IF DEFINED (SmartComponentLibrary) &THEN
    /*------------------------------------------------------------------------------
        Purpose: Verifies and if required prompts the user for loading additional 
                 rows
        Notes:   
        @param poGrid The SmartDataBrowser to load more rows for 
        @return Logical value indicating FALSE when the Excel Export should be cancelled
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL VerifyFetchMoreRows (poGrid AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser):
        
        DEFINE VARIABLE oRetrieveDataStatus AS RetrieveDataStatusEnum             NO-UNDO . 
        DEFINE VARIABLE oAdapter            AS SmartBusinessEntityAdapter         NO-UNDO . 
        DEFINE VARIABLE oDialogResultEnum   AS DialogResultEnum                   NO-UNDO .                  
        DEFINE VARIABLE lRetrieveData       AS LOGICAL                            NO-UNDO INIT FALSE .
        DEFINE VARIABLE oDialog             AS ExcelExportFetchMoreRowsPromptForm NO-UNDO . 
        DEFINE VARIABLE iValue              AS INTEGER                            NO-UNDO .
        DEFINE VARIABLE iBatchSize          AS INTEGER                            NO-UNDO .
        
        IF NOT VALID-OBJECT (poGrid:SmartDataSource) THEN 
            RETURN TRUE . 
            
        IF NOT TYPE-OF (poGrid:SmartDataSource,
                        Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter) THEN 
            RETURN TRUE . 

        ASSIGN oAdapter = CAST (poGrid:SmartDataSource,
                                Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter) .
        
        oRetrieveDataStatus = oAdapter:GetRetrieveDataStatus() .
        
        IF oRetrieveDataStatus = RetrieveDataStatusEnum:NotBatching THEN 
            RETURN TRUE . 
        
        IF oRetrieveDataStatus = RetrieveDataStatusEnum:BackwardsBatching OR 
           oRetrieveDataStatus = RetrieveDataStatusEnum:ForwardAndBackwardsBatching THEN DO:
        
            oDialogResultEnum = MessageFormHelper:ShowMessage ("You are not navigating the data from the beginning."{&TRAN} + System.Environment:NewLine + 
                                                               "Start from the beginning?"{&TRAN},
                                                               "Excel Export"{&TRAN}, "":U,
                                                               Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageQuestion,
                                                               Consultingwerk.Util.Forms.MessageFormDefaults:LabelYesNoCancel,
                                                               Consultingwerk.Util.Forms.MessageFormDefaults:GetDialogResultYesNoCancel(),
                                                               DialogResultEnum:DialogResultYes) .
            
            IF oDialogResultEnum = DialogResultEnum:DialogResultCancel THEN 
                RETURN FALSE .
               
            IF oDialogResultEnum = DialogResultEnum:DialogResultYes THEN 
                ASSIGN lRetrieveData = TRUE . 
        END.
        
        oDialog = NEW ExcelExportFetchMoreRowsPromptForm () .        
        oDialog:Owner = poGrid:FindForm () . 
        
        Consultingwerk.Support.DialogSupport:ShowDialog (oDialog) . 
        
        {Consultingwerk/check-dialogresult-ok.i oDialog:DialogResult FALSE}
        
        iValue = oDialog:Value .

        IF iValue > 0 THEN DO:
            IF lRetrieveData = TRUE THEN DO ON ERROR UNDO, THROW:
                
                ASSIGN iBatchSize         = oAdapter:BatchSize 
                       oAdapter:BatchSize = iValue. 
                
                oAdapter:RetrieveData () .
                
                FINALLY:
                    ASSIGN oAdapter:BatchSize = iBatchSize .                         
                END FINALLY.
            END.
            ELSE
                oAdapter:FetchMoreRowsForwards (iValue) .
        END.
        
        RETURN TRUE . 

    END METHOD .
    &ENDIF
    
    /*------------------------------------------------------------------------------
        Purpose: Destructor for the UltraGridExcelExportController class                                                                       
        Notes:          
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC UltraGridExcelExportController ():

        GarbageCollectorHelper:DeleteObject (THIS-OBJECT:Exporter) . 

    END DESTRUCTOR.

END CLASS.
