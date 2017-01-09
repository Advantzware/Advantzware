/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : AblFrameMigrationForm
    Purpose     : Provides user interface for turning ABL GUI FRAME Widgets
                  into GUI for .NET UI Objects
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Dec 02 20:03:21 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                                      FROM PROPATH .
USING Consultingwerk.Assertion.*                                            FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.*                               FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.*              FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.*        FROM PROPATH .
USING Consultingwerk.Framework.*                                            FROM PROPATH . 
USING Consultingwerk.SmartComponents.Base.*                                 FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*                                 FROM PROPATH . 
USING Consultingwerk.SmartComponents.Implementation.*                       FROM PROPATH . 
USING Consultingwerk.SmartComponents.Support.*                              FROM PROPATH .
USING Consultingwerk.Studio.LegacyGuiMigration.CreateBusinessEntityWizard.* FROM PROPATH .
USING Consultingwerk.Studio.LegacyGuiMigration.Frame.*                      FROM PROPATH .
USING Consultingwerk.Util.*                                                 FROM PROPATH .   
USING Consultingwerk.Windows.Util.Forms.*                                   FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*                                    FROM ASSEMBLY .
USING Progress.Lang.*                                                       FROM PROPATH .
USING System.Windows.Forms.*                                                FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.Studio.LegacyGuiMigration.Frame.AblFrameMigrationForm 
    INHERITS SmartWindowForm: 

    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE coFrameName AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE m_AblFrameMigrationForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_AblFrameMigrationForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_AblFrameMigrationForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_AblFrameMigrationForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE AblFrameMigrationForm_Fill_Panel AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE saveFileDialog1 AS System.Windows.Forms.SaveFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE smartTempTableAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.
    DEFINE PRIVATE VARIABLE smartUpdatableBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser NO-UNDO.
    
    DEFINE PRIVATE VARIABLE ultraCheckEditor1 AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraStatusBar1 AS Infragistics.Win.UltraWinStatusBar.UltraStatusBar NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor2 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor4 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor3 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    
    {Consultingwerk/Studio/LegacyGuiMigration/Frame/ttFrameWidget.i}
    {Consultingwerk/Studio/LegacyGuiMigration/Frame/dsTrigger.i}

    DEFINE VARIABLE oParser AS FrameParser NO-UNDO .

    DEFINE TEMP-TABLE ttSelectedFrameWidget NO-UNDO 
        LIKE ttFrameWidget
        FIELD IsSelected             AS LOGICAL   INIT TRUE 
        FIELD AlternativeControlType AS CHARACTER INIT "":U.
    
    DEFINE VARIABLE oLruController AS Consultingwerk.Windows.Controls.UltraRibbonLruController NO-UNDO  .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the currently opened file 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AblFrameMigrationForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC AblFrameMigrationForm ():
        		
        SUPER().

        InitializeComponent().
        
        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF
                
        oParser = NEW FrameParser () . 
        oParser:StatusChanged:Subscribe (StatusChangedHandler) .
    
        smartTempTableAdapter1:TempTableHandle = TEMP-TABLE ttSelectedFrameWidget:HANDLE .
        smartTempTableAdapter1:RetrieveData () .
              
        oLruController = NEW Consultingwerk.Windows.Controls.UltraRibbonLruController (THIS-OBJECT:smartToolbarController1,
                                                                                       "LRU":U,
                                                                                       "&1":U,
                                                                                       10) .
                
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the CustomizeBusinessEntity event of the 
	             CreatBusinessEntityWizardForm
	    Notes:
	    @param sender The reference to the object that raised the event
	    @param e The CustomizeBusinessEntityEventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID CustomizeBusinessEntity (sender AS Progress.Lang.Object,
	                                               e AS CustomizeBusinessEntityEventArgs):
		
		DEFINE VARIABLE cFieldName  AS CHARACTER       NO-UNDO .
		DEFINE VARIABLE oFieldName  AS BufferFieldName NO-UNDO .
	    DEFINE VARIABLE hBuffer     AS HANDLE          NO-UNDO .
	    DEFINE VARIABLE roRowid     AS ROWID           NO-UNDO . 
        DEFINE VARIABLE lKeepSource AS LOGICAL         NO-UNDO .
	
	    DEFINE VARIABLE cBufferNames AS CHARACTER NO-UNDO.
	
	    hBuffer = e:Dataset:GET-BUFFER-HANDLE ("eTable":U) .
	    hBuffer:FIND-FIRST () .

        roRowid = hBuffer:ROWID .

        ASSIGN cBufferNames = hBuffer::SourceTableNames . 

		FOR EACH ttSelectedFrameWidget WHERE ttSelectedFrameWidget.IsSelected:
		    
		    IF NUM-ENTRIES (ttSelectedFrameWidget.FieldName, ".":U) > 1 THEN DO:
    		    ASSIGN oFieldName = BufferHelper:ParseFieldName (ttSelectedFrameWidget.FieldName) . 
    		    
    		    IF LOOKUP (oFieldName:TableName, cBufferNames) = 0 THEN 
    		        cBufferNames = cBufferNames + ",":U + oFieldName:TableName .
    		      
    		    ASSIGN cFieldName = SUBSTITUTE ("&1.&2.&3":U,
    		                                    IF oFieldName:DatabaseName > "":U THEN 
    		                                          oFieldName:DatabaseName 
    		                                    ELSE 
    		                                          "":U,
    		                                    oFieldName:TableName,
    		                                    oFieldName:FieldName) 
    		           lKeepSource = TRUE  .
    		                                    
		    END.
		    ELSE 
		        ASSIGN cFieldName  = ttSelectedFrameWidget.FieldName 
		               lKeepSource = FALSE.
		    
		    e:Controller:AddExistingField (roRowid, 
		                                   cFieldName,
		                                   SUBSTITUTE ("&2&1&3&1&4&1&5":U, 
		                                               CHR (1),
	                                                   ttSelectedFrameWidget.FieldDataType,
	                                                   "":U, 
	                                                   ttSelectedFrameWidget.WidgetLabel,
	                                                   ttSelectedFrameWidget.FieldFormat),
		                                   lKeepSource) .
		    
		END.

        ASSIGN hBuffer::SourceTableNames  = cBufferNames 
               hBuffer::SourceBufferNames = cBufferNames . 

        THIS-OBJECT:ultraTextEditor2:Text = hBuffer::TempTableName .

        hBuffer = e:Dataset:GET-BUFFER-HANDLE ("eBusinessEntity":U) .
        hBuffer:FIND-FIRST () .

        THIS-OBJECT:ultraTextEditor1:Text = SUBSTITUTE ("&1.&2":U,
                                                        hBuffer::BusinessEntityPackage,
                                                        hBuffer::BusinessEntityName) .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Generates an Web2 View from the selected Widgets
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID GenerateWeb2View ():
		
        DEFINE VARIABLE oBindingSource AS SmartBusinessEntityBindingSource NO-UNDO .
        DEFINE VARIABLE oBuilder       AS SmartViewerBuilder               NO-UNDO .
        DEFINE VARIABLE cFileName      AS CHARACTER                        NO-UNDO .
        DEFINE VARIABLE oList          AS ListViewerFieldSpec              NO-UNDO .
    
        DEFINE VARIABLE oSaveFileDialog  AS SaveFileDialog                NO-UNDO .
        DEFINE VARIABLE oDialogResult    AS DialogResult                  NO-UNDO . 
    
        ASSIGN oSaveFileDialog                  = NEW SaveFileDialog () 
               oSaveFileDialog:AddExtension     = TRUE 
               oSaveFileDialog:CheckPathExists  = TRUE 
               oSaveFileDialog:DefaultExt       = "html":U
               oSaveFileDialog:DereferenceLinks = TRUE
               oSaveFileDialog:Filter           = "HTML Files (*.html)|*.html"{&TRAN}
               oSaveFileDialog:FilterIndex      = 0 
               oSaveFileDialog:OverwritePrompt  = TRUE
               oSaveFileDialog:RestoreDirectory = TRUE
               oSaveFileDialog:Title            = "Save Web View"{&TRAN} .  

        WAIT-FOR oSaveFileDialog:ShowDialog (THIS-OBJECT) SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        ASSIGN cFileName = oSaveFileDialog:FileName .    
    
        THIS-OBJECT:SaveUnsavedChanges () .
        
        /* Mike Fechner, Consultingwerk Ltd. 10.07.2014
           Cannot proceed without Entity Name, Entity Table */
        IF ultraCheckEditor1:Checked THEN DO: 
            Assert:NotNullOrEmpty (ultraTextEditor1:Text, "Business Entity Name"{&TRAN}) .
            Assert:NotNullOrEmpty (ultraTextEditor2:Text, "Entity Table"{&TRAN}) .

            oBindingSource = NEW SmartBusinessEntityBindingSource () .
            
            ASSIGN oBindingSource:EntityName  = ultraTextEditor1:Text
                   oBindingSource:EntityTable = ENTRY (1, ultraTextEditor2:Text)
                   oBindingSource:EntityView  = ListHelper:RemoveEntry (ultraTextEditor2:Text, 1)
                   oBindingSource:EntityJoin  = TRIM (FILL ("yes,":U, NUM-ENTRIES (oBindingSource:EntityView)), ",":U) 
                        .  
    
            oBindingSource:DesignerImportSchema (TRUE) .        
    
            /* Mike Fechner, Consultingwerk Ltd. 10.07.2014
               Unable to continue without Table Schmea */
            IF NOT VALID-OBJECT (oBindingSource:TableSchema) THEN 
                RETURN . 
        END.
        
        oBuilder = NEW SmartViewerBuilder () .
        oBuilder:Generator = NEW SmartWeb2ViewGenerator (cFileName) . 
        
        oList = THIS-OBJECT:PrepareViewerBuilder (oBuilder, oBindingSource) .
                
        oBuilder:Generate (oList) .
        
        IF cFileName > "":U THEN 
            WindowsExplorerHelper:ShowFileInExplorer (cFileName) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Prepares the SmartViewerBuilder (adds fields)
        Notes:   
        @param poBuilder The SmartViewerBuilder instance to prepare
        @param poBindingSource The SmartBusinessEntityBindingSource with the reference schema
        @return The list of viewer field specs to pass to the builder with the Generate() method
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED ListViewerFieldSpec PrepareViewerBuilder (poBuilder AS SmartViewerBuilder,
	                                                           poBindingSource AS SmartBusinessEntityBindingSource):

        DEFINE VARIABLE oField         AS ViewerFieldSpec                  NO-UNDO . 
        DEFINE VARIABLE cName          AS CHARACTER                        NO-UNDO .
        DEFINE VARIABLE oList          AS ListViewerFieldSpec              NO-UNDO .
        DEFINE VARIABLE iMinLeft       AS INTEGER                          NO-UNDO INIT 99999 .
        DEFINE VARIABLE iMinTop        AS INTEGER                          NO-UNDO INIT 99999 .
        DEFINE VARIABLE lParseTrigger  AS LOGICAL                          NO-UNDO .
        DEFINE VARIABLE oTriggerParser AS TriggerParser                    NO-UNDO . 
        DEFINE VARIABLE lcCode         AS LONGCHAR                         NO-UNDO .
		
        poBuilder:BindingSource = poBindingSource .
        poBuilder:ClassName = ultraTextEditor4:Text .
        poBuilder:PackageName = ultraTextEditor3:Text .
        poBuilder:CreateBindingSource = ultraCheckEditor1:Checked . 
        
        IF VALID-OBJECT (poBindingSource) THEN DO:
            poBuilder:EntityJoin  = poBindingSource:EntityJoin . 
            poBuilder:EntityName  = poBindingSource:EntityName . 
            poBuilder:EntityTable = poBindingSource:EntityTable . 
            poBuilder:EntityView  = poBindingSource:EntityView .
        END. 

        oList = NEW ListViewerFieldSpec () .

        /* Left / Top Align */
        IF UltraToolbarsHelper:GetStateButtonToolChecked (smartToolbarController1, 
                                                          "Left/Top Align":U) THEN DO:

            FOR EACH ttSelectedFrameWidget WHERE ttSelectedFrameWidget.IsSelected = TRUE
                           BY ttSelectedFrameWidget.Order:
           
                ASSIGN iMinLeft = MIN (iMinLeft, ttSelectedFrameWidget.AtColumn * SESSION:PIXELS-PER-COLUMN) 
                       iMinTop  = MIN (iMinTop, ttSelectedFrameWidget.AtRow * SESSION:PIXELS-PER-ROW) .
            END.            
        END.

        /* Mike Fechner, Consultingwerk Ltd. 15.11.2014
           SCL-419 : Parsing of trigger blocks */
        ASSIGN lParseTrigger = UltraToolbarsHelper:GetStateButtonToolChecked (smartToolbarController1, 
                                                                              "Parse Trigger Code":U) .
              
        IF lParseTrigger THEN DO:
            oTriggerParser = NEW TriggerParser () .
            oTriggerParser:StatusChanged:Subscribe (StatusChangedHandler) .
            oTriggerParser:GetTriggerCode (THIS-OBJECT:FileName,
                                           OUTPUT DATASET dsTrigger BY-REFERENCE) .                                                                             
            
            oTriggerParser:StatusChanged:Unsubscribe (StatusChangedHandler) .

            DELETE OBJECT oTriggerParser . 
            
            THIS-OBJECT:DisplayStatus ("":U) .
        END.

        IF iMinLeft = 99999 THEN iMinLeft = 0 . 
        IF iMinTop  = 99999 THEN iMinTop  = 0 . 

        FOR EACH ttSelectedFrameWidget WHERE ttSelectedFrameWidget.IsSelected = TRUE
                       BY ttSelectedFrameWidget.Order:

            IF ttSelectedFrameWidget.FieldName > "":U THEN 
                ASSIGN cName = ENTRY (NUM-ENTRIES (ttSelectedFrameWidget.FieldName, ".":U), ttSelectedFrameWidget.FieldName, ".":U) . 
            ELSE 
                ASSIGN cName = "":U . 

            oField = NEW ViewerFieldSpec (cName) .

            IF NUM-ENTRIES (ttSelectedFrameWidget.FieldName, ".":U) > 1 THEN 
                ASSIGN oField:ControlName = DesignerHelper:DesignerSafeName (REPLACE (BusinessEntityDesignerSettings:DefaultBeforeTablePrefix + 
                                                                                      ttSelectedFrameWidget.FieldName, ".":U, "_":U) + 
                                                                                      BusinessEntityDesignerSettings:DefaultTableSuffix) .
            ELSE 
                ASSIGN oField:ControlName = DesignerHelper:DesignerSafeName (REPLACE (ttSelectedFrameWidget.FieldName, ".":U, "_":U)) .
            
            ASSIGN oField:DataType               = ttSelectedFrameWidget.FieldDataType
                   oField:FieldLabel             = ttSelectedFrameWidget.WidgetLabel
                   oField:Left                   = ttSelectedFrameWidget.AtColumn * SESSION:PIXELS-PER-COLUMN - iMinLeft  
                   oField:Top                    = ttSelectedFrameWidget.AtRow * SESSION:PIXELS-PER-ROW - iMinTop
                   oField:NoLabel                = ttSelectedFrameWidget.NoLabels
                   oField:ViewAs                 = ttSelectedFrameWidget.ViewAs
                   oField:PrivateData            = ttSelectedFrameWidget.PrivateData
                   oField:RadioButtons           = ttSelectedFrameWidget.RadioButtons
                   oField:ListItems              = ttSelectedFrameWidget.ListItems
                   oField:ListItemPairs          = ttSelectedFrameWidget.ListItemPairs 
                   oField:WidgetValue            = StringHelper:Unquote (ttSelectedFrameWidget.WidgetValue)
                   oField:AlternativeControlType = ttSelectedFrameWidget.AlternativeControlType
                   .
        
            IF ttSelectedFrameWidget.WidgetType > "":U THEN 
                oField:WidgetType = ttSelectedFrameWidget.WidgetType .
        
            IF ttSelectedFrameWidget.WidgetType = "BUTTON":U OR ttSelectedFrameWidget.WidgetType = "RECTANGLE":U THEN
                oField:NoLabel = TRUE . 
        
            IF ttSelectedFrameWidget.ViewAsWidth > 0 THEN 
                oField:Width = ttSelectedFrameWidget.ViewAsWidth * SESSION:PIXELS-PER-COLUMN .
            ELSE            
                oField:Width = SESSION:PIXELS-PER-COLUMN . 
                   
            IF ttSelectedFrameWidget.ViewAsHeight > 0 THEN 
                oField:Height = ttSelectedFrameWidget.ViewAsHeight * SESSION:PIXELS-PER-ROW .
            ELSE            
                oField:Height = SESSION:PIXELS-PER-ROW . 
                   
            IF lParseTrigger THEN DO:
                
                FOR EACH ttTriggerEvent WHERE ttTriggerEvent.TriggerWidget = ttSelectedFrameWidget.FieldName
                                         AND (ttTriggerEvent.TriggerFrame  = UNBOX (coFrameName:Value)
                                          OR  ttTriggerEvent.TriggerFrame  = "":U),
                    FIRST ttTriggerCode OF ttTriggerEvent:
                                              
                    ASSIGN lcCode = ttTriggerCode.TriggerCode .

                    CASE ttTriggerEvent.TriggerEvent:
                        WHEN "CHOOSE":U THEN 
                            oField:ClickTriggerBlock = THIS-OBJECT:CommentBlock (ttTriggerEvent.TriggerEvent,
                                                                                 ttTriggerEvent.TriggerWidget,
                                                                                 ttTriggerEvent.TriggerFrame,
                                                                                 lcCode) .

                        WHEN "LEAVE":U THEN 
                            oField:ValidatingTriggerBlock = THIS-OBJECT:CommentBlock (ttTriggerEvent.TriggerEvent,
                                                                                      ttTriggerEvent.TriggerWidget,
                                                                                      ttTriggerEvent.TriggerFrame,
                                                                                      lcCode) .

                        WHEN "VALUE-CHANGED":U THEN 
                            oField:ValueChangedTriggerBlock = THIS-OBJECT:CommentBlock (ttTriggerEvent.TriggerEvent,
                                                                                        ttTriggerEvent.TriggerWidget,
                                                                                        ttTriggerEvent.TriggerFrame,
                                                                                        lcCode) .
                                              
                        OTHERWISE DO:
                            MessageFormHelper:ShowMessage (SUBSTITUTE ((IF ttTriggerEvent.TriggerFrame <> "":U THEN "Unable to migrate trigger code for &1 of &2 in frame &3."{&TRAN}
                                                                                                               ELSE "Unable to migrate trigger code for &1 of &2"{&TRAN}),
                                                                        ttTriggerEvent.TriggerEvent,
                                                                        ttTriggerEvent.TriggerWidget,
                                                                        ttTriggerEvent.TriggerFrame),
                                                           THIS-OBJECT:Text,
                                                           MessageFormImages:ImageWarning) . 
                        END.
                    END.
                END.
            END.                   
                   
            oList:Add (oField) .                   
        END.

        RETURN oList . 

	END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Shows the wizard to create a new Business Entity
	    Notes:
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ShowNewBusinessEntityWizard ():
		
		DEFINE VARIABLE oForm        AS CreateBusinessEntityWizardForm NO-UNDO . 
        DEFINE VARIABLE cTableName   AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE oBufferField AS BufferFieldName                NO-UNDO .
        		
		oForm = NEW CreateBusinessEntityWizardForm () . 
	       
	    oForm:StatusChanged:Subscribe (StatusChangedHandler) .
	    oForm:CustomizeBusinessEntity:Subscribe (CustomizeBusinessEntity) . 
	    
	    oForm:Tables:Clear () .
	    
	    FOR EACH ttSelectedFrameWidget WHERE ttSelectedFrameWidget.IsSelected ON ERROR UNDO, THROW:
	        
            IF NUM-ENTRIES (ttSelectedFrameWidget.FieldName, ".":U) > 1 THEN DO:
                oBufferField = BufferHelper:ParseFieldName (ttSelectedFrameWidget.FieldName) .
                
                IF NOT oForm:Tables:ContainsValue (oBufferField:TableName) THEN 
                    oForm:Tables:Add (oBufferField:TableName) .
	        END.
	    END.  
	       
		WAIT-FOR oForm:ShowDialog () . 

        oForm:StatusChanged:Unsubscribe (StatusChangedHandler) .
        
        DELETE OBJECT oForm . 
    
        FINALLY:
            THIS-OBJECT:DisplayStatus ("":U) .    		
        END FINALLY.

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Opens the Options Dialog
	    Notes:
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ShowOptionsDialog ():
		
		DEFINE VARIABLE oForm AS AblFrameMigrationSettingsForm NO-UNDO . 

        oForm = NEW AblFrameMigrationSettingsForm () . 
        
        WAIT-FOR oForm:ShowDialog () .  

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DragDrop event of the Form
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The DragEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID AblFrameMigrationForm_DragDrop (sender AS System.Object,
	                                                    e AS System.Windows.Forms.DragEventArgs):
		
		DEFINE VARIABLE cFileNames AS CHARACTER NO-UNDO EXTENT .
		
        IF DragAndDropHelper:IsFileDrop (e:Data) AND 
           smartToolbarController1:Tools["Open File":U]:SharedProps:Enabled THEN DO: 
		
		    ASSIGN cFileNames = DragAndDropHelper:GetDropFileNames (e:Data) .

            THIS-OBJECT:Activate () .  
            THIS-OBJECT:OpenFile (cFileNames[1]) . 
		END.

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DragEnter event of the Form
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The DragEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID AblFrameMigrationForm_DragEnter (sender AS System.Object, 
	                                                     e AS System.Windows.Forms.DragEventArgs):
		
		IF DragAndDropHelper:IsFileDrop (e:Data) AND 
		   smartToolbarController1:Tools["Open File":U]:SharedProps:Enabled THEN 
		   
		   e:Effect = System.Windows.Forms.DragDropEffects:Copy . 

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ValueChanged event of the coFrameName
        Notes:   
        @param sender The reference to the object that raised the evnet
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID coFrameName_ValueChanged (sender AS System.Object, 
	                                              e AS System.EventArgs):
		
		DEFINE VARIABLE oPreProcessor AS IAblFrameMigrationPreProcessor NO-UNDO .
		DEFINE VARIABLE oSettings     AS ViewerSettings                 NO-UNDO .  
		DEFINE VARIABLE cFrame        AS CHARACTER                      NO-UNDO .
		
        DEFINE BUFFER ttSelectedFrameWidget FOR ttSelectedFrameWidget .

        ASSIGN cFrame = UNBOX (coFrameName:Value) .

        IF CharacterType:IsNullOrEmpty (cFrame) THEN 
            RETURN . 
        
        oParser:GetFrameWidgets (THIS-OBJECT:FileName,
                                 cFrame,
                                 OUTPUT TABLE ttFrameWidget) .
        
        EMPTY TEMP-TABLE ttSelectedFrameWidget .
        
        FOR EACH ttFrameWidget:
            CREATE ttSelectedFrameWidget.
            BUFFER-COPY ttFrameWidget TO ttSelectedFrameWidget .
            
            /* Mike Fechner, Consultingwerk Ltd. 01.11.2015
               Typically a Browse becomes it's own object */
            IF ttSelectedFrameWidget.WidgetType = WidgetTypeEnum:Browse THEN 
                ASSIGN ttSelectedFrameWidget.IsSelected = FALSE .
        END. 
        
        /* Mike Fechner, Consultingwerk Ltd. 10.12.2014
           SCL-541: Allow to post-process parser result through customization class */
        oPreProcessor = {Consultingwerk/get-service.i Consultingwerk.Studio.LegacyGuiMigration.Frame.IAblFrameMigrationPreProcessor} .
        
        IF VALID-OBJECT (oPreProcessor) THEN DO:

            ASSIGN oSettings = NEW ViewerSettings () 
                   oSettings:CreateBindingSource = ultraCheckEditor1:Checked 
                   oSettings:EntityName          = ultraTextEditor1:Text
                   oSettings:EntityTables        = ultraTextEditor2:Text
                   oSettings:ViewerPackage       = ultraTextEditor3:Text
                   oSettings:ViewerName          = ultraTextEditor4:Text .

            oPreProcessor:PreProcessFrameWidgets (THIS-OBJECT:FileName,
                                                  cFrame,
                                                  INPUT-OUTPUT TABLE ttSelectedFrameWidget BY-REFERENCE,
                                                  INPUT-OUTPUT oSettings) .
                                                  
            IF VALID-OBJECT (oSettings) THEN                                                   
                ASSIGN ultraCheckEditor1:Checked = oSettings:CreateBindingSource   
                       ultraTextEditor1:Text     = oSettings:EntityName           
                       ultraTextEditor2:Text     = oSettings:EntityTables        
                       ultraTextEditor3:Text     = oSettings:ViewerPackage        
                       ultraTextEditor4:Text     = oSettings:ViewerName           
                       .
        END.
        
        smartTempTableAdapter1:RetrieveData () .
        
        THIS-OBJECT:DisplayStatus ("":U) .
        
        CATCH err AS Progress.Lang.Error :
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

	END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Converts a source code block into a comment
	    Notes:
	    @param pcEvent The name of the event
	    @param pcWidget The name of the widget
	    @param pcFrame The frame name
	    @param pcSource The trigger source code
	    @return The commented source code block
	------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER CommentBlock (pcEvent AS CHARACTER,
	                                         pcWidget AS CHARACTER,
	                                         pcFrame AS CHARACTER,
	                                         pcSource AS LONGCHAR):
		
		DEFINE VARIABLE cComment AS CHARACTER NO-UNDO.
		
		IF pcFrame > "":U THEN 
		    ASSIGN cComment = SUBSTITUTE ("        /* Trigger code from ON &1 OF &2 IN FRAME &3 ~n":U,
		                                  pcEvent, 
		                                  pcWidget,
		                                  pcFrame) .
        ELSE 
            ASSIGN cComment = SUBSTITUTE ("        /* Trigger code from ON &1 OF &2 ~n":U,
                                          pcEvent, 
                                          pcWidget) .

        ASSIGN cComment = cComment + SUBSTITUTE ("           &1 - &2 ~n~n&3~n":U,
                                                 THIS-OBJECT:FileName,
                                                 STRING (NOW, "99.99.9999 hh:mm:ss":U),
                                                 StringHelper:Indent (STRING (pcSource), 8)) .
                                                 
        ASSIGN cComment = cComment + "        */~n":U .

        RETURN cComment .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Marks all records as deselected 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID DeselectAll ():
		
		THIS-OBJECT:SelectAll (FALSE) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Displays the Status
        Notes:   
        @param pcStatus The status to display
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID DisplayStatus (pcStatus AS CHARACTER):
		
		ultraStatusBar1:Panels["status":U]:Text = pcStatus . 

        UserInterfaceHelper:ProcessEvents () . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates a Temp-Table based on the Variables or Fields used in 
                 a FRAME
        Notes:   Starting point for generating Business Entities based on existing 
                 Frames
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID GenerateTempTable ():
		
        DEFINE VARIABLE lcFields      AS LONGCHAR                          NO-UNDO . 
        DEFINE VARIABLE lcSource      AS LONGCHAR                          NO-UNDO . 
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO . 
        DEFINE VARIABLE cName         AS CHARACTER                         NO-UNDO .

        WAIT-FOR saveFileDialog1:ShowDialog () SET oDialogResult . 
        
        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .	   
	
        THIS-OBJECT:SaveUnsavedChanges () .
		
		ASSIGN cName = ENTRY (1, FileHelper:ShortFileName (saveFileDialog1:FileName), ".":U) . 
		
        FOR EACH ttSelectedFrameWidget WHERE ttSelectedFrameWidget.IsSelected
                                         AND ttSelectedFrameWidget.FieldDataType > "":U: 
            
            ASSIGN lcFields = lcFields + SUBSTITUTE ("    FIELD &1 AS &2":U, ENTRY (NUM-ENTRIES (ttSelectedFrameWidget.FieldName, ".":U), ttSelectedFrameWidget.FieldName, ".":U),
                                                                             ttSelectedFrameWidget.FieldDataType) . 

            IF ttSelectedFrameWidget.FieldFormat > "":U THEN 
                lcFields = lcFields + SUBSTITUTE (" FORMAT ~"&1~"":U, ttSelectedFrameWidget.FieldFormat) .  

            IF ttSelectedFrameWidget.WidgetLabel > "":U THEN 
                lcFields = lcFields + SUBSTITUTE (" LABEL ~"&1~"":U, ttSelectedFrameWidget.WidgetLabel) .  

            lcFields = lcFields + Consultingwerk.Environment:NewLine .
                                   
        END . 

        COPY-LOB FROM FILE TemplateHelper:Find ("temp-table.template":U) 
                 TO lcSource.
        
        ASSIGN lcSource = SUBSTITUTE (lcSource, 
                                      cName,
                                      cName + "Before":U,
                                      "BEFORE-TABLE":U,
                                      lcFields,
                                      "":U,
                                      STRING (NOW, "99.99.9999 HH:MM:SS":U),
                                      "":U,
                                      "":U,
                                      "":U) .

        /* Mike Fechner, Consultingwerk Ltd. 18.06.2012
           user name, replace @USER@, with the current user name */                                      
        ASSIGN lcSource = REPLACE (lcSource, "@USER@":U, "":U) 
               lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, "":U)
               lcSource = StringHelper:FixLineEnding (lcSource) .           

        COPY-LOB FROM lcSource TO FILE saveFileDialog1:FileName .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the SmartViewerControl based on the setting in the screen 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID GenerateViewer ():
		
        DEFINE VARIABLE oBindingSource AS SmartBusinessEntityBindingSource NO-UNDO .
        DEFINE VARIABLE oBuilder       AS SmartViewerBuilder               NO-UNDO .
        DEFINE VARIABLE cFileName      AS CHARACTER                        NO-UNDO .
        DEFINE VARIABLE oList          AS ListViewerFieldSpec              NO-UNDO .
    
        THIS-OBJECT:SaveUnsavedChanges () .
        
        /* Mike Fechner, Consultingwerk Ltd. 10.07.2014
           Cannot proceed without Entity Name, Entity Table */
        IF ultraCheckEditor1:Checked THEN DO: 
            Assert:NotNullOrEmpty (ultraTextEditor1:Text, "Business Entity Name"{&TRAN}) .
            Assert:NotNullOrEmpty (ultraTextEditor2:Text, "Entity Table"{&TRAN}) .

            oBindingSource = NEW SmartBusinessEntityBindingSource () .
            
            ASSIGN oBindingSource:EntityName  = ultraTextEditor1:Text
                   oBindingSource:EntityTable = ENTRY (1, ultraTextEditor2:Text)
                   oBindingSource:EntityView  = ListHelper:RemoveEntry (ultraTextEditor2:Text, 1)
                   oBindingSource:EntityJoin  = TRIM (FILL ("yes,":U, NUM-ENTRIES (oBindingSource:EntityView)), ",":U) 
                        .  
    
            oBindingSource:DesignerImportSchema (TRUE) .        
    
            /* Mike Fechner, Consultingwerk Ltd. 10.07.2014
               Unable to continue without Table Schmea */
            IF NOT VALID-OBJECT (oBindingSource:TableSchema) THEN 
                RETURN . 
        END.
        
        oBuilder = NEW SmartViewerBuilder () .
        
        oList = THIS-OBJECT:PrepareViewerBuilder (oBuilder, oBindingSource) .
                
        cFileName = oBuilder:Generate (oList) .
        
        IF cFileName > "":U THEN 
            WindowsExplorerHelper:ShowFileInExplorer (cFileName) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID InitializeComponent ():
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool56 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool56 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Options":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool58 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool58 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open File":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool59 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool59 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open in Proparse":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("start":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("migration":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool48 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool48 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open File":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Generate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool39 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool39 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GenerateWeb2View":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Generate Temp-Table":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("New Business Entity":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool50 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool50 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Select all":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool51 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool51 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Deselect all":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("settings":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool1 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool1 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Left/Top Align":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool3 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool3 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Parse Trigger Code":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("navigation":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup4 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("maintenance":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool15 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool15 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool17 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool17 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup5 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup5 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool54 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool54 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open in Proparse":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool20 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool20 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool21 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool21 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool22 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool22 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("NavigationToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool23 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool23 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool24 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool24 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool25 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool25 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool26 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool26 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TableIOToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool29 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool29 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool30 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool30 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool31 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool31 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool32 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool32 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar3 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar3 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TransactionToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool33 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool33 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool34 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool34 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool35 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool35 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool36 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool36 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool37 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool37 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool38 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool38 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool41 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool41 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool42 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool42 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool43 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool43 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool44 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool44 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool45 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool45 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool46 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool46 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool47 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool47 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
        appearance28 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool49 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool49 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open File":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
        appearance29 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
        appearance30 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool16 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool16 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Generate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
        appearance31 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
        appearance32 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Generate Temp-Table":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
        appearance33 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
        appearance34 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool52 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool52 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Select all":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance35 AS Infragistics.Win.Appearance NO-UNDO.
        appearance35 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance36 AS Infragistics.Win.Appearance NO-UNDO.
        appearance36 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool53 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool53 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Deselect all":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance37 AS Infragistics.Win.Appearance NO-UNDO.
        appearance37 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance38 AS Infragistics.Win.Appearance NO-UNDO.
        appearance38 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool2 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool2 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Left/Top Align":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance39 AS Infragistics.Win.Appearance NO-UNDO.
        appearance39 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance40 AS Infragistics.Win.Appearance NO-UNDO.
        appearance40 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool55 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool55 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open in Proparse":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance41 AS Infragistics.Win.Appearance NO-UNDO.
        appearance41 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance42 AS Infragistics.Win.Appearance NO-UNDO.
        appearance42 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool4 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool4 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Parse Trigger Code":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance43 AS Infragistics.Win.Appearance NO-UNDO.
        appearance43 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance44 AS Infragistics.Win.Appearance NO-UNDO.
        appearance44 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool57 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool57 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Options":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance45 AS Infragistics.Win.Appearance NO-UNDO.
        appearance45 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance46 AS Infragistics.Win.Appearance NO-UNDO.
        appearance46 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("New Business Entity":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance47 AS Infragistics.Win.Appearance NO-UNDO.
        appearance47 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance48 AS Infragistics.Win.Appearance NO-UNDO.
        appearance48 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool28 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool28 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GenerateWeb2View":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance49 AS Infragistics.Win.Appearance NO-UNDO.
        appearance49 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance50 AS Infragistics.Win.Appearance NO-UNDO.
        appearance50 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton3 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton3 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ttFrameWidget":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn14 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn14 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("IsSelected":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("Order":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldDataType":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("WidgetType":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldFormat":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn6 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn6 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("AtRow":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn7 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn7 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("AtColumn":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn8 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn8 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ViewAs":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn9 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn9 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ViewAsWidth":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn10 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn10 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ViewAsHeight":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn11 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn11 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("WidgetValue":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn12 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn12 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("WidgetLabel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn13 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn13 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("NoLabels":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn15 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn15 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("AlternativeControlType":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn16 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn16 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("RadioButtons":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn17 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn17 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PrivateData":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn18 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn18 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ListItems":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn19 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn19 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ListItemPairs":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("ttFrameWidget":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraStatusPanel1 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel1 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraStatusPanel2 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel2 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Studio.LegacyGuiMigration.Frame.AblFrameMigrationForm":U).
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraCheckEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTextEditor4 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTextEditor3 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTextEditor2 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:coFrameName = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:smartUpdatableBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartTempTableAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        THIS-OBJECT:saveFileDialog1 = NEW System.Windows.Forms.SaveFileDialog().
        THIS-OBJECT:ultraStatusBar1 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusBar().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:ultraCheckEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor4, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor3, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:coFrameName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraStatusBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar0[1] = buttonTool56.
        arrayvar0[2] = buttonTool1.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar0).
        buttonTool3:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool5:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar1[1] = buttonTool58.
        arrayvar1[2] = buttonTool59.
        arrayvar1[3] = buttonTool3.
        arrayvar1[4] = buttonTool4.
        arrayvar1[5] = buttonTool5.
        arrayvar1[6] = buttonTool7.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar1).
        resources:ApplyResources(ribbonTab1, "ribbonTab1":U).
        resources:ApplyResources(ribbonGroup1, "ribbonGroup1":U).
        buttonTool48:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool12:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool39:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool18:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool18:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool2:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool50:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 7 NO-UNDO.
        arrayvar2[1] = buttonTool48.
        arrayvar2[2] = buttonTool12.
        arrayvar2[3] = buttonTool39.
        arrayvar2[4] = buttonTool18.
        arrayvar2[5] = buttonTool2.
        arrayvar2[6] = buttonTool50.
        arrayvar2[7] = buttonTool51.
        ribbonGroup1:Tools:AddRange(arrayvar2).
        resources:ApplyResources(ribbonGroup2, "ribbonGroup2":U).
        stateButtonTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        stateButtonTool1:MenuDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonMenuDisplayStyle:DisplayCheckmark.
        stateButtonTool3:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar3[1] = stateButtonTool1.
        arrayvar3[2] = stateButtonTool3.
        ribbonGroup2:Tools:AddRange(arrayvar3).
        resources:ApplyResources(ribbonGroup3, "ribbonGroup3":U).
        buttonTool8:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool8:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool9:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool9:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool10:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool10:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool11:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool11:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar4[1] = buttonTool8.
        arrayvar4[2] = buttonTool9.
        arrayvar4[3] = buttonTool10.
        arrayvar4[4] = buttonTool11.
        ribbonGroup3:Tools:AddRange(arrayvar4).
        resources:ApplyResources(ribbonGroup4, "ribbonGroup4":U).
        buttonTool13:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool13:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool14:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool14:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool15:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool17:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar5[1] = buttonTool13.
        arrayvar5[2] = buttonTool14.
        arrayvar5[3] = buttonTool15.
        arrayvar5[4] = buttonTool17.
        ribbonGroup4:Tools:AddRange(arrayvar5).
        resources:ApplyResources(ribbonGroup5, "ribbonGroup5":U).
        buttonTool54:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar6[1] = buttonTool54.
        ribbonGroup5:Tools:AddRange(arrayvar6).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 5 NO-UNDO.
        arrayvar7[1] = ribbonGroup1.
        arrayvar7[2] = ribbonGroup2.
        arrayvar7[3] = ribbonGroup3.
        arrayvar7[4] = ribbonGroup4.
        arrayvar7[5] = ribbonGroup5.
        ribbonTab1:Groups:AddRange(arrayvar7).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar8[1] = ribbonTab1.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar8).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar9[1] = buttonTool20.
        arrayvar9[2] = buttonTool21.
        arrayvar9[3] = buttonTool22.
        THIS-OBJECT:smartToolbarController1:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar9).
        THIS-OBJECT:smartToolbarController1:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        THIS-OBJECT:smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        buttonTool23:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar10[1] = buttonTool23.
        arrayvar10[2] = buttonTool24.
        arrayvar10[3] = buttonTool25.
        arrayvar10[4] = buttonTool26.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar10).
        resources:ApplyResources(ultraToolbar1, "ultraToolbar1":U).
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar11[1] = buttonTool29.
        arrayvar11[2] = buttonTool30.
        arrayvar11[3] = buttonTool31.
        arrayvar11[4] = buttonTool32.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar11).
        resources:ApplyResources(ultraToolbar2, "ultraToolbar2":U).
        ultraToolbar3:DockedColumn = 1.
        ultraToolbar3:DockedRow = 1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar12[1] = buttonTool33.
        arrayvar12[2] = buttonTool34.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar12).
        resources:ApplyResources(ultraToolbar3, "ultraToolbar3":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 3 NO-UNDO.
        arrayvar13[1] = ultraToolbar1.
        arrayvar13[2] = ultraToolbar2.
        arrayvar13[3] = ultraToolbar3.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar13).
        appearance16:Image = CAST(resources:GetObject("appearance16.Image":U), System.Object).
        buttonTool35:SharedPropsInternal:AppearancesSmall:Appearance = appearance16.
        buttonTool35:SharedPropsInternal:Caption = resources:GetString("resource.Caption":U).
        buttonTool35:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption":U).
        buttonTool35:SharedPropsInternal:Enabled = FALSE.
        buttonTool35:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText":U).
        appearance17:Image = CAST(resources:GetObject("appearance17.Image":U), System.Object).
        buttonTool36:SharedPropsInternal:AppearancesSmall:Appearance = appearance17.
        buttonTool36:SharedPropsInternal:Caption = resources:GetString("resource.Caption1":U).
        buttonTool36:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption1":U).
        buttonTool36:SharedPropsInternal:Enabled = FALSE.
        buttonTool36:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText1":U).
        appearance18:Image = CAST(resources:GetObject("appearance18.Image":U), System.Object).
        buttonTool37:SharedPropsInternal:AppearancesSmall:Appearance = appearance18.
        buttonTool37:SharedPropsInternal:Caption = resources:GetString("resource.Caption2":U).
        buttonTool37:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption2":U).
        buttonTool37:SharedPropsInternal:Enabled = FALSE.
        buttonTool37:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText2":U).
        appearance19:Image = CAST(resources:GetObject("appearance19.Image":U), System.Object).
        buttonTool38:SharedPropsInternal:AppearancesSmall:Appearance = appearance19.
        buttonTool38:SharedPropsInternal:Caption = resources:GetString("resource.Caption3":U).
        buttonTool38:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption3":U).
        buttonTool38:SharedPropsInternal:Enabled = FALSE.
        buttonTool38:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText3":U).
        appearance20:Image = CAST(resources:GetObject("appearance20.Image":U), System.Object).
        buttonTool41:SharedPropsInternal:AppearancesSmall:Appearance = appearance20.
        buttonTool41:SharedPropsInternal:Caption = resources:GetString("resource.Caption4":U).
        buttonTool41:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption4":U).
        buttonTool41:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText4":U).
        appearance21:Image = CAST(resources:GetObject("appearance21.Image":U), System.Object).
        buttonTool42:SharedPropsInternal:AppearancesSmall:Appearance = appearance21.
        buttonTool42:SharedPropsInternal:Caption = resources:GetString("resource.Caption5":U).
        buttonTool42:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption5":U).
        buttonTool42:SharedPropsInternal:Enabled = FALSE.
        buttonTool42:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText5":U).
        appearance22:Image = CAST(resources:GetObject("appearance22.Image":U), System.Object).
        buttonTool43:SharedPropsInternal:AppearancesSmall:Appearance = appearance22.
        buttonTool43:SharedPropsInternal:Caption = resources:GetString("resource.Caption6":U).
        buttonTool43:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption6":U).
        buttonTool43:SharedPropsInternal:Enabled = FALSE.
        buttonTool43:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText6":U).
        appearance23:Image = CAST(resources:GetObject("appearance23.Image":U), System.Object).
        buttonTool44:SharedPropsInternal:AppearancesSmall:Appearance = appearance23.
        buttonTool44:SharedPropsInternal:Caption = resources:GetString("resource.Caption7":U).
        buttonTool44:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption7":U).
        buttonTool44:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText7":U).
        appearance24:Image = CAST(resources:GetObject("appearance24.Image":U), System.Object).
        buttonTool45:SharedPropsInternal:AppearancesSmall:Appearance = appearance24.
        buttonTool45:SharedPropsInternal:Caption = resources:GetString("resource.Caption8":U).
        buttonTool45:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance25:Image = CAST(resources:GetObject("appearance25.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesLarge:Appearance = appearance25.
        appearance26:Image = CAST(resources:GetObject("appearance26.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesSmall:Appearance = appearance26.
        buttonTool46:SharedPropsInternal:Caption = resources:GetString("resource.Caption9":U).
        buttonTool46:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption8":U).
        buttonTool46:SharedPropsInternal:Enabled = FALSE.
        buttonTool46:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText8":U).
        appearance27:Image = CAST(resources:GetObject("appearance27.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesLarge:Appearance = appearance27.
        appearance28:Image = CAST(resources:GetObject("appearance28.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesSmall:Appearance = appearance28.
        buttonTool47:SharedPropsInternal:Caption = resources:GetString("resource.Caption10":U).
        buttonTool47:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption9":U).
        buttonTool47:SharedPropsInternal:Enabled = FALSE.
        buttonTool47:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText9":U).
        appearance29:Image = CAST(resources:GetObject("appearance29.Image":U), System.Object).
        buttonTool49:SharedPropsInternal:AppearancesLarge:Appearance = appearance29.
        appearance30:Image = CAST(resources:GetObject("appearance30.Image":U), System.Object).
        buttonTool49:SharedPropsInternal:AppearancesSmall:Appearance = appearance30.
        buttonTool49:SharedPropsInternal:Caption = resources:GetString("resource.Caption11":U).
        appearance31:Image = CAST(resources:GetObject("appearance31.Image":U), System.Object).
        buttonTool16:SharedPropsInternal:AppearancesLarge:Appearance = appearance31.
        appearance32:Image = CAST(resources:GetObject("appearance32.Image":U), System.Object).
        buttonTool16:SharedPropsInternal:AppearancesSmall:Appearance = appearance32.
        buttonTool16:SharedPropsInternal:Caption = resources:GetString("resource.Caption12":U).
        appearance33:Image = CAST(resources:GetObject("appearance33.Image":U), System.Object).
        buttonTool19:SharedPropsInternal:AppearancesLarge:Appearance = appearance33.
        appearance34:Image = CAST(resources:GetObject("appearance34.Image":U), System.Object).
        buttonTool19:SharedPropsInternal:AppearancesSmall:Appearance = appearance34.
        buttonTool19:SharedPropsInternal:Caption = resources:GetString("resource.Caption13":U).
        appearance35:Image = CAST(resources:GetObject("appearance35.Image":U), System.Object).
        buttonTool52:SharedPropsInternal:AppearancesLarge:Appearance = appearance35.
        appearance36:Image = CAST(resources:GetObject("appearance36.Image":U), System.Object).
        buttonTool52:SharedPropsInternal:AppearancesSmall:Appearance = appearance36.
        buttonTool52:SharedPropsInternal:Caption = resources:GetString("resource.Caption14":U).
        appearance37:Image = CAST(resources:GetObject("appearance37.Image":U), System.Object).
        buttonTool53:SharedPropsInternal:AppearancesLarge:Appearance = appearance37.
        appearance38:Image = CAST(resources:GetObject("appearance38.Image":U), System.Object).
        buttonTool53:SharedPropsInternal:AppearancesSmall:Appearance = appearance38.
        buttonTool53:SharedPropsInternal:Caption = resources:GetString("resource.Caption15":U).
        stateButtonTool2:MenuDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonMenuDisplayStyle:DisplayCheckmark.
        appearance39:Image = CAST(resources:GetObject("appearance39.Image":U), System.Object).
        stateButtonTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance39.
        appearance40:Image = CAST(resources:GetObject("appearance40.Image":U), System.Object).
        stateButtonTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance40.
        stateButtonTool2:SharedPropsInternal:Caption = resources:GetString("resource.Caption16":U).
        appearance41:Image = CAST(resources:GetObject("appearance41.Image":U), System.Object).
        buttonTool55:SharedPropsInternal:AppearancesLarge:Appearance = appearance41.
        appearance42:Image = CAST(resources:GetObject("appearance42.Image":U), System.Object).
        buttonTool55:SharedPropsInternal:AppearancesSmall:Appearance = appearance42.
        buttonTool55:SharedPropsInternal:Caption = resources:GetString("resource.Caption17":U).
        buttonTool55:SharedPropsInternal:Enabled = FALSE.
        appearance43:Image = CAST(resources:GetObject("appearance43.Image":U), System.Object).
        stateButtonTool4:SharedPropsInternal:AppearancesLarge:Appearance = appearance43.
        appearance44:Image = CAST(resources:GetObject("appearance44.Image":U), System.Object).
        stateButtonTool4:SharedPropsInternal:AppearancesSmall:Appearance = appearance44.
        stateButtonTool4:SharedPropsInternal:Caption = resources:GetString("resource.Caption18":U).
        appearance45:Image = CAST(resources:GetObject("appearance45.Image":U), System.Object).
        buttonTool57:SharedPropsInternal:AppearancesLarge:Appearance = appearance45.
        appearance46:Image = CAST(resources:GetObject("appearance46.Image":U), System.Object).
        buttonTool57:SharedPropsInternal:AppearancesSmall:Appearance = appearance46.
        resources:ApplyResources(buttonTool57:SharedPropsInternal, "buttonTool57.SharedPropsInternal":U).
        buttonTool57:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        buttonTool57:ForceApplyResources = "SharedPropsInternal":U.
        appearance47:Image = CAST(resources:GetObject("appearance47.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesLarge:Appearance = appearance47.
        appearance48:Image = CAST(resources:GetObject("appearance48.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesSmall:Appearance = appearance48.
        resources:ApplyResources(buttonTool6:SharedPropsInternal, "buttonTool6.SharedPropsInternal":U).
        buttonTool6:ForceApplyResources = "SharedPropsInternal":U.
        appearance49:Image = CAST(resources:GetObject("appearance49.Image":U), System.Object).
        buttonTool28:SharedPropsInternal:AppearancesLarge:Appearance = appearance49.
        appearance50:Image = CAST(resources:GetObject("appearance50.Image":U), System.Object).
        buttonTool28:SharedPropsInternal:AppearancesSmall:Appearance = appearance50.
        resources:ApplyResources(buttonTool28:SharedPropsInternal, "buttonTool28.SharedPropsInternal":U).
        buttonTool28:ForceApplyResources = "SharedPropsInternal":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 22 NO-UNDO.
        arrayvar14[1] = buttonTool35.
        arrayvar14[2] = buttonTool36.
        arrayvar14[3] = buttonTool37.
        arrayvar14[4] = buttonTool38.
        arrayvar14[5] = buttonTool41.
        arrayvar14[6] = buttonTool42.
        arrayvar14[7] = buttonTool43.
        arrayvar14[8] = buttonTool44.
        arrayvar14[9] = buttonTool45.
        arrayvar14[10] = buttonTool46.
        arrayvar14[11] = buttonTool47.
        arrayvar14[12] = buttonTool49.
        arrayvar14[13] = buttonTool16.
        arrayvar14[14] = buttonTool19.
        arrayvar14[15] = buttonTool52.
        arrayvar14[16] = buttonTool53.
        arrayvar14[17] = stateButtonTool2.
        arrayvar14[18] = buttonTool55.
        arrayvar14[19] = stateButtonTool4.
        arrayvar14[20] = buttonTool57.
        arrayvar14[21] = buttonTool6.
        arrayvar14[22] = buttonTool28.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar14).
        THIS-OBJECT:smartToolbarController1:ToolClick:Subscribe(THIS-OBJECT:smartToolbarController1_ToolClick).
        /*  */
        /* AblFrameMigrationForm_Fill_Panel */
        /*  */
        /*  */
        /* AblFrameMigrationForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraCheckEditor1).
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraTextEditor4).
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraTextEditor3).
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraTextEditor2).
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:coFrameName).
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:smartUpdatableBrowser1).
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        resources:ApplyResources(THIS-OBJECT:AblFrameMigrationForm_Fill_Panel, "AblFrameMigrationForm_Fill_Panel":U).
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:Name = "AblFrameMigrationForm_Fill_Panel":U.
        /*  */
        /* ultraCheckEditor1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraCheckEditor1, "ultraCheckEditor1":U).
        THIS-OBJECT:ultraCheckEditor1:Checked = TRUE.
        THIS-OBJECT:ultraCheckEditor1:CheckState = System.Windows.Forms.CheckState:Checked.
        THIS-OBJECT:ultraCheckEditor1:Name = "ultraCheckEditor1":U.
        THIS-OBJECT:ultraCheckEditor1:CheckedChanged:Subscribe(THIS-OBJECT:ultraCheckEditor1_CheckedChanged).
        /*  */
        /* ultraLabel5 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel5, "ultraLabel5":U).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        /*  */
        /* ultraLabel4 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel4, "ultraLabel4":U).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        /*  */
        /* ultraTextEditor4 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTextEditor4, "ultraTextEditor4":U).
        THIS-OBJECT:ultraTextEditor4:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraTextEditor4:Name = "ultraTextEditor4":U.
        /*  */
        /* ultraTextEditor3 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTextEditor3, "ultraTextEditor3":U).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        editorButton1:Appearance = appearance1.
        THIS-OBJECT:ultraTextEditor3:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:ultraTextEditor3:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraTextEditor3:Name = "ultraTextEditor3":U.
        THIS-OBJECT:ultraTextEditor3:EditorButtonClick:Subscribe(THIS-OBJECT:ultraTextEditor3_EditorButtonClick).
        THIS-OBJECT:ultraTextEditor3:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* ultraTextEditor2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTextEditor2, "ultraTextEditor2":U).
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        editorButton2:Appearance = appearance2.
        THIS-OBJECT:ultraTextEditor2:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:ultraTextEditor2:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraTextEditor2:Name = "ultraTextEditor2":U.
        THIS-OBJECT:ultraTextEditor2:EditorButtonClick:Subscribe(THIS-OBJECT:ultraTextEditor2_EditorButtonClick).
        THIS-OBJECT:ultraTextEditor2:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* ultraTextEditor1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTextEditor1, "ultraTextEditor1":U).
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        editorButton3:Appearance = appearance3.
        THIS-OBJECT:ultraTextEditor1:ButtonsRight:Add(editorButton3).
        THIS-OBJECT:ultraTextEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:EditorButtonClick:Subscribe(THIS-OBJECT:ultraTextEditor1_EditorButtonClick).
        THIS-OBJECT:ultraTextEditor1:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* ultraLabel3 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel3, "ultraLabel3":U).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel2, "ultraLabel2":U).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* coFrameName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:coFrameName, "coFrameName":U).
        THIS-OBJECT:coFrameName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:coFrameName:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:coFrameName:Name = "coFrameName":U.
        THIS-OBJECT:coFrameName:ValueChanged:Subscribe(THIS-OBJECT:coFrameName_ValueChanged).
        /*  */
        /* smartUpdatableBrowser1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartUpdatableBrowser1, "smartUpdatableBrowser1":U).
        THIS-OBJECT:smartUpdatableBrowser1:DataSource = THIS-OBJECT:bindingSource1.
        THIS-OBJECT:smartUpdatableBrowser1:DisabledColumnsForAdd = "Order,FieldName,FieldDataType,WidgetType,FieldFormat,AtRow,AtColumn,ViewAs,ViewAsWidth,ViewAsHeight,WidgetValue":U.
        THIS-OBJECT:smartUpdatableBrowser1:DisabledColumnsForUpdate = "Order,FieldName,FieldDataType,WidgetType,FieldFormat,AtRow,AtColumn,ViewAs,ViewAsWidth,ViewAsHeight,WidgetValue":U.
        appearance4:BackColor = System.Drawing.SystemColors:Window.
        appearance4:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Appearance = appearance4.
        ultraGridColumn14:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn14:Header:VisiblePosition = 0.
        ultraGridColumn14:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn1:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn1:Header:VisiblePosition = 2.
        ultraGridColumn1:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn2:Header:VisiblePosition = 3.
        ultraGridColumn2:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn3:Header:VisiblePosition = 4.
        ultraGridColumn3:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn4:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn4:Header:VisiblePosition = 5.
        ultraGridColumn4:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn5:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn5:Header:VisiblePosition = 6.
        ultraGridColumn5:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn6:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn6:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn6:Header:VisiblePosition = 7.
        ultraGridColumn6:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn7:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn7:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn7:Header:VisiblePosition = 8.
        ultraGridColumn7:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn8:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn8:Header:VisiblePosition = 9.
        ultraGridColumn8:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn9:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn9:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn9:Header:VisiblePosition = 10.
        ultraGridColumn9:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn10:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn10:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn10:Header:VisiblePosition = 11.
        ultraGridColumn10:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn11:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn11:Header:VisiblePosition = 12.
        ultraGridColumn11:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn12:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn12:Header:VisiblePosition = 13.
        ultraGridColumn12:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn13:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn13:Header:VisiblePosition = 14.
        ultraGridColumn13:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn15:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn15:Header:VisiblePosition = 1.
        ultraGridColumn15:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn15:Style = Infragistics.Win.UltraWinGrid.ColumnStyle:DropDownList.
        ultraGridColumn16:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn16:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn16:Header:VisiblePosition = 15.
        ultraGridColumn16:Hidden = TRUE.
        ultraGridColumn16:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn17:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn17:Header:VisiblePosition = 16.
        ultraGridColumn17:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn18:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn18:Header:VisiblePosition = 17.
        ultraGridColumn18:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        ultraGridColumn19:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn19:Header:VisiblePosition = 18.
        ultraGridColumn19:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar15 AS System.Object EXTENT 19 NO-UNDO.
        arrayvar15[1] = ultraGridColumn14.
        arrayvar15[2] = ultraGridColumn1.
        arrayvar15[3] = ultraGridColumn2.
        arrayvar15[4] = ultraGridColumn3.
        arrayvar15[5] = ultraGridColumn4.
        arrayvar15[6] = ultraGridColumn5.
        arrayvar15[7] = ultraGridColumn6.
        arrayvar15[8] = ultraGridColumn7.
        arrayvar15[9] = ultraGridColumn8.
        arrayvar15[10] = ultraGridColumn9.
        arrayvar15[11] = ultraGridColumn10.
        arrayvar15[12] = ultraGridColumn11.
        arrayvar15[13] = ultraGridColumn12.
        arrayvar15[14] = ultraGridColumn13.
        arrayvar15[15] = ultraGridColumn15.
        arrayvar15[16] = ultraGridColumn16.
        arrayvar15[17] = ultraGridColumn17.
        arrayvar15[18] = ultraGridColumn18.
        arrayvar15[19] = ultraGridColumn19.
        ultraGridBand1:Columns:AddRange(arrayvar15).
        ultraGridBand1:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand1:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance5:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance5:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance5:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance5:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:Appearance = appearance5.
        appearance6:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance6.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance7:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance7:BackColor2 = System.Drawing.SystemColors:Control.
        appearance7:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance7:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:PromptAppearance = appearance7.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance8:BackColor = System.Drawing.SystemColors:Window.
        appearance8:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveCellAppearance = appearance8.
        appearance9:BackColor = System.Drawing.SystemColors:Highlight.
        appearance9:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveRowAppearance = appearance9.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance10:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CardAreaAppearance = appearance10.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        appearance11:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellAppearance = appearance11.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellPadding = 0.
        appearance12:BackColor = System.Drawing.SystemColors:Control.
        appearance12:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance12:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance12:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance12:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:GroupByRowAppearance = appearance12.
        resources:ApplyResources(appearance13, "appearance13":U).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderAppearance = appearance13.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance14:BackColor = System.Drawing.SystemColors:Window.
        appearance14:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowAppearance = appearance14.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance15:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance15.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:smartUpdatableBrowser1:LinkDataSource = THIS-OBJECT:smartTempTableAdapter1.
        THIS-OBJECT:smartUpdatableBrowser1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartUpdatableBrowser1:Name = "smartUpdatableBrowser1":U.
        THIS-OBJECT:smartUpdatableBrowser1:PromptForSaveValuesOnRowDeactivate = FALSE.
        THIS-OBJECT:smartUpdatableBrowser1:SaveMode = TRUE.
        THIS-OBJECT:smartUpdatableBrowser1:SettingsKey = "80c976fc-fc07-30b9-d813-07a334c88da3":U.
        THIS-OBJECT:smartUpdatableBrowser1:SimpleKeyboardNavigationDuringUpdate = TRUE.
        THIS-OBJECT:smartUpdatableBrowser1:SimpleKeyboardNavigationDuringUpdateForLeftRight = FALSE.
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar16 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar16 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar16.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar17 AS Progress.Data.ColumnPropDesc EXTENT 19 NO-UNDO.
        arrayvar17[1] = NEW Progress.Data.ColumnPropDesc("IsSelected":U, "Selected":U, Progress.Data.DataType:LOGICAL).
        arrayvar17[2] = NEW Progress.Data.ColumnPropDesc("Order":U, "Order":U, Progress.Data.DataType:INTEGER).
        arrayvar17[3] = NEW Progress.Data.ColumnPropDesc("FieldName":U, "Field Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[4] = NEW Progress.Data.ColumnPropDesc("FieldDataType":U, "Data Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[5] = NEW Progress.Data.ColumnPropDesc("WidgetType":U, "Widget Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[6] = NEW Progress.Data.ColumnPropDesc("FieldFormat":U, "Format":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[7] = NEW Progress.Data.ColumnPropDesc("AtRow":U, "Row":U, Progress.Data.DataType:DECIMAL).
        arrayvar17[8] = NEW Progress.Data.ColumnPropDesc("AtColumn":U, "Column":U, Progress.Data.DataType:DECIMAL).
        arrayvar17[9] = NEW Progress.Data.ColumnPropDesc("ViewAs":U, "View-As":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[10] = NEW Progress.Data.ColumnPropDesc("ViewAsWidth":U, "Width":U, Progress.Data.DataType:DECIMAL).
        arrayvar17[11] = NEW Progress.Data.ColumnPropDesc("ViewAsHeight":U, "Height":U, Progress.Data.DataType:DECIMAL).
        arrayvar17[12] = NEW Progress.Data.ColumnPropDesc("WidgetValue":U, "Value":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[13] = NEW Progress.Data.ColumnPropDesc("WidgetLabel":U, "Label":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[14] = NEW Progress.Data.ColumnPropDesc("NoLabels":U, "No Labels":U, Progress.Data.DataType:LOGICAL).
        arrayvar17[15] = NEW Progress.Data.ColumnPropDesc("AlternativeControlType":U, "Alternative Control Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[16] = NEW Progress.Data.ColumnPropDesc("RadioButtons":U, "Radio Buttons":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[17] = NEW Progress.Data.ColumnPropDesc("PrivateData":U, "PRIVATE-DATA":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[18] = NEW Progress.Data.ColumnPropDesc("ListItems":U, "List Items":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[19] = NEW Progress.Data.ColumnPropDesc("ListItemPairs":U, "List Item Pairs":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar17.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* smartTempTableAdapter1 */
        /*  */
        THIS-OBJECT:smartTempTableAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartTempTableAdapter1:LinkNavigationSource = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_AblFrameMigrationForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Left, "m_AblFrameMigrationForm_Toolbars_Dock_Area_Left":U).
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Left:Name = "m_AblFrameMigrationForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_AblFrameMigrationForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Right, "m_AblFrameMigrationForm_Toolbars_Dock_Area_Right":U).
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Right:Name = "m_AblFrameMigrationForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_AblFrameMigrationForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        resources:ApplyResources(THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Top, "m_AblFrameMigrationForm_Toolbars_Dock_Area_Top":U).
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Top:Name = "m_AblFrameMigrationForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_AblFrameMigrationForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        resources:ApplyResources(THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Bottom, "m_AblFrameMigrationForm_Toolbars_Dock_Area_Bottom":U).
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Bottom:Name = "m_AblFrameMigrationForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* openFileDialog1 */
        /*  */
        THIS-OBJECT:openFileDialog1:DefaultExt = "w":U.
        resources:ApplyResources(THIS-OBJECT:openFileDialog1, "openFileDialog1":U).
        THIS-OBJECT:openFileDialog1:RestoreDirectory = TRUE.
        /*  */
        /* saveFileDialog1 */
        /*  */
        THIS-OBJECT:saveFileDialog1:DefaultExt = "i":U.
        resources:ApplyResources(THIS-OBJECT:saveFileDialog1, "saveFileDialog1":U).
        THIS-OBJECT:saveFileDialog1:RestoreDirectory = TRUE.
        /*  */
        /* ultraStatusBar1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraStatusBar1, "ultraStatusBar1":U).
        THIS-OBJECT:ultraStatusBar1:Name = "ultraStatusBar1":U.
        ultraStatusPanel1:Key = "filename":U.
        ultraStatusPanel1:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel1:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel2:Key = "status":U.
        ultraStatusPanel2:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Spring.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar18 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel EXTENT 2 NO-UNDO.
        arrayvar18[1] = ultraStatusPanel1.
        arrayvar18[2] = ultraStatusPanel2.
        THIS-OBJECT:ultraStatusBar1:Panels:AddRange(arrayvar18).
        THIS-OBJECT:ultraStatusBar1:ViewStyle = Infragistics.Win.UltraWinStatusBar.ViewStyle:Office2007.
        /*  */
        /* AblFrameMigrationForm */
        /*  */
        THIS-OBJECT:AllowDrop = TRUE.
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:AblFrameMigrationForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraStatusBar1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_AblFrameMigrationForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Name = "AblFrameMigrationForm":U.
        THIS-OBJECT:DragDrop:Subscribe(THIS-OBJECT:AblFrameMigrationForm_DragDrop).
        THIS-OBJECT:DragEnter:Subscribe(THIS-OBJECT:AblFrameMigrationForm_DragEnter).
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:ClientArea:PerformLayout().
        THIS-OBJECT:AblFrameMigrationForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraCheckEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor4, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor3, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:coFrameName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraStatusBar1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown handler of the Text boxes with an 
                 EditorButton (Lookup)
        Notes:   Maps F3 to EditorButtonClick
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID LookupKeyDownHandler (sender AS System.Object, 
                                              e AS System.Windows.Forms.KeyEventArgs):

        DEFINE VARIABLE cControlName AS CHARACTER NO-UNDO.
        
        IF e:Handled = FALSE AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, SmartLookupSettings:ShortcutKey) 
            AND THIS-OBJECT:Enabled THEN DO:

            ASSIGN cControlName = CAST (sender, System.Windows.Forms.Control):Name .

            DYNAMIC-INVOKE (THIS-OBJECT, 
                            SUBSTITUTE ("&1_EditorButtonClick":U, cControlName), 
                            sender, 
                            NEW EditorButtonEventArgs (?, ?)) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .                
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosing Event
        Notes:   
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnFormClosing (e AS System.Windows.Forms.FormClosingEventArgs):

		SUPER:OnFormClosing (e) .

        IF e:Cancel THEN RETURN . 
		
        THIS-OBJECT:StoreSettings (THIS-OBJECT:ultraTextEditor1:Text, "BusinessEntityName":U) .
        THIS-OBJECT:StoreSettings (THIS-OBJECT:ultraTextEditor2:Text, "EntityTable":U) .
        THIS-OBJECT:StoreSettings (THIS-OBJECT:ultraTextEditor3:Text, "ViewerPackage":U) .
        THIS-OBJECT:StoreSettings (THIS-OBJECT:ultraTextEditor4:Text, "ViewerName":U) .
        THIS-OBJECT:StoreSettings (STRING (UltraToolbarsHelper:GetStateButtonToolChecked (smartToolbarController1, 
                                                                                          "Left/Top Align":U)), 
                                   "LeftTop Align":U) .
        THIS-OBJECT:StoreSettings (STRING (UltraToolbarsHelper:GetStateButtonToolChecked (smartToolbarController1, 
                                                                                          "Parse Trigger Code":U)), 
                                   "Parse Trigger Code":U) .
                                   
        THIS-OBJECT:StoreSettings (STRING (THIS-OBJECT:ultraCheckEditor1:Checked), "Create BindingSource":U) .
        
        oLruController:StoreLru () .          

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event 
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
		
        DEFINE VARIABLE cValue                          AS CHARACTER                                         NO-UNDO . 
        DEFINE VARIABLE oLoader                         AS ServiceLoader                                     NO-UNDO .

        DEFINE VARIABLE oValueList                      AS Infragistics.Win.ValueList                        NO-UNDO .
        DEFINE VARIABLE oAlternativeControlTypeProvider AS ISmartViewerBuilderAlternativeControlTypeProvider NO-UNDO .   
        DEFINE VARIABLE oCustomizer                     AS IAblFrameMigrationFormCustomizer                  NO-UNDO .         		
        		
		SUPER:OnLoad (e) .
		
        ASSIGN cValue = THIS-OBJECT:LoadSettings ("BusinessEntityName":U) .
        
        IF cValue > "":U THEN 
            ASSIGN THIS-OBJECT:ultraTextEditor1:Text = cValue . 
                
        ASSIGN cValue = THIS-OBJECT:LoadSettings ("EntityTable":U) .
        
        IF cValue > "":U THEN 
            ASSIGN THIS-OBJECT:ultraTextEditor2:Text = cValue . 

        ASSIGN cValue = THIS-OBJECT:LoadSettings ("ViewerPackage":U) .
        
        IF cValue > "":U THEN 
            ASSIGN THIS-OBJECT:ultraTextEditor3:Text = cValue . 

        ASSIGN cValue = THIS-OBJECT:LoadSettings ("ViewerName":U) .
        
        IF cValue > "":U THEN 
            ASSIGN THIS-OBJECT:ultraTextEditor4:Text = cValue . 

        ASSIGN cValue = THIS-OBJECT:LoadSettings ("LeftTop Align":U) .
        
        IF cValue = "yes":U OR cValue = "no":U THEN 
            UltraToolbarsHelper:SetStateButtonToolChecked (smartToolbarController1, 
                                                           "Left/Top Align":U, 
                                                           DataTypeHelper:ToLogical (cValue)) .
        
        ASSIGN cValue = THIS-OBJECT:LoadSettings ("Parse Trigger Code":U) .
        
        IF cValue = "yes":U OR cValue = "no":U THEN 
            UltraToolbarsHelper:SetStateButtonToolChecked (smartToolbarController1, 
                                                           "Parse Trigger Code":U, 
                                                           DataTypeHelper:ToLogical (cValue)) .
        
        ASSIGN cValue = THIS-OBJECT:LoadSettings ("Create BindingSource":U) .

        IF cValue = "yes":U OR cValue = "no":U THEN 
            ultraCheckEditor1:Checked = DataTypeHelper:ToLogical (cValue) .
        
        oLruController:LoadLru() .
        
        IF AblFrameMigrationSettings:CustomServices > "":U THEN DO ON ERROR UNDO, THROW:
            
            oLoader = NEW ServiceLoader () . 
            oLoader:Load (AblFrameMigrationSettings:CustomServices) .
            
            CATCH err AS Progress.Lang.Error:
                Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
            END CATCH.
        END.        
               
        oValueList = NEW Infragistics.Win.ValueList () . 
        oValueList:ValueListItems:Add ("":U) .                
        oValueList:ValueListItems:Add ("SmartBusinessEntityLookup":U) .                
        oValueList:ValueListItems:Add ("SmartComboEditor":U) .  
                      
        ASSIGN oAlternativeControlTypeProvider = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.ISmartViewerBuilderAlternativeControlTypeProvider} .                
                
        IF VALID-OBJECT (oAlternativeControlTypeProvider) THEN 
            oAlternativeControlTypeProvider:AddCustomControlTypes (AlternativeControlTypeProviderOriginEnum:AblFrameMigrationForm,
                                                                   oValueList) .                
                
        smartUpdatableBrowser1:DisplayLayout:Bands[0]:Columns["AlternativeControlType":U]:ValueList = oValueList .           

        oCustomizer = {Consultingwerk/get-service.i Consultingwerk.Studio.LegacyGuiMigration.Frame.IAblFrameMigrationFormCustomizer} .                 
               
        IF VALID-OBJECT (oCustomizer) THEN 
            oCustomizer:Startup (TABLE ttSelectedFrameWidget BIND,
                                 DATASET dsTrigger BIND,
                                 THIS-OBJECT,
                                 smartToolbarController1) .                
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shows the openFiledialog and opens a file for parsing 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID OpenFile ():
		
		DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .
		
		WAIT-FOR openFileDialog1:ShowDialog (THIS-OBJECT) SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult}

        THIS-OBJECT:OpenFile (openFileDialog1:FileName) . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shows the openFiledialog and opens a file for parsing 
        Notes:   
        @param pcFileName The name of the file of open
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OpenFile (pcFileName AS CHARACTER):
        
        DEFINE VARIABLE oFrames AS Consultingwerk.Framework.Collections.CharacterList NO-UNDO  .
        
        Consultingwerk.Assertion.FileAssert:Exists (pcFileName) . 
    
        THIS-OBJECT:FileName = pcFileName .  
    
        THIS-OBJECT:ultraStatusBar1:Panels["filename":U]:Text = pcFileName . 

        THIS-OBJECT:smartToolbarController1:Tools["Open in Proparse":U]:SharedProps:Enabled = TRUE .

        oLruController:AddItemToLru (pcFileName, pcFileName) .      
    
        oFrames = oParser:GetFrameNames (pcFileName) .    
        
        IF oFrames:Count > 0 THEN DO: 
            coFrameName:ValueList = Consultingwerk.Util.ValueListHelper:FromList (oFrames) .
            coFrameName:DropDown () .
        END.
        ELSE 
            MessageFormHelper:ShowMessage ("No FRAME Definitions found in source file."{&TRAN},
                                           THIS-OBJECT:Text,
                                           Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageWarning) .
               
        THIS-OBJECT:DisplayStatus ("":U) .
                             
    END METHOD . 
        
    /*------------------------------------------------------------------------------
        Purpose: Opens a File From the LRU
        Notes:   
        @param pcFileName The name of the file to open
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID OpenFileFromLru (pcFileName AS CHARACTER):
		
		THIS-OBJECT:OpenFile (pcFileName) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens the current file in the Proparse Tree View
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID OpenFileInProparse ():
		
		DEFINE VARIABLE oForm AS Consultingwerk.Studio.Proparse.ProparseTreeViewForm NO-UNDO . 
		
		oForm = NEW Consultingwerk.Studio.Proparse.ProparseTreeViewForm () .
		
		oForm:Show () . 

        THIS-OBJECT:DependentForms:Add (oForm) . 
		
		oForm:OpenFileForParsing (THIS-OBJECT:FileName) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Saves unsaved changes in the SmartUpdatableBrowser 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SaveUnsavedChanges ():
		
		IF smartUpdatableBrowser1:SmartTableIOState = TableIOStateEnum:ModifyingData THEN 
		    smartUpdatableBrowser1:SaveChanges () . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Marks all records as selected 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID SelectAll ():
		
		THIS-OBJECT:SelectAll (TRUE) . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Marks all records as selected or deselected
        Notes:   
        @param plSelected Logical value indicating if the records should be selected (true) or deselected (false)
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SelectAll (plSelected AS LOGICAL):
        
        DEFINE BUFFER ttSelectedFrameWidget FOR ttSelectedFrameWidget . 
        
        IF smartUpdatableBrowser1:SmartTableIOState = TableIOStateEnum:ModifyingData THEN 
            smartUpdatableBrowser1:SaveChanges () . 

        IF smartUpdatableBrowser1:SmartTableIOState = TableIOStateEnum:ModifyingData THEN
            RETURN . 
            
        FOR EACH ttSelectedFrameWidget:
            ASSIGN ttSelectedFrameWidget.IsSelected = plSelected . 
        END.
        
        smartTempTableAdapter1:RetrieveData () . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the smartToolbarController1  
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID smartToolbarController1_ToolClick (sender AS System.Object,    
	                                                       e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):
	
        CASE e:Tool:Key:
            WHEN "Open File":U THEN 
                THIS-OBJECT:OpenFile () . 
	        WHEN "Generate":U THEN  
	            THIS-OBJECT:GenerateViewer () . 
	        WHEN "GenerateWeb2View":U THEN 
	            THIS-OBJECT:GenerateWeb2View () .
            WHEN "Generate Temp-Table":U THEN 
                THIS-OBJECT:GenerateTempTable () . 	            
	        WHEN "Select all":U THEN 
	            THIS-OBJECT:SelectAll () . 
            WHEN "Deselect all":U THEN 
                THIS-OBJECT:DeselectAll () . 
            WHEN "Open in Proparse":U THEN 
                THIS-OBJECT:OpenFileInProparse () .  
            WHEN "Options":U THEN 
                THIS-OBJECT:ShowOptionsDialog () .         
            WHEN "New Business Entity":U THEN 
                THIS-OBJECT:ShowNewBusinessEntityWizard () .   
	
		END CASE .

        IF e:Tool:Key BEGINS "lru_":U THEN 
            OpenFileFromLru (UNBOX (e:Tool:SharedProps:RootTool:Tag)) .

        CATCH err AS Progress.Lang.Error :
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the StatusChanged event of the Frame or Trigger
                 Parser
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The StatusChangedEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID StatusChangedHandler (sender AS Progress.Lang.Object,
                                             e AS StatusChangedEventArgs):
		
		THIS-OBJECT:DisplayStatus (e:Status) . 

	END METHOD .

	/*------------------------------------------------------------------------------
	   Purpose: Event handler for the CheckedChanged event of the ultraCheckEditor1
	   Notes:
	   @param sender The reference to the object that raised the event
	   @param e The System.EventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraCheckEditor1_CheckedChanged (sender AS System.Object, 
	                                                      e AS System.EventArgs):
		
		ASSIGN ultraTextEditor1:Enabled = ultraCheckEditor1:Checked
		       ultraTextEditor2:Enabled = ultraCheckEditor1:Checked . 

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the ultraTextEditor1 
        Notes:   Opens the Business Entity search dialog
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraTextEditor1_EditorButtonClick (sender AS System.Object, 
	                                                        e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
		
		DEFINE VARIABLE cEntityName AS CHARACTER NO-UNDO.
		
        Consultingwerk.SmartComponents.Support.BusinessEntityDesignerSupport:SelectBusinessEntityName (OUTPUT cEntityName). 

        IF cEntityName > "":U THEN 
            ASSIGN ultraTextEditor1:Text = cEntityName .  

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the ultraTextEditor2 
        Notes:   Opens the Table Selection dialog for the choosen Business Entity
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraTextEditor2_EditorButtonClick (sender AS System.Object, 
	                                                        e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
		
		DEFINE VARIABLE cTables      AS CHARACTER NO-UNDO .
		DEFINE VARIABLE cEntityJoin  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hDataHandle  AS HANDLE    NO-UNDO .

        ASSIGN cTables = ultraTextEditor2:Text .  

        FrameworkSettings:ServiceAdapter:FetchDataset ("":U, 
                                                      ultraTextEditor1:Text,
                                                      OUTPUT DATASET-HANDLE hDataHandle BY-REFERENCE) . 

        ASSIGN cEntityJoin = TRIM (FILL ("yes,":U, NUM-ENTRIES (cTables)), ",":U) . 

        RUN Consultingwerk\SmartComponents\Support\SmartBusinessEntityAdapterDesigner_ShowBusinessEntityTblPic.p
                (hDataHandle,
                 ultraTextEditor2:Text, 
                 OUTPUT cTables,
                 INPUT-OUTPUT cEntityJoin) .

        IF cTables > "":U THEN 
            ASSIGN ultraTextEditor2:Text = cTables .   

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hDataHandle) .		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the ultraTextEditor3 
        Notes:   Opens the Package Selection dialog 
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraTextEditor3_EditorButtonClick (sender AS System.Object, 
	                                                        e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
		
        DEFINE VARIABLE oDialogResult        AS System.Windows.Forms.DialogResult                   NO-UNDO .
        DEFINE VARIABLE cRelative            AS CHARACTER                                           NO-UNDO .
        
        DEFINE VARIABLE oFolderBrowserDialog AS Consultingwerk.Windows.Util.Forms.FolderBrowserForm NO-UNDO . 

        FILE-INFO:FILE-NAME = ".":U .

        oFolderBrowserDialog = NEW Consultingwerk.Windows.Util.Forms.FolderBrowserForm () . 
        oFolderBrowserDialog:RootFolder = FILE-INFO:FULL-PATHNAME . 
        
        WAIT-FOR oFolderBrowserDialog:ShowDialog() SET oDialogResult .
        
        IF Progress.Util.EnumHelper:AreEqual (oDialogResult, System.Windows.Forms.DialogResult:Ok) THEN DO:
            FILE-INFO:FILE-NAME = ".":U .

            ASSIGN cRelative = FileHelper:RelativeFileName(oFolderBrowserDialog:SelectedFolder)
                   cRelative = REPLACE (cRelative, "/":U, "~\":U) . 

            IF SUBSTRING (cRelative, 2, 1) = ":":U OR cRelative BEGINS "~\~\":U OR cRelative BEGINS "/":U OR cRelative BEGINS ".":U THEN 
                Consultingwerk.Util.MessageFormHelper:ShowMessage 
                              ("Unable to translate path into package."{&TRAN},
                               "Business Entity Designer"{&TRAN},
                               "Path needs to be relative to working directory."{&TRAN},
                               Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageWarning) .
            ELSE
                THIS-OBJECT:ultraTextEditor3:Text = REPLACE (cRelative, "~\":U, ".":U) .
        END.        

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the AblFrameMigrationForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC AblFrameMigrationForm ():

        GarbageCollectorHelper:DeleteObject (oLruController) .

    END DESTRUCTOR .

END CLASS.
