/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ProcedureSchedulerJobCommandControl
    Purpose     : Specialized Job Command Control to maintain Scheduler Procedures Jobs
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Thu May 26 17:00:24 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.*              FROM PROPATH.
USING Consultingwerk.SmartFramework.Scheduler.*    FROM PROPATH .
USING Consultingwerk.Util.*                        FROM PROPATH.
USING Consultingwerk.Windows.Framework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                              FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Scheduler.ProcedureSchedulerJobCommandControl
    INHERITS JobCommandBaseControl:

     DEFINE PRIVATE TEMP-TABLE tParameter NO-UNDO
        FIELD SortOrder  AS INTEGER
        FIELD ParamType  AS CHARACTER
        FIELD ParamValue AS CHARACTER
        .

    DEFINE PRIVATE TEMP-TABLE tDataTypes NO-UNDO
       FIELD DataType AS CHARACTER
       .

    DEFINE PRIVATE VARIABLE cboDataType AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartUpdatableBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartTempTableAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartPanelController1 AS Consultingwerk.SmartComponents.Implementation.SmartPanelController NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for ProcedureSchedulerJobCommandControl
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProcedureSchedulerJobCommandControl ():

        SUPER ().

        InitializeComponent ().

        InitDataTypes().

        EMPTY TEMP-TABLE tParameter.
        smartTempTableAdapter1:TempTableHandle = TEMP-TABLE tParameter:HANDLE .
        smartTempTableAdapter1:RetrieveData () .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the DataTypes for the DropDown
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitDataTypes ():

        DEFINE VARIABLE cDataTypes AS CHARACTER NO-UNDO INITIAL "CHARACTER,DATE,DATETIME,DATETIME-TZ,INTEGER,LOGICAL,LONGCHAR":U.

        cboDataType:ValueList = ValueListHelper:FromList (cDataTypes).

    END METHOD .


    /**
     * Purpose: Initializes the Visual Design
     * Notes:
     */
    @InitializeComponent.
    METHOD PRIVATE VOID InitializeComponent ():
        /* NOTE: The following method is automatically generated.

        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.

        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Scheduler.ProcedureSchedulerJobCommandControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("ttParameter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ttParameter":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SortOrder":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ParamType":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ParamValue":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
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
        THIS-OBJECT:cboDataType = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:smartBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartUpdatableBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser().
        THIS-OBJECT:smartTempTableAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        THIS-OBJECT:smartPanelController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartPanelController().
        CAST(THIS-OBJECT:cboDataType, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* cboDataType */
        /*  */
        THIS-OBJECT:cboDataType:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBindingSource1, "ParamType":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:cboDataType, "cboDataType":U).
        THIS-OBJECT:cboDataType:Name = "cboDataType":U.
        /*  */
        /* smartBindingSource1 */
        /*  */
        THIS-OBJECT:smartBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:smartBindingSource1:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar0 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
        arrayvar1[1] = NEW Progress.Data.ColumnPropDesc("SortOrder":U, "Order":U, Progress.Data.DataType:INTEGER).
        arrayvar1[2] = NEW Progress.Data.ColumnPropDesc("ParamType":U, "DataType":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[3] = NEW Progress.Data.ColumnPropDesc("ParamValue":U, "Value":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar1.
        THIS-OBJECT:smartBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* ultraTextEditor1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTextEditor1, "ultraTextEditor1":U).
        THIS-OBJECT:ultraTextEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel2, "ultraLabel2":U).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        /*  */
        /* smartUpdatableBrowser1 */
        /*  */
        THIS-OBJECT:smartUpdatableBrowser1:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        resources:ApplyResources(THIS-OBJECT:smartUpdatableBrowser1, "smartUpdatableBrowser1":U).
        THIS-OBJECT:smartUpdatableBrowser1:DataSource = THIS-OBJECT:smartBindingSource1.
        THIS-OBJECT:smartUpdatableBrowser1:DeselectRowsOnActivate = TRUE.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Appearance = appearance1.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn1:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        resources:ApplyResources(ultraGridColumn1:Header, "ultraGridColumn1.Header":U).
        ultraGridColumn1:Header:VisiblePosition = 0.
        ultraGridColumn1:ForceApplyResources = "Header":U.
        ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn2:EditorComponent = THIS-OBJECT:cboDataType.
        resources:ApplyResources(ultraGridColumn2:Header, "ultraGridColumn2.Header":U).
        ultraGridColumn2:Header:VisiblePosition = 1.
        ultraGridColumn2:ForceApplyResources = "Header":U.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn3:Header, "ultraGridColumn3.Header":U).
        ultraGridColumn3:Header:VisiblePosition = 2.
        ultraGridColumn3:ForceApplyResources = "Header":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS System.Object EXTENT 3 NO-UNDO.
        arrayvar2[1] = ultraGridColumn1.
        arrayvar2[2] = ultraGridColumn2.
        arrayvar2[3] = ultraGridColumn3.
        ultraGridBand1:Columns:AddRange(arrayvar2).
        ultraGridBand1:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand1:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance2:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:Appearance = appearance2.
        appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:Hidden = TRUE.
        appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance4:BackColor2 = System.Drawing.SystemColors:Control.
        appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance5:BackColor = System.Drawing.SystemColors:Window.
        appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveCellAppearance = appearance5.
        appearance6:BackColor = System.Drawing.SystemColors:Highlight.
        appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveRowAppearance = appearance6.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance7:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CardAreaAppearance = appearance7.
        appearance8:BorderColor = System.Drawing.Color:Silver.
        appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellAppearance = appearance8.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellPadding = 0.
        appearance9:BackColor = System.Drawing.SystemColors:Control.
        appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance9:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:GroupByRowAppearance = appearance9.
        resources:ApplyResources(appearance10, "appearance10":U).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderAppearance = appearance10.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance11:BackColor = System.Drawing.SystemColors:Window.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowAppearance = appearance11.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartUpdatableBrowser1:LinkDataSource = THIS-OBJECT:smartTempTableAdapter1.
        THIS-OBJECT:smartUpdatableBrowser1:LinkTableIOSource = THIS-OBJECT:smartPanelController1.
        THIS-OBJECT:smartUpdatableBrowser1:Name = "smartUpdatableBrowser1":U.
        THIS-OBJECT:smartUpdatableBrowser1:SettingsKey = "ded189e2-b403-e5a1-1014-c049e435c570":U.
        /*  */
        /* smartTempTableAdapter1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartTempTableAdapter1, "smartTempTableAdapter1":U).
        THIS-OBJECT:smartTempTableAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartTempTableAdapter1:LinkNavigationSource = THIS-OBJECT:smartPanelController1.
        THIS-OBJECT:smartTempTableAdapter1:SmartDataSourceActive = TRUE.
        /*  */
        /* smartPanelController1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartPanelController1, "smartPanelController1":U).
        THIS-OBJECT:smartPanelController1:Name = "smartPanelController1":U.
        THIS-OBJECT:smartPanelController1:ShowNavigationTools = FALSE.
        /*  */
        /* ProcedureSchedulerJobCommandControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:cboDataType).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartUpdatableBrowser1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartPanelController1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:Name = "ProcedureSchedulerJobCommandControl":U.
        CAST(THIS-OBJECT:cboDataType, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Clear all editor fields
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID InternalClearDataFields ():

        ASSIGN
            THIS-OBJECT:ultraTextEditor1:Text = "":U
            .

        EMPTY TEMP-TABLE tParameter.
        smartTempTableAdapter1:RetrieveData () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data shown in the UI
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID UpdateUiData ():

        DEFINE VARIABLE oSchedulerJobCommand AS ProcedureSchedulerJobCommand NO-UNDO.
        DEFINE VARIABLE i                    AS INTEGER                      NO-UNDO.


        IF NOT TYPE-OF (THIS-OBJECT:CallParameterObject, ProcedureSchedulerJobCommand) THEN
            RETURN.

        oSchedulerJobCommand = CAST (THIS-OBJECT:CallParameterObject, ProcedureSchedulerJobCommand).

        ASSIGN
            THIS-OBJECT:ultraTextEditor1:Text = oSchedulerJobCommand:ProcedureFileName
            .

        EMPTY TEMP-TABLE tParameter.

        DO i = 1 TO oSchedulerJobCommand:ProcedureParameter:Count:
            CREATE tParameter.
            ASSIGN
                tParameter.SortOrder  = oSchedulerJobCommand:ProcedureParameter:GetITem (i):SortOrder
                tParameter.ParamType  = oSchedulerJobCommand:ProcedureParameter:GetITem (i):ParamType
                tParameter.ParamValue = oSchedulerJobCommand:ProcedureParameter:GetITem (i):ParamValue.
        END.

        smartTempTableAdapter1:RetrieveData () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data of the CallParameter object after the user has
                 entered data to the UI
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID WritebackCallParameterToTextProperty ():

        DEFINE VARIABLE oSchedulerJobCommand AS ProcedureSchedulerJobCommand NO-UNDO.

        IF NOT VALID-OBJECT (THIS-OBJECT:CallParameterObject) OR
           NOT TYPE-OF (THIS-OBJECT:CallParameterObject, ProcedureSchedulerJobCommand) THEN DO:

            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (THIS-OBJECT:CallParameterObject).

            THIS-OBJECT:CallParameterObject = NEW ProcedureSchedulerJobCommand ().

        END.

        ASSIGN
            oSchedulerJobCommand                    = CAST (THIS-OBJECT:CallParameterObject, ProcedureSchedulerJobCommand).

        ASSIGN
            oSchedulerJobCommand:ProcedureFileName  = THIS-OBJECT:ultraTextEditor1:Text.

        oSchedulerJobCommand:ProcedureParameter:Clear().

        FOR EACH tParameter BY tParameter.SortOrder:
            oSchedulerJobCommand:ProcedureParameter:Add(tParameter.SortOrder ,tParameter.ParamType, tParameter.ParamValue).
        END.

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.

    END METHOD .

END CLASS.
