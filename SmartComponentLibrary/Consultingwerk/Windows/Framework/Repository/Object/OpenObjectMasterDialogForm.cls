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
    File        : OpenObjectMasterDialogForm
    Purpose     : Selection Form for Object Master
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu May 26 15:26:03 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*             FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*   FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Object.* FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .
USING Progress.Util.*                                   FROM ASSEMBLY .
USING System.Windows.Forms.*                            FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Framework.Repository.Object.OpenObjectMasterDialogForm
    INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE btnOK AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE lookupObjectType AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE lookupModule AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDataBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE timer1 AS System.Windows.Forms.Timer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    DEFINE VARIABLE oFullWindowDragManager AS Consultingwerk.Windows.Util.Forms.FullWindowDragManager NO-UNDO EXTENT 2 .

    /**
     * Purpose: Returns the Guid of the selected object master
     * Notes:
     */
    DEFINE PUBLIC PROPERTY SelectedObjectMasterGuid AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the OpenObjectMasterDialogForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC OpenObjectMasterDialogForm ():

        SUPER().

        InitializeComponent().

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /**
     * Purpose: Event handler for the CollectFilterValues event of the smartBusinessEntityAdpater1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The
     */
    METHOD PROTECTED VOID CollectFilterValuesHandler (sender AS Progress.Lang.Object,
                                                      e AS CollectFilterValuesEventArgs):

        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.

        ASSIGN cValue = ultraTextEditor1:Text .

        IF cValue > "":U THEN
            e:QueryExpressions:GetItem ("eSmartObjectMaster":U):Add ("eSmartObjectMaster.ObjectName":U,
                                                                     Consultingwerk.OperatorEnum:Begins,
                                                                     cValue) .

        ASSIGN cValue = lookupObjectType:LookupKeyValue .

        IF cValue > "":U THEN
            e:QueryExpressions:GetItem ("eSmartObjectMaster":U):Add ("eSmartObjectMaster.ObjectTypeGuid":U,
                                                                     Consultingwerk.OperatorEnum:EQ,
                                                                     cValue) .

        ASSIGN cValue = lookupModule:LookupKeyValue .

        IF cValue > "":U THEN
            e:QueryExpressions:GetItem ("eSmartObjectMaster":U):Add ("eSmartObjectMaster.ModuleGuid":U,
                                                                     Consultingwerk.OperatorEnum:EQ,
                                                                     cValue) .
    END METHOD.

    /**
     * Purpose: Event handler for the Click event of the btnOk
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID btnOK_Click (sender AS System.Object,
                                     e AS System.EventArgs):

        IF smartBusinessEntityAdapter1:Available THEN DO:

            THIS-OBJECT:SelectedObjectMasterGuid = smartBusinessEntityAdapter1:GetFieldValues ("ObjectMasterGuid":U) .
        END.

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
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartObjectMaster":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Object.OpenObjectMasterDialogForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("eSmartObjectMaster":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ObjectMasterGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ObjectName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("CustomizationResultGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("CustomizationResultCode":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ObjectTypeGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn6 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn6 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ObjectTypeName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn7 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn7 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ModuleGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn8 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn8 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ModuleName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn9 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn9 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ObjectDescription":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn10 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn10 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ObjectPackage":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn11 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn11 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ObjectExtension":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn12 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn12 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("StaticObject":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn13 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn13 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TemplateObject":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn14 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn14 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("DeploymentType":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn15 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn15 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("RunnableFromMenu":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn16 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn16 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("Disabled":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn17 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn17 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("DesignTimeDataSourceGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn18 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn18 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("DesignTimeDataSourceName":U).
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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        THIS-OBJECT:smartBusinessEntityAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter(THIS-OBJECT:components).
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:lookupModule = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:smartDataBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:lookupObjectType = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:btnOK = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:timer1 = NEW System.Windows.Forms.Timer(THIS-OBJECT:components).
        CAST(THIS-OBJECT:smartBusinessEntityAdapter1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:lookupModule, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:lookupObjectType, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityAdapter1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityAdapter1:EntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:EntityTable = "eSmartObjectMaster":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkCommitSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkNavigationSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter1:AfterRetrieveData:Subscribe(THIS-OBJECT:smartBusinessEntityAdapter1_AfterRetrieveData).
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartObjectMaster":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 18 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("ObjectMasterGuid":U, "ObjectMasterGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("ObjectName":U, "Object Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("CustomizationResultGuid":U, "Customization Result":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("CustomizationResultCode":U, "Customization Result Code":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("ObjectTypeGuid":U, "ObjectTypeGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("ObjectTypeName":U, "Object Type Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("ModuleGuid":U, "ModuleGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("ModuleName":U, "Module Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("ObjectDescription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("ObjectPackage":U, "Object Package":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[11] = NEW Progress.Data.ColumnPropDesc("ObjectExtension":U, "Extension":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[12] = NEW Progress.Data.ColumnPropDesc("StaticObject":U, "Static Object":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[13] = NEW Progress.Data.ColumnPropDesc("TemplateObject":U, "Template Object":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[14] = NEW Progress.Data.ColumnPropDesc("DeploymentType":U, "Deployment Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[15] = NEW Progress.Data.ColumnPropDesc("RunnableFromMenu":U, "Runnable from Menu":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[16] = NEW Progress.Data.ColumnPropDesc("Disabled":U, "Disabled":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[17] = NEW Progress.Data.ColumnPropDesc("DesignTimeDataSourceGuid":U, "Design Time Data Source":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[18] = NEW Progress.Data.ColumnPropDesc("DesignTimeDataSourceName":U, "Design Time Data Source Name":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* panel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:panel1, "panel1":U).
        THIS-OBJECT:panel1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:lookupModule).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:smartDataBrowser1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:lookupObjectType).
        THIS-OBJECT:panel1:Name = "panel1":U.
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
        /* ultraTextEditor1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTextEditor1, "ultraTextEditor1":U).
        THIS-OBJECT:ultraTextEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:ValueChanged:Subscribe(THIS-OBJECT:ultraTextEditor1_ValueChanged).
        THIS-OBJECT:ultraTextEditor1:KeyDown:Subscribe(THIS-OBJECT:ultraTextEditor1_KeyDown).
        /*  */
        /* lookupModule */
        /*  */
        resources:ApplyResources(THIS-OBJECT:lookupModule, "lookupModule":U).
        editorButton1:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:lookupModule:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:lookupModule:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:lookupModule:LookupBrowserFields = "ModuleName,ModuleDescription,ProductCode":U.
        THIS-OBJECT:lookupModule:LookupDialogFilterFields = "ModuleName,ModuleDescription":U.
        THIS-OBJECT:lookupModule:LookupDialogFilterOperators = "BEGINSMATCHES,BEGINSMATCHES":U.
        THIS-OBJECT:lookupModule:LookupDialogQuerySort = "BY eSmartModule.ModuleName":U.
        THIS-OBJECT:lookupModule:LookupEntityName = "Consultingwerk.SmartFramework.Menu.ModuleBusinessEntity":U.
        THIS-OBJECT:lookupModule:LookupEntityTable = "eSmartModule":U.
        THIS-OBJECT:lookupModule:LookupKeyField = "ModuleName":U.
        THIS-OBJECT:lookupModule:LookupKeyValueColumn = "ModuleGuid":U.
        THIS-OBJECT:lookupModule:LookupQueryString = "FOR EACH eSmartModule WHERE eSmartModule.ModuleName BEGINS ~"&1~"":U.
        THIS-OBJECT:lookupModule:Name = "lookupModule":U.
        THIS-OBJECT:lookupModule:OwningGrid = ?.
        THIS-OBJECT:lookupModule:ValueChanged:Subscribe(THIS-OBJECT:ultraTextEditor1_ValueChanged).
        THIS-OBJECT:lookupModule:KeyDown:Subscribe(THIS-OBJECT:ultraTextEditor1_KeyDown).
        /*  */
        /* smartDataBrowser1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartDataBrowser1, "smartDataBrowser1":U).
        THIS-OBJECT:smartDataBrowser1:DataSource = THIS-OBJECT:smartBusinessEntityBindingSource1.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Appearance = appearance1.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn1:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        resources:ApplyResources(ultraGridColumn1:Header, "ultraGridColumn1.Header":U).
        ultraGridColumn1:Header:VisiblePosition = 0.
        ultraGridColumn1:Hidden = TRUE.
        ultraGridColumn1:ForceApplyResources = "Header":U.
        ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn2:Header, "ultraGridColumn2.Header":U).
        ultraGridColumn2:Header:VisiblePosition = 1.
        ultraGridColumn2:ForceApplyResources = "Header":U.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn3:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        resources:ApplyResources(ultraGridColumn3:Header, "ultraGridColumn3.Header":U).
        ultraGridColumn3:Header:VisiblePosition = 2.
        ultraGridColumn3:Hidden = TRUE.
        ultraGridColumn3:ForceApplyResources = "Header":U.
        ultraGridColumn4:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn4:Header, "ultraGridColumn4.Header":U).
        ultraGridColumn4:Header:VisiblePosition = 3.
        ultraGridColumn4:ForceApplyResources = "Header":U.
        ultraGridColumn5:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn5:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        resources:ApplyResources(ultraGridColumn5:Header, "ultraGridColumn5.Header":U).
        ultraGridColumn5:Header:VisiblePosition = 4.
        ultraGridColumn5:Hidden = TRUE.
        ultraGridColumn5:ForceApplyResources = "Header":U.
        ultraGridColumn6:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn6:Header, "ultraGridColumn6.Header":U).
        ultraGridColumn6:Header:VisiblePosition = 5.
        ultraGridColumn6:ForceApplyResources = "Header":U.
        ultraGridColumn7:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn7:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        resources:ApplyResources(ultraGridColumn7:Header, "ultraGridColumn7.Header":U).
        ultraGridColumn7:Header:VisiblePosition = 6.
        ultraGridColumn7:Hidden = TRUE.
        ultraGridColumn7:ForceApplyResources = "Header":U.
        ultraGridColumn8:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn8:Header, "ultraGridColumn8.Header":U).
        ultraGridColumn8:Header:VisiblePosition = 7.
        ultraGridColumn8:ForceApplyResources = "Header":U.
        ultraGridColumn9:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn9:Header, "ultraGridColumn9.Header":U).
        ultraGridColumn9:Header:VisiblePosition = 8.
        ultraGridColumn9:ForceApplyResources = "Header":U.
        ultraGridColumn10:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn10:Header, "ultraGridColumn10.Header":U).
        ultraGridColumn10:Header:VisiblePosition = 9.
        ultraGridColumn10:ForceApplyResources = "Header":U.
        ultraGridColumn11:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn11:Header, "ultraGridColumn11.Header":U).
        ultraGridColumn11:Header:VisiblePosition = 10.
        ultraGridColumn11:Hidden = TRUE.
        ultraGridColumn11:ForceApplyResources = "Header":U.
        ultraGridColumn12:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn12:Header, "ultraGridColumn12.Header":U).
        ultraGridColumn12:Header:VisiblePosition = 11.
        ultraGridColumn12:Hidden = TRUE.
        ultraGridColumn12:ForceApplyResources = "Header":U.
        ultraGridColumn13:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn13:Header, "ultraGridColumn13.Header":U).
        ultraGridColumn13:Header:VisiblePosition = 12.
        ultraGridColumn13:Hidden = TRUE.
        ultraGridColumn13:ForceApplyResources = "Header":U.
        ultraGridColumn14:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn14:Header, "ultraGridColumn14.Header":U).
        ultraGridColumn14:Header:VisiblePosition = 13.
        ultraGridColumn14:Hidden = TRUE.
        ultraGridColumn14:ForceApplyResources = "Header":U.
        ultraGridColumn15:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn15:Header, "ultraGridColumn15.Header":U).
        ultraGridColumn15:Header:VisiblePosition = 14.
        ultraGridColumn15:Hidden = TRUE.
        ultraGridColumn15:ForceApplyResources = "Header":U.
        ultraGridColumn16:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn16:Header, "ultraGridColumn16.Header":U).
        ultraGridColumn16:Header:VisiblePosition = 15.
        ultraGridColumn16:ForceApplyResources = "Header":U.
        ultraGridColumn17:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn17:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        resources:ApplyResources(ultraGridColumn17:Header, "ultraGridColumn17.Header":U).
        ultraGridColumn17:Header:VisiblePosition = 16.
        ultraGridColumn17:Hidden = TRUE.
        ultraGridColumn17:ForceApplyResources = "Header":U.
        ultraGridColumn18:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn18:Header, "ultraGridColumn18.Header":U).
        ultraGridColumn18:Header:VisiblePosition = 17.
        ultraGridColumn18:ForceApplyResources = "Header":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS System.Object EXTENT 18 NO-UNDO.
        arrayvar1[1] = ultraGridColumn1.
        arrayvar1[2] = ultraGridColumn2.
        arrayvar1[3] = ultraGridColumn3.
        arrayvar1[4] = ultraGridColumn4.
        arrayvar1[5] = ultraGridColumn5.
        arrayvar1[6] = ultraGridColumn6.
        arrayvar1[7] = ultraGridColumn7.
        arrayvar1[8] = ultraGridColumn8.
        arrayvar1[9] = ultraGridColumn9.
        arrayvar1[10] = ultraGridColumn10.
        arrayvar1[11] = ultraGridColumn11.
        arrayvar1[12] = ultraGridColumn12.
        arrayvar1[13] = ultraGridColumn13.
        arrayvar1[14] = ultraGridColumn14.
        arrayvar1[15] = ultraGridColumn15.
        arrayvar1[16] = ultraGridColumn16.
        arrayvar1[17] = ultraGridColumn17.
        arrayvar1[18] = ultraGridColumn18.
        ultraGridBand1:Columns:AddRange(arrayvar1).
        ultraGridBand1:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand1:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance2:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:Appearance = appearance2.
        appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance4:BackColor2 = System.Drawing.SystemColors:Control.
        appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance5:BackColor = System.Drawing.SystemColors:Window.
        appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:ActiveCellAppearance = appearance5.
        appearance6:BackColor = System.Drawing.SystemColors:Highlight.
        appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:ActiveRowAppearance = appearance6.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance7:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CardAreaAppearance = appearance7.
        appearance8:BorderColor = System.Drawing.Color:Silver.
        appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellAppearance = appearance8.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellPadding = 0.
        appearance9:BackColor = System.Drawing.SystemColors:Control.
        appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance9:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:GroupByRowAppearance = appearance9.
        resources:ApplyResources(appearance10, "appearance10":U).
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderAppearance = appearance10.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance11:BackColor = System.Drawing.SystemColors:Window.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:RowAppearance = appearance11.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:smartDataBrowser1:FetchAllRecordsOnGroupBy = TRUE.
        THIS-OBJECT:smartDataBrowser1:LinkDataSource = THIS-OBJECT:smartBusinessEntityAdapter1.
        THIS-OBJECT:smartDataBrowser1:Name = "smartDataBrowser1":U.
        THIS-OBJECT:smartDataBrowser1:SettingsKey = "f60fe501-c4e7-57ba-1014-623ef0a79cd9":U.
        THIS-OBJECT:smartDataBrowser1:DefaultAction:Subscribe(THIS-OBJECT:smartDataBrowser1_DefaultAction).
        /*  */
        /* lookupObjectType */
        /*  */
        resources:ApplyResources(THIS-OBJECT:lookupObjectType, "lookupObjectType":U).
        editorButton2:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton2, "editorButton2":U).
        THIS-OBJECT:lookupObjectType:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:lookupObjectType:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:lookupObjectType:LookupBrowserFields = "ObjectTypeName,ObjectTypeDescription,ExtendsObjectTypeName,ContainerType,StaticObject,TechnicalClassName":U.
        THIS-OBJECT:lookupObjectType:LookupControls = "eSmartObjectMaster_StaticObject":U.
        THIS-OBJECT:lookupObjectType:LookupDialogFilterFields = "ObjectTypeName,ObjectTypeDescription":U.
        THIS-OBJECT:lookupObjectType:LookupDialogFilterOperators = "BEGINSMATCHES,BEGINSMATCHES":U.
        THIS-OBJECT:lookupObjectType:LookupDialogQuerySort = "BY eSmartObjectType.ObjectTypeName":U.
        THIS-OBJECT:lookupObjectType:LookupEntityName = "Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity":U.
        THIS-OBJECT:lookupObjectType:LookupEntityTable = "eSmartObjectType":U.
        THIS-OBJECT:lookupObjectType:LookupFields = "StaticObject":U.
        THIS-OBJECT:lookupObjectType:LookupKeyField = "ObjectTypeName":U.
        THIS-OBJECT:lookupObjectType:LookupKeyValueColumn = "ObjectTypeGuid":U.
        THIS-OBJECT:lookupObjectType:LookupQueryString = "FOR EACH eSmartObjectType WHERE eSmartObjectType.ObjectTypeName BEGINS ~"&1~"":U.
        THIS-OBJECT:lookupObjectType:Name = "lookupObjectType":U.
        THIS-OBJECT:lookupObjectType:OwningGrid = ?.
        THIS-OBJECT:lookupObjectType:ValueChanged:Subscribe(THIS-OBJECT:ultraTextEditor1_ValueChanged).
        THIS-OBJECT:lookupObjectType:KeyDown:Subscribe(THIS-OBJECT:ultraTextEditor1_KeyDown).
        /*  */
        /* btnOK */
        /*  */
        resources:ApplyResources(THIS-OBJECT:btnOK, "btnOK":U).
        THIS-OBJECT:btnOK:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:btnOK:Name = "btnOK":U.
        THIS-OBJECT:btnOK:Click:Subscribe(THIS-OBJECT:btnOK_Click).
        /*  */
        /* btnCancel */
        /*  */
        resources:ApplyResources(THIS-OBJECT:btnCancel, "btnCancel":U).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        /*  */
        /* timer1 */
        /*  */
        THIS-OBJECT:timer1:Interval = 750.
        THIS-OBJECT:timer1:Tick:Subscribe(THIS-OBJECT:timer1_Tick).
        /*  */
        /* OpenObjectMasterDialogForm */
        /*  */
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnCancel.
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnOK).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Name = "OpenObjectMasterDialogForm":U.
        CAST(THIS-OBJECT:smartBusinessEntityAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:lookupModule, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:lookupObjectType, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        SUPER:OnLoad (e) .

        oFullWindowDragManager[1] = NEW Consultingwerk.Windows.Util.Forms.FullWindowDragManager (panel1) .
        oFullWindowDragManager[2] = NEW Consultingwerk.Windows.Util.Forms.FullWindowDragManager (THIS-OBJECT) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown (e) .

        UserInterfaceHelper:ProcessEvents() .

        smartBusinessEntityAdapter1:RetrieveData() .

        smartBusinessEntityAdapter1:CollectFilterValues:Subscribe (CollectFilterValuesHandler) .

    END METHOD.

    /**
     * Purpose: Event handler for the AfterRetrieveData event of the smartBusinessEntityAdapter1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID smartBusinessEntityAdapter1_AfterRetrieveData (sender AS System.Object,
                                                                       e AS System.EventArgs):

        btnOK:Enabled = smartBusinessEntityAdapter1:Available .

    END METHOD.

    /**
     * Purpose: Event handle for the DefaultAction event of the smartDataBrowser1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID smartDataBrowser1_DefaultAction (sender AS System.Object,
                                                         e AS System.EventArgs):

        IF smartBusinessEntityAdapter1:Available THEN DO:

            THIS-OBJECT:SelectedObjectMasterGuid = smartBusinessEntityAdapter1:GetFieldValues ("ObjectMasterGuid":U) .
            THIS-OBJECT:Close () .
        END.

    END METHOD.

    /**
     * Purpose: Event handler for the Tick event of the timer1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID timer1_Tick (sender AS System.Object,
                                     e AS System.EventArgs):

        timer1:Enabled = FALSE .

        smartBusinessEntityAdapter1:RetrieveData() .

    END METHOD .

    /*------------------------------------------------------------------------------
       Purpose: Event handler for the KeyDown event of the Object Name Text Editor
                adn the Object Type and Module lookups
       Notes:
       @param sender The reference to the object that raised the event
       @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTextEditor1_KeyDown (sender AS System.Object,
                                                  e AS System.Windows.Forms.KeyEventArgs):

        IF EnumHelper:AreEqual (e:KeyCode, Keys:Down) THEN DO:

            smartDataBrowser1:Focus ().

        END.

    END METHOD.

    /**
     * Purpose: Event handler for the ValueChanged event of the ultraTextEditor1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID ultraTextEditor1_ValueChanged (sender AS System.Object,
                                                       e AS System.EventArgs):

        timer1:Enabled = TRUE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the OpenObjectMasterDialogForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC OpenObjectMasterDialogForm ( ):

        GarbageCollectorHelper:DeleteObject (oFullWindowDragManager[1]) .
        GarbageCollectorHelper:DeleteObject (oFullWindowDragManager[2]) .

    END DESTRUCTOR.

END CLASS.
