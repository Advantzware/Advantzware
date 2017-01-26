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
    File        : BusinessEntityDesignerForm
    Purpose     : Main Form of the Consultingwerk BusinessEntity Designer
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 13 12:07:58 CEST 2010
    Notes       : Built using Infragistics Controls and Crainiate ERM
                  Controls
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                    FROM PROPATH .
USING Consultingwerk.Assertion.*                          FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.*    FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.*             FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.*          FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.Shapes.*   FROM PROPATH .
USING Consultingwerk.Framework.*                          FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                     FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*               FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*               FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*     FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*         FROM PROPATH .
USING Consultingwerk.Studio.ReleaseNotes.*                FROM PROPATH.
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Consultingwerk.Util.Forms.*                         FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*                 FROM PROPATH .
USING Consultingwerk.Windows.API.*                        FROM PROPATH .
USING Infragistics.Win.UltraWinDock.*                     FROM ASSEMBLY .
USING Infragistics.Win.UltraWinExplorerBar.*              FROM ASSEMBLY .
USING Infragistics.Win.UltraWinListView.*                 FROM ASSEMBLY .
USING Infragistics.Win.UltraWinToolbars.*                 FROM ASSEMBLY .
USING Progress.Lang.*                                     FROM ASSEMBLY .
USING System.Drawing.*                                    FROM ASSEMBLY .
USING System.Windows.Forms.*                              FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityDesignerForm
    INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE businessEntityViewerControl1 AS Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityViewerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE databaseFieldsControl1 AS Consultingwerk.BusinessEntityDesigner.UI.DatabaseFieldsControl NO-UNDO.
    DEFINE PRIVATE VARIABLE dataRelationAdapter AS Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE dataRelationViewerControl1 AS Consultingwerk.BusinessEntityDesigner.UI.DataRelationViewerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE dockableWindow1 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE dockableWindow10 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE dockableWindow3 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE dockableWindow2 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE dockableWindow4 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE dockableWindow5 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE dockableWindow8 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE dockableWindow9 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE fieldViewerControl1 AS Consultingwerk.BusinessEntityDesigner.UI.FieldViewerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE dockableWindow7 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE dockableWindow6 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE indexAdapter AS Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE fieldAdapter AS Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE indexViewerControl1 AS Consultingwerk.BusinessEntityDesigner.UI.IndexViewerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE logManagerSettings1 AS Consultingwerk.Windows.Util.Controls.LogManagerSettings NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityDesignerFormUnpinnedTabAreaTop AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityDesignerFormUnpinnedTabAreaRight AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityDesignerFormUnpinnedTabAreaLeft AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityDesignerFormUnpinnedTabAreaBottom AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityDesignerFormAutoHideControl AS Infragistics.Win.UltraWinDock.AutoHideControl NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE memberTableViewerControl1 AS Consultingwerk.BusinessEntityDesigner.UI.MemberTableViewerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE model1 AS Crainiate.ERM4.Model NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog2 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE saveFileDialog1 AS System.Windows.Forms.SaveFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE searchTimer AS System.Windows.Forms.Timer NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityAdapter2 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.
    DEFINE PRIVATE VARIABLE tableAdapter AS Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE timer1 AS System.Windows.Forms.Timer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraDockManager1 AS Infragistics.Win.UltraWinDock.UltraDockManager NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraExplorerBar1 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBar NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPrintPreviewDialog1 AS Infragistics.Win.Printing.UltraPrintPreviewDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTrackBar1 AS Infragistics.Win.UltraWinEditors.UltraTrackBar NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraStatusBar1 AS Infragistics.Win.UltraWinStatusBar.UltraStatusBar NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTree1 AS Infragistics.Win.UltraWinTree.UltraTree NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea1 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea10 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea11 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea13 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea4 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea3 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea2 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea5 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea6 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea7 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.

    DEFINE PRIVATE VARIABLE oNewConnector AS Crainiate.ERM4.Connector NO-UNDO .
    DEFINE VARIABLE cBaseTitle AS CHARACTER NO-UNDO.

    DEFINE VARIABLE oLRUList AS "System.Collections.Generic.List<System.String>":U NO-UNDO .
    DEFINE VARIABLE lRemovingElements AS LOGICAL NO-UNDO INIT FALSE .
    DEFINE PRIVATE VARIABLE windowDockingArea9 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea8 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.

    DEFINE VARIABLE oSplash AS Consultingwerk.SmartComponents.Support.Splash.Splash NO-UNDO .
    DEFINE VARIABLE oTimer  AS System.Windows.Forms.Timer NO-UNDO .

    DEFINE VARIABLE oController AS BusinessEntityDatasetController NO-UNDO .

    DEFINE VARIABLE oBusinessEntitySourceEditorForm AS BusinessEntitySourceEditorForm NO-UNDO .

    DEFINE VARIABLE oLogControl AS BusinessEntityGeneratorLogControl NO-UNDO .
    DEFINE VARIABLE lUseLocalFileHistory AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lApplyingDisplayReleaseNotesAtStart AS LOGICAL NO-UNDO INIT FALSE .

    DEFINE PROTECTED TEMP-TABLE ttPlugins NO-UNDO
       FIELD cPluginName AS CHARACTER
       FIELD oPlugin     AS Progress.Lang.Object
       INDEX cPluginName IS UNIQUE PRIMARY cPluginName .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets file name of the current bedgm file
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO
    GET.
    SET (arg AS CHARACTER):

        UpdateFileLRU (arg) .

        THIS-OBJECT:FileName = arg .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessEntityDesignerForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessEntityDesignerForm ():

        DEFINE VARIABLE oStatusManager AS IStatusManager NO-UNDO .

        SUPER().

        InitializeComponent().

        THIS-OBJECT:AddToolForReleaseNotesSetting () .

        &IF NOT PROVERSION BEGINS "10.":U &THEN
        smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010 .
        smartToolbarController1:Ribbon:ApplicationMenuButtonImage = ? .
        &ENDIF

        /* Mike Fechner, Consultingwerk Ltd. 20.02.2014
           Register the Business Entity Desginer Form and the
           BusinessEntityDatasetController in the ServiceContainer
           so that it's accessible from plugins or custom services
           loaded in the Business Entity Designer environment */
        THIS-OBJECT:RegisterServices () .

        oStatusManager = {Consultingwerk/get-service.i Consultingwerk.Framework.IStatusManager
                                                       "NEW Consultingwerk.Framework.StatusManager()"} .

        oStatusManager:GenericStatusChanded:Subscribe (GenericStatusChandedHandler) .

        /* Mike Fechner, Consultingwerk Ltd. 31.10.2011
           Top level Form does not need to be deleted when closed */
        THIS-OBJECT:DeleteOnFormClosed = FALSE  .

        oSplash = NEW Consultingwerk.SmartComponents.Support.Splash.Splash () .
        oSplash:ApplicationTitle = "Business Entity Designer":U .
        oSplash:TopMost = TRUE .
        oSplash:Show () .
        UserInterfaceHelper:ProcessEvents () .

        CAST (smartBusinessEntityAdapter2:DatasetController,
              DatabaseSchemaDatasetController):DatabaseFieldsControl = databaseFieldsControl1 .

        oController = CAST (smartBusinessEntityAdapter1:DatasetController,
                            BusinessEntityDatasetController) .

        oController:DrawShapes (model1) .
        oController:Model = THIS-OBJECT:model1.
        oController:DockManager = ultraDockManager1 .
        oController:BusinessEntityViewerControl = businessEntityViewerControl1 .
        oController:DataRelationViewerControl = dataRelationViewerControl1 .
        oController:FieldViewerControl = fieldViewerControl1 .
        oController:IndexViewerControl = indexViewerControl1 .
        oController:TableViewerControl = memberTableViewerControl1 .
        oController:TreeView = ultraTree1 .

        oController:UnsavedChangesChanged:Subscribe (UnsavedChangesChangedHandler) .
        oController:TableItemSelected:Subscribe (TableItemSelectedHandler) .

        oLRUList = NEW "System.Collections.Generic.List<System.String>":U () .
        LoadLRU () .

        ASSIGN cBaseTitle = THIS-OBJECT:Text .

        THIS-OBJECT:NewDesign () .

        THIS-OBJECT:ultraStatusBar1:Panels["proversion":U]:Text       = SessionHelper:FullVersionString() .
        THIS-OBJECT:ultraStatusBar1:Panels["frameworkversion":U]:Text = (NEW FrameworkVersion()):GetVersionString ().
        THIS-OBJECT:ultraStatusBar1:Panels["bitness":U]:Text          = SUBSTITUTE ("&1 bit":U, SessionHelper:ProcessBitness()) .

        IF PROVERSION BEGINS "10":U THEN
            smartToolbarController1:Tools["Database Login Preferences":U]:SharedProps:Enabled = FALSE .

        CATCH e AS Progress.Lang.Error:
            IF VALID-OBJECT (oSplash) THEN
                oSplash:Close () .

            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Activates the Design Canvas
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ActivateDesignCanvas ():

        ultraDockManager1:PaneFromControl(panel1):Activate () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Activates the Add connector mode (allows dragging of new relations)
                 on the model
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ActivateDrawConnector ():

        THIS-OBJECT:ActivateDesignCanvas () .

        THIS-OBJECT:model1:Runtime:InteractiveMode = Crainiate.ERM4.InteractiveMode:AddConnector .

        CAST (THIS-OBJECT:smartToolbarController1:Tools["DrawConnector":U],
              Infragistics.Win.UltraWinToolbars.StateButtonTool):CHECKED = TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Activates the pointer mode (allows selection of Shapes) on the model
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ActivateDrawPointer ():

        THIS-OBJECT:ActivateDesignCanvas () .

        THIS-OBJECT:model1:Runtime:InteractiveMode = Crainiate.ERM4.InteractiveMode:Normal .

        CAST (THIS-OBJECT:smartToolbarController1:Tools["DrawPointer":U],
              Infragistics.Win.UltraWinToolbars.StateButtonTool):CHECKED = TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Activates the (context) menu items based on the current selected
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ActivateMenuItems ():

        DEFINE VARIABLE oTables    AS Crainiate.ERM4.Elements NO-UNDO .
        DEFINE VARIABLE oRelations AS Crainiate.ERM4.Elements NO-UNDO .

        DEFINE VARIABLE oEnumerator AS System.Collections.IEnumerator NO-UNDO .
        DEFINE VARIABLE oTable      AS Crainiate.ERM4.Table           NO-UNDO .

        DEFINE VARIABLE cTag        AS CHARACTER                      NO-UNDO .

        ASSIGN oTables    = model1:SelectedElements(Progress.Util.TypeHelper:GetType ("Crainiate.ERM4.Table":U))
               oRelations = model1:SelectedElements(Progress.Util.TypeHelper:GetType ("Crainiate.ERM4.Connector":U))
               .

        smartToolbarController1:Tools["Custom Business Entity Properties":U]:SharedProps:Enabled = TRUE .

        IF oTables:Count > 0 THEN DO:
            smartToolbarController1:Tools["RefreshTable":U]:SharedProps:Enabled = TRUE .
            smartToolbarController1:Tools["RefreshTable2":U]:SharedProps:Enabled = TRUE .
            smartToolbarController1:Tools["FieldsReorder":U]:SharedProps:Enabled = TRUE .
            smartToolbarController1:Tools["Duplicate Table":U]:SharedProps:Enabled = TRUE .
            smartToolbarController1:Tools["Edit Fields":U]:SharedProps:Enabled = TRUE .

            smartToolbarController1:Tools["Custom Table Properties":U]:SharedProps:Enabled = TRUE .

            smartToolbarController1:Tools["Copy Field Names":U]:SharedProps:Enabled = TRUE .

            oEnumerator = oTables:GetEnumerator () .
            oEnumerator:Reset() .
            oEnumerator:MoveNext () .

            oTable = CAST (CAST (oEnumerator:Current, System.Collections.DictionaryEntry):Value, Crainiate.ERM4.Table) .

            IF VALID-OBJECT (oTable:SelectedItem) THEN
                ASSIGN cTag = UNBOX (oTable:SelectedItem:Tag).

            IF cTag BEGINS "eField,":U THEN DO:
                ASSIGN
                    smartToolbarController1:Tools["Custom Field Properties":U]:SharedProps:Enabled = TRUE
                    smartToolbarController1:Tools["Custom Index Properties":U]:SharedProps:Enabled = FALSE

                    smartToolbarController1:Tools["Split Extend Field into separate Fields":U]:SharedProps:Enabled = oController:IsExtentField (cTag) .
            END.
            ELSE IF cTag BEGINS "eIndex,":U THEN
                ASSIGN
                    smartToolbarController1:Tools["Custom Field Properties":U]:SharedProps:Enabled = FALSE
                    smartToolbarController1:Tools["Custom Index Properties":U]:SharedProps:Enabled = TRUE

                    smartToolbarController1:Tools["Split Extend Field into separate Fields":U]:SharedProps:Enabled = FALSE .
            ELSE
                ASSIGN
                    smartToolbarController1:Tools["Custom Field Properties":U]:SharedProps:Enabled = FALSE
                    smartToolbarController1:Tools["Custom Index Properties":U]:SharedProps:Enabled = FALSE .
        END .
        ELSE DO:
            smartToolbarController1:Tools["RefreshTable":U]:SharedProps:Enabled = FALSE .
            smartToolbarController1:Tools["RefreshTable2":U]:SharedProps:Enabled = FALSE .
            smartToolbarController1:Tools["FieldsReorder":U]:SharedProps:Enabled = FALSE .
            smartToolbarController1:Tools["Duplicate Table":U]:SharedProps:Enabled = FALSE .
            smartToolbarController1:Tools["Edit Fields":U]:SharedProps:Enabled = FALSE .

            smartToolbarController1:Tools["Copy Field Names":U]:SharedProps:Enabled = FALSE .

            smartToolbarController1:Tools["Custom Table Properties":U]:SharedProps:Enabled = FALSE .
            smartToolbarController1:Tools["Custom Field Properties":U]:SharedProps:Enabled = FALSE .
            smartToolbarController1:Tools["Custom Index Properties":U]:SharedProps:Enabled = FALSE .
            smartToolbarController1:Tools["Split Extend Field into separate Fields":U]:SharedProps:Enabled = FALSE .
        END.

        IF oRelations:Count > 0 THEN DO:
            smartToolbarController1:Tools["Relation Fields":U]:SharedProps:Enabled = TRUE .
            smartToolbarController1:Tools["Remove Relation":U]:SharedProps:Enabled = TRUE .

            smartToolbarController1:Tools["Custom Relation Properties":U]:SharedProps:Enabled = TRUE .
        END.
        ELSE DO:
            smartToolbarController1:Tools["Relation Fields":U]:SharedProps:Enabled = FALSE .
            smartToolbarController1:Tools["Remove Relation":U]:SharedProps:Enabled = FALSE .

            smartToolbarController1:Tools["Custom Relation Properties":U]:SharedProps:Enabled = FALSE .
        END.

    END METHOD.

    /**
     * Purpose: Adds a Tool to the Business Entity Designer Ribbon application
     *          menu that allows a user to toggle the display of the release
     *          notes
     * Notes:
     */
    METHOD PROTECTED VOID AddToolForReleaseNotesSetting():

        DEFINE VARIABLE oStateButtonTool AS StateButtonTool  NO-UNDO .
        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .
        DEFINE VARIABLE lSetting         AS LOGICAL          NO-UNDO .

        oStateButtonTool = NEW StateButtonTool ("Display Release Notes At Start":U) .
        oStateButtonTool:SharedProps:Caption = "Display Release Notes At Start"{&TRAN} .
        oStateButtonTool:SharedProps:AppearancesSmall:Appearance:Image = ImageHelper:Load ("Consultingwerk/BusinessEntityDesigner/UI/Images/note_pinned_16.png":U) .
        oStateButtonTool:SharedProps:AppearancesLarge:Appearance:Image = ImageHelper:Load ("Consultingwerk/BusinessEntityDesigner/UI/Images/note_pinned_32.png":U) .

        smartToolbarController1:Tools:Add (oStateButtonTool) .
        smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:Tools:AddTool ("Display Release Notes At Start":U)
                    :InstanceProps:IsFirstInGroup = TRUE .


        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        IF VALID-OBJECT (oSettingsService) THEN DO:
            lSetting = DataTypeHelper:ToLogical(oSettingsService:GetSetting (THIS-OBJECT:WindowPositionRegistryKey,
                                                                             "Display Release Notes At Start":U)) .

            IF lSetting = ? THEN
                ASSIGN lSetting = TRUE .

            ASSIGN lApplyingDisplayReleaseNotesAtStart = TRUE .

            oStateButtonTool:Checked = lSetting .
        END.

        FINALLY:
            ASSIGN lApplyingDisplayReleaseNotesAtStart = FALSE .
        END FINALLY.

    END METHOD.


    /*------------------------------------------------------------------------------
        Purpose: Performs an Auto Layout of the Current Diagram
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AutoLayout ():

        DEFINE VARIABLE oGraph  AS Crainiate.ERM4.Layouts.Graph              NO-UNDO .
        DEFINE VARIABLE oLayout AS Crainiate.ERM4.Layouts.OrthogonalLayout  NO-UNDO .

        THIS-OBJECT:ActivateDesignCanvas () .

        oGraph = NEW Crainiate.ERM4.Layouts.Graph () .

        /* Set up the nodes and edges for the graph from the diagram */
        oGraph:AddDiagram (model1) .
        oGraph:Margin = NEW Crainiate.ERM4.Margin (0, 0, 0, 0) .

        /* Set the up the parameters for the layout and perform the layout. */
        oLayout = NEW Crainiate.ERM4.Layouts.OrthogonalLayout   () .
        oLayout:ConnectedComponentDistance = 200 .
        oLayout:Distance = 300 .
        oLayout:DoLayout (oGraph) .

        oGraph:Apply (model1) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose:
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID BuildLRUMenuItems ():

        DEFINE VARIABLE i     AS INTEGER                                      NO-UNDO.
        DEFINE VARIABLE oTool AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO .

        DO i = 1 TO 10:
            IF smartToolbarController1:Tools:Exists ("LRUEntry":U + STRING (i, "99":U)) THEN
                smartToolbarController1:Tools:Remove(smartToolbarController1:Tools["LRUEntry":U + STRING (i, "99":U)]) .
        END.

        DO i = 0 TO oLRUList:Count - 1:
            IF i >= 10 THEN
                RETURN .

            oTool = NEW Infragistics.Win.UltraWinToolbars.ButtonTool ("LRUEntry":U + STRING (i + 1, "99":U)) .
            oTool:SharedProps:Caption = STRING (oLRUList:Item[i]) .
            smartToolbarController1:Tools:Add (oTool) .

            CAST (smartToolbarController1:Tools["FilterHistory":U],
                  Infragistics.Win.UltraWinToolbars.PopupMenuTool):Tools:AddTool ("LRUEntry":U + STRING (i + 1, "99":U)) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Starts the Business Entity Tester for the current Business Entity
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID BusinessEntityTester ():

        DEFINE VARIABLE oForm       AS Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.UltraBusinessEntityTesterForm NO-UNDO .
        DEFINE VARIABLE cEntityName AS CHARACTER                                                                                   NO-UNDO.

        oForm = NEW Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.UltraBusinessEntityTesterForm  () .

        IF smartBusinessEntityAdapter1:QueryHandle:GET-BUFFER-HANDLE (1):AVAILABLE THEN DO:
            IF smartBusinessEntityAdapter1:QueryHandle:GET-BUFFER-HANDLE (1)::BusinessEntityPackage > "":U THEN
                ASSIGN cEntityName = SUBSTITUTE ("&1.&2":U,
                                                 smartBusinessEntityAdapter1:QueryHandle:GET-BUFFER-HANDLE (1)::BusinessEntityPackage,
                                                 smartBusinessEntityAdapter1:QueryHandle:GET-BUFFER-HANDLE (1)::BusinessEntityName) .
            ELSE
                ASSIGN cEntityName = smartBusinessEntityAdapter1:QueryHandle:GET-BUFFER-HANDLE (1)::BusinessEntityName .

            IF cEntityName <> "New":U THEN
                ASSIGN oForm:BusinessEntityName = cEntityName .
        END.

        oForm:Show () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Prompts for SaveChanges prompt if required
        Notes:
        @return Logical value indicating if the user has chosen to save changes
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL CheckChanges ():

        DEFINE VARIABLE oDialogResult AS DialogResultEnum NO-UNDO .
        DEFINE VARIABLE lOk           AS LOGICAL          NO-UNDO .

        IF oController:UnsavedChanges THEN DO:

            THIS-OBJECT:ActivateDesignCanvas () .

            oDialogResult = MessageFormHelper:ShowMessage("Save changes?"{&TRAN},
                                                          "Business Entity Designer"{&TRAN},
                                                          "The current design has unsaved changes."{&TRAN},
                                                          MessageFormImages:ImageQuestion,
                                                          MessageFormDefaults:LabelYesNoCancel,
                                                          MessageFormDefaults:GetDialogResultYesNoCancel()) .

            IF oDialogResult:Equals (DialogResultEnum:DialogResultNo) THEN
                RETURN TRUE .

            IF oDialogResult:Equals (DialogResultEnum:DialogResultCancel) THEN
                RETURN FALSE .

            SaveDesign(OUTPUT lOk) .

            RETURN lOk .
        END.

        RETURN TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Closes the source code editor
        Notes:
        @return Logical value indicating if the editor was closed
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL CheckCloseSourceCodeEditor ():

        IF NOT VALID-OBJECT (oBusinessEntitySourceEditorForm) THEN
            RETURN TRUE .

        IF oBusinessEntitySourceEditorForm:CloseAll () = TRUE THEN DO:
            GarbageCollectorHelper:DeleteObject (oBusinessEntitySourceEditorForm) .

            oBusinessEntitySourceEditorForm = ? .

            RETURN TRUE .
        END.
            ELSE RETURN FALSE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Collapses all tables on the model
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CollapseAllTables ():

        THIS-OBJECT:ActivateDesignCanvas () .

        {Consultingwerk/foreach.i System.Collections.DictionaryEntry oElement in THIS-OBJECT:model1:Shapes}

            IF TYPE-OF (oElement:Value, TempTableShape) THEN
                CAST (oElement:Value, TempTableShape):Expanded = FALSE .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Compiles the generated source code
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID Compile ():

        CAST (smartBusinessEntityAdapter1:DatasetController,
              BusinessEntityDatasetController):CompileSource () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Shows a dialog that allows to copy field names of the selected
                 table to the clipboard
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CopyFieldNames():

        DEFINE VARIABLE oEnumerator AS System.Collections.IDictionaryEnumerator NO-UNDO .
        DEFINE VARIABLE oTables     AS Crainiate.ERM4.Elements                  NO-UNDO .

        DEFINE VARIABLE oForm       AS CopyFieldsForm                           NO-UNDO .

        oForm = NEW CopyFieldsForm () .

        THIS-OBJECT:ActivateDesignCanvas () .

        ASSIGN oTables = model1:SelectedElements(Progress.Util.TypeHelper:GetType ("Crainiate.ERM4.Table":U)) .

        IF oTables:Count = 0 THEN
            RETURN .

        oEnumerator = oTables:GetEnumerator () .
        oEnumerator:MoveNext() .

        ASSIGN oForm:BufferHandle = oController:DatasetHandle::eField
               oForm:TableName    = CAST(CAST (oEnumerator:Current, System.Collections.DictionaryEntry):Value,
                                         Consultingwerk.BusinessEntityDesigner.UI.Shapes.TempTableShape):Heading .

        WAIT-FOR oForm:ShowDialog () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Displays a text in the "status" panel of the UltraStatusBar
        Notes:
        @param pcStatusText The StatusText to display
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisplayStatus (pcStatusText AS CHARACTER):

        ultraStatusBar1:Panels["status":U]:Text = pcStatusText .

        UserInterfaceHelper:ProcessEvents() .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Duplicates the currently selected table
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DuplicateSelectedTable ():

        DEFINE VARIABLE oEnumerator AS System.Collections.IDictionaryEnumerator NO-UNDO .
        DEFINE VARIABLE oTables     AS Crainiate.ERM4.Elements                  NO-UNDO .

        THIS-OBJECT:ActivateDesignCanvas () .

        ASSIGN oTables = model1:SelectedElements (Progress.Util.TypeHelper:GetType ("Crainiate.ERM4.Table":U)) .

        IF oTables:Count = 0 THEN
            RETURN .

        oEnumerator = oTables:GetEnumerator () .
        oEnumerator:MoveNext() .

        oController:DuplicateTable (CAST (CAST (oEnumerator:Current,
                                                System.Collections.DictionaryEntry):Value,
                                          Consultingwerk.BusinessEntityDesigner.UI.Shapes.TempTableShape)) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invokes the Field Editor Form
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID EditFields ():

        DEFINE VARIABLE oEnumerator AS System.Collections.IDictionaryEnumerator NO-UNDO .
        DEFINE VARIABLE oTables     AS Crainiate.ERM4.Elements                  NO-UNDO .
        DEFINE VARIABLE cTableName  AS CHARACTER                                NO-UNDO .

       THIS-OBJECT:ActivateDesignCanvas () .

        ASSIGN oTables = model1:SelectedElements (Progress.Util.TypeHelper:GetType ("Crainiate.ERM4.Table":U)) .

        IF oTables:Count = 0 THEN
            RETURN .

        oEnumerator = oTables:GetEnumerator () .
        oEnumerator:MoveNext() .

        ASSIGN cTableName = CAST (CAST (oEnumerator:Current,
                                        System.Collections.DictionaryEntry):Value,
                                  Consultingwerk.BusinessEntityDesigner.UI.Shapes.TempTableShape):Heading.

        oController:EditFields (cTableName) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens the BusinessEntitySourceEditorForm for the current business
                 entity
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID EditSource ():

        IF VALID-OBJECT (oBusinessEntitySourceEditorForm) THEN
            oBusinessEntitySourceEditorForm:Activate () .
        ELSE DO:
            oBusinessEntitySourceEditorForm = NEW BusinessEntitySourceEditorForm (oController:DatasetHandle) .
            oBusinessEntitySourceEditorForm:Show () .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Ensures that all Viewers have saved their changes
        Notes:
        @return Logical value indicating if all Viewers are saved
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL EnsureAllViewersSaved ():

        DEFINE VARIABLE oViewers AS Consultingwerk.SmartComponents.Base.SmartViewerControl NO-UNDO EXTENT 5 .
        DEFINE VARIABLE i        AS INTEGER                                                NO-UNDO .

        ASSIGN
            oViewers[1] = THIS-OBJECT:businessEntityViewerControl1
            oViewers[2] = THIS-OBJECT:memberTableViewerControl1
            oViewers[3] = THIS-OBJECT:dataRelationViewerControl1
            oViewers[4] = THIS-OBJECT:fieldViewerControl1
            oViewers[5] = THIS-OBJECT:indexViewerControl1
            .

        DO i = 1 TO EXTENT (oViewers):
            IF oViewers[i]:SmartTableIOState = TableIOStateEnum:ModifyingData THEN DO:
                 oViewers[i]:SaveChanges() .

                /* Mike Fechner, Consultingwerk Ltd. 21.02.2010
                   Check if still in update mode */
                IF oViewers[i]:SmartTableIOState = TableIOStateEnum:ModifyingData THEN
                    RETURN FALSE .
            END.
        END.

        RETURN TRUE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .

            RETURN FALSE .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Expands all tables on the model
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ExpandAllTables ():

        THIS-OBJECT:ActivateDesignCanvas () .

        {Consultingwerk/foreach.i System.Collections.DictionaryEntry oElement in THIS-OBJECT:model1:Shapes}

            IF TYPE-OF (oElement:Value, TempTableShape) THEN
                CAST (oElement:Value, TempTableShape):Expanded = TRUE .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose:
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FieldsReorder ():

        DEFINE VARIABLE oEnumerator AS System.Collections.IDictionaryEnumerator NO-UNDO .
        DEFINE VARIABLE oTables     AS Crainiate.ERM4.Elements                  NO-UNDO .

        THIS-OBJECT:ActivateDesignCanvas () .

        ASSIGN oTables = model1:SelectedElements(Progress.Util.TypeHelper:GetType ("Crainiate.ERM4.Table":U)) .

        IF oTables:Count = 0 THEN
            RETURN .

        oEnumerator = oTables:GetEnumerator () .
        oEnumerator:MoveNext() .

        CAST (smartBusinessEntityAdapter1:DatasetController,
              BusinessEntityDatasetController):FieldsReorder (CAST(CAST (oEnumerator:Current, System.Collections.DictionaryEntry):Value,
                                                                   Consultingwerk.BusinessEntityDesigner.UI.Shapes.TempTableShape)) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Filters the list of database tables in the toolbox
        Notes:
        @param pcFilter The filter value
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FilterTables (pcFilter AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DEFINE VARIABLE oListView AS UltraListView NO-UNDO .

        searchTimer:Enabled = FALSE .

        IF pcFilter > "":U THEN .
        ELSE ASSIGN pcFilter = "":U .

        DO i = 1 TO ultraExplorerBar1:Groups:Count - 1:

            IF TYPE-OF (ultraExplorerBar1:Groups[i]:Container:Controls[0],
                        UltraListView) THEN DO:

                oListView = CAST (ultraExplorerBar1:Groups[i]:Container:Controls[0],
                                  UltraListView) .

                {Consultingwerk/foreach.i UltraListViewItem oItem in oListView:Items}
                    oItem:VISIBLE = oItem:Text BEGINS pcFilter .
                END.
            END.
        END.

        IF pcFilter > "":U THEN DO:
            IF oLRUList:Contains (pcFilter) THEN
                oLRUList:Remove (pcFilter) .

            oLRUList:Insert (0, pcFilter) .

            DO WHILE oLRUList:Count > 10:
                oLRUList:RemoveAt (10) .
            END.

            BuildLRUMenuItems () .
            StoreLRU () .
        END.

        IF pcFilter <> CAST (smartToolbarController1:Tools ["FilterTable":U],
                             Infragistics.Win.UltraWinToolbars.TextBoxTool):Text THEN

            CAST (smartToolbarController1:Tools ["FilterTable":U],
                  Infragistics.Win.UltraWinToolbars.TextBoxTool):Text = pcFilter .

        IF pcFilter = "":U THEN
            CAST (smartToolbarController1:Tools ["FilterTable":U]:SharedProps:ToolInstances[0],
                              Infragistics.Win.UltraWinToolbars.TextBoxTool):IsInEditMode = TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Generates the source code
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GenerateSource ():

        IF NOT THIS-OBJECT:EnsureAllViewersSaved () THEN
            RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 16.05.2014
           SCL-299: Prompt for save changes before generate */
        IF BusinessEntityDesignerSettings:PromptForSaveBeforeGenerate THEN DO:
            IF NOT THIS-OBJECT:CheckChanges() THEN
                RETURN .
        END.

        CAST (smartBusinessEntityAdapter1:DatasetController,
              BusinessEntityDatasetController):GenerateSource () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the GenericStatusChanded event of the IStatusManager
        Notes:
        @param sender The reference to the IStatusManager that raised the event
        @param pcStatus The new status text
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GenericStatusChandedHandler (sender AS IStatusManager,
                                                       pcStatus AS CHARACTER):

        THIS-OBJECT:DisplayStatus (pcStatus) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Prompts for a file to import and invokes the ImportTableFromFile method
                 in the BusinessEntityDatasetController class
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ImportTable ():

        DEFINE VARIABLE oDialogResult AS DialogResult NO-UNDO .

        WAIT-FOR openFileDialog2:ShowDialog (THIS-OBJECT) SET oDialogResult .

        THIS-OBJECT:ActivateDesignCanvas () .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult}

        oController:ImportTableFromFile (openFileDialog2:FileName) .

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
        DEFINE VARIABLE optionSet1 AS Infragistics.Win.UltraWinToolbars.OptionSet NO-UNDO.
        optionSet1 = NEW Infragistics.Win.UltraWinToolbars.OptionSet("Pointer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool49 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool49 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Options":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool24 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool24 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool26 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool26 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool32 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool32 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("New":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool28 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool28 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool51 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool51 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SaveAs":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool27 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool27 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Close":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool37 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool37 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Print":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool76 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool76 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Session Info":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool64 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool64 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Database Connections":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool108 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool108 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Database Login Preferences":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool110 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool110 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("New Window":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("Business Entity":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("Maintenance":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool21 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool21 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool22 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool22 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool23 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool23 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("Design":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool1 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool1 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("DrawPointer":U, "Pointer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool4 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool4 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("DrawConnector":U, "Pointer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool70 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool70 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("AutoLayout":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool66 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool66 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Import Table":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool68 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool68 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Paste ABL Source Code":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool107 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool107 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Custom Business Entity Properties":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("relation":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool59 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool59 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Relation Fields":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup4 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("Source":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool39 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool39 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GenerateSource":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool40 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool40 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Regenerate Source":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool43 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool43 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Compile":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool83 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool83 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Edit Source":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup5 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup5 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("Test":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool34 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool34 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("BusinessEntityTester":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup6 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup6 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("Filter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE textBoxTool1 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
        textBoxTool1 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("FilterTable":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool45 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool45 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("FilterClear":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool1 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool1 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("FilterHistory":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool87 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool87 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Expand All":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool88 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool88 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Collapse All":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab2 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab2 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("table":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup7 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup7 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("tablefields":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool53 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool53 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("RefreshTable":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool79 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool79 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("RefreshTable2":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool56 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool56 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("FieldsReorder":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool80 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool80 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Duplicate Table":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool105 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool105 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Custom Table Properties":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab3 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab3 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("relation tab":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup8 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup8 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool81 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool81 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Relation Fields":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool82 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool82 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Remove Relation":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool106 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool106 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Custom Relation Properties":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool65 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool65 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool63 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool63 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool78 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool78 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Session Info":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool2 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool2 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("DrawPointer":U, "Pointer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool5 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool5 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("DrawConnector":U, "Pointer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool36 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool36 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("BusinessEntityTester":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool112 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool112 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("New Window":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TableIOToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("NavigationToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool17 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool17 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool20 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool20 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool15 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool15 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool16 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool16 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool25 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool25 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool29 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool29 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool30 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool30 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Close":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
        appearance32 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
        appearance31 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool31 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool31 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
        appearance29 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
        appearance30 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool33 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool33 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("New":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
        appearance28 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool3 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool3 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("DrawPointer":U, "Pointer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance38 AS Infragistics.Win.Appearance NO-UNDO.
        appearance38 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance39 AS Infragistics.Win.Appearance NO-UNDO.
        appearance39 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool6 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool6 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("DrawConnector":U, "Pointer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance40 AS Infragistics.Win.Appearance NO-UNDO.
        appearance40 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance41 AS Infragistics.Win.Appearance NO-UNDO.
        appearance41 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool35 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool35 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("BusinessEntityTester":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
        appearance34 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance35 AS Infragistics.Win.Appearance NO-UNDO.
        appearance35 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool38 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool38 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Print":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance45 AS Infragistics.Win.Appearance NO-UNDO.
        appearance45 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance44 AS Infragistics.Win.Appearance NO-UNDO.
        appearance44 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool41 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool41 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GenerateSource":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance47 AS Infragistics.Win.Appearance NO-UNDO.
        appearance47 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance48 AS Infragistics.Win.Appearance NO-UNDO.
        appearance48 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool42 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool42 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Regenerate Source":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance49 AS Infragistics.Win.Appearance NO-UNDO.
        appearance49 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance50 AS Infragistics.Win.Appearance NO-UNDO.
        appearance50 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool44 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool44 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Compile":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance51 AS Infragistics.Win.Appearance NO-UNDO.
        appearance51 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance52 AS Infragistics.Win.Appearance NO-UNDO.
        appearance52 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE textBoxTool3 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
        textBoxTool3 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("FilterTable":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool2 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool2 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("FilterHistory":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance56 AS Infragistics.Win.Appearance NO-UNDO.
        appearance56 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance55 AS Infragistics.Win.Appearance NO-UNDO.
        appearance55 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool47 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool47 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("LRUEntry01":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool46 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool46 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("FilterClear":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance54 AS Infragistics.Win.Appearance NO-UNDO.
        appearance54 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance53 AS Infragistics.Win.Appearance NO-UNDO.
        appearance53 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool48 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool48 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("LRUEntry01":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool50 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool50 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Options":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance58 AS Infragistics.Win.Appearance NO-UNDO.
        appearance58 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance57 AS Infragistics.Win.Appearance NO-UNDO.
        appearance57 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool52 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool52 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SaveAs":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance64 AS Infragistics.Win.Appearance NO-UNDO.
        appearance64 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance63 AS Infragistics.Win.Appearance NO-UNDO.
        appearance63 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool54 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool54 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("RefreshTable":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance65 AS Infragistics.Win.Appearance NO-UNDO.
        appearance65 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance66 AS Infragistics.Win.Appearance NO-UNDO.
        appearance66 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool3 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool3 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Modelpopup":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool55 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool55 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("RefreshTable":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool73 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool73 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("RefreshTable2":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool58 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool58 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("FieldsReorder":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool75 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool75 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Duplicate Table":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool86 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool86 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Edit Fields":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool102 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool102 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Split Extend Field into separate Fields":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool116 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool116 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Copy Field Names":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool61 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool61 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Relation Fields":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool100 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool100 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Remove Relation":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool98 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool98 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Custom Business Entity Properties":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool99 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool99 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Custom Table Properties":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool101 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool101 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Custom Relation Properties":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool103 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool103 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Custom Field Properties":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool104 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool104 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Custom Index Properties":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool57 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool57 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("FieldsReorder":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance67 AS Infragistics.Win.Appearance NO-UNDO.
        appearance67 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance68 AS Infragistics.Win.Appearance NO-UNDO.
        appearance68 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool60 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool60 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Relation Fields":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance69 AS Infragistics.Win.Appearance NO-UNDO.
        appearance69 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance70 AS Infragistics.Win.Appearance NO-UNDO.
        appearance70 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool62 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool62 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Remove Relation":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance72 AS Infragistics.Win.Appearance NO-UNDO.
        appearance72 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance71 AS Infragistics.Win.Appearance NO-UNDO.
        appearance71 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool67 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool67 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Import Table":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance74 AS Infragistics.Win.Appearance NO-UNDO.
        appearance74 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance73 AS Infragistics.Win.Appearance NO-UNDO.
        appearance73 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool69 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool69 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Paste ABL Source Code":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance76 AS Infragistics.Win.Appearance NO-UNDO.
        appearance76 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance75 AS Infragistics.Win.Appearance NO-UNDO.
        appearance75 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool71 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool71 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("AutoLayout":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance78 AS Infragistics.Win.Appearance NO-UNDO.
        appearance78 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance79 AS Infragistics.Win.Appearance NO-UNDO.
        appearance79 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool72 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool72 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("RefreshTable2":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance81 AS Infragistics.Win.Appearance NO-UNDO.
        appearance81 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance80 AS Infragistics.Win.Appearance NO-UNDO.
        appearance80 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool74 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool74 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Duplicate Table":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance82 AS Infragistics.Win.Appearance NO-UNDO.
        appearance82 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance83 AS Infragistics.Win.Appearance NO-UNDO.
        appearance83 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool77 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool77 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Session Info":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance85 AS Infragistics.Win.Appearance NO-UNDO.
        appearance85 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance84 AS Infragistics.Win.Appearance NO-UNDO.
        appearance84 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool84 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool84 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Edit Source":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance87 AS Infragistics.Win.Appearance NO-UNDO.
        appearance87 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance86 AS Infragistics.Win.Appearance NO-UNDO.
        appearance86 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool85 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool85 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Edit Fields":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance89 AS Infragistics.Win.Appearance NO-UNDO.
        appearance89 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance88 AS Infragistics.Win.Appearance NO-UNDO.
        appearance88 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool89 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool89 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Expand All":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance91 AS Infragistics.Win.Appearance NO-UNDO.
        appearance91 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance90 AS Infragistics.Win.Appearance NO-UNDO.
        appearance90 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool90 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool90 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Collapse All":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance93 AS Infragistics.Win.Appearance NO-UNDO.
        appearance93 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance92 AS Infragistics.Win.Appearance NO-UNDO.
        appearance92 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool91 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool91 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Custom Business Entity Properties":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance94 AS Infragistics.Win.Appearance NO-UNDO.
        appearance94 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance99 AS Infragistics.Win.Appearance NO-UNDO.
        appearance99 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool92 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool92 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Custom Table Properties":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance95 AS Infragistics.Win.Appearance NO-UNDO.
        appearance95 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance100 AS Infragistics.Win.Appearance NO-UNDO.
        appearance100 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool93 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool93 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Custom Relation Properties":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance96 AS Infragistics.Win.Appearance NO-UNDO.
        appearance96 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance101 AS Infragistics.Win.Appearance NO-UNDO.
        appearance101 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool94 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool94 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Custom Field Properties":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance97 AS Infragistics.Win.Appearance NO-UNDO.
        appearance97 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance102 AS Infragistics.Win.Appearance NO-UNDO.
        appearance102 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool95 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool95 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Custom Index Properties":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance98 AS Infragistics.Win.Appearance NO-UNDO.
        appearance98 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance103 AS Infragistics.Win.Appearance NO-UNDO.
        appearance103 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool96 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool96 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Database Connections":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance104 AS Infragistics.Win.Appearance NO-UNDO.
        appearance104 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance105 AS Infragistics.Win.Appearance NO-UNDO.
        appearance105 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool97 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool97 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Split Extend Field into separate Fields":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance107 AS Infragistics.Win.Appearance NO-UNDO.
        appearance107 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance106 AS Infragistics.Win.Appearance NO-UNDO.
        appearance106 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool109 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool109 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Database Login Preferences":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance109 AS Infragistics.Win.Appearance NO-UNDO.
        appearance109 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance108 AS Infragistics.Win.Appearance NO-UNDO.
        appearance108 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool111 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool111 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("New Window":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance112 AS Infragistics.Win.Appearance NO-UNDO.
        appearance112 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance111 AS Infragistics.Win.Appearance NO-UNDO.
        appearance111 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool113 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool113 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Copy Field Names":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance113 AS Infragistics.Win.Appearance NO-UNDO.
        appearance113 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarGroup1 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup NO-UNDO.
        ultraExplorerBarGroup1 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem1 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem1 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem2 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem2 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem3 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem3 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem4 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem4 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode1 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode1 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode2 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode2 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode3 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode3 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode4 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode4 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode5 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode5 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane1 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane1 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedBottom, NEW System.Guid("9d216137-a262-4de2-9434-922a8a48d6d3":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane1 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane1 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("0c9ce1cf-ec40-4980-96a8-60d528ed09da":U), NEW System.Guid("46b5c2e7-f1d2-4b8d-af7e-bbb1d40c8bff":U), 0, NEW System.Guid("9d216137-a262-4de2-9434-922a8a48d6d3":U), 0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityDesignerForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane2 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane2 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("2546c946-13ea-4179-b28d-e775b932b35f":U), NEW System.Guid("46b5c2e7-f1d2-4b8d-af7e-bbb1d40c8bff":U), 1, NEW System.Guid("9d216137-a262-4de2-9434-922a8a48d6d3":U), 1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane3 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane3 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("23e227a8-d239-4448-9451-a1c6caeea295":U), NEW System.Guid("9d1b11c0-41b0-479f-bd4d-e10c702ec084":U), -1, NEW System.Guid("9d216137-a262-4de2-9434-922a8a48d6d3":U), 0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane4 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane4 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("519f0dc4-9981-4fb9-a7d8-0cc5d289d2eb":U), NEW System.Guid("2aafd0f5-54ed-4e13-b29b-7f50640db9d2":U), -1, NEW System.Guid("9d216137-a262-4de2-9434-922a8a48d6d3":U), 0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane5 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane5 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("6257e870-b1bb-4b6b-95e2-6455081dd5ac":U), NEW System.Guid("a5029409-6cae-4185-a6aa-1c0bc560efe8":U), -1, NEW System.Guid("9d216137-a262-4de2-9434-922a8a48d6d3":U), 0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane2 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane2 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedRight, NEW System.Guid("10452164-36b2-4dfd-a7a3-fc98a7f9c05b":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableGroupPane1 AS Infragistics.Win.UltraWinDock.DockableGroupPane NO-UNDO.
        dockableGroupPane1 = NEW Infragistics.Win.UltraWinDock.DockableGroupPane(NEW System.Guid("e4fec495-76f4-450f-aa30-f6f71cf3998e":U), NEW System.Guid("9d6539bc-1849-4822-a623-923cb4508258":U), -1, NEW System.Guid("10452164-36b2-4dfd-a7a3-fc98a7f9c05b":U), -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane6 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane6 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("698f5dc6-e51e-44dd-8844-ce8290e5a3ec":U), NEW System.Guid("9d6539bc-1849-4822-a623-923cb4508258":U), 0, NEW System.Guid("e4fec495-76f4-450f-aa30-f6f71cf3998e":U), 0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane7 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane7 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("efffd978-2cec-49fd-a1c0-2415a367c5b9":U), NEW System.Guid("1222e1d8-4f6f-4883-9e69-1ae07e805be2":U), 0, NEW System.Guid("e4fec495-76f4-450f-aa30-f6f71cf3998e":U), 1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane8 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane8 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("8bcabf95-d601-41e1-9d2a-38e51758339e":U), NEW System.Guid("004864d1-152d-4f88-b595-7f10204aca02":U), -1, NEW System.Guid("10452164-36b2-4dfd-a7a3-fc98a7f9c05b":U), 1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance62 AS Infragistics.Win.Appearance NO-UNDO.
        appearance62 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane3 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane3 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedBottom, NEW System.Guid("e7637d20-0648-4b5c-b889-aab80a9a55eb":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane9 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane9 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("55ae1e75-1f64-479c-9bd0-11434b09b896":U), NEW System.Guid("8403b94a-0cb1-48c7-96b1-87cf91e377ef":U), -1, NEW System.Guid("e7637d20-0648-4b5c-b889-aab80a9a55eb":U), 0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
        appearance33 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane10 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane10 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("3df06839-c557-4bd1-925b-ba102cc70d9f":U), NEW System.Guid("321a63f8-8b2c-49a1-b3f7-63140a53670d":U), -1, NEW System.Guid("e7637d20-0648-4b5c-b889-aab80a9a55eb":U), 0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane4 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane4 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:Floating, NEW System.Guid("321a63f8-8b2c-49a1-b3f7-63140a53670d":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane5 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane5 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:Floating, NEW System.Guid("1222e1d8-4f6f-4883-9e69-1ae07e805be2":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane6 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane6 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:Floating, NEW System.Guid("46b5c2e7-f1d2-4b8d-af7e-bbb1d40c8bff":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane7 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane7 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:Floating, NEW System.Guid("9d1b11c0-41b0-479f-bd4d-e10c702ec084":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane8 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane8 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:Floating, NEW System.Guid("2aafd0f5-54ed-4e13-b29b-7f50640db9d2":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane9 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane9 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:Floating, NEW System.Guid("a5029409-6cae-4185-a6aa-1c0bc560efe8":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane10 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane10 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:Floating, NEW System.Guid("004864d1-152d-4f88-b595-7f10204aca02":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane11 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane11 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:Floating, NEW System.Guid("9d6539bc-1849-4822-a623-923cb4508258":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane12 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane12 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:Floating, NEW System.Guid("8403b94a-0cb1-48c7-96b1-87cf91e377ef":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraStatusPanel1 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel1 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraStatusPanel2 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel2 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraStatusPanel3 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel3 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraStatusPanel4 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel4 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraStatusPanel5 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel5 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraStatusPanel6 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel6 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraStatusPanel7 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel7 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraStatusPanel8 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel8 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraStatusPanel9 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel9 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        THIS-OBJECT:businessEntityViewerControl1 = NEW Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityViewerControl().
        THIS-OBJECT:smartBusinessEntityAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter(THIS-OBJECT:components).
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:model1 = NEW Crainiate.ERM4.Model().
        THIS-OBJECT:memberTableViewerControl1 = NEW Consultingwerk.BusinessEntityDesigner.UI.MemberTableViewerControl().
        THIS-OBJECT:tableAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter(THIS-OBJECT:components).
        THIS-OBJECT:dataRelationViewerControl1 = NEW Consultingwerk.BusinessEntityDesigner.UI.DataRelationViewerControl().
        THIS-OBJECT:dataRelationAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter(THIS-OBJECT:components).
        THIS-OBJECT:fieldViewerControl1 = NEW Consultingwerk.BusinessEntityDesigner.UI.FieldViewerControl().
        THIS-OBJECT:fieldAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter(THIS-OBJECT:components).
        THIS-OBJECT:indexViewerControl1 = NEW Consultingwerk.BusinessEntityDesigner.UI.IndexViewerControl().
        THIS-OBJECT:indexAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter(THIS-OBJECT:components).
        THIS-OBJECT:databaseFieldsControl1 = NEW Consultingwerk.BusinessEntityDesigner.UI.DatabaseFieldsControl().
        THIS-OBJECT:ultraExplorerBar1 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBar().
        THIS-OBJECT:ultraTree1 = NEW Infragistics.Win.UltraWinTree.UltraTree().
        THIS-OBJECT:logManagerSettings1 = NEW Consultingwerk.Windows.Util.Controls.LogManagerSettings().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:ultraTrackBar1 = NEW Infragistics.Win.UltraWinEditors.UltraTrackBar().
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:ultraDockManager1 = NEW Infragistics.Win.UltraWinDock.UltraDockManager(THIS-OBJECT:components).
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaLeft = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaRight = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaTop = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaBottom = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_BusinessEntityDesignerFormAutoHideControl = NEW Infragistics.Win.UltraWinDock.AutoHideControl().
        THIS-OBJECT:dockableWindow1 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:windowDockingArea2 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:dockableWindow3 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:dockableWindow5 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:dockableWindow6 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:dockableWindow7 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:dockableWindow2 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:dockableWindow4 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:windowDockingArea4 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:dockableWindow9 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:windowDockingArea5 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:dockableWindow8 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:dockableWindow10 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:windowDockingArea6 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:windowDockingArea7 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:windowDockingArea3 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:timer1 = NEW System.Windows.Forms.Timer(THIS-OBJECT:components).
        THIS-OBJECT:smartBusinessEntityAdapter2 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter(THIS-OBJECT:components).
        THIS-OBJECT:windowDockingArea1 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:saveFileDialog1 = NEW System.Windows.Forms.SaveFileDialog().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        THIS-OBJECT:windowDockingArea9 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:windowDockingArea8 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:ultraStatusBar1 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusBar().
        THIS-OBJECT:windowDockingArea11 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:ultraPrintPreviewDialog1 = NEW Infragistics.Win.Printing.UltraPrintPreviewDialog(THIS-OBJECT:components).
        THIS-OBJECT:openFileDialog2 = NEW System.Windows.Forms.OpenFileDialog().
        THIS-OBJECT:windowDockingArea10 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:searchTimer = NEW System.Windows.Forms.Timer(THIS-OBJECT:components).
        THIS-OBJECT:windowDockingArea13 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        CAST(THIS-OBJECT:businessEntityViewerControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityAdapter1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:memberTableViewerControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tableAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:dataRelationViewerControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:dataRelationAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:fieldViewerControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:fieldAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:indexViewerControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:indexAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraExplorerBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:ultraTrackBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:dockableWindow1:SuspendLayout().
        THIS-OBJECT:windowDockingArea2:SuspendLayout().
        THIS-OBJECT:dockableWindow3:SuspendLayout().
        THIS-OBJECT:dockableWindow5:SuspendLayout().
        THIS-OBJECT:dockableWindow6:SuspendLayout().
        THIS-OBJECT:dockableWindow7:SuspendLayout().
        THIS-OBJECT:dockableWindow2:SuspendLayout().
        THIS-OBJECT:dockableWindow4:SuspendLayout().
        THIS-OBJECT:windowDockingArea4:SuspendLayout().
        THIS-OBJECT:dockableWindow9:SuspendLayout().
        THIS-OBJECT:windowDockingArea5:SuspendLayout().
        THIS-OBJECT:dockableWindow8:SuspendLayout().
        THIS-OBJECT:dockableWindow10:SuspendLayout().
        CAST(THIS-OBJECT:smartBusinessEntityAdapter2, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraStatusBar1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* businessEntityViewerControl1 */
        /*  */
        THIS-OBJECT:businessEntityViewerControl1:FocusControlOnAdd = ?.
        THIS-OBJECT:businessEntityViewerControl1:FocusControlOnUpdate = ?.
        THIS-OBJECT:businessEntityViewerControl1:LinkDataSource = THIS-OBJECT:smartBusinessEntityAdapter1.
        THIS-OBJECT:businessEntityViewerControl1:LinkGroupAssignSource = ?.
        THIS-OBJECT:businessEntityViewerControl1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:businessEntityViewerControl1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:businessEntityViewerControl1:Name = "businessEntityViewerControl1":U.
        THIS-OBJECT:businessEntityViewerControl1:Size = NEW System.Drawing.Size(755, 191).
        THIS-OBJECT:businessEntityViewerControl1:TabIndex = 11.
        /*  */
        /* smartBusinessEntityAdapter1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityAdapter1:DatasetControllerType = "Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDatasetController":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:EntityName = "Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:EntityTable = "eBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkCommitSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkNavigationSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter1:ThrowErrorsFromRetrieveData = TRUE.
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm":U).
        optionSet1:AllowAllUp = FALSE.
        THIS-OBJECT:smartToolbarController1:OptionSets:Add(optionSet1).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar0[1] = buttonTool49.
        arrayvar0[2] = buttonTool24.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar0).
        buttonTool28:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool37:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool76:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool110:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 10 NO-UNDO.
        arrayvar1[1] = buttonTool26.
        arrayvar1[2] = buttonTool32.
        arrayvar1[3] = buttonTool28.
        arrayvar1[4] = buttonTool51.
        arrayvar1[5] = buttonTool27.
        arrayvar1[6] = buttonTool37.
        arrayvar1[7] = buttonTool76.
        arrayvar1[8] = buttonTool64.
        arrayvar1[9] = buttonTool108.
        arrayvar1[10] = buttonTool110.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar1).
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenuButtonImage = CAST(resources:GetObject("smartToolbarController1.Ribbon.ApplicationMenuButtonImage":U), System.Drawing.Image).
        ribbonTab1:Caption = "Business Entity":U.
        ribbonGroup1:Caption = "Maintenance":U.
        buttonTool21:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool22:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool23:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar2[1] = buttonTool21.
        arrayvar2[2] = buttonTool22.
        arrayvar2[3] = buttonTool23.
        ribbonGroup1:Tools:AddRange(arrayvar2).
        ribbonGroup2:Caption = "Design":U.
        stateButtonTool1:Checked = TRUE.
        stateButtonTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        stateButtonTool4:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool70:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool66:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool66:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool68:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool107:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool107:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar3[1] = stateButtonTool1.
        arrayvar3[2] = stateButtonTool4.
        arrayvar3[3] = buttonTool70.
        arrayvar3[4] = buttonTool66.
        arrayvar3[5] = buttonTool68.
        arrayvar3[6] = buttonTool107.
        ribbonGroup2:Tools:AddRange(arrayvar3).
        ribbonGroup3:Caption = "Relation":U.
        buttonTool59:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar4[1] = buttonTool59.
        ribbonGroup3:Tools:AddRange(arrayvar4).
        ribbonGroup4:Caption = "Build":U.
        buttonTool39:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool40:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool43:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool83:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool83:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar5[1] = buttonTool39.
        arrayvar5[2] = buttonTool40.
        arrayvar5[3] = buttonTool43.
        arrayvar5[4] = buttonTool83.
        ribbonGroup4:Tools:AddRange(arrayvar5).
        ribbonGroup5:Caption = "Test":U.
        buttonTool34:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar6[1] = buttonTool34.
        ribbonGroup5:Tools:AddRange(arrayvar6).
        ribbonGroup6:Caption = "Database Tables":U.
        buttonTool87:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool87:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool88:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 5 NO-UNDO.
        arrayvar7[1] = textBoxTool1.
        arrayvar7[2] = buttonTool45.
        arrayvar7[3] = popupMenuTool1.
        arrayvar7[4] = buttonTool87.
        arrayvar7[5] = buttonTool88.
        ribbonGroup6:Tools:AddRange(arrayvar7).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 6 NO-UNDO.
        arrayvar8[1] = ribbonGroup1.
        arrayvar8[2] = ribbonGroup2.
        arrayvar8[3] = ribbonGroup3.
        arrayvar8[4] = ribbonGroup4.
        arrayvar8[5] = ribbonGroup5.
        arrayvar8[6] = ribbonGroup6.
        ribbonTab1:Groups:AddRange(arrayvar8).
        ribbonTab2:Caption = "Temp Table":U.
        ribbonGroup7:Caption = "Table Tools":U.
        buttonTool53:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool79:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool56:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool56:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool80:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool80:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool105:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool105:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 5 NO-UNDO.
        arrayvar9[1] = buttonTool53.
        arrayvar9[2] = buttonTool79.
        arrayvar9[3] = buttonTool56.
        arrayvar9[4] = buttonTool80.
        arrayvar9[5] = buttonTool105.
        ribbonGroup7:Tools:AddRange(arrayvar9).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 1 NO-UNDO.
        arrayvar10[1] = ribbonGroup7.
        ribbonTab2:Groups:AddRange(arrayvar10).
        ribbonTab3:Caption = "Data-Relation":U.
        ribbonGroup8:Caption = "Data-Relation Tools":U.
        buttonTool81:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool82:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool106:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool106:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar11[1] = buttonTool81.
        arrayvar11[2] = buttonTool82.
        arrayvar11[3] = buttonTool106.
        ribbonGroup8:Tools:AddRange(arrayvar11).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 1 NO-UNDO.
        arrayvar12[1] = ribbonGroup8.
        ribbonTab3:Groups:AddRange(arrayvar12).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 3 NO-UNDO.
        arrayvar13[1] = ribbonTab1.
        arrayvar13[2] = ribbonTab2.
        arrayvar13[3] = ribbonTab3.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar13).
        buttonTool78:InstanceProps:IsFirstInGroup = TRUE.
        stateButtonTool2:Checked = TRUE.
        stateButtonTool2:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool36:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 7 NO-UNDO.
        arrayvar14[1] = buttonTool65.
        arrayvar14[2] = buttonTool63.
        arrayvar14[3] = buttonTool78.
        arrayvar14[4] = stateButtonTool2.
        arrayvar14[5] = stateButtonTool5.
        arrayvar14[6] = buttonTool36.
        arrayvar14[7] = buttonTool112.
        THIS-OBJECT:smartToolbarController1:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar14).
        THIS-OBJECT:smartToolbarController1:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        buttonTool7:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar15 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar15[1] = buttonTool7.
        arrayvar15[2] = buttonTool8.
        arrayvar15[3] = buttonTool9.
        arrayvar15[4] = buttonTool10.
        arrayvar15[5] = buttonTool11.
        arrayvar15[6] = buttonTool12.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar15).
        ultraToolbar1:Text = "TableIO Toolbar":U.
        buttonTool17:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar16 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar16[1] = buttonTool17.
        arrayvar16[2] = buttonTool18.
        arrayvar16[3] = buttonTool19.
        arrayvar16[4] = buttonTool20.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar16).
        ultraToolbar2:Text = "Navigation Toolbar":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar17 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 2 NO-UNDO.
        arrayvar17[1] = ultraToolbar1.
        arrayvar17[2] = ultraToolbar2.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar17).
        appearance11:Image = CAST(resources:GetObject("appearance11.Image":U), System.Object).
        buttonTool1:SharedPropsInternal:AppearancesSmall:Appearance = appearance11.
        buttonTool1:SharedPropsInternal:Caption = "Add record":U.
        buttonTool1:SharedPropsInternal:CustomizerCaption = "Add":U.
        buttonTool1:SharedPropsInternal:ToolTipText = "Add record":U.
        appearance12:Image = CAST(resources:GetObject("appearance12.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance12.
        buttonTool2:SharedPropsInternal:Caption = "Copy record":U.
        buttonTool2:SharedPropsInternal:CustomizerCaption = "Copy":U.
        buttonTool2:SharedPropsInternal:ToolTipText = "Copy record":U.
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        buttonTool3:SharedPropsInternal:AppearancesSmall:Appearance = appearance13.
        buttonTool3:SharedPropsInternal:Caption = "Update record":U.
        buttonTool3:SharedPropsInternal:CustomizerCaption = "Update":U.
        buttonTool3:SharedPropsInternal:ToolTipText = "Update record":U.
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesSmall:Appearance = appearance14.
        buttonTool4:SharedPropsInternal:Caption = "Save changes":U.
        buttonTool4:SharedPropsInternal:CustomizerCaption = "Save":U.
        buttonTool4:SharedPropsInternal:Enabled = FALSE.
        buttonTool4:SharedPropsInternal:ToolTipText = "Save changes":U.
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        buttonTool5:SharedPropsInternal:AppearancesSmall:Appearance = appearance15.
        buttonTool5:SharedPropsInternal:Caption = "Cancel update":U.
        buttonTool5:SharedPropsInternal:CustomizerCaption = "Cancel":U.
        buttonTool5:SharedPropsInternal:Enabled = FALSE.
        buttonTool5:SharedPropsInternal:ToolTipText = "Cancel update":U.
        appearance16:Image = CAST(resources:GetObject("appearance16.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesSmall:Appearance = appearance16.
        buttonTool6:SharedPropsInternal:Caption = "Delete record":U.
        buttonTool6:SharedPropsInternal:CustomizerCaption = "Delete":U.
        buttonTool6:SharedPropsInternal:ToolTipText = "Delete record":U.
        appearance17:Image = CAST(resources:GetObject("appearance17.Image":U), System.Object).
        buttonTool13:SharedPropsInternal:AppearancesSmall:Appearance = appearance17.
        buttonTool13:SharedPropsInternal:Caption = "First record":U.
        buttonTool13:SharedPropsInternal:CustomizerCaption = "First":U.
        buttonTool13:SharedPropsInternal:Enabled = FALSE.
        buttonTool13:SharedPropsInternal:ToolTipText = "First record":U.
        appearance18:Image = CAST(resources:GetObject("appearance18.Image":U), System.Object).
        buttonTool14:SharedPropsInternal:AppearancesSmall:Appearance = appearance18.
        buttonTool14:SharedPropsInternal:Caption = "Previous record":U.
        buttonTool14:SharedPropsInternal:CustomizerCaption = "Previous":U.
        buttonTool14:SharedPropsInternal:Enabled = FALSE.
        buttonTool14:SharedPropsInternal:ToolTipText = "Previous record":U.
        appearance19:Image = CAST(resources:GetObject("appearance19.Image":U), System.Object).
        buttonTool15:SharedPropsInternal:AppearancesSmall:Appearance = appearance19.
        buttonTool15:SharedPropsInternal:Caption = "Next record":U.
        buttonTool15:SharedPropsInternal:CustomizerCaption = "Next":U.
        buttonTool15:SharedPropsInternal:Enabled = FALSE.
        buttonTool15:SharedPropsInternal:ToolTipText = "Next record":U.
        appearance20:Image = CAST(resources:GetObject("appearance20.Image":U), System.Object).
        buttonTool16:SharedPropsInternal:AppearancesSmall:Appearance = appearance20.
        buttonTool16:SharedPropsInternal:Caption = "Last record":U.
        buttonTool16:SharedPropsInternal:CustomizerCaption = "Last":U.
        buttonTool16:SharedPropsInternal:Enabled = FALSE.
        buttonTool16:SharedPropsInternal:ToolTipText = "Last record":U.
        appearance21:Image = CAST(resources:GetObject("appearance21.Image":U), System.Object).
        buttonTool25:SharedPropsInternal:AppearancesSmall:Appearance = appearance21.
        buttonTool25:SharedPropsInternal:Caption = "&Exit":U.
        buttonTool25:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance25:Image = CAST(resources:GetObject("appearance25.Image":U), System.Object).
        buttonTool29:SharedPropsInternal:AppearancesLarge:Appearance = appearance25.
        appearance26:Image = CAST(resources:GetObject("appearance26.Image":U), System.Object).
        buttonTool29:SharedPropsInternal:AppearancesSmall:Appearance = appearance26.
        buttonTool29:SharedPropsInternal:Caption = "&Open":U.
        appearance32:Image = CAST(resources:GetObject("appearance32.Image":U), System.Object).
        buttonTool30:SharedPropsInternal:AppearancesLarge:Appearance = appearance32.
        appearance31:Image = CAST(resources:GetObject("appearance31.Image":U), System.Object).
        buttonTool30:SharedPropsInternal:AppearancesSmall:Appearance = appearance31.
        buttonTool30:SharedPropsInternal:Caption = "&Close":U.
        appearance29:Image = CAST(resources:GetObject("appearance29.Image":U), System.Object).
        buttonTool31:SharedPropsInternal:AppearancesLarge:Appearance = appearance29.
        appearance30:Image = CAST(resources:GetObject("appearance30.Image":U), System.Object).
        buttonTool31:SharedPropsInternal:AppearancesSmall:Appearance = appearance30.
        buttonTool31:SharedPropsInternal:Caption = "&Save":U.
        appearance28:Image = CAST(resources:GetObject("appearance28.Image":U), System.Object).
        buttonTool33:SharedPropsInternal:AppearancesLarge:Appearance = appearance28.
        appearance27:Image = CAST(resources:GetObject("appearance27.Image":U), System.Object).
        buttonTool33:SharedPropsInternal:AppearancesSmall:Appearance = appearance27.
        buttonTool33:SharedPropsInternal:Caption = "&New":U.
        stateButtonTool3:Checked = TRUE.
        stateButtonTool3:OptionSetKey = "Pointer":U.
        appearance38:Image = CAST(resources:GetObject("appearance38.Image":U), System.Object).
        stateButtonTool3:SharedPropsInternal:AppearancesLarge:Appearance = appearance38.
        appearance39:Image = CAST(resources:GetObject("appearance39.Image":U), System.Object).
        stateButtonTool3:SharedPropsInternal:AppearancesSmall:Appearance = appearance39.
        stateButtonTool3:SharedPropsInternal:Caption = "Pointer":U.
        stateButtonTool6:OptionSetKey = "Pointer":U.
        appearance40:Image = CAST(resources:GetObject("appearance40.Image":U), System.Object).
        stateButtonTool6:SharedPropsInternal:AppearancesLarge:Appearance = appearance40.
        appearance41:Image = CAST(resources:GetObject("appearance41.Image":U), System.Object).
        stateButtonTool6:SharedPropsInternal:AppearancesSmall:Appearance = appearance41.
        stateButtonTool6:SharedPropsInternal:Caption = "Relation":U.
        appearance34:Image = CAST(resources:GetObject("appearance34.Image":U), System.Object).
        buttonTool35:SharedPropsInternal:AppearancesLarge:Appearance = appearance34.
        appearance35:Image = CAST(resources:GetObject("appearance35.Image":U), System.Object).
        buttonTool35:SharedPropsInternal:AppearancesSmall:Appearance = appearance35.
        buttonTool35:SharedPropsInternal:Caption = "Business Entity Tester":U.
        appearance45:Image = CAST(resources:GetObject("appearance45.Image":U), System.Object).
        buttonTool38:SharedPropsInternal:AppearancesLarge:Appearance = appearance45.
        appearance44:Image = CAST(resources:GetObject("appearance44.Image":U), System.Object).
        buttonTool38:SharedPropsInternal:AppearancesSmall:Appearance = appearance44.
        buttonTool38:SharedPropsInternal:Caption = "Print":U.
        appearance47:Image = CAST(resources:GetObject("appearance47.Image":U), System.Object).
        buttonTool41:SharedPropsInternal:AppearancesLarge:Appearance = appearance47.
        appearance48:Image = CAST(resources:GetObject("appearance48.Image":U), System.Object).
        buttonTool41:SharedPropsInternal:AppearancesSmall:Appearance = appearance48.
        buttonTool41:SharedPropsInternal:Caption = "Generate":U.
        buttonTool41:SharedPropsInternal:Priority = 1.
        appearance49:Image = CAST(resources:GetObject("appearance49.Image":U), System.Object).
        buttonTool42:SharedPropsInternal:AppearancesLarge:Appearance = appearance49.
        appearance50:Image = CAST(resources:GetObject("appearance50.Image":U), System.Object).
        buttonTool42:SharedPropsInternal:AppearancesSmall:Appearance = appearance50.
        buttonTool42:SharedPropsInternal:Caption = "Regenerate":U.
        buttonTool42:SharedPropsInternal:Priority = 1.
        appearance51:Image = CAST(resources:GetObject("appearance51.Image":U), System.Object).
        buttonTool44:SharedPropsInternal:AppearancesLarge:Appearance = appearance51.
        appearance52:Image = CAST(resources:GetObject("appearance52.Image":U), System.Object).
        buttonTool44:SharedPropsInternal:AppearancesSmall:Appearance = appearance52.
        buttonTool44:SharedPropsInternal:Caption = "Compile":U.
        buttonTool44:SharedPropsInternal:Priority = 1.
        textBoxTool3:SharedPropsInternal:Caption = "Filter":U.
        appearance56:Image = CAST(resources:GetObject("appearance56.Image":U), System.Object).
        popupMenuTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance56.
        appearance55:Image = CAST(resources:GetObject("appearance55.Image":U), System.Object).
        popupMenuTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance55.
        popupMenuTool2:SharedPropsInternal:Caption = "History":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar18 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar18[1] = buttonTool47.
        popupMenuTool2:Tools:AddRange(arrayvar18).
        appearance54:Image = CAST(resources:GetObject("appearance54.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesLarge:Appearance = appearance54.
        appearance53:Image = CAST(resources:GetObject("appearance53.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesSmall:Appearance = appearance53.
        buttonTool46:SharedPropsInternal:Caption = "Clear":U.
        buttonTool48:SharedPropsInternal:Caption = "No entries":U.
        buttonTool48:SharedPropsInternal:Enabled = FALSE.
        appearance58:Image = CAST(resources:GetObject("appearance58.Image":U), System.Object).
        buttonTool50:SharedPropsInternal:AppearancesLarge:Appearance = appearance58.
        appearance57:Image = CAST(resources:GetObject("appearance57.Image":U), System.Object).
        buttonTool50:SharedPropsInternal:AppearancesSmall:Appearance = appearance57.
        buttonTool50:SharedPropsInternal:Caption = "Options":U.
        buttonTool50:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance64:Image = CAST(resources:GetObject("appearance64.Image":U), System.Object).
        buttonTool52:SharedPropsInternal:AppearancesLarge:Appearance = appearance64.
        appearance63:Image = CAST(resources:GetObject("appearance63.Image":U), System.Object).
        buttonTool52:SharedPropsInternal:AppearancesSmall:Appearance = appearance63.
        buttonTool52:SharedPropsInternal:Caption = "&Save As":U.
        appearance65:Image = CAST(resources:GetObject("appearance65.Image":U), System.Object).
        buttonTool54:SharedPropsInternal:AppearancesLarge:Appearance = appearance65.
        appearance66:Image = CAST(resources:GetObject("appearance66.Image":U), System.Object).
        buttonTool54:SharedPropsInternal:AppearancesSmall:Appearance = appearance66.
        buttonTool54:SharedPropsInternal:Caption = "Refresh from Database":U.
        popupMenuTool3:SharedPropsInternal:Caption = "Modelpopup":U.
        buttonTool116:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool61:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool98:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar19 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 14 NO-UNDO.
        arrayvar19[1] = buttonTool55.
        arrayvar19[2] = buttonTool73.
        arrayvar19[3] = buttonTool58.
        arrayvar19[4] = buttonTool75.
        arrayvar19[5] = buttonTool86.
        arrayvar19[6] = buttonTool102.
        arrayvar19[7] = buttonTool116.
        arrayvar19[8] = buttonTool61.
        arrayvar19[9] = buttonTool100.
        arrayvar19[10] = buttonTool98.
        arrayvar19[11] = buttonTool99.
        arrayvar19[12] = buttonTool101.
        arrayvar19[13] = buttonTool103.
        arrayvar19[14] = buttonTool104.
        popupMenuTool3:Tools:AddRange(arrayvar19).
        appearance67:Image = CAST(resources:GetObject("appearance67.Image":U), System.Object).
        buttonTool57:SharedPropsInternal:AppearancesLarge:Appearance = appearance67.
        appearance68:Image = CAST(resources:GetObject("appearance68.Image":U), System.Object).
        buttonTool57:SharedPropsInternal:AppearancesSmall:Appearance = appearance68.
        buttonTool57:SharedPropsInternal:Caption = "Reorder":U.
        appearance69:Image = CAST(resources:GetObject("appearance69.Image":U), System.Object).
        buttonTool60:SharedPropsInternal:AppearancesLarge:Appearance = appearance69.
        appearance70:Image = CAST(resources:GetObject("appearance70.Image":U), System.Object).
        buttonTool60:SharedPropsInternal:AppearancesSmall:Appearance = appearance70.
        buttonTool60:SharedPropsInternal:Caption = "Relation Fields":U.
        appearance72:Image = CAST(resources:GetObject("appearance72.Image":U), System.Object).
        buttonTool62:SharedPropsInternal:AppearancesLarge:Appearance = appearance72.
        appearance71:Image = CAST(resources:GetObject("appearance71.Image":U), System.Object).
        buttonTool62:SharedPropsInternal:AppearancesSmall:Appearance = appearance71.
        buttonTool62:SharedPropsInternal:Caption = "Remove Relation":U.
        appearance74:Image = CAST(resources:GetObject("appearance74.Image":U), System.Object).
        buttonTool67:SharedPropsInternal:AppearancesLarge:Appearance = appearance74.
        appearance73:Image = CAST(resources:GetObject("appearance73.Image":U), System.Object).
        buttonTool67:SharedPropsInternal:AppearancesSmall:Appearance = appearance73.
        buttonTool67:SharedPropsInternal:Caption = "Import Table":U.
        appearance76:Image = CAST(resources:GetObject("appearance76.Image":U), System.Object).
        buttonTool69:SharedPropsInternal:AppearancesLarge:Appearance = appearance76.
        appearance75:Image = CAST(resources:GetObject("appearance75.Image":U), System.Object).
        buttonTool69:SharedPropsInternal:AppearancesSmall:Appearance = appearance75.
        buttonTool69:SharedPropsInternal:Caption = "Paste ABL Source Code":U.
        appearance78:Image = CAST(resources:GetObject("appearance78.Image":U), System.Object).
        buttonTool71:SharedPropsInternal:AppearancesLarge:Appearance = appearance78.
        appearance79:Image = CAST(resources:GetObject("appearance79.Image":U), System.Object).
        buttonTool71:SharedPropsInternal:AppearancesSmall:Appearance = appearance79.
        buttonTool71:SharedPropsInternal:Caption = "Auto Layout":U.
        appearance81:Image = CAST(resources:GetObject("appearance81.Image":U), System.Object).
        buttonTool72:SharedPropsInternal:AppearancesLarge:Appearance = appearance81.
        appearance80:Image = CAST(resources:GetObject("appearance80.Image":U), System.Object).
        buttonTool72:SharedPropsInternal:AppearancesSmall:Appearance = appearance80.
        buttonTool72:SharedPropsInternal:Caption = "Refresh from Include File":U.
        appearance82:Image = CAST(resources:GetObject("appearance82.Image":U), System.Object).
        buttonTool74:SharedPropsInternal:AppearancesLarge:Appearance = appearance82.
        appearance83:Image = CAST(resources:GetObject("appearance83.Image":U), System.Object).
        buttonTool74:SharedPropsInternal:AppearancesSmall:Appearance = appearance83.
        buttonTool74:SharedPropsInternal:Caption = "Duplicate Table":U.
        appearance85:Image = CAST(resources:GetObject("appearance85.Image":U), System.Object).
        buttonTool77:SharedPropsInternal:AppearancesLarge:Appearance = appearance85.
        appearance84:Image = CAST(resources:GetObject("appearance84.Image":U), System.Object).
        buttonTool77:SharedPropsInternal:AppearancesSmall:Appearance = appearance84.
        buttonTool77:SharedPropsInternal:Caption = "Session Info":U.
        appearance87:Image = CAST(resources:GetObject("appearance87.Image":U), System.Object).
        buttonTool84:SharedPropsInternal:AppearancesLarge:Appearance = appearance87.
        appearance86:Image = CAST(resources:GetObject("appearance86.Image":U), System.Object).
        buttonTool84:SharedPropsInternal:AppearancesSmall:Appearance = appearance86.
        buttonTool84:SharedPropsInternal:Caption = "Edit Source":U.
        buttonTool84:SharedPropsInternal:Priority = 1.
        appearance89:Image = CAST(resources:GetObject("appearance89.Image":U), System.Object).
        buttonTool85:SharedPropsInternal:AppearancesLarge:Appearance = appearance89.
        appearance88:Image = CAST(resources:GetObject("appearance88.Image":U), System.Object).
        buttonTool85:SharedPropsInternal:AppearancesSmall:Appearance = appearance88.
        buttonTool85:SharedPropsInternal:Caption = "Edit Fields":U.
        appearance91:Image = CAST(resources:GetObject("appearance91.Image":U), System.Object).
        buttonTool89:SharedPropsInternal:AppearancesLarge:Appearance = appearance91.
        appearance90:Image = CAST(resources:GetObject("appearance90.Image":U), System.Object).
        buttonTool89:SharedPropsInternal:AppearancesSmall:Appearance = appearance90.
        buttonTool89:SharedPropsInternal:Caption = "Expand all":U.
        appearance93:Image = CAST(resources:GetObject("appearance93.Image":U), System.Object).
        buttonTool90:SharedPropsInternal:AppearancesLarge:Appearance = appearance93.
        appearance92:Image = CAST(resources:GetObject("appearance92.Image":U), System.Object).
        buttonTool90:SharedPropsInternal:AppearancesSmall:Appearance = appearance92.
        buttonTool90:SharedPropsInternal:Caption = "Collapse all":U.
        appearance94:Image = CAST(resources:GetObject("appearance94.Image":U), System.Object).
        buttonTool91:SharedPropsInternal:AppearancesLarge:Appearance = appearance94.
        appearance99:Image = CAST(resources:GetObject("appearance99.Image":U), System.Object).
        buttonTool91:SharedPropsInternal:AppearancesSmall:Appearance = appearance99.
        buttonTool91:SharedPropsInternal:Caption = "Custom Business Entity Properties":U.
        buttonTool91:SharedPropsInternal:Enabled = FALSE.
        appearance95:Image = CAST(resources:GetObject("appearance95.Image":U), System.Object).
        buttonTool92:SharedPropsInternal:AppearancesLarge:Appearance = appearance95.
        appearance100:Image = CAST(resources:GetObject("appearance100.Image":U), System.Object).
        buttonTool92:SharedPropsInternal:AppearancesSmall:Appearance = appearance100.
        buttonTool92:SharedPropsInternal:Caption = "Custom Table Properties":U.
        buttonTool92:SharedPropsInternal:Enabled = FALSE.
        appearance96:Image = CAST(resources:GetObject("appearance96.Image":U), System.Object).
        buttonTool93:SharedPropsInternal:AppearancesLarge:Appearance = appearance96.
        appearance101:Image = CAST(resources:GetObject("appearance101.Image":U), System.Object).
        buttonTool93:SharedPropsInternal:AppearancesSmall:Appearance = appearance101.
        buttonTool93:SharedPropsInternal:Caption = "Custom Relation Properties":U.
        buttonTool93:SharedPropsInternal:Enabled = FALSE.
        appearance97:Image = CAST(resources:GetObject("appearance97.Image":U), System.Object).
        buttonTool94:SharedPropsInternal:AppearancesLarge:Appearance = appearance97.
        appearance102:Image = CAST(resources:GetObject("appearance102.Image":U), System.Object).
        buttonTool94:SharedPropsInternal:AppearancesSmall:Appearance = appearance102.
        buttonTool94:SharedPropsInternal:Caption = "Custom Field Properties":U.
        buttonTool94:SharedPropsInternal:Enabled = FALSE.
        appearance98:Image = CAST(resources:GetObject("appearance98.Image":U), System.Object).
        buttonTool95:SharedPropsInternal:AppearancesLarge:Appearance = appearance98.
        appearance103:Image = CAST(resources:GetObject("appearance103.Image":U), System.Object).
        buttonTool95:SharedPropsInternal:AppearancesSmall:Appearance = appearance103.
        buttonTool95:SharedPropsInternal:Caption = "Custom Index Properties":U.
        buttonTool95:SharedPropsInternal:Enabled = FALSE.
        appearance104:Image = CAST(resources:GetObject("appearance104.Image":U), System.Object).
        buttonTool96:SharedPropsInternal:AppearancesLarge:Appearance = appearance104.
        appearance105:Image = CAST(resources:GetObject("appearance105.Image":U), System.Object).
        buttonTool96:SharedPropsInternal:AppearancesSmall:Appearance = appearance105.
        buttonTool96:SharedPropsInternal:Caption = "Database Connections":U.
        appearance107:Image = CAST(resources:GetObject("appearance107.Image":U), System.Object).
        buttonTool97:SharedPropsInternal:AppearancesLarge:Appearance = appearance107.
        appearance106:Image = CAST(resources:GetObject("appearance106.Image":U), System.Object).
        buttonTool97:SharedPropsInternal:AppearancesSmall:Appearance = appearance106.
        buttonTool97:SharedPropsInternal:Caption = "Split Extend Field into separate Fields":U.
        appearance109:Image = CAST(resources:GetObject("appearance109.Image":U), System.Object).
        buttonTool109:SharedPropsInternal:AppearancesLarge:Appearance = appearance109.
        appearance108:Image = CAST(resources:GetObject("appearance108.Image":U), System.Object).
        buttonTool109:SharedPropsInternal:AppearancesSmall:Appearance = appearance108.
        buttonTool109:SharedPropsInternal:Caption = "Database Login Preferences":U.
        appearance112:Image = CAST(resources:GetObject("appearance112.Image":U), System.Object).
        buttonTool111:SharedPropsInternal:AppearancesLarge:Appearance = appearance112.
        appearance111:Image = CAST(resources:GetObject("appearance111.Image":U), System.Object).
        buttonTool111:SharedPropsInternal:AppearancesSmall:Appearance = appearance111.
        buttonTool111:SharedPropsInternal:Caption = "New Window":U.
        appearance113:Image = CAST(resources:GetObject("appearance113.Image":U), System.Object).
        buttonTool113:SharedPropsInternal:AppearancesSmall:Appearance = appearance113.
        buttonTool113:SharedPropsInternal:Caption = "Copy Field Names":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar20 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 53 NO-UNDO.
        arrayvar20[1] = buttonTool1.
        arrayvar20[2] = buttonTool2.
        arrayvar20[3] = buttonTool3.
        arrayvar20[4] = buttonTool4.
        arrayvar20[5] = buttonTool5.
        arrayvar20[6] = buttonTool6.
        arrayvar20[7] = buttonTool13.
        arrayvar20[8] = buttonTool14.
        arrayvar20[9] = buttonTool15.
        arrayvar20[10] = buttonTool16.
        arrayvar20[11] = buttonTool25.
        arrayvar20[12] = buttonTool29.
        arrayvar20[13] = buttonTool30.
        arrayvar20[14] = buttonTool31.
        arrayvar20[15] = buttonTool33.
        arrayvar20[16] = stateButtonTool3.
        arrayvar20[17] = stateButtonTool6.
        arrayvar20[18] = buttonTool35.
        arrayvar20[19] = buttonTool38.
        arrayvar20[20] = buttonTool41.
        arrayvar20[21] = buttonTool42.
        arrayvar20[22] = buttonTool44.
        arrayvar20[23] = textBoxTool3.
        arrayvar20[24] = popupMenuTool2.
        arrayvar20[25] = buttonTool46.
        arrayvar20[26] = buttonTool48.
        arrayvar20[27] = buttonTool50.
        arrayvar20[28] = buttonTool52.
        arrayvar20[29] = buttonTool54.
        arrayvar20[30] = popupMenuTool3.
        arrayvar20[31] = buttonTool57.
        arrayvar20[32] = buttonTool60.
        arrayvar20[33] = buttonTool62.
        arrayvar20[34] = buttonTool67.
        arrayvar20[35] = buttonTool69.
        arrayvar20[36] = buttonTool71.
        arrayvar20[37] = buttonTool72.
        arrayvar20[38] = buttonTool74.
        arrayvar20[39] = buttonTool77.
        arrayvar20[40] = buttonTool84.
        arrayvar20[41] = buttonTool85.
        arrayvar20[42] = buttonTool89.
        arrayvar20[43] = buttonTool90.
        arrayvar20[44] = buttonTool91.
        arrayvar20[45] = buttonTool92.
        arrayvar20[46] = buttonTool93.
        arrayvar20[47] = buttonTool94.
        arrayvar20[48] = buttonTool95.
        arrayvar20[49] = buttonTool96.
        arrayvar20[50] = buttonTool97.
        arrayvar20[51] = buttonTool109.
        arrayvar20[52] = buttonTool111.
        arrayvar20[53] = buttonTool113.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar20).
        THIS-OBJECT:smartToolbarController1:ToolClick:Subscribe(THIS-OBJECT:smartToolbarController1_ToolClick).
        THIS-OBJECT:smartToolbarController1:ToolKeyPress:Subscribe(THIS-OBJECT:smartToolbarController1_ToolKeyPress).
        /*  */
        /* model1 */
        /*  */
        THIS-OBJECT:model1:AlignGrid = TRUE.
        THIS-OBJECT:model1:AllowDrop = TRUE.
        THIS-OBJECT:model1:AutoScroll = TRUE.
        THIS-OBJECT:model1:AutoScrollMinSize = NEW System.Drawing.Size(5000, 5000).
        THIS-OBJECT:model1:CheckBounds = FALSE.
        THIS-OBJECT:smartToolbarController1:SetContextMenuUltra(THIS-OBJECT:model1, "Modelpopup":U).
        THIS-OBJECT:model1:DiagramSize = NEW System.Drawing.Size(5000, 5000).
        THIS-OBJECT:model1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:model1:DragElement = ?.
        THIS-OBJECT:model1:GridColor = System.Drawing.Color:Silver.
        THIS-OBJECT:model1:GridSize = NEW System.Drawing.Size(20, 20).
        THIS-OBJECT:model1:GridStyle = Crainiate.ERM4.GridStyle:Dot.
        THIS-OBJECT:model1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:model1:Name = "model1":U.
        THIS-OBJECT:model1:PageLineSize = NEW System.Drawing.SizeF(0, 0).
        THIS-OBJECT:model1:Size = NEW System.Drawing.Size(338, 205).
        THIS-OBJECT:model1:TabIndex = 0.
        THIS-OBJECT:model1:WorkspaceColor = System.Drawing.SystemColors:AppWorkspace.
        THIS-OBJECT:model1:Zoom = 100.
        THIS-OBJECT:model1:DragEnter:Subscribe(THIS-OBJECT:model1_DragEnter).
        THIS-OBJECT:model1:ElementInserted:Subscribe(THIS-OBJECT:model1_ElementInserted).
        THIS-OBJECT:model1:ElementRemoved:Subscribe(THIS-OBJECT:model1_ElementRemoved).
        THIS-OBJECT:model1:DragDrop:Subscribe(THIS-OBJECT:model1_DragDrop).
        THIS-OBJECT:model1:DragOver:Subscribe(THIS-OBJECT:model1_DragOver).
        THIS-OBJECT:model1:EnabledChanged:Subscribe(THIS-OBJECT:model1_EnabledChanged).
        THIS-OBJECT:model1:SelectedChanged:Subscribe(THIS-OBJECT:model1_SelectedChanged).
        /*  */
        /* memberTableViewerControl1 */
        /*  */
        THIS-OBJECT:memberTableViewerControl1:FocusControlOnAdd = ?.
        THIS-OBJECT:memberTableViewerControl1:FocusControlOnUpdate = ?.
        THIS-OBJECT:memberTableViewerControl1:LinkDataSource = THIS-OBJECT:tableAdapter.
        THIS-OBJECT:memberTableViewerControl1:LinkGroupAssignSource = ?.
        THIS-OBJECT:memberTableViewerControl1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:memberTableViewerControl1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:memberTableViewerControl1:Name = "memberTableViewerControl1":U.
        THIS-OBJECT:memberTableViewerControl1:Size = NEW System.Drawing.Size(755, 191).
        THIS-OBJECT:memberTableViewerControl1:TabIndex = 13.
        /*  */
        /* tableAdapter */
        /*  */
        THIS-OBJECT:tableAdapter:EntityTable = "eTable":U.
        THIS-OBJECT:tableAdapter:ForeignFields = "BusinessEntityName,BusinessEntityName":U.
        THIS-OBJECT:tableAdapter:LinkDataSource = THIS-OBJECT:smartBusinessEntityAdapter1.
        THIS-OBJECT:tableAdapter:LinkGroupCreateSource = ?.
        THIS-OBJECT:tableAdapter:LinkNavigationSource = ?.
        /*  */
        /* dataRelationViewerControl1 */
        /*  */
        THIS-OBJECT:dataRelationViewerControl1:FocusControlOnAdd = ?.
        THIS-OBJECT:dataRelationViewerControl1:FocusControlOnUpdate = ?.
        THIS-OBJECT:dataRelationViewerControl1:LinkDataSource = THIS-OBJECT:dataRelationAdapter.
        THIS-OBJECT:dataRelationViewerControl1:LinkGroupAssignSource = ?.
        THIS-OBJECT:dataRelationViewerControl1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:dataRelationViewerControl1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:dataRelationViewerControl1:Name = "dataRelationViewerControl1":U.
        THIS-OBJECT:dataRelationViewerControl1:Size = NEW System.Drawing.Size(755, 191).
        THIS-OBJECT:dataRelationViewerControl1:TabIndex = 17.
        /*  */
        /* dataRelationAdapter */
        /*  */
        THIS-OBJECT:dataRelationAdapter:EntityTable = "eDataRelation":U.
        THIS-OBJECT:dataRelationAdapter:ForeignFields = "BusinessEntityName,BusinessEntityName":U.
        THIS-OBJECT:dataRelationAdapter:LinkDataSource = THIS-OBJECT:smartBusinessEntityAdapter1.
        THIS-OBJECT:dataRelationAdapter:LinkGroupCreateSource = ?.
        THIS-OBJECT:dataRelationAdapter:LinkNavigationSource = ?.
        /*  */
        /* fieldViewerControl1 */
        /*  */
        THIS-OBJECT:fieldViewerControl1:FocusControlOnAdd = ?.
        THIS-OBJECT:fieldViewerControl1:FocusControlOnUpdate = ?.
        THIS-OBJECT:fieldViewerControl1:LinkDataSource = THIS-OBJECT:fieldAdapter.
        THIS-OBJECT:fieldViewerControl1:LinkGroupAssignSource = ?.
        THIS-OBJECT:fieldViewerControl1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:fieldViewerControl1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:fieldViewerControl1:Name = "fieldViewerControl1":U.
        THIS-OBJECT:fieldViewerControl1:Size = NEW System.Drawing.Size(755, 191).
        THIS-OBJECT:fieldViewerControl1:TabIndex = 17.
        /*  */
        /* fieldAdapter */
        /*  */
        THIS-OBJECT:fieldAdapter:EntityTable = "eField":U.
        THIS-OBJECT:fieldAdapter:ForeignFields = "TempTableName,TempTableName":U.
        THIS-OBJECT:fieldAdapter:LinkDataSource = THIS-OBJECT:tableAdapter.
        THIS-OBJECT:fieldAdapter:LinkGroupCreateSource = ?.
        THIS-OBJECT:fieldAdapter:LinkNavigationSource = ?.
        THIS-OBJECT:fieldAdapter:QuerySort = "BY eField.FieldOrder":U.
        /*  */
        /* indexViewerControl1 */
        /*  */
        THIS-OBJECT:indexViewerControl1:FocusControlOnAdd = ?.
        THIS-OBJECT:indexViewerControl1:FocusControlOnUpdate = ?.
        THIS-OBJECT:indexViewerControl1:LinkDataSource = THIS-OBJECT:indexAdapter.
        THIS-OBJECT:indexViewerControl1:LinkGroupAssignSource = ?.
        THIS-OBJECT:indexViewerControl1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:indexViewerControl1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:indexViewerControl1:Name = "indexViewerControl1":U.
        THIS-OBJECT:indexViewerControl1:Size = NEW System.Drawing.Size(755, 191).
        THIS-OBJECT:indexViewerControl1:TabIndex = 18.
        /*  */
        /* indexAdapter */
        /*  */
        THIS-OBJECT:indexAdapter:EntityTable = "eIndex":U.
        THIS-OBJECT:indexAdapter:ForeignFields = "TempTableName,TempTableName":U.
        THIS-OBJECT:indexAdapter:LinkDataSource = THIS-OBJECT:tableAdapter.
        THIS-OBJECT:indexAdapter:LinkGroupCreateSource = ?.
        THIS-OBJECT:indexAdapter:LinkNavigationSource = ?.
        THIS-OBJECT:indexAdapter:QuerySort = "BY eIndex.IndexOrder":U.
        /*  */
        /* databaseFieldsControl1 */
        /*  */
        THIS-OBJECT:databaseFieldsControl1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:databaseFieldsControl1:Name = "databaseFieldsControl1":U.
        THIS-OBJECT:databaseFieldsControl1:Size = NEW System.Drawing.Size(104, 181).
        THIS-OBJECT:databaseFieldsControl1:TabIndex = 17.
        /*  */
        /* ultraExplorerBar1 */
        /*  */
        ultraExplorerBarItem1:Checked = TRUE.
        ultraExplorerBarItem1:Key = "table":U.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        ultraExplorerBarItem1:Settings:AppearancesSmall:Appearance = appearance5.
        ultraExplorerBarItem1:Text = "Table":U.
        ultraExplorerBarItem2:Key = "field":U.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        ultraExplorerBarItem2:Settings:AppearancesSmall:Appearance = appearance6.
        ultraExplorerBarItem2:Text = "Field":U.
        ultraExplorerBarItem3:Key = "index":U.
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        ultraExplorerBarItem3:Settings:AppearancesSmall:Appearance = appearance7.
        ultraExplorerBarItem3:Text = "Index":U.
        ultraExplorerBarItem4:Key = "annotation":U.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image":U), System.Object).
        ultraExplorerBarItem4:Settings:AppearancesSmall:Appearance = appearance8.
        ultraExplorerBarItem4:Text = "Annotation":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar21 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem EXTENT 4 NO-UNDO.
        arrayvar21[1] = ultraExplorerBarItem1.
        arrayvar21[2] = ultraExplorerBarItem2.
        arrayvar21[3] = ultraExplorerBarItem3.
        arrayvar21[4] = ultraExplorerBarItem4.
        ultraExplorerBarGroup1:Items:AddRange(arrayvar21).
        ultraExplorerBarGroup1:Text = "Custom":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar22 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup EXTENT 1 NO-UNDO.
        arrayvar22[1] = ultraExplorerBarGroup1.
        THIS-OBJECT:ultraExplorerBar1:Groups:AddRange(arrayvar22).
        THIS-OBJECT:ultraExplorerBar1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:ultraExplorerBar1:Name = "ultraExplorerBar1":U.
        THIS-OBJECT:ultraExplorerBar1:Size = NEW System.Drawing.Size(104, 181).
        THIS-OBJECT:ultraExplorerBar1:Style = Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarStyle:Toolbox.
        THIS-OBJECT:ultraExplorerBar1:TabIndex = 1.
        THIS-OBJECT:ultraExplorerBar1:ViewStyle = Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarViewStyle:Office2007.
        THIS-OBJECT:ultraExplorerBar1:MouseDown:Subscribe(THIS-OBJECT:ultraExplorerBar1_MouseDown).
        THIS-OBJECT:ultraExplorerBar1:ActiveGroupChanged:Subscribe(THIS-OBJECT:ultraExplorerBar1_ActiveGroupChanged).
        /*  */
        /* ultraTree1 */
        /*  */
        THIS-OBJECT:ultraTree1:BorderStyle = Infragistics.Win.UIElementBorderStyle:None.
        THIS-OBJECT:ultraTree1:DisplayStyle = Infragistics.Win.UltraWinTree.UltraTreeDisplayStyle:WindowsVista.
        THIS-OBJECT:ultraTree1:ImageTransparentColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraTree1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:ultraTree1:Name = "ultraTree1":U.
        THIS-OBJECT:ultraTree1:NodeConnectorColor = System.Drawing.SystemColors:ControlDark.
        ultraTreeNode3:Text = "Node2":U.
        ultraTreeNode4:Text = "Node3":U.
        ultraTreeNode5:Text = "Node4":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar23 AS Infragistics.Win.UltraWinTree.UltraTreeNode EXTENT 3 NO-UNDO.
        arrayvar23[1] = ultraTreeNode3.
        arrayvar23[2] = ultraTreeNode4.
        arrayvar23[3] = ultraTreeNode5.
        ultraTreeNode2:Nodes:AddRange(arrayvar23).
        ultraTreeNode2:Text = "Node1":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar24 AS Infragistics.Win.UltraWinTree.UltraTreeNode EXTENT 1 NO-UNDO.
        arrayvar24[1] = ultraTreeNode2.
        ultraTreeNode1:Nodes:AddRange(arrayvar24).
        ultraTreeNode1:Text = "Node0":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar25 AS Infragistics.Win.UltraWinTree.UltraTreeNode EXTENT 1 NO-UNDO.
        arrayvar25[1] = ultraTreeNode1.
        THIS-OBJECT:ultraTree1:Nodes:AddRange(arrayvar25).
        THIS-OBJECT:ultraTree1:Size = NEW System.Drawing.Size(97, 205).
        THIS-OBJECT:ultraTree1:TabIndex = 1.
        THIS-OBJECT:ultraTree1:AfterSelect:Subscribe(THIS-OBJECT:ultraTree1_AfterSelect).
        /*  */
        /* logManagerSettings1 */
        /*  */
        THIS-OBJECT:logManagerSettings1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:logManagerSettings1:Name = "logManagerSettings1":U.
        THIS-OBJECT:logManagerSettings1:Size = NEW System.Drawing.Size(201, 205).
        THIS-OBJECT:logManagerSettings1:TabIndex = 17.
        /*  */
        /* panel1 */
        /*  */
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:model1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(338, 205).
        THIS-OBJECT:panel1:TabIndex = 9.
        /*  */
        /* ultraTrackBar1 */
        /*  */
        appearance22:BackColor = System.Drawing.Color:Transparent.
        appearance22:BackColor2 = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraTrackBar1:Appearance = appearance22.
        THIS-OBJECT:ultraTrackBar1:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraTrackBar1:BackColorInternal = System.Drawing.SystemColors:ActiveCaption.
        THIS-OBJECT:ultraTrackBar1:ButtonStyle = Infragistics.Win.UIElementButtonStyle:Button.
        THIS-OBJECT:ultraTrackBar1:LargeChange = 50.
        THIS-OBJECT:ultraTrackBar1:Location = NEW System.Drawing.Point(599, 3).
        THIS-OBJECT:ultraTrackBar1:MaxValue = 400.
        THIS-OBJECT:ultraTrackBar1:MidpointSettings:Value = 100.
        THIS-OBJECT:ultraTrackBar1:MinValue = 10.
        THIS-OBJECT:ultraTrackBar1:Name = "ultraTrackBar1":U.
        THIS-OBJECT:ultraTrackBar1:Size = NEW System.Drawing.Size(150, 22).
        THIS-OBJECT:ultraTrackBar1:SmallChange = 10.
        THIS-OBJECT:ultraTrackBar1:TabIndex = 1.
        THIS-OBJECT:ultraTrackBar1:Value = 100.
        THIS-OBJECT:ultraTrackBar1:ValueObject = 100.
        THIS-OBJECT:ultraTrackBar1:ViewStyle = Infragistics.Win.UltraWinEditors.TrackBarViewStyle:Office2007.
        THIS-OBJECT:ultraTrackBar1:ValueChanged:Subscribe(THIS-OBJECT:ultraTrackBar1_ValueChanged).
        /*  */
        /* m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 164).
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Left:Name = "m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 461).
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(763, 164).
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Right:Name = "m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 461).
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Top:Name = "m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(771, 164).
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 625).
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Bottom:Name = "m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(771, 0).
        THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* ultraDockManager1 */
        /*  */
        THIS-OBJECT:ultraDockManager1:CompressUnpinnedTabs = FALSE.
        THIS-OBJECT:ultraDockManager1:DefaultPaneSettings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        dockAreaPane1:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:TabGroup.
        dockAreaPane1:DockedBefore = NEW System.Guid("10452164-36b2-4dfd-a7a3-fc98a7f9c05b":U).
        dockAreaPane1:FloatingLocation = NEW System.Drawing.Point(44, 715).
        dockableControlPane1:Control = THIS-OBJECT:businessEntityViewerControl1.
        dockableControlPane1:OriginalControlBounds = NEW System.Drawing.Rectangle(0, 0, 300, 300).
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        dockableControlPane1:Settings:Appearance = appearance2.
        dockableControlPane1:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane1:Text = "Entity Properties":U.
        dockableControlPane2:Control = THIS-OBJECT:memberTableViewerControl1.
        dockableControlPane2:OriginalControlBounds = NEW System.Drawing.Rectangle(0, 0, 300, 300).
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        dockableControlPane2:Settings:Appearance = appearance3.
        dockableControlPane2:Size = NEW System.Drawing.Size(100, 98).
        dockableControlPane2:Text = "Table Properties":U.
        dockableControlPane3:Control = THIS-OBJECT:dataRelationViewerControl1.
        dockableControlPane3:OriginalControlBounds = NEW System.Drawing.Rectangle(0, 0, 514, 353).
        appearance9:Image = CAST(resources:GetObject("appearance9.Image":U), System.Object).
        dockableControlPane3:Settings:Appearance = appearance9.
        dockableControlPane3:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane3:Text = "Data-Relation":U.
        dockableControlPane4:Control = THIS-OBJECT:fieldViewerControl1.
        dockableControlPane4:OriginalControlBounds = NEW System.Drawing.Rectangle(251, 96, 438, 241).
        appearance10:Image = CAST(resources:GetObject("appearance10.Image":U), System.Object).
        dockableControlPane4:Settings:Appearance = appearance10.
        dockableControlPane4:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane4:Text = "Field Properties":U.
        dockableControlPane5:Control = THIS-OBJECT:indexViewerControl1.
        dockableControlPane5:OriginalControlBounds = NEW System.Drawing.Rectangle(48, 96, 388, 238).
        appearance23:Image = CAST(resources:GetObject("appearance23.Image":U), System.Object).
        dockableControlPane5:Settings:Appearance = appearance23.
        dockableControlPane5:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane5:Text = "Index Properties":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar26 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 5 NO-UNDO.
        arrayvar26[1] = dockableControlPane1.
        arrayvar26[2] = dockableControlPane2.
        arrayvar26[3] = dockableControlPane3.
        arrayvar26[4] = dockableControlPane4.
        arrayvar26[5] = dockableControlPane5.
        dockAreaPane1:Panes:AddRange(arrayvar26).
        dockAreaPane1:SelectedTabIndex = 4.
        dockAreaPane1:Size = NEW System.Drawing.Size(755, 233).
        dockAreaPane2:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:VerticalSplit.
        dockAreaPane2:DockedBefore = NEW System.Guid("e7637d20-0648-4b5c-b889-aab80a9a55eb":U).
        dockAreaPane2:FloatingLocation = NEW System.Drawing.Point(462, 527).
        dockableGroupPane1:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:TabGroup.
        dockableControlPane6:Control = THIS-OBJECT:databaseFieldsControl1.
        dockableControlPane6:OriginalControlBounds = NEW System.Drawing.Rectangle(239, 26, 181, 411).
        appearance24:Image = CAST(resources:GetObject("appearance24.Image":U), System.Object).
        dockableControlPane6:Settings:Appearance = appearance24.
        dockableControlPane6:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane6:Text = "Database Fields Toolbox":U.
        dockableControlPane7:Control = THIS-OBJECT:ultraExplorerBar1.
        dockableControlPane7:OriginalControlBounds = NEW System.Drawing.Rectangle(338, 34, 121, 160).
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        dockableControlPane7:Settings:Appearance = appearance4.
        dockableControlPane7:Size = NEW System.Drawing.Size(39, 100).
        dockableControlPane7:Text = "Toolbox":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar27 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 2 NO-UNDO.
        arrayvar27[1] = dockableControlPane6.
        arrayvar27[2] = dockableControlPane7.
        dockableGroupPane1:Panes:AddRange(arrayvar27).
        dockableGroupPane1:SelectedTabIndex = 1.
        dockableGroupPane1:Size = NEW System.Drawing.Size(39, 100).
        dockableControlPane8:Control = THIS-OBJECT:ultraTree1.
        dockableControlPane8:OriginalControlBounds = NEW System.Drawing.Rectangle(149, 3, 136, 164).
        appearance62:Image = CAST(resources:GetObject("appearance62.Image":U), System.Object).
        dockableControlPane8:Settings:Appearance = appearance62.
        dockableControlPane8:Size = NEW System.Drawing.Size(37, 100).
        dockableControlPane8:Text = "Overview":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar28 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 2 NO-UNDO.
        arrayvar28[1] = dockableGroupPane1.
        arrayvar28[2] = dockableControlPane8.
        dockAreaPane2:Panes:AddRange(arrayvar28).
        dockAreaPane2:SelectedTabIndex = 1.
        dockAreaPane2:Size = NEW System.Drawing.Size(206, 223).
        dockAreaPane3:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:VerticalSplit.
        dockAreaPane3:DockedBefore = NEW System.Guid("321a63f8-8b2c-49a1-b3f7-63140a53670d":U).
        dockableControlPane9:Control = THIS-OBJECT:logManagerSettings1.
        dockableControlPane9:OriginalControlBounds = NEW System.Drawing.Rectangle(0, 0, 217, 550).
        appearance33:Image = CAST(resources:GetObject("appearance33.Image":U), System.Object).
        dockableControlPane9:Settings:Appearance = appearance33.
        dockableControlPane9:Size = NEW System.Drawing.Size(277, 100).
        dockableControlPane9:Text = "Logfile Settings":U.
        dockableControlPane10:Control = THIS-OBJECT:panel1.
        dockableControlPane10:OriginalControlBounds = NEW System.Drawing.Rectangle(118, 187, 393, 271).
        dockableControlPane10:Settings:AllowFloating = Infragistics.Win.DefaultableBoolean:False.
        dockableControlPane10:Settings:AllowMinimize = Infragistics.Win.DefaultableBoolean:False.
        dockableControlPane10:Settings:AllowPin = Infragistics.Win.DefaultableBoolean:False.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        dockableControlPane10:Settings:Appearance = appearance1.
        dockableControlPane10:Size = NEW System.Drawing.Size(466, 122).
        dockableControlPane10:Text = "Design":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar29 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 2 NO-UNDO.
        arrayvar29[1] = dockableControlPane9.
        arrayvar29[2] = dockableControlPane10.
        dockAreaPane3:Panes:AddRange(arrayvar29).
        dockAreaPane3:Size = NEW System.Drawing.Size(544, 223).
        dockAreaPane3:UnfilledSize = NEW System.Drawing.Size(544, 100).
        dockAreaPane4:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:TabGroup.
        dockAreaPane4:DockedBefore = NEW System.Guid("1222e1d8-4f6f-4883-9e69-1ae07e805be2":U).
        dockAreaPane4:FloatingLocation = NEW System.Drawing.Point(580, 344).
        dockAreaPane4:Size = NEW System.Drawing.Size(655, 382).
        dockAreaPane5:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:VerticalSplit.
        dockAreaPane5:DockedBefore = NEW System.Guid("46b5c2e7-f1d2-4b8d-af7e-bbb1d40c8bff":U).
        dockAreaPane5:FloatingLocation = NEW System.Drawing.Point(462, 527).
        dockAreaPane5:Size = NEW System.Drawing.Size(755, 382).
        dockAreaPane6:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:TabGroup.
        dockAreaPane6:DockedBefore = NEW System.Guid("9d1b11c0-41b0-479f-bd4d-e10c702ec084":U).
        dockAreaPane6:FloatingLocation = NEW System.Drawing.Point(44, 715).
        dockAreaPane6:Size = NEW System.Drawing.Size(755, 197).
        dockAreaPane7:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:TabGroup.
        dockAreaPane7:DockedBefore = NEW System.Guid("2aafd0f5-54ed-4e13-b29b-7f50640db9d2":U).
        dockAreaPane7:FloatingLocation = NEW System.Drawing.Point(1982, 617).
        dockAreaPane7:Size = NEW System.Drawing.Size(578, 142).
        dockAreaPane8:DockedBefore = NEW System.Guid("a5029409-6cae-4185-a6aa-1c0bc560efe8":U).
        dockAreaPane8:FloatingLocation = NEW System.Drawing.Point(2292, 611).
        dockAreaPane8:Size = NEW System.Drawing.Size(578, 142).
        dockAreaPane8:UnfilledSize = NEW System.Drawing.Size(100, 100).
        dockAreaPane9:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:TabGroup.
        dockAreaPane9:DockedBefore = NEW System.Guid("004864d1-152d-4f88-b595-7f10204aca02":U).
        dockAreaPane9:FloatingLocation = NEW System.Drawing.Point(2355, 615).
        dockAreaPane9:Size = NEW System.Drawing.Size(578, 142).
        dockAreaPane10:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:VerticalSplit.
        dockAreaPane10:DockedBefore = NEW System.Guid("9d6539bc-1849-4822-a623-923cb4508258":U).
        dockAreaPane10:FloatingLocation = NEW System.Drawing.Point(2448, 351).
        dockAreaPane10:Size = NEW System.Drawing.Size(578, 127).
        dockAreaPane10:UnfilledSize = NEW System.Drawing.Size(578, 100).
        dockAreaPane11:DockedBefore = NEW System.Guid("8403b94a-0cb1-48c7-96b1-87cf91e377ef":U).
        dockAreaPane11:FloatingLocation = NEW System.Drawing.Point(1015, 412).
        dockAreaPane11:Size = NEW System.Drawing.Size(544, 123).
        dockAreaPane11:UnfilledSize = NEW System.Drawing.Size(100, 100).
        dockAreaPane12:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:TabGroup.
        dockAreaPane12:FloatingLocation = NEW System.Drawing.Point(422, 324).
        dockAreaPane12:Size = NEW System.Drawing.Size(544, 123).
        dockAreaPane12:UnfilledSize = NEW System.Drawing.Size(100, 100).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar30 AS Infragistics.Win.UltraWinDock.DockAreaPane EXTENT 12 NO-UNDO.
        arrayvar30[1] = dockAreaPane1.
        arrayvar30[2] = dockAreaPane2.
        arrayvar30[3] = dockAreaPane3.
        arrayvar30[4] = dockAreaPane4.
        arrayvar30[5] = dockAreaPane5.
        arrayvar30[6] = dockAreaPane6.
        arrayvar30[7] = dockAreaPane7.
        arrayvar30[8] = dockAreaPane8.
        arrayvar30[9] = dockAreaPane9.
        arrayvar30[10] = dockAreaPane10.
        arrayvar30[11] = dockAreaPane11.
        arrayvar30[12] = dockAreaPane12.
        THIS-OBJECT:ultraDockManager1:DockAreas:AddRange(arrayvar30).
        THIS-OBJECT:ultraDockManager1:DragIndicatorStyle = Infragistics.Win.UltraWinDock.DragIndicatorStyle:VisualStudio2008Vista.
        THIS-OBJECT:ultraDockManager1:DragWindowStyle = Infragistics.Win.UltraWinDock.DragWindowStyle:LayeredWindowWithIndicators.
        THIS-OBJECT:ultraDockManager1:HostControl = THIS-OBJECT.
        THIS-OBJECT:ultraDockManager1:LayoutStyle = Infragistics.Win.UltraWinDock.DockAreaLayoutStyle:FillContainer.
        THIS-OBJECT:ultraDockManager1:WindowStyle = Infragistics.Win.UltraWinDock.WindowStyle:Office2007.
        THIS-OBJECT:ultraDockManager1:PaneActivate:Subscribe(THIS-OBJECT:ultraDockManager1_PaneActivate).
        /*  */
        /* m_BusinessEntityDesignerFormUnpinnedTabAreaLeft */
        /*  */
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaLeft:Dock = System.Windows.Forms.DockStyle:Left.
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaLeft:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaLeft:Location = NEW System.Drawing.Point(8, 164).
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaLeft:Name = "m_BusinessEntityDesignerFormUnpinnedTabAreaLeft":U.
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaLeft:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaLeft:Size = NEW System.Drawing.Size(0, 461).
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaLeft:TabIndex = 4.
        /*  */
        /* m_BusinessEntityDesignerFormUnpinnedTabAreaRight */
        /*  */
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaRight:Dock = System.Windows.Forms.DockStyle:Right.
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaRight:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaRight:Location = NEW System.Drawing.Point(763, 164).
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaRight:Name = "m_BusinessEntityDesignerFormUnpinnedTabAreaRight":U.
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaRight:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaRight:Size = NEW System.Drawing.Size(0, 461).
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaRight:TabIndex = 5.
        /*  */
        /* m_BusinessEntityDesignerFormUnpinnedTabAreaTop */
        /*  */
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaTop:Dock = System.Windows.Forms.DockStyle:Top.
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaTop:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaTop:Location = NEW System.Drawing.Point(8, 164).
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaTop:Name = "m_BusinessEntityDesignerFormUnpinnedTabAreaTop":U.
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaTop:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaTop:Size = NEW System.Drawing.Size(755, 0).
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaTop:TabIndex = 6.
        /*  */
        /* m_BusinessEntityDesignerFormUnpinnedTabAreaBottom */
        /*  */
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaBottom:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaBottom:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaBottom:Location = NEW System.Drawing.Point(8, 625).
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaBottom:Name = "m_BusinessEntityDesignerFormUnpinnedTabAreaBottom":U.
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaBottom:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaBottom:Size = NEW System.Drawing.Size(755, 0).
        THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaBottom:TabIndex = 7.
        /*  */
        /* m_BusinessEntityDesignerFormAutoHideControl */
        /*  */
        THIS-OBJECT:m_BusinessEntityDesignerFormAutoHideControl:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_BusinessEntityDesignerFormAutoHideControl:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_BusinessEntityDesignerFormAutoHideControl:Name = "m_BusinessEntityDesignerFormAutoHideControl":U.
        THIS-OBJECT:m_BusinessEntityDesignerFormAutoHideControl:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_BusinessEntityDesignerFormAutoHideControl:Size = NEW System.Drawing.Size(0, 0).
        THIS-OBJECT:m_BusinessEntityDesignerFormAutoHideControl:TabIndex = 8.
        /*  */
        /* dockableWindow1 */
        /*  */
        THIS-OBJECT:dockableWindow1:Controls:Add(THIS-OBJECT:businessEntityViewerControl1).
        THIS-OBJECT:dockableWindow1:Location = NEW System.Drawing.Point(-10000, 5).
        THIS-OBJECT:dockableWindow1:Name = "dockableWindow1":U.
        THIS-OBJECT:dockableWindow1:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow1:Size = NEW System.Drawing.Size(755, 211).
        THIS-OBJECT:dockableWindow1:TabIndex = 17.
        /*  */
        /* windowDockingArea2 */
        /*  */
        THIS-OBJECT:windowDockingArea2:Controls:Add(THIS-OBJECT:dockableWindow1).
        THIS-OBJECT:windowDockingArea2:Controls:Add(THIS-OBJECT:dockableWindow3).
        THIS-OBJECT:windowDockingArea2:Controls:Add(THIS-OBJECT:dockableWindow5).
        THIS-OBJECT:windowDockingArea2:Controls:Add(THIS-OBJECT:dockableWindow6).
        THIS-OBJECT:windowDockingArea2:Controls:Add(THIS-OBJECT:dockableWindow7).
        THIS-OBJECT:windowDockingArea2:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:windowDockingArea2:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea2:Location = NEW System.Drawing.Point(8, 387).
        THIS-OBJECT:windowDockingArea2:Name = "windowDockingArea2":U.
        THIS-OBJECT:windowDockingArea2:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea2:Size = NEW System.Drawing.Size(755, 238).
        THIS-OBJECT:windowDockingArea2:TabIndex = 12.
        /*  */
        /* dockableWindow3 */
        /*  */
        THIS-OBJECT:dockableWindow3:Controls:Add(THIS-OBJECT:memberTableViewerControl1).
        THIS-OBJECT:dockableWindow3:Location = NEW System.Drawing.Point(-10000, 5).
        THIS-OBJECT:dockableWindow3:Name = "dockableWindow3":U.
        THIS-OBJECT:dockableWindow3:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow3:Size = NEW System.Drawing.Size(755, 211).
        THIS-OBJECT:dockableWindow3:TabIndex = 18.
        /*  */
        /* dockableWindow5 */
        /*  */
        THIS-OBJECT:dockableWindow5:Controls:Add(THIS-OBJECT:dataRelationViewerControl1).
        THIS-OBJECT:dockableWindow5:Location = NEW System.Drawing.Point(-10000, 5).
        THIS-OBJECT:dockableWindow5:Name = "dockableWindow5":U.
        THIS-OBJECT:dockableWindow5:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow5:Size = NEW System.Drawing.Size(755, 211).
        THIS-OBJECT:dockableWindow5:TabIndex = 19.
        /*  */
        /* dockableWindow6 */
        /*  */
        THIS-OBJECT:dockableWindow6:Controls:Add(THIS-OBJECT:fieldViewerControl1).
        THIS-OBJECT:dockableWindow6:Location = NEW System.Drawing.Point(-10000, 5).
        THIS-OBJECT:dockableWindow6:Name = "dockableWindow6":U.
        THIS-OBJECT:dockableWindow6:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow6:Size = NEW System.Drawing.Size(755, 211).
        THIS-OBJECT:dockableWindow6:TabIndex = 20.
        /*  */
        /* dockableWindow7 */
        /*  */
        THIS-OBJECT:dockableWindow7:Controls:Add(THIS-OBJECT:indexViewerControl1).
        THIS-OBJECT:dockableWindow7:Location = NEW System.Drawing.Point(0, 5).
        THIS-OBJECT:dockableWindow7:Name = "dockableWindow7":U.
        THIS-OBJECT:dockableWindow7:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow7:Size = NEW System.Drawing.Size(755, 211).
        THIS-OBJECT:dockableWindow7:TabIndex = 21.
        /*  */
        /* dockableWindow2 */
        /*  */
        THIS-OBJECT:dockableWindow2:Controls:Add(THIS-OBJECT:databaseFieldsControl1).
        THIS-OBJECT:dockableWindow2:Location = NEW System.Drawing.Point(-10000, 0).
        THIS-OBJECT:dockableWindow2:Name = "dockableWindow2":U.
        THIS-OBJECT:dockableWindow2:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow2:Size = NEW System.Drawing.Size(104, 201).
        THIS-OBJECT:dockableWindow2:TabIndex = 22.
        /*  */
        /* dockableWindow4 */
        /*  */
        THIS-OBJECT:dockableWindow4:Controls:Add(THIS-OBJECT:ultraExplorerBar1).
        THIS-OBJECT:dockableWindow4:Location = NEW System.Drawing.Point(5, 0).
        THIS-OBJECT:dockableWindow4:Name = "dockableWindow4":U.
        THIS-OBJECT:dockableWindow4:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow4:Size = NEW System.Drawing.Size(104, 201).
        THIS-OBJECT:dockableWindow4:TabIndex = 23.
        /*  */
        /* windowDockingArea4 */
        /*  */
        THIS-OBJECT:windowDockingArea4:Controls:Add(THIS-OBJECT:dockableWindow2).
        THIS-OBJECT:windowDockingArea4:Controls:Add(THIS-OBJECT:dockableWindow4).
        THIS-OBJECT:windowDockingArea4:Controls:Add(THIS-OBJECT:dockableWindow9).
        THIS-OBJECT:windowDockingArea4:Dock = System.Windows.Forms.DockStyle:Right.
        THIS-OBJECT:windowDockingArea4:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea4:Location = NEW System.Drawing.Point(552, 164).
        THIS-OBJECT:windowDockingArea4:Name = "windowDockingArea4":U.
        THIS-OBJECT:windowDockingArea4:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea4:Size = NEW System.Drawing.Size(211, 223).
        THIS-OBJECT:windowDockingArea4:TabIndex = 0.
        /*  */
        /* dockableWindow9 */
        /*  */
        THIS-OBJECT:dockableWindow9:Controls:Add(THIS-OBJECT:ultraTree1).
        THIS-OBJECT:dockableWindow9:Location = NEW System.Drawing.Point(114, 0).
        THIS-OBJECT:dockableWindow9:Name = "dockableWindow9":U.
        THIS-OBJECT:dockableWindow9:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow9:Size = NEW System.Drawing.Size(97, 223).
        THIS-OBJECT:dockableWindow9:TabIndex = 24.
        /*  */
        /* windowDockingArea5 */
        /*  */
        THIS-OBJECT:windowDockingArea5:Controls:Add(THIS-OBJECT:dockableWindow8).
        THIS-OBJECT:windowDockingArea5:Controls:Add(THIS-OBJECT:dockableWindow10).
        THIS-OBJECT:windowDockingArea5:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:windowDockingArea5:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea5:Location = NEW System.Drawing.Point(8, 164).
        THIS-OBJECT:windowDockingArea5:Name = "windowDockingArea5":U.
        THIS-OBJECT:windowDockingArea5:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea5:Size = NEW System.Drawing.Size(544, 223).
        THIS-OBJECT:windowDockingArea5:TabIndex = 0.
        /*  */
        /* dockableWindow8 */
        /*  */
        THIS-OBJECT:dockableWindow8:Controls:Add(THIS-OBJECT:logManagerSettings1).
        THIS-OBJECT:dockableWindow8:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:dockableWindow8:Name = "dockableWindow8":U.
        THIS-OBJECT:dockableWindow8:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow8:Size = NEW System.Drawing.Size(201, 223).
        THIS-OBJECT:dockableWindow8:TabIndex = 25.
        /*  */
        /* dockableWindow10 */
        /*  */
        THIS-OBJECT:dockableWindow10:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:dockableWindow10:Location = NEW System.Drawing.Point(206, 0).
        THIS-OBJECT:dockableWindow10:Name = "dockableWindow10":U.
        THIS-OBJECT:dockableWindow10:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow10:Size = NEW System.Drawing.Size(338, 223).
        THIS-OBJECT:dockableWindow10:TabIndex = 26.
        /*  */
        /* windowDockingArea6 */
        /*  */
        THIS-OBJECT:windowDockingArea6:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:windowDockingArea6:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea6:Location = NEW System.Drawing.Point(586, 158).
        THIS-OBJECT:windowDockingArea6:Name = "windowDockingArea6":U.
        THIS-OBJECT:windowDockingArea6:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea6:Size = NEW System.Drawing.Size(655, 382).
        THIS-OBJECT:windowDockingArea6:TabIndex = 17.
        /*  */
        /* windowDockingArea7 */
        /*  */
        THIS-OBJECT:windowDockingArea7:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:windowDockingArea7:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea7:Location = NEW System.Drawing.Point(8, 8).
        THIS-OBJECT:windowDockingArea7:Name = "windowDockingArea7":U.
        THIS-OBJECT:windowDockingArea7:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea7:Size = NEW System.Drawing.Size(755, 382).
        THIS-OBJECT:windowDockingArea7:TabIndex = 0.
        /*  */
        /* windowDockingArea3 */
        /*  */
        THIS-OBJECT:windowDockingArea3:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:windowDockingArea3:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea3:Location = NEW System.Drawing.Point(8, 495).
        THIS-OBJECT:windowDockingArea3:Name = "windowDockingArea3":U.
        THIS-OBJECT:windowDockingArea3:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea3:Size = NEW System.Drawing.Size(755, 197).
        THIS-OBJECT:windowDockingArea3:TabIndex = 18.
        /*  */
        /* timer1 */
        /*  */
        THIS-OBJECT:timer1:Interval = 5.
        THIS-OBJECT:timer1:Tick:Subscribe(THIS-OBJECT:timer1_Tick).
        /*  */
        /* smartBusinessEntityAdapter2 */
        /*  */
        THIS-OBJECT:smartBusinessEntityAdapter2:DatasetControllerType = "Consultingwerk.BusinessEntityDesigner.Services.DatabaseSchemaDatasetController":U.
        THIS-OBJECT:smartBusinessEntityAdapter2:EntityJoin = "YES":U.
        THIS-OBJECT:smartBusinessEntityAdapter2:EntityName = "Consultingwerk.BusinessEntityDesigner.Services.DatabaseSchemaBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityAdapter2:EntityTable = "eDatabase":U.
        THIS-OBJECT:smartBusinessEntityAdapter2:EntityView = "eTable":U.
        THIS-OBJECT:smartBusinessEntityAdapter2:LinkCommitSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter2:LinkDataSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter2:LinkNavigationSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter2:StopAfter = 0.
        THIS-OBJECT:smartBusinessEntityAdapter2:ThrowErrorsFromRetrieveData = TRUE.
        /*  */
        /* windowDockingArea1 */
        /*  */
        THIS-OBJECT:windowDockingArea1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:windowDockingArea1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea1:Location = NEW System.Drawing.Point(8, 8).
        THIS-OBJECT:windowDockingArea1:Name = "windowDockingArea1":U.
        THIS-OBJECT:windowDockingArea1:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea1:Size = NEW System.Drawing.Size(578, 142).
        THIS-OBJECT:windowDockingArea1:TabIndex = 0.
        /*  */
        /* saveFileDialog1 */
        /*  */
        THIS-OBJECT:saveFileDialog1:DefaultExt = "bedgm":U.
        THIS-OBJECT:saveFileDialog1:Filter = "Business Entity Design (*.bedgm)|*.bedgm|All files (*.*)|*.*":U.
        THIS-OBJECT:saveFileDialog1:RestoreDirectory = TRUE.
        THIS-OBJECT:saveFileDialog1:Title = "Save As":U.
        /*  */
        /* openFileDialog1 */
        /*  */
        THIS-OBJECT:openFileDialog1:DefaultExt = "bedgm":U.
        THIS-OBJECT:openFileDialog1:Filter = "Business Entity Design (*.bedgm)|*.bedgm|All files (*.*)|*.*":U.
        THIS-OBJECT:openFileDialog1:RestoreDirectory = TRUE.
        /*  */
        /* windowDockingArea9 */
        /*  */
        THIS-OBJECT:windowDockingArea9:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:windowDockingArea9:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea9:Location = NEW System.Drawing.Point(8, 8).
        THIS-OBJECT:windowDockingArea9:Name = "windowDockingArea9":U.
        THIS-OBJECT:windowDockingArea9:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea9:Size = NEW System.Drawing.Size(578, 142).
        THIS-OBJECT:windowDockingArea9:TabIndex = 0.
        /*  */
        /* windowDockingArea8 */
        /*  */
        THIS-OBJECT:windowDockingArea8:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:windowDockingArea8:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea8:Location = NEW System.Drawing.Point(8, 8).
        THIS-OBJECT:windowDockingArea8:Name = "windowDockingArea8":U.
        THIS-OBJECT:windowDockingArea8:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea8:Size = NEW System.Drawing.Size(578, 142).
        THIS-OBJECT:windowDockingArea8:TabIndex = 0.
        /*  */
        /* ultraStatusBar1 */
        /*  */
        THIS-OBJECT:ultraStatusBar1:Controls:Add(THIS-OBJECT:ultraTrackBar1).
        THIS-OBJECT:ultraStatusBar1:Location = NEW System.Drawing.Point(0, 625).
        THIS-OBJECT:ultraStatusBar1:Name = "ultraStatusBar1":U.
        ultraStatusPanel1:Key = "modified":U.
        ultraStatusPanel2:Key = "status":U.
        ultraStatusPanel2:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Spring.
        ultraStatusPanel2:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel3:Key = "workdir":U.
        ultraStatusPanel3:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel3:Style = Infragistics.Win.UltraWinStatusBar.PanelStyle:Button.
        ultraStatusPanel3:Text = "workdir":U.
        ultraStatusPanel3:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel4:Key = "username":U.
        ultraStatusPanel4:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel4:Text = "user name":U.
        ultraStatusPanel4:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel5:Key = "frameworkversion":U.
        ultraStatusPanel5:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel5:Text = "frameworkversion":U.
        ultraStatusPanel5:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel6:Key = "proversion":U.
        ultraStatusPanel6:Text = "proversion":U.
        ultraStatusPanel6:Width = 40.
        ultraStatusPanel6:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel7:Key = "bitness":U.
        ultraStatusPanel7:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel7:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel8:Key = "Zoom":U.
        ultraStatusPanel8:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel8:Text = "100 %":U.
        ultraStatusPanel8:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel9:Control = THIS-OBJECT:ultraTrackBar1.
        ultraStatusPanel9:Style = Infragistics.Win.UltraWinStatusBar.PanelStyle:ControlContainer.
        ultraStatusPanel9:Width = 150.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar31 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel EXTENT 9 NO-UNDO.
        arrayvar31[1] = ultraStatusPanel1.
        arrayvar31[2] = ultraStatusPanel2.
        arrayvar31[3] = ultraStatusPanel3.
        arrayvar31[4] = ultraStatusPanel4.
        arrayvar31[5] = ultraStatusPanel5.
        arrayvar31[6] = ultraStatusPanel6.
        arrayvar31[7] = ultraStatusPanel7.
        arrayvar31[8] = ultraStatusPanel8.
        arrayvar31[9] = ultraStatusPanel9.
        THIS-OBJECT:ultraStatusBar1:Panels:AddRange(arrayvar31).
        THIS-OBJECT:ultraStatusBar1:Size = NEW System.Drawing.Size(771, 23).
        THIS-OBJECT:ultraStatusBar1:TabIndex = 1.
        THIS-OBJECT:ultraStatusBar1:Text = "ultraStatusBar1":U.
        THIS-OBJECT:ultraStatusBar1:ViewStyle = Infragistics.Win.UltraWinStatusBar.ViewStyle:Office2007.
        THIS-OBJECT:ultraStatusBar1:ButtonClick:Subscribe(THIS-OBJECT:ultraStatusBar1_ButtonClick).
        /*  */
        /* windowDockingArea11 */
        /*  */
        THIS-OBJECT:windowDockingArea11:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:windowDockingArea11:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea11:Location = NEW System.Drawing.Point(8, 8).
        THIS-OBJECT:windowDockingArea11:Name = "windowDockingArea11":U.
        THIS-OBJECT:windowDockingArea11:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea11:Size = NEW System.Drawing.Size(578, 127).
        THIS-OBJECT:windowDockingArea11:TabIndex = 0.
        /*  */
        /* ultraPrintPreviewDialog1 */
        /*  */
        THIS-OBJECT:ultraPrintPreviewDialog1:Name = "ultraPrintPreviewDialog1":U.
        THIS-OBJECT:ultraPrintPreviewDialog1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2007.
        THIS-OBJECT:ultraPrintPreviewDialog1:UseAntiAlias = TRUE.
        /*  */
        /* openFileDialog2 */
        /*  */
        THIS-OBJECT:openFileDialog2:DefaultExt = "i":U.
        THIS-OBJECT:openFileDialog2:Filter = "Include Files|*.i|Procedure Files|*.p|AppBuilder Source|*.w|Class Files|*.cls|All Files|*.*":U.
        THIS-OBJECT:openFileDialog2:RestoreDirectory = TRUE.
        THIS-OBJECT:openFileDialog2:Title = "Import ABL Source Code":U.
        /*  */
        /* windowDockingArea10 */
        /*  */
        THIS-OBJECT:windowDockingArea10:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:windowDockingArea10:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea10:Location = NEW System.Drawing.Point(8, 8).
        THIS-OBJECT:windowDockingArea10:Name = "windowDockingArea10":U.
        THIS-OBJECT:windowDockingArea10:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea10:Size = NEW System.Drawing.Size(544, 123).
        THIS-OBJECT:windowDockingArea10:TabIndex = 0.
        /*  */
        /* searchTimer */
        /*  */
        THIS-OBJECT:searchTimer:Interval = 750.
        THIS-OBJECT:searchTimer:Tick:Subscribe(THIS-OBJECT:searchTimer_Tick).
        /*  */
        /* windowDockingArea13 */
        /*  */
        THIS-OBJECT:windowDockingArea13:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:windowDockingArea13:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea13:Location = NEW System.Drawing.Point(8, 8).
        THIS-OBJECT:windowDockingArea13:Name = "windowDockingArea13":U.
        THIS-OBJECT:windowDockingArea13:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea13:Size = NEW System.Drawing.Size(544, 123).
        THIS-OBJECT:windowDockingArea13:TabIndex = 0.
        /*  */
        /* BusinessEntityDesignerForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(771, 648).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityDesignerFormAutoHideControl).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea5).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea4).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaTop).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaBottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaLeft).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityDesignerFormUnpinnedTabAreaRight).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityDesignerForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraStatusBar1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "BusinessEntityDesignerForm":U.
        THIS-OBJECT:Text = "Business Entity Designer":U.
        THIS-OBJECT:WindowPositionRegistryKey = "BusinessEntityDesignerForm":U.
        CAST(THIS-OBJECT:businessEntityViewerControl1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:memberTableViewerControl1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tableAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:dataRelationViewerControl1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:dataRelationAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:fieldViewerControl1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:fieldAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:indexViewerControl1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:indexAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraExplorerBar1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraTrackBar1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:dockableWindow1:ResumeLayout(FALSE).
        THIS-OBJECT:windowDockingArea2:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow3:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow5:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow6:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow7:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow2:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow4:ResumeLayout(FALSE).
        THIS-OBJECT:windowDockingArea4:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow9:ResumeLayout(FALSE).
        THIS-OBJECT:windowDockingArea5:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow8:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow10:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartBusinessEntityAdapter2, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraStatusBar1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraStatusBar1:PerformLayout().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Plugins for the Business Entity Designer
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializePlugins ():

        DEFINE VARIABLE i       AS INTEGER                       NO-UNDO.
        DEFINE VARIABLE cPlugin AS CHARACTER                     NO-UNDO.
        DEFINE VARIABLE oPlugin AS IBusinessEntityDesignerPlugin NO-UNDO .
        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.

        ASSIGN hDataset = oController:DatasetHandle .

        pluginloop:
        DO i = 1 TO NUM-ENTRIES (BusinessEntityDesignerSettings:BusinessEntityDesignerPlugins, "|":U):

            ASSIGN cPlugin = TRIM (ENTRY (i, BusinessEntityDesignerSettings:BusinessEntityDesignerPlugins, "|":U)) .

            IF cPlugin > "":U AND NOT cPlugin BEGINS "#":U THEN DO ON ERROR UNDO, NEXT pluginloop .

                oPlugin = DYNAMIC-NEW (cPlugin) () .

                oPlugin:Startup (DATASET-HANDLE hDataset BIND,
                                 oController,
                                 THIS-OBJECT,
                                 THIS-OBJECT:smartToolbarController1) .

                CREATE ttPlugins .
                ASSIGN ttPlugins.cPluginName = cPlugin
                       ttPlugins.oPlugin     = oPlugin .
                RELEASE ttPlugins .

                CATCH err AS Progress.Lang.Error :
                    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err,
                                                                      SUBSTITUTE ("Error loading plugin: &1"{&TRAN},
                                                                                  cPlugin)) .
                    IF VALID-OBJECT (oPlugin) THEN DO:
                        oPlugin:Shutdown() .
                        IF VALID-OBJECT (oPlugin) THEN
                            DELETE OBJECT oPlugin .
                    END.
                END CATCH.
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Loads the .bedgm file LRU from the Registry
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID LoadFileLRU ():

        DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cFileName    AS CHARACTER NO-UNDO.

        DEFINE VARIABLE cSettingsKey AS CHARACTER NO-UNDO.

        ASSIGN lUseLocalFileHistory = BusinessEntityDesignerSettings:UseLocalFileHistory .

        IF lUseLocalFileHistory THEN
            ASSIGN cSettingsKey = Consultingwerk.Framework.FrameworkSettings:BaseRegistryKey + "Settings~\":U +
                                  REPLACE (BusinessEntityDesignerSettings:SettingsFile, ":":U, "":U).
        ELSE
            ASSIGN cSettingsKey = Consultingwerk.Framework.FrameworkSettings:BaseRegistryKey + "Settings":U .

        DO i = 12 TO 0 BY -1 ON ERROR UNDO, THROW:
            cFileName = Consultingwerk.Framework.Registry:GetRegistryValue
                           ("User":U,
                            cSettingsKey,
                            "File LRU ":U + STRING (i, "99":U)) .

            IF cFileName > "":U THEN
                UpdateFileLRU (cFileName) .

            @SuppressUnusedWarnings.
            CATCH err AS Progress.Lang.Error :
                /* noop */
            END CATCH.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Loads the table filter LRU from the registry
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID LoadLRU ():

        DEFINE VARIABLE cSettings AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i         AS INTEGER   NO-UNDO.

        ASSIGN cSettings = Consultingwerk.Framework.Registry:GetRegistryValue
                                   ("User":U,
                                    Consultingwerk.Framework.FrameworkSettings:BaseRegistryKey + "Settings":U,
                                    "FilterHistory":U) .

        oLRUList:Clear() .

        DO i = 1 TO NUM-ENTRIES (cSettings, "|":U):
            oLRUList:Add (ENTRY (i, cSettings, "|":U)) .
        END .

        BuildLRUMenuItems() .

        @SuppressUnusedWarnings.
        CATCH err AS System.Exception:
            /* ignore */
        END CATCH.

    END METHOD.

    /**
     * Purpose: Handles changing the DisplayReleaseNotes toggle
     * Notes:
     */
    METHOD PROTECTED VOID ToggleDisplayReleaseNotes():

        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .
        DEFINE VARIABLE lSetting         AS LOGICAL          NO-UNDO .

        IF lApplyingDisplayReleaseNotesAtStart THEN
            RETURN .

        ASSIGN lSetting = UltraToolbarsHelper:GetStateButtonToolChecked (smartToolbarController1,
                                                                         "Display Release Notes At Start":U) .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        IF VALID-OBJECT (oSettingsService) THEN
            oSettingsService:StoreSetting(THIS-OBJECT:WindowPositionRegistryKey,
                                          "Display Release Notes At Start":U,
                                          STRING (lSetting)) .

        MessageFormHelper:ShowMessage ("This setting will be applied the next time you start the Business Entity Designer"{&TRAN}, 10) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Title that should be shown in the title bar Business Entity Designer Form
        Notes:
        @param pcFileName The currently open file name or ? for "untitled"
        @return The Title that should be shown in the title bar
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER WindowTitle (pcFileName AS CHARACTER):

        DEFINE VARIABLE cShortFileName AS CHARACTER NO-UNDO.

        IF pcFileName <> ? THEN DO:
            ASSIGN cShortFileName = FileHelper:ShortFileName(pcFileName)
                   cShortFileName = REPLACE(cShortFileName, ".bedgm":U, "":U) .

            RETURN SUBSTITUTE ("&1 - &2 (&3)":U,
                               cShortFileName,
                               cBaseTitle,
                               pcFileName) .
        END.
        ELSE
            RETURN SUBSTITUTE ("<untitled> - &1"{&TRAN}, cBaseTitle) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DragDrop event of the crainiate model1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The DragEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID model1_DragDrop (sender AS System.Object,
                                         e AS System.Windows.Forms.DragEventArgs):

        DEFINE VARIABLE oPoint         AS System.Drawing.Point                NO-UNDO .
        DEFINE VARIABLE oPointF        AS System.Drawing.PointF               NO-UNDO .
        DEFINE VARIABLE oElement       AS Crainiate.ERM4.Element              NO-UNDO .
        DEFINE VARIABLE hDataset       AS HANDLE                              NO-UNDO .
        DEFINE VARIABLE oItem          AS Infragistics.Shared.IKeyedSubObject NO-UNDO .
        DEFINE VARIABLE oArray         AS System.Array                        NO-UNDO .
        DEFINE VARIABLE cFileName      AS CHARACTER                           NO-UNDO .
        DEFINE VARIABLE oDialogResult  AS DialogResultEnum                    NO-UNDO .
        DEFINE VARIABLE oDialogResult2 AS DialogResultEnum                    NO-UNDO .
        DEFINE VARIABLE cTag           AS CHARACTER                           NO-UNDO .

        IF e:Data:GetDataPresent (System.Windows.Forms.DataFormats:FileDrop) THEN DO:
            oArray = CAST (e:Data:GetData(System.Windows.Forms.DataFormats:FileDrop),
                           System.Array) .

            IF VALID-OBJECT (oArray) THEN DO:
                cFileName = oArray:GetValue (0):ToString () .

                IF cFileName MATCHES "*.bedgm":U THEN DO:
                    IF CheckChanges () = FALSE THEN
                        RETURN .

                    CAST (smartBusinessEntityAdapter1:DatasetController,
                          BusinessEntityDatasetController):Open (cFileName, THIS-OBJECT:model1) .

                    ASSIGN THIS-OBJECT:Text     = THIS-OBJECT:WindowTitle (cFileName)
                           THIS-OBJECT:FileName = cFileName .
                END.
                ELSE IF cFileName MATCHES "*.i":U THEN
                    oController:ImportTableFromFile (cFileName) .

                THIS-OBJECT:Activate () .
            END .

            RETURN .
        END.

        IF NOT e:Data:GetDataPresent (Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem":U)) AND
           NOT e:Data:GetDataPresent (Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinListView.UltraListViewItem":U)) THEN

            RETURN .

        IF e:Data:GetDataPresent (Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem":U)) THEN
            oItem = CAST (e:Data:GetData (Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem":U)),
                          Infragistics.Shared.IKeyedSubObject) .
        ELSE
            oItem = CAST (e:Data:GetData (Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinListView.UltraListViewItem":U)),
                          Infragistics.Shared.IKeyedSubObject) .

        oPoint = model1:PointToClient(NEW System.Drawing.Point (e:X, e:Y))  .
        oPointF = model1:PointToDiagram(oPoint:X, oPoint:Y) .
        oElement = model1:ElementFromPoint (oPoint:X, oPoint:Y) .

        IF oItem:Key BEGINS "table.":U THEN DO:
            IF VALID-OBJECT (oElement) AND TYPE-OF (oElement, TempTableShape) THEN DO:
                /* Mike Fechner, Consultingwerk Ltd. 16.12.2010
                   Adding another table to an existing temp-table */

                oDialogResult = Consultingwerk.Util.MessageFormHelper:AskQuestion
                                    (SUBSTITUTE ("Add &1.&2 as an additional source table to the temp-table &3?"{&TRAN},
                                                 ENTRY (2, oItem:Key, ".":U),
                                                 ENTRY (3, oItem:Key, ".":U),
                                                 CAST (oElement, TempTableShape):Heading),
                                     "Business Entity Designer"{&TRAN},
                                     SUBSTITUTE ("The fields of the database table &1.&2 will be added to the temp-table &3."{&TRAN},
                                                 ENTRY (2, oItem:Key, ".":U),
                                                 ENTRY (3, oItem:Key, ".":U),
                                                 CAST (oElement, TempTableShape):Heading)) .

                IF NOT oDialogResult:Equals (DialogResultEnum:DialogResultYes) THEN
                    RETURN .

                hDataset = CAST (smartBusinessEntityAdapter2:DatasetController,
                                 DatabaseSchemaDatasetController):GetTableSchema (ENTRY (2, oItem:Key, ".":U), ENTRY (3, oItem:Key, ".":U)) .

                CAST (smartBusinessEntityAdapter1:DatasetController,
                      BusinessEntityDatasetController):AddSourceTable (hDataset,
                                                                       oElement:Key,
                                                                       THIS-OBJECT:model1) .

                DELETE OBJECT hDataset .
            END.
            ELSE DO:
                hDataset = CAST (smartBusinessEntityAdapter2:DatasetController,
                                 DatabaseSchemaDatasetController):GetTableSchema (ENTRY (2, oItem:Key, ".":U), ENTRY (3, oItem:Key, ".":U)) .

                CAST (smartBusinessEntityAdapter1:DatasetController,
                      BusinessEntityDatasetController):CreateTempTable (hDataset,
                                                                        THIS-OBJECT:model1,
                                                                        INTEGER(oPointF:X - 30),
                                                                        INTEGER(oPointF:Y - 10)) .

                DELETE OBJECT hDataset .
            END.
        END.
        ELSE CASE oItem:Key:
            WHEN "table":U THEN DO:
                CAST (smartBusinessEntityAdapter1:DatasetController,
                      BusinessEntityDatasetController):CreateTempTable (THIS-OBJECT:model1,
                                                                        INTEGER(oPointF:X - 30),
                                                                        INTEGER(oPointF:Y - 10)) .
            END.
            WHEN "annotation":U THEN DO:

            END.
            WHEN "field":U THEN DO:
                IF VALID-OBJECT (oElement) AND TYPE-OF (oElement, TempTableShape) THEN
                    CAST (smartBusinessEntityAdapter1:DatasetController,
                          BusinessEntityDatasetController):AddNewField (CAST (oElement, TempTableShape)) .
            END.
            WHEN "index":U THEN DO:
                IF VALID-OBJECT (oElement) AND TYPE-OF (oElement, TempTableShape) THEN
                    CAST (smartBusinessEntityAdapter1:DatasetController,
                          BusinessEntityDatasetController):AddNewIndex  (CAST (oElement, TempTableShape)) .
            END.
        END CASE .

        IF oItem:Key BEGINS "field.":U THEN DO:

            oDialogResult = Consultingwerk.Util.MessageFormHelper:AskQuestion
                                ("Keep source field information for the new field?"{&TRAN},
                                 "Business Entity Designer"{&TRAN},
                                 "When keeping the source field information, the new temp-table field will "{&TRAN} +
                                 "be populated from the database using a data-source and not from a calculated "{&TRAN} +
                                 "field expression.~n~n"{&TRAN} +
                                 "In that case, it's expected to be part of the data source query."{&TRAN}) .

            IF oDialogResult:Equals (DialogResultEnum:DialogResultYes) THEN DO:

               IF NOT CAST (smartBusinessEntityAdapter1:DatasetController,
                            BusinessEntityDatasetController):HasSourceTable(CAST (oElement, TempTableShape), ENTRY (3, oItem:Key, ".":U)) THEN DO:

                    oDialogResult2 = Consultingwerk.Util.MessageFormHelper:AskQuestion
                                         (SUBSTITUTE ("Add &1 as an additional source table?"{&TRAN}, ENTRY (3, oItem:KEY, ".":U)),
                                          "Business Entity Designer"{&TRAN}) .
                END.
                ELSE
                    ASSIGN oDialogResult2 = DialogResultEnum:DialogResultNo .
            END.
            ELSE
                ASSIGN oDialogResult2 = DialogResultEnum:DialogResultNo .

            IF VALID-OBJECT (oElement) AND TYPE-OF (oElement, TempTableShape) THEN DO:
                ASSIGN cTag = UNBOX (Consultingwerk.Util.ReflectionHelper:GetPropertyValue (oItem, "Tag":U)) .

                CAST (smartBusinessEntityAdapter1:DatasetController,
                      BusinessEntityDatasetController):AddExistingField (CAST (oElement, TempTableShape),
                                                                         SUBSTRING (oItem:Key, 7, -1, "CHARACTER":U),
                                                                         cTag,
                                                                         oDialogResult:Equals (DialogResultEnum:DialogResultYes),
                                                                         oDialogResult2:Equals (DialogResultEnum:DialogResultYes)) .
            END.
        END.

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hDataset) .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DragEnter event of the crainiate model1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The DragEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID model1_DragEnter (sender AS System.Object,
                                          e AS System.Windows.Forms.DragEventArgs):

        DEFINE VARIABLE oArray    AS System.Array           NO-UNDO .
        DEFINE VARIABLE cFileName AS CHARACTER              NO-UNDO .

        IF e:Data:GetDataPresent (System.Windows.Forms.DataFormats:FileDrop) THEN DO:
            oArray = CAST (e:Data:GetData(System.Windows.Forms.DataFormats:FileDrop),
                           System.Array) .

            IF VALID-OBJECT (oArray) THEN DO:
                cFileName = oArray:GetValue (0):ToString () .

                IF cFileName MATCHES "*.bedgm":U OR cFileName MATCHES "*.i":U THEN
                    e:Effect = System.Windows.Forms.DragDropEffects:Copy .
                ELSE
                    e:Effect = System.Windows.Forms.DragDropEffects:None .

            END.
            ELSE
                e:Effect = System.Windows.Forms.DragDropEffects:None .

            RETURN .
        END.

        IF NOT e:Data:GetDataPresent (Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem":U)) AND
           NOT e:Data:GetDataPresent (Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinListView.UltraListViewItem":U)) THEN DO:

            e:Effect = System.Windows.Forms.DragDropEffects:None .
            RETURN .
        END.

        e:Effect = System.Windows.Forms.DragDropEffects:Copy .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DragOver event of the crainiate model1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The DragEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID model1_DragOver (sender AS System.Object,
                                         e AS System.Windows.Forms.DragEventArgs):

        DEFINE VARIABLE oPoint   AS System.Drawing.Point                NO-UNDO .
        DEFINE VARIABLE oElement AS Crainiate.ERM4.Element              NO-UNDO .
        DEFINE VARIABLE oItem    AS Infragistics.Shared.IKeyedSubObject NO-UNDO .

        IF NOT e:Data:GetDataPresent (Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem":U)) AND
           NOT e:Data:GetDataPresent (Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinListView.UltraListViewItem":U)) THEN

            RETURN .

        IF e:Data:GetDataPresent (Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem":U)) THEN
            oItem = CAST (e:Data:GetData (Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem":U)),
                          Infragistics.Shared.IKeyedSubObject) .
        ELSE
            oItem = CAST (e:Data:GetData (Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinListView.UltraListViewItem":U)),
                          Infragistics.Shared.IKeyedSubObject) .

        oPoint = model1:PointToClient(NEW System.Drawing.Point (e:X, e:Y))  .
        oElement = model1:ElementFromPoint (oPoint:X, oPoint:Y) .

        IF oItem:Key BEGINS "table.":U THEN DO:
            IF VALID-OBJECT (oElement) AND TYPE-OF (oElement, TempTableShape) THEN
                e:Effect = System.Windows.Forms.DragDropEffects:Link .
            ELSE
                e:Effect = System.Windows.Forms.DragDropEffects:Copy .
        END.
        ELSE IF oItem:Key = "table":U OR oItem:Key = "annotation":U THEN DO:
            IF VALID-OBJECT (oElement) AND TYPE-OF (oElement, TempTableShape) THEN
                e:Effect = System.Windows.Forms.DragDropEffects:None .
            ELSE
                e:Effect = System.Windows.Forms.DragDropEffects:Copy .
        END .
        ELSE IF oItem:Key = "field":U OR oItem:Key = "Index":U THEN DO:
            IF VALID-OBJECT (oElement) AND TYPE-OF (oElement, TempTableShape) THEN
                e:Effect = System.Windows.Forms.DragDropEffects:Copy .
            ELSE
                e:Effect = System.Windows.Forms.DragDropEffects:None .
        END.
        ELSE IF oItem:Key BEGINS "field.":U THEN DO:
            IF VALID-OBJECT (oElement) AND TYPE-OF (oElement, TempTableShape) THEN
                e:Effect = System.Windows.Forms.DragDropEffects:Copy .
            ELSE
                e:Effect = System.Windows.Forms.DragDropEffects:None .
        END.

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ElementInserted event of the crainiate model1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ElementsEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID model1_ElementInserted (sender AS System.Object,
                                                e AS Crainiate.ERM4.ElementsEventArgs):

        IF Progress.Util.EnumHelper:AreEqual (THIS-OBJECT:model1:Runtime:InteractiveMode,
                                              Crainiate.ERM4.InteractiveMode:AddConnector) AND
           TYPE-OF (e:Value, Crainiate.ERM4.Connector) THEN DO:

            /* Mike Fechner, Consultingwerk Ltd. 14.10.2010
               Remove Connectors with no Shape at the start */
            IF NOT VALID-OBJECT (CAST(e:Value, Crainiate.ERM4.Connector):Start:Shape) THEN DO:
                THIS-OBJECT:model1:Lines:Remove(CAST (e:Value, Crainiate.ERM4.Connector):Key) .

                DELETE OBJECT e:Value.
                RETURN .
            END.

            /* Activate Timer */
            oNewConnector = CAST (e:Value, Crainiate.ERM4.Connector) .

            timer1:Enabled = TRUE .

            CAST (e:Value, Crainiate.ERM4.Connector):Start:AllowMove = FALSE .

            ActivateDrawPointer () .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ElementRemoved event of the crainiate model1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ElementsEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID model1_ElementRemoved (sender AS System.Object,
                                               e AS Crainiate.ERM4.ElementsEventArgs):

        DEFINE VARIABLE oRemoveTable AS TempTableShape NO-UNDO .

        IF lRemovingElements THEN
            RETURN .

        ELSE DO ON ERROR UNDO, THROW:
            ASSIGN lRemovingElements = TRUE .

            IF TYPE-OF (e:Value, TempTableShape) THEN DO:

                ASSIGN oRemoveTable = CAST (e:Value, TempTableShape) .

                CAST (smartBusinessEntityAdapter1:DatasetController,
                      BusinessEntityDatasetController):RemoveTable (THIS-OBJECT:model1,
                                                                    oRemoveTable) .
            END.
            ELSE IF TYPE-OF (e:Value, DataRelationConnector) THEN DO:
                PROCESS EVENTS .

                CAST (smartBusinessEntityAdapter1:DatasetController,
                      BusinessEntityDatasetController):RemoveRelation (THIS-OBJECT:model1,
                                                                       CAST (e:Value, DataRelationConnector)) .
            END.

            FINALLY:
                ASSIGN lRemovingElements = FALSE .
            END FINALLY.
        END.

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EnabledChanged event of the crainiate model1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID model1_EnabledChanged (sender AS System.Object,
                                               e AS System.EventArgs):

        DEFINE VARIABLE cNonUpdateTools AS CHARACTER NO-UNDO
            INIT "BusinessEntityTester,Options,Print,Exit,Save,SaveAs,Open,New,GenerateSource,Regenerate Source,Compile":U .

        IF THIS-OBJECT:model1:Enabled THEN
            THIS-OBJECT:smartToolbarController1:EnableTools (cNonUpdateTools) .
        ELSE
            THIS-OBJECT:smartToolbarController1:DisableTools (cNonUpdateTools) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SelectedChanged event of the crainiate model1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID model1_SelectedChanged (sender AS System.Object,
                                                e AS System.EventArgs):

        ActivateMenuItems () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a new Business Entity Design
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID NewDesign ():

        THIS-OBJECT:ActivateDesignCanvas () .

        IF CheckChanges () = FALSE THEN
            RETURN .

        IF CheckCloseSourceCodeEditor () = FALSE THEN
            RETURN .

        IF CAST (smartBusinessEntityAdapter1:DatasetController,
                 BusinessEntityDatasetController):New (THIS-OBJECT:model1) = FALSE THEN
            RETURN .

        ASSIGN THIS-OBJECT:Text     = THIS-OBJECT:WindowTitle (?)
               THIS-OBJECT:FileName = "":U .

        THIS-OBJECT:ActivateDesignCanvas () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Opens a new window
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID NewWindow ():

        DEFINE VARIABLE cStartupParameter AS CHARACTER                           NO-UNDO .
        DEFINE VARIABLE oStartInfo        AS System.Diagnostics.ProcessStartInfo NO-UNDO .

        ASSIGN cStartupParameter = SUBSTITUTE ('&1 -p Consultingwerk/BusinessEntityDesigner/start.p -param ""':U,
                                               Consultingwerk.Util.SessionHelper:StartupParameters()) .

        oStartInfo = NEW System.Diagnostics.ProcessStartInfo () .
        oStartInfo:FileName = Consultingwerk.Util.SessionHelper:ExecutablePath() .
        oStartInfo:Arguments = cStartupParameter .
        oStartInfo:WorkingDirectory = Consultingwerk.Util.SessionHelper:CurrentDirectory() .

        System.Diagnostics.Process:Start (oStartInfo) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosing Event
        Notes:   Shut's down Plugins and saves UI preferences to .BusinessEntityDesignerSettings2.xml
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnFormClosing (e AS CLASS System.Windows.Forms.FormClosingEventArgs):

        IF CheckChanges () = FALSE THEN
            e:Cancel = TRUE .
        ELSE
            IF CheckCloseSourceCodeEditor() = FALSE THEN
                e:Cancel = TRUE .

        SUPER:OnFormClosing (e) .

        IF NOT e:Cancel THEN DO:
            THIS-OBJECT:ShutdownPlugins() .

            DockManagerHelper:StoreToSettingsService (ultraDockManager1,
                                                      THIS-OBJECT:WindowPositionRegistryKey) .

            StoreFileLRU () .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .

            e:Cancel = TRUE .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        DEFINE VARIABLE cFileName            AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE oControls            AS "Control[]"         NO-UNDO .
        DEFINE VARIABLE oPane                AS DockableControlPane NO-UNDO .
        DEFINE VARIABLE oSettingsService     AS ISettingsService    NO-UNDO .
        DEFINE VARIABLE lSetting             AS LOGICAL             NO-UNDO .
        DEFINE VARIABLE oReleaseNotesControl AS ReleaseNotesControl NO-UNDO .

        SUPER:OnLoad (e) .

        LoadFileLRU () .

        UserInterfaceHelper:ProcessEvents () .

        DockManagerHelper:RestoreFromSettingsService (ultraDockManager1,
                                                      THIS-OBJECT:WindowPositionRegistryKey) .

        /* Mike Fechner, Consultingwerk Ltd. 12.10.2012
           Fix settings for new Dockable pane */
        oPane = ultraDockManager1:PaneFromControl (THIS-OBJECT:databaseFieldsControl1) .

        IF NOT VALID-OBJECT (oPane) THEN DO:
            oControls = {Consultingwerk/new-array.i System.Windows.Forms.Control 1} .
            oControls:SetValue (THIS-OBJECT:databaseFieldsControl1, 0) .

            ultraDockManager1:DockControls (oControls,
                                            DockedLocation:DockedRight,
                                            ChildPaneStyle:TabGroup) .

            oPane = ultraDockManager1:PaneFromControl (databaseFieldsControl1) .

            IF VALID-OBJECT (oPane) THEN DO:
                oPane:Text = "Database Fields"{&TRAN} .
                oPane:Settings:Appearance:Image = ImageHelper:Load
                        ("Consultingwerk/BusinessEntityDesigner/UI/Images/table2_selection_column.png":U) .
            END.
        END.
        ELSE DO:
            oPane:Text = "Database Fields"{&TRAN} .
            oPane:Settings:Appearance:Image = ImageHelper:Load
                    ("Consultingwerk/BusinessEntityDesigner/UI/Images/table2_selection_column.png":U) .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 26.03.2015
           Fix settings for new Dockable pane */
        oPane = ultraDockManager1:PaneFromControl (THIS-OBJECT:logManagerSettings1) .

        IF NOT VALID-OBJECT (oPane) THEN DO:
            oControls = {Consultingwerk/new-array.i System.Windows.Forms.Control 1} .
            oControls:SetValue (THIS-OBJECT:logManagerSettings1, 0) .

            ultraDockManager1:DockControls (oControls,
                                            DockedLocation:DockedLeft,
                                            ChildPaneStyle:TabGroup) .

            oPane = ultraDockManager1:PaneFromControl (logManagerSettings1) .

            IF VALID-OBJECT (oPane) THEN DO:
                oPane:Text = "Logfile Settings"{&TRAN} .
                oPane:Settings:Appearance:Image = ImageHelper:Load
                        ("Consultingwerk/BusinessEntityDesigner/UI/Images/date_time_16.png":U) .
            END.
        END.
        ELSE DO:
                oPane:Text = "Logfile Settings"{&TRAN} .
                oPane:Settings:Appearance:Image = ImageHelper:Load
                        ("Consultingwerk/BusinessEntityDesigner/UI/Images/date_time_16.png":U) .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 03.10.2015
           SCL-1052 - Create Release Note Control */
        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        IF VALID-OBJECT (oSettingsService) THEN DO:
            lSetting = DataTypeHelper:ToLogical(oSettingsService:GetSetting (THIS-OBJECT:WindowPositionRegistryKey,
                                                                             "Display Release Notes At Start":U)) .

            IF lSetting = ? THEN
                ASSIGN lSetting = TRUE .

            IF lSetting THEN DO:

                oReleaseNotesControl = NEW ReleaseNotesControl () .

                THIS-OBJECT:Controls:Add (oReleaseNotesControl) .

                oReleaseNotesControl:DockControl (ultraDockManager1, panel1) .
                oReleaseNotesControl:AllowDrop = TRUE .
                oReleaseNotesControl:DragEnter:Subscribe (ReleaseNotesControlDragEnter) .
            END.
        END.

        /* Mike Fechner, Consultingwerk Ltd., 02.11.2016
         * SCL-1470: Log Pane
         */
        oLogControl = NEW BusinessEntityGeneratorLogControl () .
        THIS-OBJECT:Controls:Add (oLogControl) .
        oLogControl:DockControl(ultraDockManager1, panel1) .

        ActivateMenuItems () .

        IF BusinessEntityDesignerSettings:BusinessEntityDesignerPlugins > "":U THEN
            InitializePlugins () .

        IF SESSION:PARAMETER MATCHES "*.bedgm":U THEN DO:

            FILE-INFORMATION:FILE-NAME = SESSION:PARAMETER .

            ASSIGN cFileName = FILE-INFORMATION:FULL-PATHNAME .

            IF cFileName > "":U THEN DO:
                CAST (smartBusinessEntityAdapter1:DatasetController,
                    BusinessEntityDatasetController):Open (cFileName, THIS-OBJECT:model1) .

                ASSIGN THIS-OBJECT:Text     = THIS-OBJECT:WindowTitle (cFileName)
                       THIS-OBJECT:FileName = cFileName .
            END.
        END.
        ELSE
            CAST (smartBusinessEntityAdapter1:DatasetController,
                BusinessEntityDatasetController):New (THIS-OBJECT:model1) .

        databaseFieldsControl1:DatabaseSchemaDatasetController = CAST (smartBusinessEntityAdapter2:DatasetController,
                                                                       DatabaseSchemaDatasetController) .

        businessEntityViewerControl1:FieldsEnabled:Subscribe (ViewerFieldsEnabledHandler) .
        memberTableViewerControl1:FieldsEnabled:Subscribe (ViewerFieldsEnabledHandler) .
        fieldViewerControl1:FieldsEnabled:Subscribe (ViewerFieldsEnabledHandler) .
        indexViewerControl1:FieldsEnabled:Subscribe (ViewerFieldsEnabledHandler) .
        dataRelationViewerControl1:FieldsEnabled:Subscribe (ViewerFieldsEnabledHandler) .

        ultraStatusBar1:Panels["Zoom":U]:Text = SUBSTITUTE ("&1 %":U,
                                                            ultraTrackBar1:Value) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnShown (e AS System.EventArgs ):

        DEFINE VARIABLE cWorkDir AS CHARACTER NO-UNDO.

        SUPER:OnShown(e).

        /* Mike Fechner, Consultingwerk Ltd. 02.11.2009
           Create a Timer to Close the Splash screen */

        IF VALID-OBJECT (oSplash) THEN DO:
            oTimer = NEW System.Windows.Forms.Timer () .
            oTimer:Tick:Subscribe (TimerTick) .
            oTimer:Start () .
            oTimer:Interval = 3000 .
        END.

        ASSIGN cWorkDir = SessionHelper:CurrentDirectory() .

        IF LENGTH (cWorkDir, "CHARACTER":U) > 30 THEN
            ASSIGN
                ultraStatusBar1:Panels["workdir":U]:Text = "...":U + SUBSTRING (cWorkDir, LENGTH (cWorkDir, "CHARACTER":U) - 30, -1, "CHARACTER":U)
                ultraStatusBar1:Panels["workdir":U]:ToolTipText = cWorkDir
                .
        ELSE
            ASSIGN
                ultraStatusBar1:Panels["workdir":U]:Text = cWorkDir
                .

        DO ON ERROR UNDO, THROW:

            ultraStatusBar1:Panels["username":U]:Text = Win32:GetUserName (ExtendedNameFormatEnum:NameDisplay) .

            CATCH err AS Progress.Lang.Error :
                Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
            END CATCH.

        END.

        /* Mike Fechner, Consultingwerk Ltd. 11.04.2011
           Warning message when starting with not compile session */
        IF NOT SessionHelper:AllowsCompile() THEN
            MessageFormHelper:ShowMessage ("You ABL session is not enabled for compile."{&TRAN} + System.Environment:NewLine +
                                           "Some functions of the Business Entity Designer may not function as expected."{&TRAN},
                                           "Business Entity Designer"{&TRAN},
                                           MessageFormImages:ImageWarning) .

        UserInterfaceHelper:ProcessEvents () .

        smartBusinessEntityAdapter1:RetrieveData() .

        THIS-OBJECT:DisplayStatus ("Loading Database Schema...":U) .

        smartBusinessEntityAdapter2:RetrieveData() .

        THIS-OBJECT:DisplayStatus ("":U) .

        CAST (smartBusinessEntityAdapter2:DatasetController,
              DatabaseSchemaDatasetController):PopulateToolbox (ultraExplorerBar1) .

        THIS-OBJECT:ParseStartupParameter () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Opens a design using a FileOpen Dialog
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OpenDesign ():

        DEFINE VARIABLE oResult AS System.Windows.Forms.DialogResult NO-UNDO .

        THIS-OBJECT:ActivateDesignCanvas () .

        IF CheckChanges () = FALSE THEN
            RETURN .

        IF CheckCloseSourceCodeEditor () = FALSE THEN
            RETURN .

        openFileDialog1:FileName = THIS-OBJECT:FileName .

        WAIT-FOR openFileDialog1:ShowDialog () SET oResult .

        IF NOT Progress.Util.EnumHelper:AreEqual (System.Windows.Forms.DialogResult:OK,
                                                  oResult) THEN
            RETURN .

        IF CAST (smartBusinessEntityAdapter1:DatasetController,
                 BusinessEntityDatasetController):Open (openFileDialog1:FileName, THIS-OBJECT:model1) = FALSE THEN
            RETURN .

        ASSIGN THIS-OBJECT:Text     = THIS-OBJECT:WindowTitle (openFileDialog1:FileName)
               THIS-OBJECT:FileName = openFileDialog1:FileName .

        THIS-OBJECT:ActivateDesignCanvas () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Opens a file from the LRU list
        Notes:
        @param pcFileName The file name to load
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OpenFileFromLRU (pcFileName AS CHARACTER):

        IF CheckChanges () = FALSE THEN
            RETURN .

        THIS-OBJECT:ActivateDesignCanvas () .

        IF CheckCloseSourceCodeEditor () = FALSE THEN
            RETURN .

        IF CAST (smartBusinessEntityAdapter1:DatasetController,
                 BusinessEntityDatasetController):Open (pcFileName, THIS-OBJECT:model1) = FALSE THEN
            RETURN .

        ASSIGN THIS-OBJECT:Text     = THIS-OBJECT:WindowTitle (pcFileName)
               THIS-OBJECT:FileName = pcFileName .

        THIS-OBJECT:ActivateDesignCanvas () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Parses and processes the startup parameters
        Notes:   When the -param parameter contains open= then we'll open the
                 given .bedgm file
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ParseStartupParameter ():

        DEFINE VARIABLE oParameters AS Consultingwerk.Framework.Collections.CharacterDictionary NO-UNDO .
        DEFINE VARIABLE cFileName   AS CHARACTER                                                NO-UNDO .

        oParameters = ListHelper:AlternatingListToDictionary (SESSION:PARAMETER,
                                                              ",":U,
                                                              "=":U) .

        IF oParameters:ContainsKey ("open":U) THEN DO:
            ASSIGN cFileName = oParameters:GetValue ("open":U) .

            IF CAST (smartBusinessEntityAdapter1:DatasetController,
                     BusinessEntityDatasetController):Open (cFileName, THIS-OBJECT:model1) = FALSE THEN
                RETURN .

            ASSIGN THIS-OBJECT:Text     = THIS-OBJECT:WindowTitle (cFileName)
                   THIS-OBJECT:FileName = cFileName .
        END.

        THIS-OBJECT:ActivateDesignCanvas () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Pastes temp-tables from a block of source code
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PasteSourceCode ():

        DEFINE VARIABLE oDialogResult AS DialogResult NO-UNDO .

        DEFINE VARIABLE oForm AS PasteSourceCodeForm NO-UNDO .

        oForm = NEW PasteSourceCodeForm () .

        WAIT-FOR oForm:ShowDialog (THIS-OBJECT) SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult}

        THIS-OBJECT:ActivateDesignCanvas () .

        oController:ImportTableFromLongChar (oForm:CodeBlock) .


    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose:
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID Print ():

        DEFINE VARIABLE oPrint AS Crainiate.ERM4.Printing.PrintDocument NO-UNDO .

        THIS-OBJECT:ActivateDesignCanvas () .

        oPrint = NEW Crainiate.ERM4.Printing.PrintDocument (THIS-OBJECT:model1) .

        IF ultraPrintPreviewDialog1:IsDisposed THEN DO:
            DELETE OBJECT THIS-OBJECT:ultraPrintPreviewDialog1 .

            THIS-OBJECT:ultraPrintPreviewDialog1 = NEW Infragistics.Win.Printing.UltraPrintPreviewDialog (THIS-OBJECT:components).
            THIS-OBJECT:ultraPrintPreviewDialog1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2007.
            THIS-OBJECT:ultraPrintPreviewDialog1:UseAntiAlias = TRUE.
        END.

        ultraPrintPreviewDialog1:Document = oPrint .

        WAIT-FOR ultraPrintPreviewDialog1:ShowDialog () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the selected temp-tables from the database
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RefreshTable ():

        DEFINE VARIABLE oEnumerator AS System.Collections.IDictionaryEnumerator NO-UNDO .
        DEFINE VARIABLE oTables     AS Crainiate.ERM4.Elements                  NO-UNDO .

        DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

        THIS-OBJECT:ActivateDesignCanvas () .

        ASSIGN oTables = model1:SelectedElements(Progress.Util.TypeHelper:GetType ("Crainiate.ERM4.Table":U)) .

        IF oTables:Count = 0 THEN
            RETURN .

        oEnumerator = oTables:GetEnumerator () .
        oEnumerator:MoveNext() .

        ASSIGN cMessage = CAST (smartBusinessEntityAdapter1:DatasetController,
                                BusinessEntityDatasetController):RefreshTable (CAST(CAST (oEnumerator:Current, System.Collections.DictionaryEntry):Value,
                                                                                    Consultingwerk.BusinessEntityDesigner.UI.Shapes.TempTableShape),
                                                                                       CAST (smartBusinessEntityAdapter2:DatasetController,
                                                                                             DatabaseSchemaDatasetController)) .

        IF cMessage > "":U THEN
            Consultingwerk.Util.MessageFormHelper:ShowMessage (cMessage,
                                                               "Business Entity Designer"{&TRAN}) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the selected temp-tables from the include files
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RefreshTableFromInclude ():

        DEFINE VARIABLE oEnumerator AS System.Collections.IDictionaryEnumerator NO-UNDO .
        DEFINE VARIABLE oTables     AS Crainiate.ERM4.Elements                  NO-UNDO .

        DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

        THIS-OBJECT:ActivateDesignCanvas () .

        ASSIGN oTables = model1:SelectedElements(Progress.Util.TypeHelper:GetType ("Crainiate.ERM4.Table":U)) .

        IF oTables:Count = 0 THEN
            RETURN .

        oEnumerator = oTables:GetEnumerator () .
        oEnumerator:MoveNext() .

        ASSIGN cMessage = CAST (smartBusinessEntityAdapter1:DatasetController,
                                BusinessEntityDatasetController):RefreshTableFromInclude (CAST(CAST (oEnumerator:Current, System.Collections.DictionaryEntry):Value,
                                                                                               Consultingwerk.BusinessEntityDesigner.UI.Shapes.TempTableShape)) .

        IF cMessage > "":U THEN
            Consultingwerk.Util.MessageFormHelper:ShowMessage (cMessage,
                                                               "Business Entity Designer"{&TRAN}) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Re-Generates source code (refresh)
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RegenerateSource ():

        IF NOT THIS-OBJECT:EnsureAllViewersSaved () THEN
            RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 16.05.2014
           SCL-299: Prompt for save changes before generate */
        IF BusinessEntityDesignerSettings:PromptForSaveBeforeGenerate THEN DO:
            IF NOT THIS-OBJECT:CheckChanges() THEN
                RETURN .
        END.

        CAST (smartBusinessEntityAdapter1:DatasetController,
              BusinessEntityDatasetController):ReGenerateSource () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Registers the Business Entity Desginer Form and the BusinessEntityDatasetController
                 in the ServiceContainer
        Notes:   Ensures that it's accessible from plugins or custom services loaded
                 in the Business Entity Designer environment
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RegisterServices ():

        DEFINE VARIABLE oLoader AS ServiceLoader NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER       NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 19.05.2014
           SCL-283: Optionally do not register the BusinessEntityDesignerForm in the
           ServiceContainer while starting the Business Entity Designer. This is to
           workaround crashes occuring under 10.2B at a single customer only. */
        &IF DEFINED (DoNotRegisterBusinessEntityDesignerForm) EQ 0 &THEN
        FrameworkSettings:ServiceContainer:AddService
            (Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityDesignerForm":U),
             THIS-OBJECT) .
        &ENDIF

        FrameworkSettings:ServiceContainer:AddService
            (Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDatasetController":U),
             smartBusinessEntityAdapter1:DatasetController) .

        IF BusinessEntityDesignerSettings:CustomServices > "":U THEN DO ON ERROR UNDO, THROW:

            oLoader = NEW ServiceLoader () .

            DO i = 1 TO NUM-ENTRIES (BusinessEntityDesignerSettings:CustomServices):
                oLoader:Load (ENTRY (i, BusinessEntityDesignerSettings:CustomServices)) .
            END .

            CATCH err AS Progress.Lang.Error:
                Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
            END CATCH.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DragEnter event of the ReleaseNotesControl
        Notes:
        @param sender The reference to the Control that raised the event
        @param e The DragEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ReleaseNotesControlDragEnter (sender AS System.Object,
                                                        e AS DragEventArgs):

        THIS-OBJECT:ActivateDesignCanvas() .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Removes the currently selected data relation
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RemoveDataRelation ():

        DEFINE VARIABLE oRelations    AS Crainiate.ERM4.Elements        NO-UNDO .
        DEFINE VARIABLE oDialogResult AS DialogResultEnum               NO-UNDO .
        DEFINE VARIABLE oEnumerator   AS System.Collections.IEnumerator NO-UNDO .

        THIS-OBJECT:ActivateDesignCanvas () .

        oDialogResult = MessageFormHelper:AskQuestion ("Remove Data-Relation?"{&TRAN},
                                                       "Business Entity Designer"{&TRAN}) .

        IF oDialogResult:Equals (DialogResultEnum:DialogResultYes) THEN .
        ELSE RETURN .

        ASSIGN oRelations  = model1:SelectedElements(Progress.Util.TypeHelper:GetType ("Crainiate.ERM4.Connector":U))
               oEnumerator = oRelations:Values:GetEnumerator()
               .

        oEnumerator:Reset() .
        oEnumerator:MoveNext() .

        oController:RemoveRelation (THIS-OBJECT:model1,
                                    CAST (oEnumerator:Current, DataRelationConnector)) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the design
        Notes:
        @param plOk Logical value indicating if the design was saved
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SaveDesign (OUTPUT plOk AS LOGICAL):

        THIS-OBJECT:ActivateDesignCanvas () .

        ASSIGN plOk = TRUE .

        IF THIS-OBJECT:FileName > "":U THEN DO:
            IF CAST (smartBusinessEntityAdapter1:DatasetController,
                     BusinessEntityDatasetController):Save (THIS-OBJECT:FileName) = FALSE THEN
                RETURN .
        END.
        ELSE
            THIS-OBJECT:SaveDesignAs (OUTPUT plOk) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the design under a new name
        Notes:
        @param plOk Logical value indicating if the design was saved
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SaveDesignAs (OUTPUT plOk AS LOGICAL):

        DEFINE VARIABLE oResult          AS System.Windows.Forms.DialogResult NO-UNDO .
        DEFINE VARIABLE cDefaultFileName AS CHARACTER NO-UNDO.

        THIS-OBJECT:ActivateDesignCanvas () .

        ASSIGN plOk = TRUE .

        IF THIS-OBJECT:FileName > "":U THEN
            saveFileDialog1:FileName = THIS-OBJECT:FileName .
        ELSE DO:
            cDefaultFileName = CAST (smartBusinessEntityAdapter1:DatasetController,
                                     BusinessEntityDatasetController):GetDefaultFileName () .

            IF cDefaultFileName > "":U THEN
                saveFileDialog1:FileName = cDefaultFileName .
            ELSE
                saveFileDialog1:FileName = "":U .
        END.

        WAIT-FOR saveFileDialog1:ShowDialog () SET oResult .

        IF NOT Progress.Util.EnumHelper:AreEqual (System.Windows.Forms.DialogResult:OK,
                                                  oResult) THEN
            DO:
                ASSIGN plOk = FALSE .
                RETURN .
            END.

        IF CAST (smartBusinessEntityAdapter1:DatasetController,
                 BusinessEntityDatasetController):Save (saveFileDialog1:FileName) = FALSE THEN
            RETURN .

        ASSIGN THIS-OBJECT:Text     = THIS-OBJECT:WindowTitle (saveFileDialog1:FileName)
               THIS-OBJECT:FileName = saveFileDialog1:FileName .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Tick event of the searchTimer component
        Notes:
        @param sender The reference to the System.Object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID searchTimer_Tick (sender AS System.Object,
                                          e AS System.EventArgs):

        THIS-OBJECT:FilterTables (CAST (smartToolbarController1:Tools["FilterTable":U], Infragistics.Win.UltraWinToolbars.TextBoxTool):Text) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shows the Progress ADE Database Connections Dialog
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ShowDatabaseConnectionsDialog ():

        RUN protools/_dblist.w PERSISTENT .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shows the Login Preferencse Dialog
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ShowDatabaseLoginPreferences ():

        DEFINE VARIABLE oForm AS DatabaseLoginPreferencesForm NO-UNDO .

        ASSIGN oForm = NEW DatabaseLoginPreferencesForm () .

        WAIT-FOR oForm:ShowDialog () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose:
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ShowOptionsDialog ():

        DEFINE VARIABLE oForm         AS BusinessEntityDesignerSettingsForm NO-UNDO .
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult  NO-UNDO .

        DO ON ERROR UNDO, THROW:
            oForm = NEW BusinessEntityDesignerSettingsForm () .

            CATCH err AS Progress.Lang.SysError:
                IF err:NumMessages = 0 THEN
                    /* retry */
                    oForm = NEW BusinessEntityDesignerSettingsForm () .
                ELSE
                    UNDO, THROW err .
            END CATCH.
        END.

        WAIT-FOR oForm:ShowDialog (THIS-OBJECT) SET oDialogResult .

    END METHOD.


    /*------------------------------------------------------------------------------
        Purpose: Shutdown all loaded plugins
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ShutdownPlugins ():

        DEFINE VARIABLE oPlugin AS IBusinessEntityDesignerPlugin NO-UNDO .

        FOR EACH ttPlugins ON ERROR UNDO, NEXT:
            ASSIGN oPlugin = CAST (ttPlugins.oPlugin, IBusinessEntityDesignerPlugin) .

            IF VALID-OBJECT (oPlugin) THEN DO:
                oPlugin:Shutdown() .
                IF VALID-OBJECT (oPlugin) THEN
                    DELETE OBJECT oPlugin .
            END.

            CATCH err AS Progress.Lang.Error:
                ErrorHelper:ShowErrorMessage (err).
            END CATCH.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the smartToolbarController1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartToolbarController1_ToolClick (INPUT sender AS System.Object,
                                                           INPUT e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs ):

        DEFINE VARIABLE lOk AS LOGICAL NO-UNDO.

        CASE e:Tool:Key:

            WHEN "DrawConnector":U THEN
                THIS-OBJECT:ActivateDrawConnector () .
            WHEN "DrawPointer":U THEN
                THIS-OBJECT:ActivateDrawPointer () .
            WHEN "AutoLayout":U THEN
                THIS-OBJECT:AutoLayout () .

            WHEN "BusinessEntityTester":U THEN
                THIS-OBJECT:BusinessEntityTester () .
            WHEN "Options":U THEN
                THIS-OBJECT:ShowOptionsDialog () .
            WHEN "Print":U THEN
                THIS-OBJECT:Print () .
            WHEN "Exit":U THEN
                THIS-OBJECT:Close()  .
            WHEN "Save":U THEN
                THIS-OBJECT:SaveDesign (OUTPUT lOk) .
            WHEN "SaveAs":U THEN
                THIS-OBJECT:SaveDesignAs (OUTPUT lOk) .
            WHEN "FilterClear":U THEN
                THIS-OBJECT:FilterTables ("":U) .

            WHEN "Open":U THEN
                THIS-OBJECT:OpenDesign () .
            WHEN "New":U THEN
                THIS-OBJECT:NewDesign () .

            WHEN "GenerateSource":U THEN
               THIS-OBJECT:GenerateSource () .
            WHEN "Regenerate Source":U THEN
               THIS-OBJECT:ReGenerateSource () .
            WHEN "Compile":U THEN
               THIS-OBJECT:Compile () .
            WHEN "Edit Source":U THEN
               THIS-OBJECT:EditSource () .

            /* Table */
            WHEN "RefreshTable":U THEN
                THIS-OBJECT:RefreshTable () .
            WHEN "RefreshTable2":U THEN
                THIS-OBJECT:RefreshTableFromInclude () .
            WHEN "FieldsReorder":U THEN
                THIS-OBJECT:FieldsReorder () .
            WHEN "Duplicate Table":U THEN
                THIS-OBJECT:DuplicateSelectedTable () .
            WHEN "Edit Fields":U THEN
                THIS-OBJECT:EditFields () .

            WHEN "Copy Field Names":U THEN
                THIS-OBJECT:CopyFieldNames () .

            /* Data-Relation */
            WHEN "Relation Fields":U THEN
                THIS-OBJECT:UpdateRelationFields () .
            WHEN "Remove Relation":U THEN
                THIS-OBJECT:RemoveDataRelation () .

            WHEN "Import Table":U THEN
                THIS-OBJECT:ImportTable () .
            WHEN "Paste ABL Source Code":U THEN
                THIS-OBJECT:PasteSourceCode () .

            WHEN "Session Info":U THEN
                Consultingwerk.Windows.Session.SessionInfoForm:ShowSessionInformation () .

            WHEN "New Window":U THEN
                THIS-OBJECT:NewWindow () .

            WHEN "Expand all":U THEN
                THIS-OBJECT:ExpandAllTables () .

            WHEN "Collapse all":U THEN
                THIS-OBJECT:CollapseAllTables () .

            /* Custom properties */
            WHEN "Custom Business Entity Properties":U THEN
                THIS-OBJECT:UpdateCustomProperties (CustomPropertiesScopeEnum:BusinessEntity) .

            WHEN "Custom Table Properties":U THEN
                THIS-OBJECT:UpdateCustomProperties (CustomPropertiesScopeEnum:TempTable) .

            WHEN "Custom Relation Properties":U THEN
                THIS-OBJECT:UpdateCustomProperties (CustomPropertiesScopeEnum:DataRelation) .

            WHEN "Custom Field Properties":U THEN
                THIS-OBJECT:UpdateCustomProperties (CustomPropertiesScopeEnum:Field) .

            WHEN "Custom Index Properties":U THEN
                THIS-OBJECT:UpdateCustomProperties (CustomPropertiesScopeEnum:Index) .

            WHEN "Database Connections":U THEN
                THIS-OBJECT:ShowDatabaseConnectionsDialog () .

            WHEN "Database Login Preferences":U THEN
                THIS-OBJECT:ShowDatabaseLoginPreferences () .

            WHEN "Split Extend Field into separate Fields":U THEN
                oController:SplitExtentField (fieldAdapter:GetFieldValues ("TempTableName":U),
                                              fieldAdapter:GetFieldValues ("FieldName":U)) .

            WHEN "Display Release Notes At Start":U THEN
                THIS-OBJECT:ToggleDisplayReleaseNotes () .


/*            OTHERWISE                     */
/*              MESSAGE e:Tool:Key          */
/*                  VIEW-AS ALERT-BOX.*/
/*                                            */
        END CASE .

        IF e:Tool:Key BEGINS "file_lru_":U  THEN
            OpenFileFromLRU (e:Tool:SharedProps:ToolTipText) .

        IF e:Tool:Key BEGINS "LRUEntry":U THEN
            THIS-OBJECT:FilterTables (e:Tool:SharedProps:Caption).

        CATCH err AS Progress.Lang.Error :
            IF VALID-OBJECT (oLogControl) THEN
                oLogControl:DisplayText (ErrorHelper:FormattedErrorMessages(err)) .

            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

        FINALLY:
            IF VALID-OBJECT (oLogControl) THEN
                oLogControl:InsertSeperator() .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolKeyPress event of the smartToolbarController1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolKeyPressEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartToolbarController1_ToolKeyPress (sender AS System.Object,
                                                              e AS Infragistics.Win.UltraWinToolbars.ToolKeyPressEventArgs):

        IF e:Tool:Key = "FilterTable":U THEN DO:
            IF e:KeyChar = CHR(13) THEN
                THIS-OBJECT:FilterTables (CAST (e:Tool, Infragistics.Win.UltraWinToolbars.TextBoxTool):Text) .
            ELSE
                searchTimer:Enabled = TRUE .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores the LRU file list in the registry
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StoreFileLRU ():

        DEFINE VARIABLE oButtonTool  AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO .
        DEFINE VARIABLE i            AS INTEGER                                      NO-UNDO .

        DEFINE VARIABLE cSettingsKey AS CHARACTER                                    NO-UNDO.

        IF lUseLocalFileHistory THEN
            ASSIGN cSettingsKey = Consultingwerk.Framework.FrameworkSettings:BaseRegistryKey + "Settings~\":U +
                                  REPLACE (BusinessEntityDesignerSettings:SettingsFile, ":":U, "":U).
        ELSE
            ASSIGN cSettingsKey = Consultingwerk.Framework.FrameworkSettings:BaseRegistryKey + "Settings":U .

        DO i = 0 TO smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaRight:Tools:Count - 1:

            oButtonTool = CAST (smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaRight:Tools[i],
                                Infragistics.Win.UltraWinToolbars.ButtonTool) .

            IF oButtonTool:SharedProps:TooltipText > "":U THEN
                Consultingwerk.Framework.Registry:SetRegistryValue
                               ("User":U,
                                cSettingsKey,
                                "File LRU ":U + STRING (i, "99":U),
                                oButtonTool:SharedProps:TooltipText) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose:
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StoreLRU ():

        DEFINE VARIABLE cSettings AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i         AS INTEGER   NO-UNDO.

        DO i = 0 TO oLRUList:Count - 1:
            cSettings = cSettings + "|":U + oLRUList [i] .
        END .

        Consultingwerk.Framework.Registry:SetRegistryValue
                       ("User":U,
                        Consultingwerk.Framework.FrameworkSettings:BaseRegistryKey + "Settings":U,
                        "FilterHistory":U,
                        TRIM (cSettings, "|":U)) .

        @SuppressUnusedWarnings.
        CATCH err AS System.Exception:
            /* ignore */
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TableItemSelected event of the
                 BusinessEntityDatasetController
        Notes:
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID TableItemSelectedHandler (sender AS Progress.Lang.Object,
                                                    e AS Consultingwerk.EventArgs):

        THIS-OBJECT:ActivateMenuItems () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Tick event of the temporary Timer instance
        Notes:   Close the splash screen after a while
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID TimerTick (sender AS System.Object,
                                     e AS System.EventArgs) :

        IF VALID-OBJECT (oSplash) THEN
            oSplash:Close ().

        oTimer:Dispose () .

        oTimer = ? .
        oSplash = ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler for the Tick Event of the timer1 component
        Notes:   Used when a new Data-Relation has been dragged on the design canvas
                 In the model1_ElementInserted event handler, the target shape of the
                 new connector is not yet set. This is the case immediately after
                 that event handler - thus the use of this timer.
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID timer1_Tick (sender AS System.Object,
                                     e AS System.EventArgs):

        DEFINE VARIABLE cStart          AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cEnd            AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cChildTableName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lActive         AS LOGICAL   NO-UNDO INIT TRUE .

        timer1:Enabled = FALSE .

        IF NOT VALID-OBJECT (oNewConnector) THEN
            RETURN .

        IF NOT VALID-OBJECT (oNewConnector:End:Shape) THEN DO:
            THIS-OBJECT:model1:Lines:Remove(oNewConnector:Key) .

            DELETE OBJECT oNewConnector.

            RETURN.
        END.

        ASSIGN cStart          = oNewConnector:Start:Shape:Key
               cEnd            = oNewConnector:End:Shape:Key
               cChildTableName = CAST (oNewConnector:End:Shape, Crainiate.ERM4.Table):Heading
               NO-ERROR .

        IF cStart > "":U AND cEnd > "":U AND cChildTableName > "":U THEN .
        ELSE DO:
            THIS-OBJECT:model1:Lines:Remove(oNewConnector:Key) .

            DELETE OBJECT oNewConnector.

            RETURN .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 25.04.2011
           Multiple parent tables are not supported by ProDataset - and the
           current code that populates the structure treeview has issues with
           it as well (System.ArgumentException: Key already exists, key: 0x000000000001b902) */
        IF oController:HasActiveParentRelation (cChildTableName) THEN
            ASSIGN lActive = FALSE .

        THIS-OBJECT:model1:Lines:Remove(oNewConnector:Key) .

        DELETE OBJECT oNewConnector.

        IF cStart > "":U AND cEnd > "":U THEN
            CAST (smartBusinessEntityAdapter1:DatasetController,
                  BusinessEntityDatasetController):CreateDataRelation (THIS-OBJECT:model1,
                                                                       cStart,
                                                                       cEnd,
                                                                       lActive) .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the PaneActivate event of the ultraDockManager1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ControlPaneEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraDockManager1_PaneActivate (sender AS System.Object,
                                                        e AS Infragistics.Win.UltraWinDock.ControlPaneEventArgs):

        IF TYPE-OF (CAST(e:Pane:Control, Progress.Lang.Object), ISmartTableIOTarget) THEN
            smartToolbarController1:ActivateSmartTableIOTarget
                (CAST (CAST(e:Pane:Control, Progress.Lang.Object), ISmartTableIOTarget)) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ActiveGroupChanged event of the ActiveGroupChanged
        Notes:
        @param sender The object that raised the event
        @param e The GroupEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraExplorerBar1_ActiveGroupChanged (sender AS System.Object,
                                                              e AS GroupEventArgs):

        IF VALID-OBJECT (e:Group:Container) AND
           e:Group:Container:Controls:Count > 0 THEN

            e:Group:Container:Controls[0]:Focus() .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the MouseDown event of the ultraExplorerBar1
        Notes:
        @param sender The object that raised the event
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraExplorerBar1_MouseDown (sender AS System.Object,
                                                     e AS System.Windows.Forms.MouseEventArgs):

        DEFINE VARIABLE oItem AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO .
        DEFINE VARIABLE oEffects AS System.Windows.Forms.DragDropEffects NO-UNDO .

        oItem = ultraExplorerBar1:ItemFromPoint (e:Location) .

        IF NOT VALID-OBJECT (oItem) THEN
            RETURN .

        oEffects = CAST (Progress.Util.EnumHelper:Or (System.Windows.Forms.DragDropEffects:Copy,
                                                      System.Windows.Forms.DragDropEffects:Link),
                         System.Windows.Forms.DragDropEffects) .

        ultraExplorerBar1:DoDragDrop (oItem, oEffects) .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ButtonClick event of the ultraStatusBar
        Notes:
        @param sender The reference to the object that raised the event
        @param e The PanelEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraStatusBar1_ButtonClick (sender AS System.Object,
                                                     e AS Infragistics.Win.UltraWinStatusBar.PanelEventArgs):

        IF e:Panel:Key = "workdir":U THEN
            Consultingwerk.Util.WindowsExplorerHelper:ShowFolder (SessionHelper:CurrentDirectory()) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ValueChanged event of the ultraTrackBar1
        Notes:
        @param sender The object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTrackBar1_ValueChanged (INPUT sender AS System.Object,
                                                     INPUT e AS System.EventArgs):

        model1:Zoom = ultraTrackBar1:Value .

        ultraStatusBar1:Panels["Zoom":U]:Text = SUBSTITUTE ("&1 %":U,
                                                            ultraTrackBar1:Value) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterSelect event of the ultraTree1
        Notes:
        @param sender The object that raised the event
        @param e The SelectEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTree1_AfterSelect (sender AS System.Object,
                                                e AS Infragistics.Win.UltraWinTree.SelectEventArgs):

        DEFINE VARIABLE cKey   AS CHARACTER            NO-UNDO.
        DEFINE VARIABLE oShape AS Crainiate.ERM4.Shape NO-UNDO .

        IF ultraTree1:SelectedNodes:Count = 0 THEN
            RETURN .

        ASSIGN cKey = ultraTree1:SelectedNodes[0]:Key .

        IF model1:Shapes:Contains (cKey) THEN DO:

            oShape = CAST (model1:Shapes[cKey], Crainiate.ERM4.Shape) .

            model1:SelectElements (FALSE) .

            oShape:Selected = TRUE .

            model1:ScrollToElement (oShape) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Handles the UnsavedChangesChanged event of the BusinessEntityDatasetController
        Notes:
        @param sender The object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UnsavedChangesChangedHandler (sender AS Progress.Lang.Object,
                                                        e AS Consultingwerk.EventArgs):

        IF oController:UnsavedChanges THEN
            ultraStatusBar1:Panels["modified":U]:Text = "Modified":U .
        ELSE
            ultraStatusBar1:Panels["modified":U]:Text = "":U .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Updates the custom properties for the currently selected object of
                 the given scope
        Notes:   Calls into the BusinessEntityDatasetController's UpdateCustomProperties
                 method
        @param poCustomPropertiesScope The property scope to edit the custom properties for
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateCustomProperties (poCustomPropertiesScope AS CustomPropertiesScopeEnum):

        ObjectAssert:IsValid (poCustomPropertiesScope, "poCustomPropertiesScope":U) .

        oController:UpdateCustomProperties (poCustomPropertiesScope) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the LRU file list with a recently opened file name
        Notes:
        @param pcFileName The name of the recently opened file
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateFileLRU (pcFileName AS CHARACTER):

        DEFINE VARIABLE oButtonTool AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO .
        DEFINE VARIABLE oEnumerator AS System.Collections.IEnumerator               NO-UNDO .

        oEnumerator = smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaRight:Tools:GetEnumerator() .
        oEnumerator:Reset() .

        DO WHILE oEnumerator:MoveNext():
            oButtonTool = CAST (oEnumerator:Current,
                                Infragistics.Win.UltraWinToolbars.ButtonTool) .

            IF oButtonTool:SharedProps:TooltipText = pcFileName THEN DO:
                smartToolbarController1:Tools:Remove (oButtonTool:SharedProps:RootTool) .
            END.
        END.

        DO WHILE smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaRight:Tools:Count > 10:
            oButtonTool = CAST (smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaRight:Tools[10],
                                Infragistics.Win.UltraWinToolbars.ButtonTool) .

            smartToolbarController1:Tools:Remove (oButtonTool:SharedProps:RootTool) .
        END.

        oButtonTool = NEW Infragistics.Win.UltraWinToolbars.ButtonTool ("file_lru_":U + GUID) .
        oButtonTool:SharedProps:Caption = (IF LENGTH (pcFileName, "CHARACTER":U) > 40 THEN "... ":U ELSE "":U) +
                                          SUBSTRING (pcFileName, MAX (1, LENGTH (pcFileName, "CHARACTER":U) - 40), -1, "CHARACTER":U) .
        oButtonTool:SharedProps:ToolTipText = pcFileName .
        smartToolbarController1:Tools:Add (oButtonTool) .

        smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaRight:Tools:InsertTool(0, oButtonTool:Key) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Pops up dialog for relation field mapping
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateRelationFields ():

        THIS-OBJECT:ActivateDesignCanvas () .

        CAST (smartBusinessEntityAdapter1:DatasetController,
              BusinessEntityDatasetController):UpdateDataRelationFields () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the FieldsEnabled event of the five viewers in the
                 Business Entity Designer Form
        Notes:   Ensures that only a single property viewer is in update mode
        @param sender The reference to the object that rised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ViewerFieldsEnabledHandler (sender AS System.Object,
                                                      e AS System.EventArgs):

        DEFINE VARIABLE oViewers      AS SmartViewerControl NO-UNDO EXTENT 5 .
        DEFINE VARIABLE cLabels       AS CHARACTER          NO-UNDO EXTENT 5 .
        DEFINE VARIABLE oDialogResult AS DialogResultEnum   NO-UNDO .

        DEFINE VARIABLE i        AS INTEGER            NO-UNDO .
        DEFINE VARIABLE iCurrent AS INTEGER            NO-UNDO .

        ASSIGN oViewers[1] = businessEntityViewerControl1 cLabels[1] = "Entity"{&TRAN}
               oViewers[2] = memberTableViewerControl1    cLabels[2] = "Table"{&TRAN}
               oViewers[3] = fieldViewerControl1          cLabels[3] = "Field"{&TRAN}
               oViewers[4] = indexViewerControl1          cLabels[4] = "Index"{&TRAN}
               oViewers[5] = dataRelationViewerControl1   cLabels[5] = "Data-Relation"{&TRAN}
               .

        DO iCurrent = 1 TO 5:
            IF oViewers[iCurrent] = sender THEN
                LEAVE .
        END.

        DO i = 1 TO 5:
            IF i = iCurrent THEN
                NEXT .

            IF oViewers[i]:SmartTableIOState = TableIOStateEnum:FieldsDisabled OR
               oViewers[i]:SmartTableIOState = TableIOStateEnum:NoRecordAvailable OR
               oViewers[i]:SmartTableIOState = TableIOStateEnum:NoDataSource OR
               oViewers[i]:SmartTableIOState = TableIOStateEnum:NoParentRecordAvailable THEN
                NEXT .

            IF oViewers[i]:SmartTableIOState = TableIOStateEnum:FieldsEnabled THEN DO:
                oViewers[i]:CancelUpdate() .
                NEXT.
            END.

            oDialogResult = MessageFormHelper:AskQuestion (SUBSTITUTE ("Save changes in &1 Properties?"{&TRAN}, cLabels[i]),
                                                           THIS-OBJECT:Text) .

            IF oDialogResult = DialogResultEnum:DialogResultNo THEN DO:
                oViewers[i]:CancelUpdate() .
                NEXT.
            END.

            oViewers[i]:SaveChanges() .

            IF oViewers[i]:SmartTableIOState = TableIOStateEnum:FieldsDisabled OR
               oViewers[i]:SmartTableIOState = TableIOStateEnum:NoRecordAvailable OR
               oViewers[i]:SmartTableIOState = TableIOStateEnum:NoDataSource OR
               oViewers[i]:SmartTableIOState = TableIOStateEnum:NoParentRecordAvailable THEN
                NEXT .

            oViewers[iCurrent]:DisableFields() .

            RETURN .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the BusinessEntityDesignerForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC BusinessEntityDesignerForm ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.

