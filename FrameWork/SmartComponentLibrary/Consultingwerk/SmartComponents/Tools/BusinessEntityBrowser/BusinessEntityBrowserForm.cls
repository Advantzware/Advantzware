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
    File        : BusinessEntityBrowserForm
    Purpose     : Allows the user to browse through the available business
                  entities and run queries
    Syntax      : Use start.p to start this Form
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 14 22:46:17 CET 2012
    Notes       : Used the same control as the Business Entity Tester for
                  running the actual queries
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*                           FROM PROPATH .
USING Consultingwerk.SmartComponents.Tools.BusinessEntityBrowser.*    FROM PROPATH .
USING Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.* FROM PROPATH .
USING Progress.Lang.*                                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Tools.BusinessEntityBrowser.BusinessEntityBrowserForm
    INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE dockableWindow1 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE businessEntityTreeViewControl1 AS Consultingwerk.SmartComponents.Tools.BusinessEntityBrowser.BusinessEntityTreeViewControl NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityBrowserFormUnpinnedTabAreaTop AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityBrowserFormUnpinnedTabAreaRight AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityBrowserFormUnpinnedTabAreaLeft AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityBrowserFormUnpinnedTabAreaBottom AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityBrowserFormAutoHideControl AS Infragistics.Win.UltraWinDock.AutoHideControl NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraDockManager1 AS Infragistics.Win.UltraWinDock.UltraDockManager NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraStatusBar1 AS Infragistics.Win.UltraWinStatusBar.UltraStatusBar NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabbedMdiManager1 AS Infragistics.Win.UltraWinTabbedMdi.UltraTabbedMdiManager NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea1 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.

    DEFINE PRIVATE VARIABLE oLoginController AS LoginController NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessEntityBrowserForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessEntityBrowserForm ():

        SUPER().
        InitializeComponent().

        /* Mike Fechner, Consultingwerk Ltd. 02.03.2015
           SCL-681 : Stop-After not supported on OE10.2B */
        IF PROVERSION BEGINS "10":U THEN DO:
            ultraToolbarsManager1:Tools["StopAfter":U]:SharedProps:Visible = FALSE .
        END.

        &IF NOT PROVERSION BEGINS "10.":U &THEN
        ultraToolbarsManager1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010 .
        ultraToolbarsManager1:Ribbon:ApplicationMenuButtonImage = ? .
        &ENDIF

        ASSIGN oLoginController = NEW LoginController (ultraToolbarsManager1,
                                                       THIS-OBJECT) .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SelectedClassNameChanged event of the
                 businessEntityTreeViewControl1 instance
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID businessEntityTreeViewControl1_SelectedClassNameChanged (sender AS System.Object,
                                                                                 e AS System.EventArgs):

        DEFINE VARIABLE cClassName AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE oForm      AS BusinessEntityQueryForm NO-UNDO .

        ASSIGN cClassName = businessEntityTreeViewControl1:SelectedClassName  .

        oForm = NEW BusinessEntityQueryForm ().
        oForm:Text = "Loading..." .
        oForm:MdiParent = THIS-OBJECT .
        oForm:StatusBarVisible = FALSE .
        oForm:Show () .

        oForm:SetBusinessEntity (cClassName) .

    END METHOD.

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
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("ribbon1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool20 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool20 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SelectAll":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool21 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool21 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SelectNone":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup2":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool22 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool22 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetData":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool23 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool23 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Filter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool8 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool8 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("BatchSize":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool9 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool9 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("StopAfter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup3":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE textBoxTool3 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
        textBoxTool3 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("tbCustomContext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool24 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool24 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("StopEntities":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup4 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("Info":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool25 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool25 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Data Source Query Info":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup5 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup5 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("Session Management":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Authenticate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("UltraToolbarMainMenu":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool6 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool6 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("File":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool7 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool7 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Data":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("UltraToolbarBusinessEntityTester":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool10 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool10 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("EntityName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool28 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool28 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetSchema":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool29 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool29 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetData":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool30 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool30 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Filter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool5 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool5 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("Batch Size":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool11 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool11 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("BatchSize":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar3 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar3 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("Service Manager":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool33 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool33 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("StopEntities":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar4 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar4 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("Custom Context":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool6 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool6 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("lblCustomContext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE textBoxTool4 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
        textBoxTool4 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("tbCustomContext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar5 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar5 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("Selection":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool35 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool35 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SelectAll":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool36 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool36 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SelectNone":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool8 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool8 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("File":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool37 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool37 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool9 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool9 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Data":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool38 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool38 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetSchema":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool39 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool39 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetData":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool40 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool40 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Filter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool41 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool41 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool12 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool12 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("EntityName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueList1 AS Infragistics.Win.ValueList NO-UNDO.
        valueList1 = NEW Infragistics.Win.ValueList(0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool42 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool42 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetSchema":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool43 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool43 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetData":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool44 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool44 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Filter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool45 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool45 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("RemoveFilter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool10 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool10 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("TreeContextMenu":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool46 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool46 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("RemoveFilter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool7 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool7 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("Batch Size":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool13 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool13 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("BatchSize":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueList2 AS Infragistics.Win.ValueList NO-UNDO.
        valueList2 = NEW Infragistics.Win.ValueList(0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem12 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem12 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem13 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem13 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem14 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem14 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem15 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem15 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem16 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem16 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool47 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool47 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("StopEntities":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool8 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool8 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("lblCustomContext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE textBoxTool6 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
        textBoxTool6 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("tbCustomContext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool48 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool48 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SelectAll":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool49 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool49 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SelectNone":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool14 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool14 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("StopAfter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueList3 AS Infragistics.Win.ValueList NO-UNDO.
        valueList3 = NEW Infragistics.Win.ValueList(0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem17 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem17 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem18 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem18 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem19 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem19 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem20 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem20 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem21 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem21 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem22 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem22 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool50 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool50 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Data Source Query Info":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Authenticate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane1 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane1 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedLeft, NEW System.Guid("19338eca-1497-4231-9ed4-ca2c9db78bc8":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane1 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane1 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("adb96796-d742-4b14-bf9a-80f72d71b219":U), NEW System.Guid("00000000-0000-0000-0000-000000000000":U), -1, NEW System.Guid("19338eca-1497-4231-9ed4-ca2c9db78bc8":U), -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.SmartComponents.Tools.BusinessEntityBrowser.BusinessEntityBrowserForm":U).
        THIS-OBJECT:businessEntityTreeViewControl1 = NEW Consultingwerk.SmartComponents.Tools.BusinessEntityBrowser.BusinessEntityTreeViewControl().
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:ultraDockManager1 = NEW Infragistics.Win.UltraWinDock.UltraDockManager(THIS-OBJECT:components).
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaLeft = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaRight = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaTop = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaBottom = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_BusinessEntityBrowserFormAutoHideControl = NEW Infragistics.Win.UltraWinDock.AutoHideControl().
        THIS-OBJECT:windowDockingArea1 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:dockableWindow1 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:ultraTabbedMdiManager1 = NEW Infragistics.Win.UltraWinTabbedMdi.UltraTabbedMdiManager(THIS-OBJECT:components).
        THIS-OBJECT:ultraStatusBar1 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusBar().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:windowDockingArea1:SuspendLayout().
        THIS-OBJECT:dockableWindow1:SuspendLayout().
        CAST(THIS-OBJECT:ultraTabbedMdiManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraStatusBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* businessEntityTreeViewControl1 */
        /*  */
        THIS-OBJECT:businessEntityTreeViewControl1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:businessEntityTreeViewControl1:Name = "businessEntityTreeViewControl1":U.
        THIS-OBJECT:businessEntityTreeViewControl1:Size = NEW System.Drawing.Size(286, 313).
        THIS-OBJECT:businessEntityTreeViewControl1:TabIndex = 15.
        THIS-OBJECT:businessEntityTreeViewControl1:SelectedClassNameChanged:Subscribe(THIS-OBJECT:businessEntityTreeViewControl1_SelectedClassNameChanged).
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.Forms.BaseForm":U).
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:ApplicationMenuButtonImage = CAST(resources:GetObject("ultraToolbarsManager1.Ribbon.ApplicationMenuButtonImage":U), System.Drawing.Image).
        ribbonTab1:Caption = "Browser":U.
        ribbonGroup1:Caption = "Table selection":U.
        buttonTool20:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool21:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar0[1] = buttonTool20.
        arrayvar0[2] = buttonTool21.
        ribbonGroup1:Tools:AddRange(arrayvar0).
        ribbonGroup2:Caption = "Data Retrieval":U.
        buttonTool22:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool23:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar1[1] = buttonTool22.
        arrayvar1[2] = buttonTool23.
        arrayvar1[3] = comboBoxTool8.
        arrayvar1[4] = comboBoxTool9.
        ribbonGroup2:Tools:AddRange(arrayvar1).
        ribbonGroup3:Caption = "Business Entity":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar2[1] = textBoxTool3.
        arrayvar2[2] = buttonTool24.
        ribbonGroup3:Tools:AddRange(arrayvar2).
        ribbonGroup4:Caption = "Info":U.
        buttonTool25:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar3[1] = buttonTool25.
        ribbonGroup4:Tools:AddRange(arrayvar3).
        ribbonGroup5:Caption = "Session Management":U.
        buttonTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar4[1] = buttonTool1.
        ribbonGroup5:Tools:AddRange(arrayvar4).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 5 NO-UNDO.
        arrayvar5[1] = ribbonGroup1.
        arrayvar5[2] = ribbonGroup2.
        arrayvar5[3] = ribbonGroup3.
        arrayvar5[4] = ribbonGroup4.
        arrayvar5[5] = ribbonGroup5.
        ribbonTab1:Groups:AddRange(arrayvar5).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar6[1] = ribbonTab1.
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar6).
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:Visible = TRUE.
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        THIS-OBJECT:ultraToolbarsManager1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2007.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        ultraToolbar1:IsMainMenuBar = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar7[1] = popupMenuTool6.
        arrayvar7[2] = popupMenuTool7.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar7).
        ultraToolbar1:Text = "UltraToolbarMainMenu":U.
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        labelTool5:InstanceProps:IsFirstInGroup = TRUE.
        comboBoxTool11:InstanceProps:Width = 89.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar8[1] = comboBoxTool10.
        arrayvar8[2] = buttonTool28.
        arrayvar8[3] = buttonTool29.
        arrayvar8[4] = buttonTool30.
        arrayvar8[5] = labelTool5.
        arrayvar8[6] = comboBoxTool11.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar8).
        ultraToolbar2:Text = "UltraToolbarBusinessEntityTester":U.
        ultraToolbar3:DockedColumn = 0.
        ultraToolbar3:DockedRow = 2.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar9[1] = buttonTool33.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar9).
        ultraToolbar3:Text = "Service Manager":U.
        ultraToolbar4:DockedColumn = 1.
        ultraToolbar4:DockedRow = 2.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar10[1] = labelTool6.
        arrayvar10[2] = textBoxTool4.
        ultraToolbar4:NonInheritedTools:AddRange(arrayvar10).
        ultraToolbar4:Text = "Custom Context":U.
        ultraToolbar5:DockedColumn = 2.
        ultraToolbar5:DockedRow = 2.
        ultraToolbar5:FloatingSize = NEW System.Drawing.Size(155, 24).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar11[1] = buttonTool35.
        arrayvar11[2] = buttonTool36.
        ultraToolbar5:NonInheritedTools:AddRange(arrayvar11).
        ultraToolbar5:Text = "Selection":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 5 NO-UNDO.
        arrayvar12[1] = ultraToolbar1.
        arrayvar12[2] = ultraToolbar2.
        arrayvar12[3] = ultraToolbar3.
        arrayvar12[4] = ultraToolbar4.
        arrayvar12[5] = ultraToolbar5.
        THIS-OBJECT:ultraToolbarsManager1:Toolbars:AddRange(arrayvar12).
        popupMenuTool8:SharedPropsInternal:Caption = "&File":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar13[1] = buttonTool37.
        popupMenuTool8:Tools:AddRange(arrayvar13).
        popupMenuTool9:SharedPropsInternal:Caption = "&Data":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar14[1] = buttonTool38.
        arrayvar14[2] = buttonTool39.
        arrayvar14[3] = buttonTool40.
        popupMenuTool9:Tools:AddRange(arrayvar14).
        buttonTool41:SharedPropsInternal:Caption = "&Exit":U.
        comboBoxTool12:DropDownStyle = Infragistics.Win.DropDownStyle:DropDown.
        comboBoxTool12:SharedPropsInternal:Caption = "EntityName":U.
        comboBoxTool12:SharedPropsInternal:Width = 290.
        comboBoxTool12:ValueList = valueList1.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        buttonTool42:SharedPropsInternal:AppearancesSmall:Appearance = appearance1.
        buttonTool42:SharedPropsInternal:Caption = "Get Schema":U.
        buttonTool42:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool43:SharedPropsInternal:AppearancesLarge:Appearance = appearance2.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        buttonTool43:SharedPropsInternal:AppearancesSmall:Appearance = appearance3.
        buttonTool43:SharedPropsInternal:Caption = "Get Data":U.
        buttonTool43:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        buttonTool43:SharedPropsInternal:Enabled = FALSE.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        buttonTool44:SharedPropsInternal:AppearancesLarge:Appearance = appearance4.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        buttonTool44:SharedPropsInternal:AppearancesSmall:Appearance = appearance5.
        buttonTool44:SharedPropsInternal:Caption = "Filter":U.
        buttonTool44:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        buttonTool44:SharedPropsInternal:Enabled = FALSE.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        buttonTool45:SharedPropsInternal:AppearancesLarge:Appearance = appearance6.
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        buttonTool45:SharedPropsInternal:AppearancesSmall:Appearance = appearance7.
        buttonTool45:SharedPropsInternal:Caption = "&RemoveFilter":U.
        buttonTool45:SharedPropsInternal:Enabled = FALSE.
        popupMenuTool10:SharedPropsInternal:Caption = "TreeContextMenu":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar15 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar15[1] = buttonTool46.
        popupMenuTool10:Tools:AddRange(arrayvar15).
        labelTool7:SharedPropsInternal:Caption = "Batch Size:":U.
        labelTool7:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:TextOnlyAlways.
        comboBoxTool13:AutoComplete = TRUE.
        comboBoxTool13:DropDownStyle = Infragistics.Win.DropDownStyle:DropDown.
        comboBoxTool13:SharedPropsInternal:Caption = "Batch Size":U.
        comboBoxTool13:Text = "100":U.
        valueListItem12:DataValue = System.Convert:ToInt16(100).
        valueListItem12:DisplayText = "100":U.
        valueListItem13:DataValue = System.Convert:ToInt16(200).
        valueListItem13:DisplayText = "200":U.
        valueListItem14:DataValue = System.Convert:ToInt16(500).
        valueListItem14:DisplayText = "500":U.
        valueListItem15:DataValue = System.Convert:ToInt16(1000).
        valueListItem15:DisplayText = "1000":U.
        valueListItem16:DataValue = System.Convert:ToInt16(0).
        valueListItem16:DisplayText = "(unlimited)":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar16 AS Infragistics.Win.ValueListItem EXTENT 5 NO-UNDO.
        arrayvar16[1] = valueListItem12.
        arrayvar16[2] = valueListItem13.
        arrayvar16[3] = valueListItem14.
        arrayvar16[4] = valueListItem15.
        arrayvar16[5] = valueListItem16.
        valueList2:ValueListItems:AddRange(arrayvar16).
        comboBoxTool13:ValueList = valueList2.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesLarge:Appearance = appearance8.
        appearance9:Image = CAST(resources:GetObject("appearance9.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesSmall:Appearance = appearance9.
        buttonTool47:SharedPropsInternal:Caption = "Stop Business Entities":U.
        buttonTool47:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        labelTool8:SharedPropsInternal:Caption = "Custom Context:":U.
        labelTool8:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:TextOnlyAlways.
        textBoxTool6:SharedPropsInternal:Caption = "Custom Context":U.
        textBoxTool6:SharedPropsInternal:Width = 200.
        appearance10:Image = CAST(resources:GetObject("appearance10.Image":U), System.Object).
        buttonTool48:SharedPropsInternal:AppearancesSmall:Appearance = appearance10.
        buttonTool48:SharedPropsInternal:Caption = "Select All":U.
        appearance11:Image = CAST(resources:GetObject("appearance11.Image":U), System.Object).
        buttonTool49:SharedPropsInternal:AppearancesSmall:Appearance = appearance11.
        buttonTool49:SharedPropsInternal:Caption = "Select None":U.
        comboBoxTool14:AutoComplete = TRUE.
        comboBoxTool14:DropDownStyle = Infragistics.Win.DropDownStyle:DropDown.
        comboBoxTool14:SharedPropsInternal:Caption = "Stop After":U.
        comboBoxTool14:Text = "5":U.
        valueListItem17:DataValue = System.Convert:ToInt16(0).
        valueListItem17:DisplayText = "(off)":U.
        valueListItem18:DataValue = System.Convert:ToInt16(1).
        valueListItem18:DisplayText = "1":U.
        valueListItem19:DataValue = System.Convert:ToInt16(5).
        valueListItem19:DisplayText = "5":U.
        valueListItem20:DataValue = System.Convert:ToInt16(15).
        valueListItem20:DisplayText = "15":U.
        valueListItem21:DataValue = System.Convert:ToInt16(30).
        valueListItem21:DisplayText = "30":U.
        valueListItem22:DataValue = System.Convert:ToInt16(60).
        valueListItem22:DisplayText = "60":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar17 AS Infragistics.Win.ValueListItem EXTENT 6 NO-UNDO.
        arrayvar17[1] = valueListItem17.
        arrayvar17[2] = valueListItem18.
        arrayvar17[3] = valueListItem19.
        arrayvar17[4] = valueListItem20.
        arrayvar17[5] = valueListItem21.
        arrayvar17[6] = valueListItem22.
        valueList3:ValueListItems:AddRange(arrayvar17).
        comboBoxTool14:ValueList = valueList3.
        appearance12:Image = CAST(resources:GetObject("appearance12.Image":U), System.Object).
        buttonTool50:SharedPropsInternal:AppearancesLarge:Appearance = appearance12.
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        buttonTool50:SharedPropsInternal:AppearancesSmall:Appearance = appearance13.
        buttonTool50:SharedPropsInternal:Caption = "Data Source Query Info":U.
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance14.
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance15.
        buttonTool2:SharedPropsInternal:Caption = "Authenticate":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar18 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 19 NO-UNDO.
        arrayvar18[1] = popupMenuTool8.
        arrayvar18[2] = popupMenuTool9.
        arrayvar18[3] = buttonTool41.
        arrayvar18[4] = comboBoxTool12.
        arrayvar18[5] = buttonTool42.
        arrayvar18[6] = buttonTool43.
        arrayvar18[7] = buttonTool44.
        arrayvar18[8] = buttonTool45.
        arrayvar18[9] = popupMenuTool10.
        arrayvar18[10] = labelTool7.
        arrayvar18[11] = comboBoxTool13.
        arrayvar18[12] = buttonTool47.
        arrayvar18[13] = labelTool8.
        arrayvar18[14] = textBoxTool6.
        arrayvar18[15] = buttonTool48.
        arrayvar18[16] = buttonTool49.
        arrayvar18[17] = comboBoxTool14.
        arrayvar18[18] = buttonTool50.
        arrayvar18[19] = buttonTool2.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar18).
        /*  */
        /* m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 162).
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Left:Name = "m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 331).
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(842, 162).
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Right:Name = "m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 331).
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Top:Name = "m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(850, 162).
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 493).
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Bottom:Name = "m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(850, 0).
        THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* ultraDockManager1 */
        /*  */
        THIS-OBJECT:ultraDockManager1:CompressUnpinnedTabs = FALSE.
        THIS-OBJECT:ultraDockManager1:DefaultPaneSettings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        dockableControlPane1:Control = THIS-OBJECT:businessEntityTreeViewControl1.
        dockableControlPane1:OriginalControlBounds = NEW System.Drawing.Rectangle(59, 189, 246, 494).
        dockableControlPane1:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane1:Text = "Package overview":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar19 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar19[1] = dockableControlPane1.
        dockAreaPane1:Panes:AddRange(arrayvar19).
        dockAreaPane1:Size = NEW System.Drawing.Size(286, 331).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar20 AS Infragistics.Win.UltraWinDock.DockAreaPane EXTENT 1 NO-UNDO.
        arrayvar20[1] = dockAreaPane1.
        THIS-OBJECT:ultraDockManager1:DockAreas:AddRange(arrayvar20).
        THIS-OBJECT:ultraDockManager1:DragIndicatorStyle = Infragistics.Win.UltraWinDock.DragIndicatorStyle:VisualStudio2008Vista.
        THIS-OBJECT:ultraDockManager1:DragWindowStyle = Infragistics.Win.UltraWinDock.DragWindowStyle:LayeredWindowWithIndicators.
        THIS-OBJECT:ultraDockManager1:HostControl = THIS-OBJECT.
        THIS-OBJECT:ultraDockManager1:WindowStyle = Infragistics.Win.UltraWinDock.WindowStyle:Office2007.
        /*  */
        /* m_BusinessEntityBrowserFormUnpinnedTabAreaLeft */
        /*  */
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaLeft:Dock = System.Windows.Forms.DockStyle:Left.
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaLeft:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaLeft:Location = NEW System.Drawing.Point(8, 162).
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaLeft:Name = "m_BusinessEntityBrowserFormUnpinnedTabAreaLeft":U.
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaLeft:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaLeft:Size = NEW System.Drawing.Size(0, 331).
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaLeft:TabIndex = 5.
        /*  */
        /* m_BusinessEntityBrowserFormUnpinnedTabAreaRight */
        /*  */
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaRight:Dock = System.Windows.Forms.DockStyle:Right.
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaRight:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaRight:Location = NEW System.Drawing.Point(842, 162).
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaRight:Name = "m_BusinessEntityBrowserFormUnpinnedTabAreaRight":U.
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaRight:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaRight:Size = NEW System.Drawing.Size(0, 331).
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaRight:TabIndex = 6.
        /*  */
        /* m_BusinessEntityBrowserFormUnpinnedTabAreaTop */
        /*  */
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaTop:Dock = System.Windows.Forms.DockStyle:Top.
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaTop:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaTop:Location = NEW System.Drawing.Point(8, 162).
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaTop:Name = "m_BusinessEntityBrowserFormUnpinnedTabAreaTop":U.
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaTop:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaTop:Size = NEW System.Drawing.Size(834, 0).
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaTop:TabIndex = 7.
        /*  */
        /* m_BusinessEntityBrowserFormUnpinnedTabAreaBottom */
        /*  */
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaBottom:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaBottom:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaBottom:Location = NEW System.Drawing.Point(8, 493).
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaBottom:Name = "m_BusinessEntityBrowserFormUnpinnedTabAreaBottom":U.
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaBottom:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaBottom:Size = NEW System.Drawing.Size(834, 0).
        THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaBottom:TabIndex = 8.
        /*  */
        /* m_BusinessEntityBrowserFormAutoHideControl */
        /*  */
        THIS-OBJECT:m_BusinessEntityBrowserFormAutoHideControl:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_BusinessEntityBrowserFormAutoHideControl:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_BusinessEntityBrowserFormAutoHideControl:Name = "m_BusinessEntityBrowserFormAutoHideControl":U.
        THIS-OBJECT:m_BusinessEntityBrowserFormAutoHideControl:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_BusinessEntityBrowserFormAutoHideControl:Size = NEW System.Drawing.Size(0, 0).
        THIS-OBJECT:m_BusinessEntityBrowserFormAutoHideControl:TabIndex = 9.
        /*  */
        /* windowDockingArea1 */
        /*  */
        THIS-OBJECT:windowDockingArea1:Controls:Add(THIS-OBJECT:dockableWindow1).
        THIS-OBJECT:windowDockingArea1:Dock = System.Windows.Forms.DockStyle:Left.
        THIS-OBJECT:windowDockingArea1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea1:Location = NEW System.Drawing.Point(8, 162).
        THIS-OBJECT:windowDockingArea1:Name = "windowDockingArea1":U.
        THIS-OBJECT:windowDockingArea1:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea1:Size = NEW System.Drawing.Size(291, 331).
        THIS-OBJECT:windowDockingArea1:TabIndex = 16.
        /*  */
        /* dockableWindow1 */
        /*  */
        THIS-OBJECT:dockableWindow1:Controls:Add(THIS-OBJECT:businessEntityTreeViewControl1).
        THIS-OBJECT:dockableWindow1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:dockableWindow1:Name = "dockableWindow1":U.
        THIS-OBJECT:dockableWindow1:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow1:Size = NEW System.Drawing.Size(286, 331).
        THIS-OBJECT:dockableWindow1:TabIndex = 25.
        /*  */
        /* ultraTabbedMdiManager1 */
        /*  */
        THIS-OBJECT:ultraTabbedMdiManager1:AllowMaximize = TRUE.
        THIS-OBJECT:ultraTabbedMdiManager1:AllowNestedTabGroups = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraTabbedMdiManager1:MdiParent = THIS-OBJECT.
        THIS-OBJECT:ultraTabbedMdiManager1:TabGroupSettings:CloseButtonLocation = Infragistics.Win.UltraWinTabs.TabCloseButtonLocation:Tab.
        THIS-OBJECT:ultraTabbedMdiManager1:ViewStyle = Infragistics.Win.UltraWinTabbedMdi.ViewStyle:Office2007.
        /*  */
        /* ultraStatusBar1 */
        /*  */
        THIS-OBJECT:ultraStatusBar1:Location = NEW System.Drawing.Point(0, 493).
        THIS-OBJECT:ultraStatusBar1:Name = "ultraStatusBar1":U.
        THIS-OBJECT:ultraStatusBar1:Size = NEW System.Drawing.Size(850, 23).
        THIS-OBJECT:ultraStatusBar1:TabIndex = 22.
        THIS-OBJECT:ultraStatusBar1:ViewStyle = Infragistics.Win.UltraWinStatusBar.ViewStyle:Office2007.
        /*  */
        /* BusinessEntityBrowserForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(850, 516).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityBrowserFormAutoHideControl).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaTop).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaBottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaLeft).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityBrowserFormUnpinnedTabAreaRight).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraStatusBar1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityBrowserForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:IsMdiContainer = TRUE.
        THIS-OBJECT:Name = "BusinessEntityBrowserForm":U.
        THIS-OBJECT:Text = "Business Entity Browser":U.
        THIS-OBJECT:WindowPositionRegistryKey = "BusinessEntityBrowserForm":U.
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:windowDockingArea1:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraTabbedMdiManager1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraStatusBar1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the MdiChildActivate event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnMdiChildActivate (e AS System.EventArgs):

        SUPER:OnMdiChildActivate (e) .

        THIS-OBJECT:UpdateStatusBarText() .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown (e) .

        Consultingwerk.Util.UserInterfaceHelper:ProcessEvents() .

        businessEntityTreeViewControl1:LoadBusinessEntities() .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Updates the Status Bar Text
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateStatusBarText ():

        IF VALID-OBJECT (THIS-OBJECT:ActiveMdiChild) AND TYPE-OF (THIS-OBJECT:ActiveMdiChild, BusinessEntityQueryForm) THEN
            ultraStatusBar1:Text = CAST (THIS-OBJECT:ActiveMdiChild, BusinessEntityQueryForm):StatusBarText .
        ELSE
            ultraStatusBar1:Text = "" .


    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the BusinessEntityBrowserForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC BusinessEntityBrowserForm ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
