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
    File        : SmartObjectTypeForm
    Purpose     : Maintenance of SmartObject
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Sep 29 13:40:50 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*               FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*               FROM PROPATH.
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Consultingwerk.Windows.Framework.Repository.Class.* FROM PROPATH .
USING Consultingwerk.Windows.Util.*                       FROM PROPATH .
USING Infragistics.Win.UltraWinTree.*                     FROM ASSEMBLY .
USING Progress.Lang.*                                     FROM PROPATH .
USING Progress.Util.*                                     FROM ASSEMBLY .
USING System.Windows.Forms.*                              FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Framework.Repository.Class.SmartObjectTypeForm
    INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE dockableWindow1 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartObjectTypeFormUnpinnedTabAreaTop AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartObjectTypeFormUnpinnedTabAreaRight AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartObjectTypeFormUnpinnedTabAreaLeft AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartObjectTypeFormUnpinnedTabAreaBottom AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartObjectTypeFormAutoHideControl AS Infragistics.Win.UltraWinDock.AutoHideControl NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartObjectTypeForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartObjectTypeForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartObjectTypeForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartObjectTypeForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE objectTypeAttributesUserControl1 AS Consultingwerk.Windows.Framework.Repository.Class.ObjectTypeAttributesUserControl NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityAdapter2 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDataBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartObjectTypeViewerControl1 AS Consultingwerk.Windows.Framework.Repository.Class.SmartObjectTypeViewerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE smartPanelController1 AS Consultingwerk.SmartComponents.Implementation.SmartPanelController NO-UNDO.
    DEFINE PRIVATE VARIABLE smartSupportedLinkViewerControl1 AS Consultingwerk.Windows.Framework.Repository.Class.SmartSupportedLinkViewerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.
    DEFINE PRIVATE VARIABLE SmartObjectTypeForm_Fill_Panel AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbSearchField AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea1 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTree1 AS Infragistics.Win.UltraWinTree.UltraTree NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraDockManager1 AS Infragistics.Win.UltraWinDock.UltraDockManager NO-UNDO.

    DEFINE VARIABLE oTreeModel AS SmartObjectTreeModel NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartObjectTypeForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartObjectTypeForm ():

        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Fetches Object Types from the Backend and populates the TreeView
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FetchObjectTypes ():

        smartBusinessEntityAdapter1:RetrieveData() .

        IF NOT VALID-OBJECT (oTreeModel) THEN
            THIS-OBJECT:InitializeTreeModel () .

        oTreeModel:PopulateTreeView() .

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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Class.SmartObjectTypeForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("eSmartSupportedLink":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SupportedLinkGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ObjectTypeGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LinkTypeGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LinkSource":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LinkTarget":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn6 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn6 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("DeactivateOnHide":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn7 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn7 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LinkName":U).
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
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartSupportedLink":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
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
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("start":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("navigation":U).
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
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("maintenance":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool1 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool1 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Add record":U).
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
        DEFINE VARIABLE buttonTool16 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool16 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool17 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool17 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("transaction":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
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
        DEFINE VARIABLE buttonTool27 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool27 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool28 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool28 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
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
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool36 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool36 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool37 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool37 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool38 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool38 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool39 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool39 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool40 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool40 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool41 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool41 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool42 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool42 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool43 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool43 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool44 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool44 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool45 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool45 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool46 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool46 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
        appearance28 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
        appearance29 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool47 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool47 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
        appearance30 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
        appearance31 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool2 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool2 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Add record":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
        appearance32 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool50 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool50 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool51 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool51 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Add child":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool52 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool52 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Add child":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane1 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane1 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedLeft, NEW System.Guid("0f0c179c-3bd0-4b8b-8cc6-07e2019cc38e":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane1 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane1 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("1319dc54-a1de-4c5a-a1ce-d497755ad355":U), NEW System.Guid("00000000-0000-0000-0000-000000000000":U), -1, NEW System.Guid("0f0c179c-3bd0-4b8b-8cc6-07e2019cc38e":U), -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:objectTypeAttributesUserControl1 = NEW Consultingwerk.Windows.Framework.Repository.Class.ObjectTypeAttributesUserControl().
        THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartDataBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartBusinessEntityAdapter2 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter(THIS-OBJECT:components).
        THIS-OBJECT:smartBusinessEntityAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter(THIS-OBJECT:components).
        THIS-OBJECT:smartSupportedLinkViewerControl1 = NEW Consultingwerk.Windows.Framework.Repository.Class.SmartSupportedLinkViewerControl().
        THIS-OBJECT:smartPanelController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartPanelController().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:ultraTree1 = NEW Infragistics.Win.UltraWinTree.UltraTree().
        THIS-OBJECT:tbSearchField = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:SmartObjectTypeForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:smartObjectTypeViewerControl1 = NEW Consultingwerk.Windows.Framework.Repository.Class.SmartObjectTypeViewerControl().
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:ultraDockManager1 = NEW Infragistics.Win.UltraWinDock.UltraDockManager(THIS-OBJECT:components).
        THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaLeft = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaRight = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaTop = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaBottom = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_SmartObjectTypeFormAutoHideControl = NEW Infragistics.Win.UltraWinDock.AutoHideControl().
        THIS-OBJECT:windowDockingArea1 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:dockableWindow1 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        THIS-OBJECT:ultraTabPageControl2:SuspendLayout().
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityAdapter2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityAdapter1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartSupportedLinkViewerControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbSearchField, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SmartObjectTypeForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:SmartObjectTypeForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        CAST(THIS-OBJECT:smartObjectTypeViewerControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:windowDockingArea1:SuspendLayout().
        THIS-OBJECT:dockableWindow1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:objectTypeAttributesUserControl1).
        resources:ApplyResources(THIS-OBJECT:ultraTabPageControl1, "ultraTabPageControl1":U).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        /*  */
        /* objectTypeAttributesUserControl1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:objectTypeAttributesUserControl1, "objectTypeAttributesUserControl1":U).
        THIS-OBJECT:objectTypeAttributesUserControl1:Name = "objectTypeAttributesUserControl1":U.
        THIS-OBJECT:objectTypeAttributesUserControl1:HasChangesChanged:Subscribe(THIS-OBJECT:objectTypeAttributesUserControl1_HasChangesChanged).
        /*  */
        /* ultraTabPageControl2 */
        /*  */
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartDataBrowser1).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartSupportedLinkViewerControl1).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartPanelController1).
        resources:ApplyResources(THIS-OBJECT:ultraTabPageControl2, "ultraTabPageControl2":U).
        THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2":U.
        /*  */
        /* smartDataBrowser1 */
        /*  */
        THIS-OBJECT:smartDataBrowser1:DataSource = THIS-OBJECT:smartBusinessEntityBindingSource1.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Appearance = appearance1.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn1:Header, "ultraGridColumn1.Header":U).
        ultraGridColumn1:Header:VisiblePosition = 1.
        ultraGridColumn1:Hidden = TRUE.
        ultraGridColumn1:ForceApplyResources = "Header":U.
        ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn2:Header, "ultraGridColumn2.Header":U).
        ultraGridColumn2:Header:VisiblePosition = 2.
        ultraGridColumn2:Hidden = TRUE.
        ultraGridColumn2:ForceApplyResources = "Header":U.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn3:Header, "ultraGridColumn3.Header":U).
        ultraGridColumn3:Header:VisiblePosition = 3.
        ultraGridColumn3:Hidden = TRUE.
        ultraGridColumn3:ForceApplyResources = "Header":U.
        ultraGridColumn4:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn4:Header, "ultraGridColumn4.Header":U).
        ultraGridColumn4:Header:VisiblePosition = 4.
        ultraGridColumn4:ForceApplyResources = "Header":U.
        ultraGridColumn5:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn5:Header, "ultraGridColumn5.Header":U).
        ultraGridColumn5:Header:VisiblePosition = 5.
        ultraGridColumn5:ForceApplyResources = "Header":U.
        ultraGridColumn6:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn6:Header, "ultraGridColumn6.Header":U).
        ultraGridColumn6:Header:VisiblePosition = 6.
        ultraGridColumn6:ForceApplyResources = "Header":U.
        ultraGridColumn7:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn7:Header, "ultraGridColumn7.Header":U).
        ultraGridColumn7:Header:VisiblePosition = 0.
        ultraGridColumn7:Width = 176.
        ultraGridColumn7:ForceApplyResources = "Header":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS System.Object EXTENT 7 NO-UNDO.
        arrayvar0[1] = ultraGridColumn1.
        arrayvar0[2] = ultraGridColumn2.
        arrayvar0[3] = ultraGridColumn3.
        arrayvar0[4] = ultraGridColumn4.
        arrayvar0[5] = ultraGridColumn5.
        arrayvar0[6] = ultraGridColumn6.
        arrayvar0[7] = ultraGridColumn7.
        ultraGridBand1:Columns:AddRange(arrayvar0).
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
        resources:ApplyResources(THIS-OBJECT:smartDataBrowser1, "smartDataBrowser1":U).
        THIS-OBJECT:smartDataBrowser1:LinkDataSource = THIS-OBJECT:smartBusinessEntityAdapter2.
        THIS-OBJECT:smartDataBrowser1:Name = "smartDataBrowser1":U.
        THIS-OBJECT:smartDataBrowser1:SettingsKey = "a7ee681a-3094-18aa-0e14-dcd98ce14966":U.
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Repository.Class.SupportedLinkBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartSupportedLink":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Progress.Data.ColumnPropDesc EXTENT 7 NO-UNDO.
        arrayvar1[1] = NEW Progress.Data.ColumnPropDesc("SupportedLinkGuid":U, "SupportedLinkGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[2] = NEW Progress.Data.ColumnPropDesc("ObjectTypeGuid":U, "ObjectTypeGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[3] = NEW Progress.Data.ColumnPropDesc("LinkTypeGuid":U, "LinkTypeGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[4] = NEW Progress.Data.ColumnPropDesc("LinkSource":U, "Supported as source":U, Progress.Data.DataType:LOGICAL).
        arrayvar1[5] = NEW Progress.Data.ColumnPropDesc("LinkTarget":U, "Supported as target":U, Progress.Data.DataType:LOGICAL).
        arrayvar1[6] = NEW Progress.Data.ColumnPropDesc("DeactivateOnHide":U, "DeactivateLinkOnHide":U, Progress.Data.DataType:LOGICAL).
        arrayvar1[7] = NEW Progress.Data.ColumnPropDesc("LinkName":U, "Link Name":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar1.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* smartBusinessEntityAdapter2 */
        /*  */
        THIS-OBJECT:smartBusinessEntityAdapter2:EntityName = "Consultingwerk.SmartFramework.Repository.Class.SupportedLinkBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityAdapter2:EntityTable = "eSmartSupportedLink":U.
        THIS-OBJECT:smartBusinessEntityAdapter2:ForeignFields = "ObjectTypeGuid,ObjectTypeGuid":U.
        THIS-OBJECT:smartBusinessEntityAdapter2:LinkCommitSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter2:LinkDataSource = THIS-OBJECT:smartBusinessEntityAdapter1.
        THIS-OBJECT:smartBusinessEntityAdapter2:LinkNavigationSource = ?.
        /*  */
        /* smartBusinessEntityAdapter1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityAdapter1:BatchSize = 0.
        THIS-OBJECT:smartBusinessEntityAdapter1:DatasetControllerType = "Consultingwerk.SmartFramework.Repository.Class.ObjectTypeDatasetController":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:EntityName = "Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:EntityTable = "eSmartObjectType":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkCommitSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkNavigationSource = ?.
        /*  */
        /* smartSupportedLinkViewerControl1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartSupportedLinkViewerControl1, "smartSupportedLinkViewerControl1":U).
        THIS-OBJECT:smartSupportedLinkViewerControl1:FocusControlOnAdd = ?.
        THIS-OBJECT:smartSupportedLinkViewerControl1:FocusControlOnUpdate = ?.
        THIS-OBJECT:smartSupportedLinkViewerControl1:LinkDataSource = THIS-OBJECT:smartBusinessEntityAdapter2.
        THIS-OBJECT:smartSupportedLinkViewerControl1:LinkGroupAssignSource = ?.
        THIS-OBJECT:smartSupportedLinkViewerControl1:LinkTableIOSource = THIS-OBJECT:smartPanelController1.
        THIS-OBJECT:smartSupportedLinkViewerControl1:Name = "smartSupportedLinkViewerControl1":U.
        /*  */
        /* smartPanelController1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartPanelController1, "smartPanelController1":U).
        THIS-OBJECT:smartPanelController1:Name = "smartPanelController1":U.
        /*  */
        /* panel1 */
        /*  */
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraTree1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbSearchField).
        resources:ApplyResources(THIS-OBJECT:panel1, "panel1":U).
        THIS-OBJECT:panel1:Name = "panel1":U.
        /*  */
        /* ultraTree1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTree1, "ultraTree1":U).
        THIS-OBJECT:ultraTree1:Name = "ultraTree1":U.
        /*  */
        /* tbSearchField */
        /*  */
        THIS-OBJECT:tbSearchField:AcceptsReturn = TRUE.
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        THIS-OBJECT:tbSearchField:Appearance = appearance13.
        editorButton1:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:tbSearchField:ButtonsRight:Add(editorButton1).
        resources:ApplyResources(THIS-OBJECT:tbSearchField, "tbSearchField":U).
        THIS-OBJECT:tbSearchField:LookupBrowserFields = "ObjectTypeName,ObjectTypeDescription":U.
        THIS-OBJECT:tbSearchField:LookupDialogFilterFields = "ObjectTypeName,ObjectTypeDescription":U.
        THIS-OBJECT:tbSearchField:LookupDialogFilterOperators = "BEGINSMATCHES,BEGINSMATCHES":U.
        THIS-OBJECT:tbSearchField:LookupEntityName = "Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity":U.
        THIS-OBJECT:tbSearchField:LookupEntityTable = "eSmartObjectType":U.
        THIS-OBJECT:tbSearchField:LookupKeyField = "ObjectTypeName":U.
        THIS-OBJECT:tbSearchField:LookupQueryString = "FOR EACH eSmartObjectType WHERE eSmartObjectType.ObjectTypeName BEGINS ~"&1~"":U.
        THIS-OBJECT:tbSearchField:Name = "tbSearchField":U.
        THIS-OBJECT:tbSearchField:OwningGrid = ?.
        THIS-OBJECT:tbSearchField:LookupComplete:Subscribe(THIS-OBJECT:tbSearchField_LookupComplete).
        THIS-OBJECT:tbSearchField:KeyDown:Subscribe(THIS-OBJECT:tbSearchField_KeyDown).
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar2[1] = buttonTool1.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar2).
        buttonTool5:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar3[1] = buttonTool2.
        arrayvar3[2] = buttonTool3.
        arrayvar3[3] = buttonTool4.
        arrayvar3[4] = buttonTool5.
        arrayvar3[5] = buttonTool6.
        arrayvar3[6] = buttonTool7.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar3).
        resources:ApplyResources(ribbonTab1, "ribbonTab1":U).
        resources:ApplyResources(ribbonGroup1, "ribbonGroup1":U).
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
        ribbonGroup1:Tools:AddRange(arrayvar4).
        resources:ApplyResources(ribbonGroup2, "ribbonGroup2":U).
        popupMenuTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool13:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool13:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool14:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool14:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool15:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool16:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool17:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar5[1] = popupMenuTool1.
        arrayvar5[2] = buttonTool13.
        arrayvar5[3] = buttonTool14.
        arrayvar5[4] = buttonTool15.
        arrayvar5[5] = buttonTool16.
        arrayvar5[6] = buttonTool17.
        ribbonGroup2:Tools:AddRange(arrayvar5).
        resources:ApplyResources(ribbonGroup3, "ribbonGroup3":U).
        buttonTool18:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool19:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar6[1] = buttonTool18.
        arrayvar6[2] = buttonTool19.
        ribbonGroup3:Tools:AddRange(arrayvar6).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 3 NO-UNDO.
        arrayvar7[1] = ribbonGroup1.
        arrayvar7[2] = ribbonGroup2.
        arrayvar7[3] = ribbonGroup3.
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
        buttonTool27:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar11[1] = buttonTool27.
        arrayvar11[2] = buttonTool28.
        arrayvar11[3] = buttonTool29.
        arrayvar11[4] = buttonTool30.
        arrayvar11[5] = buttonTool31.
        arrayvar11[6] = buttonTool32.
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
        appearance17:Image = CAST(resources:GetObject("appearance17.Image":U), System.Object).
        buttonTool35:SharedPropsInternal:AppearancesSmall:Appearance = appearance17.
        resources:ApplyResources(buttonTool35:SharedPropsInternal, "buttonTool35.SharedPropsInternal":U).
        buttonTool35:SharedPropsInternal:Enabled = FALSE.
        buttonTool35:ForceApplyResources = "SharedPropsInternal":U.
        appearance18:Image = CAST(resources:GetObject("appearance18.Image":U), System.Object).
        buttonTool36:SharedPropsInternal:AppearancesSmall:Appearance = appearance18.
        resources:ApplyResources(buttonTool36:SharedPropsInternal, "buttonTool36.SharedPropsInternal":U).
        buttonTool36:SharedPropsInternal:Enabled = FALSE.
        buttonTool36:ForceApplyResources = "SharedPropsInternal":U.
        appearance19:Image = CAST(resources:GetObject("appearance19.Image":U), System.Object).
        buttonTool37:SharedPropsInternal:AppearancesSmall:Appearance = appearance19.
        resources:ApplyResources(buttonTool37:SharedPropsInternal, "buttonTool37.SharedPropsInternal":U).
        buttonTool37:SharedPropsInternal:Enabled = FALSE.
        buttonTool37:ForceApplyResources = "SharedPropsInternal":U.
        appearance20:Image = CAST(resources:GetObject("appearance20.Image":U), System.Object).
        buttonTool38:SharedPropsInternal:AppearancesSmall:Appearance = appearance20.
        resources:ApplyResources(buttonTool38:SharedPropsInternal, "buttonTool38.SharedPropsInternal":U).
        buttonTool38:SharedPropsInternal:Enabled = FALSE.
        buttonTool38:ForceApplyResources = "SharedPropsInternal":U.
        appearance21:Image = CAST(resources:GetObject("appearance21.Image":U), System.Object).
        buttonTool39:SharedPropsInternal:AppearancesSmall:Appearance = appearance21.
        resources:ApplyResources(buttonTool39:SharedPropsInternal, "buttonTool39.SharedPropsInternal":U).
        buttonTool39:SharedPropsInternal:Enabled = FALSE.
        buttonTool39:ForceApplyResources = "SharedPropsInternal":U.
        appearance22:Image = CAST(resources:GetObject("appearance22.Image":U), System.Object).
        buttonTool40:SharedPropsInternal:AppearancesSmall:Appearance = appearance22.
        resources:ApplyResources(buttonTool40:SharedPropsInternal, "buttonTool40.SharedPropsInternal":U).
        buttonTool40:SharedPropsInternal:Enabled = FALSE.
        buttonTool40:ForceApplyResources = "SharedPropsInternal":U.
        appearance23:Image = CAST(resources:GetObject("appearance23.Image":U), System.Object).
        buttonTool41:SharedPropsInternal:AppearancesSmall:Appearance = appearance23.
        resources:ApplyResources(buttonTool41:SharedPropsInternal, "buttonTool41.SharedPropsInternal":U).
        buttonTool41:SharedPropsInternal:Enabled = FALSE.
        buttonTool41:ForceApplyResources = "SharedPropsInternal":U.
        appearance24:Image = CAST(resources:GetObject("appearance24.Image":U), System.Object).
        buttonTool42:SharedPropsInternal:AppearancesSmall:Appearance = appearance24.
        resources:ApplyResources(buttonTool42:SharedPropsInternal, "buttonTool42.SharedPropsInternal":U).
        buttonTool42:SharedPropsInternal:Enabled = FALSE.
        buttonTool42:ForceApplyResources = "SharedPropsInternal":U.
        appearance25:Image = CAST(resources:GetObject("appearance25.Image":U), System.Object).
        buttonTool43:SharedPropsInternal:AppearancesSmall:Appearance = appearance25.
        resources:ApplyResources(buttonTool43:SharedPropsInternal, "buttonTool43.SharedPropsInternal":U).
        buttonTool43:SharedPropsInternal:Enabled = FALSE.
        buttonTool43:ForceApplyResources = "SharedPropsInternal":U.
        appearance26:Image = CAST(resources:GetObject("appearance26.Image":U), System.Object).
        buttonTool44:SharedPropsInternal:AppearancesSmall:Appearance = appearance26.
        resources:ApplyResources(buttonTool44:SharedPropsInternal, "buttonTool44.SharedPropsInternal":U).
        buttonTool44:SharedPropsInternal:Enabled = FALSE.
        buttonTool44:ForceApplyResources = "SharedPropsInternal":U.
        appearance27:Image = CAST(resources:GetObject("appearance27.Image":U), System.Object).
        buttonTool45:SharedPropsInternal:AppearancesSmall:Appearance = appearance27.
        resources:ApplyResources(buttonTool45:SharedPropsInternal, "buttonTool45.SharedPropsInternal":U).
        buttonTool45:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        buttonTool45:ForceApplyResources = "SharedPropsInternal":U.
        appearance28:Image = CAST(resources:GetObject("appearance28.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesLarge:Appearance = appearance28.
        appearance29:Image = CAST(resources:GetObject("appearance29.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesSmall:Appearance = appearance29.
        resources:ApplyResources(buttonTool46:SharedPropsInternal, "buttonTool46.SharedPropsInternal":U).
        buttonTool46:SharedPropsInternal:Enabled = FALSE.
        buttonTool46:ForceApplyResources = "SharedPropsInternal":U.
        appearance30:Image = CAST(resources:GetObject("appearance30.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesLarge:Appearance = appearance30.
        appearance31:Image = CAST(resources:GetObject("appearance31.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesSmall:Appearance = appearance31.
        resources:ApplyResources(buttonTool47:SharedPropsInternal, "buttonTool47.SharedPropsInternal":U).
        buttonTool47:SharedPropsInternal:Enabled = FALSE.
        buttonTool47:ForceApplyResources = "SharedPropsInternal":U.
        appearance32:Image = CAST(resources:GetObject("appearance32.Image":U), System.Object).
        popupMenuTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance32.
        resources:ApplyResources(popupMenuTool2:SharedPropsInternal, "popupMenuTool2.SharedPropsInternal":U).
        buttonTool50:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool50:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar14[1] = buttonTool50.
        arrayvar14[2] = buttonTool51.
        popupMenuTool2:Tools:AddRange(arrayvar14).
        popupMenuTool2:ForceApplyResources = "SharedPropsInternal":U.
        resources:ApplyResources(buttonTool52:SharedPropsInternal, "buttonTool52.SharedPropsInternal":U).
        buttonTool52:ForceApplyResources = "SharedPropsInternal":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar15 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 15 NO-UNDO.
        arrayvar15[1] = buttonTool35.
        arrayvar15[2] = buttonTool36.
        arrayvar15[3] = buttonTool37.
        arrayvar15[4] = buttonTool38.
        arrayvar15[5] = buttonTool39.
        arrayvar15[6] = buttonTool40.
        arrayvar15[7] = buttonTool41.
        arrayvar15[8] = buttonTool42.
        arrayvar15[9] = buttonTool43.
        arrayvar15[10] = buttonTool44.
        arrayvar15[11] = buttonTool45.
        arrayvar15[12] = buttonTool46.
        arrayvar15[13] = buttonTool47.
        arrayvar15[14] = popupMenuTool2.
        arrayvar15[15] = buttonTool52.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar15).
        THIS-OBJECT:smartToolbarController1:ToolClick:Subscribe(THIS-OBJECT:smartToolbarController1_ToolClick).
        /*  */
        /* SmartObjectTypeForm_Fill_Panel */
        /*  */
        /*  */
        /* SmartObjectTypeForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:SmartObjectTypeForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:SmartObjectTypeForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:smartObjectTypeViewerControl1).
        THIS-OBJECT:SmartObjectTypeForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        resources:ApplyResources(THIS-OBJECT:SmartObjectTypeForm_Fill_Panel, "SmartObjectTypeForm_Fill_Panel":U).
        THIS-OBJECT:SmartObjectTypeForm_Fill_Panel:Name = "SmartObjectTypeForm_Fill_Panel":U.
        /*  */
        /* ultraTabControl1 */
        /*  */
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl2).
        resources:ApplyResources(THIS-OBJECT:ultraTabControl1, "ultraTabControl1":U).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        ultraTab1:Appearance = appearance15.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        resources:ApplyResources(ultraTab1, "ultraTab1":U).
        ultraTab1:ForceApplyResources = "":U.
        appearance16:Image = CAST(resources:GetObject("appearance16.Image":U), System.Object).
        ultraTab2:Appearance = appearance16.
        ultraTab2:TabPage = THIS-OBJECT:ultraTabPageControl2.
        resources:ApplyResources(ultraTab2, "ultraTab2":U).
        ultraTab2:ForceApplyResources = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar16 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 2 NO-UNDO.
        arrayvar16[1] = ultraTab1.
        arrayvar16[2] = ultraTab2.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar16).
        THIS-OBJECT:ultraTabControl1:ViewStyle = Infragistics.Win.UltraWinTabControl.ViewStyle:Office2007.
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTabSharedControlsPage1, "ultraTabSharedControlsPage1":U).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        /*  */
        /* smartObjectTypeViewerControl1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartObjectTypeViewerControl1, "smartObjectTypeViewerControl1":U).
        THIS-OBJECT:smartObjectTypeViewerControl1:FocusControlOnAdd = ?.
        THIS-OBJECT:smartObjectTypeViewerControl1:FocusControlOnUpdate = ?.
        THIS-OBJECT:smartObjectTypeViewerControl1:LinkDataSource = THIS-OBJECT:smartBusinessEntityAdapter1.
        THIS-OBJECT:smartObjectTypeViewerControl1:LinkGroupAssignSource = ?.
        THIS-OBJECT:smartObjectTypeViewerControl1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartObjectTypeViewerControl1:Name = "smartObjectTypeViewerControl1":U.
        THIS-OBJECT:smartObjectTypeViewerControl1:AfterAddRecord:Subscribe(THIS-OBJECT:smartObjectTypeViewerControl1_AfterAddRecord).
        THIS-OBJECT:smartObjectTypeViewerControl1:AfterCancelUpdate:Subscribe(THIS-OBJECT:smartObjectTypeViewerControl1_AfterCancelUpdate).
        THIS-OBJECT:smartObjectTypeViewerControl1:AfterSaveChanges:Subscribe(THIS-OBJECT:smartObjectTypeViewerControl1_AfterSaveChanges).
        /*  */
        /* m_SmartObjectTypeForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Left, "m_SmartObjectTypeForm_Toolbars_Dock_Area_Left":U).
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Left:Name = "m_SmartObjectTypeForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartObjectTypeForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Right, "m_SmartObjectTypeForm_Toolbars_Dock_Area_Right":U).
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Right:Name = "m_SmartObjectTypeForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartObjectTypeForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        resources:ApplyResources(THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Top, "m_SmartObjectTypeForm_Toolbars_Dock_Area_Top":U).
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Top:Name = "m_SmartObjectTypeForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartObjectTypeForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Bottom, "m_SmartObjectTypeForm_Toolbars_Dock_Area_Bottom":U).
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Bottom:Name = "m_SmartObjectTypeForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* ultraDockManager1 */
        /*  */
        THIS-OBJECT:ultraDockManager1:CompressUnpinnedTabs = FALSE.
        dockableControlPane1:Control = THIS-OBJECT:panel1.
        dockableControlPane1:OriginalControlBounds = NEW System.Drawing.Rectangle(128, 74, 200, 100).
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        dockableControlPane1:Settings:Appearance = appearance14.
        dockableControlPane1:Size = NEW System.Drawing.Size(100, 100).
        resources:ApplyResources(dockableControlPane1, "dockableControlPane1":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar17 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar17[1] = dockableControlPane1.
        dockAreaPane1:Panes:AddRange(arrayvar17).
        dockAreaPane1:Size = NEW System.Drawing.Size(218, 390).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar18 AS Infragistics.Win.UltraWinDock.DockAreaPane EXTENT 1 NO-UNDO.
        arrayvar18[1] = dockAreaPane1.
        THIS-OBJECT:ultraDockManager1:DockAreas:AddRange(arrayvar18).
        THIS-OBJECT:ultraDockManager1:DragIndicatorStyle = Infragistics.Win.UltraWinDock.DragIndicatorStyle:VisualStudio2008Vista.
        THIS-OBJECT:ultraDockManager1:DragWindowStyle = Infragistics.Win.UltraWinDock.DragWindowStyle:LayeredWindowWithIndicators.
        THIS-OBJECT:ultraDockManager1:HostControl = THIS-OBJECT.
        THIS-OBJECT:ultraDockManager1:WindowStyle = Infragistics.Win.UltraWinDock.WindowStyle:Office2007.
        /*  */
        /* m_SmartObjectTypeFormUnpinnedTabAreaLeft */
        /*  */
        resources:ApplyResources(THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaLeft, "m_SmartObjectTypeFormUnpinnedTabAreaLeft":U).
        THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaLeft:Name = "m_SmartObjectTypeFormUnpinnedTabAreaLeft":U.
        THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaLeft:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* m_SmartObjectTypeFormUnpinnedTabAreaRight */
        /*  */
        resources:ApplyResources(THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaRight, "m_SmartObjectTypeFormUnpinnedTabAreaRight":U).
        THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaRight:Name = "m_SmartObjectTypeFormUnpinnedTabAreaRight":U.
        THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaRight:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* m_SmartObjectTypeFormUnpinnedTabAreaTop */
        /*  */
        resources:ApplyResources(THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaTop, "m_SmartObjectTypeFormUnpinnedTabAreaTop":U).
        THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaTop:Name = "m_SmartObjectTypeFormUnpinnedTabAreaTop":U.
        THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaTop:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* m_SmartObjectTypeFormUnpinnedTabAreaBottom */
        /*  */
        resources:ApplyResources(THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaBottom, "m_SmartObjectTypeFormUnpinnedTabAreaBottom":U).
        THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaBottom:Name = "m_SmartObjectTypeFormUnpinnedTabAreaBottom":U.
        THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaBottom:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* m_SmartObjectTypeFormAutoHideControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT:m_SmartObjectTypeFormAutoHideControl, "m_SmartObjectTypeFormAutoHideControl":U).
        THIS-OBJECT:m_SmartObjectTypeFormAutoHideControl:Name = "m_SmartObjectTypeFormAutoHideControl":U.
        THIS-OBJECT:m_SmartObjectTypeFormAutoHideControl:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* windowDockingArea1 */
        /*  */
        THIS-OBJECT:windowDockingArea1:Controls:Add(THIS-OBJECT:dockableWindow1).
        resources:ApplyResources(THIS-OBJECT:windowDockingArea1, "windowDockingArea1":U).
        THIS-OBJECT:windowDockingArea1:Name = "windowDockingArea1":U.
        THIS-OBJECT:windowDockingArea1:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* dockableWindow1 */
        /*  */
        THIS-OBJECT:dockableWindow1:Controls:Add(THIS-OBJECT:panel1).
        resources:ApplyResources(THIS-OBJECT:dockableWindow1, "dockableWindow1":U).
        THIS-OBJECT:dockableWindow1:Name = "dockableWindow1":U.
        THIS-OBJECT:dockableWindow1:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* SmartObjectTypeForm */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartObjectTypeFormAutoHideControl).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:SmartObjectTypeForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaBottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaTop).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaRight).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartObjectTypeFormUnpinnedTabAreaLeft).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartObjectTypeForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Name = "SmartObjectTypeForm":U.
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityAdapter2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartSupportedLinkViewerControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbSearchField, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:SmartObjectTypeForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:SmartObjectTypeForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartObjectTypeViewerControl1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:windowDockingArea1:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the model for the tree
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeTreeModel ():

        oTreeModel = NEW SmartObjectTreeModel (smartBusinessEntityAdapter1:DataSet,
                                               ultraTree1,
                                               smartBusinessEntityAdapter1,
                                               smartObjectTypeViewerControl1,
                                               objectTypeAttributesUserControl1) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosing event
        Notes:
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnFormClosed (e AS System.Windows.Forms.FormClosedEventArgs):

        IF VALID-OBJECT (oTreeModel) THEN
            oTreeModel:Dispose () .

        GarbageCollectorHelper:DeleteObject (oTreeModel) .
        GarbageCollectorHelper:DeleteObject (objectTypeAttributesUserControl1) .
        GarbageCollectorHelper:ClearUltraTreeNodes (ultraTree1) .

        SUPER:OnFormClosed (e).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosing event
        Notes:
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnFormClosing (e AS FormClosingEventArgs):

        SUPER:OnFormClosing (e) .

        DockManagerHelper:StoreToSettingsService (ultraDockManager1,
                                                  THIS-OBJECT:WindowPositionRegistryKey) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        SUPER:OnLoad (e) .

        /* DockManager */
        DockManagerHelper:RestoreFromSettingsService (ultraDockManager1,
                                                      THIS-OBJECT:WindowPositionRegistryKey) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown (e) .

        UserInterfaceHelper:ProcessEvents() .

        THIS-OBJECT:FetchObjectTypes () .

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Searches a node in the tree with the given text
        Notes:
        @param pcSearchString The text to search for
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PerformSearch (pcSearchString AS CHARACTER):

        DEFINE VARIABLE oNodes   AS "System.Collections.Generic.List<UltraTreeNode>":U NO-UNDO.
        DEFINE VARIABLE lMatches AS LOGICAL NO-UNDO .

        oNodes = UltraTreeHelper:GetAllNodes (ultraTree1) .

        ASSIGN lMatches = INDEX (pcSearchString, "*":U) > 0 .

        /* Exact match */
        IF NOT lMatches THEN DO:

            {Consultingwerk/foreach.i UltraTreeNode oNode in oNodes}

                IF oNode:Text = pcSearchString THEN DO:
                    ultraTree1:ActiveNode = oNode .
                    tbSearchField:Text = oNode:Text .
                    ultraTree1:Focus () .
                    RETURN .
                END.
            END.
        END.

        IF lMatches THEN DO:
            {Consultingwerk/foreach.i UltraTreeNode oNode in oNodes nodefine}

                IF oNode:Text MATCHES pcSearchString THEN DO:
                    ultraTree1:ActiveNode = oNode .
                    tbSearchField:Text = oNode:Text .
                    ultraTree1:Focus () .
                    RETURN .
                END.
            END.
        END.
        ELSE DO:
            {Consultingwerk/foreach.i UltraTreeNode oNode in oNodes nodefine}

                IF oNode:Text BEGINS pcSearchString THEN DO:
                    ultraTree1:ActiveNode = oNode .
                    tbSearchField:Text = oNode:Text .
                    ultraTree1:Focus () .
                    RETURN .
                END.
            END.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterAddRecord event of the smartObjectTypeViewerControl1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID smartObjectTypeViewerControl1_AfterAddRecord (sender AS System.Object,
                                                                        e AS System.EventArgs):

        objectTypeAttributesUserControl1:Enabled = FALSE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterCancelUpdate event of the smartObjectTypeViewerControl1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID smartObjectTypeViewerControl1_AfterCancelUpdate (sender AS System.Object,
                                                                           e AS System.EventArgs):

        objectTypeAttributesUserControl1:ResetChanges() .

        objectTypeAttributesUserControl1:Enabled = TRUE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the HasChangesChanged event of the objectTypeAttributesUserControl1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartObjectTypeViewerControl1_AfterSaveChanges (sender AS System.Object,
                                                                        e AS System.EventArgs):

        objectTypeAttributesUserControl1:SaveChanges (smartBusinessEntityAdapter1:GetFieldValues("ObjectTypeGuid":U)) .

        objectTypeAttributesUserControl1:Enabled = TRUE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the HasChangesChanged event of the objectTypeAttributesUserControl1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID objectTypeAttributesUserControl1_HasChangesChanged (sender AS System.Object,
                                                                              e AS System.EventArgs):

        IF objectTypeAttributesUserControl1:HasChanges THEN DO:

            IF smartObjectTypeViewerControl1:SmartTableIOState = TableIOStateEnum:FieldsDisabled THEN
                smartObjectTypeViewerControl1:EnableFields(EnableFieldsEnum:Update) .

            smartObjectTypeViewerControl1:SetModifyingState() .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the smartToolbarController1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartToolbarController1_ToolClick (sender AS System.Object,
                                                           e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):

        CASE e:Tool:Key:

            WHEN "Add child":U THEN DO:
                oTreeModel:AddChild = TRUE .
                smartObjectTypeViewerControl1:AddRecord() .
            END.

        END CASE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown
        Notes:
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbSearchField_KeyDown (sender AS System.Object,
                                               e AS System.Windows.Forms.KeyEventArgs):

        IF (EnumHelper:AreEqual (e:KeyCode, Keys:Enter) OR
           EnumHelper:AreEqual (e:KeyCode, Keys:Return) OR
           EnumHelper:AreEqual (e:KeyCode, Keys:Tab)) AND
           tbSearchField:Text > "":U AND
           ultraTree1:Enabled THEN DO:

            THIS-OBJECT:PerformSearch (tbSearchField:Text) .
        END.

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the LookupComplete event of the tbSearchField
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbSearchField_LookupComplete (INPUT sender AS System.Object, INPUT e AS System.EventArgs):

        IF tbSearchField:Text > "":U AND
           ultraTree1:Enabled THEN

        THIS-OBJECT:PerformSearch (tbSearchField:Text) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartObjectTypeForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartObjectTypeForm ():

        IF VALID-OBJECT (ultraDockManager1) THEN
            ultraDockManager1:Dispose () .

        GarbageCollectorHelper:DeleteObject (oTreeModel) .
        GarbageCollectorHelper:ClearUltraTreeNodes (ultraTree1) .

    END DESTRUCTOR .

END CLASS.
