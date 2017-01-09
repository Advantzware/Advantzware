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
    File        : MainMenuForm
    Purpose     : Reference Main Menu Form for SmartComponent Library and
                  SmartFramework based applications
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 13 12:13:40 CEST 2009
    Notes       : See the InitializeMenu function for a sample how to create
                  multiple menu bands in the UltraExplorerBar
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                                    FROM PROPATH .
USING Consultingwerk.Framework.*                                          FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                                     FROM PROPATH .
USING Consultingwerk.Framework.Session.*                                  FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                               FROM PROPATH .
USING Consultingwerk.SmartFramework.*                                     FROM PROPATH .
USING Consultingwerk.OERA.*                                               FROM PROPATH .
USING Consultingwerk.Util.*                                               FROM PROPATH .
USING Consultingwerk.Windows.Framework.*                                  FROM PROPATH .
USING Consultingwerk.Windows.Framework.Controls.*                         FROM PROPATH .
USING Consultingwerk.Windows.Framework.Menu.*                             FROM PROPATH .
USING Consultingwerk.Windows.Framework.System.*                           FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*                                 FROM PROPATH .
USING Infragistics.Win.*                                                  FROM ASSEMBLY .
USING Infragistics.Win.AppStyling.*                                       FROM ASSEMBLY .
USING Infragistics.Win.AppStyling.Runtime.*                               FROM ASSEMBLY .
USING Infragistics.Win.Misc.*                                             FROM ASSEMBLY .
USING Infragistics.Win.UltraWinDock.*                                     FROM ASSEMBLY .
USING Infragistics.Win.UltraWinExplorerBar.*                              FROM ASSEMBLY .
USING Infragistics.Win.UltraWinToolbars.*                                 FROM ASSEMBLY .
USING Progress.Lang.*                                                     FROM PROPATH .
USING System.Windows.Forms.*                                              FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Framework.Reference.MainMenuForm
    INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE backgroundJobMonitorControl1 AS Consultingwerk.Windows.BackgroundOperation.BackgroundJobMonitorControl NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE dockableWindow3 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE dockableWindow6 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE dockableWindow4 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE logManagerSettings1 AS Consultingwerk.Windows.Util.Controls.LogManagerSettings NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraDesktopAlert1 AS Infragistics.Win.Misc.UltraDesktopAlert NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGridColumnChooser1 AS Infragistics.Win.UltraWinGrid.UltraGridColumnChooser NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPictureBox1 AS Infragistics.Win.UltraWinEditors.UltraPictureBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraStatusBar1 AS Infragistics.Win.UltraWinStatusBar.UltraStatusBar NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabbedMdiManager1 AS Infragistics.Win.UltraWinTabbedMdi.UltraTabbedMdiManager NO-UNDO.
    DEFINE PRIVATE VARIABLE m_CustomerExplorerAutoHideControl AS Infragistics.Win.UltraWinDock.AutoHideControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraDockManager1 AS Infragistics.Win.UltraWinDock.UltraDockManager NO-UNDO.
    DEFINE PRIVATE VARIABLE m_CustomerExplorerUnpinnedTabAreaTop AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_CustomerExplorerUnpinnedTabAreaBottom AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_CustomerExplorerUnpinnedTabAreaLeft AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_CustomerExplorerUnpinnedTabAreaRight AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.

    DEFINE PRIVATE VARIABLE ultraExplorerBar1 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBar NO-UNDO.
    DEFINE PRIVATE VARIABLE appStylistRuntime1 AS Infragistics.Win.AppStyling.Runtime.AppStylistRuntime NO-UNDO.

    DEFINE PRIVATE VARIABLE windowDockingArea1 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE dockableWindow1 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.

    DEFINE PRIVATE VARIABLE windowDockingArea2 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.

    DEFINE PRIVATE VARIABLE m_CustomerExplorer_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.
    DEFINE PRIVATE VARIABLE m_CustomerExplorer_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_CustomerExplorer_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_CustomerExplorer_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.


    DEFINE PRIVATE VARIABLE windowDockingArea4 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.

    DEFINE VARIABLE oSplash AS Consultingwerk.SmartComponents.Support.Splash.Splash NO-UNDO .
    DEFINE PRIVATE VARIABLE windowDockingArea5 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea6 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.

    DEFINE VARIABLE oCloseTimer      AS System.Windows.Forms.Timer NO-UNDO .
    DEFINE VARIABLE oAfterShownTimer AS System.Windows.Forms.Timer NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CustomerExplorer class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MainMenuForm ():

        DEFINE VARIABLE oForm         AS System.Windows.Forms.Form                  NO-UNDO .
        DEFINE VARIABLE oStyleService AS Consultingwerk.Windows.IAppStylistService  NO-UNDO .

        SUPER().
        InitializeComponent().

        SessionManager:UserNameChanged:Subscribe(UserNameChangedHandler) .
        SessionManager:LoginCompanyNameChanged:Subscribe(LoginCompanyNameChangedHandler) .

        Consultingwerk.Framework.FrameworkSettings:MdiContainer = THIS-OBJECT .

        oSplash = NEW Consultingwerk.SmartComponents.Support.Splash.Splash () .
        oSplash:ApplicationTitle = "SmartFramework~nMaintenance Application":U .
        oSplash:TopMost = TRUE .
        oSplash:Show () .
        PROCESS EVENTS .

        oStyleService = {Consultingwerk/get-service.i
                            Consultingwerk.Windows.IAppStylistService
                            "NEW Consultingwerk.Windows.AppStylistService ()"} .

        oStyleService:StyleLibraryChanged:Subscribe (StyleLibraryChangedHandler) .

        THIS-OBJECT:StyleLibraryChangedHandler (oStyleService,
                                                Consultingwerk.EventArgs:Empty) .



        Consultingwerk.SmartComponents.Implementation.SmartDataBrowserSettings:SmartDataBrowserActivated:Subscribe (SmartDataBrowserActivatedHandler) .

        SessionManager:LanguageDisplayNameChanged:Subscribe (DisplayLanguageName) .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the LanguageDisplayNameChanged event of the
                 SessionManager
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DisplayLanguageName ():

        THIS-OBJECT:ultraStatusBar1:Panels ["language":U]:Text = SessionManager:LanguageDisplayName .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: On 10.2B0612 and 11.1 pressing cancel from the login dialog caused
                 the AVM to fall into an infinitive loop after closing the MainMenuForm
        Notes:
        @param sender The object that raised the Tick event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FormCloseTimerHandler (sender AS System.Object,
                                                 e AS System.EventArgs):

        THIS-OBJECT:Close () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creates and configures a new instance of the MainMenuRenderer class
        Notes:
        @return The configured instance of the MainMenuRenderer class
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IMainMenuRenderer NewMainMenuRenderer ():

        DEFINE VARIABLE oMainMenuRenderer AS MainMenuRenderer NO-UNDO.

        oMainMenuRenderer = NEW MainMenuRenderer (THIS-OBJECT).

        oMainMenuRenderer:ConstantExplorerBarGroups = "":U.
        oMainMenuRenderer:MenuTreeViewControlSortAlphabetical = FALSE.
        oMainMenuRenderer:RenderRecentGroup = TRUE.

        RETURN oMainMenuRenderer.

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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Reference.MainMenuForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane1 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane1 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedLeft, NEW System.Guid("0b3aa0d5-1ba1-47d9-b8f7-67212859fbf1":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane1 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane1 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("007f2fb8-f8f0-40a5-98e4-3650503edc40":U), NEW System.Guid("00000000-0000-0000-0000-000000000000":U), -1, NEW System.Guid("0b3aa0d5-1ba1-47d9-b8f7-67212859fbf1":U), -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane2 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane2 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedRight, NEW System.Guid("5f63358c-7481-42af-80c1-369dae518cf8":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane2 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane2 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("c7b44b28-45c5-4ec1-908a-fe653738cd78":U), NEW System.Guid("7557a4e2-a353-42ef-83bc-51e4281055e8":U), -1, NEW System.Guid("5f63358c-7481-42af-80c1-369dae518cf8":U), 0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane3 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane3 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:Floating, NEW System.Guid("7557a4e2-a353-42ef-83bc-51e4281055e8":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane4 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane4 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedRight, NEW System.Guid("d5f9cdd7-5c09-44ef-8fa6-31e4217132e2":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane3 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane3 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("4576c7d8-74df-4fbe-b22b-71beb26f36e9":U), NEW System.Guid("00000000-0000-0000-0000-000000000000":U), -1, NEW System.Guid("d5f9cdd7-5c09-44ef-8fa6-31e4217132e2":U), -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane5 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane5 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedBottom, NEW System.Guid("5f3180e5-df14-43d8-80bb-243333b66ac6":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane4 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane4 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("4b179fc3-01e4-44db-a4b5-cd0afdaa99db":U), NEW System.Guid("00000000-0000-0000-0000-000000000000":U), -1, NEW System.Guid("5f3180e5-df14-43d8-80bb-243333b66ac6":U), -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE optionSet1 AS Infragistics.Win.UltraWinToolbars.OptionSet NO-UNDO.
        optionSet1 = NEW Infragistics.Win.UltraWinToolbars.OptionSet("View":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool56 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool56 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool38 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool38 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool37 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool37 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool40 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool40 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool39 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool39 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool41 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool41 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool42 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool42 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE contextualTabGroup1 AS Infragistics.Win.UltraWinToolbars.ContextualTabGroup NO-UNDO.
        contextualTabGroup1 = NEW Infragistics.Win.UltraWinToolbars.ContextualTabGroup("search":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("start":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool23 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool23 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool24 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool24 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool25 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool25 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool26 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool26 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool27 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool27 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool28 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool28 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup2":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool29 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool29 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool30 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool30 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool31 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool31 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool32 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool32 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup4":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool57 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool57 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("LaunchForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab2 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab2 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("view":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup4 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup2":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool11 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool11 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Window":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool26 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool26 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("ApplicationMenu":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool33 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool33 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Reload Menu":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool50 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool50 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("StartNewSession":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup5 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup5 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("links":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE controlContainerTool1 AS Infragistics.Win.UltraWinToolbars.ControlContainerTool NO-UNDO.
        controlContainerTool1 = NEW Infragistics.Win.UltraWinToolbars.ControlContainerTool("ControlContainerTool1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool43 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool43 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool44 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool44 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TableIOToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool15 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool15 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("UltraToolbar1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool1 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool1 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("File":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool2 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool2 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Edit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool3 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool3 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Options":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool4 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool4 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Window":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool5 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool5 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Help":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar3 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar3 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("NavigationToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar4 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar4 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("View":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool16 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool16 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool17 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool17 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool20 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool20 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool6 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool6 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("File":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool21 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool21 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool7 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool7 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Edit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool8 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool8 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Options":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool9 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool9 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Window":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE mdiWindowListTool1 AS Infragistics.Win.UltraWinToolbars.MdiWindowListTool NO-UNDO.
        mdiWindowListTool1 = NEW Infragistics.Win.UltraWinToolbars.MdiWindowListTool("MDIWindowListTool1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool10 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool10 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Help":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool22 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool22 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE mdiWindowListTool2 AS Infragistics.Win.UltraWinToolbars.MdiWindowListTool NO-UNDO.
        mdiWindowListTool2 = NEW Infragistics.Win.UltraWinToolbars.MdiWindowListTool("MDIWindowListTool1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool34 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool34 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ClearSearch":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool36 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool36 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("StartSearch":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
        appearance28 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool13 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool13 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("PreviousSearches":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
        appearance29 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool1 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool1 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("(empty)":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool2 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool2 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("(empty)":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool51 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool51 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("StartNewSession":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
        appearance30 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
        appearance31 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE controlContainerTool2 AS Infragistics.Win.UltraWinToolbars.ControlContainerTool NO-UNDO.
        controlContainerTool2 = NEW Infragistics.Win.UltraWinToolbars.ControlContainerTool("ControlContainerTool1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
        appearance32 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool58 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool58 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("LaunchForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
        appearance33 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
        appearance34 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool27 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool27 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("ApplicationMenu":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance35 AS Infragistics.Win.Appearance NO-UNDO.
        appearance35 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool35 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool35 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Reload Menu":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance36 AS Infragistics.Win.Appearance NO-UNDO.
        appearance36 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance37 AS Infragistics.Win.Appearance NO-UNDO.
        appearance37 = NEW Infragistics.Win.Appearance().
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
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:ultraExplorerBar1 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBar().
        THIS-OBJECT:ultraGridColumnChooser1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumnChooser().
        THIS-OBJECT:logManagerSettings1 = NEW Consultingwerk.Windows.Util.Controls.LogManagerSettings().
        THIS-OBJECT:backgroundJobMonitorControl1 = NEW Consultingwerk.Windows.BackgroundOperation.BackgroundJobMonitorControl().
        THIS-OBJECT:ultraTabbedMdiManager1 = NEW Infragistics.Win.UltraWinTabbedMdi.UltraTabbedMdiManager(THIS-OBJECT:components).
        THIS-OBJECT:ultraDockManager1 = NEW Infragistics.Win.UltraWinDock.UltraDockManager(THIS-OBJECT:components).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaLeft = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaRight = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaTop = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaBottom = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_CustomerExplorerAutoHideControl = NEW Infragistics.Win.UltraWinDock.AutoHideControl().
        THIS-OBJECT:dockableWindow1 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:dockableWindow6 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:dockableWindow3 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:dockableWindow4 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:appStylistRuntime1 = NEW Infragistics.Win.AppStyling.Runtime.AppStylistRuntime(THIS-OBJECT:components).
        THIS-OBJECT:windowDockingArea1 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:windowDockingArea2 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:windowDockingArea4 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:ultraStatusBar1 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusBar().
        THIS-OBJECT:windowDockingArea5 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:ultraDesktopAlert1 = NEW Infragistics.Win.Misc.UltraDesktopAlert(THIS-OBJECT:components).
        THIS-OBJECT:ultraPictureBox1 = NEW Infragistics.Win.UltraWinEditors.UltraPictureBox().
        THIS-OBJECT:windowDockingArea6 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        CAST(THIS-OBJECT:ultraExplorerBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraGridColumnChooser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabbedMdiManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:SuspendLayout().
        THIS-OBJECT:dockableWindow1:SuspendLayout().
        THIS-OBJECT:dockableWindow6:SuspendLayout().
        THIS-OBJECT:dockableWindow3:SuspendLayout().
        THIS-OBJECT:dockableWindow4:SuspendLayout().
        THIS-OBJECT:windowDockingArea1:SuspendLayout().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraStatusBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraDesktopAlert1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraExplorerBar1 */
        /*  */
        THIS-OBJECT:ultraExplorerBar1:GroupSettings:AllowEdit = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraExplorerBar1:GroupSettings:HotTracking = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraExplorerBar1:GroupSettings:Style = Infragistics.Win.UltraWinExplorerBar.GroupStyle:LargeImagesWithText.
        THIS-OBJECT:ultraExplorerBar1:ItemSettings:AllowEdit = Infragistics.Win.DefaultableBoolean:False.
        resources:ApplyResources(THIS-OBJECT:ultraExplorerBar1, "ultraExplorerBar1":U).
        THIS-OBJECT:ultraExplorerBar1:Name = "ultraExplorerBar1":U.
        THIS-OBJECT:ultraExplorerBar1:NavigationMaxGroupHeaders = 5.
        THIS-OBJECT:ultraExplorerBar1:Style = Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarStyle:OutlookNavigationPane.
        THIS-OBJECT:ultraExplorerBar1:UseLargeGroupHeaderImages = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraExplorerBar1:ContextMenuInitializing:Subscribe(THIS-OBJECT:ultraExplorerBar1_ContextMenuInitializing).
        THIS-OBJECT:ultraExplorerBar1:ItemRemoving:Subscribe(THIS-OBJECT:ultraExplorerBar1_ItemRemoving).
        /*  */
        /* ultraGridColumnChooser1 */
        /*  */
        THIS-OBJECT:ultraGridColumnChooser1:ColumnDisplayOrder = Infragistics.Win.UltraWinGrid.ColumnDisplayOrder:SameAsGrid.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ResizeAllColumns.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:MaxRowScrollRegions = 1.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:AllowColMoving = Infragistics.Win.UltraWinGrid.AllowColMoving:NotAllowed.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:AllowColSizing = Infragistics.Win.UltraWinGrid.AllowColSizing:None.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:AllowRowLayoutCellSizing = Infragistics.Win.UltraWinGrid.RowLayoutSizing:None.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:AllowRowLayoutLabelSizing = Infragistics.Win.UltraWinGrid.RowLayoutSizing:None.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:CellPadding = 2.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:ExpansionIndicator = Infragistics.Win.UltraWinGrid.ShowExpansionIndicator:Never.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:Select.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:RowSizing = Infragistics.Win.UltraWinGrid.RowSizing:AutoFixed.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:None.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:RowConnectorStyle = Infragistics.Win.UltraWinGrid.RowConnectorStyle:None.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        resources:ApplyResources(THIS-OBJECT:ultraGridColumnChooser1, "ultraGridColumnChooser1":U).
        THIS-OBJECT:ultraGridColumnChooser1:MultipleBandSupport = Infragistics.Win.UltraWinGrid.MultipleBandSupport:SingleBandOnly.
        THIS-OBJECT:ultraGridColumnChooser1:Name = "ultraGridColumnChooser1":U.
        THIS-OBJECT:ultraGridColumnChooser1:Style = Infragistics.Win.UltraWinGrid.ColumnChooserStyle:HiddenColumnsOnly.
        THIS-OBJECT:ultraGridColumnChooser1:StyleLibraryName = "":U.
        THIS-OBJECT:ultraGridColumnChooser1:StyleSetName = "":U.
        /*  */
        /* logManagerSettings1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:logManagerSettings1, "logManagerSettings1":U).
        THIS-OBJECT:logManagerSettings1:Name = "logManagerSettings1":U.
        /*  */
        /* backgroundJobMonitorControl1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:backgroundJobMonitorControl1, "backgroundJobMonitorControl1":U).
        THIS-OBJECT:backgroundJobMonitorControl1:Name = "backgroundJobMonitorControl1":U.
        /*  */
        /* ultraTabbedMdiManager1 */
        /*  */
        THIS-OBJECT:ultraTabbedMdiManager1:AllowMaximize = TRUE.
        THIS-OBJECT:ultraTabbedMdiManager1:AllowNestedTabGroups = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraTabbedMdiManager1:MdiParent = THIS-OBJECT.
        THIS-OBJECT:ultraTabbedMdiManager1:TabSettings:AllowClose = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraTabbedMdiManager1:TabSettings:AllowDrag = Infragistics.Win.UltraWinTabbedMdi.MdiTabDragStyle:WithinAndAcrossGroups.
        THIS-OBJECT:ultraTabbedMdiManager1:TabSettings:DisplayFormIcon = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraTabbedMdiManager1:ViewStyle = Infragistics.Win.UltraWinTabbedMdi.ViewStyle:Office2007.
        /*  */
        /* ultraDockManager1 */
        /*  */
        THIS-OBJECT:ultraDockManager1:CaptionStyle = Infragistics.Win.UltraWinDock.CaptionStyle:Office2007.
        THIS-OBJECT:ultraDockManager1:DefaultPaneSettings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        dockAreaPane1:DockedBefore = NEW System.Guid("5f63358c-7481-42af-80c1-369dae518cf8":U).
        dockableControlPane1:Control = THIS-OBJECT:ultraExplorerBar1.
        dockableControlPane1:OriginalControlBounds = NEW System.Drawing.Rectangle(35, 88, 205, 472).
        dockableControlPane1:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        dockableControlPane1:Settings:Appearance = appearance1.
        dockableControlPane1:Size = NEW System.Drawing.Size(100, 100).
        resources:ApplyResources(dockableControlPane1, "dockableControlPane1":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar0[1] = dockableControlPane1.
        dockAreaPane1:Panes:AddRange(arrayvar0).
        dockAreaPane1:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        dockAreaPane1:Size = NEW System.Drawing.Size(205, 417).
        dockAreaPane2:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:TabGroup.
        dockAreaPane2:DockedBefore = NEW System.Guid("7557a4e2-a353-42ef-83bc-51e4281055e8":U).
        dockableControlPane2:Control = THIS-OBJECT:ultraGridColumnChooser1.
        dockableControlPane2:FlyoutSize = NEW System.Drawing.Size(199, -1).
        dockableControlPane2:OriginalControlBounds = NEW System.Drawing.Rectangle(494, 203, 140, 160).
        dockableControlPane2:Pinned = FALSE.
        dockableControlPane2:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        dockableControlPane2:Settings:Appearance = appearance2.
        dockableControlPane2:Size = NEW System.Drawing.Size(100, 100).
        resources:ApplyResources(dockableControlPane2, "dockableControlPane2":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar1[1] = dockableControlPane2.
        dockAreaPane2:Panes:AddRange(arrayvar1).
        dockAreaPane2:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        dockAreaPane2:Size = NEW System.Drawing.Size(199, 416).
        dockAreaPane3:DockedBefore = NEW System.Guid("d5f9cdd7-5c09-44ef-8fa6-31e4217132e2":U).
        dockAreaPane3:FloatingLocation = NEW System.Drawing.Point(1170, 514).
        dockAreaPane3:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        dockAreaPane3:Size = NEW System.Drawing.Size(95, 446).
        dockAreaPane4:DockedBefore = NEW System.Guid("5f3180e5-df14-43d8-80bb-243333b66ac6":U).
        dockableControlPane3:Control = THIS-OBJECT:logManagerSettings1.
        dockableControlPane3:FlyoutSize = NEW System.Drawing.Size(179, -1).
        dockableControlPane3:OriginalControlBounds = NEW System.Drawing.Rectangle(526, 199, 217, 314).
        dockableControlPane3:Pinned = FALSE.
        dockableControlPane3:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        dockableControlPane3:Settings:Appearance = appearance3.
        dockableControlPane3:Size = NEW System.Drawing.Size(100, 100).
        resources:ApplyResources(dockableControlPane3, "dockableControlPane3":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar2[1] = dockableControlPane3.
        dockAreaPane4:Panes:AddRange(arrayvar2).
        dockAreaPane4:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        dockAreaPane4:Size = NEW System.Drawing.Size(179, 404).
        dockableControlPane4:Control = THIS-OBJECT:backgroundJobMonitorControl1.
        dockableControlPane4:FlyoutSize = NEW System.Drawing.Size(-1, 95).
        dockableControlPane4:OriginalControlBounds = NEW System.Drawing.Rectangle(238, 418, 584, 131).
        dockableControlPane4:Pinned = FALSE.
        dockableControlPane4:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        dockableControlPane4:Settings:Appearance = appearance4.
        dockableControlPane4:Size = NEW System.Drawing.Size(100, 100).
        resources:ApplyResources(dockableControlPane4, "dockableControlPane4":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar3[1] = dockableControlPane4.
        dockAreaPane5:Panes:AddRange(arrayvar3).
        dockAreaPane5:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        dockAreaPane5:Size = NEW System.Drawing.Size(637, 95).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinDock.DockAreaPane EXTENT 5 NO-UNDO.
        arrayvar4[1] = dockAreaPane1.
        arrayvar4[2] = dockAreaPane2.
        arrayvar4[3] = dockAreaPane3.
        arrayvar4[4] = dockAreaPane4.
        arrayvar4[5] = dockAreaPane5.
        THIS-OBJECT:ultraDockManager1:DockAreas:AddRange(arrayvar4).
        THIS-OBJECT:ultraDockManager1:DragWindowStyle = Infragistics.Win.UltraWinDock.DragWindowStyle:LayeredWindowWithIndicators.
        THIS-OBJECT:ultraDockManager1:HostControl = THIS-OBJECT.
        THIS-OBJECT:ultraDockManager1:WindowStyle = Infragistics.Win.UltraWinDock.WindowStyle:Office2007.
        /*  */
        /* m_CustomerExplorerUnpinnedTabAreaLeft */
        /*  */
        resources:ApplyResources(THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaLeft, "m_CustomerExplorerUnpinnedTabAreaLeft":U).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaLeft:Name = "m_CustomerExplorerUnpinnedTabAreaLeft":U.
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaLeft:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* m_CustomerExplorerUnpinnedTabAreaRight */
        /*  */
        resources:ApplyResources(THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaRight, "m_CustomerExplorerUnpinnedTabAreaRight":U).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaRight:Name = "m_CustomerExplorerUnpinnedTabAreaRight":U.
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaRight:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* m_CustomerExplorerUnpinnedTabAreaTop */
        /*  */
        resources:ApplyResources(THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaTop, "m_CustomerExplorerUnpinnedTabAreaTop":U).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaTop:Name = "m_CustomerExplorerUnpinnedTabAreaTop":U.
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaTop:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* m_CustomerExplorerUnpinnedTabAreaBottom */
        /*  */
        resources:ApplyResources(THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaBottom, "m_CustomerExplorerUnpinnedTabAreaBottom":U).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaBottom:Name = "m_CustomerExplorerUnpinnedTabAreaBottom":U.
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaBottom:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* m_CustomerExplorerAutoHideControl */
        /*  */
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:Controls:Add(THIS-OBJECT:dockableWindow6).
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:Controls:Add(THIS-OBJECT:dockableWindow3).
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:Controls:Add(THIS-OBJECT:dockableWindow4).
        resources:ApplyResources(THIS-OBJECT:m_CustomerExplorerAutoHideControl, "m_CustomerExplorerAutoHideControl":U).
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:Name = "m_CustomerExplorerAutoHideControl":U.
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* dockableWindow1 */
        /*  */
        THIS-OBJECT:dockableWindow1:Controls:Add(THIS-OBJECT:ultraExplorerBar1).
        resources:ApplyResources(THIS-OBJECT:dockableWindow1, "dockableWindow1":U).
        THIS-OBJECT:dockableWindow1:Name = "dockableWindow1":U.
        THIS-OBJECT:dockableWindow1:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* dockableWindow6 */
        /*  */
        THIS-OBJECT:dockableWindow6:Controls:Add(THIS-OBJECT:ultraGridColumnChooser1).
        resources:ApplyResources(THIS-OBJECT:dockableWindow6, "dockableWindow6":U).
        THIS-OBJECT:dockableWindow6:Name = "dockableWindow6":U.
        THIS-OBJECT:dockableWindow6:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* dockableWindow3 */
        /*  */
        THIS-OBJECT:dockableWindow3:Controls:Add(THIS-OBJECT:logManagerSettings1).
        resources:ApplyResources(THIS-OBJECT:dockableWindow3, "dockableWindow3":U).
        THIS-OBJECT:dockableWindow3:Name = "dockableWindow3":U.
        THIS-OBJECT:dockableWindow3:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* dockableWindow4 */
        /*  */
        THIS-OBJECT:dockableWindow4:Controls:Add(THIS-OBJECT:backgroundJobMonitorControl1).
        resources:ApplyResources(THIS-OBJECT:dockableWindow4, "dockableWindow4":U).
        THIS-OBJECT:dockableWindow4:Name = "dockableWindow4":U.
        THIS-OBJECT:dockableWindow4:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* windowDockingArea1 */
        /*  */
        THIS-OBJECT:windowDockingArea1:Controls:Add(THIS-OBJECT:dockableWindow1).
        resources:ApplyResources(THIS-OBJECT:windowDockingArea1, "windowDockingArea1":U).
        THIS-OBJECT:windowDockingArea1:Name = "windowDockingArea1":U.
        THIS-OBJECT:windowDockingArea1:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* windowDockingArea2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:windowDockingArea2, "windowDockingArea2":U).
        THIS-OBJECT:windowDockingArea2:Name = "windowDockingArea2":U.
        THIS-OBJECT:windowDockingArea2:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm":U).
        optionSet1:AllowAllUp = FALSE.
        THIS-OBJECT:smartToolbarController1:OptionSets:Add(optionSet1).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar5[1] = buttonTool56.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar5).
        buttonTool41:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar6[1] = buttonTool38.
        arrayvar6[2] = buttonTool37.
        arrayvar6[3] = buttonTool40.
        arrayvar6[4] = buttonTool39.
        arrayvar6[5] = buttonTool41.
        arrayvar6[6] = buttonTool42.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar6).
        resources:ApplyResources(contextualTabGroup1, "contextualTabGroup1":U).
        contextualTabGroup1:Key = "search":U.
        contextualTabGroup1:Visible = FALSE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ContextualTabGroup EXTENT 1 NO-UNDO.
        arrayvar7[1] = contextualTabGroup1.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedContextualTabGroups:AddRange(arrayvar7).
        resources:ApplyResources(ribbonTab1, "ribbonTab1":U).
        resources:ApplyResources(ribbonGroup1, "ribbonGroup1":U).
        buttonTool23:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool24:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool25:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar8[1] = buttonTool23.
        arrayvar8[2] = buttonTool24.
        arrayvar8[3] = buttonTool25.
        arrayvar8[4] = buttonTool26.
        arrayvar8[5] = buttonTool27.
        arrayvar8[6] = buttonTool28.
        ribbonGroup1:Tools:AddRange(arrayvar8).
        resources:ApplyResources(ribbonGroup2, "ribbonGroup2":U).
        buttonTool29:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool30:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool31:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool32:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar9[1] = buttonTool29.
        arrayvar9[2] = buttonTool30.
        arrayvar9[3] = buttonTool31.
        arrayvar9[4] = buttonTool32.
        ribbonGroup2:Tools:AddRange(arrayvar9).
        resources:ApplyResources(ribbonGroup3, "ribbonGroup3":U).
        buttonTool57:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar10[1] = buttonTool57.
        ribbonGroup3:Tools:AddRange(arrayvar10).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 3 NO-UNDO.
        arrayvar11[1] = ribbonGroup1.
        arrayvar11[2] = ribbonGroup2.
        arrayvar11[3] = ribbonGroup3.
        ribbonTab1:Groups:AddRange(arrayvar11).
        resources:ApplyResources(ribbonTab2, "ribbonTab2":U).
        resources:ApplyResources(ribbonGroup4, "ribbonGroup4":U).
        popupMenuTool11:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        stateButtonTool26:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool33:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool50:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar12[1] = popupMenuTool11.
        arrayvar12[2] = stateButtonTool26.
        arrayvar12[3] = buttonTool33.
        arrayvar12[4] = buttonTool50.
        ribbonGroup4:Tools:AddRange(arrayvar12).
        resources:ApplyResources(ribbonGroup5, "ribbonGroup5":U).
        controlContainerTool1:ControlName = "ultraPictureBox1":U.
        controlContainerTool1:InstanceProps:Width = 133.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar13[1] = controlContainerTool1.
        ribbonGroup5:Tools:AddRange(arrayvar13).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 2 NO-UNDO.
        arrayvar14[1] = ribbonGroup4.
        arrayvar14[2] = ribbonGroup5.
        ribbonTab2:Groups:AddRange(arrayvar14).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar15 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 2 NO-UNDO.
        arrayvar15[1] = ribbonTab1.
        arrayvar15[2] = ribbonTab2.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar15).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar16 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar16[1] = buttonTool43.
        arrayvar16[2] = buttonTool44.
        THIS-OBJECT:smartToolbarController1:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar16).
        THIS-OBJECT:smartToolbarController1:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        THIS-OBJECT:smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 1.
        buttonTool10:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar17 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar17[1] = buttonTool10.
        arrayvar17[2] = buttonTool11.
        arrayvar17[3] = buttonTool12.
        arrayvar17[4] = buttonTool13.
        arrayvar17[5] = buttonTool14.
        arrayvar17[6] = buttonTool15.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar17).
        resources:ApplyResources(ultraToolbar1, "ultraToolbar1":U).
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 0.
        ultraToolbar2:IsMainMenuBar = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar18 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 5 NO-UNDO.
        arrayvar18[1] = popupMenuTool1.
        arrayvar18[2] = popupMenuTool2.
        arrayvar18[3] = popupMenuTool3.
        arrayvar18[4] = popupMenuTool4.
        arrayvar18[5] = popupMenuTool5.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar18).
        resources:ApplyResources(ultraToolbar2, "ultraToolbar2":U).
        ultraToolbar3:DockedColumn = 1.
        ultraToolbar3:DockedRow = 1.
        buttonTool2:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar19 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar19[1] = buttonTool2.
        arrayvar19[2] = buttonTool3.
        arrayvar19[3] = buttonTool4.
        arrayvar19[4] = buttonTool5.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar19).
        resources:ApplyResources(ultraToolbar3, "ultraToolbar3":U).
        ultraToolbar4:DockedColumn = 2.
        ultraToolbar4:DockedRow = 1.
        resources:ApplyResources(ultraToolbar4, "ultraToolbar4":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar20 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 4 NO-UNDO.
        arrayvar20[1] = ultraToolbar1.
        arrayvar20[2] = ultraToolbar2.
        arrayvar20[3] = ultraToolbar3.
        arrayvar20[4] = ultraToolbar4.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar20).
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        buttonTool1:SharedPropsInternal:AppearancesLarge:Appearance = appearance6.
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        buttonTool1:SharedPropsInternal:AppearancesSmall:Appearance = appearance7.
        resources:ApplyResources(buttonTool1:SharedPropsInternal, "buttonTool1.SharedPropsInternal":U).
        buttonTool1:SharedPropsInternal:Enabled = FALSE.
        buttonTool1:ForceApplyResources = "SharedPropsInternal":U.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesLarge:Appearance = appearance8.
        appearance9:Image = CAST(resources:GetObject("appearance9.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesSmall:Appearance = appearance9.
        resources:ApplyResources(buttonTool6:SharedPropsInternal, "buttonTool6.SharedPropsInternal":U).
        buttonTool6:SharedPropsInternal:Enabled = FALSE.
        buttonTool6:ForceApplyResources = "SharedPropsInternal":U.
        appearance10:Image = CAST(resources:GetObject("appearance10.Image":U), System.Object).
        buttonTool7:SharedPropsInternal:AppearancesLarge:Appearance = appearance10.
        appearance11:Image = CAST(resources:GetObject("appearance11.Image":U), System.Object).
        buttonTool7:SharedPropsInternal:AppearancesSmall:Appearance = appearance11.
        resources:ApplyResources(buttonTool7:SharedPropsInternal, "buttonTool7.SharedPropsInternal":U).
        buttonTool7:SharedPropsInternal:Enabled = FALSE.
        buttonTool7:ForceApplyResources = "SharedPropsInternal":U.
        appearance12:Image = CAST(resources:GetObject("appearance12.Image":U), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesLarge:Appearance = appearance12.
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesSmall:Appearance = appearance13.
        resources:ApplyResources(buttonTool8:SharedPropsInternal, "buttonTool8.SharedPropsInternal":U).
        buttonTool8:SharedPropsInternal:Enabled = FALSE.
        buttonTool8:ForceApplyResources = "SharedPropsInternal":U.
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        buttonTool9:SharedPropsInternal:AppearancesLarge:Appearance = appearance14.
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        buttonTool9:SharedPropsInternal:AppearancesSmall:Appearance = appearance15.
        resources:ApplyResources(buttonTool9:SharedPropsInternal, "buttonTool9.SharedPropsInternal":U).
        buttonTool9:SharedPropsInternal:Enabled = FALSE.
        buttonTool9:ForceApplyResources = "SharedPropsInternal":U.
        appearance16:Image = CAST(resources:GetObject("appearance16.Image":U), System.Object).
        buttonTool16:SharedPropsInternal:AppearancesLarge:Appearance = appearance16.
        appearance17:Image = CAST(resources:GetObject("appearance17.Image":U), System.Object).
        buttonTool16:SharedPropsInternal:AppearancesSmall:Appearance = appearance17.
        resources:ApplyResources(buttonTool16:SharedPropsInternal, "buttonTool16.SharedPropsInternal":U).
        buttonTool16:SharedPropsInternal:Enabled = FALSE.
        buttonTool16:ForceApplyResources = "SharedPropsInternal":U.
        appearance18:Image = CAST(resources:GetObject("appearance18.Image":U), System.Object).
        buttonTool17:SharedPropsInternal:AppearancesLarge:Appearance = appearance18.
        appearance19:Image = CAST(resources:GetObject("appearance19.Image":U), System.Object).
        buttonTool17:SharedPropsInternal:AppearancesSmall:Appearance = appearance19.
        resources:ApplyResources(buttonTool17:SharedPropsInternal, "buttonTool17.SharedPropsInternal":U).
        buttonTool17:SharedPropsInternal:Enabled = FALSE.
        buttonTool17:ForceApplyResources = "SharedPropsInternal":U.
        appearance20:Image = CAST(resources:GetObject("appearance20.Image":U), System.Object).
        buttonTool18:SharedPropsInternal:AppearancesLarge:Appearance = appearance20.
        appearance21:Image = CAST(resources:GetObject("appearance21.Image":U), System.Object).
        buttonTool18:SharedPropsInternal:AppearancesSmall:Appearance = appearance21.
        resources:ApplyResources(buttonTool18:SharedPropsInternal, "buttonTool18.SharedPropsInternal":U).
        buttonTool18:SharedPropsInternal:Enabled = FALSE.
        buttonTool18:ForceApplyResources = "SharedPropsInternal":U.
        appearance22:Image = CAST(resources:GetObject("appearance22.Image":U), System.Object).
        buttonTool19:SharedPropsInternal:AppearancesLarge:Appearance = appearance22.
        appearance23:Image = CAST(resources:GetObject("appearance23.Image":U), System.Object).
        buttonTool19:SharedPropsInternal:AppearancesSmall:Appearance = appearance23.
        resources:ApplyResources(buttonTool19:SharedPropsInternal, "buttonTool19.SharedPropsInternal":U).
        buttonTool19:SharedPropsInternal:Enabled = FALSE.
        buttonTool19:ForceApplyResources = "SharedPropsInternal":U.
        appearance24:Image = CAST(resources:GetObject("appearance24.Image":U), System.Object).
        buttonTool20:SharedPropsInternal:AppearancesLarge:Appearance = appearance24.
        appearance25:Image = CAST(resources:GetObject("appearance25.Image":U), System.Object).
        buttonTool20:SharedPropsInternal:AppearancesSmall:Appearance = appearance25.
        resources:ApplyResources(buttonTool20:SharedPropsInternal, "buttonTool20.SharedPropsInternal":U).
        buttonTool20:SharedPropsInternal:Enabled = FALSE.
        buttonTool20:ForceApplyResources = "SharedPropsInternal":U.
        resources:ApplyResources(popupMenuTool6:SharedPropsInternal, "popupMenuTool6.SharedPropsInternal":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar21 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar21[1] = buttonTool21.
        popupMenuTool6:Tools:AddRange(arrayvar21).
        popupMenuTool6:ForceApplyResources = "SharedPropsInternal":U.
        resources:ApplyResources(popupMenuTool7:SharedPropsInternal, "popupMenuTool7.SharedPropsInternal":U).
        popupMenuTool7:ForceApplyResources = "SharedPropsInternal":U.
        resources:ApplyResources(popupMenuTool8:SharedPropsInternal, "popupMenuTool8.SharedPropsInternal":U).
        popupMenuTool8:ForceApplyResources = "SharedPropsInternal":U.
        appearance26:Image = CAST(resources:GetObject("appearance26.Image":U), System.Object).
        popupMenuTool9:SharedPropsInternal:AppearancesLarge:Appearance = appearance26.
        resources:ApplyResources(popupMenuTool9:SharedPropsInternal, "popupMenuTool9.SharedPropsInternal":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar22 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar22[1] = mdiWindowListTool1.
        popupMenuTool9:Tools:AddRange(arrayvar22).
        popupMenuTool9:ForceApplyResources = "SharedPropsInternal":U.
        resources:ApplyResources(popupMenuTool10:SharedPropsInternal, "popupMenuTool10.SharedPropsInternal":U).
        popupMenuTool10:ForceApplyResources = "SharedPropsInternal":U.
        resources:ApplyResources(buttonTool22:SharedPropsInternal, "buttonTool22.SharedPropsInternal":U).
        buttonTool22:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:TextOnlyAlways.
        buttonTool22:ForceApplyResources = "SharedPropsInternal":U.
        resources:ApplyResources(mdiWindowListTool2:SharedPropsInternal, "mdiWindowListTool2.SharedPropsInternal":U).
        mdiWindowListTool2:ForceApplyResources = "SharedPropsInternal":U.
        appearance27:Image = CAST(resources:GetObject("appearance27.Image":U), System.Object).
        buttonTool34:SharedPropsInternal:AppearancesLarge:Appearance = appearance27.
        resources:ApplyResources(buttonTool34:SharedPropsInternal, "buttonTool34.SharedPropsInternal":U).
        buttonTool34:ForceApplyResources = "SharedPropsInternal":U.
        appearance28:Image = CAST(resources:GetObject("appearance28.Image":U), System.Object).
        buttonTool36:SharedPropsInternal:AppearancesLarge:Appearance = appearance28.
        resources:ApplyResources(buttonTool36:SharedPropsInternal, "buttonTool36.SharedPropsInternal":U).
        buttonTool36:ForceApplyResources = "SharedPropsInternal":U.
        appearance29:Image = CAST(resources:GetObject("appearance29.Image":U), System.Object).
        popupMenuTool13:SharedPropsInternal:AppearancesLarge:Appearance = appearance29.
        resources:ApplyResources(popupMenuTool13:SharedPropsInternal, "popupMenuTool13.SharedPropsInternal":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar23 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar23[1] = labelTool1.
        popupMenuTool13:Tools:AddRange(arrayvar23).
        popupMenuTool13:ForceApplyResources = "SharedPropsInternal":U.
        resources:ApplyResources(labelTool2:SharedPropsInternal, "labelTool2.SharedPropsInternal":U).
        labelTool2:ForceApplyResources = "SharedPropsInternal":U.
        appearance30:Image = CAST(resources:GetObject("appearance30.Image":U), System.Object).
        buttonTool51:SharedPropsInternal:AppearancesLarge:Appearance = appearance30.
        appearance31:Image = CAST(resources:GetObject("appearance31.Image":U), System.Object).
        buttonTool51:SharedPropsInternal:AppearancesSmall:Appearance = appearance31.
        resources:ApplyResources(buttonTool51:SharedPropsInternal, "buttonTool51.SharedPropsInternal":U).
        buttonTool51:ForceApplyResources = "SharedPropsInternal":U.
        controlContainerTool2:ControlName = "ultraPictureBox1":U.
        appearance32:BorderAlpha = Infragistics.Win.Alpha:Transparent.
        controlContainerTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance32.
        controlContainerTool2:SharedPropsInternal:Width = 133.
        appearance33:Image = CAST(resources:GetObject("appearance33.Image":U), System.Object).
        buttonTool58:SharedPropsInternal:AppearancesLarge:Appearance = appearance33.
        appearance34:Image = CAST(resources:GetObject("appearance34.Image":U), System.Object).
        buttonTool58:SharedPropsInternal:AppearancesSmall:Appearance = appearance34.
        resources:ApplyResources(buttonTool58:SharedPropsInternal, "buttonTool58.SharedPropsInternal":U).
        buttonTool58:ForceApplyResources = "SharedPropsInternal":U.
        appearance35:Image = CAST(resources:GetObject("appearance35.Image":U), System.Object).
        stateButtonTool27:SharedPropsInternal:AppearancesLarge:Appearance = appearance35.
        resources:ApplyResources(stateButtonTool27:SharedPropsInternal, "stateButtonTool27.SharedPropsInternal":U).
        stateButtonTool27:ForceApplyResources = "SharedPropsInternal":U.
        appearance36:Image = CAST(resources:GetObject("appearance36.Image":U), System.Object).
        buttonTool35:SharedPropsInternal:AppearancesLarge:Appearance = appearance36.
        appearance37:Image = CAST(resources:GetObject("appearance37.Image":U), System.Object).
        buttonTool35:SharedPropsInternal:AppearancesSmall:Appearance = appearance37.
        resources:ApplyResources(buttonTool35:SharedPropsInternal, "buttonTool35.SharedPropsInternal":U).
        buttonTool35:ForceApplyResources = "SharedPropsInternal":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar24 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 26 NO-UNDO.
        arrayvar24[1] = buttonTool1.
        arrayvar24[2] = buttonTool6.
        arrayvar24[3] = buttonTool7.
        arrayvar24[4] = buttonTool8.
        arrayvar24[5] = buttonTool9.
        arrayvar24[6] = buttonTool16.
        arrayvar24[7] = buttonTool17.
        arrayvar24[8] = buttonTool18.
        arrayvar24[9] = buttonTool19.
        arrayvar24[10] = buttonTool20.
        arrayvar24[11] = popupMenuTool6.
        arrayvar24[12] = popupMenuTool7.
        arrayvar24[13] = popupMenuTool8.
        arrayvar24[14] = popupMenuTool9.
        arrayvar24[15] = popupMenuTool10.
        arrayvar24[16] = buttonTool22.
        arrayvar24[17] = mdiWindowListTool2.
        arrayvar24[18] = buttonTool34.
        arrayvar24[19] = buttonTool36.
        arrayvar24[20] = popupMenuTool13.
        arrayvar24[21] = labelTool2.
        arrayvar24[22] = buttonTool51.
        arrayvar24[23] = controlContainerTool2.
        arrayvar24[24] = buttonTool58.
        arrayvar24[25] = stateButtonTool27.
        arrayvar24[26] = buttonTool35.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar24).
        THIS-OBJECT:smartToolbarController1:ToolClick:Subscribe(THIS-OBJECT:smartToolbarController1_ToolClick).
        /*  */
        /* m_CustomerExplorer_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left, "m_CustomerExplorer_Toolbars_Dock_Area_Left":U).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left:Name = "m_CustomerExplorer_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_CustomerExplorer_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right, "m_CustomerExplorer_Toolbars_Dock_Area_Right":U).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right:Name = "m_CustomerExplorer_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_CustomerExplorer_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        resources:ApplyResources(THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top, "m_CustomerExplorer_Toolbars_Dock_Area_Top":U).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top:Name = "m_CustomerExplorer_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_CustomerExplorer_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        resources:ApplyResources(THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom, "m_CustomerExplorer_Toolbars_Dock_Area_Bottom":U).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom:Name = "m_CustomerExplorer_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* windowDockingArea4 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:windowDockingArea4, "windowDockingArea4":U).
        THIS-OBJECT:windowDockingArea4:Name = "windowDockingArea4":U.
        THIS-OBJECT:windowDockingArea4:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* ultraStatusBar1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraStatusBar1, "ultraStatusBar1":U).
        THIS-OBJECT:ultraStatusBar1:Name = "ultraStatusBar1":U.
        ultraStatusPanel1:Key = "status":U.
        ultraStatusPanel1:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Spring.
        ultraStatusPanel1:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel2:Key = "logincompanyname":U.
        ultraStatusPanel2:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel2:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel3:Key = "username":U.
        ultraStatusPanel3:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel3:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel4:Key = "system":U.
        ultraStatusPanel4:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel5:Key = "proversion":U.
        ultraStatusPanel5:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel6:Key = "language":U.
        ultraStatusPanel6:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel6:Style = Infragistics.Win.UltraWinStatusBar.PanelStyle:Button.
        resources:ApplyResources(ultraStatusPanel6, "ultraStatusPanel6":U).
        ultraStatusPanel7:Key = "date":U.
        ultraStatusPanel7:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel7:Style = Infragistics.Win.UltraWinStatusBar.PanelStyle:Date.
        ultraStatusPanel7:WrapText = Infragistics.Win.DefaultableBoolean:False.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar25 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel EXTENT 7 NO-UNDO.
        arrayvar25[1] = ultraStatusPanel1.
        arrayvar25[2] = ultraStatusPanel2.
        arrayvar25[3] = ultraStatusPanel3.
        arrayvar25[4] = ultraStatusPanel4.
        arrayvar25[5] = ultraStatusPanel5.
        arrayvar25[6] = ultraStatusPanel6.
        arrayvar25[7] = ultraStatusPanel7.
        THIS-OBJECT:ultraStatusBar1:Panels:AddRange(arrayvar25).
        THIS-OBJECT:ultraStatusBar1:SizeGripVisible = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraStatusBar1:ViewStyle = Infragistics.Win.UltraWinStatusBar.ViewStyle:Standard.
        THIS-OBJECT:ultraStatusBar1:ButtonClick:Subscribe(THIS-OBJECT:ultraStatusBar1_ButtonClick).
        /*  */
        /* windowDockingArea5 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:windowDockingArea5, "windowDockingArea5":U).
        THIS-OBJECT:windowDockingArea5:Name = "windowDockingArea5":U.
        THIS-OBJECT:windowDockingArea5:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* ultraDesktopAlert1 */
        /*  */
        THIS-OBJECT:ultraDesktopAlert1:AnimationSpeed = Infragistics.Win.Misc.AnimationSpeed:Slow.
        THIS-OBJECT:ultraDesktopAlert1:AnimationStyleAutoClose = Infragistics.Win.Misc.AnimationStyle:Fade.
        THIS-OBJECT:ultraDesktopAlert1:AnimationStyleShow = Infragistics.Win.Misc.AnimationStyle:Scroll.
        THIS-OBJECT:ultraDesktopAlert1:AutoClose = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraDesktopAlert1:AutoCloseDelay = 60000.
        THIS-OBJECT:ultraDesktopAlert1:CloseButtonVisible = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraDesktopAlert1:MultipleWindowDisplayStyle = Infragistics.Win.Misc.MultipleWindowDisplayStyle:Tiled.
        THIS-OBJECT:ultraDesktopAlert1:Style = Infragistics.Win.Misc.DesktopAlertStyle:Office2007.
        /*  */
        /* ultraPictureBox1 */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance5:BorderAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraPictureBox1:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:ultraPictureBox1, "ultraPictureBox1":U).
        THIS-OBJECT:ultraPictureBox1:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraPictureBox1:BorderShadowColor = System.Drawing.Color:Empty.
        THIS-OBJECT:ultraPictureBox1:BorderStyle = Infragistics.Win.UIElementBorderStyle:None.
        THIS-OBJECT:ultraPictureBox1:Image = CAST(resources:GetObject("ultraPictureBox1.Image":U), System.Object).
        THIS-OBJECT:ultraPictureBox1:Name = "ultraPictureBox1":U.
        THIS-OBJECT:ultraPictureBox1:Click:Subscribe(THIS-OBJECT:ultraPictureBox1_Click).
        /*  */
        /* windowDockingArea6 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:windowDockingArea6, "windowDockingArea6":U).
        THIS-OBJECT:windowDockingArea6:Name = "windowDockingArea6":U.
        THIS-OBJECT:windowDockingArea6:Owner = THIS-OBJECT:ultraDockManager1.
        /*  */
        /* MainMenuForm */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorerAutoHideControl).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPictureBox1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea5).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea6).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaTop).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaBottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaLeft).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaRight).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraStatusBar1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top).
        THIS-OBJECT:IsMdiContainer = TRUE.
        THIS-OBJECT:Name = "MainMenuForm":U.
        CAST(THIS-OBJECT:ultraExplorerBar1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraGridColumnChooser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabbedMdiManager1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow1:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow6:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow3:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow4:ResumeLayout(FALSE).
        THIS-OBJECT:windowDockingArea1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraStatusBar1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraDesktopAlert1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Loads an Application Style Library (.isl)
        Notes:   Style libraries are located in the Consulingwerk/Windows/Styles folder
        @param pcStyleSet The name of the Styleset
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID LoadApplicationStyle (pcStyleSet AS CHARACTER):

        DEFINE VARIABLE oService AS Consultingwerk.Windows.IAppStylistService NO-UNDO .

        oService = {Consultingwerk/get-service.i
                        Consultingwerk.Windows.IAppStylistService
                        "NEW Consultingwerk.Windows.AppStylistService ()"} .

        oService:LoadStyleLibrary (pcStyleSet) .

        CATCH ex AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ex, "Error loading style library file"{&TRAN}) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Loads  stored window settings from the registry
        Notes:   This method is intended to be overridden to store the settings to
                 a users profile (i.e. in the database).
        @param pcSettingsKey The key under which the settings for this Form are stored (e.g. Position)
        @return The string containing the stored window settings
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED CHARACTER LoadSettings (pcSettingsKey AS CHARACTER):

        DEFINE VARIABLE cSettings AS CHARACTER NO-UNDO.

        ASSIGN cSettings = SUPER:LoadSettings (pcSettingsKey) .

        IF pcSettingsKey = "Position":U AND (cSettings = "":U OR cSettings = ?) THEN
            RETURN SUBSTITUTE ("&1,&2,&3,&4,Normal":U,
                               THIS-OBJECT:Top,
                               THIS-OBJECT:Left,
                               THIS-OBJECT:Width,
                               THIS-OBJECT:Height) .
        ELSE
            RETURN cSettings .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the LoginCompanyNameChanged event of the SessionManager
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID LoginCompanyNameChangedHandler ():

        THIS-OBJECT:ultraStatusBar1:Panels["logincompanyname":U]:Text = SessionManager:LoginCompanyName .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosing event
        Notes:
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnFormClosing (INPUT e AS FormClosingEventArgs):

        DEFINE VARIABLE oChildForm AS System.Windows.Forms.Form NO-UNDO .

        DEFINE VARIABLE oMessageProvider AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO .
        DEFINE VARIABLE oMessage         AS Consultingwerk.SmartFramework.Message          NO-UNDO .
        DEFINE VARIABLE oSettingsService AS Consultingwerk.Framework.ISettingsService      NO-UNDO .

        oMessageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IMessageProvider} .

        IF NOT VALID-OBJECT (oCloseTimer) AND SessionManager:UserName > "":U AND VALID-OBJECT (oMessageProvider) THEN DO ON ERROR UNDO, THROW:
            oMessage = oMessageProvider:GetMessage ("SFR":U, 1, FrameworkSettings:ApplicationLabel) .

            THIS-OBJECT:Activate() .

            IF MessageFormHelper:ShowMessage (oMessage) = DialogResultEnum:DialogResultNo THEN
            DO:
                e:Cancel = TRUE .
                RETURN .
            END.

            CATCH err AS Progress.Lang.Error :
                ErrorHelper:ShowErrorMessage (err) .
            END CATCH.
        END.

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        IF VALID-OBJECT (oSettingsService) THEN DO:

            IF VALID-OBJECT (ultraExplorerBar1:SelectedGroup) AND ultraExplorerBar1:SelectedGroup:Key > "":U THEN

                oSettingsService:StoreSetting (THIS-OBJECT:WindowPositionRegistryKey,
                                               "ActiveExplorerBarGroup":U,
                                               ultraExplorerBar1:SelectedGroup:Key) .

        END.

        SUPER:OnFormClosing(INPUT e).

        DockManagerHelper:StoreToSettingsService (ultraDockManager1,
                                                  THIS-OBJECT:WindowPositionRegistryKey) .

        IF NOT THIS-OBJECT:backgroundJobMonitorControl1:StopAllBackgroundJobs() THEN DO:
            e:Cancel = TRUE .
            RETURN .
        END.

        IF NOT e:Cancel THEN DO:

            DO WHILE THIS-OBJECT:MdiChildren:Length > 0:

                oChildForm = CAST (THIS-OBJECT:MdiChildren:GetValue(0),
                              System.Windows.Forms.Form) .

                oChildForm:Close () .

                IF VALID-OBJECT (oChildForm) THEN
                    DELETE OBJECT oChildForm .

            END.
        END.

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        DEFINE VARIABLE oControls                  AS "System.Windows.Forms.Control[]":U NO-UNDO .
        DEFINE VARIABLE oPane                      AS DockableControlPane                NO-UNDO .
        DEFINE VARIABLE oSystemDescription         AS ISystemDescription                 NO-UNDO .
        DEFINE VARIABLE oSystemDescriptionProvider AS ISystemDescriptionProvider         NO-UNDO .
        DEFINE VARIABLE oDockableControlPane       AS DockableControlPane                NO-UNDO .

        THIS-OBJECT:DisplayLanguageName () .

        SUPER:OnLoad (e) .

        ultraStatusBar1:Panels["proversion":U]:Text = SessionHelper:FullVersionString() .

        oSystemDescriptionProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.ISystemDescriptionProvider
                                                                   "NEW Consultingwerk.Framework.SystemDescriptionProvider ()"} .

         oSystemDescription = oSystemDescriptionProvider:GetSystemDescription() .

        IF VALID-OBJECT (oSystemDescription) THEN
             ultraStatusBar1:Panels["system":U]:Text = oSystemDescription:Description .

        /* DockManager */
        DockManagerHelper:RestoreFromSettingsService (ultraDockManager1,
                                                      THIS-OBJECT:WindowPositionRegistryKey) .

        PROCESS EVENTS .

        /* Mike Fechner, Consultingwerk Ltd. 21.08.2013
           LogManagerSettings Dockable Pane only visible, when DebugMode */
         oDockableControlPane = ultraDockManager1:PaneFromControl (logManagerSettings1) .

        IF VALID-OBJECT (oDockableControlPane) THEN DO:
             IF FrameworkSettings:DebugMode THEN
                oDockableControlPane:Show () .
             ELSE
                oDockableControlPane:Close () .
        END.

        IF smartToolbarController1:Tools:Exists ("ApplicationMenu":U) AND
           VALID-OBJECT (ultraDockManager1:PaneFromControl (ultraExplorerBar1)) THEN
            Consultingwerk.Util.UltraToolbarsHelper:SetStateButtonToolChecked (smartToolbarController1,
                                                                               "ApplicationMenu":U,
                                                                               ultraDockManager1:PaneFromControl (ultraExplorerBar1):IsVisible) .

        /* Mike Fechner, Consultingwerk Ltd. 12.10.2012
           Fix settings for new Dockable pane */
        oPane = ultraDockManager1:PaneFromControl (THIS-OBJECT:backgroundJobMonitorControl1) .

        IF NOT VALID-OBJECT (oPane) THEN DO:
            oControls = {Consultingwerk/new-array.i System.Windows.Forms.Control 1} .
            oControls:SetValue (THIS-OBJECT:backgroundJobMonitorControl1, 0) .

            ultraDockManager1:DockControls (oControls,
                                            DockedLocation:DockedBottom,
                                            ChildPaneStyle:TabGroup) .

            oPane = ultraDockManager1:PaneFromControl (THIS-OBJECT:backgroundJobMonitorControl1) .
            IF VALID-OBJECT (oPane) THEN DO:
                oPane:Text = "Job Monitor"{&TRAN}.
/*                oPane:Settings:Appearance:Image = ImageHelper:Load                                               */
/*                        ("Consultingwerk/SmartComponentsDemo/CustomerExplorer/Images/media_fast_forward.png":U) .*/
            END.
        END.

        {Consultingwerk/foreach.i DockableControlPane oControlPane in ultraDockManager1:ControlPanes}
            oControlPane:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:FALSE .
        END.

        {Consultingwerk/foreach.i DockAreaPane oAreaPane in ultraDockManager1:DockAreas}
            oAreaPane:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:FALSE .
        END.

        CATCH err AS Progress.Lang.Error :
            IF VALID-OBJECT (oSplash) THEN DO ON ERROR UNDO, THROW:

                oSplash:Close () .

                CATCH err2 AS Progress.Lang.Error:
                    /* ignore */
                END CATCH.
            END.

            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .

            System.Windows.Forms.Application:Exit() .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the MdiChildActivate event
        Notes:
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnMdiChildActivate (e AS System.EventArgs):

        DEFINE VARIABLE oChildToolbar AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO .

        SUPER:OnMdiChildActivate (e) .

        oChildToolbar = THIS-OBJECT:smartToolbarController1:ActiveMdiChildManager .

        IF VALID-OBJECT (oChildToolbar) THEN DO:

            IF oChildToolbar:Ribbon:Tabs:Count > 0 THEN DO:

                THIS-OBJECT:smartToolbarController1:Ribbon:SelectedTab =
                    oChildToolbar:Ribbon:Tabs[0]:AttachedParentTab .

            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnShown (INPUT e AS System.EventArgs):

        SUPER:OnShown(e).

        THIS-OBJECT:Enabled = FALSE .

        oAfterShownTimer = NEW System.Windows.Forms.Timer () .
        oAfterShownTimer:Tick:Subscribe (AfterShownHandler) .
        oAfterShownTimer:Interval = 1 .
        oAfterShownTimer:Enabled = TRUE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Tick  event of the oAfterShownTimer
        Notes:   Delays initialization of the SandBoxForm and LoginForm until after
                 all Shown() handling is completed
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID AfterShownHandler (sender AS System.Object,
                                           e AS System.EventArgs):

        DEFINE VARIABLE lLoginOk             AS LOGICAL              NO-UNDO .
        DEFINE VARIABLE oLightBox            AS LightBoxForm         NO-UNDO .
        DEFINE VARIABLE oTranslationProvider AS ITranslationProvider NO-UNDO .
        DEFINE VARIABLE oSettingsService     AS ISettingsService     NO-UNDO .
        DEFINE VARIABLE cGroupKey            AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE oMainMenuRenderer    AS IMainMenuRenderer    NO-UNDO .

        IF VALID-OBJECT (oAfterShownTimer) THEN DO:
            oAfterShownTimer:Enabled = FALSE .
            DELETE OBJECT oAfterShownTimer .
        END.

        THIS-OBJECT:Enabled = TRUE .

        /* Mike Fechner, Consultingwerk Ltd. 02.11.2009
           Create a Timer to Close the Splash screen */
        IF VALID-OBJECT (oSplash) THEN DO:

            oSplash:Close ().
            oSplash = ? .

            UserInterfaceHelper:ProcessEvents() .

            oLightBox = NEW LightBoxForm (THIS-OBJECT) .
            oLightBox:Show () .

            DO ON ERROR UNDO, THROW:
                SessionManager:Login (THIS-OBJECT, OUTPUT lLoginOk) .

                CATCH err AS Progress.Lang.Error:

                    ErrorHelper:ShowErrorMessage (err) .

                    ASSIGN lLoginOk = FALSE .
                END CATCH.
            END.

            IF VALID-OBJECT (oLightBox) THEN
                oLightBox:Close () .
            IF VALID-OBJECT (oLightBox) THEN
                oLightBox:Dispose () .
            IF VALID-OBJECT (oLightBox) THEN
                DELETE OBJECT oLightBox .

            UserInterfaceHelper:ProcessEvents() .

            IF NOT lLoginOk THEN DO:
                THIS-OBJECT:DeleteOnFormClosed = TRUE .

                oCloseTimer = NEW System.Windows.Forms.Timer () .
                oCloseTimer:Interval = 5 .
                oCloseTimer:Tick:Subscribe (FormCloseTimerHandler) .
                oCloseTimer:Enabled = TRUE .

                RETURN .
            END.
        END.

        oMainMenuRenderer = NewMainMenuRenderer ().
        oMainMenuRenderer:RenderMenuStructure (THIS-OBJECT:ultraExplorerBar1,
                                               THIS-OBJECT:smartToolbarController1,
                                               THIS-OBJECT).

        oTranslationProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ITranslationProvider} .

        IF VALID-OBJECT (oTranslationProvider) THEN
            oTranslationProvider:CacheTranslationsByScope ("Button":U) .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        IF VALID-OBJECT (oSettingsService) THEN DO:

            ASSIGN cGroupKey = oSettingsService:GetSetting (THIS-OBJECT:WindowPositionRegistryKey,
                                                            "ActiveExplorerBarGroup":U) .

            IF cGroupKey > "":U AND ultraExplorerBar1:Groups:Exists (cGroupKey) THEN DO:
                ultraExplorerBar1:Groups [cGroupKey]:Selected = TRUE.
                ultraExplorerBar1:Groups [cGroupKey]:Active = TRUE.
            END.
        END.

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oMainMenuRenderer).
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the static SmartDataBrowserActivated event
        Notes:   Controls the Grid which is linked to the UltraGridColumnChooser Control
        @param sender The reference to the object that raised this event
        @param e The System.EventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SmartDataBrowserActivatedHandler (sender AS System.Object,
                                                          e AS System.EventArgs):

        IF TYPE-OF (sender, Infragistics.Win.UltraWinGrid.UltraGrid) THEN
            ultraGridColumnChooser1:SourceGrid = CAST (sender, Infragistics.Win.UltraWinGrid.UltraGrid) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the smartToolbarController1
        Notes:
        @param sender The reference to the object thait raised the event
        @param e The ToolClickEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartToolbarController1_ToolClick (sender AS System.Object,
                                                           e AS ToolClickEventArgs):

        DEFINE VARIABLE oMainMenuRenderer AS IMainMenuRenderer NO-UNDO .

        CASE e:Tool:Key:

            WHEN "StartNewSession":U THEN
                THIS-OBJECT:StartNewSession () .

            WHEN "LaunchForm":U THEN
                Consultingwerk.Windows.FormLauncher:LaunchForm () .

            WHEN "ApplicationMenu":U THEN
                ToggleApplicationMenu () .

            WHEN "Reload Menu":U THEN DO:
                oMainMenuRenderer = NewMainMenuRenderer ().
                oMainMenuRenderer:RenderMenuStructure (THIS-OBJECT:ultraExplorerBar1,
                                                       THIS-OBJECT:smartToolbarController1,
                                                       THIS-OBJECT).
            END.

        END CASE.

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oMainMenuRenderer).
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Clones the current session
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StartNewSession ():

        DEFINE VARIABLE oStartInfo AS System.Diagnostics.ProcessStartInfo NO-UNDO .

        oStartInfo = NEW System.Diagnostics.ProcessStartInfo () .
        oStartInfo:FileName = Consultingwerk.Util.SessionHelper:ExecutablePath() .
        oStartInfo:Arguments = Consultingwerk.Util.SessionHelper:StartupParameters() .
        oStartInfo:WorkingDirectory = Consultingwerk.Util.SessionHelper:CurrentDirectory() .

        System.Diagnostics.Process:Start (oStartInfo) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the StyleLibraryChanged event of the IAppStylistService
        Notes:
        @param sender The object that raised the StyleLibraryChanged event
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StyleLibraryChangedHandler (sender AS Progress.Lang.Object,
                                                      e AS Consultingwerk.EventArgs):

&IF DEFINED (WinKitDemo) NE 0 &THEN
        /* Initialize WinKit colors */
        Consultingwerk.Util.StyleLibraryHelper:SetProgressColorTable
            (17, "UltraPanel":U, RoleState:Normal, "WinKit":U) .
&ENDIF

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Toggles the visibility of the Application Menu
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ToggleApplicationMenu ():

        DEFINE VARIABLE oPane AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO .

        oPane = ultraDockManager1:PaneFromControl (ultraExplorerBar1) .

        IF UltraToolbarsHelper:GetStateButtonToolChecked (smartToolbarController1,
                                                          "ApplicationMenu":U) THEN
            oPane:Show () .
        ELSE
            oPane:Close () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ContextMenuInitializing event of the
                 ultraExplorerBar1
        Notes:   Deactivates the Context menu of the Control
        @param sender The reference to object that raised the event
        @param e The CancelableContextMenuInitializingEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraExplorerBar1_ContextMenuInitializing (sender AS System.Object,
                                                                   e AS Infragistics.Win.UltraWinExplorerBar.CancelableContextMenuInitializingEventArgs):

        e:Cancel = TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ItemRemoving then of the ultraExplorerBar1
        Notes:   Cancels removal of any item
        @param sender The reference to object that raised the event
        @param e The CancelableItemEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraExplorerBar1_ItemRemoving (sender AS System.Object,
                                                        e AS Infragistics.Win.UltraWinExplorerBar.CancelableItemEventArgs):

        e:Cancel = TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraPictureBox1
        Notes:   Starts a Web Browser with the Consultingwerk facebook site
        @param sender The reference to object that raised the event
        @param e The EventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraPictureBox1_Click (sender AS System.Object,
                                                e AS System.EventArgs):

        Consultingwerk.Windows.API.Win32:ShellExecute ("open":U,
                                                       "http://www.facebook.com/Consultingwerk":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ButtonClick event of the ultraStatusBar1
        Notes:
        @param sender The reference to the object that raised this event
        @param e The PanelEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraStatusBar1_ButtonClick (sender AS System.Object,
                                                     e AS Infragistics.Win.UltraWinStatusBar.PanelEventArgs):

        &IF DEFINED (SmartFramework) NE 0 &THEN
        DEFINE VARIABLE oLanguageForm AS SwitchUserInterfaceLanguageForm NO-UNDO .

        IF e:Panel:Key = "language":U THEN DO:
            oLanguageForm = NEW SwitchUserInterfaceLanguageForm () .

            WAIT-FOR oLanguageForm:ShowDialog (THIS-OBJECT) .
        END.
        &ENDIF

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the UserNameChanged of the SessionManager
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UserNameChangedHandler ():

        THIS-OBJECT:ultraStatusBar1:Panels["username":U]:Text = SessionManager:UserName .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the MainMenuForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC MainMenuForm ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
