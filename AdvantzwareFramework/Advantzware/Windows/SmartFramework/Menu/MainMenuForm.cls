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
{system/sysconst.i}

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
USING Consultingwerk.WindowIntegrationKit.Forms.* FROM PROPATH.

CLASS Advantzware.Windows.SmartFramework.Menu.MainMenuForm
    INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE dockableWindow3 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE dockableWindow6 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
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

    DEFINE VARIABLE oSplash AS Advantzware.Windows.SmartFramework.Authentication.Splash NO-UNDO .
    DEFINE PRIVATE VARIABLE windowDockingArea5 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.

    DEFINE VARIABLE oCloseTimer      AS System.Windows.Forms.Timer NO-UNDO .
    DEFINE VARIABLE oAfterShownTimer AS System.Windows.Forms.Timer NO-UNDO .

&IF DEFINED (SmartFramework) NE 0 &THEN
    { Consultingwerk/SmartFramework/Authorization/dsMenuGroup.i }
    { Consultingwerk/SmartFramework/Menu/dsMenu.i }
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CustomerExplorer class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MainMenuForm ():

        DEFINE VARIABLE oStyleService AS Consultingwerk.Windows.IAppStylistService  NO-UNDO .

        SUPER().
        InitializeComponent().

        SessionManager:UserNameChanged:Subscribe(UserNameChangedHandler) .
        SessionManager:LoginCompanyNameChanged:Subscribe(LoginCompanyNameChangedHandler) .

        Consultingwerk.Framework.FrameworkSettings:MdiContainer = THIS-OBJECT .

        oSplash = NEW Advantzware.Windows.SmartFramework.Authentication.Splash () .
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
        Consultingwerk.WindowIntegrationKit.Controls.WinKitControls:RenderedBrowseControlActivated:Subscribe (SmartDataBrowserActivatedHandler) . 

        SessionManager:LanguageDisplayNameChanged:Subscribe (DisplayLanguageName) .

        ASSIGN SessionManager:LoginFormClassName = "Advantzware.Windows.SmartFramework.Authentication.LoginForm" .

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

        THIS-OBJECT:ultraStatusBar1:Panels ["language":U]:Text = "Language: " + SessionManager:LanguageDisplayName .

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
        Purpose: Initializes the Visual Design
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeComponent ():

        /* NOTE: The following method is automatically generated.

        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.

        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE dockAreaPane1 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane1 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedLeft, NEW System.Guid("0b3aa0d5-1ba1-47d9-b8f7-67212859fbf1")).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE dockableControlPane1 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane1 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("007f2fb8-f8f0-40a5-98e4-3650503edc40"), NEW System.Guid("00000000-0000-0000-0000-000000000000"), -1, NEW System.Guid("0b3aa0d5-1ba1-47d9-b8f7-67212859fbf1"), -1).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Advantzware.Windows.SmartFramework.Menu.MainMenuForm").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE dockAreaPane2 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane2 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedRight, NEW System.Guid("5f63358c-7481-42af-80c1-369dae518cf8")).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE dockableControlPane2 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane2 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("c7b44b28-45c5-4ec1-908a-fe653738cd78"), NEW System.Guid("7557a4e2-a353-42ef-83bc-51e4281055e8"), -1, NEW System.Guid("5f63358c-7481-42af-80c1-369dae518cf8"), 0).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE dockAreaPane3 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane3 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:Floating, NEW System.Guid("7557a4e2-a353-42ef-83bc-51e4281055e8")).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE dockAreaPane4 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane4 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedRight, NEW System.Guid("d5f9cdd7-5c09-44ef-8fa6-31e4217132e2")).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE dockableControlPane3 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane3 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("4576c7d8-74df-4fbe-b22b-71beb26f36e9"), NEW System.Guid("00000000-0000-0000-0000-000000000000"), -1, NEW System.Guid("d5f9cdd7-5c09-44ef-8fa6-31e4217132e2"), -1).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE optionSet1 AS Infragistics.Win.UltraWinToolbars.OptionSet NO-UNDO.
        optionSet1 = NEW Infragistics.Win.UltraWinToolbars.OptionSet("View").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool56 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool56 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool38 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool38 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool37 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool37 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool40 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool40 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool39 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool39 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool41 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool41 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool42 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool42 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE contextualTabGroup1 AS Infragistics.Win.UltraWinToolbars.ContextualTabGroup NO-UNDO.
        contextualTabGroup1 = NEW Infragistics.Win.UltraWinToolbars.ContextualTabGroup("search").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("start").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool23 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool23 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool24 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool24 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool25 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool25 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool26 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool26 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool27 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool27 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool28 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool28 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup2").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool29 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool29 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool30 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool30 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool31 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool31 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool32 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool32 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup4").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool57 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool57 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("LaunchForm").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ribbonTab2 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab2 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("view").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ribbonGroup4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup4 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup2").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE popupMenuTool11 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool11 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Window").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE stateButtonTool26 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool26 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("ApplicationMenu", "").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool33 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool33 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Reload Menu").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool50 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool50 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("StartNewSession").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ribbonGroup5 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup5 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("links").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE controlContainerTool1 AS Infragistics.Win.UltraWinToolbars.ControlContainerTool NO-UNDO.
        controlContainerTool1 = NEW Infragistics.Win.UltraWinToolbars.ControlContainerTool("ControlContainerTool1").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool43 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool43 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool44 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool44 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TableIOToolbar").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool15 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool15 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("UltraToolbar1").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE popupMenuTool1 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool1 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("File").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE popupMenuTool2 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool2 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Edit").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE popupMenuTool3 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool3 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Options").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE popupMenuTool4 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool4 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Window").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE popupMenuTool5 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool5 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Help").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraToolbar3 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar3 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("NavigationToolbar").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraToolbar4 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar4 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("View").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool16 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool16 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool17 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool17 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool20 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool20 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE popupMenuTool6 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool6 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("File").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool21 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool21 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE popupMenuTool7 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool7 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Edit").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE popupMenuTool8 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool8 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Options").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE popupMenuTool9 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool9 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Window").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE mdiWindowListTool1 AS Infragistics.Win.UltraWinToolbars.MdiWindowListTool NO-UNDO.
        mdiWindowListTool1 = NEW Infragistics.Win.UltraWinToolbars.MdiWindowListTool("MDIWindowListTool1").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE popupMenuTool10 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool10 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Help").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool22 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool22 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE mdiWindowListTool2 AS Infragistics.Win.UltraWinToolbars.MdiWindowListTool NO-UNDO.
        mdiWindowListTool2 = NEW Infragistics.Win.UltraWinToolbars.MdiWindowListTool("MDIWindowListTool1").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool34 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool34 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ClearSearch").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool36 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool36 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("StartSearch").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE popupMenuTool13 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool13 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("PreviousSearches").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
        appearance28 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE labelTool1 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool1 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("(empty)").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE labelTool2 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool2 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("(empty)").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool51 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool51 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("StartNewSession").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
        appearance29 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
        appearance30 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE controlContainerTool2 AS Infragistics.Win.UltraWinToolbars.ControlContainerTool NO-UNDO.
        controlContainerTool2 = NEW Infragistics.Win.UltraWinToolbars.ControlContainerTool("ControlContainerTool1").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
        appearance31 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool58 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool58 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("LaunchForm").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
        appearance32 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
        appearance33 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE stateButtonTool27 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool27 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("ApplicationMenu", "").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
        appearance34 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool35 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool35 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Reload Menu").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance35 AS Infragistics.Win.Appearance NO-UNDO.
        appearance35 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance36 AS Infragistics.Win.Appearance NO-UNDO.
        appearance36 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraStatusPanel1 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel1 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraStatusPanel2 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel2 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraStatusPanel3 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel3 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraStatusPanel4 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel4 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraStatusPanel5 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel5 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraStatusPanel6 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel6 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraStatusPanel7 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel7 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:ultraExplorerBar1 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBar().
        THIS-OBJECT:ultraGridColumnChooser1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumnChooser().
        THIS-OBJECT:logManagerSettings1 = NEW Consultingwerk.Windows.Util.Controls.LogManagerSettings().
        THIS-OBJECT:ultraTabbedMdiManager1 = NEW Infragistics.Win.UltraWinTabbedMdi.UltraTabbedMdiManager(THIS-OBJECT:components).
        THIS-OBJECT:ultraDockManager1 = NEW Infragistics.Win.UltraWinDock.UltraDockManager(THIS-OBJECT:components).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaLeft = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaRight = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaTop = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaBottom = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_CustomerExplorerAutoHideControl = NEW Infragistics.Win.UltraWinDock.AutoHideControl().
        THIS-OBJECT:dockableWindow3 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:dockableWindow1 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:dockableWindow6 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
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
        CAST(THIS-OBJECT:ultraExplorerBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraGridColumnChooser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabbedMdiManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:SuspendLayout().
        THIS-OBJECT:dockableWindow3:SuspendLayout().
        THIS-OBJECT:dockableWindow1:SuspendLayout().
        THIS-OBJECT:dockableWindow6:SuspendLayout().
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
        THIS-OBJECT:ultraExplorerBar1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:ultraExplorerBar1:Name = "ultraExplorerBar1".
        THIS-OBJECT:ultraExplorerBar1:NavigationMaxGroupHeaders = 5.
        THIS-OBJECT:ultraExplorerBar1:Size = NEW System.Drawing.Size(205, 420).
        THIS-OBJECT:ultraExplorerBar1:Style = Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarStyle:OutlookNavigationPane.
        THIS-OBJECT:ultraExplorerBar1:TabIndex = 5.
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
        THIS-OBJECT:ultraGridColumnChooser1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:ultraGridColumnChooser1:MultipleBandSupport = Infragistics.Win.UltraWinGrid.MultipleBandSupport:SingleBandOnly.
        THIS-OBJECT:ultraGridColumnChooser1:Name = "ultraGridColumnChooser1".
        THIS-OBJECT:ultraGridColumnChooser1:Size = NEW System.Drawing.Size(199, 373).
        THIS-OBJECT:ultraGridColumnChooser1:Style = Infragistics.Win.UltraWinGrid.ColumnChooserStyle:HiddenColumnsOnly.
        THIS-OBJECT:ultraGridColumnChooser1:StyleLibraryName = "".
        THIS-OBJECT:ultraGridColumnChooser1:StyleSetName = "".
        THIS-OBJECT:ultraGridColumnChooser1:TabIndex = 20.
        THIS-OBJECT:ultraGridColumnChooser1:Text = "Column Chooser".
        /*  */
        /* logManagerSettings1 */
        /*  */
        THIS-OBJECT:logManagerSettings1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:logManagerSettings1:Name = "logManagerSettings1".
        THIS-OBJECT:logManagerSettings1:Size = NEW System.Drawing.Size(179, 398).
        THIS-OBJECT:logManagerSettings1:TabIndex = 29.
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
        dockAreaPane1:DockedBefore = NEW System.Guid("5f63358c-7481-42af-80c1-369dae518cf8").
        dockableControlPane1:Control = THIS-OBJECT:ultraExplorerBar1.
        dockableControlPane1:OriginalControlBounds = NEW System.Drawing.Rectangle(35, 88, 205, 472).
        dockableControlPane1:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image"), System.Object).
        dockableControlPane1:Settings:Appearance = appearance1.
        dockableControlPane1:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane1:Text = "Application Menu".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar0[1] = dockableControlPane1.
        dockAreaPane1:Panes:AddRange(arrayvar0).
        dockAreaPane1:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        dockAreaPane1:Size = NEW System.Drawing.Size(205, 438).
        dockAreaPane2:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:TabGroup.
        dockAreaPane2:DockedBefore = NEW System.Guid("7557a4e2-a353-42ef-83bc-51e4281055e8").
        dockableControlPane2:Control = THIS-OBJECT:ultraGridColumnChooser1.
        dockableControlPane2:FlyoutSize = NEW System.Drawing.Size(199, -1).
        dockableControlPane2:OriginalControlBounds = NEW System.Drawing.Rectangle(494, 203, 140, 160).
        dockableControlPane2:Pinned = FALSE.
        dockableControlPane2:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image"), System.Object).
        dockableControlPane2:Settings:Appearance = appearance2.
        dockableControlPane2:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane2:Text = "Column Chooser".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar1[1] = dockableControlPane2.
        dockAreaPane2:Panes:AddRange(arrayvar1).
        dockAreaPane2:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        dockAreaPane2:Size = NEW System.Drawing.Size(199, 416).
        dockAreaPane3:DockedBefore = NEW System.Guid("d5f9cdd7-5c09-44ef-8fa6-31e4217132e2").
        dockAreaPane3:FloatingLocation = NEW System.Drawing.Point(1170, 514).
        dockAreaPane3:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        dockAreaPane3:Size = NEW System.Drawing.Size(95, 446).
        dockableControlPane3:Control = THIS-OBJECT:logManagerSettings1.
        dockableControlPane3:FlyoutSize = NEW System.Drawing.Size(179, -1).
        dockableControlPane3:OriginalControlBounds = NEW System.Drawing.Rectangle(526, 199, 217, 314).
        dockableControlPane3:Pinned = FALSE.
        dockableControlPane3:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image"), System.Object).
        dockableControlPane3:Settings:Appearance = appearance3.
        dockableControlPane3:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane3:Text = "Logfile Settings".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar2[1] = dockableControlPane3.
        dockAreaPane4:Panes:AddRange(arrayvar2).
        dockAreaPane4:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        dockAreaPane4:Size = NEW System.Drawing.Size(179, 404).
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinDock.DockAreaPane EXTENT 4 NO-UNDO.
        arrayvar3[1] = dockAreaPane1.
        arrayvar3[2] = dockAreaPane2.
        arrayvar3[3] = dockAreaPane3.
        arrayvar3[4] = dockAreaPane4.
        THIS-OBJECT:ultraDockManager1:DockAreas:AddRange(arrayvar3).
        THIS-OBJECT:ultraDockManager1:DragWindowStyle = Infragistics.Win.UltraWinDock.DragWindowStyle:LayeredWindowWithIndicators.
        THIS-OBJECT:ultraDockManager1:HostControl = THIS-OBJECT.
        THIS-OBJECT:ultraDockManager1:WindowStyle = Infragistics.Win.UltraWinDock.WindowStyle:Office2007.
        /*  */
        /* m_CustomerExplorerUnpinnedTabAreaLeft */
        /*  */
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaLeft:Dock = System.Windows.Forms.DockStyle:Left.
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaLeft:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaLeft:Location = NEW System.Drawing.Point(8, 151).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaLeft:Name = "m_CustomerExplorerUnpinnedTabAreaLeft".
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaLeft:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaLeft:Size = NEW System.Drawing.Size(0, 438).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaLeft:TabIndex = 0.
        /*  */
        /* m_CustomerExplorerUnpinnedTabAreaRight */
        /*  */
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaRight:Dock = System.Windows.Forms.DockStyle:Right.
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaRight:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaRight:Location = NEW System.Drawing.Point(855, 151).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaRight:Name = "m_CustomerExplorerUnpinnedTabAreaRight".
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaRight:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaRight:Size = NEW System.Drawing.Size(21, 438).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaRight:TabIndex = 1.
        /*  */
        /* m_CustomerExplorerUnpinnedTabAreaTop */
        /*  */
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaTop:Dock = System.Windows.Forms.DockStyle:Top.
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaTop:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaTop:Location = NEW System.Drawing.Point(8, 151).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaTop:Name = "m_CustomerExplorerUnpinnedTabAreaTop".
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaTop:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaTop:Size = NEW System.Drawing.Size(847, 0).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaTop:TabIndex = 2.
        /*  */
        /* m_CustomerExplorerUnpinnedTabAreaBottom */
        /*  */
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaBottom:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaBottom:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaBottom:Location = NEW System.Drawing.Point(8, 589).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaBottom:Name = "m_CustomerExplorerUnpinnedTabAreaBottom".
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaBottom:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaBottom:Size = NEW System.Drawing.Size(847, 0).
        THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaBottom:TabIndex = 3.
        /*  */
        /* m_CustomerExplorerAutoHideControl */
        /*  */
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:Controls:Add(THIS-OBJECT:dockableWindow1).
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:Controls:Add(THIS-OBJECT:dockableWindow6).
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:Location = NEW System.Drawing.Point(8, 468).
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:Name = "m_CustomerExplorerAutoHideControl".
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:Size = NEW System.Drawing.Size(847, 100).
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:TabIndex = 4.
        /*  */
        /* dockableWindow3 */
        /*  */
        THIS-OBJECT:dockableWindow3:Controls:Add(THIS-OBJECT:ultraExplorerBar1).
        THIS-OBJECT:dockableWindow3:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:dockableWindow3:Name = "dockableWindow3".
        THIS-OBJECT:dockableWindow3:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow3:Size = NEW System.Drawing.Size(205, 438).
        THIS-OBJECT:dockableWindow3:TabIndex = 42.
        /*  */
        /* dockableWindow1 */
        /*  */
        THIS-OBJECT:dockableWindow1:Controls:Add(THIS-OBJECT:ultraGridColumnChooser1).
        THIS-OBJECT:dockableWindow1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:dockableWindow1:Name = "dockableWindow1".
        THIS-OBJECT:dockableWindow1:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow1:Size = NEW System.Drawing.Size(205, 438).
        THIS-OBJECT:dockableWindow1:TabIndex = 43.
        /*  */
        /* dockableWindow6 */
        /*  */
        THIS-OBJECT:dockableWindow6:Controls:Add(THIS-OBJECT:logManagerSettings1).
        THIS-OBJECT:dockableWindow6:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:dockableWindow6:Name = "dockableWindow6".
        THIS-OBJECT:dockableWindow6:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow6:Size = NEW System.Drawing.Size(205, 413).
        THIS-OBJECT:dockableWindow6:TabIndex = 44.
        /*  */
        /* windowDockingArea1 */
        /*  */
        THIS-OBJECT:windowDockingArea1:Controls:Add(THIS-OBJECT:dockableWindow3).
        THIS-OBJECT:windowDockingArea1:Dock = System.Windows.Forms.DockStyle:Left.
        THIS-OBJECT:windowDockingArea1:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea1:Location = NEW System.Drawing.Point(8, 151).
        THIS-OBJECT:windowDockingArea1:Name = "windowDockingArea1".
        THIS-OBJECT:windowDockingArea1:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea1:Size = NEW System.Drawing.Size(210, 438).
        THIS-OBJECT:windowDockingArea1:TabIndex = 7.
        /*  */
        /* windowDockingArea2 */
        /*  */
        THIS-OBJECT:windowDockingArea2:Dock = System.Windows.Forms.DockStyle:Right.
        THIS-OBJECT:windowDockingArea2:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea2:Location = NEW System.Drawing.Point(651, 152).
        THIS-OBJECT:windowDockingArea2:Name = "windowDockingArea2".
        THIS-OBJECT:windowDockingArea2:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea2:Size = NEW System.Drawing.Size(204, 416).
        THIS-OBJECT:windowDockingArea2:TabIndex = 8.
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm").
        THIS-OBJECT:smartToolbarController1:Office2007UICompatibility = FALSE.
        optionSet1:AllowAllUp = FALSE.
        THIS-OBJECT:smartToolbarController1:OptionSets:Add(optionSet1).
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar4[1] = buttonTool56.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar4).
        buttonTool41:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar5[1] = buttonTool38.
        arrayvar5[2] = buttonTool37.
        arrayvar5[3] = buttonTool40.
        arrayvar5[4] = buttonTool39.
        arrayvar5[5] = buttonTool41.
        arrayvar5[6] = buttonTool42.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar5).
        contextualTabGroup1:Caption = "Search".
        contextualTabGroup1:Key = "search".
        contextualTabGroup1:Visible = FALSE.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ContextualTabGroup EXTENT 1 NO-UNDO.
        arrayvar6[1] = contextualTabGroup1.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedContextualTabGroups:AddRange(arrayvar6).
        ribbonTab1:Caption = "Start".
        ribbonGroup1:Caption = "Edit".
        buttonTool23:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool24:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool25:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar7[1] = buttonTool23.
        arrayvar7[2] = buttonTool24.
        arrayvar7[3] = buttonTool25.
        arrayvar7[4] = buttonTool26.
        arrayvar7[5] = buttonTool27.
        arrayvar7[6] = buttonTool28.
        ribbonGroup1:Tools:AddRange(arrayvar7).
        ribbonGroup2:Caption = "Navigation".
        buttonTool29:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool30:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool31:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool32:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar8[1] = buttonTool29.
        arrayvar8[2] = buttonTool30.
        arrayvar8[3] = buttonTool31.
        arrayvar8[4] = buttonTool32.
        ribbonGroup2:Tools:AddRange(arrayvar8).
        ribbonGroup3:Caption = "Launch".
        buttonTool57:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar9[1] = buttonTool57.
        ribbonGroup3:Tools:AddRange(arrayvar9).
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 3 NO-UNDO.
        arrayvar10[1] = ribbonGroup1.
        arrayvar10[2] = ribbonGroup2.
        arrayvar10[3] = ribbonGroup3.
        ribbonTab1:Groups:AddRange(arrayvar10).
        ribbonTab2:Caption = "View".
        ribbonGroup4:Caption = "Windows".
        popupMenuTool11:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        stateButtonTool26:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool33:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool50:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar11[1] = popupMenuTool11.
        arrayvar11[2] = stateButtonTool26.
        arrayvar11[3] = buttonTool33.
        arrayvar11[4] = buttonTool50.
        ribbonGroup4:Tools:AddRange(arrayvar11).
        ribbonGroup5:Caption = "Links".
        controlContainerTool1:ControlName = "ultraPictureBox1".
        controlContainerTool1:InstanceProps:Width = 133.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar12 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar12[1] = controlContainerTool1.
        ribbonGroup5:Tools:AddRange(arrayvar12).
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar13 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 2 NO-UNDO.
        arrayvar13[1] = ribbonGroup4.
        arrayvar13[2] = ribbonGroup5.
        ribbonTab2:Groups:AddRange(arrayvar13).
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar14 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 2 NO-UNDO.
        arrayvar14[1] = ribbonTab1.
        arrayvar14[2] = ribbonTab2.
/*        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar14).*/
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar15 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar15[1] = buttonTool43.
        arrayvar15[2] = buttonTool44.
        THIS-OBJECT:smartToolbarController1:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar15).
        THIS-OBJECT:smartToolbarController1:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        THIS-OBJECT:smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 1.
        buttonTool10:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar16 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar16[1] = buttonTool10.
        arrayvar16[2] = buttonTool11.
        arrayvar16[3] = buttonTool12.
        arrayvar16[4] = buttonTool13.
        arrayvar16[5] = buttonTool14.
        arrayvar16[6] = buttonTool15.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar16).
        ultraToolbar1:Text = "TableIO Toolbar".
        ultraToolbar1:Visible = FALSE.
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar17 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 5 NO-UNDO.
        arrayvar17[1] = popupMenuTool1.
        arrayvar17[2] = popupMenuTool2.
        arrayvar17[3] = popupMenuTool3.
        arrayvar17[4] = popupMenuTool4.
        arrayvar17[5] = popupMenuTool5.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar17).
        ultraToolbar2:Text = "UltraToolbar1".
        ultraToolbar2:Visible = FALSE.
        ultraToolbar3:DockedColumn = 0.
        ultraToolbar3:DockedRow = 1.
        buttonTool2:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar18 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar18[1] = buttonTool2.
        arrayvar18[2] = buttonTool3.
        arrayvar18[3] = buttonTool4.
        arrayvar18[4] = buttonTool5.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar18).
        ultraToolbar3:Text = "Navigation Toolbar".
        ultraToolbar3:Visible = FALSE.
        ultraToolbar4:DockedColumn = 0.
        ultraToolbar4:DockedRow = 1.
        ultraToolbar4:Text = "View".
        ultraToolbar4:Visible = FALSE.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar19 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 4 NO-UNDO.
        arrayvar19[1] = ultraToolbar1.
        arrayvar19[2] = ultraToolbar2.
        arrayvar19[3] = ultraToolbar3.
        arrayvar19[4] = ultraToolbar4.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar19).
        appearance5:Image = CAST(resources:GetObject("appearance5.Image"), System.Object).
        buttonTool1:SharedPropsInternal:AppearancesLarge:Appearance = appearance5.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image"), System.Object).
        buttonTool1:SharedPropsInternal:AppearancesSmall:Appearance = appearance6.
        buttonTool1:SharedPropsInternal:Caption = "First record".
        buttonTool1:SharedPropsInternal:CustomizerCaption = "First".
        buttonTool1:SharedPropsInternal:Enabled = FALSE.
        buttonTool1:SharedPropsInternal:ToolTipText = "First record".
        appearance7:Image = CAST(resources:GetObject("appearance7.Image"), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesLarge:Appearance = appearance7.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image"), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesSmall:Appearance = appearance8.
        buttonTool6:SharedPropsInternal:Caption = "Previous record".
        buttonTool6:SharedPropsInternal:CustomizerCaption = "Previous".
        buttonTool6:SharedPropsInternal:Enabled = FALSE.
        buttonTool6:SharedPropsInternal:ToolTipText = "Previous record".
        appearance9:Image = CAST(resources:GetObject("appearance9.Image"), System.Object).
        buttonTool7:SharedPropsInternal:AppearancesLarge:Appearance = appearance9.
        appearance10:Image = CAST(resources:GetObject("appearance10.Image"), System.Object).
        buttonTool7:SharedPropsInternal:AppearancesSmall:Appearance = appearance10.
        buttonTool7:SharedPropsInternal:Caption = "Next record".
        buttonTool7:SharedPropsInternal:CustomizerCaption = "Next".
        buttonTool7:SharedPropsInternal:Enabled = FALSE.
        buttonTool7:SharedPropsInternal:ToolTipText = "Next record".
        appearance11:Image = CAST(resources:GetObject("appearance11.Image"), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesLarge:Appearance = appearance11.
        appearance12:Image = CAST(resources:GetObject("appearance12.Image"), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesSmall:Appearance = appearance12.
        buttonTool8:SharedPropsInternal:Caption = "Last record".
        buttonTool8:SharedPropsInternal:CustomizerCaption = "Last".
        buttonTool8:SharedPropsInternal:Enabled = FALSE.
        buttonTool8:SharedPropsInternal:ToolTipText = "Last record".
        appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
        buttonTool9:SharedPropsInternal:AppearancesLarge:Appearance = appearance13.
        appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
        buttonTool9:SharedPropsInternal:AppearancesSmall:Appearance = appearance14.
        buttonTool9:SharedPropsInternal:Caption = "Add record".
        buttonTool9:SharedPropsInternal:CustomizerCaption = "Add".
        buttonTool9:SharedPropsInternal:Enabled = FALSE.
        buttonTool9:SharedPropsInternal:ToolTipText = "Add record".
        appearance15:Image = CAST(resources:GetObject("appearance15.Image"), System.Object).
        buttonTool16:SharedPropsInternal:AppearancesLarge:Appearance = appearance15.
        appearance16:Image = CAST(resources:GetObject("appearance16.Image"), System.Object).
        buttonTool16:SharedPropsInternal:AppearancesSmall:Appearance = appearance16.
        buttonTool16:SharedPropsInternal:Caption = "Copy record".
        buttonTool16:SharedPropsInternal:CustomizerCaption = "Copy".
        buttonTool16:SharedPropsInternal:Enabled = FALSE.
        buttonTool16:SharedPropsInternal:ToolTipText = "Copy record".
        appearance17:Image = CAST(resources:GetObject("appearance17.Image"), System.Object).
        buttonTool17:SharedPropsInternal:AppearancesLarge:Appearance = appearance17.
        appearance18:Image = CAST(resources:GetObject("appearance18.Image"), System.Object).
        buttonTool17:SharedPropsInternal:AppearancesSmall:Appearance = appearance18.
        buttonTool17:SharedPropsInternal:Caption = "Update record".
        buttonTool17:SharedPropsInternal:CustomizerCaption = "Update".
        buttonTool17:SharedPropsInternal:Enabled = FALSE.
        buttonTool17:SharedPropsInternal:ToolTipText = "Update record".
        appearance19:Image = CAST(resources:GetObject("appearance19.Image"), System.Object).
        buttonTool18:SharedPropsInternal:AppearancesLarge:Appearance = appearance19.
        appearance20:Image = CAST(resources:GetObject("appearance20.Image"), System.Object).
        buttonTool18:SharedPropsInternal:AppearancesSmall:Appearance = appearance20.
        buttonTool18:SharedPropsInternal:Caption = "Save changes".
        buttonTool18:SharedPropsInternal:CustomizerCaption = "Save".
        buttonTool18:SharedPropsInternal:Enabled = FALSE.
        buttonTool18:SharedPropsInternal:ToolTipText = "Save changes".
        appearance21:Image = CAST(resources:GetObject("appearance21.Image"), System.Object).
        buttonTool19:SharedPropsInternal:AppearancesLarge:Appearance = appearance21.
        appearance22:Image = CAST(resources:GetObject("appearance22.Image"), System.Object).
        buttonTool19:SharedPropsInternal:AppearancesSmall:Appearance = appearance22.
        buttonTool19:SharedPropsInternal:Caption = "Cancel update".
        buttonTool19:SharedPropsInternal:CustomizerCaption = "Cancel".
        buttonTool19:SharedPropsInternal:Enabled = FALSE.
        buttonTool19:SharedPropsInternal:ToolTipText = "Cancel update".
        appearance23:Image = CAST(resources:GetObject("appearance23.Image"), System.Object).
        buttonTool20:SharedPropsInternal:AppearancesLarge:Appearance = appearance23.
        appearance24:Image = CAST(resources:GetObject("appearance24.Image"), System.Object).
        buttonTool20:SharedPropsInternal:AppearancesSmall:Appearance = appearance24.
        buttonTool20:SharedPropsInternal:Caption = "Delete record".
        buttonTool20:SharedPropsInternal:CustomizerCaption = "Delete".
        buttonTool20:SharedPropsInternal:Enabled = FALSE.
        buttonTool20:SharedPropsInternal:ToolTipText = "Delete record".
        popupMenuTool6:SharedPropsInternal:Caption = "&File".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar20 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar20[1] = buttonTool21.
        popupMenuTool6:Tools:AddRange(arrayvar20).
        popupMenuTool7:SharedPropsInternal:Caption = "&Edit".
        popupMenuTool8:SharedPropsInternal:Caption = "&Options".
        appearance25:Image = CAST(resources:GetObject("appearance25.Image"), System.Object).
        popupMenuTool9:SharedPropsInternal:AppearancesLarge:Appearance = appearance25.
        popupMenuTool9:SharedPropsInternal:Caption = "&Window".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar21 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar21[1] = mdiWindowListTool1.
        popupMenuTool9:Tools:AddRange(arrayvar21).
        popupMenuTool10:SharedPropsInternal:Caption = "&Help".
        buttonTool22:SharedPropsInternal:Caption = "&Exit".
        buttonTool22:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:TextOnlyAlways.
        mdiWindowListTool2:SharedPropsInternal:Caption = "MDIWindowListTool1".
        appearance26:Image = CAST(resources:GetObject("appearance26.Image"), System.Object).
        buttonTool34:SharedPropsInternal:AppearancesLarge:Appearance = appearance26.
        buttonTool34:SharedPropsInternal:Caption = "Clear".
        appearance27:Image = CAST(resources:GetObject("appearance27.Image"), System.Object).
        buttonTool36:SharedPropsInternal:AppearancesLarge:Appearance = appearance27.
        buttonTool36:SharedPropsInternal:Caption = "Search".
        appearance28:Image = CAST(resources:GetObject("appearance28.Image"), System.Object).
        popupMenuTool13:SharedPropsInternal:AppearancesLarge:Appearance = appearance28.
        popupMenuTool13:SharedPropsInternal:Caption = "Previous searches".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar22 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar22[1] = labelTool1.
        popupMenuTool13:Tools:AddRange(arrayvar22).
        labelTool2:SharedPropsInternal:Caption = "(empty)".
        appearance29:Image = CAST(resources:GetObject("appearance29.Image"), System.Object).
        buttonTool51:SharedPropsInternal:AppearancesLarge:Appearance = appearance29.
        appearance30:Image = CAST(resources:GetObject("appearance30.Image"), System.Object).
        buttonTool51:SharedPropsInternal:AppearancesSmall:Appearance = appearance30.
        buttonTool51:SharedPropsInternal:Caption = "Start new session".
        controlContainerTool2:ControlName = "ultraPictureBox1".
        appearance31:BorderAlpha = Infragistics.Win.Alpha:Transparent.
        controlContainerTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance31.
        controlContainerTool2:SharedPropsInternal:Width = 133.
        appearance32:Image = CAST(resources:GetObject("appearance32.Image"), System.Object).
        buttonTool58:SharedPropsInternal:AppearancesLarge:Appearance = appearance32.
        appearance33:Image = CAST(resources:GetObject("appearance33.Image"), System.Object).
        buttonTool58:SharedPropsInternal:AppearancesSmall:Appearance = appearance33.
        buttonTool58:SharedPropsInternal:Caption = "Launch Form".
        appearance34:Image = CAST(resources:GetObject("appearance34.Image"), System.Object).
        stateButtonTool27:SharedPropsInternal:AppearancesLarge:Appearance = appearance34.
        stateButtonTool27:SharedPropsInternal:Caption = "Application Menu".
        appearance35:Image = CAST(resources:GetObject("appearance35.Image"), System.Object).
        buttonTool35:SharedPropsInternal:AppearancesLarge:Appearance = appearance35.
        appearance36:Image = CAST(resources:GetObject("appearance36.Image"), System.Object).
        buttonTool35:SharedPropsInternal:AppearancesSmall:Appearance = appearance36.
        buttonTool35:SharedPropsInternal:Caption = "Reload Menu".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar23 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 26 NO-UNDO.
        arrayvar23[1] = buttonTool1.
        arrayvar23[2] = buttonTool6.
        arrayvar23[3] = buttonTool7.
        arrayvar23[4] = buttonTool8.
        arrayvar23[5] = buttonTool9.
        arrayvar23[6] = buttonTool16.
        arrayvar23[7] = buttonTool17.
        arrayvar23[8] = buttonTool18.
        arrayvar23[9] = buttonTool19.
        arrayvar23[10] = buttonTool20.
        arrayvar23[11] = popupMenuTool6.
        arrayvar23[12] = popupMenuTool7.
        arrayvar23[13] = popupMenuTool8.
        arrayvar23[14] = popupMenuTool9.
        arrayvar23[15] = popupMenuTool10.
        arrayvar23[16] = buttonTool22.
        arrayvar23[17] = mdiWindowListTool2.
        arrayvar23[18] = buttonTool34.
        arrayvar23[19] = buttonTool36.
        arrayvar23[20] = popupMenuTool13.
        arrayvar23[21] = labelTool2.
        arrayvar23[22] = buttonTool51.
        arrayvar23[23] = controlContainerTool2.
        arrayvar23[24] = buttonTool58.
        arrayvar23[25] = stateButtonTool27.
        arrayvar23[26] = buttonTool35.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar23).
        THIS-OBJECT:smartToolbarController1:ToolClick:Subscribe(THIS-OBJECT:smartToolbarController1_ToolClick).
        /*  */
        /* m_CustomerExplorer_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 151).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left:Name = "m_CustomerExplorer_Toolbars_Dock_Area_Left".
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 438).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_CustomerExplorer_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(876, 151).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right:Name = "m_CustomerExplorer_Toolbars_Dock_Area_Right".
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 438).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_CustomerExplorer_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top:Name = "m_CustomerExplorer_Toolbars_Dock_Area_Top".
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(884, 151).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_CustomerExplorer_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 589).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom:Name = "m_CustomerExplorer_Toolbars_Dock_Area_Bottom".
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(884, 0).
        THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* windowDockingArea4 */
        /*  */
        THIS-OBJECT:windowDockingArea4:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:windowDockingArea4:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea4:Location = NEW System.Drawing.Point(8, 8).
        THIS-OBJECT:windowDockingArea4:Name = "windowDockingArea4".
        THIS-OBJECT:windowDockingArea4:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea4:Size = NEW System.Drawing.Size(95, 446).
        THIS-OBJECT:windowDockingArea4:TabIndex = 0.
        /*  */
        /* ultraStatusBar1 */
        /*  */
        THIS-OBJECT:ultraStatusBar1:Location = NEW System.Drawing.Point(0, 589).
        THIS-OBJECT:ultraStatusBar1:Name = "ultraStatusBar1".
        ultraStatusPanel1:Key = "status".
        ultraStatusPanel1:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Spring.
        ultraStatusPanel1:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel2:Key = "logincompanyname".
        ultraStatusPanel2:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel2:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel3:Key = "username".
        ultraStatusPanel3:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel3:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel4:Key = "system".
        ultraStatusPanel4:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel5:Key = "proversion".
        ultraStatusPanel5:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel6:Key = "language".
        ultraStatusPanel6:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel6:Style = Infragistics.Win.UltraWinStatusBar.PanelStyle:Button.
        ultraStatusPanel6:Text = "Language".
        ultraStatusPanel7:Key = "date".
        ultraStatusPanel7:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel7:Style = Infragistics.Win.UltraWinStatusBar.PanelStyle:Date.
        ultraStatusPanel7:WrapText = Infragistics.Win.DefaultableBoolean:False.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar24 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel EXTENT 7 NO-UNDO.
        arrayvar24[1] = ultraStatusPanel1.
        arrayvar24[2] = ultraStatusPanel2.
        arrayvar24[3] = ultraStatusPanel3.
        arrayvar24[4] = ultraStatusPanel4.
        arrayvar24[5] = ultraStatusPanel5.
        arrayvar24[6] = ultraStatusPanel6.
        arrayvar24[7] = ultraStatusPanel7.
        THIS-OBJECT:ultraStatusBar1:Panels:AddRange(arrayvar24).
        THIS-OBJECT:ultraStatusBar1:Size = NEW System.Drawing.Size(884, 23).
        THIS-OBJECT:ultraStatusBar1:SizeGripVisible = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraStatusBar1:TabIndex = 20.
        THIS-OBJECT:ultraStatusBar1:Text = "ultraStatusBar1".
        THIS-OBJECT:ultraStatusBar1:ViewStyle = Infragistics.Win.UltraWinStatusBar.ViewStyle:Standard.
        THIS-OBJECT:ultraStatusBar1:ButtonClick:Subscribe(THIS-OBJECT:ultraStatusBar1_ButtonClick).
        /*  */
        /* windowDockingArea5 */
        /*  */
        THIS-OBJECT:windowDockingArea5:Dock = System.Windows.Forms.DockStyle:Right.
        THIS-OBJECT:windowDockingArea5:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea5:Location = NEW System.Drawing.Point(671, 164).
        THIS-OBJECT:windowDockingArea5:Name = "windowDockingArea5".
        THIS-OBJECT:windowDockingArea5:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea5:Size = NEW System.Drawing.Size(184, 404).
        THIS-OBJECT:windowDockingArea5:TabIndex = 30.
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
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance4:BorderAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraPictureBox1:Appearance = appearance4.
        THIS-OBJECT:ultraPictureBox1:AutoSize = TRUE.
        THIS-OBJECT:ultraPictureBox1:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraPictureBox1:BorderShadowColor = System.Drawing.Color:Empty.
        THIS-OBJECT:ultraPictureBox1:BorderStyle = Infragistics.Win.UIElementBorderStyle:None.
        THIS-OBJECT:ultraPictureBox1:Image = CAST(resources:GetObject("ultraPictureBox1.Image"), System.Object).
        THIS-OBJECT:ultraPictureBox1:Location = NEW System.Drawing.Point(1051, 63).
        THIS-OBJECT:ultraPictureBox1:Name = "ultraPictureBox1".
        THIS-OBJECT:ultraPictureBox1:Size = NEW System.Drawing.Size(133, 32).
        THIS-OBJECT:ultraPictureBox1:TabIndex = 36.
        THIS-OBJECT:ultraPictureBox1:Click:Subscribe(THIS-OBJECT:ultraPictureBox1_Click).
        /*  */
        /* MainMenuForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(884, 612).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorerAutoHideControl).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPictureBox1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea5).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaBottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaTop).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaLeft).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorerUnpinnedTabAreaRight).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraStatusBar1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_CustomerExplorer_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon"), System.Drawing.Icon).
        THIS-OBJECT:IsMdiContainer = TRUE.
        THIS-OBJECT:Name = "MainMenuForm".
        THIS-OBJECT:Text = "Advantzware Manufacturing & Distribution Software v{&awversion}".
        THIS-OBJECT:WindowPositionRegistryKey = "CustomerExplorer".
        CAST(THIS-OBJECT:ultraExplorerBar1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraGridColumnChooser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabbedMdiManager1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:m_CustomerExplorerAutoHideControl:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow3:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow1:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow6:ResumeLayout(FALSE).
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
        Purpose: Initializes an ExplorerBarGroup with the SmartFramework MenuTreeViewControl
        Notes:
        @param pcGroupKey The key of the Group to initialize
        @param pcGroupTitle The title of the group to initialize
        @param pcRootNodeKey The key of the RootNode for the Menu
        @param pcSmallImageKey The key of the small Group Image
        @param pcLargeImageKey The key of the large Group Image
        @param piPosition The position of the Explorer Bar Group or ?
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeExplorerBarGroup (pcGroupKey AS CHARACTER,
                                                      pcGroupTitle AS CHARACTER,
                                                      pcRootNodeKey AS CHARACTER,
                                                      pcSmallImageKey AS CHARACTER,
                                                      pcLargeImageKey AS CHARACTER,
                                                      piPosition AS INTEGER):

        THIS-OBJECT:InitializeExplorerBarGroup (pcGroupKey,
                                                pcGroupTitle,
                                                pcRootNodeKey,
                                                pcSmallImageKey,
                                                pcLargeImageKey,
                                                piPosition,
                                                TRUE) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes an ExplorerBarGroup with the SmartFramework MenuTreeViewControl
        Notes:
        @param pcGroupKey The key of the Group to initialize
        @param pcGroupTitle The title of the group to initialize
        @param pcRootNodeKey The key of the RootNode for the Menu
        @param pcSmallImageKey The key of the small Group Image
        @param pcLargeImageKey The key of the large Group Image
        @param piPosition The position of the Explorer Bar Group or ?
        @param plRenderRootNode Logical value indicating if the RootNode should be rendered
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeExplorerBarGroup (pcGroupKey AS CHARACTER,
                                                      pcGroupTitle AS CHARACTER,
                                                      pcRootNodeKey AS CHARACTER,
                                                      pcSmallImageKey AS CHARACTER,
                                                      pcLargeImageKey AS CHARACTER,
                                                      piPosition AS INTEGER,
                                                      plRenderRootNode AS LOGICAL):

        DEFINE VARIABLE oMenuTreeViewControl AS MenuTreeViewControl   NO-UNDO.
        DEFINE VARIABLE oGroup               AS UltraExplorerBarGroup NO-UNDO .
        DEFINE VARIABLE oImageProvider       AS IImageProvider        NO-UNDO .

        oMenuTreeViewControl = NEW MenuTreeViewControl ().

        oMenuTreeViewControl:BackColor = System.Drawing.Color:White.
        oMenuTreeViewControl:RootNodeKey = pcRootNodeKey .
        oMenuTreeViewControl:RenderRootNode = plRenderRootNode .
        oMenuTreeViewControl:ExpandFirstLevelAfterStart = FALSE .

        /* Mike Fechner, Consultingwerk Ltd. 15.01.2014
           Activate this line, whne menu should be sorted by alphabet and not
           by menu structure provider */
/*        oMenuTreeViewControl:Sorted = TRUE .*/

        IF piPosition = ? THEN
            oGroup = ultraExplorerBar1:Groups:Add (pcGroupKey, pcGroupTitle) .
        ELSE
            oGroup = ultraExplorerBar1:Groups:Insert (piPosition, pcGroupKey, pcGroupTitle) .

        oGroup:Settings:Style = GroupStyle:ControlContainer .
        oGroup:Container:Controls:Add (oMenuTreeViewControl) .
        oMenuTreeViewControl:Dock = DockStyle:Fill.

        oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider} .

        IF VALID-OBJECT (oImageProvider) THEN DO:
            oGroup:Settings:AppearancesSmall:HeaderAppearance:Image = oImageProvider:ImageForKey
                        (pcSmallImageKey) .
            oGroup:Settings:AppearancesLarge:HeaderAppearance:Image = oImageProvider:ImageForKey
                        (pcLargeImageKey) .
        END.

    END METHOD .

&IF DEFINED (SmartFramework) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Initializes an ExplorerBarGroup with the native Explorer Bar Items
        Notes:
        @param pcGroupKey The key of the Group to initialize
        @param pcGroupTitle The title of the group to initialize
        @param pcRootNodeKey The key of the RootNode for the Menu
        @param pcSmallImageKey The key of the small Group Image
        @param pcLargeImageKey The key of the large Group Image
        @param piPosition The position of the Explorer Bar Group or ?
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeNativeExplorerBarGroup (pcGroupKey AS CHARACTER,
                                                            pcGroupTitle AS CHARACTER,
                                                            pcRootNodeKey AS CHARACTER,
                                                            pcSmallImageKey AS CHARACTER,
                                                            pcLargeImageKey AS CHARACTER,
                                                            piPosition AS INTEGER):

        DEFINE VARIABLE oController AS MenuStructureExplorerBarGroupController NO-UNDO .

        oController = NEW MenuStructureExplorerBarGroupController (ultraExplorerBar1,
                                                                   THIS-OBJECT,
                                                                   pcGroupKey,
                                                                   pcGroupTitle,
                                                                   pcRootNodeKey,
                                                                   pcSmallImageKey,
                                                                   pcLargeImageKey,
                                                                   piPosition) .
    END .

    /*------------------------------------------------------------------------------
        Purpose: Initializes an ExplorerBarGroup with recently used menu items
        Notes:
        @param pcGroupKey The key of the Group to initialize
        @param pcGroupTitle The title of the group to initialize
        @param pcSmallImageKey The key of the small Group Image
        @param pcLargeImageKey The key of the large Group Image
        @param piPosition The position of the Explorer Bar Group or ?
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeRecentlyUsedMenuItemsGroup (pcGroupKey AS CHARACTER,
                                                                pcGroupTitle AS CHARACTER,
                                                                pcSmallImageKey AS CHARACTER,
                                                                pcLargeImageKey AS CHARACTER,
                                                                piPosition AS INTEGER):

        DEFINE VARIABLE oController AS LastRecentlyUsedExplorerBarGroupController NO-UNDO .

        oController = NEW LastRecentlyUsedExplorerBarGroupController (ultraExplorerBar1,
                                                                      THIS-OBJECT,
                                                                      pcGroupKey,
                                                                      pcGroupTitle,
                                                                      pcSmallImageKey,
                                                                      pcLargeImageKey,
                                                                      piPosition) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes a MenuStructureRibbonTabController instance
        Notes:
        @param pcMenuStructureGuid The Guid of the Menu Structure to render
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeRibbonTab (pcMenuStructureGuid AS CHARACTER):

        DEFINE VARIABLE oController AS MenuStructureRibbonTabController NO-UNDO .

        oController = NEW MenuStructureRibbonTabController (smartToolbarController1,
                                                            THIS-OBJECT,
                                                            pcMenuStructureGuid) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes an ExplorerBarGroup with the SmartFramework MenuTaskListControl
        Notes:
        @param pcGroupKey The key of the Group to initialize
        @param pcGroupTitle The title of the group to initialize
        @param pcRootNodeKey The key of the RootNode for the Menu
        @param pcSmallImageKey The key of the small Group Image
        @param pcLargeImageKey The key of the large Group Image
        @param piPosition The position of the Explorer Bar Group or ?
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeTaskList (pcGroupKey AS CHARACTER,
                                              pcGroupTitle AS CHARACTER,
                                              pcRootNodeKey AS CHARACTER,
                                              pcSmallImageKey AS CHARACTER,
                                              pcLargeImageKey AS CHARACTER,
                                              piPosition AS INTEGER):

        DEFINE VARIABLE oMenuTaskListControl AS MenuTaskListControl   NO-UNDO.
        DEFINE VARIABLE oGroup               AS UltraExplorerBarGroup NO-UNDO .
        DEFINE VARIABLE oImageProvider       AS IImageProvider        NO-UNDO .

        oMenuTaskListControl = NEW MenuTaskListControl ().

        oMenuTaskListControl:BackColor = System.Drawing.Color:White.
        oMenuTaskListControl:RootNodeKey = pcRootNodeKey .

        IF piPosition = ? THEN
            oGroup = ultraExplorerBar1:Groups:Add (pcGroupKey, pcGroupTitle) .
        ELSE
            oGroup = ultraExplorerBar1:Groups:Insert (piPosition, pcGroupKey, pcGroupTitle) .

        oGroup:Settings:Style = GroupStyle:ControlContainer .
        oGroup:Container:Controls:Add (oMenuTaskListControl) .
        oMenuTaskListControl:Dock = DockStyle:Fill.

        oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider} .

        IF VALID-OBJECT (oImageProvider) THEN DO:
            oGroup:Settings:AppearancesSmall:HeaderAppearance:Image = oImageProvider:ImageForKey
                        (pcSmallImageKey) .
            oGroup:Settings:AppearancesLarge:HeaderAppearance:Image = oImageProvider:ImageForKey
                        (pcLargeImageKey) .
        END.

    END METHOD .
&ENDIF

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

        THIS-OBJECT:ultraStatusBar1:Panels["logincompanyname":U]:Text = "Company: " + SessionManager:LoginCompanyName .

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

        DEFINE VARIABLE hProcedure AS HANDLE NO-UNDO.

        oMessageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IMessageProvider} .

        IF NOT VALID-OBJECT (oCloseTimer) AND SessionManager:UserName > "":U AND VALID-OBJECT (oMessageProvider) THEN DO ON ERROR UNDO, THROW:
            oMessage = oMessageProvider:GetMessage ("SFR":U, 1, FrameworkSettings:ApplicationLabel) .

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

        IF NOT e:Cancel THEN DO:

            DO WHILE THIS-OBJECT:MdiChildren:Length > 0:

                oChildForm = CAST (THIS-OBJECT:MdiChildren:GetValue(0),
                              System.Windows.Forms.Form) .

                hProcedure = (CAST (oChildForm, IEmbeddedWindowForm):ProcedureHandle) .

                IF TYPE-OF (oChildForm, IEmbeddedWindowForm) THEN
                    APPLY "CLOSE" TO hProcedure .
                ELSE
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

        DEFINE VARIABLE oSystemDescription         AS ISystemDescription                 NO-UNDO .
        DEFINE VARIABLE oSystemDescriptionProvider AS ISystemDescriptionProvider         NO-UNDO .
        DEFINE VARIABLE oDockableControlPane       AS DockableControlPane                NO-UNDO .

        THIS-OBJECT:DisplayLanguageName () .

        SUPER:OnLoad (e) .

        ultraStatusBar1:Panels["proversion":U]:Text = "ProVersion: " + SessionHelper:FullVersionString() .
        ultraStatusBar1:Panels["logincompanyname":U]:Text = "Company: " + "Name Here".

        oSystemDescriptionProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.ISystemDescriptionProvider
                                                                   "NEW Consultingwerk.Framework.SystemDescriptionProvider ()"} .

         oSystemDescription = oSystemDescriptionProvider:GetSystemDescription() .

        IF VALID-OBJECT (oSystemDescription) THEN
             ultraStatusBar1:Panels["system":U]:Text = "System: " + oSystemDescription:Description .

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

        {Consultingwerk/foreach.i DockableControlPane oControlPane in ultraDockManager1:ControlPanes}
            oControlPane:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:FALSE .
        END.

        {Consultingwerk/foreach.i DockAreaPane oAreaPane in ultraDockManager1:DockAreas}
            oAreaPane:Settings:AllowClose = Infragistics.Win.DefaultableBoolean:FALSE .
        END.

        CATCH err AS Progress.Lang.Error :
            IF VALID-OBJECT (oSplash) THEN DO ON ERROR UNDO, THROW:

                oSplash:Close () .

                @SuppressUnusedWarnings.
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
        DEFINE VARIABLE oLightBox            AS LightboxForm         NO-UNDO .
        DEFINE VARIABLE oTranslationProvider AS ITranslationProvider NO-UNDO .
        DEFINE VARIABLE oSettingsService     AS ISettingsService     NO-UNDO .
        DEFINE VARIABLE cGroupKey            AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE i                    AS INTEGER              NO-UNDO .

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

            oLightBox = NEW LightboxForm (THIS-OBJECT) .
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

&IF DEFINED (SmartFramework) NE 0 &THEN
        ultraExplorerBar1:SuspendLayout() .

        Consultingwerk.Windows.API.Win32:LockWindowUpdate (THIS-OBJECT) .

        THIS-OBJECT:InitializeRecentlyUsedMenuItemsGroup ("Recent":U,
                                                          "Recent"{&TRAN},
                                                          "Consultingwerk/Windows/Framework/Images/step_16.png":U,
                                                          "Consultingwerk/Windows/Framework/Images/step_32.png":U,
                                                          0) .

        ASSIGN i = 1 .

        DEFINE VARIABLE oGroups AS CharacterHolder NO-UNDO .

        IF SessionManager:UserGroupKeys > "":U THEN DO:
            oGroups = NEW CharacterHolder (SessionManager:UserGroupKeys) .

            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           "Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U,
                                                           "GetMenusForGroups":U,
                                                           INPUT-OUTPUT DATASET dsMenu,
                                                           oGroups) .

            FOR EACH eSmartMenu WHERE eSmartMenu.MenuStyleCode = "":U
                                   OR eSmartMenu.MenuStyleCode = "TREE":U
                                   OR eSmartMenu.MenuStyleCode = "EXPLORERBAR":U
                                   OR eSmartMenu.MenuStyleCode = "TASKLIST":U
                                BY eSmartMenu.MenuSequence:

                IF eSmartMenu.MenuStyleCode = "":U OR
                   eSmartMenu.MenuStyleCode = "TREE":U THEN

                THIS-OBJECT:InitializeExplorerBarGroup (eSmartMenu.MenuGuid,
                                                        eSmartMenu.MenuName,
                                                        eSmartMenu.MenuGuid,
                                                        eSmartMenu.MenuSmallImage,
                                                        eSmartMenu.MenuLargeImage,
                                                        i,
                                                        FALSE) .

                ELSE IF eSmartMenu.MenuStyleCode = "EXPLORERBAR":U THEN
                    THIS-OBJECT:InitializeNativeExplorerBarGroup (eSmartMenu.MenuGuid,
                                                                  eSmartMenu.MenuName,
                                                                  eSmartMenu.MenuGuid,
                                                                  eSmartMenu.MenuSmallImage,
                                                                  eSmartMenu.MenuLargeImage,
                                                                  i) .

                ELSE THIS-OBJECT:InitializeTaskList (eSmartMenu.MenuGuid,
                                                     eSmartMenu.MenuName,
                                                     eSmartMenu.MenuGuid,
                                                     eSmartMenu.MenuSmallImage,
                                                     eSmartMenu.MenuLargeImage,
                                                     i) .

                i = i + 1 .
            END.

            FOR EACH eSmartMenu WHERE eSmartMenu.MenuStyleCode = "RIBBON":U
                    BY eSmartMenu.MenuSequence:

                THIS-OBJECT:InitializeRibbonTab (eSmartMenu.MenuGuid) .
            END.

        END.

        Consultingwerk.Windows.API.Win32:LockWindowUpdate (0) .

        ultraExplorerBar1:ResumeLayout(TRUE) .
&ENDIF

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

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Reloads the Menu Structure
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ReloadMenu ():

        {Consultingwerk/foreach.i UltraExplorerBarGroup oGroup in ultraExplorerBar1:Groups}

            IF VALID-OBJECT (oGroup:Container) AND oGroup:Container:Controls:Count > 0 THEN DO:

                IF TYPE-OF (oGroup:Container:Controls[0],
                            MenuTreeViewControl) THEN

                    CAST (oGroup:Container:Controls[0],
                            MenuTreeViewControl):LoadMenu () .
            END.
        END.

    END METHOD .

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
        ELSE IF NOT VALID-OBJECT (sender) THEN 
            ultraGridColumnChooser1:SourceGrid = ? .  

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the smartToolbarController1
        Notes:
        @param sender The reference to the object thait raised the event
        @param e The ToolClickEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartToolbarController1_ToolClick (sender AS System.Object,
                                                           e AS ToolClickEventArgs):

        CASE e:Tool:Key:

            WHEN "StartNewSession":U THEN
                THIS-OBJECT:StartNewSession () .

            WHEN "LaunchForm":U THEN
                Consultingwerk.Windows.FormLauncher:LaunchForm () .

            WHEN "ApplicationMenu":U THEN
                ToggleApplicationMenu () .

            WHEN "Reload Menu":U THEN
                THIS-OBJECT:ReloadMenu () .

        END CASE.

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

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

        /* Initialize WinKit colors */
        Consultingwerk.Util.StyleLibraryHelper:SetProgressColorTable
            (31, "UltraPanel":U, RoleState:Normal, "WinKit":U) .

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
                                                       "http://www.advantzware.com":U) .

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

        THIS-OBJECT:ultraStatusBar1:Panels["username":U]:Text = "User Login: " + SessionManager:UserName .

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
