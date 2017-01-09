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
    File        : SampleMDIContainer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu May 07 09:52:52 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.MDIContainer                     FROM PROPATH .
USING Consultingwerk.WindowIntegrationKit.Forms.*           FROM PROPATH .
USING Advantzware.WinKit.Samples.Options.*                  FROM PROPATH .
USING DevComponents.WinUI.*                                 FROM ASSEMBLY .
USING Infragistics.Win.*                                    FROM ASSEMBLY .
USING Infragistics.Win.AppStyling.*                         FROM ASSEMBLY .
USING Infragistics.Win.AppStyling.Runtime.*                 FROM ASSEMBLY .
USING Infragistics.Win.UltraWinToolbars.*                   FROM ASSEMBLY .
USING Progress.Lang.*                                       FROM PROPATH .

CLASS Advantzware.WinKit.Samples.SampleMDIContainer INHERITS MDIContainer   : 
	
    DEFINE PRIVATE VARIABLE dockableWindow4 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE dockablePaneSizeTest1 AS Advantzware.WinKit.Samples.DockablePaneSizeTest NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SampleMDIContainer_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SampleMDIContainer_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SampleMDIContainer_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SampleMDIContainer_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

	DEFINE PRIVATE VARIABLE ultraTabbedMdiManager1 AS Infragistics.Win.UltraWinTabbedMdi.UltraTabbedMdiManager NO-UNDO.

	DEFINE PRIVATE VARIABLE m_SampleMDIContainerAutoHideControl AS Infragistics.Win.UltraWinDock.AutoHideControl NO-UNDO.
	DEFINE PRIVATE VARIABLE ultraDockManager1 AS Infragistics.Win.UltraWinDock.UltraDockManager NO-UNDO.
	DEFINE PRIVATE VARIABLE m_SampleMDIContainerUnpinnedTabAreaTop AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
	DEFINE PRIVATE VARIABLE m_SampleMDIContainerUnpinnedTabAreaBottom AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
	DEFINE PRIVATE VARIABLE m_SampleMDIContainerUnpinnedTabAreaLeft AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
	DEFINE PRIVATE VARIABLE m_SampleMDIContainerUnpinnedTabAreaRight AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.

	DEFINE PRIVATE VARIABLE ultraExplorerBar1 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBar NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
	DEFINE PRIVATE VARIABLE windowDockingArea1 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
	DEFINE PRIVATE VARIABLE dockableWindow1 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.

	DEFINE PRIVATE VARIABLE ultraMonthViewMulti1 AS Infragistics.Win.UltraWinSchedule.UltraMonthViewMulti NO-UNDO.
	DEFINE PRIVATE VARIABLE ultraCalendarInfo1 AS Infragistics.Win.UltraWinSchedule.UltraCalendarInfo NO-UNDO.
	DEFINE PRIVATE VARIABLE ultraCalendarLook1 AS Infragistics.Win.UltraWinSchedule.UltraCalendarLook NO-UNDO.
	DEFINE PRIVATE VARIABLE windowDockingArea2 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
	DEFINE PRIVATE VARIABLE dockableWindow2 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.

	DEFINE PRIVATE VARIABLE taskPanelControl1 AS Advantzware.WinKit.Samples.TaskPanel.TaskPanelControl NO-UNDO.
	DEFINE PRIVATE VARIABLE dockableWindow3 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.

    DEFINE VARIABLE hWindow AS HANDLE NO-UNDO.

    DEFINE PRIVATE VARIABLE windowContainer1 AS Progress.Windows.WindowContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE windowDockingArea3 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea4 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea5 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.

    DEFINE VARIABLE oSplash AS Advantzware.WinKit.Samples.Splash NO-UNDO . 
    DEFINE VARIABLE oTimer  AS System.Windows.Forms.Timer NO-UNDO . 
		
    /*------------------------------------------------------------------------------
        Purpose: Returns the current application style                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
	DEFINE PUBLIC PROPERTY ApplicationStyle AS CHARACTER NO-UNDO 
	GET.
	PRIVATE SET. 

	CONSTRUCTOR PUBLIC SampleMDIContainer ():		
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO . 

        SUPER().
       
        Consultingwerk.Framework.FrameworkSettings:StoreWindowPosition = TRUE . 
        Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive = TRUE . 
        Consultingwerk.WindowIntegrationKit.Forms.WinKitForms:TabFolderBGColor = 17 . 

        oSplash = NEW Advantzware.WinKit.Samples.Splash () . 
        oSplash:TopMost = TRUE . 
        oSplash:Show () .
        
        PROCESS EVENTS . 
       
        LoadApplicationStyle ("Office2010Blue":U) .
        
        Consultingwerk.Framework.FrameworkSettings:MdiContainer = THIS-OBJECT . 
                
        InitializeComponent().
        CreateDefaultABLWindow () .        

        oForm = NEW Advantzware.WinKit.Samples.StartPage.StartPageForm () .
        oForm:MdiParent = THIS-OBJECT . 
        oForm:VISIBLE = TRUE . 
                   
        /* Initialize WinKit Designer */
        Consultingwerk.Design.WinKit.DesignManager:Initialize() .                   
                    
                      
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

  	/*------------------------------------------------------------------------------
    	Purpose: Create a dummy windows as session:default ABL window  																	  
		Notes:   If there is no ABL window embedded in the .NET Main Form, ABL
		         Dialog Frames (like a login dialog) will hide behind the .NET Form 
		         when the user clicks on the .NET Form's taskbar icon. The only way 
		         of getting back to the Dialog Form is ALT-TAB rotation.		         																	  
	------------------------------------------------------------------------------*/	
	METHOD PROTECTED VOID CreateDefaultABLWindow(  ):
		        
        CREATE WINDOW hWindow . 
        windowContainer1:EmbeddedWindow = hWindow.
        ASSIGN hWindow:BGCOLOR = 12 .
        hWindow:VISIBLE = TRUE .
        CURRENT-WINDOW = hWindow .

        /* Move out of the way! */
        windowContainer1:SIZE = NEW System.Drawing.Size (0, 0) .
        windowContainer1:Location = NEW System.Drawing.Point (0, 0) .

	END METHOD.
		
	METHOD PRIVATE VOID InitializeComponent(  ):
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarGroup1 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup NO-UNDO.
        ultraExplorerBarGroup1 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem1 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem1 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Advantzware.WinKit.Samples.SampleMDIContainer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem2 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem2 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem3 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem3 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem4 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem4 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem9 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem9 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem5 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem5 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem6 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem6 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem7 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem7 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem23 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem23 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance36 AS Infragistics.Win.Appearance NO-UNDO.
        appearance36 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarGroup2 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup NO-UNDO.
        ultraExplorerBarGroup2 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem10 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem10 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem11 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem11 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem12 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem12 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem21 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem21 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance35 AS Infragistics.Win.Appearance NO-UNDO.
        appearance35 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarGroup3 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup NO-UNDO.
        ultraExplorerBarGroup3 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem8 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem8 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem14 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem14 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem15 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem15 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem17 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem17 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
        appearance30 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem18 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem18 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
        appearance31 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem19 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem19 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
        appearance32 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
        appearance33 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem20 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem20 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
        appearance34 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem22 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem22 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem24 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem24 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem16 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem16 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
        appearance28 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem25 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem25 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarGroup4 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup NO-UNDO.
        ultraExplorerBarGroup4 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem13 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem13 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane1 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane1 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedRight, NEW System.Guid("b9c2d6e4-a747-404c-9c9a-8611a258480e":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane1 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane1 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("a28ad783-af14-4cae-80b9-61bf17aca0aa":U), NEW System.Guid("51b50fa4-8024-4c2b-bf8d-3399110ae595":U), 0, NEW System.Guid("b9c2d6e4-a747-404c-9c9a-8611a258480e":U), 0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane2 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane2 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedLeft, NEW System.Guid("c86595a1-165c-4326-863c-022ce7680f3b":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane2 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane2 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("ba6c7030-45a3-4d37-b015-fdbaaed1e677":U), NEW System.Guid("00000000-0000-0000-0000-000000000000":U), -1, NEW System.Guid("c86595a1-165c-4326-863c-022ce7680f3b":U), -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane3 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane3 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedRight, NEW System.Guid("c4ae2a1c-b62b-4932-a250-2c6d3f515bb5":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane3 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane3 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("01eac635-fa30-4da8-87a5-d2853eb4283e":U), NEW System.Guid("00000000-0000-0000-0000-000000000000":U), -1, NEW System.Guid("c4ae2a1c-b62b-4932-a250-2c6d3f515bb5":U), -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane4 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane4 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:Floating, NEW System.Guid("51b50fa4-8024-4c2b-bf8d-3399110ae595":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane5 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane5 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedBottom, NEW System.Guid("11ae04a2-6640-43f4-abf2-009ebb653c0e":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane4 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane4 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("4e3c149a-d2fd-49a2-b0ef-290d254a949d":U), NEW System.Guid("00000000-0000-0000-0000-000000000000":U), -1, NEW System.Guid("11ae04a2-6640-43f4-abf2-009ebb653c0e":U), -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("MainMenu":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool1 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool1 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("File":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool2 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool2 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Programs":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool3 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool3 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Window":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("Toolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool15 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool15 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("customer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool16 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool16 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("customersmart":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool17 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool17 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("wlargesmart":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("dockedsmartwindow":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("orderinq":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool20 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool20 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("veryold":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool4 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool4 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("File":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool5 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool5 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Programs":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("customer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("customersmart":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("wlargesmart":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("veryold":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("dockedsmartwindow":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("orderinq":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool6 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool6 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Window":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool1 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool1 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("startasmdichild":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE mdiWindowListTool1 AS Infragistics.Win.UltraWinToolbars.MdiWindowListTool NO-UNDO.
        mdiWindowListTool1 = NEW Infragistics.Win.UltraWinToolbars.MdiWindowListTool("MDIWindowListTool1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("customer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("customersmart":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("wlargesmart":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("veryold":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("dockedsmartwindow":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("orderinq":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool2 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool2 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("startasmdichild":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE mdiWindowListTool2 AS Infragistics.Win.UltraWinToolbars.MdiWindowListTool NO-UNDO.
        mdiWindowListTool2 = NEW Infragistics.Win.UltraWinToolbars.MdiWindowListTool("MDIWindowListTool1":U).
        THIS-OBJECT:taskPanelControl1 = NEW Advantzware.WinKit.Samples.TaskPanel.TaskPanelControl().
        THIS-OBJECT:ultraExplorerBar1 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBar().
        THIS-OBJECT:ultraMonthViewMulti1 = NEW Infragistics.Win.UltraWinSchedule.UltraMonthViewMulti().
        THIS-OBJECT:ultraCalendarInfo1 = NEW Infragistics.Win.UltraWinSchedule.UltraCalendarInfo(THIS-OBJECT:components).
        THIS-OBJECT:ultraCalendarLook1 = NEW Infragistics.Win.UltraWinSchedule.UltraCalendarLook(THIS-OBJECT:components).
        THIS-OBJECT:dockablePaneSizeTest1 = NEW Advantzware.WinKit.Samples.DockablePaneSizeTest().
        THIS-OBJECT:ultraTabbedMdiManager1 = NEW Infragistics.Win.UltraWinTabbedMdi.UltraTabbedMdiManager(THIS-OBJECT:components).
        THIS-OBJECT:ultraDockManager1 = NEW Infragistics.Win.UltraWinDock.UltraDockManager(THIS-OBJECT:components).
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaLeft = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaRight = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaTop = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaBottom = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_SampleMDIContainerAutoHideControl = NEW Infragistics.Win.UltraWinDock.AutoHideControl().
        THIS-OBJECT:dockableWindow1 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:dockableWindow2 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:dockableWindow3 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:dockableWindow4 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:windowDockingArea1 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:windowDockingArea2 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:windowContainer1 = NEW Progress.Windows.WindowContainer().
        THIS-OBJECT:windowDockingArea4 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:windowDockingArea5 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:windowDockingArea3 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        CAST(THIS-OBJECT:ultraExplorerBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraMonthViewMulti1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabbedMdiManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:m_SampleMDIContainerAutoHideControl:SuspendLayout().
        THIS-OBJECT:dockableWindow1:SuspendLayout().
        THIS-OBJECT:dockableWindow2:SuspendLayout().
        THIS-OBJECT:dockableWindow3:SuspendLayout().
        THIS-OBJECT:dockableWindow4:SuspendLayout().
        THIS-OBJECT:windowDockingArea2:SuspendLayout().
        THIS-OBJECT:windowDockingArea4:SuspendLayout().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* taskPanelControl1 */
        /*  */
        THIS-OBJECT:taskPanelControl1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:taskPanelControl1:Name = "taskPanelControl1":U.
        THIS-OBJECT:taskPanelControl1:Size = NEW System.Drawing.Size(299, 496).
        THIS-OBJECT:taskPanelControl1:TabIndex = 19.
        /*  */
        /* ultraExplorerBar1 */
        /*  */
        THIS-OBJECT:ultraExplorerBar1:AcceptsFocus = Infragistics.Win.DefaultableBoolean:True.
        ultraExplorerBarItem1:Key = "kundenstamm_alt":U.
        appearance10:Image = CAST(resources:GetObject("appearance10.Image":U), System.Object).
        ultraExplorerBarItem1:Settings:AppearancesLarge:Appearance = appearance10.
        ultraExplorerBarItem1:Text = "Kundenstamm":U.
        ultraExplorerBarItem2:Key = "kundenstamm_smart":U.
        appearance11:Image = CAST(resources:GetObject("appearance11.Image":U), System.Object).
        ultraExplorerBarItem2:Settings:AppearancesLarge:Appearance = appearance11.
        ultraExplorerBarItem2:Text = "SmartWindow":U.
        ultraExplorerBarItem3:Key = "adm2customerwindow":U.
        appearance12:Image = CAST(resources:GetObject("appearance12.Image":U), System.Object).
        ultraExplorerBarItem3:Settings:AppearancesLarge:Appearance = appearance12.
        ultraExplorerBarItem3:Text = "ADM2 Customer Window":U.
        ultraExplorerBarItem4:Key = "orderinq":U.
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        ultraExplorerBarItem4:Settings:AppearancesLarge:Appearance = appearance13.
        ultraExplorerBarItem4:Text = "Order Inquiry":U.
        ultraExplorerBarItem9:Key = "ordermaint":U.
        appearance9:Image = CAST(resources:GetObject("appearance9.Image":U), System.Object).
        ultraExplorerBarItem9:Settings:AppearancesLarge:Appearance = appearance9.
        ultraExplorerBarItem9:Text = "Order Maintenance":U.
        ultraExplorerBarItem5:Key = "dockedsmartwindow":U.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        ultraExplorerBarItem5:Settings:AppearancesLarge:Appearance = appearance1.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        ultraExplorerBarItem5:Settings:AppearancesSmall:Appearance = appearance2.
        ultraExplorerBarItem5:Text = "Docked SmartWindow":U.
        ultraExplorerBarItem6:Key = "tabfolder":U.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        ultraExplorerBarItem6:Settings:AppearancesLarge:Appearance = appearance3.
        ultraExplorerBarItem6:Text = "Tab Folder":U.
        ultraExplorerBarItem7:Key = "browserwindow":U.
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        ultraExplorerBarItem7:Settings:AppearancesLarge:Appearance = appearance7.
        ultraExplorerBarItem7:Text = "Browser Window":U.
        ultraExplorerBarItem23:Key = "step-by-step":U.
        appearance36:Image = CAST(resources:GetObject("appearance36.Image":U), System.Object).
        ultraExplorerBarItem23:Settings:AppearancesLarge:Appearance = appearance36.
        ultraExplorerBarItem23:Text = "Step by step migration":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem EXTENT 9 NO-UNDO.
        arrayvar0[1] = ultraExplorerBarItem1.
        arrayvar0[2] = ultraExplorerBarItem2.
        arrayvar0[3] = ultraExplorerBarItem3.
        arrayvar0[4] = ultraExplorerBarItem4.
        arrayvar0[5] = ultraExplorerBarItem9.
        arrayvar0[6] = ultraExplorerBarItem5.
        arrayvar0[7] = ultraExplorerBarItem6.
        arrayvar0[8] = ultraExplorerBarItem7.
        arrayvar0[9] = ultraExplorerBarItem23.
        ultraExplorerBarGroup1:Items:AddRange(arrayvar0).
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        ultraExplorerBarGroup1:Settings:AppearancesLarge:Appearance = appearance14.
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        ultraExplorerBarGroup1:Settings:AppearancesSmall:Appearance = appearance15.
        ultraExplorerBarGroup1:Text = "Master Data":U.
        ultraExplorerBarItem10:Key = "orderwin-std":U.
        appearance16:Image = CAST(resources:GetObject("appearance16.Image":U), System.Object).
        ultraExplorerBarItem10:Settings:AppearancesLarge:Appearance = appearance16.
        ultraExplorerBarItem10:Text = "Standard":U.
        ultraExplorerBarItem11:Key = "orderwin-emb":U.
        appearance17:Image = CAST(resources:GetObject("appearance17.Image":U), System.Object).
        ultraExplorerBarItem11:Settings:AppearancesLarge:Appearance = appearance17.
        ultraExplorerBarItem11:Text = "Embedded":U.
        ultraExplorerBarItem12:Key = "orderwin-emb-plus":U.
        appearance18:Image = CAST(resources:GetObject("appearance18.Image":U), System.Object).
        ultraExplorerBarItem12:Settings:AppearancesLarge:Appearance = appearance18.
        ultraExplorerBarItem12:Text = "Embedded PLUS":U.
        ultraExplorerBarItem21:Key = "orderwin-emb-plus2":U.
        appearance35:Image = CAST(resources:GetObject("appearance35.Image":U), System.Object).
        ultraExplorerBarItem21:Settings:AppearancesLarge:Appearance = appearance35.
        ultraExplorerBarItem21:Text = "Embedded PLUS2":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem EXTENT 4 NO-UNDO.
        arrayvar1[1] = ultraExplorerBarItem10.
        arrayvar1[2] = ultraExplorerBarItem11.
        arrayvar1[3] = ultraExplorerBarItem12.
        arrayvar1[4] = ultraExplorerBarItem21.
        ultraExplorerBarGroup2:Items:AddRange(arrayvar1).
        ultraExplorerBarGroup2:Text = "Order Maintenance":U.
        ultraExplorerBarItem8:Key = "demanet-pdf":U.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image":U), System.Object).
        ultraExplorerBarItem8:Settings:AppearancesLarge:Appearance = appearance8.
        ultraExplorerBarItem8:Text = "Document viewer":U.
        ultraExplorerBarItem14:Key = "defaultbutton":U.
        appearance20:Image = CAST(resources:GetObject("appearance20.Image":U), System.Object).
        ultraExplorerBarItem14:Settings:AppearancesLarge:Appearance = appearance20.
        ultraExplorerBarItem14:Text = "Default Button":U.
        ultraExplorerBarItem15:Key = "KeyPreview":U.
        appearance21:Image = CAST(resources:GetObject("appearance21.Image":U), System.Object).
        ultraExplorerBarItem15:Settings:AppearancesLarge:Appearance = appearance21.
        ultraExplorerBarItem15:Text = "KeyPreview Handler":U.
        ultraExplorerBarItem17:Key = "context-menu":U.
        appearance30:Image = CAST(resources:GetObject("appearance30.Image":U), System.Object).
        ultraExplorerBarItem17:Settings:AppearancesLarge:Appearance = appearance30.
        ultraExplorerBarItem17:Text = "Context Menu":U.
        ultraExplorerBarItem18:Key = "adm1test":U.
        appearance31:Image = CAST(resources:GetObject("appearance31.Image":U), System.Object).
        ultraExplorerBarItem18:Settings:AppearancesLarge:Appearance = appearance31.
        ultraExplorerBarItem18:Text = "ADM1 Test":U.
        ultraExplorerBarItem19:Key = "objects":U.
        appearance32:Image = CAST(resources:GetObject("appearance32.Image":U), System.Object).
        ultraExplorerBarItem19:Settings:AppearancesLarge:Appearance = appearance32.
        appearance33:Image = CAST(resources:GetObject("appearance33.Image":U), System.Object).
        ultraExplorerBarItem19:Settings:AppearancesSmall:Appearance = appearance33.
        ultraExplorerBarItem19:Text = "View Objects":U.
        ultraExplorerBarItem20:Key = "btnpopup":U.
        appearance34:Image = CAST(resources:GetObject("appearance34.Image":U), System.Object).
        ultraExplorerBarItem20:Settings:AppearancesLarge:Appearance = appearance34.
        ultraExplorerBarItem20:Text = "Popup Menu Button":U.
        ultraExplorerBarItem22:Key = "updatable-browser":U.
        ultraExplorerBarItem22:Text = "Updatable browser":U.
        ultraExplorerBarItem24:Key = "updatable-browser-2":U.
        ultraExplorerBarItem24:Text = "Updatable Browser 2":U.
        ultraExplorerBarItem16:Key = "dynbrowserwindow":U.
        appearance28:Image = CAST(resources:GetObject("appearance28.Image":U), System.Object).
        ultraExplorerBarItem16:Settings:AppearancesLarge:Appearance = appearance28.
        ultraExplorerBarItem16:Text = "Dynamic Browser":U.
        ultraExplorerBarItem25:Key = "lot-of-widgets":U.
        ultraExplorerBarItem25:Text = "Lots of widgets":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem EXTENT 11 NO-UNDO.
        arrayvar2[1] = ultraExplorerBarItem8.
        arrayvar2[2] = ultraExplorerBarItem14.
        arrayvar2[3] = ultraExplorerBarItem15.
        arrayvar2[4] = ultraExplorerBarItem17.
        arrayvar2[5] = ultraExplorerBarItem18.
        arrayvar2[6] = ultraExplorerBarItem19.
        arrayvar2[7] = ultraExplorerBarItem20.
        arrayvar2[8] = ultraExplorerBarItem22.
        arrayvar2[9] = ultraExplorerBarItem24.
        arrayvar2[10] = ultraExplorerBarItem16.
        arrayvar2[11] = ultraExplorerBarItem25.
        ultraExplorerBarGroup3:Items:AddRange(arrayvar2).
        ultraExplorerBarGroup3:Text = "Miscelangelous":U.
        ultraExplorerBarItem13:Key = "extremedesktopmenu":U.
        appearance19:Image = CAST(resources:GetObject("appearance19.Image":U), System.Object).
        ultraExplorerBarItem13:Settings:AppearancesLarge:Appearance = appearance19.
        ultraExplorerBarItem13:Text = "Menu":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem EXTENT 1 NO-UNDO.
        arrayvar3[1] = ultraExplorerBarItem13.
        ultraExplorerBarGroup4:Items:AddRange(arrayvar3).
        ultraExplorerBarGroup4:Text = "Extreme Windows Desktop Integration":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup EXTENT 4 NO-UNDO.
        arrayvar4[1] = ultraExplorerBarGroup1.
        arrayvar4[2] = ultraExplorerBarGroup2.
        arrayvar4[3] = ultraExplorerBarGroup3.
        arrayvar4[4] = ultraExplorerBarGroup4.
        THIS-OBJECT:ultraExplorerBar1:Groups:AddRange(arrayvar4).
        THIS-OBJECT:ultraExplorerBar1:GroupSettings:Style = Infragistics.Win.UltraWinExplorerBar.GroupStyle:LargeImagesWithTextBelow.
        THIS-OBJECT:ultraExplorerBar1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:ultraExplorerBar1:Name = "ultraExplorerBar1":U.
        THIS-OBJECT:ultraExplorerBar1:SettingsKey = "Advantzware.WinKit.Samples.SampleMDIContainer.ultraExplorerBar1":U.
        THIS-OBJECT:ultraExplorerBar1:ShowDefaultContextMenu = FALSE.
        THIS-OBJECT:ultraExplorerBar1:Size = NEW System.Drawing.Size(130, 475).
        THIS-OBJECT:ultraExplorerBar1:Style = Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarStyle:OutlookNavigationPane.
        THIS-OBJECT:ultraExplorerBar1:TabIndex = 10.
        THIS-OBJECT:ultraExplorerBar1:ViewStyle = Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarViewStyle:Office2007.
        THIS-OBJECT:ultraExplorerBar1:ItemClick:Subscribe(THIS-OBJECT:ultraExplorerBar1_ItemClick).
        /*  */
        /* ultraMonthViewMulti1 */
        /*  */
        THIS-OBJECT:ultraMonthViewMulti1:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:ultraMonthViewMulti1:CalendarInfo = THIS-OBJECT:ultraCalendarInfo1.
        THIS-OBJECT:ultraMonthViewMulti1:CalendarLook = THIS-OBJECT:ultraCalendarLook1.
        THIS-OBJECT:ultraMonthViewMulti1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:ultraMonthViewMulti1:MonthDimensions = NEW System.Drawing.Size(1, 3).
        THIS-OBJECT:ultraMonthViewMulti1:Name = "ultraMonthViewMulti1":U.
        THIS-OBJECT:ultraMonthViewMulti1:Size = NEW System.Drawing.Size(142, 372).
        THIS-OBJECT:ultraMonthViewMulti1:TabIndex = 12.
        /*  */
        /* ultraCalendarInfo1 */
        /*  */
        THIS-OBJECT:ultraCalendarInfo1:DataBindingsForAppointments:BindingContextControl = THIS-OBJECT.
        THIS-OBJECT:ultraCalendarInfo1:DataBindingsForOwners:BindingContextControl = THIS-OBJECT.
        /*  */
        /* ultraCalendarLook1 */
        /*  */
        THIS-OBJECT:ultraCalendarLook1:ViewStyle = Infragistics.Win.UltraWinSchedule.ViewStyle:Office2007.
        /*  */
        /* dockablePaneSizeTest1 */
        /*  */
        THIS-OBJECT:dockablePaneSizeTest1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:dockablePaneSizeTest1:Name = "dockablePaneSizeTest1":U.
        THIS-OBJECT:dockablePaneSizeTest1:Size = NEW System.Drawing.Size(863, 186).
        THIS-OBJECT:dockablePaneSizeTest1:TabIndex = 38.
        /*  */
        /* ultraTabbedMdiManager1 */
        /*  */
        THIS-OBJECT:ultraTabbedMdiManager1:AllowMaximize = TRUE.
        THIS-OBJECT:ultraTabbedMdiManager1:AllowNestedTabGroups = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraTabbedMdiManager1:MdiParent = THIS-OBJECT.
        THIS-OBJECT:ultraTabbedMdiManager1:Orientation = System.Windows.Forms.Orientation:Vertical.
        THIS-OBJECT:ultraTabbedMdiManager1:SaveSettings = TRUE.
        THIS-OBJECT:ultraTabbedMdiManager1:SettingsKey = "SampleMDIContainer.ultraTabbedMdiManager1":U.
        THIS-OBJECT:ultraTabbedMdiManager1:TabSettings:DisplayFormIcon = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraTabbedMdiManager1:TabSettings:HotTrack = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraTabbedMdiManager1:ViewStyle = Infragistics.Win.UltraWinTabbedMdi.ViewStyle:Office2007.
        /*  */
        /* ultraDockManager1 */
        /*  */
        THIS-OBJECT:ultraDockManager1:AutoHideDelay = 750.
        THIS-OBJECT:ultraDockManager1:CaptionStyle = Infragistics.Win.UltraWinDock.CaptionStyle:Office2007.
        THIS-OBJECT:ultraDockManager1:DefaultPaneSettings:AllowClose = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraDockManager1:DefaultPaneSettings:CanDisplayAsMdiChild = Infragistics.Win.DefaultableBoolean:False.
        dockAreaPane1:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:TabGroup.
        dockAreaPane1:DockedBefore = NEW System.Guid("c86595a1-165c-4326-863c-022ce7680f3b":U).
        dockAreaPane1:FloatingLocation = NEW System.Drawing.Point(433, 564).
        dockableControlPane1:Control = THIS-OBJECT:taskPanelControl1.
        dockableControlPane1:FlyoutSize = NEW System.Drawing.Size(299, 163).
        dockableControlPane1:OriginalControlBounds = NEW System.Drawing.Rectangle(272, 209, 453, 197).
        dockableControlPane1:Pinned = FALSE.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        dockableControlPane1:Settings:Appearance = appearance4.
        dockableControlPane1:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane1:Text = "Active tasks":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar5[1] = dockableControlPane1.
        dockAreaPane1:Panes:AddRange(arrayvar5).
        dockAreaPane1:Size = NEW System.Drawing.Size(299, 514).
        dockAreaPane2:DockedBefore = NEW System.Guid("c4ae2a1c-b62b-4932-a250-2c6d3f515bb5":U).
        dockableControlPane2:Control = THIS-OBJECT:ultraExplorerBar1.
        dockableControlPane2:OriginalControlBounds = NEW System.Drawing.Rectangle(98, 196, 175, 230).
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        dockableControlPane2:Settings:Appearance = appearance6.
        dockableControlPane2:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane2:Text = "Menu":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar6[1] = dockableControlPane2.
        dockAreaPane2:Panes:AddRange(arrayvar6).
        dockAreaPane2:Size = NEW System.Drawing.Size(130, 493).
        dockAreaPane3:DockedBefore = NEW System.Guid("51b50fa4-8024-4c2b-bf8d-3399110ae595":U).
        dockableControlPane3:Control = THIS-OBJECT:ultraMonthViewMulti1.
        dockableControlPane3:OriginalControlBounds = NEW System.Drawing.Rectangle(505, 42, 146, 382).
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        dockableControlPane3:Settings:Appearance = appearance5.
        dockableControlPane3:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane3:Text = "Appointments":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar7[1] = dockableControlPane3.
        dockAreaPane3:Panes:AddRange(arrayvar7).
        dockAreaPane3:Size = NEW System.Drawing.Size(146, 493).
        dockAreaPane4:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:TabGroup.
        dockAreaPane4:DockedBefore = NEW System.Guid("11ae04a2-6640-43f4-abf2-009ebb653c0e":U).
        dockAreaPane4:FloatingLocation = NEW System.Drawing.Point(433, 564).
        dockAreaPane4:Size = NEW System.Drawing.Size(598, 163).
        dockableControlPane4:Control = THIS-OBJECT:dockablePaneSizeTest1.
        dockableControlPane4:FlyoutSize = NEW System.Drawing.Size(-1, 204).
        dockableControlPane4:OriginalControlBounds = NEW System.Drawing.Rectangle(160, 233, 552, 215).
        dockableControlPane4:Pinned = FALSE.
        dockableControlPane4:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane4:Text = "Dockable Pane Meter":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar8[1] = dockableControlPane4.
        dockAreaPane5:Panes:AddRange(arrayvar8).
        dockAreaPane5:Size = NEW System.Drawing.Size(577, 204).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinDock.DockAreaPane EXTENT 5 NO-UNDO.
        arrayvar9[1] = dockAreaPane1.
        arrayvar9[2] = dockAreaPane2.
        arrayvar9[3] = dockAreaPane3.
        arrayvar9[4] = dockAreaPane4.
        arrayvar9[5] = dockAreaPane5.
        THIS-OBJECT:ultraDockManager1:DockAreas:AddRange(arrayvar9).
        THIS-OBJECT:ultraDockManager1:DragWindowStyle = Infragistics.Win.UltraWinDock.DragWindowStyle:LayeredWindowWithIndicators.
        THIS-OBJECT:ultraDockManager1:HostControl = THIS-OBJECT.
        THIS-OBJECT:ultraDockManager1:SaveSettingsFormat = Infragistics.Win.SaveSettingsFormat:Xml.
        THIS-OBJECT:ultraDockManager1:ShowCloseButton = FALSE.
        THIS-OBJECT:ultraDockManager1:ShowMenuButton = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraDockManager1:WindowStyle = Infragistics.Win.UltraWinDock.WindowStyle:Office2007.
        /*  */
        /* m_SampleMDIContainerUnpinnedTabAreaLeft */
        /*  */
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaLeft:Dock = System.Windows.Forms.DockStyle:Left.
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaLeft:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaLeft:Location = NEW System.Drawing.Point(0, 50).
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaLeft:Name = "m_SampleMDIContainerUnpinnedTabAreaLeft":U.
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaLeft:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaLeft:Size = NEW System.Drawing.Size(0, 514).
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaLeft:TabIndex = 1.
        /*  */
        /* m_SampleMDIContainerUnpinnedTabAreaRight */
        /*  */
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaRight:Dock = System.Windows.Forms.DockStyle:Right.
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaRight:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaRight:Location = NEW System.Drawing.Point(863, 50).
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaRight:Name = "m_SampleMDIContainerUnpinnedTabAreaRight":U.
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaRight:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaRight:Size = NEW System.Drawing.Size(21, 514).
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaRight:TabIndex = 2.
        /*  */
        /* m_SampleMDIContainerUnpinnedTabAreaTop */
        /*  */
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaTop:Dock = System.Windows.Forms.DockStyle:Top.
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaTop:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaTop:Location = NEW System.Drawing.Point(0, 50).
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaTop:Name = "m_SampleMDIContainerUnpinnedTabAreaTop":U.
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaTop:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaTop:Size = NEW System.Drawing.Size(863, 0).
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaTop:TabIndex = 3.
        /*  */
        /* m_SampleMDIContainerUnpinnedTabAreaBottom */
        /*  */
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaBottom:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaBottom:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaBottom:Location = NEW System.Drawing.Point(0, 543).
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaBottom:Name = "m_SampleMDIContainerUnpinnedTabAreaBottom":U.
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaBottom:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaBottom:Size = NEW System.Drawing.Size(863, 21).
        THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaBottom:TabIndex = 4.
        /*  */
        /* m_SampleMDIContainerAutoHideControl */
        /*  */
        THIS-OBJECT:m_SampleMDIContainerAutoHideControl:Controls:Add(THIS-OBJECT:dockableWindow1).
        THIS-OBJECT:m_SampleMDIContainerAutoHideControl:Controls:Add(THIS-OBJECT:dockableWindow4).
        THIS-OBJECT:m_SampleMDIContainerAutoHideControl:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_SampleMDIContainerAutoHideControl:Location = NEW System.Drawing.Point(0, 524).
        THIS-OBJECT:m_SampleMDIContainerAutoHideControl:Name = "m_SampleMDIContainerAutoHideControl":U.
        THIS-OBJECT:m_SampleMDIContainerAutoHideControl:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_SampleMDIContainerAutoHideControl:Size = NEW System.Drawing.Size(863, 19).
        THIS-OBJECT:m_SampleMDIContainerAutoHideControl:TabIndex = 5.
        /*  */
        /* dockableWindow1 */
        /*  */
        THIS-OBJECT:dockableWindow1:Controls:Add(THIS-OBJECT:taskPanelControl1).
        THIS-OBJECT:dockableWindow1:Location = NEW System.Drawing.Point(-10000, 0).
        THIS-OBJECT:dockableWindow1:Name = "dockableWindow1":U.
        THIS-OBJECT:dockableWindow1:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow1:Size = NEW System.Drawing.Size(299, 514).
        THIS-OBJECT:dockableWindow1:TabIndex = 45.
        /*  */
        /* dockableWindow2 */
        /*  */
        THIS-OBJECT:dockableWindow2:Controls:Add(THIS-OBJECT:ultraExplorerBar1).
        THIS-OBJECT:dockableWindow2:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:dockableWindow2:Name = "dockableWindow2":U.
        THIS-OBJECT:dockableWindow2:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow2:Size = NEW System.Drawing.Size(130, 493).
        THIS-OBJECT:dockableWindow2:TabIndex = 46.
        /*  */
        /* dockableWindow3 */
        /*  */
        THIS-OBJECT:dockableWindow3:Controls:Add(THIS-OBJECT:ultraMonthViewMulti1).
        THIS-OBJECT:dockableWindow3:Location = NEW System.Drawing.Point(5, 0).
        THIS-OBJECT:dockableWindow3:Name = "dockableWindow3":U.
        THIS-OBJECT:dockableWindow3:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow3:Size = NEW System.Drawing.Size(146, 493).
        THIS-OBJECT:dockableWindow3:TabIndex = 47.
        /*  */
        /* dockableWindow4 */
        /*  */
        THIS-OBJECT:dockableWindow4:Controls:Add(THIS-OBJECT:dockablePaneSizeTest1).
        THIS-OBJECT:dockableWindow4:Location = NEW System.Drawing.Point(5, 0).
        THIS-OBJECT:dockableWindow4:Name = "dockableWindow4":U.
        THIS-OBJECT:dockableWindow4:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow4:Size = NEW System.Drawing.Size(146, 493).
        THIS-OBJECT:dockableWindow4:TabIndex = 48.
        /*  */
        /* windowDockingArea1 */
        /*  */
        THIS-OBJECT:windowDockingArea1:Dock = System.Windows.Forms.DockStyle:Right.
        THIS-OBJECT:windowDockingArea1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea1:Location = NEW System.Drawing.Point(0, 50).
        THIS-OBJECT:windowDockingArea1:Name = "windowDockingArea1":U.
        THIS-OBJECT:windowDockingArea1:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea1:Size = NEW System.Drawing.Size(304, 514).
        THIS-OBJECT:windowDockingArea1:TabIndex = 11.
        /*  */
        /* windowDockingArea2 */
        /*  */
        THIS-OBJECT:windowDockingArea2:Controls:Add(THIS-OBJECT:dockableWindow2).
        THIS-OBJECT:windowDockingArea2:Dock = System.Windows.Forms.DockStyle:Left.
        THIS-OBJECT:windowDockingArea2:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea2:Location = NEW System.Drawing.Point(0, 50).
        THIS-OBJECT:windowDockingArea2:Name = "windowDockingArea2":U.
        THIS-OBJECT:windowDockingArea2:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea2:Size = NEW System.Drawing.Size(135, 493).
        THIS-OBJECT:windowDockingArea2:TabIndex = 13.
        /*  */
        /* windowContainer1 */
        /*  */
        THIS-OBJECT:windowContainer1:Location = NEW System.Drawing.Point(257, 177).
        THIS-OBJECT:windowContainer1:Name = "windowContainer1":U.
        THIS-OBJECT:windowContainer1:Size = NEW System.Drawing.Size(100, 100).
        THIS-OBJECT:windowContainer1:TabIndex = 26.
        THIS-OBJECT:windowContainer1:TabStop = FALSE.
        /*  */
        /* windowDockingArea4 */
        /*  */
        THIS-OBJECT:windowDockingArea4:Controls:Add(THIS-OBJECT:dockableWindow3).
        THIS-OBJECT:windowDockingArea4:Dock = System.Windows.Forms.DockStyle:Right.
        THIS-OBJECT:windowDockingArea4:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea4:Location = NEW System.Drawing.Point(712, 50).
        THIS-OBJECT:windowDockingArea4:Name = "windowDockingArea4":U.
        THIS-OBJECT:windowDockingArea4:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea4:Size = NEW System.Drawing.Size(151, 493).
        THIS-OBJECT:windowDockingArea4:TabIndex = 0.
        /*  */
        /* windowDockingArea5 */
        /*  */
        THIS-OBJECT:windowDockingArea5:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:windowDockingArea5:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea5:Location = NEW System.Drawing.Point(580, 50).
        THIS-OBJECT:windowDockingArea5:Name = "windowDockingArea5":U.
        THIS-OBJECT:windowDockingArea5:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea5:Size = NEW System.Drawing.Size(598, 163).
        THIS-OBJECT:windowDockingArea5:TabIndex = 38.
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.Forms.MDIContainer":U).
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        THIS-OBJECT:ultraToolbarsManager1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2007.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        ultraToolbar1:IsMainMenuBar = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar11[1] = popupMenuTool1.
        arrayvar11[2] = popupMenuTool2.
        arrayvar11[3] = popupMenuTool3.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar11).
        ultraToolbar1:Text = "MainMenu":U.
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar12[1] = buttonTool15.
        arrayvar12[2] = buttonTool16.
        arrayvar12[3] = buttonTool17.
        arrayvar12[4] = buttonTool18.
        arrayvar12[5] = buttonTool19.
        arrayvar12[6] = buttonTool20.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar12).
        ultraToolbar2:Text = "Toolbar":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 2 NO-UNDO.
        arrayvar13[1] = ultraToolbar1.
        arrayvar13[2] = ultraToolbar2.
        THIS-OBJECT:ultraToolbarsManager1:Toolbars:AddRange(arrayvar13).
        popupMenuTool4:SharedPropsInternal:Caption = "&File":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar14[1] = buttonTool1.
        popupMenuTool4:Tools:AddRange(arrayvar14).
        popupMenuTool5:SharedPropsInternal:Caption = "&Programs":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar15 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar15[1] = buttonTool3.
        arrayvar15[2] = buttonTool4.
        arrayvar15[3] = buttonTool7.
        arrayvar15[4] = buttonTool9.
        arrayvar15[5] = buttonTool11.
        arrayvar15[6] = buttonTool13.
        popupMenuTool5:Tools:AddRange(arrayvar15).
        popupMenuTool6:SharedPropsInternal:Caption = "&Window":U.
        popupMenuTool6:SharedPropsInternal:MergeOrder = 100.
        stateButtonTool1:Checked = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar16 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar16[1] = stateButtonTool1.
        arrayvar16[2] = mdiWindowListTool1.
        popupMenuTool6:Tools:AddRange(arrayvar16).
        buttonTool2:SharedPropsInternal:Caption = "&Exit":U.
        appearance22:Image = CAST(resources:GetObject("appearance22.Image":U), System.Object).
        buttonTool5:SharedPropsInternal:AppearancesSmall:Appearance = appearance22.
        buttonTool5:SharedPropsInternal:Caption = "Customer (simple)":U.
        appearance23:Image = CAST(resources:GetObject("appearance23.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesSmall:Appearance = appearance23.
        buttonTool6:SharedPropsInternal:Caption = "Customer (SmartWindow)":U.
        appearance24:Image = CAST(resources:GetObject("appearance24.Image":U), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesSmall:Appearance = appearance24.
        buttonTool8:SharedPropsInternal:Caption = "Customer (large SmartWindow)":U.
        appearance25:Image = CAST(resources:GetObject("appearance25.Image":U), System.Object).
        buttonTool10:SharedPropsInternal:AppearancesSmall:Appearance = appearance25.
        buttonTool10:SharedPropsInternal:Caption = "Very old window (Down-Frame)":U.
        appearance26:Image = CAST(resources:GetObject("appearance26.Image":U), System.Object).
        buttonTool12:SharedPropsInternal:AppearancesSmall:Appearance = appearance26.
        buttonTool12:SharedPropsInternal:Caption = "Docked SmartWindow":U.
        appearance27:Image = CAST(resources:GetObject("appearance27.Image":U), System.Object).
        buttonTool14:SharedPropsInternal:AppearancesSmall:Appearance = appearance27.
        buttonTool14:SharedPropsInternal:Caption = "Order inquiry":U.
        stateButtonTool2:Checked = TRUE.
        stateButtonTool2:SharedPropsInternal:Caption = "Start as MDI Child":U.
        mdiWindowListTool2:SharedPropsInternal:Caption = "MDIWindowListTool1":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar17 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 12 NO-UNDO.
        arrayvar17[1] = popupMenuTool4.
        arrayvar17[2] = popupMenuTool5.
        arrayvar17[3] = popupMenuTool6.
        arrayvar17[4] = buttonTool2.
        arrayvar17[5] = buttonTool5.
        arrayvar17[6] = buttonTool6.
        arrayvar17[7] = buttonTool8.
        arrayvar17[8] = buttonTool10.
        arrayvar17[9] = buttonTool12.
        arrayvar17[10] = buttonTool14.
        arrayvar17[11] = stateButtonTool2.
        arrayvar17[12] = mdiWindowListTool2.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar17).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        /*  */
        /* m_SampleMDIContainer_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 50).
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Left:Name = "m_SampleMDIContainer_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(0, 514).
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_SampleMDIContainer_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(884, 50).
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Right:Name = "m_SampleMDIContainer_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(0, 514).
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_SampleMDIContainer_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Top:Name = "m_SampleMDIContainer_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(884, 50).
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_SampleMDIContainer_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 564).
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Bottom:Name = "m_SampleMDIContainer_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(884, 0).
        THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* windowDockingArea3 */
        /*  */
        THIS-OBJECT:windowDockingArea3:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:windowDockingArea3:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea3:Location = NEW System.Drawing.Point(135, 334).
        THIS-OBJECT:windowDockingArea3:Name = "windowDockingArea3":U.
        THIS-OBJECT:windowDockingArea3:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea3:Size = NEW System.Drawing.Size(577, 209).
        THIS-OBJECT:windowDockingArea3:TabIndex = 39.
        /*  */
        /* SampleMDIContainer */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(884, 564).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SampleMDIContainerAutoHideControl).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowContainer1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea3).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea4).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaTop).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaBottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaLeft).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SampleMDIContainerUnpinnedTabAreaRight).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SampleMDIContainer_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:IsMdiContainer = TRUE.
        THIS-OBJECT:Name = "SampleMDIContainer":U.
        THIS-OBJECT:Text = "WinKit Sample Application":U.
        THIS-OBJECT:WindowPositionRegistryKey = "SampleMDIContainer":U.
        CAST(THIS-OBJECT:ultraExplorerBar1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraMonthViewMulti1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabbedMdiManager1, System.Configuration.IPersistComponentSettings):LoadComponentSettings().
        CAST(THIS-OBJECT:ultraTabbedMdiManager1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:m_SampleMDIContainerAutoHideControl:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow1:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow2:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow3:ResumeLayout(FALSE).
        THIS-OBJECT:dockableWindow4:ResumeLayout(FALSE).
        THIS-OBJECT:windowDockingArea2:ResumeLayout(FALSE).
        THIS-OBJECT:windowDockingArea4:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/	
	METHOD PUBLIC VOID LaunchWindow (pcWindow AS CHARACTER):
		
        DEFINE VARIABLE hWindow AS HANDLE                    NO-UNDO.
        DEFINE VARIABLE oForm   AS System.Windows.Forms.Form NO-UNDO .
		
		DO ON STOP UNDO, LEAVE: 
    		IF pcWindow = "customer" THEN 
    		    RUN demo\customer.w PERSISTENT SET hWindow .  
            IF pcWindow = "customersmart" THEN DO:
                RUN demo\customersmart.w PERSISTENT SET hWindow .
                RUN initializeObject IN hWindow . 
            END.
            IF pcWindow = "wlargesmart" THEN DO:
                RUN demo\wlargesmart.w PERSISTENT SET hWindow .
                RUN initializeObject IN hWindow . 
            END.
            IF pcWindow = "veryold-order":U THEN DO:
                RUN demo\veryold-order.w PERSISTENT SET hWindow (?) . 
            END.
            IF pcWindow = "dockedsmartwindow":U THEN DO:
                RUN demo\dockedsmartwindow\wdockedsmartwindow.w PERSISTENT SET hWindow . 
                RUN initializeObject IN hWindow . 
            END.
/*            IF pcWindow = "orderinq" THEN DO:                                              */
/*                oForm = NEW Advantzware.WinKit.Samples.PimpedOldWindow.OrderLookupForm () .*/
/*                oForm:MdiParent = THIS-OBJECT .                                            */
/*                oForm:Show () .                                                            */
/*            END.                                                                           */
            IF pcWindow = "tabfolder" THEN DO:
                RUN demo\tabfolderwin.w PERSISTENT SET hWindow. 
            END.
            IF pcWindow = "browserwindow" THEN DO:
                RUN demo\browserwindow.w PERSISTENT SET hWindow. 
            END.
            IF pcWindow = "demanet-pdf" THEN DO:
/*                RUN demanet\w-documents.w PERSISTENT SET hWindow.*/
/*                RUN initializeObject IN hWindow .                */

                RUN Test/Ess/ANTRST.W PERSISTENT .

            END.
            IF pcWindow = "ordermaint":U THEN DO:
                RUN demo\orderwindow\worder.w PERSISTENT SET hWindow.
                RUN initializeObject IN hWindow .
            END.
    
            IF pcWindow = "adm2customerwindow":U THEN DO:
                RUN demo/adm2smartwindow/wdemo.w PERSISTENT SET hWindow.
                RUN initializeObject IN hWindow .
            END.
            
            IF pcWindow = "orderwin-std":U THEN 
                RUN demo\ordermaint\orderwin.w PERSISTENT . 
            IF pcWindow = "orderwin-emd":U THEN 
                RUN demo\ordermaint\orderwin_embedded.w PERSISTENT .
            IF pcWindow = "orderwin-emd-plus":U THEN
                RUN demo\ordermaint\orderwin_embedded_plus.w PERSISTENT .
            IF pcWindow = "orderwin-emb-plus2":U THEN
                RUN demo\ordermaint\orderwin_embedded_plus2.w PERSISTENT .
            IF pcWindow = "extremedesktopmenu":U THEN 
                RUN extreme-windows-desktop\menuwin.w PERSISTENT . 
            
            IF pcWindow = "defaultbutton":U THEN 
                RUN test\default-buttons.w PERSISTENT . 
            IF pcWindow = "KeyPreview":U THEN 
                RUN test\keypreview\esc-keypreview.w PERSISTENT . 
            IF pcWindow = "context-menu":U THEN 
                RUN test\ContextMenu\w-context.w PERSISTENT . 
    
            IF pcWindow = "adm1test":U THEN DO:
                RUN demo/adm1window/w-win.w PERSISTENT SET hWindow.
                RUN dispatch IN hWindow ("initialize":U) .
			END.
            IF pcWindow = "objects":U THEN DO:
                oForm = NEW Consultingwerk.Windows.Util.Forms.ViewObjectsForm () . 
                oForm:MdiParent = THIS-OBJECT . 
                oForm:Show () .
            END.
            IF pcWindow = "btnpopup":U THEN 
                RUN src/demo/w-popupmenubutton.w .
            IF pcWindow = "updatable-browser":U THEN 
                RUN src/test/updatable-browser.w PERSISTENT . 
            IF pcWindow = "updatable-browser-2":U THEN 
                RUN src/test/updatable-browser-mit-pick-event.w PERSISTENT .                 
                
            IF pcWindow = "step-by-step":U THEN 
                RUN src/demo/step-by-step/c-win.w PERSISTENT . 
                
            IF pcWindow = "dynbrowserwindow":U THEN 
                RUN src/demo/dynbrowserwindow.w PERSISTENT . 
                
            IF pcWindow = "lot-of-widgets":U THEN 
                RUN src/test/w-lots-of-widgets.w PERSISTENT . 
        END.
        
        CATCH err AS Progress.Lang.Error :
        	Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err)	.
        END CATCH.
        

	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/	
	METHOD PUBLIC VOID LoadApplicationStyle (pcStyleSet AS CHARACTER):
		
        FILE-INFORMATION:FILE-NAME = SUBSTITUTE ("Advantzware\WinKit\Styles\&1.isl":U,
                                                 pcStyleSet).
        
        IF FILE-INFORMATION:FULL-PATHNAME = ? THEN DO:
            MESSAGE SUBSTITUTE ("Invalid style set &1.", pcStyleSet) 
                VIEW-AS ALERT-BOX ERROR .
            RETURN . 
        END.                
                           
        Infragistics.Win.AppStyling.StyleManager:Load (FILE-INFORMATION:FULL-PATHNAME) .         
    
        /* Initialize colors */
        Consultingwerk.Util.StyleLibraryHelper:SetProgressColorTable 
            (17, "UltraPanel":U, RoleState:Normal, ?) .         

        THIS-OBJECT:ApplicationStyle = pcStyleSet . 
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose:                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD OVERRIDE PROTECTED VOID OnFormClosing (INPUT e AS System.Windows.Forms.FormClosingEventArgs):
        
        DEFINE VARIABLE i                   AS INTEGER                   NO-UNDO.
        DEFINE VARIABLE oForm               AS System.Windows.Forms.Form NO-UNDO . 
        DEFINE VARIABLE oEmbeddedWindowForm AS IEmbeddedWindowForm       NO-UNDO . 
        DEFINE VARIABLE lOk                 AS LOGICAL                   NO-UNDO.
        
        DeactivateWindowTitleEventHandler() .               
                                               
        IF THIS-OBJECT:MdiChildren:Length > 0 THEN 
        DO i = THIS-OBJECT:MdiChildren:Length - 1 TO 0 BY -1:
            
              oForm = CAST(THIS-OBJECT:MdiChildren:GetValue(i),
                           System.Windows.Forms.Form) . 
        
              IF TYPE-OF (oForm, IEmbeddedWindowForm) THEN DO:                
                  oEmbeddedWindowForm = CAST (oForm, IEmbeddedWindowForm) . 
                  
                  oEmbeddedWindowForm:CloseEmbeddedWindows(OUTPUT lOk) .
                  
                  IF NOT lOk THEN DO:
                      oForm:Activate () . 
                      
                      MESSAGE "Cannot close. Please close child window first."
                          VIEW-AS ALERT-BOX INFORMATION . 
                      
                      e:Cancel = TRUE . 
                      RETURN . 
                  END.
              END.  
        END.        

        SUPER:OnFormClosing(INPUT e).

        IF NOT e:Cancel = TRUE THEN 
            Consultingwerk.Design.WinKit.DesignManager:Close() .

    END METHOD.


	/*------------------------------------------------------------------------------
		Purpose: Raises the Load event  																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
		
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i         AS INTEGER   NO-UNDO.
		
		SUPER:OnLoad (e) .
		
		FILE-INFO:FILE-NAME = "Consultingwerk\WindowIntegrationKit\Samples\Images\winkit400.png":U .
		
		ASSIGN cFileName = FILE-INFO:FULL-PATHNAME . 
		
		IF cFileName > "":U THEN 
    		DO i = 0 TO THIS-OBJECT:Controls:Count - 1:
    
    		    IF TYPE-OF (THIS-OBJECT:Controls[i], System.Windows.Forms.MdiClient) THEN DO:
    		      
    		        CAST (THIS-OBJECT:Controls[i], System.Windows.Forms.MdiClient):BackgroundImage 
    		              = System.Drawing.Bitmap:FromFile (cFileName) . 
    		        
    		        LEAVE . 
    		    END.
    		END.

	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Raises the Shown event  																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/	
	METHOD OVERRIDE PROTECTED VOID OnShown (e AS System.EventArgs):
		
		SUPER:OnShown(e).
		
		/* Mike Fechner, Consultingwerk Ltd. 02.11.2009
           Create a Timer to Close the Splash screen */
        
		IF VALID-OBJECT (oSplash) THEN DO:
		    oTimer = NEW System.Windows.Forms.Timer () .
		    oTimer:Tick:Subscribe (TimerTick) .
		    oTimer:Start () .    
		    oTimer:Interval = 3000 .
		END.
		    
	END METHOD.

 	/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/	
	METHOD PUBLIC VOID SetMdiChildDefaultMode (plMdiChild AS LOGICAL):
		
		WinKitForms:DefaultMakeMdiChild = plMdiChild .

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/	
	METHOD PROTECTED VOID ShowOptionsDialog(  ):
		
		DEFINE VARIABLE oOptionsDialogForm AS OptionsDialogForm NO-UNDO . 
		
		oOptionsDialogForm = NEW OptionsDialogForm () .
		
		WAIT-FOR oOptionsDialogForm:ShowDialog (THIS-OBJECT) . 
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/	
	METHOD PROTECTED VOID TimerTick (sender AS System.Object, 
	                                 e AS System.EventArgs) :
		
		oSplash:Close (). 
		oTimer:Dispose () .
		
	END METHOD.

   	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ultraExplorerBar1_ItemClick( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinExplorerBar.ItemEventArgs ):
		
		CASE e:Item:Key:
		    
		    WHEN "kundenstamm_alt":U THEN 
		        LaunchWindow ("customer") .		        
		    WHEN "kundenstamm_smart":U THEN 
		        LaunchWindow ("customersmart") .
		    WHEN "wlargesmart":U THEN 
		        LaunchWindow ("wlargesmart") .
	        WHEN "veryold":U THEN 
	            LaunchWindow ("veryold-order":U) .
            WHEN "orderinq":U THEN 
            	LaunchWindow ("orderinq":U) .
            WHEN "ordermaint":U THEN 
                LaunchWindow ("ordermaint":U) .     
            WHEN "dockedsmartwindow":U THEN 
                LaunchWindow ("dockedsmartwindow":U) . 	
            WHEN "tabfolder":U THEN
                LaunchWindow ("tabfolder":U) . 
            WHEN "browserwindow":U THEN 
                LaunchWindow ("browserwindow":U) .    
                
            WHEN "orderwin-std":U THEN 
                LaunchWindow ("orderwin-std":U) .
            WHEN "orderwin-emb":U THEN 
                LaunchWindow ("orderwin-emd":U) .
            WHEN "orderwin-emb-plus":U THEN 
                LaunchWindow ("orderwin-emd-plus":U) .
            WHEN "orderwin-emb-plus2":U THEN 
                LaunchWindow ("orderwin-emb-plus2":U) .
            WHEN "extremedesktopmenu":U THEN 
                LaunchWindow ("extremedesktopmenu":U) . 
                                
            WHEN "demanet-pdf":U THEN 
                LaunchWindow ("demanet-pdf":U) .    

            WHEN "defaultbutton":U THEN 
                LaunchWindow ("defaultbutton":U) .    
            WHEN "KeyPreview":U THEN 
                LaunchWindow ("KeyPreview":U) . 
            WHEN "context-menu":U THEN 
                LaunchWindow ("context-menu":U) .
            
            WHEN "adm1test" THEN 
                LaunchWindow ("adm1test":U) .                        		        
            WHEN "objects":U THEN 
                LaunchWindow ("objects":U) .
            WHEN "btnpopup":U THEN 
                LaunchWindow ("btnpopup":U) .
            WHEN "updatable-browser":U THEN 
                LaunchWindow ("updatable-browser":U) . 
            WHEN "updatable-browser-2":U THEN 
                LaunchWindow ("updatable-browser-2":U) .
            
    
            WHEN "adm2customerwindow":U THEN 
                LaunchWindow ("adm2customerwindow":U) .
            
            WHEN "step-by-step":U THEN 
                LaunchWindow ("step-by-step":U) .
                
            WHEN "dynbrowserwindow":U THEN 
                LaunchWindow ("dynbrowserwindow":U) .
                
            WHEN "lot-of-widgets":U THEN 
                LaunchWindow ("lot-of-widgets":U) .                
                                
		END CASE.

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

	END METHOD.

  	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ultraToolbarsManager1_ToolClick( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs ):
			  
		CASE e:Tool:Key:
	        WHEN "Exit":U THEN 
	            THIS-OBJECT:Close () .
		    WHEN "customer":U THEN 
		        LaunchWindow ("customer":U) .		        
            WHEN "customersmart":U THEN  
                LaunchWindow ("customersmart":U) .	
            WHEN "wlargesmart":U THEN             
                LaunchWindow ("wlargesmart":U) .	        
	        WHEN "veryold":U THEN 
	            LaunchWindow ("veryold-order":U) .
            WHEN "dockedsmartwindow" THEN 
                LaunchWindow ("dockedsmartwindow":U) .
            WHEN "orderinq" THEN 
                LaunchWindow ("orderinq":U) .	            
            WHEN "startasmdichild" THEN 
                SetMdiChildDefaultMode (CAST(e:Tool, StateButtonTool):Checked) .
            WHEN "btnpopup":U THEN 
                LaunchWindow ("btnpopup":U) .

		END CASE. 			

        CATCH err AS Progress.Lang.Error :
        	Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

	END METHOD.

	DESTRUCTOR PUBLIC SampleMDIContainer ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.
