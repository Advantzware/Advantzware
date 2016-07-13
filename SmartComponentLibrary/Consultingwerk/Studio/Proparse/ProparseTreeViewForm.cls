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
    File        : ProparseTreeViewForm
    Purpose     : Vsualizes the Proparse AST of progress source code 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jun 28 20:25:50 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (DotNetAccessible) NE 0 &THEN 
USING Consultingwerk.Forms.BaseForm    FROM PROPATH .
USING Consultingwerk.Studio.Proparse.* FROM PROPATH .
USING org.prorefactor.core.JPNode      FROM ASSEMBLY .
USING org.prorefactor.treeparser.*     FROM ASSEMBLY .
USING Progress.Lang.*                  FROM PROPATH .
USING System.Windows.Forms.*           FROM ASSEMBLY .
&ENDIF

CLASS Consultingwerk.Studio.Proparse.ProparseTreeViewForm 
&IF DEFINED (DotNetAccessible) NE 0 &THEN 
    INHERITS BaseForm
&ENDIF
    : 
    
&IF DEFINED (DotNetAccessible) NE 0 &THEN 
    DEFINE PRIVATE VARIABLE dockableWindow1 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ProparseTreeViewFormUnpinnedTabAreaTop AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ProparseTreeViewFormUnpinnedTabAreaRight AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ProparseTreeViewFormUnpinnedTabAreaLeft AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ProparseTreeViewFormUnpinnedTabAreaBottom AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ProparseTreeViewFormAutoHideControl AS Infragistics.Win.UltraWinDock.AutoHideControl NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ProparseTreeViewForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ProparseTreeViewForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ProparseTreeViewForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ProparseTreeViewForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraDockManager1 AS Infragistics.Win.UltraWinDock.UltraDockManager NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraStatusBar1 AS Infragistics.Win.UltraWinStatusBar.UltraStatusBar NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabbedMdiManager1 AS Infragistics.Win.UltraWinTabbedMdi.UltraTabbedMdiManager NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
    DEFINE PRIVATE VARIABLE proparseTreeViewControl1 AS Consultingwerk.Studio.Proparse.ProparseTreeViewControl NO-UNDO.
    DEFINE PRIVATE VARIABLE windowDockingArea1 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
        
    DEFINE VARIABLE oEditors AS "System.Collections.Generic.Dictionary<System.String, System.Windows.Forms.Form>" NO-UNDO .
    DEFINE VARIABLE lFirstTime AS LOGICAL NO-UNDO INIT TRUE .
        
    DEFINE VARIABLE oLruController AS Consultingwerk.Windows.Controls.UltraRibbonLruController NO-UNDO  .
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ProparseTreeViewForm class                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProparseTreeViewForm ():
        
        SUPER().
        InitializeComponent().
        
        Consultingwerk.Framework.FrameworkSettings:MdiContainer = THIS-OBJECT . 
        Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive = TRUE . 
        
        oEditors = NEW "System.Collections.Generic.Dictionary<System.String, System.Windows.Forms.Form>" () . 

        /* Mike Fechner, Consultingwerk Ltd. 07.08.2014
           Setup registry profile when launched from PDSOE directly */
        IF Consultingwerk.Framework.FrameworkSettings:BaseRegistryKey MATCHES "*Sample Applications*":U THEN DO:
        
            Consultingwerk.Framework.FrameworkSettings:BaseRegistryKey = "Software~\Consultingwerk Ltd.~\Proparse TreeView Form~\":U .
            Consultingwerk.Framework.FrameworkSettings:StoreWindowPosition = TRUE . 
            Consultingwerk.Framework.FrameworkSettings:StoreRibbonQuickAccessToolbar = TRUE . 
        END.
        
        oLruController = NEW Consultingwerk.Windows.Controls.UltraRibbonLruController (THIS-OBJECT:ultraToolbarsManager1,
                                                                                       "LRU":U,
                                                                                       "&1":U,
                                                                                       10) .
        
        IF NOT PROVERSION BEGINS "10":U THEN DO:
            ultraToolbarsManager1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010 . 
            ultraToolbarsManager1:Ribbon:ApplicationMenuButtonImage = ? .
        END.
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Displays status in the status bar                                                                       
        Notes:                                    
        @param pcStatus The status to display                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisplayStatus (pcStatus AS CHARACTER):
        
        THIS-OBJECT:ultraStatusBar1:Panels["status":U]:Text = pcStatus . 
        
        PROCESS EVENTS . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handle for the FormClosedHandler event of the Procedure Editor 
                 Forms                                                                        
        Notes:        
        @param sender The reference to the object that raised the event
        @param e The FormClosedEventArgs with the data for this event                                                                
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID EditorFormClosedHandler (sender AS System.Object,
                                                   e AS System.Windows.Forms.FormClosedEventArgs):
        
        DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO .
        
        oForm = CAST (sender, System.Windows.Forms.Form) . 
        
        IF oEditors:ContainsValue (oForm) THEN DO: 
            {Consultingwerk/foreach.i '"System.Collections.Generic.KeyValuePair<System.String, System.Windows.Forms.Form>"' oPair in oEditors}
            
                IF oPair:Value = oForm THEN DO:
                    oEditors:Remove (oPair:Key) . 
                    
                    LEAVE .                   
                END. 
            END.
        END . 

        oForm:FormClosed:Unsubscribe (EditorFormClosedHandler) .

    END METHOD.

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
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("ribbon1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ParseFromClipboard":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup2":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("OpenFile":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup3":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool1 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool1 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Window":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("OpenFile":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool2 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool2 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Window":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE mdiWindowListTool1 AS Infragistics.Win.UltraWinToolbars.MdiWindowListTool NO-UNDO.
        mdiWindowListTool1 = NEW Infragistics.Win.UltraWinToolbars.MdiWindowListTool("MDIWindowListTool1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE mdiWindowListTool2 AS Infragistics.Win.UltraWinToolbars.MdiWindowListTool NO-UNDO.
        mdiWindowListTool2 = NEW Infragistics.Win.UltraWinToolbars.MdiWindowListTool("MDIWindowListTool1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ParseFromClipboard":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockAreaPane1 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
        dockAreaPane1 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedLeft, NEW System.Guid("4f533908-1ea4-48f9-9206-ad6bec27346f":U)).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dockableControlPane1 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
        dockableControlPane1 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("bb207e24-ef89-4d0e-a5c5-43379b943782":U), NEW System.Guid("00000000-0000-0000-0000-000000000000":U), -1, NEW System.Guid("4f533908-1ea4-48f9-9206-ad6bec27346f":U), -1).
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
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Studio.Proparse.ProparseTreeViewForm":U).
        THIS-OBJECT:proparseTreeViewControl1 = NEW Consultingwerk.Studio.Proparse.ProparseTreeViewControl().
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:ultraDockManager1 = NEW Infragistics.Win.UltraWinDock.UltraDockManager(THIS-OBJECT:components).
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaLeft = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaRight = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaTop = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaBottom = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_ProparseTreeViewFormAutoHideControl = NEW Infragistics.Win.UltraWinDock.AutoHideControl().
        THIS-OBJECT:windowDockingArea1 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
        THIS-OBJECT:dockableWindow1 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
        THIS-OBJECT:ultraTabbedMdiManager1 = NEW Infragistics.Win.UltraWinTabbedMdi.UltraTabbedMdiManager(THIS-OBJECT:components).
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        THIS-OBJECT:ultraStatusBar1 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusBar().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:windowDockingArea1:SuspendLayout().
        THIS-OBJECT:dockableWindow1:SuspendLayout().
        CAST(THIS-OBJECT:ultraTabbedMdiManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraStatusBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* proparseTreeViewControl1 */
        /*  */
        THIS-OBJECT:proparseTreeViewControl1:Location = NEW System.Drawing.Point(0, 18).
        THIS-OBJECT:proparseTreeViewControl1:Name = "proparseTreeViewControl1":U.
        THIS-OBJECT:proparseTreeViewControl1:Size = NEW System.Drawing.Size(314, 201).
        THIS-OBJECT:proparseTreeViewControl1:TabIndex = 5.
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.Forms.BaseForm":U).
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:ApplicationMenuButtonImage = CAST(resources:GetObject("ultraToolbarsManager1.Ribbon.ApplicationMenuButtonImage":U), System.Drawing.Image).
        ribbonTab1:Caption = "Start":U.
        ribbonGroup1:Caption = "Start":U.
        buttonTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool5:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        arrayvar0[2] = buttonTool5.
        ribbonGroup1:Tools:AddRange(arrayvar0).
        ribbonGroup2:Caption = "Source Code":U.
        buttonTool3:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar1[1] = buttonTool3.
        ribbonGroup2:Tools:AddRange(arrayvar1).
        ribbonGroup3:Caption = "View":U.
        popupMenuTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar2[1] = popupMenuTool1.
        ribbonGroup3:Tools:AddRange(arrayvar2).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 3 NO-UNDO.
        arrayvar3[1] = ribbonGroup1.
        arrayvar3[2] = ribbonGroup2.
        arrayvar3[3] = ribbonGroup3.
        ribbonTab1:Groups:AddRange(arrayvar3).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar4[1] = ribbonTab1.
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar4).
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:Visible = TRUE.
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance1.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        buttonTool2:SharedPropsInternal:Caption = "Open":U.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesLarge:Appearance = appearance3.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesSmall:Appearance = appearance4.
        buttonTool4:SharedPropsInternal:Caption = "Open File":U.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        popupMenuTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance5.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        popupMenuTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance6.
        popupMenuTool2:SharedPropsInternal:Caption = "Window":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar5[1] = mdiWindowListTool1.
        popupMenuTool2:Tools:AddRange(arrayvar5).
        mdiWindowListTool2:SharedPropsInternal:Caption = "MDIWindowListTool1":U.
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesLarge:Appearance = appearance7.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesSmall:Appearance = appearance8.
        buttonTool6:SharedPropsInternal:Caption = "Parse from Clipboard":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 5 NO-UNDO.
        arrayvar6[1] = buttonTool2.
        arrayvar6[2] = buttonTool4.
        arrayvar6[3] = popupMenuTool2.
        arrayvar6[4] = mdiWindowListTool2.
        arrayvar6[5] = buttonTool6.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar6).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        /*  */
        /* m_ProparseTreeViewForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 11.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 211).
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Left:Name = "m_ProparseTreeViewForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(11, 219).
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ProparseTreeViewForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 11.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(715, 211).
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Right:Name = "m_ProparseTreeViewForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(11, 219).
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ProparseTreeViewForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Top:Name = "m_ProparseTreeViewForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(726, 211).
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ProparseTreeViewForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 430).
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Bottom:Name = "m_ProparseTreeViewForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(726, 0).
        THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* ultraDockManager1 */
        /*  */
        THIS-OBJECT:ultraDockManager1:CaptionStyle = Infragistics.Win.UltraWinDock.CaptionStyle:Office2007.
        THIS-OBJECT:ultraDockManager1:CompressUnpinnedTabs = FALSE.
        dockableControlPane1:Control = THIS-OBJECT:proparseTreeViewControl1.
        dockableControlPane1:OriginalControlBounds = NEW System.Drawing.Rectangle(53, 178, 223, 366).
        dockableControlPane1:Size = NEW System.Drawing.Size(100, 100).
        dockableControlPane1:Text = "Parser Tree":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
        arrayvar7[1] = dockableControlPane1.
        dockAreaPane1:Panes:AddRange(arrayvar7).
        dockAreaPane1:Size = NEW System.Drawing.Size(314, 219).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinDock.DockAreaPane EXTENT 1 NO-UNDO.
        arrayvar8[1] = dockAreaPane1.
        THIS-OBJECT:ultraDockManager1:DockAreas:AddRange(arrayvar8).
        THIS-OBJECT:ultraDockManager1:DragIndicatorStyle = Infragistics.Win.UltraWinDock.DragIndicatorStyle:VisualStudio2008Vista.
        THIS-OBJECT:ultraDockManager1:DragWindowStyle = Infragistics.Win.UltraWinDock.DragWindowStyle:LayeredWindowWithIndicators.
        THIS-OBJECT:ultraDockManager1:HostControl = THIS-OBJECT.
        THIS-OBJECT:ultraDockManager1:WindowStyle = Infragistics.Win.UltraWinDock.WindowStyle:Office2007.
        /*  */
        /* m_ProparseTreeViewFormUnpinnedTabAreaLeft */
        /*  */
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaLeft:Dock = System.Windows.Forms.DockStyle:Left.
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaLeft:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaLeft:Location = NEW System.Drawing.Point(11, 211).
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaLeft:Name = "m_ProparseTreeViewFormUnpinnedTabAreaLeft":U.
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaLeft:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaLeft:Size = NEW System.Drawing.Size(0, 219).
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaLeft:TabIndex = 7.
        /*  */
        /* m_ProparseTreeViewFormUnpinnedTabAreaRight */
        /*  */
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaRight:Dock = System.Windows.Forms.DockStyle:Right.
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaRight:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaRight:Location = NEW System.Drawing.Point(715, 211).
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaRight:Name = "m_ProparseTreeViewFormUnpinnedTabAreaRight":U.
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaRight:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaRight:Size = NEW System.Drawing.Size(0, 219).
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaRight:TabIndex = 8.
        /*  */
        /* m_ProparseTreeViewFormUnpinnedTabAreaTop */
        /*  */
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaTop:Dock = System.Windows.Forms.DockStyle:Top.
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaTop:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaTop:Location = NEW System.Drawing.Point(11, 211).
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaTop:Name = "m_ProparseTreeViewFormUnpinnedTabAreaTop":U.
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaTop:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaTop:Size = NEW System.Drawing.Size(704, 0).
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaTop:TabIndex = 9.
        /*  */
        /* m_ProparseTreeViewFormUnpinnedTabAreaBottom */
        /*  */
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaBottom:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaBottom:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaBottom:Location = NEW System.Drawing.Point(11, 430).
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaBottom:Name = "m_ProparseTreeViewFormUnpinnedTabAreaBottom":U.
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaBottom:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaBottom:Size = NEW System.Drawing.Size(704, 0).
        THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaBottom:TabIndex = 10.
        /*  */
        /* m_ProparseTreeViewFormAutoHideControl */
        /*  */
        THIS-OBJECT:m_ProparseTreeViewFormAutoHideControl:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_ProparseTreeViewFormAutoHideControl:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_ProparseTreeViewFormAutoHideControl:Name = "m_ProparseTreeViewFormAutoHideControl":U.
        THIS-OBJECT:m_ProparseTreeViewFormAutoHideControl:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_ProparseTreeViewFormAutoHideControl:Size = NEW System.Drawing.Size(0, 0).
        THIS-OBJECT:m_ProparseTreeViewFormAutoHideControl:TabIndex = 11.
        /*  */
        /* windowDockingArea1 */
        /*  */
        THIS-OBJECT:windowDockingArea1:Controls:Add(THIS-OBJECT:dockableWindow1).
        THIS-OBJECT:windowDockingArea1:Dock = System.Windows.Forms.DockStyle:Left.
        THIS-OBJECT:windowDockingArea1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:windowDockingArea1:Location = NEW System.Drawing.Point(11, 211).
        THIS-OBJECT:windowDockingArea1:Name = "windowDockingArea1":U.
        THIS-OBJECT:windowDockingArea1:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:windowDockingArea1:Size = NEW System.Drawing.Size(319, 219).
        THIS-OBJECT:windowDockingArea1:TabIndex = 12.
        /*  */
        /* dockableWindow1 */
        /*  */
        THIS-OBJECT:dockableWindow1:Controls:Add(THIS-OBJECT:proparseTreeViewControl1).
        THIS-OBJECT:dockableWindow1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:dockableWindow1:Name = "dockableWindow1":U.
        THIS-OBJECT:dockableWindow1:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:dockableWindow1:Size = NEW System.Drawing.Size(314, 219).
        THIS-OBJECT:dockableWindow1:TabIndex = 20.
        /*  */
        /* ultraTabbedMdiManager1 */
        /*  */
        THIS-OBJECT:ultraTabbedMdiManager1:AllowMaximize = TRUE.
        THIS-OBJECT:ultraTabbedMdiManager1:AllowNestedTabGroups = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraTabbedMdiManager1:MdiParent = THIS-OBJECT.
        THIS-OBJECT:ultraTabbedMdiManager1:ViewStyle = Infragistics.Win.UltraWinTabbedMdi.ViewStyle:Office2007.
        /*  */
        /* openFileDialog1 */
        /*  */
        THIS-OBJECT:openFileDialog1:DefaultExt = "p":U.
        THIS-OBJECT:openFileDialog1:Filter = "ABL Procedures|*.p|ABL Classes|*.cls|AppBuilder Source|*.w|All files|*.*":U.
        THIS-OBJECT:openFileDialog1:RestoreDirectory = TRUE.
        THIS-OBJECT:openFileDialog1:Title = "Open file for parsing":U.
        /*  */
        /* ultraStatusBar1 */
        /*  */
        THIS-OBJECT:ultraStatusBar1:Location = NEW System.Drawing.Point(0, 430).
        THIS-OBJECT:ultraStatusBar1:Name = "ultraStatusBar1":U.
        ultraStatusPanel1:Key = "status":U.
        ultraStatusPanel1:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Spring.
        ultraStatusPanel2:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel2:Style = Infragistics.Win.UltraWinStatusBar.PanelStyle:Date.
        ultraStatusPanel3:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel3:Style = Infragistics.Win.UltraWinStatusBar.PanelStyle:Time.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel EXTENT 3 NO-UNDO.
        arrayvar9[1] = ultraStatusPanel1.
        arrayvar9[2] = ultraStatusPanel2.
        arrayvar9[3] = ultraStatusPanel3.
        THIS-OBJECT:ultraStatusBar1:Panels:AddRange(arrayvar9).
        THIS-OBJECT:ultraStatusBar1:Size = NEW System.Drawing.Size(726, 22).
        THIS-OBJECT:ultraStatusBar1:TabIndex = 18.
        THIS-OBJECT:ultraStatusBar1:Text = "ultraStatusBar1":U.
        THIS-OBJECT:ultraStatusBar1:ViewStyle = Infragistics.Win.UltraWinStatusBar.ViewStyle:Office2007.
        /*  */
        /* ProparseTreeViewForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(726, 452).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProparseTreeViewFormAutoHideControl).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaBottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaTop).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaRight).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProparseTreeViewFormUnpinnedTabAreaLeft).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraStatusBar1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProparseTreeViewForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:IsMdiContainer = TRUE.
        THIS-OBJECT:Name = "ProparseTreeViewForm":U.
        THIS-OBJECT:Text = "Proparse Browser":U.
        THIS-OBJECT:WindowPositionRegistryKey = "ProparseTreeViewForm":U.
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
        Purpose: Initializes Proparse                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeProparse ():
    
        THIS-OBJECT:DisplayStatus ("Initializing Proparse environment.":U) . 
        
        Consultingwerk.Studio.Proparse.ProparseHelper:SetProparseEnvironment() . 

        THIS-OBJECT:DisplayStatus ("Processing database schema.":U) . 

        Consultingwerk.Studio.Proparse.ProparseHelper:ExportDatabaseSchema () .
        
        THIS-OBJECT:DisplayStatus ("Initializing Proparse.":U) . 
        
        Consultingwerk.Studio.Proparse.ProparseHelper:Initialize() .     

        THIS-OBJECT:DisplayStatus ("":U) . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosing event 
        Notes:   
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnFormClosing (e AS System.Windows.Forms.FormClosingEventArgs):
        
        DEFINE VARIABLE oChildForm AS System.Windows.Forms.Form NO-UNDO .
        
        SUPER:OnFormClosing (e) .

        IF NOT e:Cancel THEN DO:
            
            DO WHILE THIS-OBJECT:MdiChildren:Length > 0:

                oChildForm = CAST (THIS-OBJECT:MdiChildren:GetValue (0),
                                   System.Windows.Forms.Form) .  
    
                oChildForm:Close () .

                IF VALID-OBJECT (oChildForm) AND oChildForm:Visible THEN DO: 
                    e:Cancel = TRUE .
                    
                    RETURN .
                END.
            END.        

            oLruController:StoreLru () .          
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event 
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
        
        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
        
        SUPER:OnLoad (e) .
        
        oLruController:LoadLru() .
        
    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event                                                                         
        Notes:                                                                        
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):
        
        SUPER:OnShown (e) .
        
        PROCESS EVENTS .
        
        IF lFirstTime THEN 
            InitializeProparse () .
            
        FINALLY:
            ASSIGN lFirstTime = FALSE .        
        END FINALLY.
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Opens a file in the embedded ABL procedure editor                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OpenFile ():
        
        DEFINE VARIABLE oNode         AS JPNode                                                    NO-UNDO . 
        DEFINE VARIABLE cKey          AS CHARACTER                                                 NO-UNDO.
        DEFINE VARIABLE cFileName     AS CHARACTER                                                 NO-UNDO.
        DEFINE VARIABLE oEditorForm   AS Consultingwerk.Studio.ProcedureEditor.ProcedureEditorForm NO-UNDO . 

        DEFINE VARIABLE pw_Window     AS HANDLE                                                    NO-UNDO.
        DEFINE VARIABLE pw_Editor     AS HANDLE                                                    NO-UNDO.
        DEFINE VARIABLE p_Cursor_Line AS INTEGER                                                   NO-UNDO.
        
        ASSIGN cKey = proparseTreeViewControl1:GetCurrentNodeKey () .

        oNode = proparseTreeViewControl1:GetJPNode (INTEGER (cKey)) .

        ASSIGN cFileName = oNode:getFileName() . 
        
        IF oEditors:ContainsKey (cFileName) THEN DO:
            
            oEditorForm = CAST (oEditors[cFileName], 
                                Consultingwerk.Studio.ProcedureEditor.ProcedureEditorForm) NO-ERROR .
                                
            oEditorForm:Activate () .                                   
        END.

        IF NOT VALID-OBJECT (oEditorForm) THEN         
            RUN adecommx/_pwmain.p ("":U, 
                                    cFileName,
                                    "READ-ONLY":U, 
                                    OUTPUT oEditorForm) .   
        
        IF NOT oEditors:ContainsValue (oEditorForm) THEN DO:
            oEditors:Add (cFileName, oEditorForm) .
            oEditorForm:FormClosed:Subscribe (EditorFormClosedHandler) .
        END.
        
        RUN adecomm/_pwgetwh.p (oEditorForm:EmbeddedWindow, OUTPUT pw_Window).
        RUN adecomm/_pwgeteh.p (INPUT pw_Window , OUTPUT pw_Editor).
        
        ASSIGN p_Cursor_Line = oNode:getLine() . 

        IF VALID-HANDLE (pw_Editor) THEN DO: 
            IF pw_Editor:EMPTY THEN RETURN.
            /* If entered line is greater than number of lines in buffer, then
               move to EOF. */
            IF p_Cursor_Line > pw_Editor:NUM-LINES
                THEN pw_Editor:MOVE-TO-EOF().
            ELSE DO:
            /* If entered line 0, goto line 1. */
                IF ( p_Cursor_Line < 1 ) THEN ASSIGN p_Cursor_Line = 1.
        
                ASSIGN pw_Editor:CURSOR-LINE = p_Cursor_Line.
                
                IF oNode:getColumn () > 0 THEN 
                    pw_Editor:CURSOR-CHAR = oNode:getColumn () . 
            END.          
            
            APPLY "ENTRY":U TO pw_Editor .   
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Opens a file for parsing                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OpenFileForParsing ():
    
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .  
        
        WAIT-FOR openFileDialog1:ShowDialog () SET oDialogResult . 
        
        IF Progress.Util.EnumHelper:AreEqual (oDialogResult, 
                                              System.Windows.Forms.DialogResult:Ok) THEN . 
        ELSE 
            RETURN .
            
        THIS-OBJECT:OpenFileForParsing (openFileDialog1:FileName) .            
                                                          
    END METHOD .     
        
    /*------------------------------------------------------------------------------
        Purpose: Opens a file for parsing                                                                       
        Notes:                                                       
        @param pcFileName The name of the file of open
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OpenFileForParsing (pcFileName AS CHARACTER):
        
        DEFINE VARIABLE oParseUnit    AS ParseUnit                         NO-UNDO . 

        oLruController:AddItemToLru (pcFileName, pcFileName) .      

        THIS-OBJECT:DisplayStatus ("Initializing Proparse.":U) . 
        
        Consultingwerk.Studio.Proparse.ProparseHelper:Initialize() .

        THIS-OBJECT:DisplayStatus ("Parsing source code.":U) . 
        
        oParseUnit = Consultingwerk.Studio.Proparse.ProparseHelper:ParseFile (pcFileName) .

        THIS-OBJECT:DisplayStatus ("Populating treeview.":U) . 

        proparseTreeViewControl1:LoadTree (oParseUnit) .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .                
        END CATCH.

        FINALLY:
            THIS-OBJECT:DisplayStatus ("":U) . 
        END FINALLY.
    
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Parses the current clipboard value 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ParseFromClipboard ():

        DEFINE VARIABLE oParseUnit    AS ParseUnit                         NO-UNDO . 
        DEFINE VARIABLE lcText        AS CHARACTER                         NO-UNDO .
        
        IF NOT System.Windows.Forms.Clipboard:ContainsText (TextDataFormat:Text) THEN 
            UNDO, THROW NEW AppError ("The clipboard does not contain any text."{&TRAN}, 0) .

        ASSIGN lcText = System.Windows.Forms.Clipboard:GetText (TextDataFormat:Text) .
    
        THIS-OBJECT:DisplayStatus ("Initializing Proparse.":U) . 
        
        Consultingwerk.Studio.Proparse.ProparseHelper:Initialize() .

        THIS-OBJECT:DisplayStatus ("Parsing source code.":U) . 
        
        oParseUnit = ProparseHelper:ParseString (lcText) .

        THIS-OBJECT:DisplayStatus ("Populating treeview.":U) . 

        proparseTreeViewControl1:LoadTree (oParseUnit) .

        DELETE OBJECT oParseUnit . 

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .                
        END CATCH.

        FINALLY:
            THIS-OBJECT:DisplayStatus ("":U) . 
        END FINALLY.
        

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the ultraToolbarsManager1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraToolbarsManager1_ToolClick (sender AS System.Object, 
                                                         e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):
        
        CASE e:Tool:Key:
            WHEN "Open":U THEN 
                OpenFileForParsing () .
            WHEN "ParseFromClipboard":U THEN 
                ParseFromClipboard () .
            WHEN "OpenFile":U THEN 
                OpenFile () .
        END CASE .

        IF e:Tool:Key BEGINS "lru_":U THEN 
            OpenFileForParsing (UNBOX (e:Tool:SharedProps:RootTool:Tag)) .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .                
        END CATCH.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ProparseTreeViewForm class 
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ProparseTreeViewForm ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.
&ENDIF
END CLASS.