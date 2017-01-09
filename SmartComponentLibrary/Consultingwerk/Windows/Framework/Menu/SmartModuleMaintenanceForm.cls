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
    File        : SmartModuleMaintenanceForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu Oct 11 17:16:32 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.SmartWindowForm FROM PROPATH .
USING Consultingwerk.Windows.Framework.Menu.*             FROM PROPATH .  
USING Progress.Lang.*                                     FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Menu.SmartModuleMaintenanceForm 
    INHERITS SmartWindowForm: 
    
    DEFINE PRIVATE VARIABLE m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top    AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea                        NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right  AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea                        NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left   AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea                        NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea                        NO-UNDO.
    DEFINE PRIVATE VARIABLE components                                             AS System.ComponentModel.IContainer                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE ModuleBrowser                                          AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser                 NO-UNDO.
    DEFINE PRIVATE VARIABLE ModuleBindingSource                                    AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE ModuleAdapter                                          AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter       NO-UNDO.
    DEFINE PRIVATE VARIABLE smartModuleViewer1                                     AS Consultingwerk.Windows.Framework.Menu.SmartModuleViewer                        NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1                                AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController           NO-UNDO.
    DEFINE PRIVATE VARIABLE SmartModuleMaintenanceForm_Fill_Panel                  AS Infragistics.Win.Misc.UltraPanel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraSplitter1 AS Infragistics.Win.Misc.UltraSplitter NO-UNDO.

    CONSTRUCTOR PUBLIC SmartModuleMaintenanceForm ():
        
        SUPER ().
        InitializeComponent ().
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

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
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
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
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool36 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool36 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool37 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool37 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool38 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool38 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool39 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool39 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool40 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool40 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool41 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool41 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool42 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool42 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool43 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool43 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool44 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool44 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool45 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool45 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool46 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool46 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool47 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool47 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("eSmartModule":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ModuleGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ModuleName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ModuleDescription":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ProductGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ProductCode":U).
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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartModule":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Menu.SmartModuleMaintenanceForm":U).
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ModuleBrowser = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:ModuleBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:ModuleAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter(THIS-OBJECT:components).
        THIS-OBJECT:smartModuleViewer1 = NEW Consultingwerk.Windows.Framework.Menu.SmartModuleViewer().
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:ultraSplitter1 = NEW Infragistics.Win.Misc.UltraSplitter().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:ModuleBrowser, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ModuleBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ModuleAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartModuleViewer1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar0).
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:KeyTip = resources:GetString("smartToolbarController1.Ribbon.ApplicationMenu.KeyTip":U).
        buttonTool5:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar1[1] = buttonTool2.
        arrayvar1[2] = buttonTool3.
        arrayvar1[3] = buttonTool4.
        arrayvar1[4] = buttonTool5.
        arrayvar1[5] = buttonTool6.
        arrayvar1[6] = buttonTool7.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar1).
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolTipText = resources:GetString("smartToolbarController1.Ribbon.ApplicationMenu.ToolTipText":U).
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolTipTextFormatted = resources:GetString("smartToolbarController1.Ribbon.ApplicationMenu.ToolTipTextFormatted":U).
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolTipTitle = resources:GetString("smartToolbarController1.Ribbon.ApplicationMenu.ToolTipTitle":U).
        ribbonTab1:Caption = resources:GetString("ribbonTab1.Caption":U).
        ribbonGroup1:Caption = resources:GetString("ribbonGroup1.Caption":U).
        ribbonGroup1:KeyTip = resources:GetString("ribbonGroup1.KeyTip":U).
        buttonTool8:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool8:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool9:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool9:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool10:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool10:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool11:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool11:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar2[1] = buttonTool8.
        arrayvar2[2] = buttonTool9.
        arrayvar2[3] = buttonTool10.
        arrayvar2[4] = buttonTool11.
        ribbonGroup1:Tools:AddRange(arrayvar2).
        ribbonGroup2:Caption = resources:GetString("ribbonGroup2.Caption":U).
        ribbonGroup2:KeyTip = resources:GetString("ribbonGroup2.KeyTip":U).
        buttonTool12:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool12:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool13:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool13:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool14:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool14:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool15:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool16:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool17:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar3[1] = buttonTool12.
        arrayvar3[2] = buttonTool13.
        arrayvar3[3] = buttonTool14.
        arrayvar3[4] = buttonTool15.
        arrayvar3[5] = buttonTool16.
        arrayvar3[6] = buttonTool17.
        ribbonGroup2:Tools:AddRange(arrayvar3).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 2 NO-UNDO.
        arrayvar4[1] = ribbonGroup1.
        arrayvar4[2] = ribbonGroup2.
        ribbonTab1:Groups:AddRange(arrayvar4).
        ribbonTab1:KeyTip = resources:GetString("ribbonTab1.KeyTip":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar5[1] = ribbonTab1.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar5).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar6[1] = buttonTool20.
        arrayvar6[2] = buttonTool21.
        arrayvar6[3] = buttonTool22.
        THIS-OBJECT:smartToolbarController1:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar6).
        THIS-OBJECT:smartToolbarController1:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        THIS-OBJECT:smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        ultraToolbar1:KeyTip = resources:GetString("ultraToolbar1.KeyTip":U).
        buttonTool23:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar7[1] = buttonTool23.
        arrayvar7[2] = buttonTool24.
        arrayvar7[3] = buttonTool25.
        arrayvar7[4] = buttonTool26.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar7).
        ultraToolbar1:Text = resources:GetString("ultraToolbar1.Text":U).
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        ultraToolbar2:KeyTip = resources:GetString("ultraToolbar2.KeyTip":U).
        buttonTool27:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar8[1] = buttonTool27.
        arrayvar8[2] = buttonTool28.
        arrayvar8[3] = buttonTool29.
        arrayvar8[4] = buttonTool30.
        arrayvar8[5] = buttonTool31.
        arrayvar8[6] = buttonTool32.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar8).
        ultraToolbar2:Text = resources:GetString("ultraToolbar2.Text":U).
        ultraToolbar3:DockedColumn = 1.
        ultraToolbar3:DockedRow = 1.
        ultraToolbar3:KeyTip = resources:GetString("ultraToolbar3.KeyTip":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar9[1] = buttonTool33.
        arrayvar9[2] = buttonTool34.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar9).
        ultraToolbar3:Text = resources:GetString("ultraToolbar3.Text":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 3 NO-UNDO.
        arrayvar10[1] = ultraToolbar1.
        arrayvar10[2] = ultraToolbar2.
        arrayvar10[3] = ultraToolbar3.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar10).
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        appearance13:TextHAlignAsString = resources:GetString("appearance13.TextHAlignAsString":U).
        appearance13:TextVAlignAsString = resources:GetString("appearance13.TextVAlignAsString":U).
        buttonTool35:SharedPropsInternal:AppearancesSmall:Appearance = appearance13.
        buttonTool35:SharedPropsInternal:Caption = resources:GetString("resource.Caption2":U).
        buttonTool35:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption":U).
        buttonTool35:SharedPropsInternal:Enabled = FALSE.
        buttonTool35:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText":U).
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        appearance14:TextHAlignAsString = resources:GetString("appearance14.TextHAlignAsString":U).
        appearance14:TextVAlignAsString = resources:GetString("appearance14.TextVAlignAsString":U).
        buttonTool36:SharedPropsInternal:AppearancesSmall:Appearance = appearance14.
        buttonTool36:SharedPropsInternal:Caption = resources:GetString("resource.Caption3":U).
        buttonTool36:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption1":U).
        buttonTool36:SharedPropsInternal:Enabled = FALSE.
        buttonTool36:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText1":U).
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        appearance15:TextHAlignAsString = resources:GetString("appearance15.TextHAlignAsString":U).
        appearance15:TextVAlignAsString = resources:GetString("appearance15.TextVAlignAsString":U).
        buttonTool37:SharedPropsInternal:AppearancesSmall:Appearance = appearance15.
        buttonTool37:SharedPropsInternal:Caption = resources:GetString("resource.Caption4":U).
        buttonTool37:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption2":U).
        buttonTool37:SharedPropsInternal:Enabled = FALSE.
        buttonTool37:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText2":U).
        appearance16:Image = CAST(resources:GetObject("appearance16.Image":U), System.Object).
        appearance16:TextHAlignAsString = resources:GetString("appearance16.TextHAlignAsString":U).
        appearance16:TextVAlignAsString = resources:GetString("appearance16.TextVAlignAsString":U).
        buttonTool38:SharedPropsInternal:AppearancesSmall:Appearance = appearance16.
        buttonTool38:SharedPropsInternal:Caption = resources:GetString("resource.Caption5":U).
        buttonTool38:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption3":U).
        buttonTool38:SharedPropsInternal:Enabled = FALSE.
        buttonTool38:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText3":U).
        appearance17:Image = CAST(resources:GetObject("appearance17.Image":U), System.Object).
        appearance17:TextHAlignAsString = resources:GetString("appearance17.TextHAlignAsString":U).
        appearance17:TextVAlignAsString = resources:GetString("appearance17.TextVAlignAsString":U).
        buttonTool39:SharedPropsInternal:AppearancesSmall:Appearance = appearance17.
        buttonTool39:SharedPropsInternal:Caption = resources:GetString("resource.Caption6":U).
        buttonTool39:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption4":U).
        buttonTool39:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText4":U).
        appearance18:Image = CAST(resources:GetObject("appearance18.Image":U), System.Object).
        appearance18:TextHAlignAsString = resources:GetString("appearance18.TextHAlignAsString":U).
        appearance18:TextVAlignAsString = resources:GetString("appearance18.TextVAlignAsString":U).
        buttonTool40:SharedPropsInternal:AppearancesSmall:Appearance = appearance18.
        buttonTool40:SharedPropsInternal:Caption = resources:GetString("resource.Caption7":U).
        buttonTool40:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption5":U).
        buttonTool40:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText5":U).
        appearance19:Image = CAST(resources:GetObject("appearance19.Image":U), System.Object).
        appearance19:TextHAlignAsString = resources:GetString("appearance19.TextHAlignAsString":U).
        appearance19:TextVAlignAsString = resources:GetString("appearance19.TextVAlignAsString":U).
        buttonTool41:SharedPropsInternal:AppearancesSmall:Appearance = appearance19.
        buttonTool41:SharedPropsInternal:Caption = resources:GetString("resource.Caption8":U).
        buttonTool41:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption6":U).
        buttonTool41:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText6":U).
        appearance20:Image = CAST(resources:GetObject("appearance20.Image":U), System.Object).
        appearance20:TextHAlignAsString = resources:GetString("appearance20.TextHAlignAsString":U).
        appearance20:TextVAlignAsString = resources:GetString("appearance20.TextVAlignAsString":U).
        buttonTool42:SharedPropsInternal:AppearancesSmall:Appearance = appearance20.
        buttonTool42:SharedPropsInternal:Caption = resources:GetString("resource.Caption9":U).
        buttonTool42:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption7":U).
        buttonTool42:SharedPropsInternal:Enabled = FALSE.
        buttonTool42:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText7":U).
        appearance21:Image = CAST(resources:GetObject("appearance21.Image":U), System.Object).
        appearance21:TextHAlignAsString = resources:GetString("appearance21.TextHAlignAsString":U).
        appearance21:TextVAlignAsString = resources:GetString("appearance21.TextVAlignAsString":U).
        buttonTool43:SharedPropsInternal:AppearancesSmall:Appearance = appearance21.
        buttonTool43:SharedPropsInternal:Caption = resources:GetString("resource.Caption10":U).
        buttonTool43:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption8":U).
        buttonTool43:SharedPropsInternal:Enabled = FALSE.
        buttonTool43:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText8":U).
        appearance22:Image = CAST(resources:GetObject("appearance22.Image":U), System.Object).
        appearance22:TextHAlignAsString = resources:GetString("appearance22.TextHAlignAsString":U).
        appearance22:TextVAlignAsString = resources:GetString("appearance22.TextVAlignAsString":U).
        buttonTool44:SharedPropsInternal:AppearancesSmall:Appearance = appearance22.
        buttonTool44:SharedPropsInternal:Caption = resources:GetString("resource.Caption11":U).
        buttonTool44:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption9":U).
        buttonTool44:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText9":U).
        appearance23:Image = CAST(resources:GetObject("appearance23.Image":U), System.Object).
        appearance23:TextHAlignAsString = resources:GetString("appearance23.TextHAlignAsString":U).
        appearance23:TextVAlignAsString = resources:GetString("appearance23.TextVAlignAsString":U).
        buttonTool45:SharedPropsInternal:AppearancesSmall:Appearance = appearance23.
        buttonTool45:SharedPropsInternal:Caption = resources:GetString("resource.Caption12":U).
        buttonTool45:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance24:Image = CAST(resources:GetObject("appearance24.Image":U), System.Object).
        appearance24:TextHAlignAsString = resources:GetString("appearance24.TextHAlignAsString":U).
        appearance24:TextVAlignAsString = resources:GetString("appearance24.TextVAlignAsString":U).
        buttonTool46:SharedPropsInternal:AppearancesLarge:Appearance = appearance24.
        appearance25:Image = CAST(resources:GetObject("appearance25.Image":U), System.Object).
        appearance25:TextHAlignAsString = resources:GetString("appearance25.TextHAlignAsString":U).
        appearance25:TextVAlignAsString = resources:GetString("appearance25.TextVAlignAsString":U).
        buttonTool46:SharedPropsInternal:AppearancesSmall:Appearance = appearance25.
        buttonTool46:SharedPropsInternal:Caption = resources:GetString("resource.Caption13":U).
        buttonTool46:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption10":U).
        buttonTool46:SharedPropsInternal:Enabled = FALSE.
        buttonTool46:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText10":U).
        appearance26:Image = CAST(resources:GetObject("appearance26.Image":U), System.Object).
        appearance26:TextHAlignAsString = resources:GetString("appearance26.TextHAlignAsString":U).
        appearance26:TextVAlignAsString = resources:GetString("appearance26.TextVAlignAsString":U).
        buttonTool47:SharedPropsInternal:AppearancesLarge:Appearance = appearance26.
        appearance27:Image = CAST(resources:GetObject("appearance27.Image":U), System.Object).
        appearance27:TextHAlignAsString = resources:GetString("appearance27.TextHAlignAsString":U).
        appearance27:TextVAlignAsString = resources:GetString("appearance27.TextVAlignAsString":U).
        buttonTool47:SharedPropsInternal:AppearancesSmall:Appearance = appearance27.
        buttonTool47:SharedPropsInternal:Caption = resources:GetString("resource.Caption14":U).
        buttonTool47:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption11":U).
        buttonTool47:SharedPropsInternal:Enabled = FALSE.
        buttonTool47:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText11":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 13 NO-UNDO.
        arrayvar11[1] = buttonTool35.
        arrayvar11[2] = buttonTool36.
        arrayvar11[3] = buttonTool37.
        arrayvar11[4] = buttonTool38.
        arrayvar11[5] = buttonTool39.
        arrayvar11[6] = buttonTool40.
        arrayvar11[7] = buttonTool41.
        arrayvar11[8] = buttonTool42.
        arrayvar11[9] = buttonTool43.
        arrayvar11[10] = buttonTool44.
        arrayvar11[11] = buttonTool45.
        arrayvar11[12] = buttonTool46.
        arrayvar11[13] = buttonTool47.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar11).
        /*  */
        /* SmartModuleMaintenanceForm_Fill_Panel */
        /*  */
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:AccessibleDescription = resources:GetString("SmartModuleMaintenanceForm_Fill_Panel.AccessibleDescription":U).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:AccessibleName = resources:GetString("SmartModuleMaintenanceForm_Fill_Panel.AccessibleName":U).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:Anchor = CAST(resources:GetObject("SmartModuleMaintenanceForm_Fill_Panel.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:AutoSize = System.Convert:ToBoolean(resources:GetObject("SmartModuleMaintenanceForm_Fill_Panel.AutoSize":U)).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:BackgroundImageLayout = CAST(resources:GetObject("SmartModuleMaintenanceForm_Fill_Panel.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        /*  */
        /* SmartModuleMaintenanceForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:ClientArea:AccessibleDescription = resources:GetString("SmartModuleMaintenanceForm_Fill_Panel.ClientArea.AccessibleDescription":U).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:ClientArea:AccessibleName = resources:GetString("SmartModuleMaintenanceForm_Fill_Panel.ClientArea.AccessibleName":U).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:smartModuleViewer1).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraSplitter1).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ModuleBrowser).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:ClientArea:Font = CAST(resources:GetObject("SmartModuleMaintenanceForm_Fill_Panel.ClientArea.Font":U), System.Drawing.Font).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:Dock = CAST(resources:GetObject("SmartModuleMaintenanceForm_Fill_Panel.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:Font = CAST(resources:GetObject("SmartModuleMaintenanceForm_Fill_Panel.Font":U), System.Drawing.Font).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:ImeMode = CAST(resources:GetObject("SmartModuleMaintenanceForm_Fill_Panel.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:Location = CAST(resources:GetObject("SmartModuleMaintenanceForm_Fill_Panel.Location":U), System.Drawing.Point).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:MaximumSize = CAST(resources:GetObject("SmartModuleMaintenanceForm_Fill_Panel.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:Name = "SmartModuleMaintenanceForm_Fill_Panel":U.
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:RightToLeft = CAST(resources:GetObject("SmartModuleMaintenanceForm_Fill_Panel.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:Size = CAST(resources:GetObject("SmartModuleMaintenanceForm_Fill_Panel.Size":U), System.Drawing.Size).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:TabIndex = System.Convert:ToInt32(resources:GetObject("SmartModuleMaintenanceForm_Fill_Panel.TabIndex":U)).
        /*  */
        /* ModuleBrowser */
        /*  */
        THIS-OBJECT:ModuleBrowser:AccessibleDescription = resources:GetString("ModuleBrowser.AccessibleDescription":U).
        THIS-OBJECT:ModuleBrowser:AccessibleName = resources:GetString("ModuleBrowser.AccessibleName":U).
        THIS-OBJECT:ModuleBrowser:Anchor = CAST(resources:GetObject("ModuleBrowser.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ModuleBrowser:BackgroundImage = CAST(resources:GetObject("ModuleBrowser.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ModuleBrowser:BackgroundImageLayout = CAST(resources:GetObject("ModuleBrowser.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ModuleBrowser:DataSource = THIS-OBJECT:ModuleBindingSource.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        appearance1:TextHAlignAsString = resources:GetString("appearance1.TextHAlignAsString":U).
        appearance1:TextVAlignAsString = resources:GetString("appearance1.TextVAlignAsString":U).
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Appearance = appearance1.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ExtendLastColumn.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn1:Header:VisiblePosition = 0.
        ultraGridColumn1:Hidden = TRUE.
        ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn2:Header:Caption = resources:GetString("resource.Caption":U).
        ultraGridColumn2:Header:VisiblePosition = 1.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn3:Header:VisiblePosition = 2.
        ultraGridColumn3:Hidden = TRUE.
        ultraGridColumn4:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn4:Header:Caption = resources:GetString("resource.Caption1":U).
        ultraGridColumn4:Header:VisiblePosition = 3.
        ultraGridColumn5:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn5:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn5:Header:VisiblePosition = 4.
        ultraGridColumn5:Hidden = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS System.Object EXTENT 5 NO-UNDO.
        arrayvar12[1] = ultraGridColumn1.
        arrayvar12[2] = ultraGridColumn2.
        arrayvar12[3] = ultraGridColumn3.
        arrayvar12[4] = ultraGridColumn4.
        arrayvar12[5] = ultraGridColumn5.
        ultraGridBand1:Columns:AddRange(arrayvar12).
        ultraGridBand1:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand1:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:ModuleBrowser:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance2:BorderColor = System.Drawing.SystemColors:Window.
        appearance2:TextHAlignAsString = resources:GetString("appearance2.TextHAlignAsString":U).
        appearance2:TextVAlignAsString = resources:GetString("appearance2.TextVAlignAsString":U).
        THIS-OBJECT:ModuleBrowser:DisplayLayout:GroupByBox:Appearance = appearance2.
        appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
        appearance3:TextHAlignAsString = resources:GetString("appearance3.TextHAlignAsString":U).
        appearance3:TextVAlignAsString = resources:GetString("appearance3.TextVAlignAsString":U).
        THIS-OBJECT:ModuleBrowser:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance4:BackColor2 = System.Drawing.SystemColors:Control.
        appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
        appearance4:TextHAlignAsString = resources:GetString("appearance4.TextHAlignAsString":U).
        appearance4:TextVAlignAsString = resources:GetString("appearance4.TextVAlignAsString":U).
        THIS-OBJECT:ModuleBrowser:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:MaxRowScrollRegions = 1.
        appearance5:BackColor = System.Drawing.SystemColors:Window.
        appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
        appearance5:TextHAlignAsString = resources:GetString("appearance5.TextHAlignAsString":U).
        appearance5:TextVAlignAsString = resources:GetString("appearance5.TextVAlignAsString":U).
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:ActiveCellAppearance = appearance5.
        appearance6:BackColor = System.Drawing.SystemColors:Highlight.
        appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
        appearance6:TextHAlignAsString = resources:GetString("appearance6.TextHAlignAsString":U).
        appearance6:TextVAlignAsString = resources:GetString("appearance6.TextVAlignAsString":U).
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:ActiveRowAppearance = appearance6.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance7:BackColor = System.Drawing.SystemColors:Window.
        appearance7:TextHAlignAsString = resources:GetString("appearance7.TextHAlignAsString":U).
        appearance7:TextVAlignAsString = resources:GetString("appearance7.TextVAlignAsString":U).
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:CardAreaAppearance = appearance7.
        appearance8:BorderColor = System.Drawing.Color:Silver.
        appearance8:TextHAlignAsString = resources:GetString("appearance8.TextHAlignAsString":U).
        appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        appearance8:TextVAlignAsString = resources:GetString("appearance8.TextVAlignAsString":U).
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:CellAppearance = appearance8.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:CellPadding = 0.
        appearance9:BackColor = System.Drawing.SystemColors:Control.
        appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance9:BorderColor = System.Drawing.SystemColors:Window.
        appearance9:TextHAlignAsString = resources:GetString("appearance9.TextHAlignAsString":U).
        appearance9:TextVAlignAsString = resources:GetString("appearance9.TextVAlignAsString":U).
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:GroupByRowAppearance = appearance9.
        appearance10:TextHAlignAsString = resources:GetString("appearance10.TextHAlignAsString":U).
        appearance10:TextVAlignAsString = resources:GetString("appearance10.TextVAlignAsString":U).
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:HeaderAppearance = appearance10.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance11:BackColor = System.Drawing.SystemColors:Window.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        appearance11:TextHAlignAsString = resources:GetString("appearance11.TextHAlignAsString":U).
        appearance11:TextVAlignAsString = resources:GetString("appearance11.TextVAlignAsString":U).
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:RowAppearance = appearance11.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        appearance12:TextHAlignAsString = resources:GetString("appearance12.TextHAlignAsString":U).
        appearance12:TextVAlignAsString = resources:GetString("appearance12.TextVAlignAsString":U).
        THIS-OBJECT:ModuleBrowser:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:ModuleBrowser:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:ModuleBrowser:Dock = CAST(resources:GetObject("ModuleBrowser.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ModuleBrowser:FetchAllRecordsOnGroupBy = TRUE.
        THIS-OBJECT:ModuleBrowser:Font = CAST(resources:GetObject("ModuleBrowser.Font":U), System.Drawing.Font).
        THIS-OBJECT:ModuleBrowser:ImeMode = CAST(resources:GetObject("ModuleBrowser.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ModuleBrowser:LinkDataSource = THIS-OBJECT:ModuleAdapter.
        THIS-OBJECT:ModuleBrowser:Location = CAST(resources:GetObject("ModuleBrowser.Location":U), System.Drawing.Point).
        THIS-OBJECT:ModuleBrowser:MaximumSize = CAST(resources:GetObject("ModuleBrowser.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ModuleBrowser:Name = "ModuleBrowser":U.
        THIS-OBJECT:ModuleBrowser:RightToLeft = CAST(resources:GetObject("ModuleBrowser.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ModuleBrowser:SettingsKey = resources:GetString("ModuleBrowser.SettingsKey":U).
        THIS-OBJECT:ModuleBrowser:Size = CAST(resources:GetObject("ModuleBrowser.Size":U), System.Drawing.Size).
        THIS-OBJECT:ModuleBrowser:SmartFilterActive = TRUE.
        THIS-OBJECT:ModuleBrowser:TabIndex = System.Convert:ToInt32(resources:GetObject("ModuleBrowser.TabIndex":U)).
        THIS-OBJECT:ModuleBrowser:Text = resources:GetString("ModuleBrowser.Text":U).
        THIS-OBJECT:ModuleBrowser:VisibleGridColumns = resources:GetString("ModuleBrowser.VisibleGridColumns":U).
        /*  */
        /* ModuleBindingSource */
        /*  */
        THIS-OBJECT:ModuleBindingSource:AddFields = FALSE.
        THIS-OBJECT:ModuleBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:ModuleBindingSource:ClearSchema = FALSE.
        THIS-OBJECT:ModuleBindingSource:EntityName = "Consultingwerk.SmartFramework.Menu.ModuleBusinessEntity":U.
        THIS-OBJECT:ModuleBindingSource:EntityTable = "eSmartModule":U.
        THIS-OBJECT:ModuleBindingSource:ImportSchema = FALSE.
        THIS-OBJECT:ModuleBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:ModuleBindingSource:NoLOBs = FALSE.
        THIS-OBJECT:ModuleBindingSource:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar13 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar13.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar14[1] = NEW Progress.Data.ColumnPropDesc("ModuleGuid":U, "ModuleGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar14[2] = NEW Progress.Data.ColumnPropDesc("ModuleName":U, "ModuleName":U, Progress.Data.DataType:CHARACTER).
        arrayvar14[3] = NEW Progress.Data.ColumnPropDesc("ModuleDescription":U, "ModuleDescription":U, Progress.Data.DataType:CHARACTER).
        arrayvar14[4] = NEW Progress.Data.ColumnPropDesc("ProductGuid":U, "ProductGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar14[5] = NEW Progress.Data.ColumnPropDesc("ProductCode":U, "Code":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar14.
        THIS-OBJECT:ModuleBindingSource:TableSchema = tableDesc1.
        /*  */
        /* ModuleAdapter */
        /*  */
        THIS-OBJECT:ModuleAdapter:BatchSize = 0.
        THIS-OBJECT:ModuleAdapter:EntityName = "Consultingwerk.SmartFramework.Menu.ModuleBusinessEntity":U.
        THIS-OBJECT:ModuleAdapter:EntityTable = "eSmartModule":U.
        THIS-OBJECT:ModuleAdapter:ForeignFields = "":U.
        THIS-OBJECT:ModuleAdapter:LinkCommitSource = ?.
        THIS-OBJECT:ModuleAdapter:LinkDataSource = ?.
        THIS-OBJECT:ModuleAdapter:LinkNavigationSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:ModuleAdapter:NonUpdatableColumns = "":U.
        THIS-OBJECT:ModuleAdapter:QuerySort = "":U.
        THIS-OBJECT:ModuleAdapter:ThrowErrorsFromRetrieveData = FALSE.
        /*  */
        /* smartModuleViewer1 */
        /*  */
        THIS-OBJECT:smartModuleViewer1:AccessibleDescription = resources:GetString("smartModuleViewer1.AccessibleDescription":U).
        THIS-OBJECT:smartModuleViewer1:AccessibleName = resources:GetString("smartModuleViewer1.AccessibleName":U).
        THIS-OBJECT:smartModuleViewer1:Anchor = CAST(resources:GetObject("smartModuleViewer1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartModuleViewer1:AutoScroll = System.Convert:ToBoolean(resources:GetObject("smartModuleViewer1.AutoScroll":U)).
        THIS-OBJECT:smartModuleViewer1:AutoSize = System.Convert:ToBoolean(resources:GetObject("smartModuleViewer1.AutoSize":U)).
        THIS-OBJECT:smartModuleViewer1:AutoSizeMode = CAST(resources:GetObject("smartModuleViewer1.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:smartModuleViewer1:BackgroundImage = CAST(resources:GetObject("smartModuleViewer1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:smartModuleViewer1:BackgroundImageLayout = CAST(resources:GetObject("smartModuleViewer1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:smartModuleViewer1:Dock = CAST(resources:GetObject("smartModuleViewer1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:smartModuleViewer1:FocusControlOnAdd = ?.
        THIS-OBJECT:smartModuleViewer1:FocusControlOnUpdate = ?.
        THIS-OBJECT:smartModuleViewer1:Font = CAST(resources:GetObject("smartModuleViewer1.Font":U), System.Drawing.Font).
        THIS-OBJECT:smartModuleViewer1:ImeMode = CAST(resources:GetObject("smartModuleViewer1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:smartModuleViewer1:LinkDataSource = THIS-OBJECT:ModuleAdapter.
        THIS-OBJECT:smartModuleViewer1:LinkGroupAssignSource = ?.
        THIS-OBJECT:smartModuleViewer1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartModuleViewer1:Location = CAST(resources:GetObject("smartModuleViewer1.Location":U), System.Drawing.Point).
        THIS-OBJECT:smartModuleViewer1:MaximumSize = CAST(resources:GetObject("smartModuleViewer1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:smartModuleViewer1:Name = "smartModuleViewer1":U.
        THIS-OBJECT:smartModuleViewer1:RightToLeft = CAST(resources:GetObject("smartModuleViewer1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:smartModuleViewer1:SecurityKey = resources:GetString("smartModuleViewer1.SecurityKey":U).
        THIS-OBJECT:smartModuleViewer1:Size = CAST(resources:GetObject("smartModuleViewer1.Size":U), System.Drawing.Size).
        THIS-OBJECT:smartModuleViewer1:SmartTableIOState = resources:GetString("smartModuleViewer1.SmartTableIOState":U).
        THIS-OBJECT:smartModuleViewer1:TabIndex = System.Convert:ToInt32(resources:GetObject("smartModuleViewer1.TabIndex":U)).
        THIS-OBJECT:smartModuleViewer1:ViewerLogicClass = resources:GetString("smartModuleViewer1.ViewerLogicClass":U).
        /*  */
        /* m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:AccessibleDescription = resources:GetString("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left.AccessibleDescription":U).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:AccessibleName = resources:GetString("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left.AccessibleName":U).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:Anchor = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:BackgroundImage = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:BackgroundImageLayout = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:ImeMode = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:Location = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left.Location":U), System.Drawing.Point).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:MaximumSize = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:Name = "m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:RightToLeft = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:Size = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left.Size":U), System.Drawing.Size).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:AccessibleDescription = resources:GetString("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right.AccessibleDescription":U).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:AccessibleName = resources:GetString("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right.AccessibleName":U).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:Anchor = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:BackgroundImage = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:BackgroundImageLayout = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:ImeMode = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:Location = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right.Location":U), System.Drawing.Point).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:MaximumSize = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:Name = "m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:RightToLeft = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:Size = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right.Size":U), System.Drawing.Size).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top:AccessibleDescription = resources:GetString("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top.AccessibleDescription":U).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top:AccessibleName = resources:GetString("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top.AccessibleName":U).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top:Anchor = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top:BackgroundImage = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top:BackgroundImageLayout = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top:ImeMode = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top:Location = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top.Location":U), System.Drawing.Point).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top:MaximumSize = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top:Name = "m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top:RightToLeft = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top:Size = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top.Size":U), System.Drawing.Size).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:AccessibleDescription = resources:GetString("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom.AccessibleDescription":U).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:AccessibleName = resources:GetString("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom.AccessibleName":U).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:Anchor = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:BackgroundImage = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:BackgroundImageLayout = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:ImeMode = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:Location = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom.Location":U), System.Drawing.Point).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:MaximumSize = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:Name = "m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:RightToLeft = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:Size = CAST(resources:GetObject("m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom.Size":U), System.Drawing.Size).
        THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* ultraSplitter1 */
        /*  */
        THIS-OBJECT:ultraSplitter1:AccessibleDescription = resources:GetString("ultraSplitter1.AccessibleDescription":U).
        THIS-OBJECT:ultraSplitter1:AccessibleName = resources:GetString("ultraSplitter1.AccessibleName":U).
        THIS-OBJECT:ultraSplitter1:Anchor = CAST(resources:GetObject("ultraSplitter1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraSplitter1:BackgroundImage = CAST(resources:GetObject("ultraSplitter1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraSplitter1:BackgroundImageLayout = CAST(resources:GetObject("ultraSplitter1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraSplitter1:Dock = CAST(resources:GetObject("ultraSplitter1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraSplitter1:Font = CAST(resources:GetObject("ultraSplitter1.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraSplitter1:ImeMode = CAST(resources:GetObject("ultraSplitter1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraSplitter1:Location = CAST(resources:GetObject("ultraSplitter1.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraSplitter1:MaximumSize = CAST(resources:GetObject("ultraSplitter1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraSplitter1:Name = "ultraSplitter1":U.
        THIS-OBJECT:ultraSplitter1:RestoreExtent = 0.
        THIS-OBJECT:ultraSplitter1:RightToLeft = CAST(resources:GetObject("ultraSplitter1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraSplitter1:Size = CAST(resources:GetObject("ultraSplitter1.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraSplitter1:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraSplitter1.TabIndex":U)).
        /*  */
        /* SmartModuleMaintenanceForm */
        /*  */
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ClientSize = CAST(resources:GetObject("$this.ClientSize":U), System.Drawing.Size).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartModuleMaintenanceForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:Name = "SmartModuleMaintenanceForm":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:RightToLeftLayout = System.Convert:ToBoolean(resources:GetObject("$this.RightToLeftLayout":U)).
        THIS-OBJECT:StartPosition = CAST(resources:GetObject("$this.StartPosition":U), System.Windows.Forms.FormStartPosition).
        THIS-OBJECT:Text = resources:GetString("$this.Text":U).
        THIS-OBJECT:WindowPositionRegistryKey = resources:GetString("$this.WindowPositionRegistryKey":U).
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:SmartModuleMaintenanceForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ModuleBrowser, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ModuleBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ModuleAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartModuleViewer1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:   
        @param e The System.EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnLoad (e AS System.EventArgs):

        DEFINE VARIABLE oValueList AS Infragistics.Win.ValueList           NO-UNDO . 
        DEFINE VARIABLE hDataset   AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE oRequest   AS Consultingwerk.OERA.FetchDataRequest NO-UNDO .

        SUPER:OnLoad (e) .

        oRequest = NEW Consultingwerk.OERA.FetchDataRequest ("*":U) .
        
        Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:RetrieveData ("":U,
                                                                                "Consultingwerk.SmartFramework.Menu.ProductBusinessEntity":U,
                                                                                oRequest,
                                                                                OUTPUT DATASET-HANDLE hDataset) . 

        IF VALID-HANDLE (hDataset) THEN DO:
            oValueList = Consultingwerk.Util.ValueListHelper:FromTempTable (hDataset::eSmartProduct:TABLE-HANDLE,
                                                                            "ProductGuid":U,
                                                                            "ProductCode":U,
                                                                            "&1":U,
                                                                            "":U,
                                                                            "None":U) .
            
            ModuleBrowser:DisplayLayout:Bands[0]:Columns["ProductGuid":U]:ValueList = oValueList . 
        END.        
        
        FINALLY:
            IF VALID-HANDLE (hDataset) THEN 
                DELETE OBJECT hDataset	 .	
        END FINALLY.
        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Override of the OnShown Method to first show the form and then 
                 initiate reading data.
        Notes:   
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnShown (e AS System.EventArgs):
        
        SUPER:OnShown (e).

        Consultingwerk.Util.UserInterfaceHelper:ProcessEvents() .

        ModuleAdapter:RetrieveData ().
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartModuleMaintenanceForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartModuleMaintenanceForm ():

    END DESTRUCTOR .

END CLASS.
