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
    File        : SmartFunctionMaintenanceForm
    Purpose     : Maintenance of Menu Functions
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu Oct 11 17:49:14 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Menu.*    FROM PROPATH . 
USING Consultingwerk.SmartComponents.Base.*   FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*   FROM PROPATH . 
USING Consultingwerk.Util.*                   FROM PROPATH .  
USING Consultingwerk.Windows.Framework.Menu.* FROM PROPATH .  
USING Progress.Lang.*                         FROM PROPATH .
USING System.*                                FROM ASSEMBLY .
USING System.Collections.Generic.*            FROM ASSEMBLY .
USING System.Windows.Forms.*                  FROM ASSEMBLY . 

CLASS Consultingwerk.Windows.Framework.Menu.SmartFunctionMaintenanceForm 
    INHERITS SmartWindowForm
    IMPLEMENTS Consultingwerk.SmartFramework.IReceiveMenuContext: 
    
    DEFINE PRIVATE VARIABLE FunctionBindingSource                                    AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE FunctionAdapter                                          AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter       NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Top    AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea                        NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Right  AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea                        NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Left   AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea                        NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea                        NO-UNDO.
    DEFINE PRIVATE VARIABLE components                                               AS System.ComponentModel.IContainer                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDataBrowser1                                        AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser                 NO-UNDO.
    DEFINE PRIVATE VARIABLE smartFunctionViewer1                                     AS Consultingwerk.Windows.Framework.Menu.SmartFunctionViewer                      NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1                                  AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController           NO-UNDO.
    DEFINE PRIVATE VARIABLE SmartFunctionMaintenanceForm_Fill_Panel                  AS Infragistics.Win.Misc.UltraPanel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraSplitter1 AS Infragistics.Win.Misc.UltraSplitter NO-UNDO.

    CONSTRUCTOR PUBLIC SmartFunctionMaintenanceForm ():
        
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
        DEFINE VARIABLE buttonTool48 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool48 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool49 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool49 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool50 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool50 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool51 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool51 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool52 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool52 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool53 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool53 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool54 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool54 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("start":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("navigation":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool55 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool55 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool56 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool56 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool57 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool57 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool58 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool58 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("maintenance":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool59 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool59 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool60 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool60 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool61 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool61 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool62 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool62 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool63 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool63 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool64 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool64 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool67 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool67 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool68 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool68 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool69 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool69 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("NavigationToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool70 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool70 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool71 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool71 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool72 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool72 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool73 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool73 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TableIOToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool74 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool74 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool75 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool75 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool76 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool76 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool77 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool77 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool78 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool78 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool79 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool79 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar3 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar3 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TransactionToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool80 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool80 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool81 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool81 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool82 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool82 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool83 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool83 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool84 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool84 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool85 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool85 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool86 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool86 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool87 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool87 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool88 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool88 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool89 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool89 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool90 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool90 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool91 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool91 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool92 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool92 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool93 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool93 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool94 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool94 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
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
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("eSmartFunction":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FunctionGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FunctionName":U, -1, ?, 0, Infragistics.Win.UltraWinGrid.SortIndicator:Ascending, FALSE).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FunctionDescription":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FunctionSmallImage":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn6 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn6 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FunctionLargeImage":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FunctionCallParameter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn7 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn7 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FunctionModuleGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn8 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn8 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ModuleName":U).
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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartFunction":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Menu.SmartFunctionMaintenanceForm":U).
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:SmartFunctionMaintenanceForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:smartFunctionViewer1 = NEW Consultingwerk.Windows.Framework.Menu.SmartFunctionViewer().
        THIS-OBJECT:FunctionAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter(THIS-OBJECT:components).
        THIS-OBJECT:ultraSplitter1 = NEW Infragistics.Win.Misc.UltraSplitter().
        THIS-OBJECT:smartDataBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:FunctionBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SmartFunctionMaintenanceForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:SmartFunctionMaintenanceForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:smartFunctionViewer1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:FunctionAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:FunctionBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar0[1] = buttonTool48.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar0).
        buttonTool52:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar1[1] = buttonTool49.
        arrayvar1[2] = buttonTool50.
        arrayvar1[3] = buttonTool51.
        arrayvar1[4] = buttonTool52.
        arrayvar1[5] = buttonTool53.
        arrayvar1[6] = buttonTool54.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar1).
        ribbonTab1:Caption = "Start":U.
        ribbonGroup1:Caption = "Navigation":U.
        buttonTool55:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool55:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool56:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool56:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool57:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool57:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool58:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool58:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar2[1] = buttonTool55.
        arrayvar2[2] = buttonTool56.
        arrayvar2[3] = buttonTool57.
        arrayvar2[4] = buttonTool58.
        ribbonGroup1:Tools:AddRange(arrayvar2).
        ribbonGroup2:Caption = "Maintenance":U.
        buttonTool59:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool59:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool60:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool60:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool61:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool61:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool62:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool63:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool64:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar3[1] = buttonTool59.
        arrayvar3[2] = buttonTool60.
        arrayvar3[3] = buttonTool61.
        arrayvar3[4] = buttonTool62.
        arrayvar3[5] = buttonTool63.
        arrayvar3[6] = buttonTool64.
        ribbonGroup2:Tools:AddRange(arrayvar3).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 2 NO-UNDO.
        arrayvar4[1] = ribbonGroup1.
        arrayvar4[2] = ribbonGroup2.
        ribbonTab1:Groups:AddRange(arrayvar4).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar5[1] = ribbonTab1.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar5).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar6[1] = buttonTool67.
        arrayvar6[2] = buttonTool68.
        arrayvar6[3] = buttonTool69.
        THIS-OBJECT:smartToolbarController1:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar6).
        THIS-OBJECT:smartToolbarController1:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        THIS-OBJECT:smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        buttonTool70:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar7[1] = buttonTool70.
        arrayvar7[2] = buttonTool71.
        arrayvar7[3] = buttonTool72.
        arrayvar7[4] = buttonTool73.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar7).
        ultraToolbar1:Text = "Navigation Toolbar":U.
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        buttonTool74:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar8[1] = buttonTool74.
        arrayvar8[2] = buttonTool75.
        arrayvar8[3] = buttonTool76.
        arrayvar8[4] = buttonTool77.
        arrayvar8[5] = buttonTool78.
        arrayvar8[6] = buttonTool79.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar8).
        ultraToolbar2:Text = "TableIO Toolbar":U.
        ultraToolbar3:DockedColumn = 1.
        ultraToolbar3:DockedRow = 1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar9[1] = buttonTool80.
        arrayvar9[2] = buttonTool81.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar9).
        ultraToolbar3:Text = "Transaction Toolbar":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 3 NO-UNDO.
        arrayvar10[1] = ultraToolbar1.
        arrayvar10[2] = ultraToolbar2.
        arrayvar10[3] = ultraToolbar3.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar10).
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        buttonTool82:SharedPropsInternal:AppearancesSmall:Appearance = appearance13.
        buttonTool82:SharedPropsInternal:Caption = "First record":U.
        buttonTool82:SharedPropsInternal:CustomizerCaption = "First":U.
        buttonTool82:SharedPropsInternal:Enabled = FALSE.
        buttonTool82:SharedPropsInternal:ToolTipText = "First record":U.
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        buttonTool83:SharedPropsInternal:AppearancesSmall:Appearance = appearance14.
        buttonTool83:SharedPropsInternal:Caption = "Previous record":U.
        buttonTool83:SharedPropsInternal:CustomizerCaption = "Previous":U.
        buttonTool83:SharedPropsInternal:Enabled = FALSE.
        buttonTool83:SharedPropsInternal:ToolTipText = "Previous record":U.
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        buttonTool84:SharedPropsInternal:AppearancesSmall:Appearance = appearance15.
        buttonTool84:SharedPropsInternal:Caption = "Next record":U.
        buttonTool84:SharedPropsInternal:CustomizerCaption = "Next":U.
        buttonTool84:SharedPropsInternal:Enabled = FALSE.
        buttonTool84:SharedPropsInternal:ToolTipText = "Next record":U.
        appearance16:Image = CAST(resources:GetObject("appearance16.Image":U), System.Object).
        buttonTool85:SharedPropsInternal:AppearancesSmall:Appearance = appearance16.
        buttonTool85:SharedPropsInternal:Caption = "Last record":U.
        buttonTool85:SharedPropsInternal:CustomizerCaption = "Last":U.
        buttonTool85:SharedPropsInternal:Enabled = FALSE.
        buttonTool85:SharedPropsInternal:ToolTipText = "Last record":U.
        appearance17:Image = CAST(resources:GetObject("appearance17.Image":U), System.Object).
        buttonTool86:SharedPropsInternal:AppearancesSmall:Appearance = appearance17.
        buttonTool86:SharedPropsInternal:Caption = "Add record":U.
        buttonTool86:SharedPropsInternal:CustomizerCaption = "Add":U.
        buttonTool86:SharedPropsInternal:ToolTipText = "Add record":U.
        appearance18:Image = CAST(resources:GetObject("appearance18.Image":U), System.Object).
        buttonTool87:SharedPropsInternal:AppearancesSmall:Appearance = appearance18.
        buttonTool87:SharedPropsInternal:Caption = "Copy record":U.
        buttonTool87:SharedPropsInternal:CustomizerCaption = "Copy":U.
        buttonTool87:SharedPropsInternal:ToolTipText = "Copy record":U.
        appearance19:Image = CAST(resources:GetObject("appearance19.Image":U), System.Object).
        buttonTool88:SharedPropsInternal:AppearancesSmall:Appearance = appearance19.
        buttonTool88:SharedPropsInternal:Caption = "Update record":U.
        buttonTool88:SharedPropsInternal:CustomizerCaption = "Update":U.
        buttonTool88:SharedPropsInternal:ToolTipText = "Update record":U.
        appearance20:Image = CAST(resources:GetObject("appearance20.Image":U), System.Object).
        buttonTool89:SharedPropsInternal:AppearancesSmall:Appearance = appearance20.
        buttonTool89:SharedPropsInternal:Caption = "Save changes":U.
        buttonTool89:SharedPropsInternal:CustomizerCaption = "Save":U.
        buttonTool89:SharedPropsInternal:Enabled = FALSE.
        buttonTool89:SharedPropsInternal:ToolTipText = "Save changes":U.
        appearance21:Image = CAST(resources:GetObject("appearance21.Image":U), System.Object).
        buttonTool90:SharedPropsInternal:AppearancesSmall:Appearance = appearance21.
        buttonTool90:SharedPropsInternal:Caption = "Cancel update":U.
        buttonTool90:SharedPropsInternal:CustomizerCaption = "Cancel":U.
        buttonTool90:SharedPropsInternal:Enabled = FALSE.
        buttonTool90:SharedPropsInternal:ToolTipText = "Cancel update":U.
        appearance22:Image = CAST(resources:GetObject("appearance22.Image":U), System.Object).
        buttonTool91:SharedPropsInternal:AppearancesSmall:Appearance = appearance22.
        buttonTool91:SharedPropsInternal:Caption = "Delete record":U.
        buttonTool91:SharedPropsInternal:CustomizerCaption = "Delete":U.
        buttonTool91:SharedPropsInternal:ToolTipText = "Delete record":U.
        appearance23:Image = CAST(resources:GetObject("appearance23.Image":U), System.Object).
        buttonTool92:SharedPropsInternal:AppearancesSmall:Appearance = appearance23.
        buttonTool92:SharedPropsInternal:Caption = "&Exit":U.
        buttonTool92:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance24:Image = CAST(resources:GetObject("appearance24.Image":U), System.Object).
        buttonTool93:SharedPropsInternal:AppearancesLarge:Appearance = appearance24.
        appearance25:Image = CAST(resources:GetObject("appearance25.Image":U), System.Object).
        buttonTool93:SharedPropsInternal:AppearancesSmall:Appearance = appearance25.
        buttonTool93:SharedPropsInternal:Caption = "Commit":U.
        buttonTool93:SharedPropsInternal:CustomizerCaption = "Commit":U.
        buttonTool93:SharedPropsInternal:Enabled = FALSE.
        buttonTool93:SharedPropsInternal:ToolTipText = "Commit transaction":U.
        appearance26:Image = CAST(resources:GetObject("appearance26.Image":U), System.Object).
        buttonTool94:SharedPropsInternal:AppearancesLarge:Appearance = appearance26.
        appearance27:Image = CAST(resources:GetObject("appearance27.Image":U), System.Object).
        buttonTool94:SharedPropsInternal:AppearancesSmall:Appearance = appearance27.
        buttonTool94:SharedPropsInternal:Caption = "Revert":U.
        buttonTool94:SharedPropsInternal:CustomizerCaption = "Revert":U.
        buttonTool94:SharedPropsInternal:Enabled = FALSE.
        buttonTool94:SharedPropsInternal:ToolTipText = "Revert transaction":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 13 NO-UNDO.
        arrayvar11[1] = buttonTool82.
        arrayvar11[2] = buttonTool83.
        arrayvar11[3] = buttonTool84.
        arrayvar11[4] = buttonTool85.
        arrayvar11[5] = buttonTool86.
        arrayvar11[6] = buttonTool87.
        arrayvar11[7] = buttonTool88.
        arrayvar11[8] = buttonTool89.
        arrayvar11[9] = buttonTool90.
        arrayvar11[10] = buttonTool91.
        arrayvar11[11] = buttonTool92.
        arrayvar11[12] = buttonTool93.
        arrayvar11[13] = buttonTool94.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar11).
        /*  */
        /* SmartFunctionMaintenanceForm_Fill_Panel */
        /*  */
        /*  */
        /* SmartFunctionMaintenanceForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:SmartFunctionMaintenanceForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:smartFunctionViewer1).
        THIS-OBJECT:SmartFunctionMaintenanceForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraSplitter1).
        THIS-OBJECT:SmartFunctionMaintenanceForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:smartDataBrowser1).
        THIS-OBJECT:SmartFunctionMaintenanceForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:SmartFunctionMaintenanceForm_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:SmartFunctionMaintenanceForm_Fill_Panel:Location = NEW System.Drawing.Point(8, 151).
        THIS-OBJECT:SmartFunctionMaintenanceForm_Fill_Panel:Name = "SmartFunctionMaintenanceForm_Fill_Panel":U.
        THIS-OBJECT:SmartFunctionMaintenanceForm_Fill_Panel:Size = NEW System.Drawing.Size(745, 398).
        THIS-OBJECT:SmartFunctionMaintenanceForm_Fill_Panel:TabIndex = 0.
        /*  */
        /* smartFunctionViewer1 */
        /*  */
        THIS-OBJECT:smartFunctionViewer1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:smartFunctionViewer1:FocusControlOnAdd = ?.
        THIS-OBJECT:smartFunctionViewer1:FocusControlOnUpdate = ?.
        THIS-OBJECT:smartFunctionViewer1:LinkDataSource = THIS-OBJECT:FunctionAdapter.
        THIS-OBJECT:smartFunctionViewer1:LinkGroupAssignSource = ?.
        THIS-OBJECT:smartFunctionViewer1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartFunctionViewer1:Location = NEW System.Drawing.Point(252, 0).
        THIS-OBJECT:smartFunctionViewer1:Margin = NEW System.Windows.Forms.Padding(5).
        THIS-OBJECT:smartFunctionViewer1:Name = "smartFunctionViewer1":U.
        THIS-OBJECT:smartFunctionViewer1:Size = NEW System.Drawing.Size(493, 398).
        THIS-OBJECT:smartFunctionViewer1:TabIndex = 0.
        /*  */
        /* FunctionAdapter */
        /*  */
        THIS-OBJECT:FunctionAdapter:DatasetControllerType = "Consultingwerk.SmartFramework.Menu.MenuDatasetController":U.
        THIS-OBJECT:FunctionAdapter:EntityName = "Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U.
        THIS-OBJECT:FunctionAdapter:EntityTable = "eSmartFunction":U.
        THIS-OBJECT:FunctionAdapter:LinkCommitSource = ?.
        THIS-OBJECT:FunctionAdapter:LinkDataSource = ?.
        THIS-OBJECT:FunctionAdapter:LinkNavigationSource = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* ultraSplitter1 */
        /*  */
        THIS-OBJECT:ultraSplitter1:Location = NEW System.Drawing.Point(246, 0).
        THIS-OBJECT:ultraSplitter1:Name = "ultraSplitter1":U.
        THIS-OBJECT:ultraSplitter1:RestoreExtent = 0.
        THIS-OBJECT:ultraSplitter1:Size = NEW System.Drawing.Size(6, 398).
        THIS-OBJECT:ultraSplitter1:TabIndex = 1.
        /*  */
        /* smartDataBrowser1 */
        /*  */
        THIS-OBJECT:smartDataBrowser1:DataSource = THIS-OBJECT:FunctionBindingSource.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Appearance = appearance1.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ExtendLastColumn.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn1:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn1:Header:Caption = "Function Guid":U.
        ultraGridColumn1:Header:VisiblePosition = 0.
        ultraGridColumn1:Hidden = TRUE.
        ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn2:Header:Caption = "Function Name":U.
        ultraGridColumn2:Header:VisiblePosition = 1.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn3:Header:Caption = "Function Description":U.
        ultraGridColumn3:Header:VisiblePosition = 2.
        ultraGridColumn5:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn5:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn5:Header:Caption = "Function Small Image":U.
        ultraGridColumn5:Header:VisiblePosition = 4.
        ultraGridColumn5:Hidden = TRUE.
        ultraGridColumn6:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn6:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn6:Header:Caption = "Function Large Image":U.
        ultraGridColumn6:Header:VisiblePosition = 5.
        ultraGridColumn6:Hidden = TRUE.
        ultraGridColumn4:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn4:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn4:Header:Caption = "Function Call Parameter":U.
        ultraGridColumn4:Header:VisiblePosition = 3.
        ultraGridColumn4:Hidden = TRUE.
        ultraGridColumn4:SortIndicator = Infragistics.Win.UltraWinGrid.SortIndicator:Disabled.
        ultraGridColumn7:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn7:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:False.
        ultraGridColumn7:Header:Caption = "Module Name":U.
        ultraGridColumn7:Header:VisiblePosition = 6.
        ultraGridColumn8:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn8:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn8:Header:Caption = "Module Name":U.
        ultraGridColumn8:Header:VisiblePosition = 7.
        ultraGridColumn8:Hidden = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS System.Object EXTENT 8 NO-UNDO.
        arrayvar12[1] = ultraGridColumn1.
        arrayvar12[2] = ultraGridColumn2.
        arrayvar12[3] = ultraGridColumn3.
        arrayvar12[4] = ultraGridColumn5.
        arrayvar12[5] = ultraGridColumn6.
        arrayvar12[6] = ultraGridColumn4.
        arrayvar12[7] = ultraGridColumn7.
        arrayvar12[8] = ultraGridColumn8.
        ultraGridBand1:Columns:AddRange(arrayvar12).
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
        appearance10:TextHAlignAsString = "Left":U.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderAppearance = appearance10.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance11:BackColor = System.Drawing.SystemColors:Window.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:RowAppearance = appearance11.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:smartDataBrowser1:Dock = System.Windows.Forms.DockStyle:Left.
        THIS-OBJECT:smartDataBrowser1:FetchAllRecordsOnGroupBy = TRUE.
        THIS-OBJECT:smartDataBrowser1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:smartDataBrowser1:LinkDataSource = THIS-OBJECT:FunctionAdapter.
        THIS-OBJECT:smartDataBrowser1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:smartDataBrowser1:Name = "smartDataBrowser1":U.
        THIS-OBJECT:smartDataBrowser1:SettingsKey = "98ce54b5-c31f-afac-e211-bc13d62974ec":U.
        THIS-OBJECT:smartDataBrowser1:Size = NEW System.Drawing.Size(246, 398).
        THIS-OBJECT:smartDataBrowser1:SmartFilterActive = TRUE.
        THIS-OBJECT:smartDataBrowser1:TabIndex = 0.
        THIS-OBJECT:smartDataBrowser1:Text = "smartDataBrowser1":U.
        THIS-OBJECT:smartDataBrowser1:MouseDown:Subscribe(THIS-OBJECT:smartDataBrowser1_MouseDown).
        /*  */
        /* FunctionBindingSource */
        /*  */
        THIS-OBJECT:FunctionBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:FunctionBindingSource:EntityName = "Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U.
        THIS-OBJECT:FunctionBindingSource:EntityTable = "eSmartFunction":U.
        THIS-OBJECT:FunctionBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:FunctionBindingSource:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS Progress.Data.ColumnPropDesc EXTENT 9 NO-UNDO.
        arrayvar13[1] = NEW Progress.Data.ColumnPropDesc("FunctionGuid":U, "FunctionGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar13[2] = NEW Progress.Data.ColumnPropDesc("FunctionName":U, "FunctionName":U, Progress.Data.DataType:CHARACTER).
        arrayvar13[3] = NEW Progress.Data.ColumnPropDesc("FunctionDescription":U, "FunctionDescription":U, Progress.Data.DataType:CHARACTER).
        arrayvar13[4] = NEW Progress.Data.ColumnPropDesc("FunctionSmallImage":U, "FunctionSmallImage":U, Progress.Data.DataType:CHARACTER).
        arrayvar13[5] = NEW Progress.Data.ColumnPropDesc("FunctionLargeImage":U, "FunctionLargeImage":U, Progress.Data.DataType:CHARACTER).
        arrayvar13[6] = NEW Progress.Data.ColumnPropDesc("FunctionCallParameter":U, "FunctionCallParameter":U, Progress.Data.DataType:CLOB).
        arrayvar13[7] = NEW Progress.Data.ColumnPropDesc("FunctionModuleGuid":U, "FunctionModuleGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar13[8] = NEW Progress.Data.ColumnPropDesc("ModuleName":U, "ModuleName":U, Progress.Data.DataType:CHARACTER).
        arrayvar13[9] = NEW Progress.Data.ColumnPropDesc("ExternalId":U, "External ID":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar13.
        THIS-OBJECT:FunctionBindingSource:TableSchema = tableDesc1.
        /*  */
        /* m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 151).
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Left:Name = "m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 398).
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(753, 151).
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Right:Name = "m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 398).
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Top:Name = "m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(761, 151).
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 549).
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Bottom:Name = "m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(761, 8).
        THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* SmartFunctionMaintenanceForm */
        /*  */
        THIS-OBJECT:AllowDrop = TRUE.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(761, 557).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:SmartFunctionMaintenanceForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartFunctionMaintenanceForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "SmartFunctionMaintenanceForm":U.
        THIS-OBJECT:Text = "Function Maintenance":U.
        THIS-OBJECT:WindowPositionRegistryKey = "SmartFunctionMaintenanceForm":U.
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:SmartFunctionMaintenanceForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:SmartFunctionMaintenanceForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartFunctionViewer1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:FunctionAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:FunctionBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the DragDrop event
        Notes:   Verify if dragged data conatins a Form 
        @param e An DragEventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnDragDrop (e AS System.Windows.Forms.DragEventArgs):

        DEFINE VARIABLE oController AS MenuDatasetController NO-UNDO . 
        DEFINE VARIABLE cFileNames  AS CHARACTER             NO-UNDO EXTENT .
        
        IF smartFunctionViewer1:SmartTableIOState = TableIOStateEnum:ModifyingData OR 
           smartFunctionViewer1:SmartTableIOState = TableIOStateEnum:FieldsEnabled THEN 
            RETURN . 

        oController = CAST (FunctionAdapter:DatasetController, 
                            MenuDatasetController) .

        IF DragAndDropHelper:IsFileDrop (e:Data) THEN DO:
            ASSIGN cFileNames = DragAndDropHelper:GetDropFileNames (e:Data) .

            oController:CreateFunctionsForClasses (cFileNames) .
        END.

        SUPER:OnDragDrop (e) .
        
        CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.
        
    END METHOD .   
    
    /*------------------------------------------------------------------------------
        Purpose: Raises the DragEnter event
        Notes:   Verify if dragged data conatins a Form 
        @param e An DragEventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnDragEnter (e AS System.Windows.Forms.DragEventArgs):

        DEFINE VARIABLE oController AS MenuDatasetController NO-UNDO . 
        DEFINE VARIABLE cFileNames  AS CHARACTER             NO-UNDO EXTENT .
        
        e:Effect = DragDropEffects:None .                            
        
        IF smartFunctionViewer1:SmartTableIOState = TableIOStateEnum:ModifyingData OR 
           smartFunctionViewer1:SmartTableIOState = TableIOStateEnum:FieldsEnabled THEN 
            RETURN . 
        
        oController = CAST (FunctionAdapter:DatasetController, 
                            MenuDatasetController) .
                                                        
        IF DragAndDropHelper:IsFileDrop (e:Data) THEN DO:
            ASSIGN cFileNames = DragAndDropHelper:GetDropFileNames (e:Data) .
            
            IF oController:ContainsClassFileNames (cFileNames) THEN 
                e:Effect = DragDropEffects:Copy . 
        END.                                             
        
        SUPER:OnDragEnter (e) .
        
        CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.
        
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

        smartDataBrowser1:DisplayLayout:Bands[0]:Columns["FunctionModuleGuid":U]:ValueList = ValueListHelper:FromBusinessEntity ("":U,
                                                                                                                                 "Consultingwerk.SmartFramework.Menu.ModuleBusinessEntity":U,
                                                                                                                                 "eSmartModule":U,
                                                                                                                                 "":U,
                                                                                                                                 "ModuleGuid":U,
                                                                                                                                 "ModuleName":U,
                                                                                                                                 "&1":U) .

        FunctionAdapter:RetrieveData ().
            
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Provides the context of the menu item that launched the current
                 function to the called instance
        Notes:   May be an object of the MenuContext class that provides information 
                 about the Label of the menu item that launched the current function
        @param poContext The context of the meun function that launched the current object
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ReceiveMenuContext (INPUT poContext AS Progress.Lang.Object):
		
		DEFINE VARIABLE cTitle  AS CHARACTER NO-UNDO.
		
		IF VALID-OBJECT (poContext) AND TYPE-OF (poContext, Consultingwerk.SmartFramework.MenuContext) THEN DO: 
		    
		    ASSIGN cTitle = CAST (poContext, Consultingwerk.SmartFramework.MenuContext):MenuTitle .
		    
		    IF cTitle > "":U THEN 
		       ASSIGN THIS-OBJECT:TEXT = cTitle . 
		END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Starts a drag and drop action with a dictionary
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartDataBrowser1_MouseDown (sender AS System.Object, 
                                                     e      AS System.Windows.Forms.MouseEventArgs):
        
        DEFINE VARIABLE oUIElement  AS Infragistics.Win.UIElement    NO-UNDO . 
        DEFINE VARIABLE oDictionary AS "Dictionary<String,String>":U NO-UNDO .
        
        /* Find out if the RowSelect is being clicked, don't do D&D when clicking a cell */      
        oUIElement = smartDataBrowser1:DisplayLayout:UIElement:ElementFromPoint (e:Location) . 
        
        IF TYPE-OF (oUIElement, Infragistics.Win.UltraWinGrid.RowSelectorUIElement)  
           AND smartDataBrowser1:Selected:Rows:Count > 0 THEN DO:
            oDictionary = NEW "Dictionary<String,String>":U ().
            
            oDictionary:Add ("eSmartFunction.FunctionGuid":U, FunctionAdapter:GetFieldValues("FunctionGuid":U)).
            oDictionary:Add ("eSmartFunction.FunctionName":U, FunctionAdapter:GetFieldValues("FunctionName":U)).
            oDictionary:Add ("eSmartFunction.FunctionSmallImage":U, FunctionAdapter:GetFieldValues("FunctionSmallImage":U)).
            oDictionary:Add ("eSmartFunction.FunctionLargeImage":U, FunctionAdapter:GetFieldValues("FunctionLargeImage":U)).
            
            smartDataBrowser1:DoDragDrop (oDictionary, System.Windows.Forms.DragDropEffects:Link).
        END.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartFunctionMaintenanceForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartFunctionMaintenanceForm ():

    END DESTRUCTOR .

END CLASS.
