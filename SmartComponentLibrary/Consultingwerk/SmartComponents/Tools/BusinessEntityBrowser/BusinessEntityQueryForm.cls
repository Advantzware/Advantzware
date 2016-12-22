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
    File        : BusinessEntityQueryForm
    Purpose     : Shows data from a single business entity within the 
                  BusinessEntityBrowserForm
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 14 23:57:15 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.*                                          FROM PROPATH .
USING Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.* FROM PROPATH . 
USING Consultingwerk.Util.*                                           FROM PROPATH . 
USING Infragistics.Win.UltraWinToolbars.*                             FROM ASSEMBLY . 
USING Progress.Lang.*                                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Tools.BusinessEntityBrowser.BusinessEntityQueryForm 
    INHERITS BaseForm
    IMPLEMENTS IOERABusinessEntityTesterForm: 
    
    DEFINE PROTECTED VARIABLE imageList1 AS System.Windows.Forms.ImageList NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityQueryForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityQueryForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityQueryForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityQueryForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE BusinessEntityQueryForm_Fill_Panel AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE oeraBusinessEntityTesterUserControl1 AS Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.OERABusinessEntityTesterUserControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity
        Notes:   
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY BusinessEntityName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the BusinessEntityQueryForm Class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessEntityQueryForm ():
                
        SUPER().
        InitializeComponent().
        
        oeraBusinessEntityTesterUserControl1:OERABusinessEntityTesterForm = THIS-OBJECT . 
        oeraBusinessEntityTesterUserControl1:ImageList = THIS-OBJECT:imageList1.
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Set the tools (ToolButton and MenuItem) disabled.
                 Enforced by the Interface IOERABusinessEntityTesterForm
        Notes:   
        @param pcToolName The key of the Tool to Disable
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisableTool (pcToolName AS CHARACTER):
        
        IF THIS-OBJECT:ultraToolbarsManager1:Tools:Exists(pcToolName) THEN DO:
            THIS-OBJECT:ultraToolbarsManager1:Tools[pcToolName]:SharedProps:Enabled = FALSE.
        END.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Set the tools (ToolButton and MenuItem) enabled.
                 Enforced by the Interface IOERABusinessEntityTesterForm
        Notes:   
        @param pcToolName The key of the Tool to Enable
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnableTool (pcToolName AS CHARACTER):
        
        IF THIS-OBJECT:ultraToolbarsManager1:Tools:Exists(pcToolName) THEN DO:
            THIS-OBJECT:ultraToolbarsManager1:Tools[pcToolName]:SharedProps:Enabled = TRUE.
        END.
        
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
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.SmartComponents.Tools.BusinessEntityBrowser.BusinessEntityQueryForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("ribbon1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SelectAll":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SelectNone":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup2":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetData":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Filter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool1 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool1 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("BatchSize":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool2 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool2 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("StopAfter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup3":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE textBoxTool1 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
        textBoxTool1 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("tbCustomContext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("StopEntities":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup4 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("Info":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool20 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool20 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Data Source Query Info":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("UltraToolbarMainMenu":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool1 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool1 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("File":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool2 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool2 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Data":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("UltraToolbarBusinessEntityTester":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool3 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool3 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("EntityName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetSchema":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetData":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Filter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool1 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool1 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("Batch Size":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool4 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool4 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("BatchSize":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar3 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar3 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("Service Manager":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("StopEntities":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar4 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar4 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("Custom Context":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool2 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool2 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("lblCustomContext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE textBoxTool2 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
        textBoxTool2 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("tbCustomContext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar5 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar5 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("Selection":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SelectAll":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SelectNone":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool3 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool3 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("File":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool4 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool4 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Data":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetSchema":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetData":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool15 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool15 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Filter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool16 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool16 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool10 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool10 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("EntityName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueList1 AS Infragistics.Win.ValueList NO-UNDO.
        valueList1 = NEW Infragistics.Win.ValueList(0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool17 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool17 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetSchema":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetData":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Filter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool24 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool24 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("RemoveFilter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool5 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool5 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("TreeContextMenu":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool45 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool45 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("RemoveFilter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool3 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool3 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("Batch Size":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool11 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool11 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("BatchSize":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueList2 AS Infragistics.Win.ValueList NO-UNDO.
        valueList2 = NEW Infragistics.Win.ValueList(0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem1 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem1 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem2 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem2 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem3 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem3 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem4 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem4 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem5 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem5 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool46 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool46 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("StopEntities":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool4 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool4 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("lblCustomContext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE textBoxTool6 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
        textBoxTool6 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("tbCustomContext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool47 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool47 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SelectAll":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool48 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool48 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SelectNone":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool12 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool12 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("StopAfter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueList3 AS Infragistics.Win.ValueList NO-UNDO.
        valueList3 = NEW Infragistics.Win.ValueList(0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem11 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem11 = NEW Infragistics.Win.ValueListItem().
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
        DEFINE VARIABLE buttonTool21 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool21 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Data Source Query Info":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:BusinessEntityQueryForm_Fill_Panel = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:oeraBusinessEntityTesterUserControl1 = NEW Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.OERABusinessEntityTesterUserControl().
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:imageList1 = NEW System.Windows.Forms.ImageList(THIS-OBJECT:components).
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:BusinessEntityQueryForm_Fill_Panel:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.Forms.BaseForm":U).
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:ApplicationMenuButtonImage = CAST(resources:GetObject("ultraToolbarsManager1.Ribbon.ApplicationMenuButtonImage":U), System.Drawing.Image).
        ribbonTab1:Caption = "Browser":U.
        ribbonGroup1:Caption = "Table selection":U.
        buttonTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool2:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        arrayvar0[2] = buttonTool2.
        ribbonGroup1:Tools:AddRange(arrayvar0).
        ribbonGroup2:Caption = "Data Retrieval":U.
        buttonTool3:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool4:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar1[1] = buttonTool3.
        arrayvar1[2] = buttonTool4.
        arrayvar1[3] = comboBoxTool1.
        arrayvar1[4] = comboBoxTool2.
        ribbonGroup2:Tools:AddRange(arrayvar1).
        ribbonGroup3:Caption = "Business Entity":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar2[1] = textBoxTool1.
        arrayvar2[2] = buttonTool5.
        ribbonGroup3:Tools:AddRange(arrayvar2).
        ribbonGroup4:Caption = "Info":U.
        buttonTool20:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar3[1] = buttonTool20.
        ribbonGroup4:Tools:AddRange(arrayvar3).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 4 NO-UNDO.
        arrayvar4[1] = ribbonGroup1.
        arrayvar4[2] = ribbonGroup2.
        arrayvar4[3] = ribbonGroup3.
        arrayvar4[4] = ribbonGroup4.
        ribbonTab1:Groups:AddRange(arrayvar4).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar5[1] = ribbonTab1.
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar5).
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:Visible = TRUE.
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        THIS-OBJECT:ultraToolbarsManager1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2007.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        ultraToolbar1:IsMainMenuBar = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar6[1] = popupMenuTool1.
        arrayvar6[2] = popupMenuTool2.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar6).
        ultraToolbar1:Text = "UltraToolbarMainMenu":U.
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        labelTool1:InstanceProps:IsFirstInGroup = TRUE.
        comboBoxTool4:InstanceProps:Width = 89.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar7[1] = comboBoxTool3.
        arrayvar7[2] = buttonTool6.
        arrayvar7[3] = buttonTool7.
        arrayvar7[4] = buttonTool8.
        arrayvar7[5] = labelTool1.
        arrayvar7[6] = comboBoxTool4.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar7).
        ultraToolbar2:Text = "UltraToolbarBusinessEntityTester":U.
        ultraToolbar3:DockedColumn = 0.
        ultraToolbar3:DockedRow = 2.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar8[1] = buttonTool9.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar8).
        ultraToolbar3:Text = "Service Manager":U.
        ultraToolbar4:DockedColumn = 1.
        ultraToolbar4:DockedRow = 2.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar9[1] = labelTool2.
        arrayvar9[2] = textBoxTool2.
        ultraToolbar4:NonInheritedTools:AddRange(arrayvar9).
        ultraToolbar4:Text = "Custom Context":U.
        ultraToolbar5:DockedColumn = 2.
        ultraToolbar5:DockedRow = 2.
        ultraToolbar5:FloatingSize = NEW System.Drawing.Size(155, 24).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar10[1] = buttonTool10.
        arrayvar10[2] = buttonTool11.
        ultraToolbar5:NonInheritedTools:AddRange(arrayvar10).
        ultraToolbar5:Text = "Selection":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 5 NO-UNDO.
        arrayvar11[1] = ultraToolbar1.
        arrayvar11[2] = ultraToolbar2.
        arrayvar11[3] = ultraToolbar3.
        arrayvar11[4] = ultraToolbar4.
        arrayvar11[5] = ultraToolbar5.
        THIS-OBJECT:ultraToolbarsManager1:Toolbars:AddRange(arrayvar11).
        popupMenuTool3:SharedPropsInternal:Caption = "&File":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar12[1] = buttonTool12.
        popupMenuTool3:Tools:AddRange(arrayvar12).
        popupMenuTool4:SharedPropsInternal:Caption = "&Data":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar13[1] = buttonTool13.
        arrayvar13[2] = buttonTool14.
        arrayvar13[3] = buttonTool15.
        popupMenuTool4:Tools:AddRange(arrayvar13).
        buttonTool16:SharedPropsInternal:Caption = "&Exit":U.
        comboBoxTool10:DropDownStyle = Infragistics.Win.DropDownStyle:DropDown.
        comboBoxTool10:SharedPropsInternal:Caption = "EntityName":U.
        comboBoxTool10:SharedPropsInternal:Width = 290.
        comboBoxTool10:ValueList = valueList1.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        buttonTool17:SharedPropsInternal:AppearancesSmall:Appearance = appearance1.
        buttonTool17:SharedPropsInternal:Caption = "Get Schema":U.
        buttonTool17:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool18:SharedPropsInternal:AppearancesLarge:Appearance = appearance2.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        buttonTool18:SharedPropsInternal:AppearancesSmall:Appearance = appearance3.
        buttonTool18:SharedPropsInternal:Caption = "Get Data":U.
        buttonTool18:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        buttonTool18:SharedPropsInternal:Enabled = FALSE.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        buttonTool19:SharedPropsInternal:AppearancesLarge:Appearance = appearance4.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        buttonTool19:SharedPropsInternal:AppearancesSmall:Appearance = appearance5.
        buttonTool19:SharedPropsInternal:Caption = "Filter":U.
        buttonTool19:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        buttonTool19:SharedPropsInternal:Enabled = FALSE.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        buttonTool24:SharedPropsInternal:AppearancesLarge:Appearance = appearance6.
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        buttonTool24:SharedPropsInternal:AppearancesSmall:Appearance = appearance7.
        buttonTool24:SharedPropsInternal:Caption = "&RemoveFilter":U.
        buttonTool24:SharedPropsInternal:Enabled = FALSE.
        popupMenuTool5:SharedPropsInternal:Caption = "TreeContextMenu":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar14[1] = buttonTool45.
        popupMenuTool5:Tools:AddRange(arrayvar14).
        labelTool3:SharedPropsInternal:Caption = "Batch Size:":U.
        labelTool3:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:TextOnlyAlways.
        comboBoxTool11:AutoComplete = TRUE.
        comboBoxTool11:DropDownStyle = Infragistics.Win.DropDownStyle:DropDown.
        comboBoxTool11:SharedPropsInternal:Caption = "Batch Size":U.
        comboBoxTool11:Text = "100":U.
        valueListItem1:DataValue = System.Convert:ToInt16(100).
        valueListItem1:DisplayText = "100":U.
        valueListItem2:DataValue = System.Convert:ToInt16(200).
        valueListItem2:DisplayText = "200":U.
        valueListItem3:DataValue = System.Convert:ToInt16(500).
        valueListItem3:DisplayText = "500":U.
        valueListItem4:DataValue = System.Convert:ToInt16(1000).
        valueListItem4:DisplayText = "1000":U.
        valueListItem5:DataValue = System.Convert:ToInt16(0).
        valueListItem5:DisplayText = "(unlimited)":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar15 AS Infragistics.Win.ValueListItem EXTENT 5 NO-UNDO.
        arrayvar15[1] = valueListItem1.
        arrayvar15[2] = valueListItem2.
        arrayvar15[3] = valueListItem3.
        arrayvar15[4] = valueListItem4.
        arrayvar15[5] = valueListItem5.
        valueList2:ValueListItems:AddRange(arrayvar15).
        comboBoxTool11:ValueList = valueList2.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesLarge:Appearance = appearance8.
        appearance9:Image = CAST(resources:GetObject("appearance9.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesSmall:Appearance = appearance9.
        buttonTool46:SharedPropsInternal:Caption = "Stop Business Entities":U.
        buttonTool46:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        labelTool4:SharedPropsInternal:Caption = "Custom Context:":U.
        labelTool4:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:TextOnlyAlways.
        textBoxTool6:SharedPropsInternal:Caption = "Custom Context":U.
        textBoxTool6:SharedPropsInternal:Width = 200.
        appearance10:Image = CAST(resources:GetObject("appearance10.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesSmall:Appearance = appearance10.
        buttonTool47:SharedPropsInternal:Caption = "Select All":U.
        appearance11:Image = CAST(resources:GetObject("appearance11.Image":U), System.Object).
        buttonTool48:SharedPropsInternal:AppearancesSmall:Appearance = appearance11.
        buttonTool48:SharedPropsInternal:Caption = "Select None":U.
        comboBoxTool12:AutoComplete = TRUE.
        comboBoxTool12:DropDownStyle = Infragistics.Win.DropDownStyle:DropDown.
        comboBoxTool12:SharedPropsInternal:Caption = "Stop After":U.
        comboBoxTool12:Text = "5":U.
        valueListItem11:DataValue = System.Convert:ToInt16(0).
        valueListItem11:DisplayText = "(off)":U.
        valueListItem12:DataValue = System.Convert:ToInt16(1).
        valueListItem12:DisplayText = "1":U.
        valueListItem13:DataValue = System.Convert:ToInt16(5).
        valueListItem13:DisplayText = "5":U.
        valueListItem14:DataValue = System.Convert:ToInt16(15).
        valueListItem14:DisplayText = "15":U.
        valueListItem15:DataValue = System.Convert:ToInt16(30).
        valueListItem15:DisplayText = "30":U.
        valueListItem16:DataValue = System.Convert:ToInt16(60).
        valueListItem16:DisplayText = "60":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar16 AS Infragistics.Win.ValueListItem EXTENT 6 NO-UNDO.
        arrayvar16[1] = valueListItem11.
        arrayvar16[2] = valueListItem12.
        arrayvar16[3] = valueListItem13.
        arrayvar16[4] = valueListItem14.
        arrayvar16[5] = valueListItem15.
        arrayvar16[6] = valueListItem16.
        valueList3:ValueListItems:AddRange(arrayvar16).
        comboBoxTool12:ValueList = valueList3.
        appearance12:Image = CAST(resources:GetObject("appearance12.Image":U), System.Object).
        buttonTool21:SharedPropsInternal:AppearancesLarge:Appearance = appearance12.
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        buttonTool21:SharedPropsInternal:AppearancesSmall:Appearance = appearance13.
        buttonTool21:SharedPropsInternal:Caption = "Data Source Query Info":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar17 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 18 NO-UNDO.
        arrayvar17[1] = popupMenuTool3.
        arrayvar17[2] = popupMenuTool4.
        arrayvar17[3] = buttonTool16.
        arrayvar17[4] = comboBoxTool10.
        arrayvar17[5] = buttonTool17.
        arrayvar17[6] = buttonTool18.
        arrayvar17[7] = buttonTool19.
        arrayvar17[8] = buttonTool24.
        arrayvar17[9] = popupMenuTool5.
        arrayvar17[10] = labelTool3.
        arrayvar17[11] = comboBoxTool11.
        arrayvar17[12] = buttonTool46.
        arrayvar17[13] = labelTool4.
        arrayvar17[14] = textBoxTool6.
        arrayvar17[15] = buttonTool47.
        arrayvar17[16] = buttonTool48.
        arrayvar17[17] = comboBoxTool12.
        arrayvar17[18] = buttonTool21.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar17).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager_ToolClick).
        /*  */
        /* BusinessEntityQueryForm_Fill_Panel */
        /*  */
        THIS-OBJECT:BusinessEntityQueryForm_Fill_Panel:Controls:Add(THIS-OBJECT:oeraBusinessEntityTesterUserControl1).
        THIS-OBJECT:BusinessEntityQueryForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:BusinessEntityQueryForm_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:BusinessEntityQueryForm_Fill_Panel:Location = NEW System.Drawing.Point(11, 236).
        THIS-OBJECT:BusinessEntityQueryForm_Fill_Panel:Name = "BusinessEntityQueryForm_Fill_Panel":U.
        THIS-OBJECT:BusinessEntityQueryForm_Fill_Panel:Size = NEW System.Drawing.Size(829, 313).
        THIS-OBJECT:BusinessEntityQueryForm_Fill_Panel:TabIndex = 0.
        /*  */
        /* oeraBusinessEntityTesterUserControl1 */
        /*  */
        THIS-OBJECT:oeraBusinessEntityTesterUserControl1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:oeraBusinessEntityTesterUserControl1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:oeraBusinessEntityTesterUserControl1:Name = "oeraBusinessEntityTesterUserControl1":U.
        THIS-OBJECT:oeraBusinessEntityTesterUserControl1:Size = NEW System.Drawing.Size(829, 313).
        THIS-OBJECT:oeraBusinessEntityTesterUserControl1:TabIndex = 0.
        /*  */
        /* m_BusinessEntityQueryForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 11.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 236).
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Left:Name = "m_BusinessEntityQueryForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(11, 313).
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_BusinessEntityQueryForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 11.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(840, 236).
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Right:Name = "m_BusinessEntityQueryForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(11, 313).
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_BusinessEntityQueryForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Top:Name = "m_BusinessEntityQueryForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(851, 236).
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_BusinessEntityQueryForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 11.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 549).
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Bottom:Name = "m_BusinessEntityQueryForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(851, 11).
        THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* imageList1 */
        /*  */
        THIS-OBJECT:imageList1:ImageStream = CAST(resources:GetObject("imageList1.ImageStream":U), System.Windows.Forms.ImageListStreamer).
        THIS-OBJECT:imageList1:TransparentColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:imageList1:Images:SetKeyName(0, "blank.gif":U).
        THIS-OBJECT:imageList1:Images:SetKeyName(1, "filter.gif":U).
        /*  */
        /* BusinessEntityQueryForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(851, 560).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:BusinessEntityQueryForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityQueryForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Name = "BusinessEntityQueryForm":U.
        THIS-OBJECT:Text = "BusinessEntityQueryForm":U.
        THIS-OBJECT:WindowPositionRegistryKey = "BusinessEntityQueryForm":U.
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:BusinessEntityQueryForm_Fill_Panel:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Selects all nodes in the Business Entity Tester Control                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SelectAll ():
        
        oeraBusinessEntityTesterUserControl1:SelectAll () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deselects all nodes in the Busienss Entity Tester Control                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SelectNone ():
        
        oeraBusinessEntityTesterUserControl1:SelectNone () .

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Set's the Business Entity
        Notes:                                                  
        @param pcClassName The name of the Business Entity class                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetBusinessEntity (pcClassName AS CHARACTER):
        
        oeraBusinessEntityTesterUserControl1:FetchEntity (pcClassName) .
    
        ASSIGN 
            THIS-OBJECT:BusinessEntityName = pcClassName    
            THIS-OBJECT:Text               = pcClassName . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handle for the ToolClick event of the ultraToolbarsManager 
                 instance
        Notes:   
        @param sender The refence to the sender that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraToolbarsManager_ToolClick (sender AS System.Object, 
                                                        e      AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):
        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iValue AS INTEGER NO-UNDO.

        CASE e:Tool:Key :

            WHEN "GetData":U THEN DO:
/*                IF NOT CAST (THIS-OBJECT:ultraToolbarsManager1:Tools["EntityName":U], ComboBoxTool):Text > "":U THEN RETURN.*/
        
                ASSIGN cValue = CAST(ultraToolbarsManager1:Tools["BatchSize":U], Infragistics.Win.UltraWinToolbars.ComboBoxTool):Text 
                       iValue = INTEGER (cValue) NO-ERROR .
                   
                IF iValue > 0 THEN . 
                ELSE 
                   iValue = 0 .  
        
                THIS-OBJECT:oeraBusinessEntityTesterUserControl1:BatchSize = iValue .

                ASSIGN cValue = CAST(ultraToolbarsManager1:Tools["StopAfter":U], Infragistics.Win.UltraWinToolbars.ComboBoxTool):Text 
                       iValue = INTEGER (cValue) NO-ERROR .
                   
                IF iValue > 0 THEN . 
                ELSE 
                   iValue = 0 .  

                THIS-OBJECT:oeraBusinessEntityTesterUserControl1:StopAfter = iValue .

                THIS-OBJECT:oeraBusinessEntityTesterUserControl1:CustomContext = CAST (ultraToolbarsManager1:Tools["tbCustomContext"], TextBoxTool):Text .
                 
                THIS-OBJECT:oeraBusinessEntityTesterUserControl1:FetchDataFromBackend (THIS-OBJECT:BusinessEntityName).
            END.
            WHEN "Filter":U THEN THIS-OBJECT:oeraBusinessEntityTesterUserControl1:ShowFilterDialog ().
            WHEN "RemoveFilter":U THEN THIS-OBJECT:oeraBusinessEntityTesterUserControl1:RemoveFilter ().
            WHEN "StopEntities":U THEN oeraBusinessEntityTesterUserControl1:ShutDownBusinessEntities () .
            WHEN "SelectAll":U THEN SelectAll () .
            WHEN "SelectNone":U THEN SelectNone () .
            WHEN "Data Source Query Info":U THEN 
                oeraBusinessEntityTesterUserControl1:ShowDataSourceQueryInfo () .            
            WHEN "Exit":U THEN THIS-OBJECT:Close ().
        END CASE.

        CATCH ple AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (ple, SUBSTITUTE ("Error while performing the task &1!", QUOTER (e:Tool:Key))) .
        END CATCH.
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Destructor of the BusinessEntityQueryForm Class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC BusinessEntityQueryForm ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.