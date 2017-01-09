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
    File        : UltraBusinessEntityTesterFormDesigner
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Mon Aug 03 10:54:09 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.*                FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .
USING Consultingwerk.Util.*                 FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*    FROM ASSEMBLY .
USING Progress.Lang.*                       FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.UltraBusinessEntityTesterFormDesigner
    INHERITS BaseForm :

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PROTECTED VARIABLE tbCustomParameter AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PROTECTED VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
    DEFINE PRIVATE VARIABLE Form_Fill_Panel AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PROTECTED VARIABLE oeraBusinessEntityTesterUserControl1 AS Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.OERABusinessEntityTesterUserControl NO-UNDO.
    DEFINE PRIVATE VARIABLE m_Form_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_Form_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_Form_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_Form_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.

    DEFINE PROTECTED VARIABLE imageList1 AS System.Windows.Forms.ImageList NO-UNDO.

    CONSTRUCTOR PUBLIC UltraBusinessEntityTesterFormDesigner ():
        SUPER ().
        InitializeComponent ().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

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
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("UltraToolbarMainMenu":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool1 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool1 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("File":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool3 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool3 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Data":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("UltraToolbarBusinessEntityTester":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool3 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool3 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("EntityName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetSchema":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetData":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Filter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool1 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool1 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("Batch Size":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool1 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool1 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("BatchSize":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool5 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool5 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("Stop-After:":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool5 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool5 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("StopAfter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar3 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar3 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("Service Manager":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("StopEntities":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool22 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool22 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Monitor Business Services":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar4 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar4 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("Custom Context":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool3 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool3 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("lblCustomContext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE textBoxTool1 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
        textBoxTool1 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("tbCustomContext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupControlContainerTool1 AS Infragistics.Win.UltraWinToolbars.PopupControlContainerTool NO-UNDO.
        popupControlContainerTool1 = NEW Infragistics.Win.UltraWinToolbars.PopupControlContainerTool("Custom Parameter Object":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar5 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar5 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("Selection":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool16 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool16 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SelectAll":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool17 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool17 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SelectNone":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar6 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar6 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("Query Info":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool20 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool20 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Data Source Query Info":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar7 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar7 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("Session Management":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool24 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool24 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Authenticate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool2 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool2 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("File":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool4 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool4 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Data":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetSchema":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetData":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Filter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool4 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool4 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("EntityName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueList1 AS Infragistics.Win.ValueList NO-UNDO.
        valueList1 = NEW Infragistics.Win.ValueList(0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetSchema":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GetData":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Filter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("RemoveFilter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool5 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool5 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("TreeContextMenu":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("RemoveFilter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool2 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool2 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("Batch Size":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool2 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool2 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("BatchSize":U).
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
        DEFINE VARIABLE buttonTool15 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool15 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("StopEntities":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool4 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool4 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("lblCustomContext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE textBoxTool2 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
        textBoxTool2 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("tbCustomContext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SelectAll":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SelectNone":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool6 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool6 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("StopAfter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueList3 AS Infragistics.Win.ValueList NO-UNDO.
        valueList3 = NEW Infragistics.Win.ValueList(0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem11 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem11 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem6 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem6 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem8 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem8 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem7 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem7 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem9 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem9 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem10 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem10 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE labelTool6 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
        labelTool6 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("Stop-After:":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool21 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool21 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Data Source Query Info":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupControlContainerTool2 AS Infragistics.Win.UltraWinToolbars.PopupControlContainerTool NO-UNDO.
        popupControlContainerTool2 = NEW Infragistics.Win.UltraWinToolbars.PopupControlContainerTool("Custom Parameter Object":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool23 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool23 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Monitor Business Services":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool25 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool25 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Authenticate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.UltraBusinessEntityTesterFormDesigner":U).
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:Form_Fill_Panel = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:tbCustomParameter = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:oeraBusinessEntityTesterUserControl1 = NEW Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.OERABusinessEntityTesterUserControl().
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:imageList1 = NEW System.Windows.Forms.ImageList(THIS-OBJECT:components).
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:Form_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:tbCustomParameter, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Progress.Windows.Form":U).
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        THIS-OBJECT:ultraToolbarsManager1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2007.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        ultraToolbar1:IsMainMenuBar = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar0[1] = popupMenuTool1.
        arrayvar0[2] = popupMenuTool3.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar0).
        ultraToolbar1:Text = "UltraToolbarMainMenu":U.
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        labelTool1:InstanceProps:IsFirstInGroup = TRUE.
        comboBoxTool1:InstanceProps:Width = 89.
        labelTool5:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 8 NO-UNDO.
        arrayvar1[1] = comboBoxTool3.
        arrayvar1[2] = buttonTool7.
        arrayvar1[3] = buttonTool8.
        arrayvar1[4] = buttonTool9.
        arrayvar1[5] = labelTool1.
        arrayvar1[6] = comboBoxTool1.
        arrayvar1[7] = labelTool5.
        arrayvar1[8] = comboBoxTool5.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar1).
        ultraToolbar2:Text = "UltraToolbarBusinessEntityTester":U.
        ultraToolbar3:DockedColumn = 0.
        ultraToolbar3:DockedRow = 2.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar2[1] = buttonTool14.
        arrayvar2[2] = buttonTool22.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar2).
        ultraToolbar3:Text = "Service Manager":U.
        ultraToolbar4:DockedColumn = 1.
        ultraToolbar4:DockedRow = 2.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar3[1] = labelTool3.
        arrayvar3[2] = textBoxTool1.
        arrayvar3[3] = popupControlContainerTool1.
        ultraToolbar4:NonInheritedTools:AddRange(arrayvar3).
        ultraToolbar4:Text = "Custom Context":U.
        ultraToolbar5:DockedColumn = 1.
        ultraToolbar5:DockedRow = 3.
        ultraToolbar5:FloatingSize = NEW System.Drawing.Size(155, 24).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar4[1] = buttonTool16.
        arrayvar4[2] = buttonTool17.
        ultraToolbar5:NonInheritedTools:AddRange(arrayvar4).
        ultraToolbar5:Text = "Selection":U.
        ultraToolbar6:DockedColumn = 0.
        ultraToolbar6:DockedRow = 3.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar5[1] = buttonTool20.
        ultraToolbar6:NonInheritedTools:AddRange(arrayvar5).
        ultraToolbar6:Text = "Query Info":U.
        ultraToolbar7:DockedColumn = 2.
        ultraToolbar7:DockedRow = 3.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar6[1] = buttonTool24.
        ultraToolbar7:NonInheritedTools:AddRange(arrayvar6).
        ultraToolbar7:Text = "Session Management":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 7 NO-UNDO.
        arrayvar7[1] = ultraToolbar1.
        arrayvar7[2] = ultraToolbar2.
        arrayvar7[3] = ultraToolbar3.
        arrayvar7[4] = ultraToolbar4.
        arrayvar7[5] = ultraToolbar5.
        arrayvar7[6] = ultraToolbar6.
        arrayvar7[7] = ultraToolbar7.
        THIS-OBJECT:ultraToolbarsManager1:Toolbars:AddRange(arrayvar7).
        popupMenuTool2:SharedPropsInternal:Caption = "&File":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar8[1] = buttonTool5.
        popupMenuTool2:Tools:AddRange(arrayvar8).
        popupMenuTool4:SharedPropsInternal:Caption = "&Data":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar9[1] = buttonTool3.
        arrayvar9[2] = buttonTool2.
        arrayvar9[3] = buttonTool1.
        popupMenuTool4:Tools:AddRange(arrayvar9).
        buttonTool6:SharedPropsInternal:Caption = "&Exit":U.
        comboBoxTool4:DropDownStyle = Infragistics.Win.DropDownStyle:DropDown.
        comboBoxTool4:SharedPropsInternal:Caption = "EntityName":U.
        comboBoxTool4:SharedPropsInternal:Width = 290.
        comboBoxTool4:ValueList = valueList1.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool10:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        buttonTool10:SharedPropsInternal:Caption = "Get Schema":U.
        buttonTool10:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        buttonTool11:SharedPropsInternal:AppearancesSmall:Appearance = appearance3.
        buttonTool11:SharedPropsInternal:Caption = "Get Data":U.
        buttonTool11:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        buttonTool12:SharedPropsInternal:AppearancesSmall:Appearance = appearance4.
        buttonTool12:SharedPropsInternal:Caption = "Filter":U.
        buttonTool12:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        buttonTool4:SharedPropsInternal:Caption = "&RemoveFilter":U.
        popupMenuTool5:SharedPropsInternal:Caption = "TreeContextMenu":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar10[1] = buttonTool13.
        popupMenuTool5:Tools:AddRange(arrayvar10).
        labelTool2:SharedPropsInternal:Caption = "Batch Size:":U.
        labelTool2:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:TextOnlyAlways.
        comboBoxTool2:AutoComplete = TRUE.
        comboBoxTool2:DropDownStyle = Infragistics.Win.DropDownStyle:DropDown.
        comboBoxTool2:SharedPropsInternal:Caption = "Batch Size":U.
        comboBoxTool2:Text = "100":U.
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
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.ValueListItem EXTENT 5 NO-UNDO.
        arrayvar11[1] = valueListItem1.
        arrayvar11[2] = valueListItem2.
        arrayvar11[3] = valueListItem3.
        arrayvar11[4] = valueListItem4.
        arrayvar11[5] = valueListItem5.
        valueList2:ValueListItems:AddRange(arrayvar11).
        comboBoxTool2:ValueList = valueList2.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        buttonTool15:SharedPropsInternal:AppearancesLarge:Appearance = appearance5.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        buttonTool15:SharedPropsInternal:AppearancesSmall:Appearance = appearance6.
        buttonTool15:SharedPropsInternal:Caption = "Stop Business Entities":U.
        buttonTool15:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        labelTool4:SharedPropsInternal:Caption = "Custom Context:":U.
        labelTool4:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:TextOnlyAlways.
        textBoxTool2:SharedPropsInternal:Caption = "Custom Context":U.
        textBoxTool2:SharedPropsInternal:Width = 200.
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        buttonTool18:SharedPropsInternal:AppearancesSmall:Appearance = appearance7.
        buttonTool18:SharedPropsInternal:Caption = "Select All":U.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image":U), System.Object).
        buttonTool19:SharedPropsInternal:AppearancesSmall:Appearance = appearance8.
        buttonTool19:SharedPropsInternal:Caption = "Select None":U.
        comboBoxTool6:DropDownStyle = Infragistics.Win.DropDownStyle:DropDown.
        comboBoxTool6:SharedPropsInternal:Caption = "Stop After":U.
        comboBoxTool6:SharedPropsInternal:Width = 60.
        comboBoxTool6:Text = "5":U.
        valueListItem11:DataValue = System.Convert:ToInt16(0).
        valueListItem11:DisplayText = "(off)":U.
        valueListItem6:DataValue = System.Convert:ToInt16(1).
        valueListItem6:DisplayText = "1":U.
        valueListItem8:DataValue = System.Convert:ToInt16(5).
        valueListItem8:DisplayText = "5":U.
        valueListItem7:DataValue = System.Convert:ToInt16(15).
        valueListItem7:DisplayText = "15":U.
        valueListItem9:DataValue = System.Convert:ToInt16(30).
        valueListItem9:DisplayText = "30":U.
        valueListItem10:DataValue = System.Convert:ToInt16(60).
        valueListItem10:DisplayText = "60":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS Infragistics.Win.ValueListItem EXTENT 6 NO-UNDO.
        arrayvar12[1] = valueListItem11.
        arrayvar12[2] = valueListItem6.
        arrayvar12[3] = valueListItem8.
        arrayvar12[4] = valueListItem7.
        arrayvar12[5] = valueListItem9.
        arrayvar12[6] = valueListItem10.
        valueList3:ValueListItems:AddRange(arrayvar12).
        comboBoxTool6:ValueList = valueList3.
        labelTool6:SharedPropsInternal:Caption = "Stop-After:":U.
        labelTool6:SharedPropsInternal:Width = 60.
        appearance9:Image = CAST(resources:GetObject("appearance9.Image":U), System.Object).
        buttonTool21:SharedPropsInternal:AppearancesLarge:Appearance = appearance9.
        appearance10:Image = CAST(resources:GetObject("appearance10.Image":U), System.Object).
        buttonTool21:SharedPropsInternal:AppearancesSmall:Appearance = appearance10.
        buttonTool21:SharedPropsInternal:Caption = "Data Source Query Info":U.
        buttonTool21:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        popupControlContainerTool2:ControlName = "tbCustomParameter":U.
        popupControlContainerTool2:DropDownArrowStyle = Infragistics.Win.UltraWinToolbars.DropDownArrowStyle:None.
        popupControlContainerTool2:SharedPropsInternal:Caption = "Custom Parameter Object (ISerializable)":U.
        popupControlContainerTool2:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:TextOnlyAlways.
        appearance11:Image = CAST(resources:GetObject("appearance11.Image":U), System.Object).
        buttonTool23:SharedPropsInternal:AppearancesSmall:Appearance = appearance11.
        buttonTool23:SharedPropsInternal:Caption = "Monitor Business Services":U.
        buttonTool23:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance12:Image = CAST(resources:GetObject("appearance12.Image":U), System.Object).
        buttonTool25:SharedPropsInternal:AppearancesSmall:Appearance = appearance12.
        buttonTool25:SharedPropsInternal:Caption = "Authenticate":U.
        buttonTool25:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 22 NO-UNDO.
        arrayvar13[1] = popupMenuTool2.
        arrayvar13[2] = popupMenuTool4.
        arrayvar13[3] = buttonTool6.
        arrayvar13[4] = comboBoxTool4.
        arrayvar13[5] = buttonTool10.
        arrayvar13[6] = buttonTool11.
        arrayvar13[7] = buttonTool12.
        arrayvar13[8] = buttonTool4.
        arrayvar13[9] = popupMenuTool5.
        arrayvar13[10] = labelTool2.
        arrayvar13[11] = comboBoxTool2.
        arrayvar13[12] = buttonTool15.
        arrayvar13[13] = labelTool4.
        arrayvar13[14] = textBoxTool2.
        arrayvar13[15] = buttonTool18.
        arrayvar13[16] = buttonTool19.
        arrayvar13[17] = comboBoxTool6.
        arrayvar13[18] = labelTool6.
        arrayvar13[19] = buttonTool21.
        arrayvar13[20] = popupControlContainerTool2.
        arrayvar13[21] = buttonTool23.
        arrayvar13[22] = buttonTool25.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar13).
        THIS-OBJECT:ultraToolbarsManager1:ToolValueChanged:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolValueChanged).
        /*  */
        /* Form_Fill_Panel */
        /*  */
        THIS-OBJECT:Form_Fill_Panel:Controls:Add(THIS-OBJECT:tbCustomParameter).
        THIS-OBJECT:Form_Fill_Panel:Controls:Add(THIS-OBJECT:oeraBusinessEntityTesterUserControl1).
        THIS-OBJECT:Form_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:Form_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:Form_Fill_Panel:Location = NEW System.Drawing.Point(0, 106).
        THIS-OBJECT:Form_Fill_Panel:Name = "Form_Fill_Panel":U.
        THIS-OBJECT:Form_Fill_Panel:Size = NEW System.Drawing.Size(1214, 458).
        THIS-OBJECT:Form_Fill_Panel:TabIndex = 0.
        /*  */
        /* tbCustomParameter */
        /*  */
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        appearance1:ImageHAlign = Infragistics.Win.HAlign:Center.
        appearance1:ImageVAlign = Infragistics.Win.VAlign:Middle.
        editorButton1:Appearance = appearance1.
        THIS-OBJECT:tbCustomParameter:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tbCustomParameter:Location = NEW System.Drawing.Point(244, 34).
        THIS-OBJECT:tbCustomParameter:Multiline = TRUE.
        THIS-OBJECT:tbCustomParameter:Name = "tbCustomParameter":U.
        THIS-OBJECT:tbCustomParameter:Size = NEW System.Drawing.Size(471, 161).
        THIS-OBJECT:tbCustomParameter:TabIndex = 1.
        THIS-OBJECT:tbCustomParameter:EditorButtonClick:Subscribe(THIS-OBJECT:tbCustomParameter_EditorButtonClick).
        THIS-OBJECT:tbCustomParameter:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* oeraBusinessEntityTesterUserControl1 */
        /*  */
        THIS-OBJECT:oeraBusinessEntityTesterUserControl1:CustomParameterObject = "":U.
        THIS-OBJECT:oeraBusinessEntityTesterUserControl1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:oeraBusinessEntityTesterUserControl1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:oeraBusinessEntityTesterUserControl1:Name = "oeraBusinessEntityTesterUserControl1":U.
        THIS-OBJECT:oeraBusinessEntityTesterUserControl1:Size = NEW System.Drawing.Size(1214, 458).
        THIS-OBJECT:oeraBusinessEntityTesterUserControl1:StatusBarVisible = TRUE.
        THIS-OBJECT:oeraBusinessEntityTesterUserControl1:TabIndex = 0.
        /*  */
        /* m_Form_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 106).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left:Name = "m_Form_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(0, 458).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_Form_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(1214, 106).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right:Name = "m_Form_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(0, 458).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_Form_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top:Name = "m_Form_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(1214, 106).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_Form_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 564).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom:Name = "m_Form_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(1214, 0).
        THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* imageList1 */
        /*  */
        THIS-OBJECT:imageList1:ImageStream = CAST(resources:GetObject("imageList1.ImageStream":U), System.Windows.Forms.ImageListStreamer).
        THIS-OBJECT:imageList1:TransparentColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:imageList1:Images:SetKeyName(0, "blank.gif":U).
        THIS-OBJECT:imageList1:Images:SetKeyName(1, "filter.gif":U).
        /*  */
        /* UltraBusinessEntityTesterFormDesigner */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(1214, 564).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:Form_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Form_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Form_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Form_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Form_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "UltraBusinessEntityTesterFormDesigner":U.
        THIS-OBJECT:Text = "Business Entity Tester":U.
        THIS-OBJECT:WindowPositionRegistryKey = "UltraBusinessEntityTesterFormDesigner":U.
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:Form_Fill_Panel:ResumeLayout(FALSE).
        THIS-OBJECT:Form_Fill_Panel:PerformLayout().
        CAST(THIS-OBJECT:tbCustomParameter, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown handler of the Text boxes with an
                 EditorButton (Lookup)
        Notes:   Maps F3 to EditorButtonClick
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID LookupKeyDownHandler (sender AS System.Object,
                                              e AS System.Windows.Forms.KeyEventArgs):

        DEFINE VARIABLE cControlName AS CHARACTER NO-UNDO.

        IF e:Handled = FALSE AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, SmartLookupSettings:ShortcutKey)
            AND THIS-OBJECT:Enabled THEN DO:

            ASSIGN cControlName = CAST (sender, System.Windows.Forms.Control):Name .

            DYNAMIC-INVOKE (THIS-OBJECT,
                            SUBSTITUTE ("&1_EditorButtonClick":U, cControlName),
                            sender,
                            NEW EditorButtonEventArgs (?, ?)) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the tbCustomParameter
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbCustomParameter_EditorButtonClick (sender AS System.Object,
                                                             e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        ASSIGN tbCustomParameter:Text = "~{":U + System.Environment:NewLine +
                                        "  ~"SerializedType~": ~"Package.TypeName~",":U + System.Environment:NewLine +
                                        "  ~"PropertyName1~": 42,":U + System.Environment:NewLine +
                                        "  ~"PropertyName2~": ~"Value~"":U + System.Environment:NewLine +
                                        "}":U .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolValueChanged event of the ultraToolbarsManager1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraToolbarsManager1_ToolValueChanged (sender AS System.Object,
                                                                e AS Infragistics.Win.UltraWinToolbars.ToolEventArgs):

        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iValue AS INTEGER NO-UNDO.

        IF e:Tool:Key = "BatchSize":U THEN DO:
            ASSIGN cValue = STRING (UNBOX (CAST(e:Tool, Infragistics.Win.UltraWinToolbars.ComboBoxTool):Value))
                   iValue = INTEGER (cValue) NO-ERROR .

            IF iValue > 0 THEN
                CAST(e:Tool, Infragistics.Win.UltraWinToolbars.ComboBoxTool):Value = BOX(iValue) .
            ELSE
                CAST(e:Tool, Infragistics.Win.UltraWinToolbars.ComboBoxTool):Value = BOX(0) .

        END.

    END METHOD.



    DESTRUCTOR PUBLIC UltraBusinessEntityTesterFormDesigner ():
        IF VALID-OBJECT (components) THEN DO:
            CAST (components, System.IDisposable):Dispose ().
        END.
    END DESTRUCTOR.

END CLASS.