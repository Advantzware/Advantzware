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
    File        : ViewObjectsForm
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Mar 27 15:31:26 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .
USING Progress.Windows.*    FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Util.Forms.ViewObjectsForm
    INHERITS Form:

    DEFINE PRIVATE VARIABLE m_ViewObjectsForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ViewObjectsForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ViewObjectsForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ViewObjectsForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ViewObjectsForm_Fill_Panel AS System.Windows.Forms.Panel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ViewObjectsForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ViewObjectsForm ():

        SUPER().
        InitializeComponent().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Clears the output
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID Clear ():

        THIS-OBJECT:ultraTextEditor1:Clear() .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Closes the Form
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID Exit ():

        THIS-OBJECT:Exit () .

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
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Refresh":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Clear":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup2":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE textBoxTool4 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
        textBoxTool4 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("tbFilter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("Objects":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Refresh":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Clear":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("Filter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE textBoxTool1 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
        textBoxTool1 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("tbFilter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar3 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar3 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("UltraToolbar1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool1 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool1 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Objects":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Refresh":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Clear":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE textBoxTool2 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
        textBoxTool2 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("tbFilter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool2 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool2 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Objects":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Refresh":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Clear":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE textBoxTool3 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
        textBoxTool3 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("tbFilter":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Util.Forms.ViewObjectsForm":U).
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:ViewObjectsForm_Fill_Panel = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ViewObjectsForm_Fill_Panel:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTextEditor1 */
        /*  */
        THIS-OBJECT:ultraTextEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:ultraTextEditor1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraTextEditor1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraTextEditor1:MaxLength = 327670000.
        THIS-OBJECT:ultraTextEditor1:Multiline = TRUE.
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:ReadOnly = TRUE.
        THIS-OBJECT:ultraTextEditor1:Scrollbars = System.Windows.Forms.ScrollBars:Both.
        THIS-OBJECT:ultraTextEditor1:Size = NEW System.Drawing.Size(696, 427).
        THIS-OBJECT:ultraTextEditor1:TabIndex = 0.
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Progress.Windows.Form":U).
        ribbonTab1:Caption = "Objects":U.
        ribbonGroup1:Caption = "Objects":U.
        buttonTool9:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool10:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar0[1] = buttonTool9.
        arrayvar0[2] = buttonTool10.
        ribbonGroup1:Tools:AddRange(arrayvar0).
        ribbonGroup2:Caption = "Filter":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar1[1] = textBoxTool4.
        ribbonGroup2:Tools:AddRange(arrayvar1).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 2 NO-UNDO.
        arrayvar2[1] = ribbonGroup1.
        arrayvar2[2] = ribbonGroup2.
        ribbonTab1:Groups:AddRange(arrayvar2).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar3[1] = ribbonTab1.
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar3).
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        THIS-OBJECT:ultraToolbarsManager1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2007.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar4[1] = buttonTool1.
        arrayvar4[2] = buttonTool2.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar4).
        ultraToolbar1:Text = "Objects":U.
        ultraToolbar2:DockedColumn = 1.
        ultraToolbar2:DockedRow = 1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar5[1] = textBoxTool1.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar5).
        ultraToolbar2:Text = "Filter":U.
        ultraToolbar3:DockedColumn = 0.
        ultraToolbar3:DockedRow = 0.
        ultraToolbar3:IsMainMenuBar = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar6[1] = popupMenuTool1.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar6).
        ultraToolbar3:Text = "UltraToolbar1":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 3 NO-UNDO.
        arrayvar7[1] = ultraToolbar1.
        arrayvar7[2] = ultraToolbar2.
        arrayvar7[3] = ultraToolbar3.
        THIS-OBJECT:ultraToolbarsManager1:Toolbars:AddRange(arrayvar7).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        buttonTool3:SharedPropsInternal:AppearancesLarge:Appearance = appearance1.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool3:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        buttonTool3:SharedPropsInternal:Caption = "Refresh":U.
        buttonTool3:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesLarge:Appearance = appearance3.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesSmall:Appearance = appearance4.
        buttonTool4:SharedPropsInternal:Caption = "Clear":U.
        buttonTool4:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        textBoxTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance5.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        textBoxTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance6.
        textBoxTool2:SharedPropsInternal:Caption = "Filter":U.
        textBoxTool2:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        textBoxTool2:SharedPropsInternal:Width = 250.
        popupMenuTool2:SharedPropsInternal:Caption = "&Objects":U.
        textBoxTool3:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar8[1] = buttonTool5.
        arrayvar8[2] = buttonTool6.
        arrayvar8[3] = textBoxTool3.
        arrayvar8[4] = buttonTool7.
        popupMenuTool2:Tools:AddRange(arrayvar8).
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesLarge:Appearance = appearance7.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image":U), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesSmall:Appearance = appearance8.
        buttonTool8:SharedPropsInternal:Caption = "Exit":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 5 NO-UNDO.
        arrayvar9[1] = buttonTool3.
        arrayvar9[2] = buttonTool4.
        arrayvar9[3] = textBoxTool2.
        arrayvar9[4] = popupMenuTool2.
        arrayvar9[5] = buttonTool8.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar9).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        /*  */
        /* ViewObjectsForm_Fill_Panel */
        /*  */
        THIS-OBJECT:ViewObjectsForm_Fill_Panel:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:ViewObjectsForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:ViewObjectsForm_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ViewObjectsForm_Fill_Panel:Location = NEW System.Drawing.Point(0, 52).
        THIS-OBJECT:ViewObjectsForm_Fill_Panel:Name = "ViewObjectsForm_Fill_Panel":U.
        THIS-OBJECT:ViewObjectsForm_Fill_Panel:Size = NEW System.Drawing.Size(696, 427).
        THIS-OBJECT:ViewObjectsForm_Fill_Panel:TabIndex = 0.
        /*  */
        /* m_ViewObjectsForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 52).
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Left:Name = "m_ViewObjectsForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(0, 427).
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ViewObjectsForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(696, 52).
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Right:Name = "m_ViewObjectsForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(0, 427).
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ViewObjectsForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Top:Name = "m_ViewObjectsForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(696, 52).
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ViewObjectsForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 479).
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Bottom:Name = "m_ViewObjectsForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(696, 0).
        THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* ViewObjectsForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(696, 479).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ViewObjectsForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ViewObjectsForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "ViewObjectsForm":U.
        THIS-OBJECT:Text = "Loaded objects":U.
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ViewObjectsForm_Fill_Panel:ResumeLayout(FALSE).
        THIS-OBJECT:ViewObjectsForm_Fill_Panel:PerformLayout().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnLoad (e AS System.EventArgs):

        SUPER:OnLoad(INPUT e).

        THIS-OBJECT:RefreshObjects () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown (e) .

        IF VALID-OBJECT (THIS-OBJECT:ultraToolbarsManager1:MdiParentManager) AND
           THIS-OBJECT:ultraToolbarsManager1:MdiParentManager:Ribbon:Visible = TRUE THEN

           THIS-OBJECT:ultraToolbarsManager1:MdiParentManager:Ribbon:SelectedTab =
                THIS-OBJECT:ultraToolbarsManager1:Ribbon:Tabs[0]:AttachedParentTab .


    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the List of objects
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RefreshObjects ():

        DEFINE VARIABLE oObject AS Progress.Lang.Object NO-UNDO .
        DEFINE VARIABLE cLine   AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER              NO-UNDO .
        DEFINE VARIABLE cFilter AS CHARACTER            NO-UNDO .

        THIS-OBJECT:Clear () .

        ASSIGN cFilter = CAST (ultraToolbarsManager1:Tools["tbFilter":U],
                               Infragistics.Win.UltraWinToolbars.TextBoxTool):Text .

        oObject = SESSION:FIRST-OBJECT .

        DO WHILE VALID-OBJECT (oObject) ON ERROR UNDO, NEXT:

            ASSIGN i = i + 1
                   cLine = "":U  .

            IF TYPE-OF (oObject, System.Object) THEN
                ASSIGN cLine = CAST (oObject, System.Object):GetType ():FullName + " ":U .
            ELSE DO:
                DEFINE VARIABLE oClass AS Progress.Lang.Class NO-UNDO .

/*                ASSIGN cLine = STRING (oObject:GetClass()) .*/

                /* Mike Fechner, Consultingwerk Ltd. 28.03.2011
                   Accessing GetClass() may lead to crash - looks
                   like an OpenEdge issue! */

                oClass = oObject:GetClass () .

                ASSIGN cLine = "ABL":U + " (":U + oClass:TypeName + ")":U.
            END.

            IF TYPE-OF (oObject, System.Windows.Forms.Control) AND oObject <> THIS-OBJECT:ultraTextEditor1 THEN
                cLine = SUBSTITUTE ("&1 ~"&2~"":U, cLine, CAST (oObject, System.Windows.Forms.Control):Text) .

            ASSIGN cLine = SUBSTITUTE ("&1 &2":U,
                                       cLine,
                                       oObject:ToString ()) .

            IF cFilter = "":U OR cLine MATCHES SUBSTITUTE ("*&1*":U, cFilter) THEN DO:

                ASSIGN cLine = SUBSTITUTE ("&1 &2":U,
                                           i,
                                           cLine) .

                THIS-OBJECT:ultraTextEditor1:AppendText (cLine +
                                                         System.Environment:NewLine) .

            END.

            FINALLY:
                oObject = oObject:NEXT-SIBLING .
            END FINALLY.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the UltraToolbarsManager
                 intance
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraToolbarsManager1_ToolClick (sender AS System.Object,
                                                         e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):

        CASE e:Tool:Key:
            WHEN "Refresh":U THEN
                THIS-OBJECT:RefreshObjects () .
            WHEN "Clear":U THEN
                THIS-OBJECT:Clear () .
            WHEN "Exit":U THEN
                THIS-OBJECT:Exit () .
        END CASE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ViewObjectsForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ViewObjectsForm ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
