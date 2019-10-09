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
    File        : ProjectMinimizerForm
    Purpose     : Provides an user interface that provides a minimized
                  file set to run a program function
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 03 08:31:09 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Forms.*                   FROM PROPATH .
USING Consultingwerk.Framework.*               FROM PROPATH .
USING Consultingwerk.Studio.ProjectMinimizer.* FROM PROPATH .
USING Consultingwerk.Util.*                    FROM PROPATH .
USING Progress.Lang.*                          FROM PROPATH .

CLASS Consultingwerk.Studio.ProjectMinimizer.ProjectMinimizerForm
&IF DEFINED (DotNetAccessible) NE 0 &THEN
    INHERITS BaseForm
&ENDIF
    :

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ProjectMinimizerForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ProjectMinimizerForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ProjectMinimizerForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ProjectMinimizerForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGrid1 AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraStatusBar1 AS Infragistics.Win.UltraWinStatusBar.UltraStatusBar NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
    DEFINE PRIVATE VARIABLE ProjectMinimizerForm_Fill_Panel AS System.Windows.Forms.Panel NO-UNDO.

    DEFINE VARIABLE oParser AS LogFileParser NO-UNDO .

    DEFINE VARIABLE oProgressBarInfo AS Infragistics.Win.UltraWinStatusBar.ProgressBarInfo NO-UNDO .

    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ProjectMinimizerForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProjectMinimizerForm ():

        SUPER().
        InitializeComponent().

        oProgressBarInfo = THIS-OBJECT:ultraStatusBar1:Panels ["progress":U]:ProgressBarInfo .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Closes the log file
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CloseLogFile ():

        THIS-OBJECT:ultraStatusBar1:Panels ["progress":U]:Visible = FALSE .
        SetStatus ("Log file closed."{&TRAN}) .

        THIS-OBJECT:ultraToolbarsManager1:Tools["Open":U]:SharedProps:Enabled = TRUE .
        THIS-OBJECT:ultraToolbarsManager1:Tools["Close":U]:SharedProps:Enabled = FALSE .

        hQuery:QUERY-PREPARE ("for each ttProcedures where RunName = ?":U) .
        hQuery:QUERY-OPEN ().

        bindingSource1:RefreshAll() .

        ultraGrid1:Enabled = FALSE .

        IF VALID-OBJECT (oParser) THEN
            DELETE OBJECT oParser .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Closes the Form
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID Exit ():

        THIS-OBJECT:Close () .

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
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Close":U).
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
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Close":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Studio.ProjectMinimizer.ProjectMinimizerForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Close":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ttProcedures":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("RunName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FullPathName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("IsClass":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("Found":U).
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
        tableDesc1 = NEW Progress.Data.TableDesc("ttProcedures":U).
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
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:ProjectMinimizerForm_Fill_Panel = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:ultraGrid1 = NEW Infragistics.Win.UltraWinGrid.UltraGrid().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:ultraStatusBar1 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusBar().
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ProjectMinimizerForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:ultraGrid1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraStatusBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.Forms.BaseForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar0[1] = buttonTool7.
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar0).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar1[1] = buttonTool5.
        arrayvar1[2] = buttonTool6.
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar1).
        ribbonTab1:Caption = "Start":U.
        ribbonGroup1:Caption = "Logfile":U.
        buttonTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool2:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar2[1] = buttonTool1.
        arrayvar2[2] = buttonTool2.
        ribbonGroup1:Tools:AddRange(arrayvar2).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 1 NO-UNDO.
        arrayvar3[1] = ribbonGroup1.
        ribbonTab1:Groups:AddRange(arrayvar3).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar4[1] = ribbonTab1.
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar4).
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:Visible = TRUE.
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        buttonTool3:SharedPropsInternal:AppearancesLarge:Appearance = appearance13.
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        buttonTool3:SharedPropsInternal:AppearancesSmall:Appearance = appearance14.
        buttonTool3:SharedPropsInternal:Caption = "Open":U.
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesLarge:Appearance = appearance15.
        appearance16:Image = CAST(resources:GetObject("appearance16.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesSmall:Appearance = appearance16.
        buttonTool4:SharedPropsInternal:Caption = "Close":U.
        buttonTool8:SharedPropsInternal:Caption = "Exit":U.
        buttonTool8:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:TextOnlyAlways.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar5[1] = buttonTool3.
        arrayvar5[2] = buttonTool4.
        arrayvar5[3] = buttonTool8.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar5).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        /*  */
        /* ProjectMinimizerForm_Fill_Panel */
        /*  */
        THIS-OBJECT:ProjectMinimizerForm_Fill_Panel:Controls:Add(THIS-OBJECT:ultraGrid1).
        THIS-OBJECT:ProjectMinimizerForm_Fill_Panel:Controls:Add(THIS-OBJECT:ultraStatusBar1).
        THIS-OBJECT:ProjectMinimizerForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:ProjectMinimizerForm_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ProjectMinimizerForm_Fill_Panel:Location = NEW System.Drawing.Point(8, 162).
        THIS-OBJECT:ProjectMinimizerForm_Fill_Panel:Name = "ProjectMinimizerForm_Fill_Panel":U.
        THIS-OBJECT:ProjectMinimizerForm_Fill_Panel:Size = NEW System.Drawing.Size(905, 346).
        THIS-OBJECT:ProjectMinimizerForm_Fill_Panel:TabIndex = 0.
        /*  */
        /* ultraGrid1 */
        /*  */
        THIS-OBJECT:ultraGrid1:DataSource = THIS-OBJECT:bindingSource1.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Appearance = appearance1.
        ultraGridColumn1:Header:Caption = "Run Name":U.
        ultraGridColumn1:Header:VisiblePosition = 0.
        ultraGridColumn1:Width = 236.
        ultraGridColumn2:Header:Caption = "Full Path":U.
        ultraGridColumn2:Header:VisiblePosition = 1.
        ultraGridColumn2:Width = 369.
        ultraGridColumn3:Header:Caption = "Class":U.
        ultraGridColumn3:Header:VisiblePosition = 2.
        ultraGridColumn4:Header:Caption = "Found":U.
        ultraGridColumn4:Header:VisiblePosition = 3.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS System.Object EXTENT 4 NO-UNDO.
        arrayvar6[1] = ultraGridColumn1.
        arrayvar6[2] = ultraGridColumn2.
        arrayvar6[3] = ultraGridColumn3.
        arrayvar6[4] = ultraGridColumn4.
        ultraGridBand1:Columns:AddRange(arrayvar6).
        THIS-OBJECT:ultraGrid1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:ultraGrid1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:ultraGrid1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance2:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:Appearance = appearance2.
        appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
        THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance4:BackColor2 = System.Drawing.SystemColors:Control.
        appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
        THIS-OBJECT:ultraGrid1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:ultraGrid1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance5:BackColor = System.Drawing.SystemColors:Window.
        appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:ActiveCellAppearance = appearance5.
        appearance6:BackColor = System.Drawing.SystemColors:Highlight.
        appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:ActiveRowAppearance = appearance6.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:AllowGroupBy = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance7:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CardAreaAppearance = appearance7.
        appearance8:BorderColor = System.Drawing.Color:Silver.
        appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CellAppearance = appearance8.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CellPadding = 0.
        appearance9:BackColor = System.Drawing.SystemColors:Control.
        appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance9:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:GroupByRowAppearance = appearance9.
        appearance10:TextHAlignAsString = "Left":U.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:HeaderAppearance = appearance10.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance11:BackColor = System.Drawing.SystemColors:Window.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:RowAppearance = appearance11.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:SelectTypeGroupByRow = Infragistics.Win.UltraWinGrid.SelectType:None.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Extended.
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:ultraGrid1:DisplayLayout:TabNavigation = Infragistics.Win.UltraWinGrid.TabNavigation:NextControlOnLastCell.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:ultraGrid1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraGrid1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:ultraGrid1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraGrid1:Name = "ultraGrid1":U.
        THIS-OBJECT:ultraGrid1:Size = NEW System.Drawing.Size(905, 323).
        THIS-OBJECT:ultraGrid1:TabIndex = 1.
        THIS-OBJECT:ultraGrid1:Text = "ultraGrid1":U.
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:AllowEdit = FALSE.
        THIS-OBJECT:bindingSource1:AllowNew = FALSE.
        THIS-OBJECT:bindingSource1:AllowRemove = FALSE.
        THIS-OBJECT:bindingSource1:AutoSort = TRUE.
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar7 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar7.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Progress.Data.ColumnPropDesc EXTENT 4 NO-UNDO.
        arrayvar8[1] = NEW Progress.Data.ColumnPropDesc("RunName":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar8[2] = NEW Progress.Data.ColumnPropDesc("FullPathName":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar8[3] = NEW Progress.Data.ColumnPropDesc("IsClass":U, "":U, Progress.Data.DataType:LOGICAL).
        arrayvar8[4] = NEW Progress.Data.ColumnPropDesc("Found":U, "":U, Progress.Data.DataType:LOGICAL).
        tableDesc1:Columns = arrayvar8.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* ultraStatusBar1 */
        /*  */
        THIS-OBJECT:ultraStatusBar1:Location = NEW System.Drawing.Point(0, 323).
        THIS-OBJECT:ultraStatusBar1:Name = "ultraStatusBar1":U.
        ultraStatusPanel1:Key = "status":U.
        ultraStatusPanel1:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Spring.
        ultraStatusPanel1:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel2:Key = "progress":U.
        ultraStatusPanel2:ProgressBarInfo:Style = Infragistics.Win.UltraWinProgressBar.ProgressBarStyle:Office2007Continuous.
        ultraStatusPanel2:Style = Infragistics.Win.UltraWinStatusBar.PanelStyle:Progress.
        ultraStatusPanel2:Width = 150.
        ultraStatusPanel2:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel3:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel3:Style = Infragistics.Win.UltraWinStatusBar.PanelStyle:Date.
        ultraStatusPanel3:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel4:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel4:Style = Infragistics.Win.UltraWinStatusBar.PanelStyle:Time.
        ultraStatusPanel4:WrapText = Infragistics.Win.DefaultableBoolean:False.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel EXTENT 4 NO-UNDO.
        arrayvar9[1] = ultraStatusPanel1.
        arrayvar9[2] = ultraStatusPanel2.
        arrayvar9[3] = ultraStatusPanel3.
        arrayvar9[4] = ultraStatusPanel4.
        THIS-OBJECT:ultraStatusBar1:Panels:AddRange(arrayvar9).
        THIS-OBJECT:ultraStatusBar1:Size = NEW System.Drawing.Size(905, 23).
        THIS-OBJECT:ultraStatusBar1:TabIndex = 0.
        THIS-OBJECT:ultraStatusBar1:Text = "ultraStatusBar1":U.
        THIS-OBJECT:ultraStatusBar1:ViewStyle = Infragistics.Win.UltraWinStatusBar.ViewStyle:Office2007.
        /*  */
        /* m_ProjectMinimizerForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 162).
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Left:Name = "m_ProjectMinimizerForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 346).
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ProjectMinimizerForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(913, 162).
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Right:Name = "m_ProjectMinimizerForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 346).
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ProjectMinimizerForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Top:Name = "m_ProjectMinimizerForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(921, 162).
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ProjectMinimizerForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 508).
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Bottom:Name = "m_ProjectMinimizerForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(921, 8).
        THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* openFileDialog1 */
        /*  */
        THIS-OBJECT:openFileDialog1:DefaultExt = "log":U.
        THIS-OBJECT:openFileDialog1:Filter = "Log files|*.log|All files|*.*":U.
        THIS-OBJECT:openFileDialog1:RestoreDirectory = TRUE.
        THIS-OBJECT:openFileDialog1:Title = "Open client logfile":U.
        /*  */
        /* ProjectMinimizerForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(921, 516).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ProjectMinimizerForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProjectMinimizerForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Name = "ProjectMinimizerForm":U.
        THIS-OBJECT:Text = "Project Minimizer":U.
        THIS-OBJECT:WindowPositionRegistryKey = "ProjectMinimizerForm":U.
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ProjectMinimizerForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraGrid1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraStatusBar1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Opens a log file
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OpenLogFile ():

        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .

        WAIT-FOR openFileDialog1:ShowDialog (THIS-OBJECT) SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        ultraToolbarsManager1:Enabled = FALSE .

        SetStatus (openFileDialog1:FileName) .

        THIS-OBJECT:ultraToolbarsManager1:Tools["Open":U]:SharedProps:Enabled = FALSE .
        THIS-OBJECT:ultraToolbarsManager1:Tools["Close":U]:SharedProps:Enabled = TRUE .

        ASSIGN oParser = NEW LogFileParser (openFileDialog1:FileName) .

        oParser:ProgressChanged:Subscribe (ParserProgressChangedHandler) .
        oParser:ParseLogFile() .

        hQuery = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (oParser:TempTableBuffer) .

        bindingSource1:HANDLE = hQuery .

        ultraGrid1:Enabled = TRUE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .

            CloseLogFile() .
        END CATCH.

        FINALLY:
            ultraToolbarsManager1:Enabled = TRUE .
            THIS-OBJECT:ultraStatusBar1:Panels ["progress":U]:Visible = FALSE .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ProgressChangedHandler event of the parser instance
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ProgressChangedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ParserProgressChangedHandler (sender AS Progress.Lang.Object,
                                                        e AS ProgressChangedEventArgs):

        ASSIGN
            oProgressBarInfo:Value = e:Value
            oProgressBarInfo:Maximum= e:Maximum .

        PROCESS EVENTS .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets the status bar text
        Notes:
        @param pcStatus The text for the status bar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetStatus (pcStatus AS CHARACTER):

        THIS-OBJECT:ultraStatusBar1:Panels ["status":U]:Text = pcStatus .
        PROCESS EVENTS .

    END METHOD.

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
                OpenLogFile () .

            WHEN "Close":U THEN
                CloseLogFile () .

            WHEN "Exit":U THEN
                Exit () .

        END CASE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ProjectMinimizerForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ProjectMinimizerForm ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.
&ENDIF
END CLASS.
