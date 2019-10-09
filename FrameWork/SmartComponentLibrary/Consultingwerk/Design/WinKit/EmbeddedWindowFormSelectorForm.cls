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
    File        : EmbeddedWindowFormSelectorForm
    Purpose     : Provides an user interface to select an embedded window
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 12 14:19:45 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.WindowIntegrationKit.Forms.* FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .
USING Progress.Windows.Form                       FROM ASSEMBLY .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Design.WinKit.EmbeddedWindowFormSelectorForm
    INHERITS Form:

    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGrid1 AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton2 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.

    {Consultingwerk/Design/WinKit/ttEmbeddedWindows.i &ACCESS = "PRIVATE"}

    DEFINE PRIVATE QUERY qEmbeddedWindows FOR ttEmbeddedWindows SCROLLING .

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the selected IEmbeddedWindowForm instance
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SelectedEmbeddedWindowForm AS IEmbeddedWindowForm NO-UNDO
    GET:
        IF NOT AVAILABLE ttEmbeddedWindows THEN
           RETURN ? .

        ELSE RETURN
            CAST (ttEmbeddedWindows.EmbeddedWindow,
                  Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowForm) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the EmbeddedWindowFormSelectorForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC EmbeddedWindowFormSelectorForm (  ):

        SUPER().
        InitializeComponent().
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
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ttEmbeddedWindows":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("WindowTitle":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ClassName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn6 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn6 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ProcedureName":U).
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
        tableDesc1 = NEW Progress.Data.TableDesc("ttEmbeddedWindows":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Design.WinKit.EmbeddedWindowFormSelectorForm":U).
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraGrid1 = NEW Infragistics.Win.UltraWinGrid.UltraGrid().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:ultraButton2 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        CAST(THIS-OBJECT:ultraGrid1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraGrid1).
        THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(1, 23).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(591, 230).
        /*  */
        /* ultraGrid1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraGrid1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraGrid1:DataSource = THIS-OBJECT:bindingSource1.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Appearance = appearance1.
        THIS-OBJECT:ultraGrid1:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ResizeAllColumns.
        ultraGridColumn4:Header:VisiblePosition = 0.
        ultraGridColumn4:Width = 184.
        ultraGridColumn5:Header:VisiblePosition = 1.
        ultraGridColumn5:Width = 178.
        ultraGridColumn6:Header:VisiblePosition = 2.
        ultraGridColumn6:Width = 221.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS System.Object EXTENT 3 NO-UNDO.
        arrayvar0[1] = ultraGridColumn4.
        arrayvar0[2] = ultraGridColumn5.
        arrayvar0[3] = ultraGridColumn6.
        ultraGridBand1:Columns:AddRange(arrayvar0).
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
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:AllowRowFiltering = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:AllowRowSummaries = Infragistics.Win.UltraWinGrid.AllowRowSummaries:True.
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
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:ultraGrid1:DisplayLayout:UseFixedHeaders = TRUE.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:ultraGrid1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:ultraGrid1:Location = NEW System.Drawing.Point(3, 3).
        THIS-OBJECT:ultraGrid1:Name = "ultraGrid1":U.
        THIS-OBJECT:ultraGrid1:Size = NEW System.Drawing.Size(585, 224).
        THIS-OBJECT:ultraGrid1:TabIndex = 0.
        THIS-OBJECT:ultraGrid1:Text = "ultraGrid1":U.
        THIS-OBJECT:ultraGrid1:DoubleClickRow:Subscribe(THIS-OBJECT:ultraGrid1_DoubleClickRow).
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar1 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
        arrayvar2[1] = NEW Progress.Data.ColumnPropDesc("WindowTitle":U, "Window Title":U, Progress.Data.DataType:CHARACTER).
        arrayvar2[2] = NEW Progress.Data.ColumnPropDesc("ClassName":U, ".NET Form":U, Progress.Data.DataType:CHARACTER).
        arrayvar2[3] = NEW Progress.Data.ColumnPropDesc("ProcedureName":U, "Procedure Name":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar2.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* ultraButton2 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton2:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButton2:Location = NEW System.Drawing.Point(532, 274).
        THIS-OBJECT:ultraButton2:Name = "ultraButton2":U.
        THIS-OBJECT:ultraButton2:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton2:TabIndex = 5.
        THIS-OBJECT:ultraButton2:Text = "&Cancel":U.
        /*  */
        /* ultraButton1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ultraButton1:Enabled = FALSE.
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(451, 274).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton1:TabIndex = 4.
        THIS-OBJECT:ultraButton1:Text = "&Ok":U.
        /*  */
        /* ultraTabControl1 */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(595, 256).
        THIS-OBJECT:ultraTabControl1:TabIndex = 3.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        ultraTab1:Text = "Embedded Windows":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 1 NO-UNDO.
        arrayvar3[1] = ultraTab1.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar3).
        THIS-OBJECT:ultraTabControl1:UseAppStyling = FALSE.
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(591, 230).
        /*  */
        /* EmbeddedWindowFormSelectorForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(619, 309).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "EmbeddedWindowFormSelectorForm":U.
        THIS-OBJECT:SizeGripStyle = System.Windows.Forms.SizeGripStyle:Show.
        THIS-OBJECT:Text = "Embedded Window Selection":U.
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraGrid1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Receives the temp-table
        Notes:
        @param ttEmbeddedWindows TABLE The temp-table with the embedded windows
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ReceiveEmbeddedWindows (TABLE ttEmbeddedWindows):

        OPEN QUERY qEmbeddedWindows
            PRESELECT EACH ttEmbeddedWindows .

        bindingSource1:HANDLE = QUERY qEmbeddedWindows:HANDLE.

        IF bindingSource1:COUNT > 0 THEN
            THIS-OBJECT:ultraButton1:Enabled = TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DoubleClickRow event of the ultraGrid1
            Notes:
        @param sender The reference to the object that raised the event
        @param e The DoubleClickRowEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraGrid1_DoubleClickRow (sender AS System.Object,
                                                   e AS Infragistics.Win.UltraWinGrid.DoubleClickRowEventArgs):

        THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:OK .
        THIS-OBJECT:Close () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the EmbeddedWindowFormSelectorForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC EmbeddedWindowFormSelectorForm ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
