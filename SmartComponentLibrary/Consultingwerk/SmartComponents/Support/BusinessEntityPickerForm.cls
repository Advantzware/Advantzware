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
    File        : BusinessEntityPickerForm
    Purpose     : Selectes a Business Entity
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 14 14:18:42 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm INHERITS Form:

    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE btnRefresh AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE buttonOK AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE buttonCancel AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE label1 AS System.Windows.Forms.Label NO-UNDO.
    DEFINE PRIVATE VARIABLE lbRemarks AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE pictureBoxHeader AS System.Windows.Forms.PictureBox NO-UNDO.
    DEFINE PRIVATE VARIABLE textBox1 AS System.Windows.Forms.TextBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGrid1 AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO.

    {Consultingwerk/Util/TempTables/ttClassNames.i &ACCESS="PRIVATE"}

    DEFINE PRIVATE QUERY qClassNames FOR ttClassNames SCROLLING .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the Refresh Button has been clicked
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT RefreshButtonClick DELEGATE System.EventHandler .

    /*------------------------------------------------------------------------------
        Purpose: Toggles the visibility of the Refresh Button
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RefreshButtonVisible AS LOGICAL NO-UNDO
    GET:
        RETURN btnRefresh:Visible .
    END GET.
    SET (arg AS LOGICAL):
        ASSIGN btnRefresh:Visible = arg .
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the text shown in the "Remarks" field
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Remarks AS CHARACTER NO-UNDO
    GET:
        RETURN lbRemarks:Text .
    END GET.
    SET (arg AS CHARACTER):
        ASSIGN lbRemarks:Text = arg .
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessEntityPickerForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessEntityPickerForm ():

        SUPER().
        InitializeComponent().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose:
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnRefresh_Click (sender AS System.Object,
                                          e AS System.EventArgs):

        THIS-OBJECT:OnRefreshButtonClick (System.EventArgs:Empty) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the selected Business Entity Name
        Notes:
        @return The selected Business Entity Name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetSelectedBusinessEntityName ():

        IF AVAILABLE ttClassNames THEN
            RETURN ttClassNames.ClassName .
        ELSE
            RETURN ? .

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
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("Table":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("Table":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ClassName":U, -1, ?, 0, Infragistics.Win.UltraWinGrid.SortIndicator:Ascending, FALSE).
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
        THIS-OBJECT:buttonCancel = NEW System.Windows.Forms.Button().
        THIS-OBJECT:buttonOK = NEW System.Windows.Forms.Button().
        THIS-OBJECT:pictureBoxHeader = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:ultraGrid1 = NEW Infragistics.Win.UltraWinGrid.UltraGrid().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:label1 = NEW System.Windows.Forms.Label().
        THIS-OBJECT:textBox1 = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:lbRemarks = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:btnRefresh = NEW System.Windows.Forms.Button().
        CAST(THIS-OBJECT:pictureBoxHeader, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraGrid1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:panel1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* buttonCancel */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:buttonCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:buttonCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:buttonCancel:Location = NEW System.Drawing.Point(718, 414).
        THIS-OBJECT:buttonCancel:Name = "buttonCancel":U.
        THIS-OBJECT:buttonCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:buttonCancel:TabIndex = 5.
        THIS-OBJECT:buttonCancel:Text = "&Cancel":U.
        THIS-OBJECT:buttonCancel:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:buttonCancel:UseVisualStyleBackColor = TRUE.
        /*  */
        /* buttonOK */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:buttonOK:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:buttonOK:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:buttonOK:Location = NEW System.Drawing.Point(637, 414).
        THIS-OBJECT:buttonOK:Name = "buttonOK":U.
        THIS-OBJECT:buttonOK:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:buttonOK:TabIndex = 4.
        THIS-OBJECT:buttonOK:Text = "&OK":U.
        THIS-OBJECT:buttonOK:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:buttonOK:UseVisualStyleBackColor = TRUE.
        /*  */
        /* pictureBoxHeader */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:pictureBoxHeader:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:pictureBoxHeader:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:pictureBoxHeader:Cursor = System.Windows.Forms.Cursors:Hand.
        THIS-OBJECT:pictureBoxHeader:Image = CAST(resources:GetObject("pictureBoxHeader.Image":U), System.Drawing.Image).
        THIS-OBJECT:pictureBoxHeader:Location = NEW System.Drawing.Point(404, 6).
        THIS-OBJECT:pictureBoxHeader:Name = "pictureBoxHeader":U.
        THIS-OBJECT:pictureBoxHeader:Size = NEW System.Drawing.Size(389, 70).
        THIS-OBJECT:pictureBoxHeader:SizeMode = System.Windows.Forms.PictureBoxSizeMode:AutoSize.
        THIS-OBJECT:pictureBoxHeader:TabIndex = 3.
        THIS-OBJECT:pictureBoxHeader:TabStop = FALSE.
        THIS-OBJECT:pictureBoxHeader:Click:Subscribe(THIS-OBJECT:pictureBoxHeader_Click).
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar0 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Progress.Data.ColumnPropDesc EXTENT 1 NO-UNDO.
        arrayvar1[1] = NEW Progress.Data.ColumnPropDesc("ClassName":U, "Class name":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar1.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* ultraGrid1 */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraGrid1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraGrid1:DataSource = THIS-OBJECT:bindingSource1.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Appearance = appearance1.
        THIS-OBJECT:ultraGrid1:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ExtendLastColumn.
        ultraGridColumn1:Header:VisiblePosition = 0.
        ultraGridColumn1:Width = 726.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS System.Object EXTENT 1 NO-UNDO.
        arrayvar2[1] = ultraGridColumn1.
        ultraGridBand1:Columns:AddRange(arrayvar2).
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
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:AllowRowFiltering = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:AllowRowSummaries = Infragistics.Win.UltraWinGrid.AllowRowSummaries:False.
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
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:ultraGrid1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:ultraGrid1:Location = NEW System.Drawing.Point(12, 88).
        THIS-OBJECT:ultraGrid1:Name = "ultraGrid1":U.
        THIS-OBJECT:ultraGrid1:Size = NEW System.Drawing.Size(781, 317).
        THIS-OBJECT:ultraGrid1:TabIndex = 6.
        THIS-OBJECT:ultraGrid1:Text = "ultraGrid1":U.
        THIS-OBJECT:ultraGrid1:DoubleClickRow:Subscribe(THIS-OBJECT:ultraGrid1_DoubleClickRow).
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:label1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:textBox1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(805, 409).
        THIS-OBJECT:panel1:TabIndex = 7.
        /*  */
        /* label1 */
        /*  */
        THIS-OBJECT:label1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:label1:Location = NEW System.Drawing.Point(12, 58).
        THIS-OBJECT:label1:Name = "label1":U.
        THIS-OBJECT:label1:Size = NEW System.Drawing.Size(116, 23).
        THIS-OBJECT:label1:TabIndex = 1.
        THIS-OBJECT:label1:Text = "Search class name:":U.
        THIS-OBJECT:label1:UseCompatibleTextRendering = TRUE.
        /*  */
        /* textBox1 */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:textBox1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:textBox1:Location = NEW System.Drawing.Point(134, 55).
        THIS-OBJECT:textBox1:Name = "textBox1":U.
        THIS-OBJECT:textBox1:Size = NEW System.Drawing.Size(247, 20).
        THIS-OBJECT:textBox1:TabIndex = 0.
        THIS-OBJECT:textBox1:TextChanged:Subscribe(THIS-OBJECT:textBox1_TextChanged).
        THIS-OBJECT:textBox1:KeyDown:Subscribe(THIS-OBJECT:textBox1_KeyDown).
        /*  */
        /* lbRemarks */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Bottom, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:lbRemarks:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:lbRemarks:Location = NEW System.Drawing.Point(12, 419).
        THIS-OBJECT:lbRemarks:Name = "lbRemarks":U.
        THIS-OBJECT:lbRemarks:Size = NEW System.Drawing.Size(619, 23).
        THIS-OBJECT:lbRemarks:TabIndex = 8.
        THIS-OBJECT:lbRemarks:WrapText = FALSE.
        /*  */
        /* btnRefresh */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnRefresh:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnRefresh:Location = NEW System.Drawing.Point(12, 414).
        THIS-OBJECT:btnRefresh:Name = "btnRefresh":U.
        THIS-OBJECT:btnRefresh:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnRefresh:TabIndex = 9.
        THIS-OBJECT:btnRefresh:Text = "&Refresh":U.
        THIS-OBJECT:btnRefresh:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnRefresh:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:btnRefresh:Visible = FALSE.
        THIS-OBJECT:btnRefresh:Click:Subscribe(THIS-OBJECT:btnRefresh_Click).
        /*  */
        /* BusinessEntityPickerForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:buttonOK.
        THIS-OBJECT:CancelButton = THIS-OBJECT:buttonCancel.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(805, 449).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnRefresh).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:lbRemarks).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraGrid1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:buttonCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:buttonOK).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:pictureBoxHeader).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MinimumSize = NEW System.Drawing.Size(821, 487).
        THIS-OBJECT:Name = "BusinessEntityPickerForm":U.
        THIS-OBJECT:ShowIcon = FALSE.
        THIS-OBJECT:Text = "Business Entity Picker":U.
        CAST(THIS-OBJECT:pictureBoxHeader, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraGrid1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the RefreshButtonClick
        Notes:
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnRefreshButtonClick (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:RefreshButtonClick:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown (e) .

        THIS-OBJECT:textBox1:Focus () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the pictureBoxHeader
        Notes:
        @param sender The reference to the object Object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID pictureBoxHeader_Click (sender AS System.Object,
                                                e AS System.EventArgs):

        Consultingwerk.Windows.API.Win32:ShellExecute ("open":U, "http://www.facebook.com/Consultingwerk":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Receives the temp-table with the business entity names
        Notes:
        @param ttClassNames The temp-table with the class names
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ReceiveBusinessEntityNames (TABLE ttClassNames):

        OPEN QUERY qClassNames PRESELECT EACH ttClassNames NO-LOCK .

        THIS-OBJECT:ultraGrid1:Selected:Rows:Clear () .

        bindingSource1:HANDLE = QUERY qClassNames:HANDLE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown event of the textBox1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID textBox1_KeyDown (sender AS System.Object,
                                          e AS System.Windows.Forms.KeyEventArgs):

        IF Progress.Util.EnumHelper:AreEqual (e:KeyCode,
                                              System.Windows.Forms.Keys:Down) THEN

            THIS-OBJECT:ultraGrid1:Focus () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the TextBox1
        Notes:   Re-Opens the Query using MATCHES
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID textBox1_TextChanged (sender AS System.Object,
                                              e AS System.EventArgs):

        OPEN QUERY qClassNames PRESELECT EACH ttClassNames NO-LOCK
            WHERE ttClassNames.ClassName MATCHES SUBSTITUTE ("*&1*":U,
                                                             textBox1:Text).

        bindingSource1:RefreshAll() .

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
        Purpose: Destructor for the BusinessEntityPickerForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC BusinessEntityPickerForm ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
