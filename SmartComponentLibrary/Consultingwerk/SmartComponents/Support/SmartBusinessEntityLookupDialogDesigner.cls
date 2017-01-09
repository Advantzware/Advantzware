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
    File        : SmartBusinessEntityLookupDialogDesigner
    Purpose     : Designer class for the SmartBusinessEntityLookupDialog
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Apr 07 08:32:59 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*  FROM PROPATH .
USING Progress.Lang.*        FROM ASSEMBLY .
USING Progress.Windows.Form  FROM ASSEMBLY .
USING System.Windows.Forms.* FROM ASSEMBLY .
USING System.Drawing.*       FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Support.SmartBusinessEntityLookupDialogDesigner
    INHERITS Form :

    DEFINE PROTECTED VARIABLE cbFilterField AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PROTECTED VARIABLE button2 AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE button1 AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PROTECTED VARIABLE smartNavigationPanel1 AS Consultingwerk.SmartComponents.Implementation.SmartNavigationPanel NO-UNDO.
    DEFINE PROTECTED VARIABLE smartDataBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser NO-UNDO.
    DEFINE PROTECTED VARIABLE tbSearchField AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the Search Field still contains the Search Prompt
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SearchPromptInSearchField AS LOGICAL INITIAL TRUE NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityLookupDialogDesigner class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityLookupDialogDesigner ():
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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("reset":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.SmartComponents.Support.SmartBusinessEntityLookupDialogDesigner":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton("apply":U).
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
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:button2 = NEW System.Windows.Forms.Button().
        THIS-OBJECT:button1 = NEW System.Windows.Forms.Button().
        THIS-OBJECT:cbFilterField = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:tbSearchField = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartNavigationPanel1 = NEW Consultingwerk.SmartComponents.Implementation.SmartNavigationPanel().
        THIS-OBJECT:smartDataBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:cbFilterField, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbSearchField, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* panel1 */
        /*  */
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:button2).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:button1).
        THIS-OBJECT:panel1:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 417).
        THIS-OBJECT:panel1:Margin = NEW System.Windows.Forms.Padding(0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(630, 37).
        THIS-OBJECT:panel1:TabIndex = 3.
        /*  */
        /* button2 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:button2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:button2:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:button2:Location = NEW System.Drawing.Point(462, 7).
        THIS-OBJECT:button2:Name = "button2":U.
        THIS-OBJECT:button2:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:button2:TabIndex = 0.
        THIS-OBJECT:button2:Text = "OK":U.
        THIS-OBJECT:button2:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:button2:UseVisualStyleBackColor = TRUE.
        /*  */
        /* button1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:button1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:button1:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:button1:Location = NEW System.Drawing.Point(543, 7).
        THIS-OBJECT:button1:Name = "button1":U.
        THIS-OBJECT:button1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:button1:TabIndex = 1.
        THIS-OBJECT:button1:Text = "Cancel":U.
        THIS-OBJECT:button1:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:button1:UseVisualStyleBackColor = TRUE.
        /*  */
        /* cbFilterField */
        /*  */
        THIS-OBJECT:cbFilterField:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:WindowsVista.
        THIS-OBJECT:cbFilterField:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:cbFilterField:LimitToList = TRUE.
        THIS-OBJECT:cbFilterField:Location = NEW System.Drawing.Point(131, 8).
        THIS-OBJECT:cbFilterField:Name = "cbFilterField":U.
        THIS-OBJECT:cbFilterField:Size = NEW System.Drawing.Size(144, 21).
        THIS-OBJECT:cbFilterField:TabIndex = 1.
        /*  */
        /* tbSearchField */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbSearchField:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance1:FontData:ItalicAsString = "True":U.
        appearance1:ForeColor = System.Drawing.Color:Gray.
        THIS-OBJECT:tbSearchField:Appearance = appearance1.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        editorButton1:Appearance = appearance2.
        editorButton1:ButtonStyle = Infragistics.Win.UIElementButtonStyle:WindowsVistaToolbarButton.
        editorButton1:Key = "reset":U.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        editorButton2:Appearance = appearance3.
        editorButton2:ButtonStyle = Infragistics.Win.UIElementButtonStyle:WindowsVistaToolbarButton.
        editorButton2:Key = "apply":U.
        THIS-OBJECT:tbSearchField:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tbSearchField:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:tbSearchField:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:WindowsVista.
        THIS-OBJECT:tbSearchField:Location = NEW System.Drawing.Point(281, 8).
        THIS-OBJECT:tbSearchField:Name = "tbSearchField":U.
        THIS-OBJECT:tbSearchField:Size = NEW System.Drawing.Size(349, 21).
        THIS-OBJECT:tbSearchField:TabIndex = 2.
        THIS-OBJECT:tbSearchField:Text = "Type search pattern here...":U.
        THIS-OBJECT:tbSearchField:Enter:Subscribe(THIS-OBJECT:ultraTextEditor1_Enter).
        THIS-OBJECT:tbSearchField:KeyDown:Subscribe(THIS-OBJECT:tbSearchField_KeyDown).
        /*  */
        /* smartNavigationPanel1 */
        /*  */
        THIS-OBJECT:smartNavigationPanel1:AutoSize = TRUE.
        THIS-OBJECT:smartNavigationPanel1:AutoSizeMode = System.Windows.Forms.AutoSizeMode:GrowAndShrink.
        THIS-OBJECT:smartNavigationPanel1:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:smartNavigationPanel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:smartNavigationPanel1:Margin = NEW System.Windows.Forms.Padding(0).
        THIS-OBJECT:smartNavigationPanel1:Name = "smartNavigationPanel1":U.
        THIS-OBJECT:smartNavigationPanel1:Size = NEW System.Drawing.Size(128, 32).
        THIS-OBJECT:smartNavigationPanel1:TabIndex = 4.
        /*  */
        /* smartDataBrowser1 */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartDataBrowser1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance4:BackColor = System.Drawing.SystemColors:Window.
        appearance4:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Appearance = appearance4.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance5:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance5:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance5:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance5:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:Appearance = appearance5.
        appearance6:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance6.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance7:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance7:BackColor2 = System.Drawing.SystemColors:Control.
        appearance7:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance7:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:PromptAppearance = appearance7.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance8:BackColor = System.Drawing.SystemColors:Window.
        appearance8:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:ActiveCellAppearance = appearance8.
        appearance9:BackColor = System.Drawing.SystemColors:Highlight.
        appearance9:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:ActiveRowAppearance = appearance9.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance10:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CardAreaAppearance = appearance10.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        appearance11:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellAppearance = appearance11.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellPadding = 0.
        appearance12:BackColor = System.Drawing.SystemColors:Control.
        appearance12:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance12:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance12:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance12:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:GroupByRowAppearance = appearance12.
        appearance13:TextHAlignAsString = "Left":U.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderAppearance = appearance13.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance14:BackColor = System.Drawing.SystemColors:Window.
        appearance14:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:RowAppearance = appearance14.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        appearance15:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance15.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:smartDataBrowser1:LinkDataSource = ?.
        THIS-OBJECT:smartDataBrowser1:Location = NEW System.Drawing.Point(0, 35).
        THIS-OBJECT:smartDataBrowser1:Margin = NEW System.Windows.Forms.Padding(0).
        THIS-OBJECT:smartDataBrowser1:Name = "smartDataBrowser1":U.
        THIS-OBJECT:smartDataBrowser1:SettingsKey = "c97a8bb2-cedf-f5b4-e111-d420191adb0a":U.
        THIS-OBJECT:smartDataBrowser1:Size = NEW System.Drawing.Size(630, 382).
        THIS-OBJECT:smartDataBrowser1:TabIndex = 0.
        THIS-OBJECT:smartDataBrowser1:Text = "smartDataBrowser1":U.
        /*  */
        /* SmartBusinessEntityLookupDialogDesigner */
        /*  */
        THIS-OBJECT:CancelButton = THIS-OBJECT:button1.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(630, 454).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbSearchField).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:cbFilterField).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartNavigationPanel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartDataBrowser1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "SmartBusinessEntityLookupDialogDesigner":U.
        THIS-OBJECT:ShowIcon = FALSE.
        THIS-OBJECT:Text = "SmartBusinessEntityLookupDialogDesigner":U.
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:cbFilterField, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbSearchField, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
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

        IF Consultingwerk.Framework.FrameworkSettings:ExtendedTransparency AND
           Consultingwerk.Windows.API.AeroGlass:IsGlassSupported ()  THEN DO:

            SetGlassRegion () .

            Invalidate ().
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the PaintBackground event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnPaintBackground (e AS PaintEventArgs):

        DEFINE VARIABLE clientArea AS System.Drawing.Rectangle NO-UNDO .
        DEFINE VARIABLE b AS Brush NO-UNDO .

        SUPER:OnPaintBackground (e) .

        IF Consultingwerk.Framework.FrameworkSettings:ExtendedTransparency AND
           Consultingwerk.Windows.API.AeroGlass:IsGlassSupported () THEN DO:
            THIS-OBJECT:TransparencyKey = System.Drawing.Color:FromArgb (255, 221, 220, 220) .

            e:Graphics:Clear(System.Drawing.Color:FromArgb (255, 221, 220, 220)) .

            /* put back the original form background for non-glass area */
            clientArea = NEW System.Drawing.Rectangle (0,
                                                       50,
                                                       THIS-OBJECT:ClientRectangle:Width,
                                                       THIS-OBJECT:ClientRectangle:Height) .

            b = NEW SolidBrush (THIS-OBJECT:BackColor).
            e:Graphics:FillRectangle (b, clientArea).
        END.
        ELSE DO:
            e:Graphics:Clear(THIS-OBJECT:BackColor) .

            /* put back the original form background for non-glass area */
            clientArea = NEW System.Drawing.Rectangle (0,
                                                       50,
                                                       THIS-OBJECT:ClientRectangle:Width,
                                                       THIS-OBJECT:ClientRectangle:Height) .

            b = NEW SolidBrush (THIS-OBJECT:BackColor).
            e:Graphics:FillRectangle (b, clientArea).
        END.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Defines the transparent region
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SetGlassRegion ():

        IF Consultingwerk.Framework.FrameworkSettings:ExtendedTransparency AND
           Consultingwerk.Windows.API.AeroGlass:IsGlassSupported () THEN

            Consultingwerk.Windows.API.AeroGlass:SetGlassMargins
                (THIS-OBJECT:Handle:ToInt32(), 0, 0, 50, 0) .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown event of the ultraTextEditor1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbSearchField_KeyDown (sender AS System.Object,
                                               e AS System.Windows.Forms.KeyEventArgs ):

        IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Down) THEN
            THIS-OBJECT:smartDataBrowser1:Focus () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Event handler for the Enter event of the ultraTextEditor1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTextEditor1_Enter (sender AS System.Object,
                                                e AS System.EventArgs):

        IF THIS-OBJECT:SearchPromptInSearchField THEN DO:
            THIS-OBJECT:tbSearchField:TEXT = "":U .
            THIS-OBJECT:tbSearchField:Appearance:ResetFontData() .
            THIS-OBJECT:tbSearchField:Appearance:ResetForeColor() .
        END.

        THIS-OBJECT:tbSearchField:SelectionStart = 0 NO-ERROR .
        THIS-OBJECT:tbSearchField:SelectionLength = LENGTH (THIS-OBJECT:tbSearchField:Text) NO-ERROR .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

        FINALLY:
            THIS-OBJECT:SearchPromptInSearchField = FALSE .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartBusinessEntityLookupDialogDesigner class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartBusinessEntityLookupDialogDesigner ():
        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.
    END DESTRUCTOR.

END CLASS.
