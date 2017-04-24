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
    File        : RichTextEditorControl
    Purpose     : Provides rich text editing capabilities
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Dec 30 22:17:26 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*               FROM PROPATH .
USING Consultingwerk.Windows.Controls.*   FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.* FROM ASSEMBLY .
USING Progress.Lang.*                     FROM PROPATH .
USING Progress.Windows.UserControl        FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Controls.RichTextEditorControl INHERITS UserControl:


    DEFINE PRIVATE VARIABLE m_RichTextEditorControl_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_RichTextEditorControl_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_RichTextEditorControl_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_RichTextEditorControl_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE richTextBox1 AS System.Windows.Forms.RichTextBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.

    DEFINE VARIABLE iFontSizes AS INTEGER NO-UNDO EXTENT 19 INIT
        [8, 9, 10, 11, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 40, 50, 60, 70, 80].

    DEFINE VARIABLE lSettingFormat AS LOGICAL NO-UNDO INIT FALSE .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RichTextEditorControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RichTextEditorControl ():

        DEFINE VARIABLE i     AS INTEGER                    NO-UNDO.
        DEFINE VARIABLE oList AS Infragistics.Win.ValueList NO-UNDO .

        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
        &ENDIF

        oList = CAST (ultraToolbarsManager1:Tools["FontSize":U],
                      ComboBoxTool):ValueList .
        oList:ValueListItems:Clear () .

        DO i = 1 TO EXTENT (iFontSizes):
            oList:ValueListItems:Add (BOX (iFontSizes[i]), STRING (iFontSizes[i])) .
        END.

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
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("RtfToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE fontListTool1 AS Infragistics.Win.UltraWinToolbars.FontListTool NO-UNDO.
        fontListTool1 = NEW Infragistics.Win.UltraWinToolbars.FontListTool("FontList":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool1 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool1 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Bold":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool2 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool2 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Italic":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool3 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool3 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Underline":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool1 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool1 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("FontSize":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool4 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool4 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Bold":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Controls.RichTextEditorControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool5 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool5 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Italic":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool6 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool6 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Underline":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE fontListTool2 AS Infragistics.Win.UltraWinToolbars.FontListTool NO-UNDO.
        fontListTool2 = NEW Infragistics.Win.UltraWinToolbars.FontListTool("FontList":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE comboBoxTool2 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
        comboBoxTool2 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("FontSize":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueList1 AS Infragistics.Win.ValueList NO-UNDO.
        valueList1 = NEW Infragistics.Win.ValueList(0).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem1 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem1 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem2 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem2 = NEW Infragistics.Win.ValueListItem().
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:richTextBox1 = NEW System.Windows.Forms.RichTextBox().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        THIS-OBJECT:ultraToolbarsManager1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2007.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 5 NO-UNDO.
        arrayvar0[1] = fontListTool1.
        arrayvar0[2] = stateButtonTool1.
        arrayvar0[3] = stateButtonTool2.
        arrayvar0[4] = stateButtonTool3.
        arrayvar0[5] = comboBoxTool1.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar0).
        ultraToolbar1:Text = "RtfToolbar":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 1 NO-UNDO.
        arrayvar1[1] = ultraToolbar1.
        THIS-OBJECT:ultraToolbarsManager1:Toolbars:AddRange(arrayvar1).
        THIS-OBJECT:ultraToolbarsManager1:ToolbarSettings:AllowCustomize = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraToolbarsManager1:ToolbarSettings:AllowDockBottom = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraToolbarsManager1:ToolbarSettings:AllowDockLeft = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraToolbarsManager1:ToolbarSettings:AllowDockRight = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraToolbarsManager1:ToolbarSettings:AllowFloating = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraToolbarsManager1:ToolbarSettings:AllowHiding = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraToolbarsManager1:ToolbarSettings:FillEntireRow = Infragistics.Win.DefaultableBoolean:True.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        stateButtonTool4:SharedPropsInternal:AppearancesLarge:Appearance = appearance1.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        stateButtonTool4:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        stateButtonTool4:SharedPropsInternal:Caption = "Bold":U.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        stateButtonTool5:SharedPropsInternal:AppearancesLarge:Appearance = appearance3.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        stateButtonTool5:SharedPropsInternal:AppearancesSmall:Appearance = appearance4.
        stateButtonTool5:SharedPropsInternal:Caption = "Italic":U.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        stateButtonTool6:SharedPropsInternal:AppearancesLarge:Appearance = appearance5.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        stateButtonTool6:SharedPropsInternal:AppearancesSmall:Appearance = appearance6.
        stateButtonTool6:SharedPropsInternal:Caption = "Underline":U.
        fontListTool2:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        fontListTool2:SharedPropsInternal:Caption = "FontListTool1":U.
        fontListTool2:SharedPropsInternal:Width = 200.
        comboBoxTool2:AutoComplete = TRUE.
        comboBoxTool2:DropDownStyle = Infragistics.Win.DropDownStyle:DropDown.
        comboBoxTool2:SharedPropsInternal:Caption = "FontSize":U.
        comboBoxTool2:SharedPropsInternal:Width = 60.
        valueListItem1:DataValue = System.Convert:ToInt16(8).
        valueListItem2:DataValue = System.Convert:ToInt16(9).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.ValueListItem EXTENT 2 NO-UNDO.
        arrayvar2[1] = valueListItem1.
        arrayvar2[2] = valueListItem2.
        valueList1:ValueListItems:AddRange(arrayvar2).
        comboBoxTool2:ValueList = valueList1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 5 NO-UNDO.
        arrayvar3[1] = stateButtonTool4.
        arrayvar3[2] = stateButtonTool5.
        arrayvar3[3] = stateButtonTool6.
        arrayvar3[4] = fontListTool2.
        arrayvar3[5] = comboBoxTool2.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar3).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        THIS-OBJECT:ultraToolbarsManager1:ToolValueChanged:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolValueChanged).
        /*  */
        /* m_RichTextEditorControl_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 53).
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Left:Name = "m_RichTextEditorControl_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(0, 326).
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_RichTextEditorControl_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(697, 53).
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Right:Name = "m_RichTextEditorControl_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(0, 326).
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_RichTextEditorControl_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Top:Name = "m_RichTextEditorControl_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(697, 53).
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_RichTextEditorControl_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 379).
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Bottom:Name = "m_RichTextEditorControl_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(697, 0).
        THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* richTextBox1 */
        /*  */
        THIS-OBJECT:richTextBox1:AcceptsTab = TRUE.
        THIS-OBJECT:richTextBox1:AutoWordSelection = TRUE.
        THIS-OBJECT:richTextBox1:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
        THIS-OBJECT:richTextBox1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:richTextBox1:EnableAutoDragDrop = TRUE.
        THIS-OBJECT:richTextBox1:HideSelection = FALSE.
        THIS-OBJECT:richTextBox1:Location = NEW System.Drawing.Point(0, 53).
        THIS-OBJECT:richTextBox1:Name = "richTextBox1":U.
        THIS-OBJECT:richTextBox1:ScrollBars = System.Windows.Forms.RichTextBoxScrollBars:ForcedBoth.
        THIS-OBJECT:richTextBox1:Size = NEW System.Drawing.Size(697, 326).
        THIS-OBJECT:richTextBox1:TabIndex = 4.
        THIS-OBJECT:richTextBox1:Text = "":U.
        THIS-OBJECT:richTextBox1:SelectionChanged:Subscribe(THIS-OBJECT:richTextBox1_SelectionChanged).
        /*  */
        /* RichTextEditorControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:richTextBox1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_RichTextEditorControl_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Name = "RichTextEditorControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(697, 379).
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SelectionChanged event of the richTextBox1
                 control
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID richTextBox1_SelectionChanged (sender AS System.Object,
                                                       e AS System.EventArgs):

        IF VALID-OBJECT (richTextBox1:SelectionFont) THEN DO:
            UltraToolbarsHelper:SetStateButtonToolChecked (ultraToolbarsManager1, "Bold":U, richTextBox1:SelectionFont:Bold) .
            UltraToolbarsHelper:SetStateButtonToolChecked (ultraToolbarsManager1, "Italic":U, richTextBox1:SelectionFont:Italic) .
            UltraToolbarsHelper:SetStateButtonToolChecked (ultraToolbarsManager1, "Underline":U, richTextBox1:SelectionFont:Underline) .

            CAST (ultraToolbarsManager1:Tools["FontList":U], FontListTool):Text = richTextBox1:SelectionFont:OriginalFontName .

            CAST (ultraToolbarsManager1:Tools["FontSize":U], ComboBoxTool):Text = STRING (richTextBox1:SelectionFont:SizeInPoints) .
        END.

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Toggles the FontFace
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ToggleFontFace ():

        DEFINE VARIABLE cFont AS CHARACTER NO-UNDO.

        ASSIGN cFont = CAST (ultraToolbarsManager1:Tools["FontList":U], FontListTool):Text .

        IF Consultingwerk.CharacterType:IsNullOrEmpty (cFont) THEN
            RETURN .

        IF lSettingFormat = TRUE THEN
            RETURN .

        DO ON ERROR UNDO, THROW:
            ASSIGN lSettingFormat = TRUE .

            Consultingwerk.Support.RtfEditorSupport:SetFontFace (cFont, richTextBox1) .

            FINALLY:
                ASSIGN lSettingFormat = FALSE .
            END FINALLY.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Toggles the setting of a format
        Notes:
        @param pcFormat The format (Bold, Italic, Underline) to Toggle
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ToggleFormat (pcFormat AS CHARACTER):

        IF lSettingFormat = TRUE THEN
            RETURN .

        DO ON ERROR UNDO, THROW:
            ASSIGN lSettingFormat = TRUE .

            IF pcFormat = "Bold":U THEN
                IF UltraToolbarsHelper:GetStateButtonToolChecked (ultraToolbarsManager1, "Bold":U) THEN
                    Consultingwerk.Support.RtfEditorSupport:SetSelectionBoldOn (richTextBox1) .
                ELSE
                    Consultingwerk.Support.RtfEditorSupport:SetSelectionBoldOff (richTextBox1) .

            IF pcFormat = "Italic":U THEN
                IF UltraToolbarsHelper:GetStateButtonToolChecked (ultraToolbarsManager1, "Italic":U) THEN
                    Consultingwerk.Support.RtfEditorSupport:SetSelectionItalicOn (richTextBox1) .
                ELSE
                    Consultingwerk.Support.RtfEditorSupport:SetSelectionItalicOff (richTextBox1) .

            IF pcFormat = "Underline":U THEN
                IF UltraToolbarsHelper:GetStateButtonToolChecked (ultraToolbarsManager1, "Underline":U) THEN
                    Consultingwerk.Support.RtfEditorSupport:SetSelectionUnderlineOn (richTextBox1) .
                ELSE
                    Consultingwerk.Support.RtfEditorSupport:SetSelectionUnderlineOff (richTextBox1) .

            FINALLY:
                ASSIGN lSettingFormat = FALSE .
            END FINALLY.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Toggles the FontSize
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ToggleFontSize ():

        DEFINE VARIABLE iFontSize AS INTEGER NO-UNDO.

        ASSIGN iFontSize = INTEGER (CAST (ultraToolbarsManager1:Tools["FontSize":U], ComboBoxTool):TEXT) .

        IF Consultingwerk.IntegerType:IsNullOrEmpty (iFontSize) THEN
            RETURN .

        IF lSettingFormat = TRUE THEN
            RETURN .

        DO ON ERROR UNDO, THROW:
            ASSIGN lSettingFormat = TRUE .

            Consultingwerk.Support.RtfEditorSupport:SetFontSize (iFontSize * 20, richTextBox1) .

            FINALLY:
                ASSIGN lSettingFormat = FALSE .
            END FINALLY.
        END.

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

            WHEN "Bold":U THEN
                ToggleFormat (e:Tool:Key) .

            WHEN "Italic":U THEN
                ToggleFormat (e:Tool:Key) .

            WHEN "Underline":U THEN
                ToggleFormat (e:Tool:Key) .

        END CASE.

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolValueChanged event of the ultraToolbarsManager1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraToolbarsManager1_ToolValueChanged (sender AS System.Object,
                                                                e AS Infragistics.Win.UltraWinToolbars.ToolEventArgs):

        CASE e:Tool:Key:
            WHEN "FontList":U THEN
                ToggleFontFace () .
            WHEN "FontSize":U THEN
                ToggleFontSize () .
        END CASE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the RichTextEditorControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC RichTextEditorControl ():

    END DESTRUCTOR .

END CLASS.
