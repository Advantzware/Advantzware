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
    File        : UltraGridColumnChooserForm
    Purpose     : Provides a generic UI with the UltraGrid Column Chooser
                  functionality
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 13 06:39:06 CET 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.BaseForm      FROM PROPATH .
USING Consultingwerk.Util.*              FROM PROPATH .
USING Consultingwerk.Windows.Resources.* FROM PROPATH .
USING Progress.Lang.*                    FROM PROPATH .

CLASS Consultingwerk.Windows.Forms.UltraGridColumnChooserForm
    INHERITS BaseForm
    IMPLEMENTS Consultingwerk.Windows.Forms.IUltraGridColumnChooserForm:

    DEFINE PRIVATE VARIABLE m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGridColumnChooser1 AS Infragistics.Win.UltraWinGrid.UltraGridColumnChooser NO-UNDO.
    DEFINE PRIVATE VARIABLE UltraGridColumnChooserForm_Fill_Panel AS System.Windows.Forms.Panel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Column Chooser Control
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ColumnChooser AS Infragistics.Win.UltraWinGrid.UltraGridColumnChooser NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UltraGridColumnChooserForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UltraGridColumnChooserForm ():

        SUPER().
        InitializeComponent().

        THIS-OBJECT:ColumnChooser = ultraGridColumnChooser1 .

        THIS-OBJECT:Text = WindowsCustomizer:ColumnChooserFormCaption .
        THIS-OBJECT:ultraToolbarsManager1:Tools["Close":U]:SharedProps:Caption = WindowsCustomizer:ColumnChooserCloseButtonCaption .

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
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("UltraToolbar1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Close":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Close":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Forms.UltraGridColumnChooserForm":U).
        THIS-OBJECT:ultraGridColumnChooser1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumnChooser().
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:UltraGridColumnChooserForm_Fill_Panel = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        CAST(THIS-OBJECT:ultraGridColumnChooser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:UltraGridColumnChooserForm_Fill_Panel:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraGridColumnChooser1 */
        /*  */
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ResizeAllColumns.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:MaxRowScrollRegions = 1.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:AllowColMoving = Infragistics.Win.UltraWinGrid.AllowColMoving:NotAllowed.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:AllowColSizing = Infragistics.Win.UltraWinGrid.AllowColSizing:None.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:AllowRowLayoutCellSizing = Infragistics.Win.UltraWinGrid.RowLayoutSizing:None.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:AllowRowLayoutLabelSizing = Infragistics.Win.UltraWinGrid.RowLayoutSizing:None.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:CellPadding = 2.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:ExpansionIndicator = Infragistics.Win.UltraWinGrid.ShowExpansionIndicator:Never.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:Select.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:RowSizing = Infragistics.Win.UltraWinGrid.RowSizing:AutoFixed.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:None.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:RowConnectorStyle = Infragistics.Win.UltraWinGrid.RowConnectorStyle:None.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:ultraGridColumnChooser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:ultraGridColumnChooser1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraGridColumnChooser1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraGridColumnChooser1:Name = "ultraGridColumnChooser1":U.
        THIS-OBJECT:ultraGridColumnChooser1:Size = NEW System.Drawing.Size(184, 438).
        THIS-OBJECT:ultraGridColumnChooser1:Style = Infragistics.Win.UltraWinGrid.ColumnChooserStyle:HiddenColumnsOnly.
        THIS-OBJECT:ultraGridColumnChooser1:StyleLibraryName = "":U.
        THIS-OBJECT:ultraGridColumnChooser1:StyleSetName = "":U.
        THIS-OBJECT:ultraGridColumnChooser1:TabIndex = 0.
        THIS-OBJECT:ultraGridColumnChooser1:Text = "ultraGridColumnChooser1":U.
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.Forms.BaseForm":U).
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        THIS-OBJECT:ultraToolbarsManager1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2007.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar0).
        ultraToolbar1:Text = "UltraToolbar1":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 1 NO-UNDO.
        arrayvar1[1] = ultraToolbar1.
        THIS-OBJECT:ultraToolbarsManager1:Toolbars:AddRange(arrayvar1).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance1.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        buttonTool2:SharedPropsInternal:Caption = "Close":U.
        buttonTool2:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar2[1] = buttonTool2.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar2).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        /*  */
        /* UltraGridColumnChooserForm_Fill_Panel */
        /*  */
        THIS-OBJECT:UltraGridColumnChooserForm_Fill_Panel:Controls:Add(THIS-OBJECT:ultraGridColumnChooser1).
        THIS-OBJECT:UltraGridColumnChooserForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:UltraGridColumnChooserForm_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:UltraGridColumnChooserForm_Fill_Panel:Location = NEW System.Drawing.Point(0, 28).
        THIS-OBJECT:UltraGridColumnChooserForm_Fill_Panel:Name = "UltraGridColumnChooserForm_Fill_Panel":U.
        THIS-OBJECT:UltraGridColumnChooserForm_Fill_Panel:Size = NEW System.Drawing.Size(184, 438).
        THIS-OBJECT:UltraGridColumnChooserForm_Fill_Panel:TabIndex = 0.
        /*  */
        /* m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 28).
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Left:Name = "m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(0, 438).
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(184, 28).
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Right:Name = "m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(0, 438).
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Top:Name = "m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(184, 28).
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 466).
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Bottom:Name = "m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(184, 0).
        THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* UltraGridColumnChooserForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(184, 466).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:UltraGridColumnChooserForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_UltraGridColumnChooserForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:SizableToolWindow.
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "UltraGridColumnChooserForm":U.
        THIS-OBJECT:Text = "Column Chooser Form":U.
        THIS-OBJECT:WindowPositionRegistryKey = "UltraGridColumnChooserForm":U.
        CAST(THIS-OBJECT:ultraGridColumnChooser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:UltraGridColumnChooserForm_Fill_Panel:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
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

            WHEN "Close":U THEN
                THIS-OBJECT:Close () .

        END CASE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the UltraGridColumnChooserForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC UltraGridColumnChooserForm ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.