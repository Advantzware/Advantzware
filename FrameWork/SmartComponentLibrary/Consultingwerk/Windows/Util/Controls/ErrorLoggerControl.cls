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
    File        : ErrorLoggerControl
    Purpose     : Displays Error Objects in a scrollable view
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 31 20:59:50 CEST 2011
    Notes       : Reference Implementation of IErrorHandler
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Windows.Util.Controls.ErrorLoggerControl
    INHERITS UserControl
    IMPLEMENTS Consultingwerk.Framework.IErrorHandler:

    DEFINE PRIVATE VARIABLE m_ErrorLoggerControl_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ErrorLoggerControl_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ErrorLoggerControl_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ErrorLoggerControl_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE saveFileDialog1 AS System.Windows.Forms.SaveFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ErrorLoggerControl_Fill_Panel AS System.Windows.Forms.Panel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ErrorLoggerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ErrorLoggerControl ( ):

        SUPER().
        InitializeComponent().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Clears the Editor with the Error message view
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ClearEditor ():

        ultraTextEditor1:Clear () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Handles an Error object
        Notes:   Logical return value indicates if the error has been completely
                 Handled, that is when this method returns TRUE, the ErrorHelper
                 will not display the error anymore, return false when the
                 ErrorHelper should still display the error message dialog.
        @param poError The reference to the Error to handle
        @return Logical value indicating if the error has been completely handled
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL HandleError (poError AS Progress.Lang.Error):

        DEFINE VARIABLE cTypeName AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i         AS INTEGER   NO-UNDO .

        IF TYPE-OF (poError, System.Exception) THEN
            ASSIGN cTypeName = CAST (poError, System.Exception):GetType():FullName .
        ELSE
            ASSIGN cTypeName = CAST (poError, Progress.Lang.Object):GetClass():TypeName .

        ultraTextEditor1:AppendText (SUBSTITUTE ("&1 - &2&3&3":U,
                                                 STRING (NOW, "99.99.9999 HH:MM:SS":U),
                                                 cTypeName,
                                                 System.Environment:NewLine)) .

        ultraTextEditor1:AppendText (REPLACE(REPLACE (Consultingwerk.Util.ErrorHelper:FormattedErrorMessages (poError),
                                                      "~n":U, System.Environment:NewLine),
                                             CHR(13), System.Environment:NewLine)) .

        IF TYPE-OF (poError, Progress.Lang.AppError) THEN
            ultraTextEditor1:AppendText (CAST (poError, Progress.Lang.AppError):ReturnValue + System.Environment:NewLine) .

        ultraTextEditor1:AppendText (System.Environment:NewLine +
                                     REPLACE (Consultingwerk.Util.ErrorHelper:StackTrace (poError),
                                              CHR (10), System.Environment:NewLine) +
                                     System.Environment:NewLine +
                                     System.Environment:NewLine) .

        ultraTextEditor1:SelectionStart = ultraTextEditor1:TextLength .
        ultraTextEditor1:ScrollToCaret () .

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
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("ErrorLogger":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Clear":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Clear":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Util.Controls.ErrorLoggerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:ErrorLoggerControl_Fill_Panel = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:saveFileDialog1 = NEW System.Windows.Forms.SaveFileDialog().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ErrorLoggerControl_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
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
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        arrayvar0[2] = buttonTool2.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar0).
        ultraToolbar1:Text = "ErrorLogger":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 1 NO-UNDO.
        arrayvar1[1] = ultraToolbar1.
        THIS-OBJECT:ultraToolbarsManager1:Toolbars:AddRange(arrayvar1).
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool3:SharedPropsInternal:AppearancesLarge:Appearance = appearance2.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        buttonTool3:SharedPropsInternal:AppearancesSmall:Appearance = appearance3.
        buttonTool3:SharedPropsInternal:Caption = "Clear":U.
        buttonTool3:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesLarge:Appearance = appearance4.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesSmall:Appearance = appearance5.
        buttonTool4:SharedPropsInternal:Caption = "Save":U.
        buttonTool4:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar2[1] = buttonTool3.
        arrayvar2[2] = buttonTool4.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar2).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        /*  */
        /* ErrorLoggerControl_Fill_Panel */
        /*  */
        THIS-OBJECT:ErrorLoggerControl_Fill_Panel:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:ErrorLoggerControl_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:ErrorLoggerControl_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ErrorLoggerControl_Fill_Panel:Location = NEW System.Drawing.Point(0, 62).
        THIS-OBJECT:ErrorLoggerControl_Fill_Panel:Name = "ErrorLoggerControl_Fill_Panel":U.
        THIS-OBJECT:ErrorLoggerControl_Fill_Panel:Size = NEW System.Drawing.Size(484, 186).
        THIS-OBJECT:ErrorLoggerControl_Fill_Panel:TabIndex = 0.
        /*  */
        /* ultraTextEditor1 */
        /*  */
        THIS-OBJECT:ultraTextEditor1:AlwaysInEditMode = TRUE.
        appearance1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:ultraTextEditor1:Appearance = appearance1.
        THIS-OBJECT:ultraTextEditor1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:ultraTextEditor1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraTextEditor1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraTextEditor1:Multiline = TRUE.
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:ReadOnly = TRUE.
        THIS-OBJECT:ultraTextEditor1:Scrollbars = System.Windows.Forms.ScrollBars:Both.
        THIS-OBJECT:ultraTextEditor1:Size = NEW System.Drawing.Size(484, 186).
        THIS-OBJECT:ultraTextEditor1:TabIndex = 0.
        /*  */
        /* m_ErrorLoggerControl_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 62).
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Left:Name = "m_ErrorLoggerControl_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(0, 186).
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ErrorLoggerControl_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(484, 62).
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Right:Name = "m_ErrorLoggerControl_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(0, 186).
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ErrorLoggerControl_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Top:Name = "m_ErrorLoggerControl_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(484, 62).
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ErrorLoggerControl_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 248).
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Bottom:Name = "m_ErrorLoggerControl_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(484, 0).
        THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* saveFileDialog1 */
        /*  */
        THIS-OBJECT:saveFileDialog1:DefaultExt = "log":U.
        THIS-OBJECT:saveFileDialog1:FileName = "error.log":U.
        THIS-OBJECT:saveFileDialog1:Filter = "Log-Files|*.log|All Files|*.*":U.
        THIS-OBJECT:saveFileDialog1:RestoreDirectory = TRUE.
        THIS-OBJECT:saveFileDialog1:SupportMultiDottedExtensions = TRUE.
        THIS-OBJECT:saveFileDialog1:Title = "Save Error-Log":U.
        /*  */
        /* ErrorLoggerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ErrorLoggerControl_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ErrorLoggerControl_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Name = "ErrorLoggerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(484, 248).
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ErrorLoggerControl_Fill_Panel:ResumeLayout(FALSE).
        THIS-OBJECT:ErrorLoggerControl_Fill_Panel:PerformLayout().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the contents of the editor to a file
        Notes:   The user will be prompted for the file name
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SaveEditorContents ():

        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO.
        DEFINE VARIABLE lcText        AS LONGCHAR                           NO-UNDO.

        WAIT-FOR saveFileDialog1:ShowDialog() SET oDialogResult .

        IF Progress.Util.EnumHelper:AreEqual (oDialogResult,
                                              System.Windows.Forms.DialogResult:OK) THEN DO:

            ASSIGN lcText = ultraTextEditor1:Text .
            COPY-LOB FROM lcText TO FILE saveFileDialog1:FileName .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the UltraToolbarsManager
                 instance
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraToolbarsManager1_ToolClick (sender AS System.Object,
                                                         e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):

        CASE e:Tool:Key:
            WHEN "Clear":U THEN
                ClearEditor () .
            WHEN "Save":U THEN
                SaveEditorContents () .
        END CASE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the ErrorLoggerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ErrorLoggerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.