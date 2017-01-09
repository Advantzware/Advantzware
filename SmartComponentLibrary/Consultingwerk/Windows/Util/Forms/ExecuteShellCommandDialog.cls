/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ExecuteShellCommandDialog
    Purpose     :
    Syntax      : oDialog:Execute ("DIR c:\Windows") .
    Description : Dialog that can launch a Windows Shell Command (DOS Command)
                  and display the output in an Editor Control
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Dec 21 12:00:44 CET 2014
    Notes       : SCL-597
                  Does not support executing built in OS-Shell commands like
                  "dir", needs executable file passed in as parameter
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*               FROM PROPATH .
USING Consultingwerk.Windows.BaseForms.*  FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH .
USING Consultingwerk.Support.*            FROM ASSEMBLY.
USING Progress.Lang.*                     FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Util.Forms.ExecuteShellCommandDialog
    INHERITS BaseDialogForm:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE label1 AS System.Windows.Forms.Label NO-UNDO.
    DEFINE PRIVATE VARIABLE richTextBox1 AS System.Windows.Forms.RichTextBox NO-UNDO.

    DEFINE VARIABLE cCommand   AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cParameter AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cTitle     AS CHARACTER NO-UNDO .

    DEFINE VARIABLE oProcess AS System.Diagnostics.Process NO-UNDO .
    DEFINE VARIABLE oDataReader AS ProcessOutputReader     NO-UNDO .
    DEFINE PRIVATE VARIABLE ultraTrackBar1 AS Infragistics.Win.UltraWinEditors.UltraTrackBar NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ExecuteShellCommandDialog class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ExecuteShellCommandDialog ():

        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        ASSIGN cTitle = THIS-OBJECT:Text .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Executes a Windows Shell Command (DOS Command)
        Notes:   Executes this Form via ShowDialog
        @param pcCommand The command to execute
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Execute (pcCommand AS CHARACTER):

        DEFINE VARIABLE iPos AS INTEGER NO-UNDO.

        IF pcCommand BEGINS "~"":U THEN DO:
            ASSIGN iPos = StringHelper:FindClosingQuote (pcCommand, 1) .

            ASSIGN cCommand   = StringHelper:Unquote(SUBSTRING (pcCommand, 1, iPos))
                   cParameter = SUBSTRING (pcCommand, iPos + 1).
        END.
        ELSE DO:
            ASSIGN cCommand = ENTRY (1, pcCommand, "":U)
                   iPos     = INDEX (pcCommand, " ":U) .

            IF iPos > 0 THEN
                cParameter = SUBSTRING (pcCommand, iPos + 1) .
        END.

        WAIT-FOR THIS-OBJECT:ShowDialog () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .

            ButtonOk:Enabled = TRUE .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Executes the command
        Notes:   Enables the OK Button when donw
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ExecuteCommand ():

        DEFINE VARIABLE cOutput AS CHARACTER NO-UNDO.

        THIS-OBJECT:Text = SUBSTITUTE ("&1 &2":U,
                                       cTitle, cCommand) .

        richTextBox1:Clear() .

        richTextBox1:AppendText (SUBSTITUTE  ("&1 &2 &3":U,
                                              cCommand,
                                              cParameter,
                                              System.Environment:NewLine)) .

        oProcess = NEW System.Diagnostics.Process () .

        oProcess:StartInfo:FileName  = cCommand .
        oProcess:StartInfo:Arguments = cParameter .

        oProcess:StartInfo:WorkingDirectory = System.Environment:CurrentDirectory .

        /* obtain access to stdout of the process */
        oProcess:StartInfo:RedirectStandardOutput = TRUE .
        oProcess:StartInfo:RedirectStandardError = TRUE  .

        oProcess:StartInfo:UseShellExecute = FALSE .
        oProcess:StartInfo:CreateNoWindow = TRUE .

        oProcess:StartInfo:ErrorDialog = TRUE .

        /* start process, get "reference" */

        oProcess:Start () .

        IF NOT VALID-OBJECT (oProcess) THEN DO:
            MessageFormHelper:ShowMessage ("Invalid process reference."{&TRAN},
                                           MessageFormImages:ImageWarning) .
            ButtonOk:Enabled = TRUE .
        END.

        PROCESS EVENTS .

        oDataReader = NEW ProcessOutputReader (oProcess) .
        THIS-OBJECT:Controls:Add (oDataReader) .
        oDataReader:Show () .
        oDataReader:DataAvailable:Subscribe (THIS-OBJECT:ProcessDataAvailableHandler) .
        oDataReader:ErrorDataAvailable:Subscribe (THIS-OBJECT:ProcessErrorDataAvailableHandler) .
        oDataReader:ProcessExited:Subscribe (THIS-OBJECT:ProcessExitedHandler) .

    END METHOD .

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
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Util.Forms.ExecuteShellCommandDialog":U).
        THIS-OBJECT:richTextBox1 = NEW System.Windows.Forms.RichTextBox().
        THIS-OBJECT:ultraTrackBar1 = NEW Infragistics.Win.UltraWinEditors.UltraTrackBar().
        THIS-OBJECT:label1 = NEW System.Windows.Forms.Label().
        THIS-OBJECT:ContentPanel:SuspendLayout().
        CAST(THIS-OBJECT:ultraTrackBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ButtonOk */
        /*  */
        THIS-OBJECT:ButtonOk:Enabled = FALSE.
        THIS-OBJECT:ButtonOk:Location = NEW System.Drawing.Point(373, 342).
        /*  */
        /* ButtonCancel */
        /*  */
        THIS-OBJECT:ButtonCancel:Enabled = FALSE.
        THIS-OBJECT:ButtonCancel:Location = NEW System.Drawing.Point(373, 342).
        THIS-OBJECT:ButtonCancel:Visible = FALSE.
        /*  */
        /* ContentPanel */
        /*  */
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:richTextBox1).
        THIS-OBJECT:ContentPanel:Size = NEW System.Drawing.Size(460, 336).
        /*  */
        /* richTextBox1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:richTextBox1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:richTextBox1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:richTextBox1:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
        THIS-OBJECT:richTextBox1:Font = NEW System.Drawing.Font("Consolas":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:richTextBox1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:richTextBox1:Name = "richTextBox1":U.
        THIS-OBJECT:richTextBox1:ReadOnly = TRUE.
        THIS-OBJECT:richTextBox1:ShortcutsEnabled = FALSE.
        THIS-OBJECT:richTextBox1:Size = NEW System.Drawing.Size(436, 309).
        THIS-OBJECT:richTextBox1:TabIndex = 0.
        THIS-OBJECT:richTextBox1:Text = "":U.
        THIS-OBJECT:richTextBox1:WordWrap = FALSE.
        /*  */
        /* ultraTrackBar1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraTrackBar1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTrackBar1:LargeChange = 10.
        THIS-OBJECT:ultraTrackBar1:Location = NEW System.Drawing.Point(54, 342).
        THIS-OBJECT:ultraTrackBar1:MaxValue = 40.
        THIS-OBJECT:ultraTrackBar1:MinValue = 10.
        THIS-OBJECT:ultraTrackBar1:Name = "ultraTrackBar1":U.
        THIS-OBJECT:ultraTrackBar1:Size = NEW System.Drawing.Size(110, 31).
        THIS-OBJECT:ultraTrackBar1:TabIndex = 3.
        THIS-OBJECT:ultraTrackBar1:TickmarkSettingsMajor:Frequency = 10.
        THIS-OBJECT:ultraTrackBar1:TickmarkSettingsMajor:Thickness = 2.
        THIS-OBJECT:ultraTrackBar1:TickmarkSettingsMajor:Visible = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraTrackBar1:TickmarkSettingsMinor:Frequency = 5.
        THIS-OBJECT:ultraTrackBar1:Value = 10.
        THIS-OBJECT:ultraTrackBar1:ValueObject = 10.
        THIS-OBJECT:ultraTrackBar1:ViewStyle = Infragistics.Win.UltraWinEditors.TrackBarViewStyle:Standard.
        THIS-OBJECT:ultraTrackBar1:ValueChanged:Subscribe(THIS-OBJECT:ultraTrackBar1_ValueChanged).
        /*  */
        /* label1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:label1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:label1:AutoSize = TRUE.
        THIS-OBJECT:label1:Location = NEW System.Drawing.Point(12, 347).
        THIS-OBJECT:label1:Name = "label1":U.
        THIS-OBJECT:label1:Size = NEW System.Drawing.Size(36, 17).
        THIS-OBJECT:label1:TabIndex = 4.
        THIS-OBJECT:label1:Text = "Zoom:":U.
        THIS-OBJECT:label1:UseCompatibleTextRendering = TRUE.
        /*  */
        /* ExecuteShellCommandDialog */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(460, 377).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:label1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTrackBar1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "ExecuteShellCommandDialog":U.
        THIS-OBJECT:Text = "Executing":U.
        THIS-OBJECT:Controls:SetChildIndex(THIS-OBJECT:ContentPanel, 0).
        THIS-OBJECT:Controls:SetChildIndex(THIS-OBJECT:ButtonOk, 0).
        THIS-OBJECT:Controls:SetChildIndex(THIS-OBJECT:ButtonCancel, 0).
        THIS-OBJECT:Controls:SetChildIndex(THIS-OBJECT:ultraTrackBar1, 0).
        THIS-OBJECT:Controls:SetChildIndex(THIS-OBJECT:label1, 0).
        THIS-OBJECT:ContentPanel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraTrackBar1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown (e) .

        PROCESS EVENTS .

        THIS-OBJECT:ExecuteCommand () .

        CATCH ex AS System.Exception:
            richTextBox1:AppendText (ex:Message) .
            richTextBox1:AppendText ("~n") .

            ButtonOk:Enabled = TRUE .
        END CATCH.

        CATCH err AS Progress.Lang.Error:
            richTextBox1:AppendText (err:GetMessage (1)) .
            richTextBox1:AppendText ("~n") .

            ButtonOk:Enabled = TRUE .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DataAvailable event of the
                 Consultingwerk.Support.ProcessOutputReader control instance
        Notes:   Raises the ProcessOutputChanged event of the BackgroundJob instance
        @param sender The reference to the object that raised the event
        @param e The default System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ProcessDataAvailableHandler (sender AS System.Object,
                                                     e AS System.EventArgs):

        DEFINE VARIABLE cData AS CHARACTER NO-UNDO.

        ASSIGN cData = oDataReader:GetCurrentData (TRUE) .

        IF cData > "":U THEN
            richTextBox1:AppendText (cData) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ErrorDataAvailable event of the
                 Consultingwerk.Support.ProcessOutputReader control instance
        Notes:   Raises the ProcessOutputChanged event of the BackgroundJob instance
        @param sender The reference to the object that raised the event
        @param e The default System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ProcessErrorDataAvailableHandler (sender AS System.Object,
                                                          e AS System.EventArgs):

        DEFINE VARIABLE cData AS CHARACTER NO-UNDO.

        ASSIGN cData = oDataReader:GetCurrentErrorData (TRUE) .

        IF cData > "":U THEN DO:
            RtfEditorSupport:SetSelectionBoldOn (richTextBox1).
            richTextBox1:SelectionColor = System.Drawing.Color:Red .

            richTextBox1:AppendText (cData) .

            RtfEditorSupport:SetSelectionBoldOff (richTextBox1).
            richTextBox1:SelectionColor = richTextBox1:ForeColor .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Exited event of the Process
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ProcessExitedHandler (sender AS System.Object,
                                              e AS System.EventArgs):

        ButtonOk:Enabled = TRUE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ValueChanged event of the ultraTrackBar1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTrackBar1_ValueChanged (sender AS System.Object,
                                                     e AS System.EventArgs):

        richTextBox1:ZoomFactor = ultraTrackBar1:Value / 10 .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ExecuteShellCommandDialog class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ExecuteShellCommandDialog ():

    END DESTRUCTOR .

END CLASS.
