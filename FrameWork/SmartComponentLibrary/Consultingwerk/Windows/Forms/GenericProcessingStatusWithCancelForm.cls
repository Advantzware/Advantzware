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
    File        : GenericProcessingStatusWithCancelForm
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 04 20:02:37 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.*         FROM PROPATH .
USING Consultingwerk.Util.*          FROM PROPATH .
USING Consultingwerk.Windows.Forms.* FROM PROPATH .
USING Progress.Lang.*                FROM PROPATH .

CLASS Consultingwerk.Windows.Forms.GenericProcessingStatusWithCancelForm
    INHERITS BaseForm:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE lbStatus AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE pictureBox1 AS System.Windows.Forms.PictureBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the Cancel button has been pressed
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Cancelled AS LOGICAL INITIAL FALSE NO-UNDO
    GET:
        UserInterfaceHelper:ProcessEvents() .

        RETURN THIS-OBJECT:Cancelled .
    END GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Status Text
        Notes:   Directly updates the UI and processes UI events
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StatusText AS CHARACTER NO-UNDO
    GET:
        RETURN lbStatus:Text .
    END GET .
    SET (arg AS CHARACTER):
        lbStatus:Text = arg .

        UserInterfaceHelper:ProcessEvents () .
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GenericProcessingStatusWithCancelForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GenericProcessingStatusWithCancelForm ():

        SUPER().
        InitializeComponent().

        THIS-OBJECT:StoreFormPosition = FALSE .

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
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Forms.GenericProcessingStatusWithCancelForm":U).
        THIS-OBJECT:lbStatus = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:pictureBox1 = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* lbStatus */
        /*  */
        appearance1:FontData:BoldAsString = "True":U.
        THIS-OBJECT:lbStatus:Appearance = appearance1.
        THIS-OBJECT:lbStatus:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:lbStatus:Name = "lbStatus":U.
        THIS-OBJECT:lbStatus:Size = NEW System.Drawing.Size(479, 23).
        THIS-OBJECT:lbStatus:TabIndex = 0.
        /*  */
        /* pictureBox1 */
        /*  */
        THIS-OBJECT:pictureBox1:Image = CAST(resources:GetObject("pictureBox1.Image":U), System.Drawing.Image).
        THIS-OBJECT:pictureBox1:Location = NEW System.Drawing.Point(184, 59).
        THIS-OBJECT:pictureBox1:Name = "pictureBox1":U.
        THIS-OBJECT:pictureBox1:Size = NEW System.Drawing.Size(128, 15).
        THIS-OBJECT:pictureBox1:SizeMode = System.Windows.Forms.PictureBoxSizeMode:AutoSize.
        THIS-OBJECT:pictureBox1:TabIndex = 1.
        THIS-OBJECT:pictureBox1:TabStop = FALSE.
        /*  */
        /* ultraButton1 */
        /*  */
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(184, 95).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(128, 23).
        THIS-OBJECT:ultraButton1:TabIndex = 2.
        THIS-OBJECT:ultraButton1:Text = "&Cancel":U.
        THIS-OBJECT:ultraButton1:Click:Subscribe(THIS-OBJECT:ultraButton1_Click).
        /*  */
        /* GenericProcessingStatusWithCancelForm */
        /*  */
        THIS-OBJECT:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(503, 135).
        THIS-OBJECT:ControlBox = FALSE.
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:pictureBox1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:lbStatus).
        THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:FixedDialog.
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "GenericProcessingStatusWithCancelForm":U.
        THIS-OBJECT:ShowIcon = FALSE.
        THIS-OBJECT:ShowInTaskbar = FALSE.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
        THIS-OBJECT:Text = "Processing":U.
        THIS-OBJECT:WindowPositionRegistryKey = "GenericProcessingStatusWithCancelForm":U.
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose:
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton1_Click (sender AS System.Object,
                                            e AS System.EventArgs):

        ASSIGN THIS-OBJECT:Cancelled = TRUE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD .

    DESTRUCTOR PUBLIC GenericProcessingStatusWithCancelForm ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
