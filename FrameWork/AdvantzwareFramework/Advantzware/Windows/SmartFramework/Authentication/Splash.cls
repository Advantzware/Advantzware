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
    File        : Splash
    Purpose     : Splash screen for SmartComponent Library tools and
                  samples
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 02 06:42:17 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.*                    FROM PROPATH .
USING Consultingwerk.Util.*               FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH .
USING Progress.Lang.*                     FROM PROPATH .
USING Progress.Windows.*                  FROM ASSEMBLY .
USING Consultingwerk.* FROM PROPATH.

CLASS Advantzware.Windows.SmartFramework.Authentication.Splash INHERITS Form   :

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE pictureBox1 AS System.Windows.Forms.PictureBox NO-UNDO.

    DEFINE VARIABLE oFullWindowDragManager AS FullWindowDragManager NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Splash class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Splash ():

        DEFINE VARIABLE oFrameworkVersion AS FrameworkVersion NO-UNDO .

        SUPER().

        InitializeComponent().

        oFullWindowDragManager = NEW FullWindowDragManager (pictureBox1) .

        oFrameworkVersion = NEW FrameworkVersion () .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeComponent ():

        /* NOTE: The following method is automatically generated.

        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.

        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Advantzware.Windows.SmartFramework.Authentication.Splash").
        THIS-OBJECT:pictureBox1 = NEW System.Windows.Forms.PictureBox().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* pictureBox1 */
        /*  */
        THIS-OBJECT:pictureBox1:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
        THIS-OBJECT:pictureBox1:Image = CAST(resources:GetObject("pictureBox1.Image"), System.Drawing.Image).
        THIS-OBJECT:pictureBox1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:pictureBox1:Margin = NEW System.Windows.Forms.Padding(0).
        THIS-OBJECT:pictureBox1:Name = "pictureBox1".
        THIS-OBJECT:pictureBox1:Size = NEW System.Drawing.Size(410, 260).
        THIS-OBJECT:pictureBox1:SizeMode = System.Windows.Forms.PictureBoxSizeMode:AutoSize.
        THIS-OBJECT:pictureBox1:TabIndex = 0.
        THIS-OBJECT:pictureBox1:TabStop = FALSE.
        /*  */
        /* Splash */
        /*  */
        THIS-OBJECT:AutoSize = TRUE.
        THIS-OBJECT:AutoSizeMode = System.Windows.Forms.AutoSizeMode:GrowAndShrink.
        THIS-OBJECT:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(411, 262).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:pictureBox1).
        THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:None.
        THIS-OBJECT:Name = "Splash".
        THIS-OBJECT:ShowIcon = FALSE.
        THIS-OBJECT:ShowInTaskbar = FALSE.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterScreen.
        THIS-OBJECT:Text = "SmartComponent Library".
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.




    DESTRUCTOR PUBLIC Splash ( ):

        GarbageCollectorHelper:DeleteObject (oFullWindowDragManager) .

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
