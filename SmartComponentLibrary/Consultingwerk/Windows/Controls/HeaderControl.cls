/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : HeaderControl
    Purpose     : Header Control
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 08 18:12:21 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.Controls.* FROM PROPATH .
USING Progress.Lang.*                   FROM PROPATH .
USING Progress.Windows.*                FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Controls.HeaderControl
    INHERITS UserControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE label1 AS System.Windows.Forms.Label NO-UNDO.

    /**
     * Purpose: Gets and sets the Label text of the header
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Label AS CHARACTER NO-UNDO
    GET:
        RETURN label1:Text .
    END GET .
    SET (arg AS CHARACTER):

        ASSIGN label1:Text = arg .

    END SET .

    /**
     * Purpose: Constructor for the HeaderColor class
     * Notes:
     */
    CONSTRUCTOR PUBLIC HeaderControl ():

        SUPER().
        InitializeComponent().
        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
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
        THIS-OBJECT:label1 = NEW System.Windows.Forms.Label().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* label1 */
        /*  */
        THIS-OBJECT:label1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:label1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:label1:ForeColor = System.Drawing.SystemColors:ActiveCaptionText.
        THIS-OBJECT:label1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:label1:Name = "label1":U.
        THIS-OBJECT:label1:Padding = NEW System.Windows.Forms.Padding(0, 3, 0, 0).
        THIS-OBJECT:label1:Size = NEW System.Drawing.Size(556, 23).
        THIS-OBJECT:label1:TabIndex = 0.
        THIS-OBJECT:label1:Text = "label1":U.
        THIS-OBJECT:label1:UseCompatibleTextRendering = TRUE.
        /*  */
        /* HeaderControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:BackColor = System.Drawing.SystemColors:ActiveCaption.
        THIS-OBJECT:Controls:Add(THIS-OBJECT:label1).
        THIS-OBJECT:Name = "HeaderControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(556, 23).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /**
     * Purpose: Destructor for the HeaderControl class
     * Notes:
     */
    DESTRUCTOR PUBLIC HeaderControl ():

    END DESTRUCTOR .

END CLASS.
