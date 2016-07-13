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
    File        : ErrorMessageDetailForm
    Purpose     : Displays detials about OpenEdge Runtime Errors 
                  (Progress.Lang.SysError)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Aug 17 21:31:16 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.*     FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH .  
USING Progress.Lang.*                     FROM PROPATH .
USING Progress.Windows.*                  FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Util.Forms.ErrorMessageDetailForm 
    INHERITS Form: 

    DEFINE PRIVATE VARIABLE button1 AS System.Windows.Forms.Button NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE textBox1 AS System.Windows.Forms.TextBox NO-UNDO.
    DEFINE PRIVATE VARIABLE pictureBox1 AS System.Windows.Forms.PictureBox NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
	
    DEFINE VARIABLE oFullWindowDragManager AS FullWindowDragManager           NO-UNDO EXTENT 3 .              
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ErrorMessageDetailForm class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ErrorMessageDetailForm ():

        SUPER().

        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        ASSIGN oFullWindowDragManager[1] = NEW FullWindowDragManager (THIS-OBJECT) 
               oFullWindowDragManager[2] = NEW FullWindowDragManager (THIS-OBJECT:panel1) 
               oFullWindowDragManager[3] = NEW FullWindowDragManager (THIS-OBJECT:pictureBox1) .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the button1
        Notes:   Closes the Form
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID button1_Click (sender AS System.Object, 
	                                   e AS System.EventArgs):
		
		THIS-OBJECT:Close () .
 
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shows the details about a Progress.Lang.SysError instance
        Notes:   Displays the Form
        @param poError The SysError to be shown
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID DisplayErrorDetails (poError AS Progress.Lang.SysError):
		
        DEFINE VARIABLE oMessageProvider AS IMessageProvider NO-UNDO . 		
		DEFINE VARIABLE i AS INTEGER NO-UNDO.

        oMessageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IMessageProvider} .
		
		DO i = 1 TO poError:NumMessages:
		
		    THIS-OBJECT:textBox1:Text = THIS-OBJECT:textBox1:Text +  
		                                (IF i > 1 THEN System.Environment:NewLine + System.Environment:NewLine ELSE "":U) + 
		                                oMessageProvider:GetMessageDetail ("OpenEdge":U, 
		                                                                   poError:GetMessageNum (i)) .   
		END.
		
		THIS-OBJECT:Show () .

	END METHOD .

	METHOD PRIVATE VOID InitializeComponent ():
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Util.Forms.ErrorMessageDetailForm":U).
        THIS-OBJECT:pictureBox1 = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:textBox1 = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:button1 = NEW System.Windows.Forms.Button().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:panel1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* pictureBox1 */
        /*  */
        THIS-OBJECT:pictureBox1:Image = CAST(resources:GetObject("pictureBox1.Image":U), System.Drawing.Image).
        THIS-OBJECT:pictureBox1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:pictureBox1:Name = "pictureBox1":U.
        THIS-OBJECT:pictureBox1:Size = NEW System.Drawing.Size(32, 32).
        THIS-OBJECT:pictureBox1:SizeMode = System.Windows.Forms.PictureBoxSizeMode:AutoSize.
        THIS-OBJECT:pictureBox1:TabIndex = 1.
        THIS-OBJECT:pictureBox1:TabStop = FALSE.
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:textBox1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:pictureBox1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Margin = NEW System.Windows.Forms.Padding(0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(579, 196).
        THIS-OBJECT:panel1:TabIndex = 7.
        /*  */
        /* textBox1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:textBox1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:textBox1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:textBox1:BorderStyle = System.Windows.Forms.BorderStyle:None.
        THIS-OBJECT:textBox1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:textBox1:Location = NEW System.Drawing.Point(50, 12).
        THIS-OBJECT:textBox1:Multiline = TRUE.
        THIS-OBJECT:textBox1:Name = "textBox1":U.
        THIS-OBJECT:textBox1:ReadOnly = TRUE.
        THIS-OBJECT:textBox1:Size = NEW System.Drawing.Size(517, 170).
        THIS-OBJECT:textBox1:TabIndex = 2.
        /*  */
        /* button1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:button1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:button1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:button1:Location = NEW System.Drawing.Point(488, 204).
        THIS-OBJECT:button1:Name = "button1":U.
        THIS-OBJECT:button1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:button1:TabIndex = 6.
        THIS-OBJECT:button1:Text = "&Ok":U.
        THIS-OBJECT:button1:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:button1:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:button1:Click:Subscribe(THIS-OBJECT:button1_Click).
        /*  */
        /* ErrorMessageDetailForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(577, 235).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:button1).
        THIS-OBJECT:Name = "ErrorMessageDetailForm":U.
        THIS-OBJECT:ShowIcon = FALSE.
        THIS-OBJECT:ShowInTaskbar = FALSE.
        THIS-OBJECT:SizeGripStyle = System.Windows.Forms.SizeGripStyle:Show.
        THIS-OBJECT:Text = "Details":U.
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ErrorMessageDetailForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ErrorMessageDetailForm ():

        Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oFullWindowDragManager[1]) .
        Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oFullWindowDragManager[2]) .
        Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oFullWindowDragManager[3]) .

    END DESTRUCTOR .

END CLASS.
