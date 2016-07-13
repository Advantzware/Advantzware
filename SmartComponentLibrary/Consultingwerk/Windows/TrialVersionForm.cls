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
    File        : TrialVersionForm
    Purpose     : Nagscreen for Trial Versions of the SmartComponent Library
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 10 09:12:32 CET 2014
    Notes       : OK Button becomes active after five seconds, so that the 
                  used can close the form. Form closes itself after 15 seconds
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.*           FROM PROPATH .  
USING Consultingwerk.Windows.BaseForms.* FROM PROPATH .
USING Progress.Lang.*                    FROM PROPATH .
USING Progress.Util.*                    FROM ASSEMBLY .

CLASS Consultingwerk.Windows.TrialVersionForm 
    INHERITS BaseDialogForm: 

	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE pictureBox1 AS System.Windows.Forms.PictureBox NO-UNDO.
    DEFINE PRIVATE VARIABLE timer2 AS System.Windows.Forms.Timer NO-UNDO.
    DEFINE PRIVATE VARIABLE timer1 AS System.Windows.Forms.Timer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TrialVersionForm class  
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC TrialVersionForm ():
		
        SUPER().
        
        InitializeComponent().
        
        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Tick event of the timer1
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ButtonOk_Click (sender AS System.Object, e AS System.EventArgs):
		
		IF NOT THIS-OBJECT:Modal THEN 
		    THIS-OBJECT:Close () .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design  
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID InitializeComponent ():
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.TrialVersionForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:timer1 = NEW System.Windows.Forms.Timer(THIS-OBJECT:components).
        THIS-OBJECT:timer2 = NEW System.Windows.Forms.Timer(THIS-OBJECT:components).
        THIS-OBJECT:pictureBox1 = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ContentPanel:SuspendLayout().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ButtonOk */
        /*  */
        THIS-OBJECT:ButtonOk:Enabled = FALSE.
        THIS-OBJECT:ButtonOk:Location = NEW System.Drawing.Point(435, 154).
        THIS-OBJECT:ButtonOk:Click:Subscribe(THIS-OBJECT:ButtonOk_Click).
        /*  */
        /* ButtonCancel */
        /*  */
        THIS-OBJECT:ButtonCancel:Location = NEW System.Drawing.Point(435, 154).
        THIS-OBJECT:ButtonCancel:Visible = FALSE.
        /*  */
        /* ContentPanel */
        /*  */
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:pictureBox1).
        THIS-OBJECT:ContentPanel:Size = NEW System.Drawing.Size(522, 148).
        /*  */
        /* timer1 */
        /*  */
        THIS-OBJECT:timer1:Interval = 5000.
        THIS-OBJECT:timer1:Tick:Subscribe(THIS-OBJECT:timer1_Tick).
        /*  */
        /* timer2 */
        /*  */
        THIS-OBJECT:timer2:Interval = 10000.
        THIS-OBJECT:timer2:Tick:Subscribe(THIS-OBJECT:timer2_Tick).
        /*  */
        /* pictureBox1 */
        /*  */
        THIS-OBJECT:pictureBox1:Image = CAST(resources:GetObject("pictureBox1.Image":U), System.Drawing.Image).
        THIS-OBJECT:pictureBox1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:pictureBox1:Name = "pictureBox1":U.
        THIS-OBJECT:pictureBox1:Size = NEW System.Drawing.Size(498, 51).
        THIS-OBJECT:pictureBox1:SizeMode = System.Windows.Forms.PictureBoxSizeMode:AutoSize.
        THIS-OBJECT:pictureBox1:TabIndex = 0.
        THIS-OBJECT:pictureBox1:TabStop = FALSE.
        /*  */
        /* ultraLabel1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance1:FontData:BoldAsString = "True":U.
        appearance1:ForeColor = System.Drawing.Color:DarkRed.
        THIS-OBJECT:ultraLabel1:Appearance = appearance1.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(12, 87).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(498, 58).
        THIS-OBJECT:ultraLabel1:TabIndex = 1.
        THIS-OBJECT:ultraLabel1:Text = "You are using a trial version of the SmartComponent Library framework. This copy is licensed for evaluation purposes only for a limited time. ":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraLabel2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(12, 159).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(417, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 3.
        THIS-OBJECT:ultraLabel2:Text = "The SmartComponent Library is licensed by Consultingwerk Ltd.":U.
        /*  */
        /* TrialVersionForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(522, 189).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimumSize = NEW System.Drawing.Size(538, 228).
        THIS-OBJECT:Name = "TrialVersionForm":U.
        THIS-OBJECT:Text = "SmartComponent Library":U.
        THIS-OBJECT:TopMost = TRUE.
        THIS-OBJECT:Controls:SetChildIndex(THIS-OBJECT:ContentPanel, 0).
        THIS-OBJECT:Controls:SetChildIndex(THIS-OBJECT:ButtonOk, 0).
        THIS-OBJECT:Controls:SetChildIndex(THIS-OBJECT:ButtonCancel, 0).
        THIS-OBJECT:Controls:SetChildIndex(THIS-OBJECT:ultraLabel2, 0).
        THIS-OBJECT:ContentPanel:ResumeLayout(FALSE).
        THIS-OBJECT:ContentPanel:PerformLayout().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the shown event
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):
		
		SUPER:OnShown (e) .
		
		timer1:Enabled = TRUE . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Tick event of the timer1
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID timer1_Tick (sender AS System.Object, e AS System.EventArgs):
		
		ASSIGN timer1:Enabled   = FALSE
		       timer2:Enabled   = TRUE
		       ButtonOk:Enabled = TRUE . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Tick event of the timer2
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID timer2_Tick (sender AS System.Object, e AS System.EventArgs):
		
		THIS-OBJECT:Close () . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the TrialVersionForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC TrialVersionForm ():

    END DESTRUCTOR .

END CLASS.
