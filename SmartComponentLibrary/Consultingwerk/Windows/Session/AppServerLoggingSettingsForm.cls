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
    File        : AppServerLoggingSettingsForm
    Purpose     : Allows setting of logging options for the AppServer
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 13 22:24:13 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Consultingwerk.Forms.BaseForm.
USING Consultingwerk.Framework.Session.* FROM PROPATH.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Windows.Session.AppServerLoggingSettingsForm INHERITS BaseForm: 
	
    DEFINE PRIVATE VARIABLE btnClose AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE btnApplyClose AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE btnApply AS System.Windows.Forms.Button NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tgDebugMode AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbConsultingwerkLogManager AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbAblLogManager AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.

		
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AppServerLoggingSettingsForm class
        Notes:
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC AppServerLoggingSettingsForm ():
		
        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	    Purpose: Applies the current settings 
	    Notes:
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ApplySettings ():
		
		ASSIGN SessionManager:ServerLogEntryTypes    = tbAblLogManager:Text  
		       SessionManager:ServerCustomLogEntries = tbConsultingwerkLogManager:Text
		       SessionManager:ServerDebugMode        = tgDebugMode:Checked . 

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnClose
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID btnApply_Click (sender AS System.Object, 
	                                    e AS System.EventArgs):
		
        THIS-OBJECT:ApplySettings () . 

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnClose
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID btnApplyClose_Click (sender AS System.Object, 
                                             e AS System.EventArgs):
		
		THIS-OBJECT:ApplySettings () . 
		THIS-OBJECT:Close () .

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the Click event of the btnClose
	    Notes:
	    @param sender The reference to the object that raised the event
	    @param e The System.EventArgs with the data for this event 
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID btnClose_Click (sender AS System.Object, 
	                                    e AS System.EventArgs):
		
        THIS-OBJECT:Close () .

	END METHOD.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Session.AppServerLoggingSettingsForm":U).
        THIS-OBJECT:btnApplyClose = NEW System.Windows.Forms.Button().
        THIS-OBJECT:btnClose = NEW System.Windows.Forms.Button().
        THIS-OBJECT:tbAblLogManager = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbConsultingwerkLogManager = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tgDebugMode = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:btnApply = NEW System.Windows.Forms.Button().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        CAST(THIS-OBJECT:tbAblLogManager, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbConsultingwerkLogManager, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tgDebugMode, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:panel1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* btnApplyClose */
        /*  */
        resources:ApplyResources(THIS-OBJECT:btnApplyClose, "btnApplyClose":U).
        THIS-OBJECT:btnApplyClose:Name = "btnApplyClose":U.
        THIS-OBJECT:btnApplyClose:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnApplyClose:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:btnApplyClose:Click:Subscribe(THIS-OBJECT:btnApplyClose_Click).
        /*  */
        /* btnClose */
        /*  */
        resources:ApplyResources(THIS-OBJECT:btnClose, "btnClose":U).
        THIS-OBJECT:btnClose:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnClose:Name = "btnClose":U.
        THIS-OBJECT:btnClose:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnClose:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:btnClose:Click:Subscribe(THIS-OBJECT:btnClose_Click).
        /*  */
        /* tbAblLogManager */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbAblLogManager, "tbAblLogManager":U).
        THIS-OBJECT:tbAblLogManager:Name = "tbAblLogManager":U.
        /*  */
        /* tbConsultingwerkLogManager */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbConsultingwerkLogManager, "tbConsultingwerkLogManager":U).
        THIS-OBJECT:tbConsultingwerkLogManager:Name = "tbConsultingwerkLogManager":U.
        /*  */
        /* tgDebugMode */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tgDebugMode, "tgDebugMode":U).
        THIS-OBJECT:tgDebugMode:Name = "tgDebugMode":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel2, "ultraLabel2":U).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        /*  */
        /* btnApply */
        /*  */
        resources:ApplyResources(THIS-OBJECT:btnApply, "btnApply":U).
        THIS-OBJECT:btnApply:Name = "btnApply":U.
        THIS-OBJECT:btnApply:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnApply:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:btnApply:Click:Subscribe(THIS-OBJECT:btnApply_Click).
        /*  */
        /* panel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:panel1, "panel1":U).
        THIS-OBJECT:panel1:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tgDebugMode).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbConsultingwerkLogManager).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbAblLogManager).
        THIS-OBJECT:panel1:Name = "panel1":U.
        /*  */
        /* AppServerLoggingSettingsForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:btnApplyClose.
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnClose.
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnApply).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnClose).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnApplyClose).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "AppServerLoggingSettingsForm":U.
        CAST(THIS-OBJECT:tbAblLogManager, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbConsultingwerkLogManager, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tgDebugMode, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Raises the Shown event
	    Notes:
	    @param e The System.EventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):
		
		SUPER:OnShown (e) .
		
        ASSIGN tbAblLogManager:Text = SessionManager:ServerLogEntryTypes .
        
        IF SessionManager:ServerCustomLogEntries <> ? THEN 
            tbConsultingwerkLogManager:Text = SessionManager:ServerCustomLogEntries .  
    
        IF SessionManager:ServerDebugMode <> ? THEN 
            tgDebugMode:Checked = SessionManager:ServerDebugMode . 

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the AppServerLoggingSettingsForm class
        Notes:
    ------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC AppServerLoggingSettingsForm ():

	END DESTRUCTOR.

END CLASS.