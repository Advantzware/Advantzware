/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : AblBrowseMigrationSettingsForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 27 20:54:13 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.*                            FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*             FROM PROPATH .
USING Consultingwerk.Studio.LegacyGuiMigration.Browse.* FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*                FROM ASSEMBLY .
USING Progress.Lang.*                                   FROM PROPATH .
USING System.Windows.Forms.*                            FROM ASSEMBLY .

CLASS Consultingwerk.Studio.LegacyGuiMigration.Browse.AblBrowseMigrationSettingsForm 
    INHERITS BaseForm: 
	
    DEFINE PRIVATE VARIABLE btnOK AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS System.Windows.Forms.Button NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE label1 AS System.Windows.Forms.Label NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AblBrowseMigrationSettingsForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC AblBrowseMigrationSettingsForm ():
		
        SUPER().

        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        ASSIGN ultraTextEditor1:Text = AblBrowseMigrationSettings:CustomServices 
               label1:Text           = AblBrowseMigrationSettings:SettingsFile .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the Click event of the btnOK
	    Notes:
	    @param sender The reference to the object that raised the event
	    @param e The System.EventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID btnOK_Click (sender AS System.Object, 
	                                 e AS System.EventArgs):
		
		ASSIGN AblBrowseMigrationSettings:CustomServices = ultraTextEditor1:Text .

        AblBrowseMigrationSettings:SaveSettings () .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID InitializeComponent ():
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Studio.LegacyGuiMigration.Browse.AblBrowseMigrationSettingsForm":U).
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:btnOK = NEW System.Windows.Forms.Button().
        THIS-OBJECT:btnCancel = NEW System.Windows.Forms.Button().
        THIS-OBJECT:label1 = NEW System.Windows.Forms.Label().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(545, 107).
        THIS-OBJECT:panel1:TabIndex = 0.
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(149, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 4.
        THIS-OBJECT:ultraLabel1:Text = "Custom Services Definition:":U.
        /*  */
        /* ultraTextEditor1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTextEditor1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:ultraTextEditor1:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:ultraTextEditor1:Location = NEW System.Drawing.Point(167, 8).
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:Size = NEW System.Drawing.Size(364, 21).
        THIS-OBJECT:ultraTextEditor1:TabIndex = 5.
        THIS-OBJECT:ultraTextEditor1:EditorButtonClick:Subscribe(THIS-OBJECT:ultraTextEditor1_EditorButtonClick).
        THIS-OBJECT:ultraTextEditor1:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* btnOK */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnOK:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnOK:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:btnOK:Location = NEW System.Drawing.Point(375, 113).
        THIS-OBJECT:btnOK:Name = "btnOK":U.
        THIS-OBJECT:btnOK:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnOK:TabIndex = 1.
        THIS-OBJECT:btnOK:Text = "&OK":U.
        THIS-OBJECT:btnOK:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnOK:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:btnOK:Click:Subscribe(THIS-OBJECT:btnOK_Click).
        /*  */
        /* btnCancel */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(456, 113).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCancel:TabIndex = 3.
        THIS-OBJECT:btnCancel:Text = "&Cancel":U.
        THIS-OBJECT:btnCancel:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnCancel:UseVisualStyleBackColor = TRUE.
        /*  */
        /* label1 */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Bottom, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:label1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:label1:AutoEllipsis = TRUE.
        THIS-OBJECT:label1:Location = NEW System.Drawing.Point(12, 118).
        THIS-OBJECT:label1:Name = "label1":U.
        THIS-OBJECT:label1:Size = NEW System.Drawing.Size(357, 23).
        THIS-OBJECT:label1:TabIndex = 4.
        THIS-OBJECT:label1:UseCompatibleTextRendering = TRUE.
        /*  */
        /* AblBrowseMigrationSettingsForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:btnOK.
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnCancel.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(543, 148).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:label1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnOK).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MinimumSize = NEW System.Drawing.Size(559, 187).
        THIS-OBJECT:Name = "AblBrowseMigrationSettingsForm":U.
        THIS-OBJECT:Text = "ABL GUI WIDGET Browse Migration  Settings":U.
        THIS-OBJECT:WindowPositionRegistryKey = "AblBrowseMigrationSettingsForm":U.
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown handler of the Text boxes with an 
                 EditorButton (Lookup)
        Notes:   Maps F3 to EditorButtonClick
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID LookupKeyDownHandler (sender AS System.Object, 
                                              e AS System.Windows.Forms.KeyEventArgs):

        DEFINE VARIABLE cControlName AS CHARACTER NO-UNDO.
        
        IF e:Handled = FALSE AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, SmartLookupSettings:ShortcutKey) 
            AND THIS-OBJECT:Enabled THEN DO:

            ASSIGN cControlName = CAST (sender, System.Windows.Forms.Control):Name .

            DYNAMIC-INVOKE (THIS-OBJECT, 
                            SUBSTITUTE ("&1_EditorButtonClick":U, cControlName), 
                            sender, 
                            NEW EditorButtonEventArgs (?, ?)) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .                
        END CATCH.

    END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the EditorButtonClick event of the ultraTextEditor1
	    Notes:
	    @param sender The reference to the object that raised the event
	    @param e The EditorButtonEventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraTextEditor1_EditorButtonClick (sender AS System.Object,   
	                                                        e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
		
		DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO .
        DEFINE VARIABLE lOk       AS LOGICAL   NO-UNDO .		
		
		ASSIGN cFileName = ultraTextEditor1:Text .
		
	    SYSTEM-DIALOG GET-FILE cFileName 
	       FILTERS "XML Files (*.xml)":U "*.xml":U
	       INITIAL-FILTER 1
	       MUST-EXIST
	       RETURN-TO-START-DIR
	       TITLE "Open Custom Services File":U
	       USE-FILENAME
	       UPDATE lOk.	
		
		IF lOk THEN 
		  ASSIGN ultraTextEditor1:Text = cFileName . 

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the AblBrowseMigrationSettingsForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC AblBrowseMigrationSettingsForm ():

	END DESTRUCTOR.

END CLASS.
