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
    File        : ExcelExportFetchMoreRowsPromptForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 20 09:42:33 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.BaseForm.


USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Windows.Util.Forms.ExcelExportFetchMoreRowsPromptForm INHERITS BaseForm: 

    DEFINE PRIVATE VARIABLE btnOk AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE tbNumRows AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraCalcManager1 AS Infragistics.Win.UltraWinCalcManager.UltraCalcManager NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTrackBar1 AS Infragistics.Win.UltraWinEditors.UltraTrackBar NO-UNDO.

    DEFINE VARIABLE iNumberOfRecords AS INTEGER NO-UNDO EXTENT 9 
        INIT [0, 100, 250, 500, 1000, 2500, 5000, 10000, 25000].

    /*------------------------------------------------------------------------------
        Purpose: Returns the users selection for the number of rows 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Value AS INTEGER NO-UNDO 
	GET():
		RETURN UNBOX (THIS-OBJECT:tbNumRows:Value) .
	END GET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ExcelExportFetchMoreRowsPromptForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ExcelExportFetchMoreRowsPromptForm ():
		
        SUPER().
        InitializeComponent().
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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Util.Forms.ExcelExportFetchMoreRowsPromptForm":U).
        THIS-OBJECT:ultraTrackBar1 = NEW Infragistics.Win.UltraWinEditors.UltraTrackBar().
        THIS-OBJECT:btnOk = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraCalcManager1 = NEW Infragistics.Win.UltraWinCalcManager.UltraCalcManager(THIS-OBJECT:components).
        THIS-OBJECT:btnCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:tbNumRows = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:ultraTrackBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraCalcManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbNumRows, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTrackBar1 */
        /*  */
        THIS-OBJECT:ultraTrackBar1:LargeChange = 3.
        THIS-OBJECT:ultraTrackBar1:Location = NEW System.Drawing.Point(12, 73).
        THIS-OBJECT:ultraTrackBar1:MaxValue = 9.
        THIS-OBJECT:ultraTrackBar1:MinValue = 1.
        THIS-OBJECT:ultraTrackBar1:Name = "ultraTrackBar1":U.
        THIS-OBJECT:ultraTrackBar1:Size = NEW System.Drawing.Size(407, 29).
        THIS-OBJECT:ultraTrackBar1:TabIndex = 0.
        THIS-OBJECT:ultraTrackBar1:Value = 1.
        THIS-OBJECT:ultraTrackBar1:ValueObject = 1.
        THIS-OBJECT:ultraTrackBar1:ValueChanged:Subscribe(THIS-OBJECT:ultraTrackBar1_ValueChanged).
        /*  */
        /* btnOk */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnOk:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnOk:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:btnOk:Location = NEW System.Drawing.Point(263, 114).
        THIS-OBJECT:btnOk:Name = "btnOk":U.
        THIS-OBJECT:btnOk:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnOk:TabIndex = 1.
        THIS-OBJECT:btnOk:Text = "OK":U.
        /*  */
        /* ultraCalcManager1 */
        /*  */
        THIS-OBJECT:ultraCalcManager1:ContainingControl = THIS-OBJECT.
        /*  */
        /* btnCancel */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(344, 114).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCancel:TabIndex = 2.
        THIS-OBJECT:btnCancel:Text = "Cancel":U.
        /*  */
        /* tbNumRows */
        /*  */
        THIS-OBJECT:tbNumRows:Location = NEW System.Drawing.Point(319, 34).
        THIS-OBJECT:tbNumRows:Name = "tbNumRows":U.
        THIS-OBJECT:tbNumRows:Size = NEW System.Drawing.Size(100, 21).
        THIS-OBJECT:tbNumRows:SpinButtonDisplayStyle = Infragistics.Win.ButtonDisplayStyle:Always.
        THIS-OBJECT:tbNumRows:SpinIncrement = 250.
        THIS-OBJECT:tbNumRows:TabIndex = 3.
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(12, 29).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(301, 35).
        THIS-OBJECT:ultraLabel1:TabIndex = 4.
        THIS-OBJECT:ultraLabel1:Text = "Select the number of rows that should be fetched from the server in addition to the rows that are already on the client:":U.
        /*  */
        /* ExcelExportFetchMoreRowsPromptForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:btnOk.
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnCancel.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(431, 149).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbNumRows).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnOk).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTrackBar1).
        THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:FixedDialog.
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "ExcelExportFetchMoreRowsPromptForm":U.
        THIS-OBJECT:Text = "Excel Export":U.
        THIS-OBJECT:WindowPositionRegistryKey = "ExcelExportFetchMoreRowsPromptForm":U.
        CAST(THIS-OBJECT:ultraTrackBar1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraCalcManager1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbNumRows, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
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
		
		tbNumRows:Value = BOX (iNumberOfRecords [ultraTrackBar1:Value]) . 
		
		IF ultraTrackBar1:Value < 5 THEN 
		    tbNumRows:Appearance:ForeColor = System.Drawing.Color:Empty .
		ELSE IF ultraTrackBar1:Value < 7 THEN 
            tbNumRows:Appearance:ForeColor = System.Drawing.Color:Orange .
        ELSE 
            tbNumRows:Appearance:ForeColor = System.Drawing.Color:Red .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ExcelExportFetchMoreRowsPromptForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ExcelExportFetchMoreRowsPromptForm ():

    END DESTRUCTOR .

END CLASS.
