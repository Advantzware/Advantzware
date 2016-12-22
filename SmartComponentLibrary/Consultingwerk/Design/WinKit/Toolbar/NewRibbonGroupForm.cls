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
    File        : NewRibbonGroupForm
    Purpose     : Provides an user interface to create a new Ribbon Group
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 08 23:03:31 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Design.WinKit.Toolbar.NewRibbonGroupForm INHERITS Form: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton2 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor2 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

		
	DEFINE PUBLIC PROPERTY ParentTab AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NewRibbonGroupForm class                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC NewRibbonGroupForm (  ):
		
        SUPER().
        InitializeComponent().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
        Purpose: Returns the entered values                                                                       
		Notes:  																	  
        @param pcKey OUTPUT The Ribbon Group Key value
        @param pcCaption OUTPUT The Ribbon Group Caption value                                           
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID GetValues (OUTPUT pcKey AS CHARACTER,
	                              OUTPUT pcCaption AS CHARACTER):
		
		ASSIGN pcKey = ultraTextEditor1:Text 
		       pcCaption = ultraTextEditor2:TEXT
		       .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID InitializeComponent(  ):
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Design.WinKit.Toolbar.NewRibbonGroupForm":U).
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTextEditor2 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton2 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraTextEditor2).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(1, 23).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(344, 111).
        /*  */
        /* ultraLabel2 */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel2:Appearance = appearance1.
        THIS-OBJECT:ultraLabel2:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(3, 45).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(46, 14).
        THIS-OBJECT:ultraLabel2:TabIndex = 2.
        THIS-OBJECT:ultraLabel2:Text = "&Caption:":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance2.
        THIS-OBJECT:ultraLabel1:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(3, 18).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(27, 14).
        THIS-OBJECT:ultraLabel1:TabIndex = 0.
        THIS-OBJECT:ultraLabel1:Text = "&Key:":U.
        /*  */
        /* ultraTextEditor2 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTextEditor2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTextEditor2:Location = NEW System.Drawing.Point(65, 41).
        THIS-OBJECT:ultraTextEditor2:Name = "ultraTextEditor2":U.
        THIS-OBJECT:ultraTextEditor2:Size = NEW System.Drawing.Size(261, 21).
        THIS-OBJECT:ultraTextEditor2:TabIndex = 3.
        /*  */
        /* ultraTextEditor1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTextEditor1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTextEditor1:Location = NEW System.Drawing.Point(65, 14).
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:Size = NEW System.Drawing.Size(261, 21).
        THIS-OBJECT:ultraTextEditor1:TabIndex = 1.
        THIS-OBJECT:ultraTextEditor1:Text = "New Group":U.
        THIS-OBJECT:ultraTextEditor1:TextChanged:Subscribe(THIS-OBJECT:ultraTextEditor1_TextChanged).
        /*  */
        /* ultraTabControl1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(348, 137).
        THIS-OBJECT:ultraTabControl1:TabIndex = 0.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        ultraTab1:Text = "Group Properties":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 1 NO-UNDO.
        arrayvar0[1] = ultraTab1.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar0).
        THIS-OBJECT:ultraTabControl1:UseAppStyling = FALSE.
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(344, 111).
        /*  */
        /* ultraButton1 */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ultraButton1:Enabled = FALSE.
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(201, 155).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton1:TabIndex = 1.
        THIS-OBJECT:ultraButton1:Text = "&Ok":U.
        /*  */
        /* ultraButton2 */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton2:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButton2:Location = NEW System.Drawing.Point(282, 155).
        THIS-OBJECT:ultraButton2:Name = "ultraButton2":U.
        THIS-OBJECT:ultraButton2:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton2:TabIndex = 2.
        THIS-OBJECT:ultraButton2:Text = "&Cancel":U.
        /*  */
        /* NewRibbonGroupForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(372, 190).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "NewRibbonGroupForm":U.
        THIS-OBJECT:SizeGripStyle = System.Windows.Forms.SizeGripStyle:Show.
        THIS-OBJECT:Text = "New Ribbon Group":U.
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl1:PerformLayout().
        CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.

	/*------------------------------------------------------------------------------
        Purpose: Raises the Load event                                                                        
			Notes:  																	  
        @param e The System.EventArgs with the data for this event
	------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnLoad (e AS System.EventArgs ):
		
		SUPER:OnLoad(INPUT e).

        ultraTextEditor1:Focus() .

	END METHOD.

	/*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the ultraTextEditor1 instance                                                                        
			Notes:  																	  
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                          
	------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTextEditor1_TextChanged (sender AS System.Object, 
                                                      e AS System.EventArgs ):
		
        IF ultraTextEditor1:Text > "":U THEN . 
		ELSE DO:
		    ultraButton1:Enabled = FALSE .
		    RETURN .
	    END.
		
		ultraButton1:Enabled = NOT THIS-OBJECT:ParentTab:Groups:Exists (ultraButton1:Text) .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the NewRibbonGroupForm class                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC NewRibbonGroupForm ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.
