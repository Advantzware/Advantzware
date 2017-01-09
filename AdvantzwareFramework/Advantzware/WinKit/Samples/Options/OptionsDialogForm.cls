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
    File        : OptionsDialogForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 20 15:21:56 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* .
USING Advantzware.WinKit.Samples.* .
USING Progress.Lang.*.
USING Progress.Windows.Form.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Advantzware.WinKit.Samples.Options.OptionsDialogForm INHERITS Form   : 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PUBLIC VARIABLE Btn_OK AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE pbExit AS Infragistics.Win.Misc.UltraButton NO-UNDO.

    DEFINE PRIVATE VARIABLE Tabfolder AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.

    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    DEFINE PRIVATE VARIABLE appStylistRuntime1 AS Infragistics.Win.AppStyling.Runtime.AppStylistRuntime NO-UNDO.

    DEFINE VARIABLE cOldStyle AS CHARACTER NO-UNDO.

		
	CONSTRUCTOR PUBLIC OptionsDialogForm (  ):
		
		
        SUPER().
        InitializeComponent().
        
        ASSIGN cOldStyle = CAST (FrameworkSettings:MdiContainer, SampleMDIContainer):ApplicationStyle .
        
        CASE cOldStyle:
            WHEN "Office2007Blue":U THEN 
                ultraComboEditor1:Value = "blue" .    
            WHEN "Office2007Black":U THEN 
                ultraComboEditor1:Value = "black" .    
            WHEN "Office2007Silver":U THEN 
                ultraComboEditor1:Value = "silver" .    
        END CASE . 
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

	
	METHOD PRIVATE VOID InitializeComponent(  ):
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE valueListItem1 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem1 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE valueListItem2 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem2 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE valueListItem3 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem3 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Advantzware.WinKit.Samples.Options.OptionsDialogForm").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:pbExit = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:Btn_OK = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:Tabfolder = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:appStylistRuntime1 = NEW Infragistics.Win.AppStyling.Runtime.AppStylistRuntime(THIS-OBJECT:components).
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:Tabfolder, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:Tabfolder:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(1, 23).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1".
        THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(463, 202).
        /*  */
        /* ultraLabel2 */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel2:Appearance = appearance8.
        appearance9:FontData:UnderlineAsString = "True".
        appearance9:ForeColor = System.Drawing.SystemColors:HotTrack.
        THIS-OBJECT:ultraLabel2:HotTrackAppearance = appearance9.
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(118, 46).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2".
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(241, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 2.
        THIS-OBJECT:ultraLabel2:Text = "AppStylist Designer".
        THIS-OBJECT:ultraLabel2:UseHotTracking = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraLabel2:Click:Subscribe(THIS-OBJECT:ultraLabel2_Click).
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance5.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(3, 23).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1".
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(100, 17).
        THIS-OBJECT:ultraLabel1:TabIndex = 1.
        THIS-OBJECT:ultraLabel1:Text = "Display style:".
        /*  */
        /* ultraComboEditor1 */
        /*  */
        THIS-OBJECT:ultraComboEditor1:AutoCompleteMode = Infragistics.Win.AutoCompleteMode:Append.
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        valueListItem1:DataValue = "blue".
        valueListItem1:DisplayText = "Office 2007 blue".
        valueListItem2:DataValue = "black".
        valueListItem2:DisplayText = "Office 2007 black".
        valueListItem3:DataValue = "silver".
        valueListItem3:DisplayText = "Office 2007 silver".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.ValueListItem EXTENT 3 NO-UNDO.
        arrayvar0[1] = valueListItem1.
        arrayvar0[2] = valueListItem2.
        arrayvar0[3] = valueListItem3.
        THIS-OBJECT:ultraComboEditor1:Items:AddRange(arrayvar0).
        THIS-OBJECT:ultraComboEditor1:LimitToList = TRUE.
        THIS-OBJECT:ultraComboEditor1:Location = NEW System.Drawing.Point(118, 19).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1".
        THIS-OBJECT:ultraComboEditor1:Nullable = FALSE.
        THIS-OBJECT:ultraComboEditor1:Size = NEW System.Drawing.Size(241, 21).
        THIS-OBJECT:ultraComboEditor1:TabIndex = 0.
        THIS-OBJECT:ultraComboEditor1:ValueChanged:Subscribe(THIS-OBJECT:ultraComboEditor1_ValueChanged).
        /*  */
        /* pbExit */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:pbExit:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image"), System.Object).
        THIS-OBJECT:pbExit:Appearance = appearance1.
        THIS-OBJECT:pbExit:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:pbExit:Font = NEW System.Drawing.Font("Microsoft Sans Serif", 10.2399998, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:pbExit:ImageSize = NEW System.Drawing.Size(32, 32).
        THIS-OBJECT:pbExit:Location = NEW System.Drawing.Point(343, 247).
        THIS-OBJECT:pbExit:Name = "pbExit".
        THIS-OBJECT:pbExit:Size = NEW System.Drawing.Size(136, 40).
        THIS-OBJECT:pbExit:TabIndex = 3.
        THIS-OBJECT:pbExit:Text = "&Cancel".
        THIS-OBJECT:pbExit:Click:Subscribe(THIS-OBJECT:pbExit_Click).
        /*  */
        /* Btn_OK */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:Btn_OK:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance2:Image = CAST(resources:GetObject("appearance2.Image"), System.Object).
        THIS-OBJECT:Btn_OK:Appearance = appearance2.
        THIS-OBJECT:Btn_OK:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:Btn_OK:Font = NEW System.Drawing.Font("Microsoft Sans Serif", 10.2399998, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:Btn_OK:ImageSize = NEW System.Drawing.Size(32, 32).
        THIS-OBJECT:Btn_OK:Location = NEW System.Drawing.Point(201, 247).
        THIS-OBJECT:Btn_OK:Name = "Btn_OK".
        THIS-OBJECT:Btn_OK:Size = NEW System.Drawing.Size(136, 40).
        THIS-OBJECT:Btn_OK:TabIndex = 2.
        THIS-OBJECT:Btn_OK:Text = "&OK".
        /*  */
        /* Tabfolder */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:Tabfolder:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:Tabfolder:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:Tabfolder:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:Tabfolder:Location = NEW System.Drawing.Point(12, 13).
        THIS-OBJECT:Tabfolder:Name = "Tabfolder".
        THIS-OBJECT:Tabfolder:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        THIS-OBJECT:Tabfolder:Size = NEW System.Drawing.Size(467, 228).
        THIS-OBJECT:Tabfolder:TabIndex = 6.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        ultraTab1:Text = "Display Style".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 1 NO-UNDO.
        arrayvar1[1] = ultraTab1.
        THIS-OBJECT:Tabfolder:Tabs:AddRange(arrayvar1).
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(463, 202).
        /*  */
        /* OptionsDialogForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(491, 299).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:Tabfolder).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:pbExit).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:Btn_OK).
        THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:FixedDialog.
        THIS-OBJECT:Name = "OptionsDialogForm".
        THIS-OBJECT:Text = "Options".
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl1:PerformLayout().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:Tabfolder, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:Tabfolder:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID pbExit_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
        CAST (FrameworkSettings:MdiContainer, SampleMDIContainer):LoadApplicationStyle (cOldStyle) .		

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ultraComboEditor1_ValueChanged (INPUT sender AS System.Object, INPUT e AS System.EventArgs):
	
	    IF VALID-OBJECT (FrameworkSettings:MdiContainer) AND 
	       TYPE-OF (FrameworkSettings:MdiContainer, SampleMDIContainer) THEN DO:
	           
	        CASE STRING(UNBOX(ultraComboEditor1:Value)):
	            WHEN "blue" THEN
	                CAST (FrameworkSettings:MdiContainer, SampleMDIContainer):LoadApplicationStyle ("Office2007Blue") .	            
	            WHEN "black" THEN 
	                CAST (FrameworkSettings:MdiContainer, SampleMDIContainer):LoadApplicationStyle ("Office2007Black") .	                
	            WHEN "silver" THEN  
	                CAST (FrameworkSettings:MdiContainer, SampleMDIContainer):LoadApplicationStyle ("Office2007Silver") .
	        END CASE.	           
	    END.
	
	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ultraLabel2_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		THIS-OBJECT:Close () .
		THIS-OBJECT:appStylistRuntime1:ShowRuntimeApplicationStylingEditor (THIS-OBJECT:Owner, 
		                                                                    "Customize Application Styles") .

	END METHOD.


	DESTRUCTOR PUBLIC OptionsDialogForm ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.