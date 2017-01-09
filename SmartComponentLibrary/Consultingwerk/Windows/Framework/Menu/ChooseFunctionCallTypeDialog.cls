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
    File        : ChooseFunctionCallTypeDialog
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Dec 04 18:48:40 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.BaseForm.

USING Consultingwerk.Windows.Framework.Menu.* FROM PROPATH .  
USING Progress.Lang.*                         FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Menu.ChooseFunctionCallTypeDialog 
    INHERITS BaseForm: 
	
    DEFINE PRIVATE VARIABLE components        AS System.ComponentModel.IContainer                  NO-UNDO.
    DEFINE PRIVATE VARIABLE CallTypeCombo     AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE pictureBox1 AS System.Windows.Forms.PictureBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButtonOK     AS Infragistics.Win.Misc.UltraButton                 NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButtonCancel AS Infragistics.Win.Misc.UltraButton                 NO-UNDO.

    DEFINE VARIABLE oValueList AS Infragistics.Win.ValueList NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButtonRefreshList AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Represents the Selected CallType
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY SelectedCallTypeValue AS CHARACTER NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ChooseFunctionCallTypeDialog class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ChooseFunctionCallTypeDialog ():
        SUPER ().
        
        InitializeComponent ().
        
        THIS-OBJECT:StoreFormPosition = FALSE . 
        THIS-OBJECT:MinimumSize = THIS-OBJECT:Size .
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Assignes the selected Value to the SelectedCallTypeValue property
        Notes:   
        @param sender The publisher of the ValueChanged event
        @param e An EventArgs instance which has no event data
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID CallTypeCombo_ValueChanged (sender AS System.Object, 
	                                                e      AS System.EventArgs):
		
        SelectedCallTypeValue = UNBOX (CallTypeCombo:SelectedItem:DataValue).

        ultraButtonOK:ENABLED = TRUE . 

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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Menu.ChooseFunctionCallTypeDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:CallTypeCombo = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraButtonCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButtonOK = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:pictureBox1 = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:ultraButtonRefreshList = NEW Infragistics.Win.Misc.UltraButton().
        CAST(THIS-OBJECT:CallTypeCombo, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* CallTypeCombo */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:CallTypeCombo:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:CallTypeCombo:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:CallTypeCombo:Location = NEW System.Drawing.Point(76, 32).
        THIS-OBJECT:CallTypeCombo:Name = "CallTypeCombo":U.
        THIS-OBJECT:CallTypeCombo:Size = NEW System.Drawing.Size(304, 21).
        THIS-OBJECT:CallTypeCombo:TabIndex = 0.
        THIS-OBJECT:CallTypeCombo:ValueChanged:Subscribe(THIS-OBJECT:CallTypeCombo_ValueChanged).
        /*  */
        /* ultraButtonCancel */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButtonCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButtonCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButtonCancel:Location = NEW System.Drawing.Point(305, 75).
        THIS-OBJECT:ultraButtonCancel:Name = "ultraButtonCancel":U.
        THIS-OBJECT:ultraButtonCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButtonCancel:TabIndex = 1.
        THIS-OBJECT:ultraButtonCancel:Text = "&Cancel":U.
        /*  */
        /* ultraButtonOK */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButtonOK:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButtonOK:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ultraButtonOK:Enabled = FALSE.
        THIS-OBJECT:ultraButtonOK:Location = NEW System.Drawing.Point(224, 75).
        THIS-OBJECT:ultraButtonOK:Name = "ultraButtonOK":U.
        THIS-OBJECT:ultraButtonOK:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButtonOK:TabIndex = 1.
        THIS-OBJECT:ultraButtonOK:Text = "&OK":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(76, 12).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(254, 14).
        THIS-OBJECT:ultraLabel1:TabIndex = 2.
        THIS-OBJECT:ultraLabel1:Text = "Select the call type of the menu function to create:":U.
        /*  */
        /* pictureBox1 */
        /*  */
        THIS-OBJECT:pictureBox1:Image = CAST(resources:GetObject("pictureBox1.Image":U), System.Drawing.Image).
        THIS-OBJECT:pictureBox1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:pictureBox1:Name = "pictureBox1":U.
        THIS-OBJECT:pictureBox1:Size = NEW System.Drawing.Size(48, 48).
        THIS-OBJECT:pictureBox1:SizeMode = System.Windows.Forms.PictureBoxSizeMode:AutoSize.
        THIS-OBJECT:pictureBox1:TabIndex = 3.
        THIS-OBJECT:pictureBox1:TabStop = FALSE.
        /*  */
        /* ultraButtonRefreshList */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButtonRefreshList:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        THIS-OBJECT:ultraButtonRefreshList:Appearance = appearance1.
        THIS-OBJECT:ultraButtonRefreshList:ImageSize = NEW System.Drawing.Size(13, 13).
        THIS-OBJECT:ultraButtonRefreshList:Location = NEW System.Drawing.Point(12, 75).
        THIS-OBJECT:ultraButtonRefreshList:MinimumSize = NEW System.Drawing.Size(178, 23).
        THIS-OBJECT:ultraButtonRefreshList:Name = "ultraButtonRefreshList":U.
        THIS-OBJECT:ultraButtonRefreshList:Size = NEW System.Drawing.Size(178, 23).
        THIS-OBJECT:ultraButtonRefreshList:TabIndex = 4.
        THIS-OBJECT:ultraButtonRefreshList:Text = "Refresh Function-Call types":U.
        THIS-OBJECT:ultraButtonRefreshList:Click:Subscribe(THIS-OBJECT:ultraButtonRefreshList_Click).
        /*  */
        /* ChooseFunctionCallTypeDialog */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:ultraButtonOK.
        THIS-OBJECT:CancelButton = THIS-OBJECT:ultraButtonCancel.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(398, 123).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButtonRefreshList).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:pictureBox1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButtonOK).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButtonCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:CallTypeCombo).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "ChooseFunctionCallTypeDialog":U.
        THIS-OBJECT:ShowIcon = FALSE.
        THIS-OBJECT:ShowInTaskbar = FALSE.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
        THIS-OBJECT:Text = "Select Function-Call Type":U.
        THIS-OBJECT:WindowPositionRegistryKey = "ChooseFunctionCallTypeDialog":U.
        CAST(THIS-OBJECT:CallTypeCombo, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Execute logic after the form has been shown
        Notes:   
        @param e An EventArgs instance which has no event data
    ------------------------------------------------------------------------------*/
	METHOD OVERRIDE PROTECTED VOID OnShown (INPUT e AS System.EventArgs):
		
		SUPER:OnShown(INPUT e).

        PROCESS EVENTS.
        
        THIS-OBJECT:PopulateCallTypeCombo ().

        /* Marko Rüterbories, Consultingwerk Ltd. 04.12.2012
           Hier wird die Selektion noch nicht am UI angezeigt.
           Warum: ?? */
        IF CallTypeCombo:Items:Count > 0 THEN DO: 
            CallTypeCombo:SelectedIndex = 0. 

            CallTypeCombo:DropDown ().
        END.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populate the CallTypeCombo with all available CallTypes
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID PopulateCallTypeCombo ():
        
        DEFINE VARIABLE oFunctionCallCacheService AS Consultingwerk.SmartFramework.IFunctionCallCacheService NO-UNDO .
        
        oFunctionCallCacheService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IFunctionCallCacheService
                                                                  "NEW Consultingwerk.SmartFramework.FunctionCallCacheService ()"}. 

        CallTypeCombo:ValueList = oFunctionCallCacheService:FunctionCallTypes.
        
        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Refresh the FunctionCall list
        Notes:   
        @param sender The publisher of the ValueChanged event
        @param e An EventArgs instance which has no event data
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraButtonRefreshList_Click (sender AS System.Object, 
	                                                  e AS System.EventArgs):
		
        DEFINE VARIABLE oFunctionCallCacheService AS Consultingwerk.SmartFramework.IFunctionCallCacheService NO-UNDO .
        
        oFunctionCallCacheService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IFunctionCallCacheService
                                                                  "NEW Consultingwerk.SmartFramework.FunctionCallCacheService ()"}. 
        
        oFunctionCallCacheService:InitializeService ().

        CallTypeCombo:ValueList = oFunctionCallCacheService:FunctionCallTypes.
        
        IF CallTypeCombo:Items:Count > 0 THEN DO: 
            CallTypeCombo:SelectedIndex = 0. 

            CallTypeCombo:DropDown ().
        END.

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ChooseFunctionCallTypeDialog class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ChooseFunctionCallTypeDialog ():

    END DESTRUCTOR .

END CLASS.
