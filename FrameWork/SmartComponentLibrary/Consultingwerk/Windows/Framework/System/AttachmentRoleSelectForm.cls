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
    File        : AttachmentRoleSelectForm
    Purpose     : Provides an user interface to select an attachment role
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 14 23:35:01 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.*                    FROM PROPATH .
USING Consultingwerk.Windows.Framework.*        FROM PROPATH .
USING Consultingwerk.Windows.Framework.System.* FROM PROPATH .  
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.AttachmentRoleSelectForm INHERITS BaseForm: 

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButtonOK AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButtonCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE pictureBox1 AS System.Windows.Forms.PictureBox NO-UNDO.
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the selected value (GUID of the Attachment Role) 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SelectedValue AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AttachmentRoleSelectForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttachmentRoleSelectForm ():
        
        
        SUPER().
        InitializeComponent().
        
        ultraComboEditor1:ValueList = ValueLists:GetAttachmentRoleValueList () .

        IF ultraComboEditor1:ValueList:ValueListItems:Count > 0 THEN 
            ultraComboEditor1:Value = ultraComboEditor1:ValueList:ValueListItems[0]:DataValue .

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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.AttachmentRoleSelectForm":U).
        THIS-OBJECT:pictureBox1 = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraButtonOK = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButtonCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* pictureBox1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:pictureBox1, "pictureBox1":U).
        THIS-OBJECT:pictureBox1:Name = "pictureBox1":U.
        THIS-OBJECT:pictureBox1:TabStop = FALSE.
        /*  */
        /* ultraLabel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* ultraButtonOK */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraButtonOK, "ultraButtonOK":U).
        THIS-OBJECT:ultraButtonOK:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ultraButtonOK:Name = "ultraButtonOK":U.
        /*  */
        /* ultraButtonCancel */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraButtonCancel, "ultraButtonCancel":U).
        THIS-OBJECT:ultraButtonCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButtonCancel:Name = "ultraButtonCancel":U.
        /*  */
        /* ultraComboEditor1 */
        /*  */
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:ultraComboEditor1:LimitToList = TRUE.
        resources:ApplyResources(THIS-OBJECT:ultraComboEditor1, "ultraComboEditor1":U).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:ValueChanged:Subscribe(THIS-OBJECT:ultraComboEditor1_ValueChanged).
        /*  */
        /* AttachmentRoleSelectForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:ultraButtonOK.
        THIS-OBJECT:CancelButton = THIS-OBJECT:ultraButtonCancel.
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:pictureBox1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButtonOK).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButtonCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:Name = "AttachmentRoleSelectForm":U.
        THIS-OBJECT:ShowIcon = FALSE.
        THIS-OBJECT:ShowInTaskbar = FALSE.
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ValueChanged event of the combo editor
        Notes:   
        @param sender The reference to the control that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraComboEditor1_ValueChanged (sender AS System.Object, 
                                                        e AS System.EventArgs):
        
        THIS-OBJECT:SelectedValue = UNBOX (ultraComboEditor1:Value) .

        THIS-OBJECT:ultraButtonOK:Enabled = Consultingwerk.CharacterType:IsNotNullOrEmpty (THIS-OBJECT:SelectedValue) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the AttachmentRoleSelectForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC AttachmentRoleSelectForm ():

    END DESTRUCTOR .

END CLASS.
