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
    File        : ChangePasswordForm
    Purpose     : Allows changing the users password
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Mar 25 12:41:03 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Forms.*                              FROM PROPATH .
USING Consultingwerk.Framework.Session.*                  FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .

CLASS Consultingwerk.Windows.Session.ChangePasswordForm
    INHERITS BaseForm:

    DEFINE PRIVATE VARIABLE btnOK AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditorNewPWConfirmed AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditorNewPW AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditorCurrentPW AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabelNewPasswordConfirmed AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabelNewPassword AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabelCurrentPassword AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.

    DEFINE PRIVATE VARIABLE cUserName         AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE cLoginCompanyGuid AS CHARACTER NO-UNDO.

    DEFINE PRIVATE VARIABLE lErrorInChangePassword AS LOGICAL NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the form with initialization of user and possible
                 current password
        Notes:
        @param pcUserName The name of the user to change the password for
        @param pcCurrentPassword The current password
        @param pcLoginCompanyGuid The company the user belongs too
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ChangePasswordForm (pcUserName AS CHARACTER,
                                           pcCurrentPassword AS CHARACTER,
                                           pcLoginCompanyGuid AS CHARACTER):

        THIS-OBJECT ().

        ASSIGN
            cUserName                     = pcUserName
            cLoginCompanyGuid             = pcLoginCompanyGuid
            ultraTextEditorCurrentPW:Text = pcCurrentPassword
            .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the form
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ChangePasswordForm ():
        SUPER ().
        InitializeComponent ().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the click event for the btnOK
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnOK_Click (sender AS System.Object,
                                     e AS System.EventArgs):

        DEFINE VARIABLE oAuthenticationService AS Consultingwerk.Framework.IAuthenticationService NO-UNDO.

        oAuthenticationService = {Consultingwerk/get-service.i Consultingwerk.Framework.IAuthenticationService}.

        Consultingwerk.Assertion.ObjectAssert:IsValid (oAuthenticationService, "AuthenticationService":U).

        IF cLoginCompanyGuid = "":U AND SessionManager:LoginCompanyKey = "":U THEN
            UNDO, THROW NEW AppError ("You need to select a login company first to be able change your password!"{&tran}, 0).

        IF cUserName = "":U AND SessionManager:UserName = "":U THEN
            UNDO, THROW NEW AppError ("You need to enter a user name first to be able to change your password!"{&tran}, 0).

        IF cUserName <> "":U AND
           cLoginCompanyGuid <> "":U THEN
            oAuthenticationService:ChangePassword (cUserName,
                                                   cLoginCompanyGuid,
                                                   ultraTextEditorCurrentPW:Text,
                                                   ultraTextEditorNewPW:Text,
                                                   ultraTextEditorNewPWConfirmed:Text).
        ELSE
            oAuthenticationService:ChangePassword (SessionManager:UserName,
                                                   SessionManager:LoginCompanyKey,
                                                   ultraTextEditorCurrentPW:Text,
                                                   ultraTextEditorNewPW:Text,
                                                   ultraTextEditorNewPWConfirmed:Text).

        THIS-OBJECT:Close ().

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
            lErrorInChangePassword = TRUE.
        END CATCH.
    END METHOD.

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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Session.ChangePasswordForm":U).
        THIS-OBJECT:btnCancel = NEW System.Windows.Forms.Button().
        THIS-OBJECT:btnOK = NEW System.Windows.Forms.Button().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:ultraTextEditorNewPWConfirmed = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabelNewPasswordConfirmed = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTextEditorNewPW = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabelNewPassword = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTextEditorCurrentPW = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabelCurrentPassword = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:ultraTextEditorNewPWConfirmed, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditorNewPW, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditorCurrentPW, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* btnCancel */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(356, 105).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCancel:TabIndex = 0.
        THIS-OBJECT:btnCancel:Text = "&Cancel":U.
        THIS-OBJECT:btnCancel:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnCancel:UseVisualStyleBackColor = TRUE.
        /*  */
        /* btnOK */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnOK:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnOK:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:btnOK:Location = NEW System.Drawing.Point(275, 105).
        THIS-OBJECT:btnOK:Name = "btnOK":U.
        THIS-OBJECT:btnOK:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnOK:TabIndex = 1.
        THIS-OBJECT:btnOK:Text = "&OK":U.
        THIS-OBJECT:btnOK:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnOK:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:btnOK:Click:Subscribe(THIS-OBJECT:btnOK_Click).
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraTextEditorNewPWConfirmed).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabelNewPasswordConfirmed).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraTextEditorNewPW).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabelNewPassword).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraTextEditorCurrentPW).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabelCurrentPassword).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(1, 0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(443, 99).
        THIS-OBJECT:panel1:TabIndex = 2.
        /*  */
        /* ultraTextEditorNewPWConfirmed */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTextEditorNewPWConfirmed:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTextEditorNewPWConfirmed:Font = NEW System.Drawing.Font("Wingdings":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(2)).
        THIS-OBJECT:ultraTextEditorNewPWConfirmed:Location = NEW System.Drawing.Point(150, 62).
        THIS-OBJECT:ultraTextEditorNewPWConfirmed:Name = "ultraTextEditorNewPWConfirmed":U.
        THIS-OBJECT:ultraTextEditorNewPWConfirmed:PasswordChar = 'l':U.
        THIS-OBJECT:ultraTextEditorNewPWConfirmed:Size = NEW System.Drawing.Size(281, 21).
        THIS-OBJECT:ultraTextEditorNewPWConfirmed:TabIndex = 5.
        /*  */
        /* ultraLabelNewPasswordConfirmed */
        /*  */
        THIS-OBJECT:ultraLabelNewPasswordConfirmed:Location = NEW System.Drawing.Point(11, 66).
        THIS-OBJECT:ultraLabelNewPasswordConfirmed:Name = "ultraLabelNewPasswordConfirmed":U.
        THIS-OBJECT:ultraLabelNewPasswordConfirmed:Size = NEW System.Drawing.Size(143, 23).
        THIS-OBJECT:ultraLabelNewPasswordConfirmed:TabIndex = 4.
        THIS-OBJECT:ultraLabelNewPasswordConfirmed:Text = "New password confirmed":U.
        /*  */
        /* ultraTextEditorNewPW */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTextEditorNewPW:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTextEditorNewPW:Font = NEW System.Drawing.Font("Wingdings":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(2)).
        THIS-OBJECT:ultraTextEditorNewPW:Location = NEW System.Drawing.Point(150, 35).
        THIS-OBJECT:ultraTextEditorNewPW:Name = "ultraTextEditorNewPW":U.
        THIS-OBJECT:ultraTextEditorNewPW:PasswordChar = 'l':U.
        THIS-OBJECT:ultraTextEditorNewPW:Size = NEW System.Drawing.Size(281, 21).
        THIS-OBJECT:ultraTextEditorNewPW:TabIndex = 3.
        /*  */
        /* ultraLabelNewPassword */
        /*  */
        THIS-OBJECT:ultraLabelNewPassword:Location = NEW System.Drawing.Point(11, 39).
        THIS-OBJECT:ultraLabelNewPassword:Name = "ultraLabelNewPassword":U.
        THIS-OBJECT:ultraLabelNewPassword:Size = NEW System.Drawing.Size(143, 23).
        THIS-OBJECT:ultraLabelNewPassword:TabIndex = 2.
        THIS-OBJECT:ultraLabelNewPassword:Text = "New password":U.
        /*  */
        /* ultraTextEditorCurrentPW */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTextEditorCurrentPW:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTextEditorCurrentPW:Font = NEW System.Drawing.Font("Wingdings":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(2)).
        THIS-OBJECT:ultraTextEditorCurrentPW:Location = NEW System.Drawing.Point(150, 8).
        THIS-OBJECT:ultraTextEditorCurrentPW:Name = "ultraTextEditorCurrentPW":U.
        THIS-OBJECT:ultraTextEditorCurrentPW:PasswordChar = 'l':U.
        THIS-OBJECT:ultraTextEditorCurrentPW:Size = NEW System.Drawing.Size(281, 21).
        THIS-OBJECT:ultraTextEditorCurrentPW:TabIndex = 1.
        /*  */
        /* ultraLabelCurrentPassword */
        /*  */
        THIS-OBJECT:ultraLabelCurrentPassword:Location = NEW System.Drawing.Point(11, 12).
        THIS-OBJECT:ultraLabelCurrentPassword:Name = "ultraLabelCurrentPassword":U.
        THIS-OBJECT:ultraLabelCurrentPassword:Size = NEW System.Drawing.Size(143, 23).
        THIS-OBJECT:ultraLabelCurrentPassword:TabIndex = 0.
        THIS-OBJECT:ultraLabelCurrentPassword:Text = "Current password":U.
        /*  */
        /* ChangePasswordForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:btnOK.
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnCancel.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(443, 140).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnOK).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:MinimumSize = NEW System.Drawing.Size(459, 179).
        THIS-OBJECT:Name = "ChangePasswordForm":U.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
        THIS-OBJECT:Text = "Change Password":U.
        THIS-OBJECT:WindowPositionRegistryKey = "ChangePasswordForm":U.
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        CAST(THIS-OBJECT:ultraTextEditorNewPWConfirmed, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditorNewPW, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditorCurrentPW, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Prevent form from getting closed if there was an error in updating
                 the password.
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnClosing (e AS System.ComponentModel.CancelEventArgs):

        e:Cancel = lErrorInChangePassword.

        SUPER:OnClosing (e).

        FINALLY:
            lErrorInChangePassword = FALSE.
        END.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Override of the OnShown method to set the initial focus
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown(INPUT e).

        ultraTextEditorCurrentPW:Focus ().

    END METHOD.

END CLASS.