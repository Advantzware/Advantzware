/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : LoginForm
    Purpose     : User interface for user authentication
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 15 22:34:04 CET 2010
    Notes       : Supports visual inheritance to allow customization
                  - Images
                  - TabFolder size
                  - Login to BPM (visible)
                  - Open Task List (visible)
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

{Consultingwerk/products.i}

USING Consultingwerk.Forms.*                           FROM PROPATH .
USING Consultingwerk.Framework.*                       FROM PROPATH .
USING Consultingwerk.Framework.Session.*               FROM PROPATH .
USING Consultingwerk.SmartFramework.Authentication.*   FROM PROPATH .
USING Consultingwerk.OERA.*                            FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Consultingwerk.Windows.Session.*                 FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*              FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .
USING Progress.Windows.Form                            FROM ASSEMBLY .

CLASS Advantzware.Windows.SmartFramework.Authentication.LoginForm
    INHERITS BaseForm
    IMPLEMENTS Consultingwerk.Framework.Session.ILoginForm:

    DEFINE PRIVATE VARIABLE btnInfo AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor2 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton3 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnOk AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PROTECTED VARIABLE ultraPictureBox1 AS Infragistics.Win.UltraWinEditors.UltraPictureBox NO-UNDO.
    DEFINE PROTECTED VARIABLE ultraPictureBox2 AS Infragistics.Win.UltraWinEditors.UltraPictureBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PROTECTED VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor2 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    DEFINE VARIABLE oFullWindowDragManager AS Consultingwerk.Windows.Util.Forms.FullWindowDragManager NO-UNDO EXTENT 2 .

    DEFINE VARIABLE oClearButtonProvider   AS Consultingwerk.Windows.Controls.ClearButtonProvider NO-UNDO EXTENT 2 .

    DEFINE VARIABLE oLanguageProvider AS Consultingwerk.SmartFramework.ILanguageProvider NO-UNDO .
    DEFINE VARIABLE oLoginCompanyProvider AS Consultingwerk.SmartFramework.ILoginCompanyProvider NO-UNDO .
    DEFINE VARIABLE oImageProvider AS Consultingwerk.Windows.Framework.IImageProvider NO-UNDO .

    DEFINE VARIABLE lBpmLoginFailed AS LOGICAL NO-UNDO INIT FALSE .

    {Consultingwerk/SmartFramework/ttLanguage.i &NO-BEFORE &REFERENCE-ONLY=REFERENCE-ONLY}
    {Consultingwerk/SmartFramework/ttLoginCompany.i &NO-BEFORE &REFERENCE-ONLY=REFERENCE-ONLY}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LoginForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LoginForm ():

        SUPER().

        InitializeComponent().

        &IF DEFINED (OpenEdgeBPM) NE 0 &THEN
        ASSIGN THIS-OBJECT:toSignOnToBPM:Enabled = TRUE
               THIS-OBJECT:toSignOnToBPM:Checked = TRUE .
        &ENDIF

        THIS-OBJECT:StoreFormPosition  = FALSE .
        THIS-OBJECT:DeleteOnFormClosed = FALSE  .

        ASSIGN oLanguageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ILanguageProvider}
               oLoginCompanyProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ILoginCompanyProvider}
               oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider}.

        ASSIGN oClearButtonProvider[1] = NEW Consultingwerk.Windows.Controls.ClearButtonProvider (ultraTextEditor1)
               oClearButtonProvider[2] = NEW Consultingwerk.Windows.Controls.ClearButtonProvider (ultraTextEditor2) .

        &IF DEFINED (SmartComponentLibrary) NE 0 &THEN
        ultraButton3:Visible = TRUE .
        &ELSE
        ultraButton3:Visible = FALSE .
        &ENDIF

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
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Advantzware.Windows.SmartFramework.Authentication.LoginForm").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE valueListItem1 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem1 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE valueListItem2 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem2 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:btnInfo = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraComboEditor2 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraButton3 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnOk = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraTextEditor2 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraPictureBox1 = NEW Infragistics.Win.UltraWinEditors.UltraPictureBox().
        THIS-OBJECT:ultraPictureBox2 = NEW Infragistics.Win.UltraWinEditors.UltraPictureBox().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        CAST(THIS-OBJECT:ultraComboEditor2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:btnInfo).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraComboEditor2).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraButton3).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:btnOk).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraTextEditor2).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(1, 23).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1".
        THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(456, 210).
        /*  */
        /* btnInfo */
        /*  */
        appearance1:Image = CAST(resources:GetObject("appearance1.Image"), System.Object).
        appearance1:ImageHAlign = Infragistics.Win.HAlign:Center.
        appearance1:ImageVAlign = Infragistics.Win.VAlign:Middle.
        THIS-OBJECT:btnInfo:Appearance = appearance1.
        THIS-OBJECT:btnInfo:Location = NEW System.Drawing.Point(124, 208).
        THIS-OBJECT:btnInfo:Name = "btnInfo".
        THIS-OBJECT:btnInfo:Size = NEW System.Drawing.Size(27, 27).
        THIS-OBJECT:btnInfo:TabIndex = 13.
        THIS-OBJECT:btnInfo:Click:Subscribe(THIS-OBJECT:btnInfo_Click).
        /*  */
        /* ultraLabel4 */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel4:Appearance = appearance2.
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(36, 94).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4".
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(127, 23).
        THIS-OBJECT:ultraLabel4:TabIndex = 12.
        THIS-OBJECT:ultraLabel4:Text = "Session Language:".
        /*  */
        /* ultraComboEditor2 */
        /*  */
        THIS-OBJECT:ultraComboEditor2:AccessibleDescription = "Login company combo".
        THIS-OBJECT:ultraComboEditor2:AccessibleName = "company".
        THIS-OBJECT:ultraComboEditor2:AccessibleRole = System.Windows.Forms.AccessibleRole:List.
        THIS-OBJECT:ultraComboEditor2:DropDownButtonDisplayStyle = Infragistics.Win.ButtonDisplayStyle:OnMouseEnter.
        THIS-OBJECT:ultraComboEditor2:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        valueListItem1:DataValue = "Consultingwerk".
        valueListItem1:DisplayText = "Consultingwerk Ltd.".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.ValueListItem EXTENT 1 NO-UNDO.
        arrayvar0[1] = valueListItem1.
        THIS-OBJECT:ultraComboEditor2:Items:AddRange(arrayvar0).
        THIS-OBJECT:ultraComboEditor2:Location = NEW System.Drawing.Point(169, 90).
        THIS-OBJECT:ultraComboEditor2:Name = "ultraComboEditor2".
        THIS-OBJECT:ultraComboEditor2:Size = NEW System.Drawing.Size(195, 21).
        THIS-OBJECT:ultraComboEditor2:TabIndex = 11.
        /*  */
        /* ultraButton3 */
        /*  */
        THIS-OBJECT:ultraButton3:AccessibleDescription = "Change password".
        THIS-OBJECT:ultraButton3:AccessibleName = "btn change password".
        THIS-OBJECT:ultraButton3:AccessibleRole = System.Windows.Forms.AccessibleRole:PushButton.
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton3:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton3:Location = NEW System.Drawing.Point(3, 175).
        THIS-OBJECT:ultraButton3:Name = "ultraButton3".
        THIS-OBJECT:ultraButton3:Size = NEW System.Drawing.Size(114, 27).
        THIS-OBJECT:ultraButton3:TabIndex = 8.
        THIS-OBJECT:ultraButton3:Text = "Change Password".
        THIS-OBJECT:ultraButton3:Click:Subscribe(THIS-OBJECT:ultraButton3_Click).
        /*  */
        /* btnOk */
        /*  */
        THIS-OBJECT:btnOk:AccessibleDescription = "Login Button".
        THIS-OBJECT:btnOk:AccessibleName = "btn login".
        THIS-OBJECT:btnOk:AccessibleRole = System.Windows.Forms.AccessibleRole:PushButton.
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnOk:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnOk:Location = NEW System.Drawing.Point(273, 175).
        THIS-OBJECT:btnOk:Name = "btnOk".
        THIS-OBJECT:btnOk:Size = NEW System.Drawing.Size(75, 27).
        THIS-OBJECT:btnOk:TabIndex = 6.
        THIS-OBJECT:btnOk:Text = "&Login".
        THIS-OBJECT:btnOk:Click:Subscribe(THIS-OBJECT:btnOk_Click).
        /*  */
        /* btnCancel */
        /*  */
        THIS-OBJECT:btnCancel:AccessibleDescription = "Cancel button".
        THIS-OBJECT:btnCancel:AccessibleName = "btn cancel".
        THIS-OBJECT:btnCancel:AccessibleRole = System.Windows.Forms.AccessibleRole:PushButton.
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(354, 175).
        THIS-OBJECT:btnCancel:Name = "btnCancel".
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 27).
        THIS-OBJECT:btnCancel:TabIndex = 7.
        THIS-OBJECT:btnCancel:Text = "&Cancel".
        /*  */
        /* ultraLabel3 */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel3:Appearance = appearance3.
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(36, 121).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3".
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel3:TabIndex = 4.
        THIS-OBJECT:ultraLabel3:Text = "Login company:".
        THIS-OBJECT:ultraLabel3:Visible = FALSE.
        /*  */
        /* ultraLabel2 */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel2:Appearance = appearance4.
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(36, 67).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2".
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 2.
        THIS-OBJECT:ultraLabel2:Text = "Password:".
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance5.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(36, 40).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1".
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 0.
        THIS-OBJECT:ultraLabel1:Text = "User name:".
        /*  */
        /* ultraComboEditor1 */
        /*  */
        THIS-OBJECT:ultraComboEditor1:AccessibleDescription = "Login company combo".
        THIS-OBJECT:ultraComboEditor1:AccessibleName = "company".
        THIS-OBJECT:ultraComboEditor1:AccessibleRole = System.Windows.Forms.AccessibleRole:List.
        THIS-OBJECT:ultraComboEditor1:DropDownButtonDisplayStyle = Infragistics.Win.ButtonDisplayStyle:OnMouseEnter.
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        valueListItem2:DataValue = "Consultingwerk".
        valueListItem2:DisplayText = "Consultingwerk Ltd.".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.ValueListItem EXTENT 1 NO-UNDO.
        arrayvar1[1] = valueListItem2.
        THIS-OBJECT:ultraComboEditor1:Items:AddRange(arrayvar1).
        THIS-OBJECT:ultraComboEditor1:Location = NEW System.Drawing.Point(169, 117).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1".
        THIS-OBJECT:ultraComboEditor1:Size = NEW System.Drawing.Size(195, 21).
        THIS-OBJECT:ultraComboEditor1:TabIndex = 5.
        THIS-OBJECT:ultraComboEditor1:Text = "Consultingwerk".
        THIS-OBJECT:ultraComboEditor1:Visible = FALSE.
        /*  */
        /* ultraTextEditor2 */
        /*  */
        THIS-OBJECT:ultraTextEditor2:AccessibleDescription = "Password text box".
        THIS-OBJECT:ultraTextEditor2:AccessibleName = "password".
        THIS-OBJECT:ultraTextEditor2:AccessibleRole = System.Windows.Forms.AccessibleRole:Text.
        appearance6:FontData:Name = "Wingdings".
        THIS-OBJECT:ultraTextEditor2:Appearance = appearance6.
        THIS-OBJECT:ultraTextEditor2:Location = NEW System.Drawing.Point(169, 63).
        THIS-OBJECT:ultraTextEditor2:Name = "ultraTextEditor2".
        THIS-OBJECT:ultraTextEditor2:PasswordChar = 'l'.
        THIS-OBJECT:ultraTextEditor2:Size = NEW System.Drawing.Size(195, 21).
        THIS-OBJECT:ultraTextEditor2:TabIndex = 3.
        /*  */
        /* ultraTextEditor1 */
        /*  */
        THIS-OBJECT:ultraTextEditor1:AccessibleDescription = "User name text box".
        THIS-OBJECT:ultraTextEditor1:AccessibleName = "username".
        THIS-OBJECT:ultraTextEditor1:AccessibleRole = System.Windows.Forms.AccessibleRole:Text.
        THIS-OBJECT:ultraTextEditor1:Location = NEW System.Drawing.Point(169, 36).
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1".
        THIS-OBJECT:ultraTextEditor1:Size = NEW System.Drawing.Size(195, 21).
        THIS-OBJECT:ultraTextEditor1:TabIndex = 1.
        /*  */
        /* ultraPictureBox1 */
        /*  */
        THIS-OBJECT:ultraPictureBox1:AccessibleDescription = "SmartComponent Library".
        THIS-OBJECT:ultraPictureBox1:AccessibleName = "SmartComponent Library Logo".
        THIS-OBJECT:ultraPictureBox1:AccessibleRole = System.Windows.Forms.AccessibleRole:Graphic.
        appearance7:BorderColor = System.Drawing.Color:White.
        appearance7:BorderColor2 = System.Drawing.Color:White.
        THIS-OBJECT:ultraPictureBox1:Appearance = appearance7.
        THIS-OBJECT:ultraPictureBox1:AutoSize = TRUE.
        THIS-OBJECT:ultraPictureBox1:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraPictureBox1:BorderShadowColor = System.Drawing.Color:Empty.
        THIS-OBJECT:ultraPictureBox1:BorderStyle = Infragistics.Win.UIElementBorderStyle:None.
        THIS-OBJECT:ultraPictureBox1:Image = CAST(resources:GetObject("ultraPictureBox1.Image"), System.Object).
        THIS-OBJECT:ultraPictureBox1:Location = NEW System.Drawing.Point(4, 2).
        THIS-OBJECT:ultraPictureBox1:Margin = NEW System.Windows.Forms.Padding(6).
        THIS-OBJECT:ultraPictureBox1:Name = "ultraPictureBox1".
        THIS-OBJECT:ultraPictureBox1:Size = NEW System.Drawing.Size(396, 103).
        THIS-OBJECT:ultraPictureBox1:TabIndex = 0.
        /*  */
        /* ultraPictureBox2 */
        /*  */
        THIS-OBJECT:ultraPictureBox2:AccessibleDescription = "Footer image".
        THIS-OBJECT:ultraPictureBox2:AccessibleName = "footer image".
        THIS-OBJECT:ultraPictureBox2:AccessibleRole = System.Windows.Forms.AccessibleRole:Graphic.
        THIS-OBJECT:ultraPictureBox2:AutoSize = TRUE.
        THIS-OBJECT:ultraPictureBox2:BorderShadowColor = System.Drawing.Color:Empty.
        THIS-OBJECT:ultraPictureBox2:BorderStyle = Infragistics.Win.UIElementBorderStyle:None.
        THIS-OBJECT:ultraPictureBox2:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:ultraPictureBox2:Image = CAST(resources:GetObject("ultraPictureBox2.Image"), System.Object).
        THIS-OBJECT:ultraPictureBox2:Location = NEW System.Drawing.Point(0, 350).
        THIS-OBJECT:ultraPictureBox2:Name = "ultraPictureBox2".
        THIS-OBJECT:ultraPictureBox2:Size = NEW System.Drawing.Size(460, 12).
        THIS-OBJECT:ultraPictureBox2:TabIndex = 1.
        /*  */
        /* ultraTabControl1 */
        /*  */
        THIS-OBJECT:ultraTabControl1:AccessibleDescription = "User Credentials TabFolder".
        THIS-OBJECT:ultraTabControl1:AccessibleName = "TabFolder".
        THIS-OBJECT:ultraTabControl1:AccessibleRole = System.Windows.Forms.AccessibleRole:PageTab.
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(0, 114).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1".
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(460, 236).
        THIS-OBJECT:ultraTabControl1:TabIndex = 2.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        ultraTab1:Text = "User Credentials".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 1 NO-UNDO.
        arrayvar2[1] = ultraTab1.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar2).
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(456, 210).
        /*  */
        /* LoginForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:btnOk.
        THIS-OBJECT:AccessibleDescription = "Login".
        THIS-OBJECT:AccessibleName = "LoginDialog".
        THIS-OBJECT:AccessibleRole = System.Windows.Forms.AccessibleRole:Dialog.
        THIS-OBJECT:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnCancel.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(460, 362).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPictureBox2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPictureBox1).
        THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:FixedDialog.
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon"), System.Drawing.Icon).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "LoginForm".
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
        THIS-OBJECT:Text = "Login".
        THIS-OBJECT:WindowPositionRegistryKey = "LoginForm".
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl1:PerformLayout().
        CAST(THIS-OBJECT:ultraComboEditor2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosing event
        Notes:
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnFormClosing (e AS System.Windows.Forms.FormClosingEventArgs):

        IF lBpmLoginFailed THEN
            ASSIGN e:Cancel        = TRUE
                   lBpmLoginFailed = FALSE .

        SUPER:OnFormClosing (e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .
        DEFINE VARIABLE cLRUUserName     AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE cLRUCompanyName  AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE cLoginToBPM      AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE cStartTaskList   AS CHARACTER        NO-UNDO .

        SUPER:OnLoad (e) .

        oFullWindowDragManager[1] = NEW Consultingwerk.Windows.Util.Forms.FullWindowDragManager (ultraPictureBox1) .
        oFullWindowDragManager[2] = NEW Consultingwerk.Windows.Util.Forms.FullWindowDragManager (ultraPictureBox2) .

        /* Mike Fechner, Consultingwerk Ltd. 20.10.2011
           Get the last recently used user name from the Registry */

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        IF VALID-OBJECT (oSettingsService) THEN DO ON ERROR UNDO, THROW:
            cLRUUserName = oSettingsService:GetSetting ("UserInformation":U,
                                                        "LRUUsername":U) .
            cLRUCompanyName = oSettingsService:GetSetting ("UserInformation":U,
                                                            "LRUCompanyName":U) .

            IF cLRUUserName > "":U THEN
                THIS-OBJECT:ultraTextEditor1:Text = cLRUUserName .
            ELSE
                THIS-OBJECT:ultraTextEditor1:Text = System.Environment:UserName .

            IF cLRUCompanyName > "":U THEN
                THIS-OBJECT:ultraComboEditor1:Text = cLRUCompanyName .

            &IF DEFINED (OpenEdgeBPM) &THEN
            cLoginToBPM = oSettingsService:GetSetting ("UserInformation":U,
                                                       "LoginToBPM":U) .

            IF cLoginToBPM > "":U THEN
                THIS-OBJECT:toSignOnToBPM:Checked = DataTypeHelper:ToLogical (cLoginToBPM) .


            cStartTaskList = oSettingsService:GetSetting ("UserInformation":U,
                                                          "StartTaskListOnBPMLogin":U) .

            IF cStartTaskList > "":U THEN
                THIS-OBJECT:toStartTaskList:Checked = DataTypeHelper:ToLogical (cStartTaskList) .
            &ENDIF

            @SuppressUnusedWarnings.
            CATCH err AS Progress.Lang.Error :
                THIS-OBJECT:ultraTextEditor1:Text = "":U .
                THIS-OBJECT:ultraComboEditor1:Text = "":U .
            END CATCH.
        END .
        ELSE
            THIS-OBJECT:ultraTextEditor1:Text = System.Environment:UserName .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        DEFINE VARIABLE oValueListItem   AS Infragistics.Win.ValueListItem NO-UNDO .
        DEFINE VARIABLE cCurrentLanguage AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE cCurrentCompany  AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE oSettingsService AS ISettingsService               NO-UNDO .

        SUPER:OnShown (e) .

        /* Mike Fechner, Consultingwerk Ltd. 08.01.2013
           Retrieve Framework Languages */
        IF VALID-OBJECT (oLanguageProvider) THEN DO ON ERROR UNDO, THROW:
            ultraComboEditor2:ValueList:ValueListItems:Clear () .

            oLanguageProvider:GetAvailableLanguages (OUTPUT TABLE ttLanguage) .

            FOR EACH ttLanguage BY ttLanguage.LanguageName:
                oValueListItem = ultraComboEditor2:ValueList:ValueListItems:Add (BOX (ttLanguage.LanguageKey),
                                                                                 ttLanguage.LanguageName) .

                oValueListItem:Tag = BOX (ttLanguage.LanguageIsoCode) .

                IF ttLanguage.LanguageImage > "":U AND VALID-OBJECT (oImageProvider) THEN
                    oValueListItem:Appearance:Image = oImageProvider:ImageForKey (ttLanguage.LanguageImage) .
            END.

            IF VALID-OBJECT (oImageProvider) THEN
                ultraComboEditor2:ValueList:DisplayStyle = Infragistics.Win.ValueListDisplayStyle:DisplayTextAndPicture .

            cCurrentLanguage = oLanguageProvider:GetSessionDefaultLanguage () .

            IF cCurrentLanguage > "":U THEN
                ultraComboEditor2:Value = cCurrentLanguage .
            ELSE
                IF ultraComboEditor2:ValueList:ValueListItems:Count > 0 THEN
                    ultraComboEditor2:SelectedIndex = 0 .

            CATCH err AS Progress.Lang.Error :
                Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err, THIS-OBJECT) .
            END CATCH.
        END.

        /* Mike Fechner, Consultingwerk Ltd. 08.01.2013
           Retrieve Framework Login Companies */
        IF VALID-OBJECT (oLoginCompanyProvider) THEN DO ON ERROR UNDO, THROW:
            oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

            ultraComboEditor1:ValueList:ValueListItems:Clear () .

            oLoginCompanyProvider:GetAvailableLoginCompanies (OUTPUT TABLE ttLoginCompany) .

            FOR EACH ttLoginCompany BY ttLoginCompany.LoginCompanyName:
                oValueListItem = ultraComboEditor1:ValueList:ValueListItems:Add (BOX (ttLoginCompany.LoginCompanyKey),
                                                                                 ttLoginCompany.LoginCompanyName) .

                IF ttLoginCompany.LoginCompanyImageSmall > "":U AND VALID-OBJECT (oImageProvider) THEN
                    oValueListItem:Appearance:Image = oImageProvider:ImageForKey (ttLoginCompany.LoginCompanyImageSmall) .
            END.

            IF VALID-OBJECT (oImageProvider) THEN
                ultraComboEditor1:ValueList:DisplayStyle = Infragistics.Win.ValueListDisplayStyle:DisplayTextAndPicture .

            cCurrentLanguage = oLanguageProvider:GetSessionDefaultLanguage () .
            cCurrentCompany = oSettingsService:GetSetting ("UserInformation":U,
                                                            "LRUCompanyKey":U) .
            IF cCurrentCompany > "":U THEN
                ultraComboEditor1:Value = cCurrentCompany .
            ELSE
                IF ultraComboEditor1:ValueList:ValueListItems:Count > 0 THEN
                    ultraComboEditor1:SelectedIndex = 0 .

            CATCH err AS Progress.Lang.Error :
                Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err, THIS-OBJECT) .
            END CATCH.
        END.

        IF THIS-OBJECT:ultraTextEditor1:Text > "":U THEN
            THIS-OBJECT:ultraTextEditor2:Focus() .
        ELSE
            THIS-OBJECT:ultraTextEditor1:Focus() .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnOk
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnOk_Click (sender AS System.Object,
                                     e AS System.EventArgs):

        DEFINE VARIABLE oSettingsService     AS ISettingsService                                   NO-UNDO .
        DEFINE VARIABLE oUserCredentials     AS IUserCredentials                                   NO-UNDO .
        DEFINE VARIABLE oTranslationProvider AS Consultingwerk.SmartFramework.ITranslationProvider NO-UNDO .

        oUserCredentials = NEW UserCredentials (THIS-OBJECT:ultraTextEditor1:Text,
                                                THIS-OBJECT:ultraTextEditor2:Text,
                                                UNBOX (THIS-OBJECT:ultraComboEditor1:Value)) .

        IF VALID-OBJECT (THIS-OBJECT:ultraComboEditor1:SelectedItem) THEN
            oUserCredentials:CompanyName = THIS-OBJECT:ultraComboEditor1:SelectedItem:DisplayText .

        SessionManager:Login (oUserCredentials) .

        THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:OK .

        SessionManager:LanguageKey = UNBOX (THIS-OBJECT:ultraComboEditor2:Value) .

        oTranslationProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ITranslationProvider} .

        IF VALID-OBJECT (oTranslationProvider) THEN
            oTranslationProvider:CacheAllTranslations () .

        IF VALID-OBJECT (THIS-OBJECT:ultraComboEditor2:SelectedItem) THEN DO:
            SessionManager:LanguageDisplayName = THIS-OBJECT:ultraComboEditor2:SelectedItem:DisplayText .
            SessionManager:LanguageIsoCode = UNBOX (THIS-OBJECT:ultraComboEditor2:SelectedItem:Tag) .
        END.


        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        IF VALID-OBJECT (oSettingsService) THEN DO:
            oSettingsService:StoreSetting ("UserInformation":U,
                                           "LRUUsername":U,
                                           SessionManager:UserName) .
            oSettingsService:StoreSetting ("UserInformation":U,
                                           "LRUCompanyName":U,
                                           SessionManager:LoginCompanyName) .
            oSettingsService:StoreSetting ("UserInformation":U,
                                           "LRUCompanyKey":U,
                                           SessionManager:LoginCompanyKey) .
        END.

        &IF DEFINED (OpenEdgeBPM) NE 0 &THEN
        DEFINE VARIABLE oBPMAdapter AS Consultingwerk.BPM.IBPMAdapterProxy NO-UNDO .
        oBPMAdapter = { Consultingwerk/get-service.i Consultingwerk.BPM.IBPMAdapterProxy
                                                     "NEW Consultingwerk.BPM.BPMAdapterProxy ()"} .

        oSettingsService:StoreSetting ("UserInformation":U,
                                       "LoginToBPM":U,
                                       STRING (THIS-OBJECT:toSignOnToBPM:Checked, "Yes/No":U)) .

        oSettingsService:StoreSetting ("UserInformation":U,
                                       "StartTaskListOnBPMLogin":U,
                                       STRING (THIS-OBJECT:toStartTaskList:Checked, "Yes/No":U)) .

        IF THIS-OBJECT:toSignOnToBPM:Checked THEN DO:
            oBpmAdapter:StoreBpmCredentials (THIS-OBJECT:ultraTextEditor1:Text,
                                             THIS-OBJECT:ultraTextEditor2:Text) .

            Consultingwerk.OERA.ContextWrapper:SetLogicalContextProperty ("BpmSignOnToBpm":U,
                                                                          TRUE) .
            Consultingwerk.OERA.ContextWrapper:SetLogicalContextProperty ("BpmStartTaskList":U,
                                                                          THIS-OBJECT:toStartTaskList:Checked) .
        END.
        &ENDIF

        THIS-OBJECT:Close () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err, THIS-OBJECT) .

            IF THIS-OBJECT:ultraTextEditor1:Focused THEN
                THIS-OBJECT:ultraTextEditor1:SelectAll () .

            IF THIS-OBJECT:ultraTextEditor2:Focused THEN
                THIS-OBJECT:ultraTextEditor2:SelectAll () .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the click event for the btnInfo
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnInfo_Click (sender AS System.Object,
                                            e AS System.EventArgs):

        Consultingwerk.Windows.Session.SessionInfoForm:ShowSessionInformation() .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the click event for the ultraButton3 (ChangePassword)
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton3_Click (sender AS System.Object,
                                            e AS System.EventArgs ):

&IF DEFINED (SmartComponentLibrary) NE 0 &THEN
        DEFINE VARIABLE oChangePasswordForm AS ChangePasswordForm NO-UNDO.
        DEFINE VARIABLE oDialogResult       AS System.Windows.Forms.DialogResult NO-UNDO.

        oChangePasswordForm = NEW ChangePasswordForm (THIS-OBJECT:ultraTextEditor1:Text,
                                                      THIS-OBJECT:ultraTextEditor2:Text,
                                                      UNBOX (THIS-OBJECT:ultraComboEditor1:Value)).

        Consultingwerk.Util.FormHelper:ShowDialog (oChangePasswordForm, OUTPUT oDialogResult).

        FINALLY:
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oChangePasswordForm).
        END FINALLY.
&ENDIF

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the LoginForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC LoginForm ( ):

        GarbageCollectorHelper:DeleteObject (oClearButtonProvider[1]) .
        GarbageCollectorHelper:DeleteObject (oClearButtonProvider[2]) .

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.