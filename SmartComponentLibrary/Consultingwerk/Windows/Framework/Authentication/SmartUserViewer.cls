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
    File        : SmartUserViewer
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu Oct 11 21:28:24 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                        FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*             FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*             FROM PROPATH .
USING Consultingwerk.Windows.Framework.Authentication.* FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Authentication.SmartUserViewer
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components                                                 AS System.ComponentModel.IContainer                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartUser_UserSecurityId_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartUser_UserSecurityId AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE LanguageLookup                                             AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup        NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyLookup AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE ManagerUserNameTextEditor                                  AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE ManagerLookup                                              AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup        NO-UNDO.
    DEFINE PRIVATE VARIABLE LanguageTextEditor                                         AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel1                                                AS Infragistics.Win.Misc.UltraPanel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource_eSmartUser_LoginCompanyGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource_eSmartUser_UserPassword_Label            AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource_eSmartUser_UserPasswordChangedDate_Label AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource_eSmartUser_UserPasswordChangedDate       AS Infragistics.Win.UltraWinEditors.UltraDateTimeEditor                           NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource_eSmartUser_UserPassword                  AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource_eSmartUser_UserName_Label                AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource_eSmartUser_UserName                      AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource_eSmartUser_UserLastLogin_Label           AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource_eSmartUser_UserLastLogin                 AS Infragistics.Win.UltraWinEditors.UltraDateTimeEditor                           NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource_eSmartUser_UserFullName_Label            AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource_eSmartUser_UserFullName                  AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource_eSmartUser_UserEmail_Label               AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource_eSmartUser_UserEmail                     AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource_eSmartUser_ManagerUserGuid_Label         AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource_eSmartUser_LanguageGuid_Label            AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource                                          AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.

    CONSTRUCTOR PUBLIC SmartUserViewer ():

        DEFINE VARIABLE cMaintainPasswordInSmartDB AS CHARACTER              NO-UNDO.
        DEFINE VARIABLE oConfig                    AS IConfigurationProvider NO-UNDO .

        SUPER ().
        InitializeComponent ().

        THIS-OBJECT:SetControlEnabled(ArrayHelper:Array(UserBindingSource_eSmartUser_UserPasswordChangedDate,
                                                        UserBindingSource_eSmartUser_UserLastLogin,
                                                        LanguageTextEditor,
                                                        ManagerUserNameTextEditor),
                                      ControlEnabledEnum:Never) .

        /* Mike Fechner, Consultingwerk Ltd. 12.06.2015
           SCL-845: Security Id only required on OE11 */
        IF PROVERSION BEGINS "10":U THEN
            THIS-OBJECT:SetControlEnabled (eSmartUser_UserSecurityId,
                                           ControlEnabledEnum:Never) .

        THIS-OBJECT:LanguageLookup:InitializeLookupBrowserLayout:Subscribe (InitailizeLookupBrowserLayoutHandler).

        oConfig = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .

        IF VALID-OBJECT (oConfig) THEN DO:

            cMaintainPasswordInSmartDB = oConfig:GetValue ("maintainPasswordInSmartUser":U, "TRUE":U) .

            IF DataTypeHelper:ToLogical (cMaintainPasswordInSmartDB) <> TRUE THEN
                THIS-OBJECT:SetControlEnabled (UserBindingSource_eSmartUser_UserPassword,
                                               ControlEnabledEnum:Never).
        END.

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Initialize the Image column to show the flag images
        Notes:
        @param sender Reference to the Object which is calling the method
        @param e An EventArgs that contains the Event specific data
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitailizeLookupBrowserLayoutHandler (sender AS System.Object,
                                                                e      AS InitializeLookupBrowserLayoutEventArgs):

        IF e:Layout:Bands:Count > 0 AND
           e:Layout:Bands[0]:Columns:Exists ("LanguageImage":U) THEN

            e:Layout:Bands[0]:Columns["LanguageImage":U]:Style = Infragistics.Win.UltraWinGrid.ColumnStyle:ImageWithShadow.

    END METHOD .

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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartUser":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Authentication.SmartUserViewer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ClearValue":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton3 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton3 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton4 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton4 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:UserBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:UserBindingSource_eSmartUser_UserName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:UserBindingSource_eSmartUser_LanguageGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:UserBindingSource_eSmartUser_UserFullName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:UserBindingSource_eSmartUser_UserFullName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:UserBindingSource_eSmartUser_UserEmail_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:UserBindingSource_eSmartUser_UserEmail = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:UserBindingSource_eSmartUser_UserPassword_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:UserBindingSource_eSmartUser_UserPassword = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:UserBindingSource_eSmartUser_UserPasswordChangedDate_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:UserBindingSource_eSmartUser_UserPasswordChangedDate = NEW Infragistics.Win.UltraWinEditors.UltraDateTimeEditor().
        THIS-OBJECT:UserBindingSource_eSmartUser_UserLastLogin_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:UserBindingSource_eSmartUser_ManagerUserGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ManagerUserNameTextEditor = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraPanel1 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:LoginCompanyLookup = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:LanguageLookup = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:UserBindingSource_eSmartUser_UserLastLogin = NEW Infragistics.Win.UltraWinEditors.UltraDateTimeEditor().
        THIS-OBJECT:ManagerLookup = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:LanguageTextEditor = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:UserBindingSource_eSmartUser_LoginCompanyGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartUser_UserSecurityId = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:eSmartUser_UserSecurityId_Label = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:UserBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:UserBindingSource_eSmartUser_UserName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:UserBindingSource_eSmartUser_UserFullName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:UserBindingSource_eSmartUser_UserEmail, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:UserBindingSource_eSmartUser_UserPassword, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:UserBindingSource_eSmartUser_UserPasswordChangedDate, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ManagerUserNameTextEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraPanel1:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel1:SuspendLayout().
        CAST(THIS-OBJECT:LoginCompanyLookup, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:LanguageLookup, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:UserBindingSource_eSmartUser_UserLastLogin, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ManagerLookup, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:LanguageTextEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartUser_UserSecurityId, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* UserBindingSource */
        /*  */
        THIS-OBJECT:UserBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:UserBindingSource:EntityName = "Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U.
        THIS-OBJECT:UserBindingSource:EntityTable = "eSmartUser":U.
        THIS-OBJECT:UserBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:UserBindingSource:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 16 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("UserGuid":U, "UserGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("UserName":U, "UserName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("LanguageGuid":U, "LanguageGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("UserFullName":U, "UserFullName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("UserEmail":U, "UserEmail":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("UserPassword":U, "UserPassword":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("UserPasswordChangedDate":U, "UserPasswordChangedDate":U, Progress.Data.DataType:DATE).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("UserLastLogin":U, "UserLastLogin":U, Progress.Data.DataType:DATETIME_TZ).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("LoginCompanyGuid":U, "LoginCompanyGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("ManagerUserGuid":U, "ManagerUserGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[11] = NEW Progress.Data.ColumnPropDesc("UserSecurityId":U, "User Security Id":U, Progress.Data.DataType:INTEGER).
        arrayvar0[12] = NEW Progress.Data.ColumnPropDesc("LanguageIsoCode":U, "Language ISO Code":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[13] = NEW Progress.Data.ColumnPropDesc("LanguageName":U, "Language Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[14] = NEW Progress.Data.ColumnPropDesc("ManagerUserName":U, "Manager Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[15] = NEW Progress.Data.ColumnPropDesc("ManagerUserFullName":U, "Manager FullName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[16] = NEW Progress.Data.ColumnPropDesc("LoginCompanyName":U, "LoginCompanyName":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:UserBindingSource:TableSchema = tableDesc1.
        /*  */
        /* UserBindingSource_eSmartUser_UserName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:UserBindingSource_eSmartUser_UserName_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:UserBindingSource_eSmartUser_UserName_Label, "UserBindingSource_eSmartUser_UserName_Label":U).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserName_Label:Name = "UserBindingSource_eSmartUser_UserName_Label":U.
        /*  */
        /* UserBindingSource_eSmartUser_UserName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:UserBindingSource_eSmartUser_UserName, "UserBindingSource_eSmartUser_UserName":U).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:UserBindingSource, "UserName":U, TRUE)).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:UserBindingSource_eSmartUser_UserName:Name = "UserBindingSource_eSmartUser_UserName":U.
        /*  */
        /* UserBindingSource_eSmartUser_LanguageGuid_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:UserBindingSource_eSmartUser_LanguageGuid_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:UserBindingSource_eSmartUser_LanguageGuid_Label, "UserBindingSource_eSmartUser_LanguageGuid_Label":U).
        THIS-OBJECT:UserBindingSource_eSmartUser_LanguageGuid_Label:Name = "UserBindingSource_eSmartUser_LanguageGuid_Label":U.
        /*  */
        /* UserBindingSource_eSmartUser_UserFullName_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:UserBindingSource_eSmartUser_UserFullName_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:UserBindingSource_eSmartUser_UserFullName_Label, "UserBindingSource_eSmartUser_UserFullName_Label":U).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserFullName_Label:Name = "UserBindingSource_eSmartUser_UserFullName_Label":U.
        /*  */
        /* UserBindingSource_eSmartUser_UserFullName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:UserBindingSource_eSmartUser_UserFullName, "UserBindingSource_eSmartUser_UserFullName":U).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserFullName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:UserBindingSource, "UserFullName":U, TRUE)).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserFullName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:UserBindingSource_eSmartUser_UserFullName:Name = "UserBindingSource_eSmartUser_UserFullName":U.
        /*  */
        /* UserBindingSource_eSmartUser_UserEmail_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:UserBindingSource_eSmartUser_UserEmail_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:UserBindingSource_eSmartUser_UserEmail_Label, "UserBindingSource_eSmartUser_UserEmail_Label":U).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserEmail_Label:Name = "UserBindingSource_eSmartUser_UserEmail_Label":U.
        /*  */
        /* UserBindingSource_eSmartUser_UserEmail */
        /*  */
        resources:ApplyResources(THIS-OBJECT:UserBindingSource_eSmartUser_UserEmail, "UserBindingSource_eSmartUser_UserEmail":U).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserEmail:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:UserBindingSource, "UserEmail":U, TRUE)).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserEmail:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:UserBindingSource_eSmartUser_UserEmail:Name = "UserBindingSource_eSmartUser_UserEmail":U.
        /*  */
        /* UserBindingSource_eSmartUser_UserPassword_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:UserBindingSource_eSmartUser_UserPassword_Label:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:UserBindingSource_eSmartUser_UserPassword_Label, "UserBindingSource_eSmartUser_UserPassword_Label":U).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserPassword_Label:Name = "UserBindingSource_eSmartUser_UserPassword_Label":U.
        /*  */
        /* UserBindingSource_eSmartUser_UserPassword */
        /*  */
        resources:ApplyResources(THIS-OBJECT:UserBindingSource_eSmartUser_UserPassword, "UserBindingSource_eSmartUser_UserPassword":U).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserPassword:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:UserBindingSource, "UserPassword":U, TRUE)).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserPassword:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:UserBindingSource_eSmartUser_UserPassword:Name = "UserBindingSource_eSmartUser_UserPassword":U.
        THIS-OBJECT:UserBindingSource_eSmartUser_UserPassword:PasswordChar = '*':U.
        /*  */
        /* UserBindingSource_eSmartUser_UserPasswordChangedDate_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:UserBindingSource_eSmartUser_UserPasswordChangedDate_Label:Appearance = appearance6.
        resources:ApplyResources(THIS-OBJECT:UserBindingSource_eSmartUser_UserPasswordChangedDate_Label, "UserBindingSource_eSmartUser_UserPasswordChangedDate_Label":U).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserPasswordChangedDate_Label:Name = "UserBindingSource_eSmartUser_UserPasswordChangedDate_Label":U.
        /*  */
        /* UserBindingSource_eSmartUser_UserPasswordChangedDate */
        /*  */
        THIS-OBJECT:UserBindingSource_eSmartUser_UserPasswordChangedDate:AutoFillDate = Infragistics.Win.UltraWinMaskedEdit.AutoFillDate:MonthAndYear.
        THIS-OBJECT:UserBindingSource_eSmartUser_UserPasswordChangedDate:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:UserBindingSource, "UserPasswordChangedDate":U, TRUE)).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserPasswordChangedDate:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:UserBindingSource_eSmartUser_UserPasswordChangedDate, "UserBindingSource_eSmartUser_UserPasswordChangedDate":U).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserPasswordChangedDate:Name = "UserBindingSource_eSmartUser_UserPasswordChangedDate":U.
        /*  */
        /* UserBindingSource_eSmartUser_UserLastLogin_Label */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:UserBindingSource_eSmartUser_UserLastLogin_Label:Appearance = appearance7.
        resources:ApplyResources(THIS-OBJECT:UserBindingSource_eSmartUser_UserLastLogin_Label, "UserBindingSource_eSmartUser_UserLastLogin_Label":U).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserLastLogin_Label:Name = "UserBindingSource_eSmartUser_UserLastLogin_Label":U.
        /*  */
        /* UserBindingSource_eSmartUser_ManagerUserGuid_Label */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:UserBindingSource_eSmartUser_ManagerUserGuid_Label:Appearance = appearance8.
        resources:ApplyResources(THIS-OBJECT:UserBindingSource_eSmartUser_ManagerUserGuid_Label, "UserBindingSource_eSmartUser_ManagerUserGuid_Label":U).
        THIS-OBJECT:UserBindingSource_eSmartUser_ManagerUserGuid_Label:Name = "UserBindingSource_eSmartUser_ManagerUserGuid_Label":U.
        /*  */
        /* ManagerUserNameTextEditor */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ManagerUserNameTextEditor, "ManagerUserNameTextEditor":U).
        THIS-OBJECT:ManagerUserNameTextEditor:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:UserBindingSource, "ManagerUserFullName":U, TRUE)).
        THIS-OBJECT:ManagerUserNameTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ManagerUserNameTextEditor:Name = "ManagerUserNameTextEditor":U.
        /*  */
        /* ultraPanel1 */
        /*  */
        /*  */
        /* ultraPanel1.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:LoginCompanyLookup).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:LanguageLookup).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:UserBindingSource_eSmartUser_UserLastLogin).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ManagerLookup).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:LanguageTextEditor).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:UserBindingSource_eSmartUser_UserName_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ManagerUserNameTextEditor).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:UserBindingSource_eSmartUser_UserName).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:UserBindingSource_eSmartUser_ManagerUserGuid_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:UserBindingSource_eSmartUser_LanguageGuid_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:UserBindingSource_eSmartUser_UserFullName_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:UserBindingSource_eSmartUser_UserFullName).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:UserBindingSource_eSmartUser_UserLastLogin_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:UserBindingSource_eSmartUser_UserEmail_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:UserBindingSource_eSmartUser_UserPasswordChangedDate).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:UserBindingSource_eSmartUser_UserEmail).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:UserBindingSource_eSmartUser_UserPasswordChangedDate_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:UserBindingSource_eSmartUser_UserPassword_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:UserBindingSource_eSmartUser_UserPassword).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:UserBindingSource_eSmartUser_LoginCompanyGuid_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartUser_UserSecurityId).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartUser_UserSecurityId_Label).
        resources:ApplyResources(THIS-OBJECT:ultraPanel1, "ultraPanel1":U).
        THIS-OBJECT:ultraPanel1:Name = "ultraPanel1":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* LoginCompanyLookup */
        /*  */
        appearance9:Image = CAST(resources:GetObject("appearance9.Image":U), System.Object).
        appearance9:ImageHAlign = Infragistics.Win.HAlign:Center.
        editorButton1:Appearance = appearance9.
        editorButton1:Key = "ClearValue":U.
        THIS-OBJECT:LoginCompanyLookup:ButtonsLeft:Add(editorButton1).
        editorButton2:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton2, "editorButton2":U).
        THIS-OBJECT:LoginCompanyLookup:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:LoginCompanyLookup:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:UserBindingSource, "LoginCompanyGuid":U, TRUE)).
        THIS-OBJECT:LoginCompanyLookup:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:UserBindingSource, "LoginCompanyName":U, TRUE)).
        THIS-OBJECT:LoginCompanyLookup:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:LoginCompanyLookup, "LoginCompanyLookup":U).
        THIS-OBJECT:LoginCompanyLookup:LookupAdditionalFields = "":U.
        THIS-OBJECT:LoginCompanyLookup:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:LoginCompanyLookup:LookupBrowserFields = "LoginCompanyName":U.
        THIS-OBJECT:LoginCompanyLookup:LookupCascade = FALSE.
        THIS-OBJECT:LoginCompanyLookup:LookupControls = "":U.
        THIS-OBJECT:LoginCompanyLookup:LookupDialogClassName = "":U.
        THIS-OBJECT:LoginCompanyLookup:LookupDialogFilterFields = "LoginCompanyName":U.
        THIS-OBJECT:LoginCompanyLookup:LookupDialogFilterOperators = "BEGINS":U.
        THIS-OBJECT:LoginCompanyLookup:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:LoginCompanyLookup:LookupDialogQuerySort = "":U.
        THIS-OBJECT:LoginCompanyLookup:LookupDialogQueryString = "FOR EACH eSmartLoginCompany":U.
        THIS-OBJECT:LoginCompanyLookup:LookupEntityName = "Consultingwerk.SmartFramework.System.LoginCompanyBusinessEntity":U.
        THIS-OBJECT:LoginCompanyLookup:LookupEntityTable = "eSmartLoginCompany":U.
        THIS-OBJECT:LoginCompanyLookup:LookupEntityView = "":U.
        THIS-OBJECT:LoginCompanyLookup:LookupFields = "":U.
        THIS-OBJECT:LoginCompanyLookup:LookupKeyField = "LoginCompanyName":U.
        THIS-OBJECT:LoginCompanyLookup:LookupKeyValue = "":U.
        THIS-OBJECT:LoginCompanyLookup:LookupKeyValueColumn = "LoginCompanyGuid":U.
        THIS-OBJECT:LoginCompanyLookup:LookupQuerySort = "":U.
        THIS-OBJECT:LoginCompanyLookup:LookupQueryString = "FOR EACH eSmartLoginCompany WHERE eSmartLoginCompany.LoginCompanyName BEGINS ~"&1~"":U.
        THIS-OBJECT:LoginCompanyLookup:LookupTimerDelay = 750.
        THIS-OBJECT:LoginCompanyLookup:LookupTimerDisabled = FALSE.
        THIS-OBJECT:LoginCompanyLookup:Name = "LoginCompanyLookup":U.
        THIS-OBJECT:LoginCompanyLookup:OwningGrid = ?.
        THIS-OBJECT:LoginCompanyLookup:EditorButtonClick:Subscribe(THIS-OBJECT:LoginCompanyLookup_EditorButtonClick).
        THIS-OBJECT:LoginCompanyLookup:TextChanged:Subscribe(THIS-OBJECT:LoginCompanyLookup_TextChanged).
        /*  */
        /* LanguageLookup */
        /*  */
        editorButton3:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton3, "editorButton3":U).
        THIS-OBJECT:LanguageLookup:ButtonsRight:Add(editorButton3).
        THIS-OBJECT:LanguageLookup:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:UserBindingSource, "LanguageIsoCode":U, TRUE)).
        THIS-OBJECT:LanguageLookup:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:UserBindingSource, "LanguageGuid":U, TRUE)).
        THIS-OBJECT:LanguageLookup:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:LanguageLookup, "LanguageLookup":U).
        THIS-OBJECT:LanguageLookup:LookupAdditionalFields = "":U.
        THIS-OBJECT:LanguageLookup:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:LanguageLookup:LookupBrowserFields = "LanguageImage,LanguageName,LanguageIsoCode":U.
        THIS-OBJECT:LanguageLookup:LookupCascade = FALSE.
        THIS-OBJECT:LanguageLookup:LookupControls = "LanguageTextEditor":U.
        THIS-OBJECT:LanguageLookup:LookupDialogClassName = "":U.
        THIS-OBJECT:LanguageLookup:LookupDialogFilterFields = "LanguageName,LanguageIsoCode":U.
        THIS-OBJECT:LanguageLookup:LookupDialogFilterOperators = "BEGINS,BEGINS":U.
        THIS-OBJECT:LanguageLookup:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:LanguageLookup:LookupDialogQuerySort = "":U.
        THIS-OBJECT:LanguageLookup:LookupDialogQueryString = "FOR EACH eSmartLanguage":U.
        THIS-OBJECT:LanguageLookup:LookupEntityName = "Consultingwerk.SmartFramework.System.LanguageBusinessEntity":U.
        THIS-OBJECT:LanguageLookup:LookupEntityTable = "eSmartLanguage":U.
        THIS-OBJECT:LanguageLookup:LookupEntityView = "":U.
        THIS-OBJECT:LanguageLookup:LookupFields = "LanguageName":U.
        THIS-OBJECT:LanguageLookup:LookupKeyField = "LanguageIsoCode":U.
        THIS-OBJECT:LanguageLookup:LookupKeyValue = "":U.
        THIS-OBJECT:LanguageLookup:LookupKeyValueColumn = "LanguageGuid":U.
        THIS-OBJECT:LanguageLookup:LookupQuerySort = "":U.
        THIS-OBJECT:LanguageLookup:LookupQueryString = "FOR EACH eSmartLanguage WHERE eSmartLanguage.LanguageIsoCode = '&1'":U.
        THIS-OBJECT:LanguageLookup:LookupTimerDelay = 750.
        THIS-OBJECT:LanguageLookup:LookupTimerDisabled = FALSE.
        THIS-OBJECT:LanguageLookup:Name = "LanguageLookup":U.
        THIS-OBJECT:LanguageLookup:OwningGrid = ?.
        /*  */
        /* UserBindingSource_eSmartUser_UserLastLogin */
        /*  */
        THIS-OBJECT:UserBindingSource_eSmartUser_UserLastLogin:AutoFillDate = Infragistics.Win.UltraWinMaskedEdit.AutoFillDate:MonthAndYear.
        THIS-OBJECT:UserBindingSource_eSmartUser_UserLastLogin:AutoFillTime = Infragistics.Win.UltraWinMaskedEdit.AutoFillTime:CurrentTime.
        THIS-OBJECT:UserBindingSource_eSmartUser_UserLastLogin:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:UserBindingSource, "UserLastLogin":U, TRUE)).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserLastLogin:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:UserBindingSource_eSmartUser_UserLastLogin, "UserBindingSource_eSmartUser_UserLastLogin":U).
        THIS-OBJECT:UserBindingSource_eSmartUser_UserLastLogin:MaskInput = "~{LOC~}mm/dd/yyyy hh:mm:ss tt":U.
        THIS-OBJECT:UserBindingSource_eSmartUser_UserLastLogin:Name = "UserBindingSource_eSmartUser_UserLastLogin":U.
        /*  */
        /* ManagerLookup */
        /*  */
        editorButton4:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton4, "editorButton4":U).
        THIS-OBJECT:ManagerLookup:ButtonsRight:Add(editorButton4).
        THIS-OBJECT:ManagerLookup:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:UserBindingSource, "ManagerUserGuid":U, TRUE)).
        THIS-OBJECT:ManagerLookup:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:UserBindingSource, "ManagerUserName":U, TRUE)).
        THIS-OBJECT:ManagerLookup:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:ManagerLookup, "ManagerLookup":U).
        THIS-OBJECT:ManagerLookup:LookupAdditionalFields = "":U.
        THIS-OBJECT:ManagerLookup:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:ManagerLookup:LookupBrowserFields = "UserName,UserFullName,UserEmail":U.
        THIS-OBJECT:ManagerLookup:LookupCascade = FALSE.
        THIS-OBJECT:ManagerLookup:LookupControls = "ManagerUserNameTextEditor":U.
        THIS-OBJECT:ManagerLookup:LookupDialogClassName = "":U.
        THIS-OBJECT:ManagerLookup:LookupDialogFilterFields = "UserName":U.
        THIS-OBJECT:ManagerLookup:LookupDialogFilterOperators = "BEGINS":U.
        THIS-OBJECT:ManagerLookup:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:ManagerLookup:LookupDialogQuerySort = "":U.
        THIS-OBJECT:ManagerLookup:LookupDialogQueryString = "FOR EACH eSmartUser":U.
        THIS-OBJECT:ManagerLookup:LookupEntityName = "Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U.
        THIS-OBJECT:ManagerLookup:LookupEntityTable = "eSmartUser":U.
        THIS-OBJECT:ManagerLookup:LookupEntityView = "":U.
        THIS-OBJECT:ManagerLookup:LookupFields = "UserFullName":U.
        THIS-OBJECT:ManagerLookup:LookupKeyField = "UserName":U.
        THIS-OBJECT:ManagerLookup:LookupKeyValue = "":U.
        THIS-OBJECT:ManagerLookup:LookupKeyValueColumn = "UserGuid":U.
        THIS-OBJECT:ManagerLookup:LookupQuerySort = "":U.
        THIS-OBJECT:ManagerLookup:LookupQueryString = "FOR EACH eSmartUser WHERE eSmartUser.UserName = ~"&1~"":U.
        THIS-OBJECT:ManagerLookup:LookupTimerDelay = 750.
        THIS-OBJECT:ManagerLookup:LookupTimerDisabled = FALSE.
        THIS-OBJECT:ManagerLookup:Name = "ManagerLookup":U.
        THIS-OBJECT:ManagerLookup:OwningGrid = ?.
        /*  */
        /* LanguageTextEditor */
        /*  */
        resources:ApplyResources(THIS-OBJECT:LanguageTextEditor, "LanguageTextEditor":U).
        THIS-OBJECT:LanguageTextEditor:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:UserBindingSource, "LanguageName":U, TRUE)).
        THIS-OBJECT:LanguageTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:LanguageTextEditor:Name = "LanguageTextEditor":U.
        /*  */
        /* UserBindingSource_eSmartUser_LoginCompanyGuid_Label */
        /*  */
        appearance10:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:UserBindingSource_eSmartUser_LoginCompanyGuid_Label:Appearance = appearance10.
        resources:ApplyResources(THIS-OBJECT:UserBindingSource_eSmartUser_LoginCompanyGuid_Label, "UserBindingSource_eSmartUser_LoginCompanyGuid_Label":U).
        THIS-OBJECT:UserBindingSource_eSmartUser_LoginCompanyGuid_Label:Name = "UserBindingSource_eSmartUser_LoginCompanyGuid_Label":U.
        /*  */
        /* eSmartUser_UserSecurityId */
        /*  */
        THIS-OBJECT:eSmartUser_UserSecurityId:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:UserBindingSource, "UserSecurityId":U, TRUE)).
        THIS-OBJECT:eSmartUser_UserSecurityId:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartUser_UserSecurityId, "eSmartUser_UserSecurityId":U).
        THIS-OBJECT:eSmartUser_UserSecurityId:Name = "eSmartUser_UserSecurityId":U.
        THIS-OBJECT:eSmartUser_UserSecurityId:Nullable = TRUE.
        /*  */
        /* eSmartUser_UserSecurityId_Label */
        /*  */
        appearance11:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartUser_UserSecurityId_Label:Appearance = appearance11.
        resources:ApplyResources(THIS-OBJECT:eSmartUser_UserSecurityId_Label, "eSmartUser_UserSecurityId_Label":U).
        THIS-OBJECT:eSmartUser_UserSecurityId_Label:Name = "eSmartUser_UserSecurityId_Label":U.
        /*  */
        /* SmartUserViewer */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel1).
        THIS-OBJECT:Name = "SmartUserViewer":U.
        CAST(THIS-OBJECT:UserBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:UserBindingSource_eSmartUser_UserName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:UserBindingSource_eSmartUser_UserFullName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:UserBindingSource_eSmartUser_UserEmail, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:UserBindingSource_eSmartUser_UserPassword, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:UserBindingSource_eSmartUser_UserPasswordChangedDate, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ManagerUserNameTextEditor, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraPanel1:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ClientArea:PerformLayout().
        THIS-OBJECT:ultraPanel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:LoginCompanyLookup, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:LanguageLookup, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:UserBindingSource_eSmartUser_UserLastLogin, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ManagerLookup, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:LanguageTextEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartUser_UserSecurityId, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose:
        Notes:
        @param sender Reference to the Object which is calling the method
        @param e An EventArgs that contains the Event specific data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID LoginCompanyLookup_EditorButtonClick (sender AS System.Object,
                                                              e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
        CASE e:Button:Key:
            WHEN "ClearValue":U THEN DO:
                LoginCompanyLookup:Text           = "":U.
                LoginCompanyLookup:LookupKeyValue = "":U.
            END.
        END CASE.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Set LookupKeyValue to be able to erase the selected LoginCompany
        Notes:
        @param sender Reference to the Object which is calling the method
        @param e An EventArgs that contains no Event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID LoginCompanyLookup_TextChanged (sender AS System.Object,
                                                        e      AS System.EventArgs):

        IF LoginCompanyLookup:Text = "":U THEN
            LoginCompanyLookup:LookupKeyValue = "":U.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

END CLASS.
