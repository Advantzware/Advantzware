/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ViewGeneratorWizardForm
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 03 11:57:37 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.* FROM PROPATH .
USING Consultingwerk.Forms.*                                 FROM PROPATH .
USING Consultingwerk.Framework.*                             FROM PROPATH .
USING Consultingwerk.Util.*                                  FROM PROPATH .
USING Consultingwerk.Windows.API.*                           FROM PROPATH .
USING Progress.Lang.*                                        FROM PROPATH .
USING System.Windows.Forms.*                                 FROM ASSEMBLY .

CLASS Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.ViewGeneratorWizardForm INHERITS BaseForm:

    DEFINE PRIVATE VARIABLE btnOk AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnTestKuibUri AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE cbModule AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE cbViewType AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbDataSourceName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbDataProviderName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel6 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbKendoUiBuilderUri AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbAppLocation AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel7 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    {Consultingwerk/Util/TempTables/ttFileNames.i}

    DEFINE VARIABLE REGISTRY_BASE_KEY AS CHARACTER INIT "Kendo UI Builder Generator":U NO-UNDO .

    /**
     * Purpose: Gets and sets the name of the Business Entity
     * Notes:
     */
    DEFINE PUBLIC PROPERTY BusinessEntityName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the name of the Entity Table
     * Notes:
     */
    DEFINE PUBLIC PROPERTY EntityTable AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the name of the view
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ViewName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Constructor for the ViewGeneratorWizardForm class
     * Notes:
     */
    CONSTRUCTOR PUBLIC ViewGeneratorWizardForm ():

        SUPER().

        InitializeComponent().

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /**
     * Purpose: Returns the KendoUiBuilderData with the values of the wizard
     * Notes:
     * @return The KendoUiBuilderData with the values of the wizard
     */
    METHOD PUBLIC KendoUiBuilderData GetKendoUiBuilderData():

        DEFINE VARIABLE oData AS KendoUiBuilderData NO-UNDO .

        ASSIGN oData = NEW KendoUiBuilderData()

               oData:ModuleName        = cbModule:Text
               oData:ViewName          = tbName:Text
               oData:ViewType          = cbViewType:Text
               oData:DataProviderName  = tbDataProviderName:Text
               oData:DataSourceName    = tbDataSourceName:Text
               oData:AppLocation       = tbAppLocation:Text
               oData:KendoUiBuilderUri = tbKendoUiBuilderUri:Text .

        RETURN oData .

    END METHOD.

    /**
     * Purpose: Event handler for the Click event of the btnOk
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID btnOk_Click (sender AS System.Object,
                                     e AS System.EventArgs):

        THIS-OBJECT:StoreSettings() .

    END METHOD .

    /**
     * Purpose: Event handler for the click event of the btnTestKuibUri
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID btnTestKuibUri_Click (sender AS System.Object,
                                              e AS System.EventArgs):

        Win32:ShellExecute ("open":U, tbKendoUiBuilderUri:Text) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the BeforeDropDown event of the cbModule
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The CancelEventArgs with the data for this event
     */
    METHOD PRIVATE VOID cbModule_BeforeDropDown (sender AS System.Object,
                                                 e AS System.ComponentModel.CancelEventArgs):

        DEFINE VARIABLE cShortFileName AS CHARACTER NO-UNDO.

        FileHelper:GetFileList(tbAppLocation:Text + "~\meta~\modules":U,
                               "*.json":U, OUTPUT TABLE ttFileNames) .

        cbModule:ValueList:ValueListItems:Clear () .

        FOR EACH ttFileNames ON ERROR UNDO, THROW:

            ASSIGN cShortFileName = FileHelper:ShortFileName (ttFileNames.FileName) .

            IF cShortFileName <> "Application.json":U THEN
                cbModule:ValueList:ValueListItems:Add (SUBSTRING (cShortFileName, 1, LENGTH (cShortFileName, "CHARACTER":U) - 5, "CHARACTER":U)) .
        END.

    END METHOD .

    /**
     * Purpose: Stores the settings to the user profile
     * Notes:
     */
    METHOD PROTECTED VOID StoreSettings():

        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        oSettingsService:StoreSetting (REGISTRY_BASE_KEY, "AppLocation":U,         tbAppLocation:Text) .
        oSettingsService:StoreSetting (REGISTRY_BASE_KEY, "KendoUiBuilderUri":U,   tbKendoUiBuilderUri:Text) .

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
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
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
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.ViewGeneratorWizardForm":U).
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:ultraLabel7 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:cbModule = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:tbDataSourceName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbDataProviderName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:cbViewType = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:tbName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:btnTestKuibUri = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:tbKendoUiBuilderUri = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbAppLocation = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel6 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:btnOk = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:cbModule, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbDataSourceName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbDataProviderName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:cbViewType, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbKendoUiBuilderUri, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbAppLocation, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel7).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:cbModule).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbDataSourceName).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbDataProviderName).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:cbViewType).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbName).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:btnTestKuibUri).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbKendoUiBuilderUri).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbAppLocation).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel6).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(693, 263).
        THIS-OBJECT:panel1:TabIndex = 0.
        /*  */
        /* ultraLabel7 */
        /*  */
        appearance1:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraLabel7:Appearance = appearance1.
        THIS-OBJECT:ultraLabel7:Location = NEW System.Drawing.Point(12, 16).
        THIS-OBJECT:ultraLabel7:Name = "ultraLabel7":U.
        THIS-OBJECT:ultraLabel7:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel7:TabIndex = 0.
        THIS-OBJECT:ultraLabel7:Text = "Module Name:":U.
        /*  */
        /* cbModule */
        /*  */
        THIS-OBJECT:cbModule:Location = NEW System.Drawing.Point(139, 12).
        THIS-OBJECT:cbModule:Name = "cbModule":U.
        THIS-OBJECT:cbModule:Size = NEW System.Drawing.Size(542, 21).
        THIS-OBJECT:cbModule:TabIndex = 1.
        THIS-OBJECT:cbModule:BeforeDropDown:Subscribe(THIS-OBJECT:cbModule_BeforeDropDown).
        THIS-OBJECT:cbModule:ValueChanged:Subscribe(THIS-OBJECT:tbAppLocation_ValueChanged).
        /*  */
        /* tbDataSourceName */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDataSourceName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDataSourceName:Location = NEW System.Drawing.Point(139, 120).
        THIS-OBJECT:tbDataSourceName:Name = "tbDataSourceName":U.
        THIS-OBJECT:tbDataSourceName:Size = NEW System.Drawing.Size(542, 21).
        THIS-OBJECT:tbDataSourceName:TabIndex = 9.
        THIS-OBJECT:tbDataSourceName:ValueChanged:Subscribe(THIS-OBJECT:tbAppLocation_ValueChanged).
        /*  */
        /* ultraLabel4 */
        /*  */
        appearance2:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraLabel4:Appearance = appearance2.
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(12, 124).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel4:TabIndex = 8.
        THIS-OBJECT:ultraLabel4:Text = "Data Source:":U.
        /*  */
        /* tbDataProviderName */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDataProviderName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDataProviderName:Location = NEW System.Drawing.Point(139, 93).
        THIS-OBJECT:tbDataProviderName:Name = "tbDataProviderName":U.
        THIS-OBJECT:tbDataProviderName:Size = NEW System.Drawing.Size(542, 21).
        THIS-OBJECT:tbDataProviderName:TabIndex = 7.
        THIS-OBJECT:tbDataProviderName:ValueChanged:Subscribe(THIS-OBJECT:tbAppLocation_ValueChanged).
        /*  */
        /* ultraLabel3 */
        /*  */
        appearance3:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraLabel3:Appearance = appearance3.
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(12, 97).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel3:TabIndex = 6.
        THIS-OBJECT:ultraLabel3:Text = "Data Provider:":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        appearance4:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraLabel2:Appearance = appearance4.
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(12, 70).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 4.
        THIS-OBJECT:ultraLabel2:Text = "Type:":U.
        /*  */
        /* cbViewType */
        /*  */
        THIS-OBJECT:cbViewType:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:cbViewType:Location = NEW System.Drawing.Point(139, 66).
        THIS-OBJECT:cbViewType:Name = "cbViewType":U.
        THIS-OBJECT:cbViewType:Size = NEW System.Drawing.Size(542, 21).
        THIS-OBJECT:cbViewType:TabIndex = 5.
        /*  */
        /* tbName */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbName:Location = NEW System.Drawing.Point(139, 39).
        THIS-OBJECT:tbName:Name = "tbName":U.
        THIS-OBJECT:tbName:Size = NEW System.Drawing.Size(542, 21).
        THIS-OBJECT:tbName:TabIndex = 3.
        THIS-OBJECT:tbName:ValueChanged:Subscribe(THIS-OBJECT:tbAppLocation_ValueChanged).
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance5:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraLabel1:Appearance = appearance5.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(12, 43).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 2.
        THIS-OBJECT:ultraLabel1:Text = "Name:":U.
        /*  */
        /* btnTestKuibUri */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:btnTestKuibUri:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnTestKuibUri:Location = NEW System.Drawing.Point(642, 195).
        THIS-OBJECT:btnTestKuibUri:Name = "btnTestKuibUri":U.
        THIS-OBJECT:btnTestKuibUri:Size = NEW System.Drawing.Size(39, 23).
        THIS-OBJECT:btnTestKuibUri:TabIndex = 14.
        THIS-OBJECT:btnTestKuibUri:Text = "Test":U.
        THIS-OBJECT:btnTestKuibUri:Click:Subscribe(THIS-OBJECT:btnTestKuibUri_Click).
        /*  */
        /* tbKendoUiBuilderUri */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbKendoUiBuilderUri:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbKendoUiBuilderUri:Location = NEW System.Drawing.Point(139, 196).
        THIS-OBJECT:tbKendoUiBuilderUri:Name = "tbKendoUiBuilderUri":U.
        THIS-OBJECT:tbKendoUiBuilderUri:Size = NEW System.Drawing.Size(497, 21).
        THIS-OBJECT:tbKendoUiBuilderUri:TabIndex = 13.
        THIS-OBJECT:tbKendoUiBuilderUri:ValueChanged:Subscribe(THIS-OBJECT:tbAppLocation_ValueChanged).
        /*  */
        /* tbAppLocation */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbAppLocation:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:tbAppLocation:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tbAppLocation:Location = NEW System.Drawing.Point(139, 167).
        THIS-OBJECT:tbAppLocation:Name = "tbAppLocation":U.
        THIS-OBJECT:tbAppLocation:Size = NEW System.Drawing.Size(542, 21).
        THIS-OBJECT:tbAppLocation:TabIndex = 11.
        THIS-OBJECT:tbAppLocation:ValueChanged:Subscribe(THIS-OBJECT:tbAppLocation_ValueChanged).
        THIS-OBJECT:tbAppLocation:EditorButtonClick:Subscribe(THIS-OBJECT:tbAppLocation_EditorButtonClick).
        /*  */
        /* ultraLabel5 */
        /*  */
        appearance6:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraLabel5:Appearance = appearance6.
        THIS-OBJECT:ultraLabel5:Location = NEW System.Drawing.Point(12, 200).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        THIS-OBJECT:ultraLabel5:Size = NEW System.Drawing.Size(121, 23).
        THIS-OBJECT:ultraLabel5:TabIndex = 12.
        THIS-OBJECT:ultraLabel5:Text = "Kendo UI Builder URI:":U.
        /*  */
        /* ultraLabel6 */
        /*  */
        appearance7:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraLabel6:Appearance = appearance7.
        THIS-OBJECT:ultraLabel6:Location = NEW System.Drawing.Point(12, 171).
        THIS-OBJECT:ultraLabel6:Name = "ultraLabel6":U.
        THIS-OBJECT:ultraLabel6:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel6:TabIndex = 10.
        THIS-OBJECT:ultraLabel6:Text = "App location:":U.
        /*  */
        /* btnOk */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnOk:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnOk:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:btnOk:Location = NEW System.Drawing.Point(525, 269).
        THIS-OBJECT:btnOk:Name = "btnOk":U.
        THIS-OBJECT:btnOk:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnOk:TabIndex = 1.
        THIS-OBJECT:btnOk:Text = "&OK":U.
        THIS-OBJECT:btnOk:Click:Subscribe(THIS-OBJECT:btnOk_Click).
        /*  */
        /* btnCancel */
        /*  */
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar8 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(606, 269).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCancel:TabIndex = 2.
        THIS-OBJECT:btnCancel:Text = "&Cancel":U.
        /*  */
        /* ViewGeneratorWizardForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:btnOk.
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnCancel.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(693, 304).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnOk).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "ViewGeneratorWizardForm":U.
        THIS-OBJECT:Text = "Kendo UI Builder View Generator":U.
        THIS-OBJECT:WindowPositionRegistryKey = "ViewGeneratorWizardForm":U.
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        CAST(THIS-OBJECT:cbModule, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbDataSourceName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbDataProviderName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:cbViewType, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbKendoUiBuilderUri, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbAppLocation, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /**
     * Purpose: Initializes the Wizard
     * Notes:
     */
    METHOD PROTECTED VOID InitializeWizard():

        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .
        DEFINE VARIABLE cSetting         AS CHARACTER        NO-UNDO .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        tbName:Text = THIS-OBJECT:ViewName .

        IF THIS-OBJECT:BusinessEntityName MATCHES "*BusinessEntity":U THEN
            tbDataProviderName:Text = SUBSTRING (THIS-OBJECT:BusinessEntityName, 1, LENGTH (THIS-OBJECT:BusinessEntityName, "CHARACTER":U) - 14, "CHARACTER":U) + "DataProvider":U .
        ELSE IF THIS-OBJECT:BusinessEntityName MATCHES "*Entity":U THEN
            tbDataProviderName:Text = SUBSTRING (THIS-OBJECT:BusinessEntityName, 1, LENGTH (THIS-OBJECT:BusinessEntityName, "CHARACTER":U) - 6, "CHARACTER":U) + "DataProvider":U.
        ELSE
            tbDataProviderName:Text = THIS-OBJECT:BusinessEntityName + "DataProvider":U.

        tbDataProviderName:Text = ENTRY (NUM-ENTRIES (tbDataProviderName:Text, ".":U), tbDataProviderName:Text, ".":U) .

        tbDataSourceName:Text = SUBSTITUTE ("&1.&2":U, THIS-OBJECT:BusinessEntityName, THIS-OBJECT:EntityTable) .

        ASSIGN cSetting           = oSettingsService:GetSetting (REGISTRY_BASE_KEY, "AppLocation":U)
               tbAppLocation:Text = cSetting .

        ASSIGN cSetting                 = oSettingsService:GetSetting (REGISTRY_BASE_KEY, "KendoUiBuilderUri":U)
               tbKendoUiBuilderUri:Text = IF cSetting > "":U THEN cSetting ELSE "http://localhost:3000/#/":U .

        cbViewType:ValueList:ValueListItems:Add ("data-grid":U) .
        cbViewType:ValueList:ValueListItems:Add ("data-grid-form":U) .
        cbViewType:ValueList:ValueListItems:Add ("data-grid-separate-form":U) .

        cbViewType:Text = "data-grid-form":U .

    END METHOD.

    /**
     * Purpose: Event handler for the EditorButtonClick event of the tbAppLocation
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The EditorButtonEventArgs with the data for this event
     */
    METHOD PRIVATE VOID tbAppLocation_EditorButtonClick (sender AS System.Object,
                                                         e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .

        DEFINE VARIABLE oFolderBrowserDialog AS FolderBrowserDialog NO-UNDO .

        oFolderBrowserDialog = NEW FolderBrowserDialog () .
        oFolderBrowserDialog:Description = "Select Kendo UI Builder project root folder.":T .
        oFolderBrowserDialog:RootFolder = System.Environment+SpecialFolder:MyComputer .
        oFolderBrowserDialog:SelectedPath = THIS-OBJECT:tbAppLocation:Text .

        WAIT-FOR oFolderBrowserDialog:ShowDialog() SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        THIS-OBJECT:tbAppLocation:Text = oFolderBrowserDialog:SelectedPath .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the ValueChanged event of the text boxes on Page 3
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID tbAppLocation_ValueChanged (sender AS System.Object,
                                                    e AS System.EventArgs):

        IF cbModule:Text > "":U AND
           tbName:Text > "":U AND
           tbDataProviderName:Text > "":U AND
           tbDataSourceName:Text > "":U AND
           tbAppLocation:Text > "":U AND
           tbKendoUiBuilderUri:Text > "":U THEN

            ASSIGN btnOk:Enabled = TRUE .
        ELSE
            ASSIGN btnOk:Enabled = FALSE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Raises the Shown event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown (e) .

        THIS-OBJECT:InitializeWizard () .

    END METHOD.

    /**
     * Purpose: Destructor for the ViewGeneratorWizardForm class
     * Notes:
     */
    DESTRUCTOR PUBLIC ViewGeneratorWizardForm ():

    END DESTRUCTOR .

END CLASS.
