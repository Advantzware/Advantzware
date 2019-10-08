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
    File        : DataSourceWizardForm
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 28 23:49:37 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.* FROM PROPATH.
USING Consultingwerk.Framework.Enum.*                        FROM PROPATH.
USING Consultingwerk.Util.*                                  FROM PROPATH.
USING Consultingwerk.Windows.API.*                           FROM PROPATH .
USING Consultingwerk.Framework.*                             FROM PROPATH .
USING Consultingwerk.Forms.*                                 FROM PROPATH .
USING Progress.Lang.*                                        FROM PROPATH .
USING System.Windows.Forms.*                                 FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.DataSourceWizardForm
    INHERITS BaseForm:

    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE btnGenerate AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnPrev AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnNext AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnTest AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnTestKuibUri AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE cbAuthenticationModel AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE smartUpdatableBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartTempTableAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartPanelController1 AS Consultingwerk.SmartComponents.Implementation.SmartPanelController NO-UNDO.
    DEFINE PRIVATE VARIABLE tbKendoUiBuilderUri AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbAppLocation AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbServiceURI AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbCatalogUriEntityName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbCatalogUriBase AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGroupBox1 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGroupBox3 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGroupBox2 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel6 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl3 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.

    {Consultingwerk/BusinessEntityDesigner/KendoUiBuilder/ttDataSources.i}

    DEFINE VARIABLE REGISTRY_BASE_KEY AS CHARACTER INIT "Kendo UI Builder Generator":U NO-UNDO .

    /**
     * Purpose: Gets and sets the Business Entity Name
     * Notes:
     */
    DEFINE PUBLIC PROPERTY BusinessEntityName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the Business Entity Package
     * Notes:
     */
    DEFINE PUBLIC PROPERTY BusinessEntityPackage AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the Business Entity Package
     * Notes:
     */
    DEFINE PUBLIC PROPERTY BusinessEntityDatasetHandle AS HANDLE NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Constructor for the DataSourceWizardForm class
     * Notes:
     */
    CONSTRUCTOR PUBLIC DataSourceWizardForm ():

        SUPER().
        InitializeComponent().

        THIS-OBJECT:ultraTabControl1:SelectedTab = THIS-OBJECT:ultraTabControl1:Tabs[0] .

    END CONSTRUCTOR.

    /**
     * Purpose: Generates the data provider meta data
     * Notes:
     */
    METHOD PROTECTED VOID Generate ():

        DEFINE VARIABLE oGenerator AS DataProviderGenerator NO-UNDO .
        DEFINE VARIABLE hDataset   AS HANDLE                NO-UNDO .

        ASSIGN hDataset = THIS-OBJECT:BusinessEntityDatasetHandle .

        oGenerator = NEW DataProviderGenerator () .

        IF oGenerator:FileAlreadyExists (tbName:Text,
                                         tbAppLocation:Text) THEN DO:

            IF MessageFormHelper:AskQuestion (SUBSTITUTE ("Overwrite file &1 ?"{&TRAN},
                                                          oGenerator:BuildFileName(tbName:Text,
                                                                                   tbAppLocation:Text))) <> DialogResultEnum:DialogResultYes THEN
                RETURN .
        END.

        oGenerator:GenerateDataProvider (tbName:Text,
                                         tbServiceURI:Text,
                                         tbCatalogUriBase:Text,
                                         tbCatalogUriEntityName:Text,
                                         cbAuthenticationModel:Text,
                                         tbAppLocation:Text,
                                         tbKendoUiBuilderUri:Text,
                                         TABLE ttDataSources,
                                         DATASET-HANDLE hDataset BY-REFERENCE) .

    END METHOD.

    /**
     * Purpose: Stores the settings to the user profile
     * Notes:
     */
    METHOD PROTECTED VOID StoreSettings():

        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        oSettingsService:StoreSetting (REGISTRY_BASE_KEY, "ServiceURI":U,          tbServiceURI:Text) .
        oSettingsService:StoreSetting (REGISTRY_BASE_KEY, "CatalogURI":U,          tbCatalogUriBase:Text) .
        oSettingsService:StoreSetting (REGISTRY_BASE_KEY, "AuthenticationModel":U, cbAuthenticationModel:Text) .
        oSettingsService:StoreSetting (REGISTRY_BASE_KEY, "AppLocation":U,         tbAppLocation:Text) .
        oSettingsService:StoreSetting (REGISTRY_BASE_KEY, "KendoUiBuilderUri":U,   tbKendoUiBuilderUri:Text) .

    END METHOD.

    /**
     * Purpose: Event handler for the Click event of the btnGenerate
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID btnGenerate_Click (sender AS System.Object,
                                           e AS System.EventArgs):

        DEFINE VARIABLE cUri AS CHARACTER NO-UNDO.

        THIS-OBJECT:StoreSettings () .
        THIS-OBJECT:Generate () .

        ASSIGN cUri = SUBSTITUTE ("&1edit-provider/&2":U,
                                  tbKendoUiBuilderUri:Text,
                                  tbName:Text) .

        Win32:ShellExecute ("open":U, cUri) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD .

    /**
     * Purpose: Event handler for the click event of the btnTest
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID btnTest_Click (sender AS System.Object,
                                       e AS System.EventArgs):

        Win32:ShellExecute("open":U, SUBSTITUTE ("&1&2":U, tbCatalogUriBase:Text, tbCatalogUriEntityName:Text)) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the click event of the btnTestKuibUri
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID btnTestKuibUri_Click (sender AS System.Object,
                                              e AS System.EventArgs):

        Win32:ShellExecute("open":U, tbKendoUiBuilderUri:Text) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Initializes the Wizard
     * Notes:
     */
    METHOD PROTECTED VOID InitializeWizard():

        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .
        DEFINE VARIABLE cSetting         AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE hDataset         AS HANDLE           NO-UNDO .
        DEFINE VARIABLE i                AS INTEGER          NO-UNDO .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        IF THIS-OBJECT:BusinessEntityName MATCHES "*BusinessEntity":U THEN
            tbName:Text = SUBSTRING (THIS-OBJECT:BusinessEntityName, 1, LENGTH (THIS-OBJECT:BusinessEntityName, "CHARACTER":U) - 14, "CHARACTER":U) + "DataProvider":U .
        ELSE IF THIS-OBJECT:BusinessEntityName MATCHES "*Entity":U THEN
            tbName:Text = SUBSTRING (THIS-OBJECT:BusinessEntityName, 1, LENGTH (THIS-OBJECT:BusinessEntityName, "CHARACTER":U) - 6, "CHARACTER":U) + "DataProvider":U.
        ELSE
            tbName:Text = THIS-OBJECT:BusinessEntityName + "DataProvider":U.

        ASSIGN cSetting          = oSettingsService:GetSetting (REGISTRY_BASE_KEY, "ServiceURI":U)
               tbServiceURI:Text = IF cSetting > "":U THEN cSetting ELSE "http://localhost:8820/web/":U .

        ASSIGN cSetting              = oSettingsService:GetSetting (REGISTRY_BASE_KEY, "CatalogURI":U)
               tbCatalogUriBase:Text = IF cSetting > "":U THEN cSetting ELSE "http://localhost:8820/web/Catalog/":U .

        tbCatalogUriEntityName:Text = TRIM (SUBSTITUTE ("&1.&2":U, THIS-OBJECT:BusinessEntityPackage, THIS-OBJECT:BusinessEntityName), ".":U) .

        ASSIGN cSetting                   = oSettingsService:GetSetting (REGISTRY_BASE_KEY, "AuthenticationModel":U)
               cbAuthenticationModel:Text = IF cSetting > "":U THEN cSetting ELSE "Form":U .

        ASSIGN cSetting           = oSettingsService:GetSetting (REGISTRY_BASE_KEY, "AppLocation":U)
               tbAppLocation:Text = cSetting .

        ASSIGN cSetting                 = oSettingsService:GetSetting (REGISTRY_BASE_KEY, "KendoUiBuilderUri":U)
               tbKendoUiBuilderUri:Text = IF cSetting > "":U THEN cSetting ELSE "http://localhost:3000/#/":U .


        FrameworkSettings:ServiceAdapter:FetchDataset("":U,
                                                      SUBSTITUTE ("&1.&2":U, THIS-OBJECT:BusinessEntityPackage, THIS-OBJECT:BusinessEntityName),
                                                      OUTPUT DATASET-HANDLE hDataset) .

        DO i = 1 TO hDataset:NUM-BUFFERS:
            CREATE ttDataSources.
            ASSIGN ttDataSources.Sequence       = i
                   ttDataSources.TableName      = hDataset:GET-BUFFER-HANDLE (i):NAME
                   ttDataSources.DataSourceName = SUBSTITUTE ("&1.&2.&3":U,
                                                              THIS-OBJECT:BusinessEntityPackage,
                                                              THIS-OBJECT:BusinessEntityName,
                                                              hDataset:GET-BUFFER-HANDLE (i):NAME) .
        END.

        smartTempTableAdapter1:TempTableHandle = TEMP-TABLE ttDataSources:HANDLE .
        smartTempTableAdapter1:RetrieveData () .

    END METHOD.


    /**
     * Purpose: Raises the Load event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        SUPER:OnLoad (e) .

        THIS-OBJECT:InitializeWizard() .

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

        PROCESS EVENTS .

        tbName:Focus () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Conditionally enables the next button on Page 1
     * Notes:
     */
    METHOD PROTECTED VOID Page1EnableNext():

        ASSIGN btnNext:Enabled = tbName:Text > "" AND
                                 tbServiceURI:Text > "" AND
                                 tbCatalogUriBase:Text > "" AND
                                 tbCatalogUriEntityName:Text > "" .

    END METHOD.

    /**
     * Purpose: Conditionally enables the Generate button on Page 3
     * Notes:
     */
    METHOD PROTECTED VOID Page3EnableGenerate():

        DEFINE VARIABLE lFinish AS LOGICAL NO-UNDO INIT FALSE .

        IF tbAppLocation:Text > "":U AND
           tbKendoUiBuilderUri:Text > "" THEN

            ASSIGN lFinish = TRUE .

        btnGenerate:Enabled = lFinish .

    END METHOD.

    /**
     * Purpose: Event handler for the Click event of the btnNext_Click
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
     METHOD PRIVATE VOID btnNext_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):

        IF ultraTabControl1:SelectedTab:Index < 2 THEN
            ultraTabControl1:SelectedTab = ultraTabControl1:Tabs [ultraTabControl1:SelectedTab:Index + 1] .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the Click event of the btnPrev_Click
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID btnPrev_Click (sender AS System.Object,
                                       e AS System.EventArgs):

        IF ultraTabControl1:SelectedTab:Index > 0 THEN
            ultraTabControl1:SelectedTab = ultraTabControl1:Tabs [ultraTabControl1:SelectedTab:Index - 1] .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem1 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem1 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem2 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem2 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem3 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem3 = NEW Infragistics.Win.ValueListItem().
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
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ttDataSources":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("Sequence":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("DataSourceName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("GenerateDataSource":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ClientSideProcessing":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("CountFunctionName":U).
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
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("ttDataSources":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.DataSourceWizardForm":U).
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraGroupBox1 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:btnTest = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:tbCatalogUriEntityName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:cbAuthenticationModel = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:tbCatalogUriBase = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbServiceURI = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraGroupBox2 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:smartPanelController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartPanelController().
        THIS-OBJECT:smartUpdatableBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartTempTableAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        THIS-OBJECT:ultraTabPageControl3 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraGroupBox3 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:btnTestKuibUri = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:tbKendoUiBuilderUri = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbAppLocation = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel6 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:btnGenerate = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnPrev = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnNext = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox1:SuspendLayout().
        CAST(THIS-OBJECT:tbCatalogUriEntityName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:cbAuthenticationModel, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbCatalogUriBase, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbServiceURI, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbName, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl2:SuspendLayout().
        CAST(THIS-OBJECT:ultraGroupBox2, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox2:SuspendLayout().
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl3:SuspendLayout().
        CAST(THIS-OBJECT:ultraGroupBox3, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox3:SuspendLayout().
        CAST(THIS-OBJECT:tbKendoUiBuilderUri, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbAppLocation, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraGroupBox1).
        THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(589, 298).
        /*  */
        /* ultraGroupBox1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraGroupBox1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:btnTest).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:tbCatalogUriEntityName).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:cbAuthenticationModel).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:tbCatalogUriBase).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:tbServiceURI).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:tbName).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ultraGroupBox1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraGroupBox1:Name = "ultraGroupBox1":U.
        THIS-OBJECT:ultraGroupBox1:Size = NEW System.Drawing.Size(565, 275).
        THIS-OBJECT:ultraGroupBox1:TabIndex = 0.
        THIS-OBJECT:ultraGroupBox1:Text = "Data Provider":U.
        /*  */
        /* btnTest */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:btnTest:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnTest:Location = NEW System.Drawing.Point(520, 88).
        THIS-OBJECT:btnTest:Name = "btnTest":U.
        THIS-OBJECT:btnTest:Size = NEW System.Drawing.Size(39, 23).
        THIS-OBJECT:btnTest:TabIndex = 9.
        THIS-OBJECT:btnTest:Text = "Test":U.
        THIS-OBJECT:btnTest:Click:Subscribe(THIS-OBJECT:btnTest_Click).
        /*  */
        /* tbCatalogUriEntityName */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbCatalogUriEntityName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbCatalogUriEntityName:Location = NEW System.Drawing.Point(364, 88).
        THIS-OBJECT:tbCatalogUriEntityName:Name = "tbCatalogUriEntityName":U.
        THIS-OBJECT:tbCatalogUriEntityName:Size = NEW System.Drawing.Size(150, 21).
        THIS-OBJECT:tbCatalogUriEntityName:TabIndex = 6.
        THIS-OBJECT:tbCatalogUriEntityName:ValueChanged:Subscribe(THIS-OBJECT:tbName_ValueChanged).
        /*  */
        /* cbAuthenticationModel */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbAuthenticationModel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbAuthenticationModel:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        valueListItem1:DataValue = "Anonymous":U.
        valueListItem2:DataValue = "Basic":U.
        valueListItem3:DataValue = "Form":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.ValueListItem EXTENT 3 NO-UNDO.
        arrayvar0[1] = valueListItem1.
        arrayvar0[2] = valueListItem2.
        arrayvar0[3] = valueListItem3.
        THIS-OBJECT:cbAuthenticationModel:Items:AddRange(arrayvar0).
        THIS-OBJECT:cbAuthenticationModel:Location = NEW System.Drawing.Point(118, 117).
        THIS-OBJECT:cbAuthenticationModel:Name = "cbAuthenticationModel":U.
        THIS-OBJECT:cbAuthenticationModel:Size = NEW System.Drawing.Size(441, 21).
        THIS-OBJECT:cbAuthenticationModel:TabIndex = 8.
        /*  */
        /* tbCatalogUriBase */
        /*  */
        THIS-OBJECT:tbCatalogUriBase:Location = NEW System.Drawing.Point(118, 88).
        THIS-OBJECT:tbCatalogUriBase:Name = "tbCatalogUriBase":U.
        THIS-OBJECT:tbCatalogUriBase:Size = NEW System.Drawing.Size(240, 21).
        THIS-OBJECT:tbCatalogUriBase:TabIndex = 5.
        THIS-OBJECT:tbCatalogUriBase:ValueChanged:Subscribe(THIS-OBJECT:tbName_ValueChanged).
        /*  */
        /* tbServiceURI */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbServiceURI:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbServiceURI:Location = NEW System.Drawing.Point(118, 59).
        THIS-OBJECT:tbServiceURI:Name = "tbServiceURI":U.
        THIS-OBJECT:tbServiceURI:Size = NEW System.Drawing.Size(441, 21).
        THIS-OBJECT:tbServiceURI:TabIndex = 3.
        THIS-OBJECT:tbServiceURI:ValueChanged:Subscribe(THIS-OBJECT:tbName_ValueChanged).
        /*  */
        /* tbName */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbName:Location = NEW System.Drawing.Point(118, 30).
        THIS-OBJECT:tbName:Name = "tbName":U.
        THIS-OBJECT:tbName:Size = NEW System.Drawing.Size(441, 21).
        THIS-OBJECT:tbName:TabIndex = 1.
        THIS-OBJECT:tbName:ValueChanged:Subscribe(THIS-OBJECT:tbName_ValueChanged).
        /*  */
        /* ultraLabel4 */
        /*  */
        appearance1:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraLabel4:Appearance = appearance1.
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(6, 121).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(131, 23).
        THIS-OBJECT:ultraLabel4:TabIndex = 7.
        THIS-OBJECT:ultraLabel4:Text = "Authentication Model:":U.
        /*  */
        /* ultraLabel3 */
        /*  */
        appearance2:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraLabel3:Appearance = appearance2.
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(6, 92).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel3:TabIndex = 4.
        THIS-OBJECT:ultraLabel3:Text = "Catalog URI:":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        appearance3:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraLabel2:Appearance = appearance3.
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(6, 63).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 2.
        THIS-OBJECT:ultraLabel2:Text = "Service URI:":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance4:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraLabel1:Appearance = appearance4.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(6, 34).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 0.
        THIS-OBJECT:ultraLabel1:Text = "Name:":U.
        /*  */
        /* ultraTabPageControl2 */
        /*  */
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:ultraGroupBox2).
        THIS-OBJECT:ultraTabPageControl2:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2":U.
        THIS-OBJECT:ultraTabPageControl2:Size = NEW System.Drawing.Size(589, 298).
        /*  */
        /* ultraGroupBox2 */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraGroupBox2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:smartPanelController1).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:smartUpdatableBrowser1).
        THIS-OBJECT:ultraGroupBox2:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraGroupBox2:Name = "ultraGroupBox2":U.
        THIS-OBJECT:ultraGroupBox2:Size = NEW System.Drawing.Size(565, 275).
        THIS-OBJECT:ultraGroupBox2:TabIndex = 6.
        THIS-OBJECT:ultraGroupBox2:Text = "Data Sources":U.
        /*  */
        /* smartPanelController1 */
        /*  */
        THIS-OBJECT:smartPanelController1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:smartPanelController1:Location = NEW System.Drawing.Point(3, 16).
        THIS-OBJECT:smartPanelController1:MaximumSize = NEW System.Drawing.Size(5000, 27).
        THIS-OBJECT:smartPanelController1:MinimumSize = NEW System.Drawing.Size(100, 27).
        THIS-OBJECT:smartPanelController1:Name = "smartPanelController1":U.
        THIS-OBJECT:smartPanelController1:Size = NEW System.Drawing.Size(559, 27).
        THIS-OBJECT:smartPanelController1:TabIndex = 0.
        THIS-OBJECT:smartPanelController1:VisibleTableIOTools = "TableIOUpdate,TableIOSave,TableIOCancel":U.
        /*  */
        /* smartUpdatableBrowser1 */
        /*  */
        THIS-OBJECT:smartUpdatableBrowser1:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartUpdatableBrowser1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartUpdatableBrowser1:DataSource = THIS-OBJECT:bindingSource1.
        THIS-OBJECT:smartUpdatableBrowser1:DeselectRowsOnActivate = TRUE.
        THIS-OBJECT:smartUpdatableBrowser1:DisabledColumnsForAdd = "Sequence":U.
        THIS-OBJECT:smartUpdatableBrowser1:DisabledColumnsForUpdate = "Sequence":U.
        appearance5:BackColor = System.Drawing.SystemColors:Window.
        appearance5:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Appearance = appearance5.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn1:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn1:Header:Caption = "Sequence ":U.
        ultraGridColumn1:Header:VisiblePosition = 0.
        ultraGridColumn1:SortIndicator = Infragistics.Win.UltraWinGrid.SortIndicator:Disabled.
        ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn2:Header:Caption = "Name ":U.
        ultraGridColumn2:Header:VisiblePosition = 1.
        ultraGridColumn2:SortIndicator = Infragistics.Win.UltraWinGrid.SortIndicator:Disabled.
        ultraGridColumn2:Width = 164.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn3:Header:Caption = "Generate ":U.
        ultraGridColumn3:Header:VisiblePosition = 2.
        ultraGridColumn3:SortIndicator = Infragistics.Win.UltraWinGrid.SortIndicator:Disabled.
        ultraGridColumn4:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn4:Header:Caption = "Client-side processing ":U.
        ultraGridColumn4:Header:VisiblePosition = 3.
        ultraGridColumn4:SortIndicator = Infragistics.Win.UltraWinGrid.SortIndicator:Disabled.
        ultraGridColumn5:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn5:Header:Caption = "Count Function ":U.
        ultraGridColumn5:Header:VisiblePosition = 4.
        ultraGridColumn5:SortIndicator = Infragistics.Win.UltraWinGrid.SortIndicator:Disabled.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS System.Object EXTENT 5 NO-UNDO.
        arrayvar1[1] = ultraGridColumn1.
        arrayvar1[2] = ultraGridColumn2.
        arrayvar1[3] = ultraGridColumn3.
        arrayvar1[4] = ultraGridColumn4.
        arrayvar1[5] = ultraGridColumn5.
        ultraGridBand1:Columns:AddRange(arrayvar1).
        ultraGridBand1:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand1:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance6:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance6:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance6:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance6:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:Appearance = appearance6.
        appearance7:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance7.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance8:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance8:BackColor2 = System.Drawing.SystemColors:Control.
        appearance8:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance8:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:PromptAppearance = appearance8.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance9:BackColor = System.Drawing.SystemColors:Window.
        appearance9:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveCellAppearance = appearance9.
        appearance10:BackColor = System.Drawing.SystemColors:Highlight.
        appearance10:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveRowAppearance = appearance10.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowGroupBy = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance11:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CardAreaAppearance = appearance11.
        appearance12:BorderColor = System.Drawing.Color:Silver.
        appearance12:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellAppearance = appearance12.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellPadding = 0.
        appearance13:BackColor = System.Drawing.SystemColors:Control.
        appearance13:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance13:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance13:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance13:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:GroupByRowAppearance = appearance13.
        appearance14:TextHAlignAsString = "Left":U.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderAppearance = appearance14.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance15:BackColor = System.Drawing.SystemColors:Window.
        appearance15:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowAppearance = appearance15.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance16:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance16.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartUpdatableBrowser1:EditInViewerOnDefaultAction = FALSE.
        THIS-OBJECT:smartUpdatableBrowser1:FetchAllRecordsOnGroupBy = FALSE.
        THIS-OBJECT:smartUpdatableBrowser1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:smartUpdatableBrowser1:LinkDataSource = THIS-OBJECT:smartTempTableAdapter1.
        THIS-OBJECT:smartUpdatableBrowser1:LinkTableIOSource = THIS-OBJECT:smartPanelController1.
        THIS-OBJECT:smartUpdatableBrowser1:Location = NEW System.Drawing.Point(3, 49).
        THIS-OBJECT:smartUpdatableBrowser1:Name = "smartUpdatableBrowser1":U.
        THIS-OBJECT:smartUpdatableBrowser1:PromptForSaveValuesOnRowDeactivate = FALSE.
        THIS-OBJECT:smartUpdatableBrowser1:RepositionedRow = 0.
        THIS-OBJECT:smartUpdatableBrowser1:SaveChangesOnLeave = TRUE.
        THIS-OBJECT:smartUpdatableBrowser1:SaveMode = TRUE.
        THIS-OBJECT:smartUpdatableBrowser1:SettingsKey = "8cfd45f1-31ac-2bba-1714-c2f8203f2e0f":U.
        THIS-OBJECT:smartUpdatableBrowser1:Size = NEW System.Drawing.Size(556, 220).
        THIS-OBJECT:smartUpdatableBrowser1:SmartFilterActive = FALSE.
        THIS-OBJECT:smartUpdatableBrowser1:SmartFilterUseMatches = FALSE.
        THIS-OBJECT:smartUpdatableBrowser1:SmartFilterUseOperators = FALSE.
        THIS-OBJECT:smartUpdatableBrowser1:TabIndex = 1.
        THIS-OBJECT:smartUpdatableBrowser1:Text = "smartUpdatableBrowser1":U.
        THIS-OBJECT:smartUpdatableBrowser1:VisibleGridColumns = "":U.
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar2 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar2.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar3[1] = NEW Progress.Data.ColumnPropDesc("Sequence":U, "Sequence":U, Progress.Data.DataType:INTEGER).
        arrayvar3[2] = NEW Progress.Data.ColumnPropDesc("DataSourceName":U, "Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar3[3] = NEW Progress.Data.ColumnPropDesc("GenerateDataSource":U, "Generate":U, Progress.Data.DataType:LOGICAL).
        arrayvar3[4] = NEW Progress.Data.ColumnPropDesc("ClientSideProcessing":U, "Client-side processing":U, Progress.Data.DataType:LOGICAL).
        arrayvar3[5] = NEW Progress.Data.ColumnPropDesc("CountFunctionName":U, "Count Function":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar3.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* smartTempTableAdapter1 */
        /*  */
        THIS-OBJECT:smartTempTableAdapter1:AppServerPartition = "":U.
        THIS-OBJECT:smartTempTableAdapter1:BindingSourceExcludeFields = "":U.
        THIS-OBJECT:smartTempTableAdapter1:BindingSourceIncludeFields = "*":U.
        THIS-OBJECT:smartTempTableAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartTempTableAdapter1:LinkNavigationSource = ?.
        THIS-OBJECT:smartTempTableAdapter1:QuerySort = "BY ttDataSources.Sequence":U.
        THIS-OBJECT:smartTempTableAdapter1:SmartDataSourceActive = TRUE.
        /*  */
        /* ultraTabPageControl3 */
        /*  */
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:ultraGroupBox3).
        THIS-OBJECT:ultraTabPageControl3:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabPageControl3:Name = "ultraTabPageControl3":U.
        THIS-OBJECT:ultraTabPageControl3:Size = NEW System.Drawing.Size(589, 298).
        /*  */
        /* ultraGroupBox3 */
        /*  */
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar8 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraGroupBox3:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:btnTestKuibUri).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:tbKendoUiBuilderUri).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:tbAppLocation).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:ultraLabel6).
        THIS-OBJECT:ultraGroupBox3:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraGroupBox3:Name = "ultraGroupBox3":U.
        THIS-OBJECT:ultraGroupBox3:Size = NEW System.Drawing.Size(565, 275).
        THIS-OBJECT:ultraGroupBox3:TabIndex = 6.
        THIS-OBJECT:ultraGroupBox3:Text = "Generate":U.
        /*  */
        /* btnTestKuibUri */
        /*  */
        DEFINE VARIABLE nestedvar9 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar9 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:btnTestKuibUri:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar9, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnTestKuibUri:Location = NEW System.Drawing.Point(520, 57).
        THIS-OBJECT:btnTestKuibUri:Name = "btnTestKuibUri":U.
        THIS-OBJECT:btnTestKuibUri:Size = NEW System.Drawing.Size(39, 23).
        THIS-OBJECT:btnTestKuibUri:TabIndex = 10.
        THIS-OBJECT:btnTestKuibUri:Text = "Test":U.
        THIS-OBJECT:btnTestKuibUri:Click:Subscribe(THIS-OBJECT:btnTestKuibUri_Click).
        /*  */
        /* tbKendoUiBuilderUri */
        /*  */
        DEFINE VARIABLE nestedvar10 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar10 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbKendoUiBuilderUri:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar10, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbKendoUiBuilderUri:Location = NEW System.Drawing.Point(133, 59).
        THIS-OBJECT:tbKendoUiBuilderUri:Name = "tbKendoUiBuilderUri":U.
        THIS-OBJECT:tbKendoUiBuilderUri:Size = NEW System.Drawing.Size(381, 21).
        THIS-OBJECT:tbKendoUiBuilderUri:TabIndex = 7.
        THIS-OBJECT:tbKendoUiBuilderUri:ValueChanged:Subscribe(THIS-OBJECT:tbAppLocation_ValueChanged).
        /*  */
        /* tbAppLocation */
        /*  */
        DEFINE VARIABLE nestedvar11 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar11 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbAppLocation:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar11, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:tbAppLocation:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tbAppLocation:Location = NEW System.Drawing.Point(133, 30).
        THIS-OBJECT:tbAppLocation:Name = "tbAppLocation":U.
        THIS-OBJECT:tbAppLocation:Size = NEW System.Drawing.Size(426, 21).
        THIS-OBJECT:tbAppLocation:TabIndex = 5.
        THIS-OBJECT:tbAppLocation:ValueChanged:Subscribe(THIS-OBJECT:tbAppLocation_ValueChanged).
        THIS-OBJECT:tbAppLocation:EditorButtonClick:Subscribe(THIS-OBJECT:tbAppLocation_EditorButtonClick).
        /*  */
        /* ultraLabel5 */
        /*  */
        appearance17:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraLabel5:Appearance = appearance17.
        THIS-OBJECT:ultraLabel5:Location = NEW System.Drawing.Point(6, 63).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        THIS-OBJECT:ultraLabel5:Size = NEW System.Drawing.Size(121, 23).
        THIS-OBJECT:ultraLabel5:TabIndex = 6.
        THIS-OBJECT:ultraLabel5:Text = "Kendo UI Builder URI:":U.
        /*  */
        /* ultraLabel6 */
        /*  */
        appearance18:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraLabel6:Appearance = appearance18.
        THIS-OBJECT:ultraLabel6:Location = NEW System.Drawing.Point(6, 34).
        THIS-OBJECT:ultraLabel6:Name = "ultraLabel6":U.
        THIS-OBJECT:ultraLabel6:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel6:TabIndex = 4.
        THIS-OBJECT:ultraLabel6:Text = "App location:":U.
        /*  */
        /* ultraTabControl1 */
        /*  */
        DEFINE VARIABLE nestedvar12 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar12 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar12, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl2).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl3).
        THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(1, 1).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(589, 298).
        THIS-OBJECT:ultraTabControl1:Style = Infragistics.Win.UltraWinTabControl.UltraTabControlStyle:Wizard.
        THIS-OBJECT:ultraTabControl1:TabIndex = 0.
        THIS-OBJECT:ultraTabControl1:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomRight.
        appearance19:BackColor = System.Drawing.Color:White.
        ultraTab1:Appearance = appearance19.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        ultraTab1:Text = "tab1":U.
        appearance20:BackColor = System.Drawing.Color:White.
        ultraTab2:Appearance = appearance20.
        ultraTab2:TabPage = THIS-OBJECT:ultraTabPageControl2.
        ultraTab2:Text = "tab2":U.
        appearance21:BackColor = System.Drawing.Color:White.
        ultraTab3:Appearance = appearance21.
        ultraTab3:TabPage = THIS-OBJECT:ultraTabPageControl3.
        ultraTab3:Text = "tab3":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 3 NO-UNDO.
        arrayvar4[1] = ultraTab1.
        arrayvar4[2] = ultraTab2.
        arrayvar4[3] = ultraTab3.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar4).
        THIS-OBJECT:ultraTabControl1:SelectedTabChanged:Subscribe(THIS-OBJECT:ultraTabControl1_SelectedTabChanged).
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(589, 298).
        /*  */
        /* btnGenerate */
        /*  */
        DEFINE VARIABLE nestedvar13 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar13 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnGenerate:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar13, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnGenerate:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:btnGenerate:Enabled = FALSE.
        THIS-OBJECT:btnGenerate:Location = NEW System.Drawing.Point(174, 305).
        THIS-OBJECT:btnGenerate:Name = "btnGenerate":U.
        THIS-OBJECT:btnGenerate:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnGenerate:TabIndex = 3.
        THIS-OBJECT:btnGenerate:Text = "&Generate":U.
        THIS-OBJECT:btnGenerate:Click:Subscribe(THIS-OBJECT:btnGenerate_Click).
        /*  */
        /* btnCancel */
        /*  */
        DEFINE VARIABLE nestedvar14 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar14 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar14, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(502, 305).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCancel:TabIndex = 4.
        THIS-OBJECT:btnCancel:Text = "&Cancel":U.
        /*  */
        /* btnPrev */
        /*  */
        DEFINE VARIABLE nestedvar15 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar15 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnPrev:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar15, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnPrev:Enabled = FALSE.
        THIS-OBJECT:btnPrev:Location = NEW System.Drawing.Point(12, 305).
        THIS-OBJECT:btnPrev:Name = "btnPrev":U.
        THIS-OBJECT:btnPrev:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnPrev:TabIndex = 1.
        THIS-OBJECT:btnPrev:Text = "< Back":U.
        THIS-OBJECT:btnPrev:Click:Subscribe(THIS-OBJECT:btnPrev_Click).
        /*  */
        /* btnNext */
        /*  */
        DEFINE VARIABLE nestedvar16 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar16 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnNext:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar16, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnNext:Location = NEW System.Drawing.Point(93, 305).
        THIS-OBJECT:btnNext:Name = "btnNext":U.
        THIS-OBJECT:btnNext:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnNext:TabIndex = 2.
        THIS-OBJECT:btnNext:Text = "Next >":U.
        THIS-OBJECT:btnNext:Click:Subscribe(THIS-OBJECT:btnNext_Click).
        /*  */
        /* DataSourceWizardForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(589, 340).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnNext).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnPrev).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnGenerate).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "DataSourceWizardForm":U.
        THIS-OBJECT:Text = "Kendo UI Builder Data Source Wizard":U.
        THIS-OBJECT:WindowPositionRegistryKey = "DataSourceWizardForm":U.
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraGroupBox1:PerformLayout().
        CAST(THIS-OBJECT:tbCatalogUriEntityName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:cbAuthenticationModel, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbCatalogUriBase, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbServiceURI, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbName, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraGroupBox2, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl3:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraGroupBox3, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox3:ResumeLayout(FALSE).
        THIS-OBJECT:ultraGroupBox3:PerformLayout().
        CAST(THIS-OBJECT:tbKendoUiBuilderUri, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbAppLocation, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /**
     * Purpose: Initializes the current Page
     * Notes:
     */
    METHOD PROTECTED VOID InitializePage ():

        DEFINE VARIABLE iPage   AS INTEGER NO-UNDO .

        ASSIGN iPage = THIS-OBJECT:ultraTabControl1:SelectedTab:Index + 1
               btnPrev:Enabled = (iPage <> 1)
               btnNext:Enabled = (iPage <> 3) .

        IF iPage = 1 THEN DO:
            tbName:Focus () .

            THIS-OBJECT:Page1EnableNext () .
        END.

        IF iPage = 2 THEN DO:
            smartUpdatableBrowser1:Focus () .

            btnNext:Enabled = TRUE .
        END.

        IF iPage = 3 THEN DO:
            tbAppLocation:Focus () .

            btnNext:Enabled = FALSE .

            THIS-OBJECT:Page3EnableGenerate () .
        END.
        ELSE
            btnGenerate:Enabled = FALSE .

    END METHOD .

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

        IF ultraTabControl1:SelectedTab:Index = 2 THEN
            THIS-OBJECT:Page3EnableGenerate() .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the ValueChanged event of the text boxes on Page 1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID tbName_ValueChanged (sender AS System.Object,
                                             e AS System.EventArgs):

        THIS-OBJECT:Page1EnableNext() .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the SelectedTabChanged event of the ultraTabControl1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The SelectedTabChangedEventArgs with the data for this event
     */
    METHOD PRIVATE VOID ultraTabControl1_SelectedTabChanged (sender AS System.Object,
                                                             e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs):

        THIS-OBJECT:InitializePage() .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Destructor for the DataSourceWizardForm class
     * Notes:
     */
    DESTRUCTOR PUBLIC DataSourceWizardForm ():

    END DESTRUCTOR.

END CLASS.
