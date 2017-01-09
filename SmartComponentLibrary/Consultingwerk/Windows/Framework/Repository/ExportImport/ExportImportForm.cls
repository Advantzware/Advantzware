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
    File        : ExportImportForm
    Purpose     : Export and Import Repository Objects
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jan 06 12:38:53 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*                                FROM PROPATH .
USING Consultingwerk.Framework.*                                 FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                     FROM PROPATH .
USING Consultingwerk.OERA.*                                      FROM PROPATH .
USING Consultingwerk.SmartFramework.*                            FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Tools.*           FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*                     FROM PROPATH .
USING Consultingwerk.Util.*                                      FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                      FROM PROPATH .
USING Consultingwerk.Windows.Forms.*                             FROM PROPATH .
USING Consultingwerk.Windows.Framework.Repository.ExportImport.* FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*                        FROM PROPATH .
USING Progress.Lang.*                                            FROM PROPATH .
USING System.Windows.Forms.*                                     FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Framework.Repository.ExportImport.ExportImportForm
    INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE btnFetchObjectsForImport AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnPopulateList AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE cbSetModified AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE folderBrowserDialog1 AS System.Windows.Forms.FolderBrowserDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE multipleItemPickerControl1 AS Consultingwerk.Windows.Controls.MultipleItemPickerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE multipleItemPickerControl2 AS Consultingwerk.Windows.Controls.MultipleItemPickerControl NO-UNDO.
    DEFINE VARIABLE oDragManager AS FullWindowDragManager EXTENT 2 NO-UNDO .

    DEFINE PRIVATE VARIABLE btnExportImport AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnClose AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE cbResetModified AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE objectNameLookup AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE productModuleLookup AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraDateTimeEditor1 AS Infragistics.Win.UltraWinEditors.UltraDateTimeEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl5 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl4 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl3 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage2 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor2 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    {Consultingwerk/Util/TempTables/ttFileNames.i}
    {Consultingwerk/SmartFramework/System/dsModifiedState.i}

    /**
     * Purpose: Constructor for the ExportImportForm class
     * Notes:
     */
    CONSTRUCTOR PUBLIC ExportImportForm ():

        SUPER().

        InitializeComponent().

        oDragManager[1] = NEW FullWindowDragManager (THIS-OBJECT) .
        oDragManager[2] = NEW FullWindowDragManager (ultraTabControl1) .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /**
     * Purpose: Event handler for the Click event of the btnFetchObjectsForImport
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID btnFetchObjectsForImport_Click (sender AS System.Object,
                                                        e AS System.EventArgs):

        DEFINE VARIABLE dtDateTime  AS DATETIME      NO-UNDO .
        DEFINE VARIABLE dtDateTime2 AS DATETIME      NO-UNDO .
        DEFINE VARIABLE cDirectory  AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE oList       AS CharacterList NO-UNDO .
        DEFINE VARIABLE hQuery      AS HANDLE        NO-UNDO .

        IF NOT ultraTextEditor1:Text > "":U THEN
            ultraTextEditor1:Text = ".":U .

        FILE-INFORMATION:FILE-NAME = ultraTextEditor1:Text .

        IF NOT FILE-INFORMATION:FILE-TYPE MATCHES "*D*":U THEN
            UNDO, THROW NEW Exception ("Please select a valid directory."{&TRAN}, 0) .

        ASSIGN cDirectory = FILE-INFORMATION:FULL-PATHNAME .

        THIS-OBJECT:StoreSettings(cDirectory, "ImportFolder":U) .

        FileHelper:GetFileList (cDirectory, "*.smartrepo":U,
                                OUTPUT TABLE ttFileNames) .

        ASSIGN dtDateTime = ultraDateTimeEditor1:Value
               oList      = NEW CharacterList ("":U, CHR (1)) .

        IF dtDateTime <> ? THEN DO:
            fileLoop:
            FOR EACH ttFileNames ON ERROR UNDO, THROW:
                    FILE-INFORMATION:FILE-NAME = ttFileNames.FileName .

                    ASSIGN dtDateTime2 = DATETIME (FILE-INFORMATION:FILE-MOD-DATE, FILE-INFORMATION:FILE-MOD-TIME * 1000) .

                    IF dtDateTime >= dtDateTime2 THEN
                        DELETE ttFileNames .
            END.
        END.

        hQuery = QueryHelper:CreatePreparedQuery(BUFFER ttFileNames:HANDLE, "preselect each ttFileNames":U) .

        DO WHILE NOT hQuery:QUERY-OFF-END:
            ASSIGN ttFileNames.FileName = SUBSTRING (ttFileNames.FileName, LENGTH (cDirectory, "CHARACTER":U) + 2, -1, "CHARACTER":U) .

            hQuery:GET-NEXT () .
        END.

        multipleItemPickerControl1:SetListItems(TEMP-TABLE ttFileNames:HANDLE, oList) .

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage(err).
        END CATCH.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery).
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Event handler for the Click event of the btnPopulateList
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    @VisualDesigner.
    METHOD PRIVATE VOID btnPopulateList_Click (sender AS System.Object,
                                               e AS System.EventArgs):

        DEFINE VARIABLE oRequest AS IFetchDataRequest NO-UNDO .
        DEFINE VARIABLE oList    AS CharacterList     NO-UNDO .

        oRequest = NEW FetchDataRequest ("eSmartModifiedState":U,
                                         "for each eSmartModifiedState where eSmartModifiedState.ModifiedStateTable = 'SmartObjectMaster'":U,
                                         0) .

        FrameworkSettings:ServiceAdapter:RetrieveData("":U,
                                                      "Consultingwerk.SmartFramework.System.ModifiedStateBusinessEntity":U,
                                                      oRequest,
                                                      OUTPUT DATASET dsModifiedState) .

        oList = NEW CharacterList ("":U, CHR (1)) .

        multipleItemPickerControl2:SetListItems (TEMP-TABLE eSmartModifiedState:HANDLE, oList) .

    END METHOD .

    /**
     * Purpose: Exports repository objects using the dialog options
     * Notes:
     */
    METHOD PROTECTED VOID Export():

        DEFINE VARIABLE oExporter        AS RepositoryObjectExporter    NO-UNDO .
        DEFINE VARIABLE oMessageProvider AS IMessageProvider            NO-UNDO .
        DEFINE VARIABLE oList            AS CharacterList               NO-UNDO .
        DEFINE VARIABLE oStatusForm      AS GenericProcessingStatusForm NO-UNDO .
        DEFINE VARIABLE i                AS INTEGER                     NO-UNDO .

        THIS-OBJECT:StoreSettings (STRING (cbResetModified:CHECKED, "yes/no":U), "ResetModified":U) .
        THIS-OBJECT:StoreSettings (ultraTextEditor2:Text, "ExportFolder":U) .

        oExporter = NEW RepositoryObjectExporter ().

        oMessageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IMessageProvider
                                                         "NEW MessageProvider()"} .

        CASE ultraTabControl2:SelectedTab:Key:
            WHEN "modified":U THEN DO ON ERROR UNDO, THROW:
                oList = multipleItemPickerControl2:GetSelectedItems() .

                IF oList:Count = 0 THEN
                    UNDO, THROW NEW Exception (oMessageProvider:GetMessageText ("SFR":U, 406), 406) .

                oStatusForm = NEW GenericProcessingStatusForm() .
                oStatusForm:Owner = THIS-OBJECT .
                oStatusForm:Show ().

                {Consultingwerk/foreachPrimitiveList.i Character cItem in oList}

                    i = i + 1 .
                    oStatusForm:StatusText = SUBSTITUTE ("Exporting &1"{&TRAN}, i) .

                    oExporter:DumpRepositoryObject (cItem,
                                                    ultraTextEditor2:Text,
                                                    cbResetModified:Checked) .
                END.

                FINALLY:
                    IF VALID-OBJECT (oStatusForm) THEN
                        oStatusForm:Close () .
                END FINALLY.
            END.
            WHEN "module":U THEN DO:
                IF productModuleLookup:LookupKeyValue > "":U THEN DO:

                    oExporter:DumpRepositoryObjectsByModule (ultraTextEditor2:Text,
                                                             productModuleLookup:LookupKeyValue,
                                                             cbResetModified:Checked) .
                END.
                ELSE
                    UNDO, THROW NEW Exception (oMessageProvider:GetMessageText ("SFR":U, 4, TRIM (ultraLabel5:Text, ":":U)), 4) .

                MessageFormHelper:ShowMessage(oMessageProvider:GetMessage("SFR":U, 405)) .
            END.
            WHEN "object":U THEN DO:
                IF objectNameLookup:LookupKeyValue > "":U THEN DO:

                    oExporter:DumpRepositoryObject (objectNameLookup:LookupKeyValue,
                                                    ultraTextEditor2:Text,
                                                    cbResetModified:Checked) .
                END.
                ELSE
                    UNDO, THROW NEW Exception (oMessageProvider:GetMessageText ("SFR":U, 4, TRIM (ultraLabel4:Text, ":":U)), 4) .

                MessageFormHelper:ShowMessage(oMessageProvider:GetMessage("SFR":U, 405)) .
            END.
            OTHERWISE
                UNDO, THROW NEW Exception ("Invalid selected tab."{&TRAN}, 0) .
        END.

    END METHOD.

    /**
     * Purpose: Imports repository objects using the dialog options
     * Notes:
     */
    METHOD PROTECTED VOID Import():

        DEFINE VARIABLE oConfigurationValue AS ConfigurationValueDatasetModel NO-UNDO .
        DEFINE VARIABLE oList               AS CharacterList                  NO-UNDO .
        DEFINE VARIABLE oImporter           AS RepositoryObjectImporter       NO-UNDO .
        DEFINE VARIABLE oStatusForm         AS GenericProcessingStatusForm    NO-UNDO .
        DEFINE VARIABLE cDirectory          AS CHARACTER                      NO-UNDO .

        THIS-OBJECT:StoreSettings (STRING (cbSetModified:CHECKED, "yes/no":U), "SetModified":U) .

        oList = multipleItemPickerControl1:GetSelectedItems() .

        IF oList:Count = 0 THEN
            UNDO, THROW NEW Exception ("Please select files to import first."{&TRAN}, 0) .

        ASSIGN cDirectory = ultraTextEditor1:Text .

        oImporter = NEW RepositoryObjectImporter () .

        oStatusForm = NEW GenericProcessingStatusForm() .
        oStatusForm:Owner = THIS-OBJECT .
        oStatusForm:Show ().

        {Consultingwerk/foreachPrimitiveList.i Character cFileName in oList}

            oStatusForm:StatusText = SUBSTITUTE ("Importing &1"{&TRAN}, cFileName) .

            oImporter:LoadRepositoryObject(SUBSTITUTE ("&1/&2":U,
                                                       cDirectory,
                                                       cFileName), cbSetModified:Checked) .
        END.

        /* Display updated configuration value */
        oConfigurationValue = ConfigurationValueDatasetModel:ByName ("NewestRepositoryObjectImport":U) .

        FINALLY:
            oStatusForm:Close () .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Raises the Load event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        DEFINE VARIABLE oConfigurationValue AS ConfigurationValueDatasetModel NO-UNDO .
        DEFINE VARIABLE cSetModified        AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE cResetModified      AS CHARACTER                      NO-UNDO .

        SUPER:OnLoad (e) .

        ASSIGN cSetModified          = THIS-OBJECT:LoadSettings ("SetModified":U)
               cResetModified        = THIS-OBJECT:LoadSettings ("ResetModified":U)
               ultraTextEditor1:Text = THIS-OBJECT:LoadSettings ("ImportFolder":U)
               ultraTextEditor2:Text = THIS-OBJECT:LoadSettings ("ExportFolder":U) .

        IF DataTypeHelper:ToLogical(cSetModified) <> ? THEN
            cbSetModified:Checked = DataTypeHelper:ToLogical(cSetModified) .

        IF DataTypeHelper:ToLogical(cResetModified) <> ? THEN
            cbResetModified:Checked = DataTypeHelper:ToLogical(cResetModified) .

        oConfigurationValue = ConfigurationValueDatasetModel:ByName ("NewestRepositoryObjectImport":U) .

        ultraDateTimeEditor1:Value = BOX (oConfigurationValue:SmartConfigurationValue:ConfigurationValueDateTimeWhenAvailable(?)) .

    END METHOD.

    /**
     * Purpose: Event handler for the Click event of the btnClose
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID btnClose_Click (sender AS System.Object,
                                        e AS System.EventArgs):

        THIS-OBJECT:Close ().

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err).
        END CATCH.

    END METHOD .

    /**
     * Purpose: Event handler for the Click event of the btnExportImport
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID btnExportImport_Click (sender AS System.Object,
                                               e AS System.EventArgs):

        CASE ultraTabControl1:SelectedTab:Key:

            WHEN "export":U THEN
                THIS-OBJECT:Export () .

            WHEN "import":U THEN
                THIS-OBJECT:Import () .

            OTHERWISE UNDO, THROW NEW Exception ("Invalid selected tab."{&TRAN}, 0) .

        END CASE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err).
        END CATCH.

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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.ExportImport.ExportImportForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab4 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab4 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab5 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab5 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton3 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton3 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton4 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton4 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton5 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton5 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:ultraTabPageControl3 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:multipleItemPickerControl2 = NEW Consultingwerk.Windows.Controls.MultipleItemPickerControl().
        THIS-OBJECT:btnPopulateList = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraTabPageControl4 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:productModuleLookup = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTabPageControl5 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:objectNameLookup = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraTabControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:cbResetModified = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ultraTextEditor2 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:cbSetModified = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:btnFetchObjectsForImport = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:multipleItemPickerControl1 = NEW Consultingwerk.Windows.Controls.MultipleItemPickerControl().
        THIS-OBJECT:ultraDateTimeEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraDateTimeEditor().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:btnClose = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnExportImport = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:folderBrowserDialog1 = NEW System.Windows.Forms.FolderBrowserDialog().
        THIS-OBJECT:ultraTabPageControl3:SuspendLayout().
        THIS-OBJECT:ultraTabPageControl4:SuspendLayout().
        CAST(THIS-OBJECT:productModuleLookup, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl5:SuspendLayout().
        CAST(THIS-OBJECT:objectNameLookup, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        CAST(THIS-OBJECT:ultraTabControl2, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl2:SuspendLayout().
        CAST(THIS-OBJECT:cbResetModified, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl2:SuspendLayout().
        CAST(THIS-OBJECT:cbSetModified, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraDateTimeEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl3 */
        /*  */
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:multipleItemPickerControl2).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:btnPopulateList).
        resources:ApplyResources(THIS-OBJECT:ultraTabPageControl3, "ultraTabPageControl3":U).
        THIS-OBJECT:ultraTabPageControl3:Name = "ultraTabPageControl3":U.
        /*  */
        /* multipleItemPickerControl2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:multipleItemPickerControl2, "multipleItemPickerControl2":U).
        THIS-OBJECT:multipleItemPickerControl2:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:multipleItemPickerControl2:Name = "multipleItemPickerControl2":U.
        /*  */
        /* btnPopulateList */
        /*  */
        resources:ApplyResources(THIS-OBJECT:btnPopulateList, "btnPopulateList":U).
        THIS-OBJECT:btnPopulateList:Name = "btnPopulateList":U.
        THIS-OBJECT:btnPopulateList:Click:Subscribe(THIS-OBJECT:btnPopulateList_Click).
        /*  */
        /* ultraTabPageControl4 */
        /*  */
        THIS-OBJECT:ultraTabPageControl4:Controls:Add(THIS-OBJECT:productModuleLookup).
        THIS-OBJECT:ultraTabPageControl4:Controls:Add(THIS-OBJECT:ultraLabel5).
        resources:ApplyResources(THIS-OBJECT:ultraTabPageControl4, "ultraTabPageControl4":U).
        THIS-OBJECT:ultraTabPageControl4:Name = "ultraTabPageControl4":U.
        /*  */
        /* productModuleLookup */
        /*  */
        resources:ApplyResources(THIS-OBJECT:productModuleLookup, "productModuleLookup":U).
        editorButton1:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:productModuleLookup:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:productModuleLookup:LookupBrowserFields = "ModuleName,ProductCode,ModuleDescription":U.
        THIS-OBJECT:productModuleLookup:LookupDialogFilterFields = "ModuleName,ModuleDescription":U.
        THIS-OBJECT:productModuleLookup:LookupDialogFilterOperators = "BEGINSMATCHES,BEGINSMATCHES":U.
        THIS-OBJECT:productModuleLookup:LookupDialogQuerySort = "BY eSmartModule.ModuleName":U.
        THIS-OBJECT:productModuleLookup:LookupEntityName = "Consultingwerk.SmartFramework.Menu.ModuleBusinessEntity":U.
        THIS-OBJECT:productModuleLookup:LookupEntityTable = "eSmartModule":U.
        THIS-OBJECT:productModuleLookup:LookupKeyField = "ModuleName":U.
        THIS-OBJECT:productModuleLookup:LookupKeyValueColumn = "ModuleName":U.
        THIS-OBJECT:productModuleLookup:LookupQueryString = "FOR EACH eSmartModule WHERE eSmartModule.ModuleName BEGINS ~"&1~"":U.
        THIS-OBJECT:productModuleLookup:Name = "productModuleLookup":U.
        THIS-OBJECT:productModuleLookup:OwningGrid = ?.
        /*  */
        /* ultraLabel5 */
        /*  */
        appearance1:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraLabel5:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:ultraLabel5, "ultraLabel5":U).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        /*  */
        /* ultraTabPageControl5 */
        /*  */
        THIS-OBJECT:ultraTabPageControl5:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:ultraTabPageControl5:Controls:Add(THIS-OBJECT:objectNameLookup).
        resources:ApplyResources(THIS-OBJECT:ultraTabPageControl5, "ultraTabPageControl5":U).
        THIS-OBJECT:ultraTabPageControl5:Name = "ultraTabPageControl5":U.
        /*  */
        /* ultraLabel4 */
        /*  */
        appearance2:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraLabel4:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:ultraLabel4, "ultraLabel4":U).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        /*  */
        /* objectNameLookup */
        /*  */
        resources:ApplyResources(THIS-OBJECT:objectNameLookup, "objectNameLookup":U).
        editorButton2:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton2, "editorButton2":U).
        THIS-OBJECT:objectNameLookup:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:objectNameLookup:LookupBrowserFields = "ObjectName,ModuleName,ObjectDescription":U.
        THIS-OBJECT:objectNameLookup:LookupDialogFilterFields = "ObjectName,ModuleName,ObjectDescription":U.
        THIS-OBJECT:objectNameLookup:LookupDialogFilterOperators = "BEGINSMATCHES,BEGINSMATCHES,BEGINSMATCHES":U.
        THIS-OBJECT:objectNameLookup:LookupDialogQuerySort = "BY eSmartObjectMaster.ObjectName":U.
        THIS-OBJECT:objectNameLookup:LookupEntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:objectNameLookup:LookupEntityTable = "eSmartObjectMaster":U.
        THIS-OBJECT:objectNameLookup:LookupKeyField = "ObjectName":U.
        THIS-OBJECT:objectNameLookup:LookupKeyValueColumn = "ObjectMasterGuid":U.
        THIS-OBJECT:objectNameLookup:LookupQueryString = "FOR EACH eSmartObjectMaster WHERE eSmartObjectMaster.ObjectName BEGINS ~"&1~"":U.
        THIS-OBJECT:objectNameLookup:Name = "objectNameLookup":U.
        THIS-OBJECT:objectNameLookup:OwningGrid = ?.
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraTabControl2).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:cbResetModified).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraTextEditor2).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraLabel3).
        resources:ApplyResources(THIS-OBJECT:ultraTabPageControl1, "ultraTabPageControl1":U).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        /*  */
        /* ultraTabControl2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTabControl2, "ultraTabControl2":U).
        THIS-OBJECT:ultraTabControl2:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage2).
        THIS-OBJECT:ultraTabControl2:Controls:Add(THIS-OBJECT:ultraTabPageControl3).
        THIS-OBJECT:ultraTabControl2:Controls:Add(THIS-OBJECT:ultraTabPageControl4).
        THIS-OBJECT:ultraTabControl2:Controls:Add(THIS-OBJECT:ultraTabPageControl5).
        THIS-OBJECT:ultraTabControl2:Name = "ultraTabControl2":U.
        THIS-OBJECT:ultraTabControl2:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage2.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        ultraTab3:ActiveAppearance = appearance3.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        ultraTab3:Appearance = appearance4.
        ultraTab3:Key = "modified":U.
        ultraTab3:TabPage = THIS-OBJECT:ultraTabPageControl3.
        resources:ApplyResources(ultraTab3, "ultraTab3":U).
        ultraTab3:ForceApplyResources = "":U.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        ultraTab4:ActiveAppearance = appearance5.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        ultraTab4:Appearance = appearance6.
        ultraTab4:Key = "module":U.
        ultraTab4:TabPage = THIS-OBJECT:ultraTabPageControl4.
        resources:ApplyResources(ultraTab4, "ultraTab4":U).
        ultraTab4:ForceApplyResources = "":U.
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        ultraTab5:ActiveAppearance = appearance7.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image":U), System.Object).
        ultraTab5:Appearance = appearance8.
        ultraTab5:Key = "object":U.
        ultraTab5:TabPage = THIS-OBJECT:ultraTabPageControl5.
        resources:ApplyResources(ultraTab5, "ultraTab5":U).
        ultraTab5:ForceApplyResources = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 3 NO-UNDO.
        arrayvar0[1] = ultraTab3.
        arrayvar0[2] = ultraTab4.
        arrayvar0[3] = ultraTab5.
        THIS-OBJECT:ultraTabControl2:Tabs:AddRange(arrayvar0).
        /*  */
        /* ultraTabSharedControlsPage2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTabSharedControlsPage2, "ultraTabSharedControlsPage2":U).
        THIS-OBJECT:ultraTabSharedControlsPage2:Name = "ultraTabSharedControlsPage2":U.
        /*  */
        /* cbResetModified */
        /*  */
        resources:ApplyResources(THIS-OBJECT:cbResetModified, "cbResetModified":U).
        THIS-OBJECT:cbResetModified:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:cbResetModified:BackColorInternal = System.Drawing.Color:Transparent.
        THIS-OBJECT:cbResetModified:Name = "cbResetModified":U.
        /*  */
        /* ultraTextEditor2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTextEditor2, "ultraTextEditor2":U).
        resources:ApplyResources(editorButton3, "editorButton3":U).
        THIS-OBJECT:ultraTextEditor2:ButtonsRight:Add(editorButton3).
        THIS-OBJECT:ultraTextEditor2:Name = "ultraTextEditor2":U.
        THIS-OBJECT:ultraTextEditor2:EditorButtonClick:Subscribe(THIS-OBJECT:ultraTextEditor2_EditorButtonClick).
        /*  */
        /* ultraLabel3 */
        /*  */
        appearance9:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraLabel3:Appearance = appearance9.
        resources:ApplyResources(THIS-OBJECT:ultraLabel3, "ultraLabel3":U).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        /*  */
        /* ultraTabPageControl2 */
        /*  */
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:cbSetModified).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:btnFetchObjectsForImport).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:multipleItemPickerControl1).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:ultraDateTimeEditor1).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:ultraLabel1).
        resources:ApplyResources(THIS-OBJECT:ultraTabPageControl2, "ultraTabPageControl2":U).
        THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2":U.
        /*  */
        /* cbSetModified */
        /*  */
        resources:ApplyResources(THIS-OBJECT:cbSetModified, "cbSetModified":U).
        THIS-OBJECT:cbSetModified:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:cbSetModified:BackColorInternal = System.Drawing.Color:Transparent.
        THIS-OBJECT:cbSetModified:Name = "cbSetModified":U.
        /*  */
        /* btnFetchObjectsForImport */
        /*  */
        resources:ApplyResources(THIS-OBJECT:btnFetchObjectsForImport, "btnFetchObjectsForImport":U).
        THIS-OBJECT:btnFetchObjectsForImport:Name = "btnFetchObjectsForImport":U.
        THIS-OBJECT:btnFetchObjectsForImport:Click:Subscribe(THIS-OBJECT:btnFetchObjectsForImport_Click).
        /*  */
        /* multipleItemPickerControl1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:multipleItemPickerControl1, "multipleItemPickerControl1":U).
        THIS-OBJECT:multipleItemPickerControl1:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:multipleItemPickerControl1:Name = "multipleItemPickerControl1":U.
        /*  */
        /* ultraDateTimeEditor1 */
        /*  */
        appearance10:Image = CAST(resources:GetObject("appearance10.Image":U), System.Object).
        editorButton4:Appearance = appearance10.
        THIS-OBJECT:ultraDateTimeEditor1:ButtonsRight:Add(editorButton4).
        resources:ApplyResources(THIS-OBJECT:ultraDateTimeEditor1, "ultraDateTimeEditor1":U).
        THIS-OBJECT:ultraDateTimeEditor1:MaskInput = "~{date~} ~{longtime~}":U.
        THIS-OBJECT:ultraDateTimeEditor1:Name = "ultraDateTimeEditor1":U.
        THIS-OBJECT:ultraDateTimeEditor1:EditorButtonClick:Subscribe(THIS-OBJECT:ultraDateTimeEditor1_EditorButtonClick).
        /*  */
        /* ultraTextEditor1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTextEditor1, "ultraTextEditor1":U).
        resources:ApplyResources(editorButton5, "editorButton5":U).
        THIS-OBJECT:ultraTextEditor1:ButtonsRight:Add(editorButton5).
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:EditorButtonClick:Subscribe(THIS-OBJECT:ultraTextEditor1_EditorButtonClick).
        /*  */
        /* ultraLabel2 */
        /*  */
        appearance11:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraLabel2:Appearance = appearance11.
        resources:ApplyResources(THIS-OBJECT:ultraLabel2, "ultraLabel2":U).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance12:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance12.
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* ultraTabControl1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTabControl1, "ultraTabControl1":U).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl2).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        ultraTab1:Appearance = appearance13.
        ultraTab1:Key = "export":U.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        resources:ApplyResources(ultraTab1, "ultraTab1":U).
        ultraTab1:ForceApplyResources = "":U.
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        ultraTab2:Appearance = appearance14.
        ultraTab2:Key = "import":U.
        ultraTab2:TabPage = THIS-OBJECT:ultraTabPageControl2.
        resources:ApplyResources(ultraTab2, "ultraTab2":U).
        ultraTab2:ForceApplyResources = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 2 NO-UNDO.
        arrayvar1[1] = ultraTab1.
        arrayvar1[2] = ultraTab2.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar1).
        THIS-OBJECT:ultraTabControl1:SelectedTabChanged:Subscribe(THIS-OBJECT:ultraTabControl1_SelectedTabChanged).
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTabSharedControlsPage1, "ultraTabSharedControlsPage1":U).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        /*  */
        /* btnClose */
        /*  */
        resources:ApplyResources(THIS-OBJECT:btnClose, "btnClose":U).
        THIS-OBJECT:btnClose:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnClose:Name = "btnClose":U.
        THIS-OBJECT:btnClose:Click:Subscribe(THIS-OBJECT:btnClose_Click).
        /*  */
        /* btnExportImport */
        /*  */
        resources:ApplyResources(THIS-OBJECT:btnExportImport, "btnExportImport":U).
        THIS-OBJECT:btnExportImport:Name = "btnExportImport":U.
        THIS-OBJECT:btnExportImport:Click:Subscribe(THIS-OBJECT:btnExportImport_Click).
        /*  */
        /* folderBrowserDialog1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:folderBrowserDialog1, "folderBrowserDialog1":U).
        THIS-OBJECT:folderBrowserDialog1:RootFolder = System.Environment+SpecialFolder:MyComputer.
        /*  */
        /* ExportImportForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:btnExportImport.
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnClose.
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnExportImport).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnClose).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:Name = "ExportImportForm":U.
        THIS-OBJECT:ultraTabPageControl3:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl4:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl4:PerformLayout().
        CAST(THIS-OBJECT:productModuleLookup, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl5:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl5:PerformLayout().
        CAST(THIS-OBJECT:objectNameLookup, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl1:PerformLayout().
        CAST(THIS-OBJECT:ultraTabControl2, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:cbResetModified, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl2:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl2:PerformLayout().
        CAST(THIS-OBJECT:cbSetModified, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraDateTimeEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /**
     * Purpose: Event handle for the EditorButtonClick event of the ultraDateTimeEditor1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The EditorButtonEventArgs with the data for this event
     */
    METHOD PRIVATE VOID ultraDateTimeEditor1_EditorButtonClick (sender AS System.Object,
                                                                e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        ultraDateTimeEditor1:Value = ? .

    END METHOD .

    /**
     * Purpose: Event handler for the SelectedTabChanged event of the ultraTabControl
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The SelectedTabChangedEventArgs with the data for this event
     */
    METHOD PRIVATE VOID ultraTabControl1_SelectedTabChanged (sender AS System.Object,
                                                             e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs):

        btnExportImport:Text = e:Tab:Text .

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage(err).
        END CATCH.

    END METHOD .

    /**
     * Purpose: Event handle for the EditorButtonClick event of the ultraTextEditor1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The EditorButtonEventArgs with the data for this event
     */
    METHOD PRIVATE VOID ultraTextEditor1_EditorButtonClick (sender AS System.Object,
                                                            e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oDialogResult AS DialogResult NO-UNDO .

        IF ultraTextEditor1:Text > "":U THEN
          folderBrowserDialog1:SelectedPath = ultraTextEditor1:Text .

        WAIT-FOR folderBrowserDialog1:ShowDialog() SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        IF folderBrowserDialog1:SelectedPath > "":U THEN
          ultraTextEditor1:Text = folderBrowserDialog1:SelectedPath .

    END METHOD .

    /**
     * Purpose: Event handler for the EditorButtonClick of the ultraTextEditor2
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The EditorButtonEventArgs with the data for this event
     */
    METHOD PRIVATE VOID ultraTextEditor2_EditorButtonClick (sender AS System.Object,
                                                            e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oDialogResult AS DialogResult NO-UNDO .

        IF ultraTextEditor2:Text > "":U THEN
          folderBrowserDialog1:SelectedPath = ultraTextEditor2:Text .

        WAIT-FOR folderBrowserDialog1:ShowDialog() SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        IF folderBrowserDialog1:SelectedPath > "":U THEN
          ultraTextEditor2:Text = folderBrowserDialog1:SelectedPath .

    END METHOD .

    /**
     * Purpose: Destructor for the ExportImportForm class
     * Notes:
     */
    DESTRUCTOR PUBLIC ExportImportForm ():

        GarbageCollectorHelper:DeleteObject(oDragManager[1]) .
        GarbageCollectorHelper:DeleteObject(oDragManager[2]) .

    END DESTRUCTOR .

END CLASS.
