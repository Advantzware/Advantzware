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
    File        : ConfigurationSettingsEditorForm
    Purpose     : Maintanence Tools for Configuration Settings
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Wed Nov 30 16:42:15 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/
USING Consultingwerk.Util.Forms.*                           FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*                   FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                       FROM PROPATH .
USING Consultingwerk.Forms.*                                FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*       FROM PROPATH .
USING Consultingwerk.Studio.ConfigurationSettingsEditor.*   FROM PROPATH .
USING Consultingwerk.Util.*                                 FROM PROPATH .
USING Consultingwerk.Windows.Controls.*                     FROM PROPATH .
USING Progress.Lang.*                                       FROM PROPATH .

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Studio.ConfigurationSettingsEditor.ConfigurationSettingsEditorForm
    INHERITS BaseForm:

    {Consultingwerk/Studio/ConfigurationSettingsEditor/ttConfigurationSettings.i}

    DEFINE PRIVATE VARIABLE m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE ConfigurationSettingsEditorForm_Fill_Panel AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE smartTempTableAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE saveFileDialog1 AS System.Windows.Forms.SaveFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE smartUpdatableBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.

    DEFINE VARIABLE cBaseTitle       AS CHARACTER                NO-UNDO INITIAL "Configuration File Editor":U.
    DEFINE VARIABLE cCurrentFileName AS CHARACTER                NO-UNDO.
    DEFINE VARIABLE oLruController   AS UltraRibbonLruController NO-UNDO.
    DEFINE VARIABLE lUnsavedChanges  AS LOGICAL                  NO-UNDO.

    /**
     * Purpose: Constructor for the DesktopForm class
     * Notes:   Starts the form with the FileOpenDialog
     */
    CONSTRUCTOR PUBLIC ConfigurationSettingsEditorForm (  ):

        THIS-OBJECT(?).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the DesktopForm class
     * Notes:   Starts the form and opens a file
     * @param pcFileName The file name to open
     */
    CONSTRUCTOR PUBLIC ConfigurationSettingsEditorForm (pcFileName AS CHARACTER):

        SUPER().

        InitializeComponent().

        THIS-OBJECT:InitConfigurationParamValueListColumn ().

        ASSIGN
            cCurrentFileName = pcFileName
            oLruController   = NEW Consultingwerk.Windows.Controls.UltraRibbonLruController (THIS-OBJECT:smartToolbarController1,
                                                                                             "LRU":U,
                                                                                             "&1":U,
                                                                                             10)
            .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /**
     * Purpose: Prepares the TempTable Adapter to retreive data
     * Notes:
     */
    METHOD PRIVATE VOID PrepareTempTableAdapter ():

        ASSIGN
            smartTempTableAdapter1:TempTableHandle = TEMP-TABLE ttConfiguration:HANDLE
            .

    END METHOD .

    /**
     * Purpose: Event handler for various records manipulation events of the smartTempTableAdapter1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The ToolClickEventArgs with the data for this event
     */
    METHOD PRIVATE VOID RecordsChangedHandler (sender AS System.Object,
                                               e      AS System.EventArgs ):

        ASSIGN lUnsavedChanges = TRUE.

    END METHOD.


    /**
     * Purpose: Refreshs the WindowTitle, Retrievs the configuration data and refreshs the Grid
     * Notes:
     * @param pcFileName The file name used to show in the Window Title
     */
    METHOD PRIVATE VOID RefreshUI (pcFileName AS CHARACTER):

        ASSIGN
            cCurrentFileName = pcFileName.

        THIS-OBJECT:Text = THIS-OBJECT:WindowTitle (pcFileName).

        smartTempTableAdapter1:RetrieveData ().
        smartBindingSource1:RefreshAll ().

    END METHOD.

    /**
     * Purpose: Saves the current file in the current format
     * Notes:   If current file is a new file then the SaveFileDialog is shown
     */
    METHOD PRIVATE VOID SaveFile ():

        IF cCurrentFileName = ? THEN
            THIS-OBJECT:SaveFileAs ().
        ELSE
            THIS-OBJECT:SaveFile (cCurrentFileName).

    END METHOD.

    /**
     * Purpose: Saves a file in Json or XML format dependent on it's extension
     * Notes:
     * @param pcFileName The file to save
     */
    METHOD PRIVATE VOID SaveFile (pcFileName AS CHARACTER):

        DEFINE VARIABLE oConfigurationFileAccess AS IConfigurationFileAccess NO-UNDO.

        CASE FileHelper:FileExtension (pcFileName):

            WHEN "applicationsettings":U OR
            WHEN "restapplicationsettings":U THEN
                oConfigurationFileAccess = NEW JsonConfigurationFileAccess ().

            WHEN "xml":U THEN
                oConfigurationFileAccess = NEW XMLConfigurationFileAccess ().

            OTHERWISE DO:

                UNDO, THROW NEW Progress.Lang.AppError (SUBSTITUTE ("Extension '&1' is not supported":U,
                                                                    FileHelper:FileExtension (pcFileName)), 0).
            END.

        END CASE.

        IF VALID-OBJECT (oConfigurationFileAccess) THEN DO:

            oConfigurationFileAccess:WriteFile (pcFileName,
                                                TABLE ttConfiguration).
            cCurrentFileName = pcFileName.

            RefreshUI (cCurrentFileName).

        END.

        ASSIGN lUnsavedChanges = FALSE.

        CATCH ple AS Progress.Lang.Error:

            ErrorHelper:ShowErrorMessage (ple).

        END CATCH.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oConfigurationFileAccess).
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Shows the SaveFileDialog to let the user select a file and saves the
     *          configuration data in that file.
     * Notes:
     */
    METHOD PRIVATE VOID SaveFileAs ():

        DEFINE VARIABLE oResult AS System.Windows.Forms.DialogResult NO-UNDO.

        IF cCurrentFileName > "":U THEN
            ASSIGN THIS-OBJECT:saveFileDialog1:FileName = cCurrentFileName.

        WAIT-FOR saveFileDialog1:ShowDialog () SET oResult .

        IF NOT Progress.Util.EnumHelper:AreEqual (System.Windows.Forms.DialogResult:OK,
                                                  oResult) THEN
            RETURN .

        THIS-OBJECT:SaveFile (saveFileDialog1:FileName).

    END METHOD.


    /**
     * Purpose: Initializes the ConfigurationName column ValueItems with the known PropertyNames
     * Notes:
     */
    METHOD PUBLIC VOID InitConfigurationParamValueListColumn():

        DEFINE VARIABLE oConfigurationPropertyNames AS IConfigurationProviderPropertyNames NO-UNDO .

        oConfigurationPropertyNames = {Consultingwerk/get-service.i Consultingwerk.Studio.ConfigurationSettingsEditor.IConfigurationProviderPropertyNames "NEW Consultingwerk.Studio.ConfigurationSettingsEditor.ConfigurationProviderPropertyNames()"} .

        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Bands[0]:Columns["ConfigurationName":U]:ValueList = oConfigurationPropertyNames:GetPropertyNames ().

    END METHOD.

    /**
     * Purpose: Prompts for SaveChanges prompt if required
     * Notes:
     * @return Logical value indicating if the user has chosen to save changes
     */
    METHOD PRIVATE LOGICAL CheckChanges():

        DEFINE VARIABLE oDialogResult AS DialogResultEnum NO-UNDO .

        IF lUnsavedChanges THEN DO:

            oDialogResult = MessageFormHelper:ShowMessage("Save changes?":U,
                                                          cBaseTitle,
                                                          "The current configuration has unsaved changes.":U,
                                                          MessageFormImages:ImageQuestion,
                                                          MessageFormDefaults:LabelYesNoCancel,
                                                          MessageFormDefaults:GetDialogResultYesNoCancel()) .

            IF oDialogResult:Equals (DialogResultEnum:DialogResultNo) THEN
                RETURN TRUE .

            IF oDialogResult:Equals (DialogResultEnum:DialogResultCancel) THEN
                RETURN FALSE .

            THIS-OBJECT:SaveFile().
        END.

        RETURN TRUE .

    END METHOD.

    /**
     * Purpose: Closes the current file and prepares the UI to create a new file.
     * Notes:
     */
    METHOD PRIVATE VOID CloseFile ():

        IF NOT THIS-OBJECT:CheckChanges() THEN
            RETURN.

        THIS-OBJECT:NewFile().

    END METHOD.

    /**
     * Purpose: Exented Initialization.
     * Notes:   If the Form is instantiated with a file name then that
     *          that file gets opened and parsed.
     * @param pcFileName The file to open and parse
     */
    METHOD PUBLIC VOID InitUI (pcFileName AS CHARACTER):

        THIS-OBJECT:PrepareTempTableAdapter ().

        IF pcFileName = ? OR pcFileName = "":U THEN DO:
            THIS-OBJECT:NewFile ().

            THIS-OBJECT:OpenFile ().
        END.

    END METHOD.

    /**
     * Purpose: Initializes the Visual Design
     * Notes:
     */
    @InitializeComponent.
    METHOD PRIVATE VOID InitializeComponent(  ):

        /* NOTE: The following method is automatically generated.

        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.

        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool48 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool48 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("New":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool49 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool49 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool50 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool50 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SaveAs":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool51 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool51 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Close":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("start":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("navigation":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("maintenance":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool15 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool15 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool16 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool16 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool17 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool17 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("transaction":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool20 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool20 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool21 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool21 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool22 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool22 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("NavigationToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool23 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool23 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool24 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool24 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool25 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool25 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool26 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool26 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TableIOToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool27 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool27 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool28 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool28 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool29 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool29 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool30 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool30 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool31 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool31 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool32 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool32 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar3 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar3 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TransactionToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool33 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool33 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool34 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool34 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool35 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool35 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool36 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool36 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool37 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool37 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool38 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool38 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool39 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool39 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool40 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool40 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool41 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool41 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool42 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool42 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool43 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool43 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool44 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool44 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool45 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool45 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool46 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool46 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool47 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool47 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool52 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool52 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
        appearance28 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool53 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool53 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("New":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
        appearance29 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool54 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool54 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
        appearance30 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool55 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool55 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SaveAs":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
        appearance31 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool56 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool56 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Close":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
        appearance32 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ttConfigurationSettings":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ConfigurationName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ConfigurationValue":U).
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
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("ttConfigurationSettings":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Studio.ConfigurationSettingsEditor.ConfigurationSettingsEditorForm":U).
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:ConfigurationSettingsEditorForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:smartUpdatableBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser().
        THIS-OBJECT:smartBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartTempTableAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        THIS-OBJECT:saveFileDialog1 = NEW System.Windows.Forms.SaveFileDialog().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ConfigurationSettingsEditorForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:ConfigurationSettingsEditorForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.Forms.BaseForm":U).
        buttonTool49:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 5 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        arrayvar0[2] = buttonTool48.
        arrayvar0[3] = buttonTool49.
        arrayvar0[4] = buttonTool50.
        arrayvar0[5] = buttonTool51.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar0).
        ribbonTab1:Caption = "Start":U.
        ribbonGroup1:Caption = "Navigation":U.
        buttonTool8:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool8:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool9:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool9:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool10:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool10:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool11:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool11:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar1[1] = buttonTool8.
        arrayvar1[2] = buttonTool9.
        arrayvar1[3] = buttonTool10.
        arrayvar1[4] = buttonTool11.
        ribbonGroup1:Tools:AddRange(arrayvar1).
        ribbonGroup2:Caption = "Maintenance":U.
        buttonTool12:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool12:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool13:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool13:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool14:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool14:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool15:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool16:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool17:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar2[1] = buttonTool12.
        arrayvar2[2] = buttonTool13.
        arrayvar2[3] = buttonTool14.
        arrayvar2[4] = buttonTool15.
        arrayvar2[5] = buttonTool16.
        arrayvar2[6] = buttonTool17.
        ribbonGroup2:Tools:AddRange(arrayvar2).
        ribbonGroup3:Caption = "Transaction":U.
        buttonTool18:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool19:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar3[1] = buttonTool18.
        arrayvar3[2] = buttonTool19.
        ribbonGroup3:Tools:AddRange(arrayvar3).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 3 NO-UNDO.
        arrayvar4[1] = ribbonGroup1.
        arrayvar4[2] = ribbonGroup2.
        arrayvar4[3] = ribbonGroup3.
        ribbonTab1:Groups:AddRange(arrayvar4).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar5[1] = ribbonTab1.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar5).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar6[1] = buttonTool20.
        arrayvar6[2] = buttonTool21.
        arrayvar6[3] = buttonTool22.
        THIS-OBJECT:smartToolbarController1:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar6).
        THIS-OBJECT:smartToolbarController1:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        THIS-OBJECT:smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        buttonTool23:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar7[1] = buttonTool23.
        arrayvar7[2] = buttonTool24.
        arrayvar7[3] = buttonTool25.
        arrayvar7[4] = buttonTool26.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar7).
        ultraToolbar1:Text = "Navigation Toolbar":U.
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        buttonTool27:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar8[1] = buttonTool27.
        arrayvar8[2] = buttonTool28.
        arrayvar8[3] = buttonTool29.
        arrayvar8[4] = buttonTool30.
        arrayvar8[5] = buttonTool31.
        arrayvar8[6] = buttonTool32.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar8).
        ultraToolbar2:Text = "TableIO Toolbar":U.
        ultraToolbar3:DockedColumn = 1.
        ultraToolbar3:DockedRow = 1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar9[1] = buttonTool33.
        arrayvar9[2] = buttonTool34.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar9).
        ultraToolbar3:Text = "Transaction Toolbar":U.
        ultraToolbar3:Visible = FALSE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 3 NO-UNDO.
        arrayvar10[1] = ultraToolbar1.
        arrayvar10[2] = ultraToolbar2.
        arrayvar10[3] = ultraToolbar3.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar10).
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        buttonTool35:SharedPropsInternal:AppearancesSmall:Appearance = appearance13.
        buttonTool35:SharedPropsInternal:Caption = "First record":U.
        buttonTool35:SharedPropsInternal:CustomizerCaption = "First":U.
        buttonTool35:SharedPropsInternal:Enabled = FALSE.
        buttonTool35:SharedPropsInternal:ToolTipText = "First record":U.
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        buttonTool36:SharedPropsInternal:AppearancesSmall:Appearance = appearance14.
        buttonTool36:SharedPropsInternal:Caption = "Previous record":U.
        buttonTool36:SharedPropsInternal:CustomizerCaption = "Previous":U.
        buttonTool36:SharedPropsInternal:Enabled = FALSE.
        buttonTool36:SharedPropsInternal:ToolTipText = "Previous record":U.
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        buttonTool37:SharedPropsInternal:AppearancesSmall:Appearance = appearance15.
        buttonTool37:SharedPropsInternal:Caption = "Next record":U.
        buttonTool37:SharedPropsInternal:CustomizerCaption = "Next":U.
        buttonTool37:SharedPropsInternal:Enabled = FALSE.
        buttonTool37:SharedPropsInternal:ToolTipText = "Next record":U.
        appearance16:Image = CAST(resources:GetObject("appearance16.Image":U), System.Object).
        buttonTool38:SharedPropsInternal:AppearancesSmall:Appearance = appearance16.
        buttonTool38:SharedPropsInternal:Caption = "Last record":U.
        buttonTool38:SharedPropsInternal:CustomizerCaption = "Last":U.
        buttonTool38:SharedPropsInternal:Enabled = FALSE.
        buttonTool38:SharedPropsInternal:ToolTipText = "Last record":U.
        appearance17:Image = CAST(resources:GetObject("appearance17.Image":U), System.Object).
        buttonTool39:SharedPropsInternal:AppearancesSmall:Appearance = appearance17.
        buttonTool39:SharedPropsInternal:Caption = "Add record":U.
        buttonTool39:SharedPropsInternal:CustomizerCaption = "Add":U.
        buttonTool39:SharedPropsInternal:Enabled = FALSE.
        buttonTool39:SharedPropsInternal:ToolTipText = "Add record":U.
        appearance18:Image = CAST(resources:GetObject("appearance18.Image":U), System.Object).
        buttonTool40:SharedPropsInternal:AppearancesSmall:Appearance = appearance18.
        buttonTool40:SharedPropsInternal:Caption = "Copy record":U.
        buttonTool40:SharedPropsInternal:CustomizerCaption = "Copy":U.
        buttonTool40:SharedPropsInternal:Enabled = FALSE.
        buttonTool40:SharedPropsInternal:ToolTipText = "Copy record":U.
        appearance19:Image = CAST(resources:GetObject("appearance19.Image":U), System.Object).
        buttonTool41:SharedPropsInternal:AppearancesSmall:Appearance = appearance19.
        buttonTool41:SharedPropsInternal:Caption = "Update record":U.
        buttonTool41:SharedPropsInternal:CustomizerCaption = "Update":U.
        buttonTool41:SharedPropsInternal:Enabled = FALSE.
        buttonTool41:SharedPropsInternal:ToolTipText = "Update record":U.
        appearance20:Image = CAST(resources:GetObject("appearance20.Image":U), System.Object).
        buttonTool42:SharedPropsInternal:AppearancesSmall:Appearance = appearance20.
        buttonTool42:SharedPropsInternal:Caption = "Save changes":U.
        buttonTool42:SharedPropsInternal:CustomizerCaption = "Save":U.
        buttonTool42:SharedPropsInternal:Enabled = FALSE.
        buttonTool42:SharedPropsInternal:ToolTipText = "Save changes":U.
        appearance21:Image = CAST(resources:GetObject("appearance21.Image":U), System.Object).
        buttonTool43:SharedPropsInternal:AppearancesSmall:Appearance = appearance21.
        buttonTool43:SharedPropsInternal:Caption = "Cancel update":U.
        buttonTool43:SharedPropsInternal:CustomizerCaption = "Cancel":U.
        buttonTool43:SharedPropsInternal:Enabled = FALSE.
        buttonTool43:SharedPropsInternal:ToolTipText = "Cancel update":U.
        appearance22:Image = CAST(resources:GetObject("appearance22.Image":U), System.Object).
        buttonTool44:SharedPropsInternal:AppearancesSmall:Appearance = appearance22.
        buttonTool44:SharedPropsInternal:Caption = "Delete record":U.
        buttonTool44:SharedPropsInternal:CustomizerCaption = "Delete":U.
        buttonTool44:SharedPropsInternal:Enabled = FALSE.
        buttonTool44:SharedPropsInternal:ToolTipText = "Delete record":U.
        appearance23:Image = CAST(resources:GetObject("appearance23.Image":U), System.Object).
        buttonTool45:SharedPropsInternal:AppearancesSmall:Appearance = appearance23.
        buttonTool45:SharedPropsInternal:Caption = "&Exit":U.
        buttonTool45:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance24:Image = CAST(resources:GetObject("appearance24.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesLarge:Appearance = appearance24.
        appearance25:Image = CAST(resources:GetObject("appearance25.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesSmall:Appearance = appearance25.
        buttonTool46:SharedPropsInternal:Caption = "Commit":U.
        buttonTool46:SharedPropsInternal:CustomizerCaption = "Commit":U.
        buttonTool46:SharedPropsInternal:Enabled = FALSE.
        buttonTool46:SharedPropsInternal:ToolTipText = "Commit transaction":U.
        appearance26:Image = CAST(resources:GetObject("appearance26.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesLarge:Appearance = appearance26.
        appearance27:Image = CAST(resources:GetObject("appearance27.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesSmall:Appearance = appearance27.
        buttonTool47:SharedPropsInternal:Caption = "Revert":U.
        buttonTool47:SharedPropsInternal:CustomizerCaption = "Revert":U.
        buttonTool47:SharedPropsInternal:Enabled = FALSE.
        buttonTool47:SharedPropsInternal:ToolTipText = "Revert transaction":U.
        appearance28:Image = CAST(resources:GetObject("appearance28.Image":U), System.Object).
        buttonTool52:SharedPropsInternal:AppearancesLarge:Appearance = appearance28.
        buttonTool52:SharedPropsInternal:Caption = "Open":U.
        buttonTool52:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance29:Image = CAST(resources:GetObject("appearance29.Image":U), System.Object).
        buttonTool53:SharedPropsInternal:AppearancesLarge:Appearance = appearance29.
        buttonTool53:SharedPropsInternal:Caption = "New":U.
        buttonTool53:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance30:Image = CAST(resources:GetObject("appearance30.Image":U), System.Object).
        buttonTool54:SharedPropsInternal:AppearancesLarge:Appearance = appearance30.
        buttonTool54:SharedPropsInternal:Caption = "Save":U.
        buttonTool54:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance31:Image = CAST(resources:GetObject("appearance31.Image":U), System.Object).
        buttonTool55:SharedPropsInternal:AppearancesLarge:Appearance = appearance31.
        buttonTool55:SharedPropsInternal:Caption = "Save as":U.
        buttonTool55:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance32:Image = CAST(resources:GetObject("appearance32.Image":U), System.Object).
        buttonTool56:SharedPropsInternal:AppearancesLarge:Appearance = appearance32.
        buttonTool56:SharedPropsInternal:Caption = "Close":U.
        buttonTool56:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 18 NO-UNDO.
        arrayvar11[1] = buttonTool35.
        arrayvar11[2] = buttonTool36.
        arrayvar11[3] = buttonTool37.
        arrayvar11[4] = buttonTool38.
        arrayvar11[5] = buttonTool39.
        arrayvar11[6] = buttonTool40.
        arrayvar11[7] = buttonTool41.
        arrayvar11[8] = buttonTool42.
        arrayvar11[9] = buttonTool43.
        arrayvar11[10] = buttonTool44.
        arrayvar11[11] = buttonTool45.
        arrayvar11[12] = buttonTool46.
        arrayvar11[13] = buttonTool47.
        arrayvar11[14] = buttonTool52.
        arrayvar11[15] = buttonTool53.
        arrayvar11[16] = buttonTool54.
        arrayvar11[17] = buttonTool55.
        arrayvar11[18] = buttonTool56.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar11).
        THIS-OBJECT:smartToolbarController1:ToolClick:Subscribe(THIS-OBJECT:ToolClickHandler).
        /*  */
        /* ConfigurationSettingsEditorForm_Fill_Panel */
        /*  */
        /*  */
        /* ConfigurationSettingsEditorForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:ConfigurationSettingsEditorForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:smartUpdatableBrowser1).
        THIS-OBJECT:ConfigurationSettingsEditorForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:ConfigurationSettingsEditorForm_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ConfigurationSettingsEditorForm_Fill_Panel:Location = NEW System.Drawing.Point(8, 151).
        THIS-OBJECT:ConfigurationSettingsEditorForm_Fill_Panel:Name = "ConfigurationSettingsEditorForm_Fill_Panel":U.
        THIS-OBJECT:ConfigurationSettingsEditorForm_Fill_Panel:Size = NEW System.Drawing.Size(764, 358).
        THIS-OBJECT:ConfigurationSettingsEditorForm_Fill_Panel:TabIndex = 0.
        /*  */
        /* smartUpdatableBrowser1 */
        /*  */
        THIS-OBJECT:smartUpdatableBrowser1:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartUpdatableBrowser1:DataSource = THIS-OBJECT:smartBindingSource1.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Appearance = appearance1.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn3:Header:Caption = " Attribute Name":U.
        ultraGridColumn3:Header:VisiblePosition = 0.
        ultraGridColumn3:Width = 152.
        ultraGridColumn4:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn4:Header:Caption = " Attribute Value":U.
        ultraGridColumn4:Header:VisiblePosition = 1.
        ultraGridColumn4:Width = 432.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS System.Object EXTENT 2 NO-UNDO.
        arrayvar12[1] = ultraGridColumn3.
        arrayvar12[2] = ultraGridColumn4.
        ultraGridBand1:Columns:AddRange(arrayvar12).
        ultraGridBand1:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand1:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance2:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:Appearance = appearance2.
        appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:Hidden = TRUE.
        appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance4:BackColor2 = System.Drawing.SystemColors:Control.
        appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance5:BackColor = System.Drawing.SystemColors:Window.
        appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveCellAppearance = appearance5.
        appearance6:BackColor = System.Drawing.SystemColors:Highlight.
        appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveRowAppearance = appearance6.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance7:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CardAreaAppearance = appearance7.
        appearance8:BorderColor = System.Drawing.Color:Silver.
        appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellAppearance = appearance8.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellPadding = 0.
        appearance9:BackColor = System.Drawing.SystemColors:Control.
        appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance9:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:GroupByRowAppearance = appearance9.
        appearance10:TextHAlignAsString = "Left":U.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderAppearance = appearance10.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance11:BackColor = System.Drawing.SystemColors:Window.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowAppearance = appearance11.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartUpdatableBrowser1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:smartUpdatableBrowser1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:smartUpdatableBrowser1:LinkDataSource = THIS-OBJECT:smartTempTableAdapter1.
        THIS-OBJECT:smartUpdatableBrowser1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartUpdatableBrowser1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:smartUpdatableBrowser1:Name = "smartUpdatableBrowser1":U.
        THIS-OBJECT:smartUpdatableBrowser1:SettingsKey = "b54a09d0-d3c9-66b1-1b14-06cae8721b44":U.
        THIS-OBJECT:smartUpdatableBrowser1:Size = NEW System.Drawing.Size(764, 358).
        THIS-OBJECT:smartUpdatableBrowser1:TabIndex = 0.
        THIS-OBJECT:smartUpdatableBrowser1:Text = "smartUpdatableBrowser1":U.
        /*  */
        /* smartBindingSource1 */
        /*  */
        THIS-OBJECT:smartBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:smartBindingSource1:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar13 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar13.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS Progress.Data.ColumnPropDesc EXTENT 2 NO-UNDO.
        arrayvar14[1] = NEW Progress.Data.ColumnPropDesc("ConfigurationName":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar14[2] = NEW Progress.Data.ColumnPropDesc("ConfigurationValue":U, "":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar14.
        THIS-OBJECT:smartBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* smartTempTableAdapter1 */
        /*  */
        THIS-OBJECT:smartTempTableAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartTempTableAdapter1:LinkNavigationSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartTempTableAdapter1:AfterAssignRecord:Subscribe (THIS-OBJECT:RecordsChangedHandler).
        THIS-OBJECT:smartTempTableAdapter1:AfterDeleteRecord:Subscribe (THIS-OBJECT:RecordsChangedHandler).
        /*  */
        /* m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 151).
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Left:Name = "m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 358).
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(772, 151).
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Right:Name = "m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 358).
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Top:Name = "m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(780, 151).
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 509).
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Bottom:Name = "m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(780, 8).
        THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* openFileDialog1 */
        /*  */
        THIS-OBJECT:openFileDialog1:DefaultExt = "annotations":U.
        THIS-OBJECT:openFileDialog1:Filter = resources:GetString("openFileDialog1.Filter":U).
        /*  */
        /* saveFileDialog1 */
        /*  */
        THIS-OBJECT:saveFileDialog1:DefaultExt = "*.applicationsettings":U.
        THIS-OBJECT:saveFileDialog1:Filter = resources:GetString("saveFileDialog1.Filter":U).
        THIS-OBJECT:saveFileDialog1:RestoreDirectory = TRUE.
        /*  */
        /* ConfigurationSettingsEditorForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(780, 517).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ConfigurationSettingsEditorForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ConfigurationSettingsEditorForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "ConfigurationSettingsEditorForm":U.
        THIS-OBJECT:Text = "ConfigurationSettingsEditorForm":U.
        THIS-OBJECT:WindowPositionRegistryKey = "ConfigurationSettingsEditorForm":U.
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ConfigurationSettingsEditorForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ConfigurationSettingsEditorForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /**
     * Purpose: Prepares the Form to Create a new file
     * Notes:
     */
    METHOD PRIVATE VOID NewFile ():

        IF NOT THIS-OBJECT:CheckChanges() THEN
            RETURN.

        EMPTY TEMP-TABLE ttConfiguration.

        THIS-OBJECT:RefreshUI (?).

    END METHOD.

    /**
     * Purpose: Raises the FormClosing Event
     * Notes:
     * @param e The FormClosingEventArgs with the data for this event
     */
    METHOD PROTECTED OVERRIDE VOID OnFormClosing (e AS System.Windows.Forms.FormClosingEventArgs):

        SUPER:OnFormClosing (e) .

        IF e:Cancel THEN
            RETURN .

        oLruController:StoreLru () .

    END METHOD .

    /**
     * Purpose: Raises the Shown event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD OVERRIDE PROTECTED VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown(e).

        oLruController:LoadLru () .

        THIS-OBJECT:InitUI (cCurrentFileName).

    END METHOD .

    /**
     * Purpose: Shows the FileOpenDialog to let the user select a file and opens that file
     * Notes:
     */
    METHOD PRIVATE VOID OpenFile ():

        DEFINE VARIABLE oResult AS System.Windows.Forms.DialogResult NO-UNDO .

        IF NOT THIS-OBJECT:CheckChanges() THEN
            RETURN.

        WAIT-FOR openFileDialog1:ShowDialog () SET oResult .

        IF NOT Progress.Util.EnumHelper:AreEqual (System.Windows.Forms.DialogResult:OK,
                                                  oResult) THEN
            RETURN .

        THIS-OBJECT:OpenFile (openFileDialog1:FileName).

    END METHOD.

    /**
     * Purpose: Opens a file and dependent on it's extension it parses the Json or XML content
     * Notes:
     * @param pcFileName The file to open and parse
     */
    METHOD PRIVATE VOID OpenFile (pcFileName AS CHARACTER):

        DEFINE VARIABLE oConfigurationFileAccess AS IConfigurationFileAccess NO-UNDO.

        CASE FileHelper:FileExtension (pcFileName):

            WHEN "applicationsettings":U OR
            WHEN "restapplicationsettings":U THEN
                oConfigurationFileAccess = NEW JsonConfigurationFileAccess ().

            WHEN "xml":U THEN
                oConfigurationFileAccess = NEW XMLConfigurationFileAccess ().

            OTHERWISE DO:
                UNDO, THROW NEW Progress.Lang.AppError (SUBSTITUTE ("Extension '&1' is not supported":U,
                                                                    FileHelper:FileExtension (pcFileName)), 0).
            END.

        END CASE.

        IF VALID-OBJECT (oConfigurationFileAccess) THEN DO:

            oConfigurationFileAccess:ReadFile (pcFileName,
                                               OUTPUT TABLE ttConfiguration).

            THIS-OBJECT:RefreshUI (pcFileName).

        END.

        oLruController:AddItemToLru (pcFileName,
                                     pcFileName) .

        CATCH ple AS Progress.Lang.Error:

            ErrorHelper:ShowErrorMessage(ple).

        END CATCH.

    END METHOD.

    /**
     * Purpose: Opens a file from the LRU list
     * Notes:
     * @param pcFileName The file name to load
     */
    METHOD PROTECTED VOID OpenFileFromLRU (pcFileName AS CHARACTER):

        IF NOT THIS-OBJECT:CheckChanges() THEN
            RETURN.

        THIS-OBJECT:OpenFile (pcFileName).

    END METHOD.

    /**
     * Purpose: Event handler for the ToolClick event of the smartToolbarController1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The ToolClickEventArgs with the data for this event
     */
    METHOD PRIVATE VOID ToolClickHandler( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs ):

        CASE e:Tool:Key:

            WHEN "Open":U THEN
                THIS-OBJECT:OpenFile().

            WHEN "New":U THEN
                THIS-OBJECT:NewFile().

            WHEN "Save":U THEN
                THIS-OBJECT:SaveFile().

            WHEN "SaveAs":U THEN
                THIS-OBJECT:SaveFileAs().

            WHEN "Close":U THEN
                THIS-OBJECT:CloseFile().

        END CASE.

        IF e:Tool:Key BEGINS "lru_":U THEN
            OpenFileFromLru (UNBOX (e:Tool:SharedProps:RootTool:Tag)) .

    END METHOD.

    /**
     * Purpose: Returns the Title that should be shown in the title bar Business Entity Designer Form
     * Notes:
     * @param pcFileName The currently open file name or ? for "untitled"
     * @return The Title that should be shown in the title bar
     */
    METHOD PROTECTED CHARACTER WindowTitle (pcFileName AS CHARACTER):

        DEFINE VARIABLE cShortFileName AS CHARACTER NO-UNDO.

        IF pcFileName <> ? AND pcFileName NE "?":U THEN DO:

            ASSIGN cShortFileName = FileHelper:ShortFileName(pcFileName).

            RETURN SUBSTITUTE ("&1 - &2 (&3)":U,
                               cShortFileName,
                               cBaseTitle,
                               pcFileName) .
        END.
        ELSE
            RETURN SUBSTITUTE ("<untitled> - &1":U, cBaseTitle) .

    END METHOD.

    DESTRUCTOR PUBLIC ConfigurationSettingsEditorForm ( ):

        GarbageCollectorHelper:DeleteObject (oLruController) .

    END DESTRUCTOR.

END CLASS.