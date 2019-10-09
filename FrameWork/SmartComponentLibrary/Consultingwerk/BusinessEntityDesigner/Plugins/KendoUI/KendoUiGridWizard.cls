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
    File        : SmartWindowWizard
    Purpose     : Wizard for creating a SmartWindowForm from a Business
                  Entity Model
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 20 21:40:10 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*                               FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.*                   FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.KendoUI.* FROM PROPATH.
USING Consultingwerk.BusinessEntityDesigner.Plugins.KendoUI.*   FROM PROPATH .
USING Consultingwerk.Forms.*                                    FROM PROPATH .
USING Consultingwerk.Framework.*                                FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                    FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                     FROM PROPATH .
USING Consultingwerk.Util.*                                     FROM PROPATH .
USING Consultingwerk.Windows.*                                  FROM PROPATH .
USING Consultingwerk.Windows.Controls.*                         FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*                       FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*                        FROM ASSEMBLY .
USING Progress.Lang.*                                           FROM PROPATH .
USING System.Windows.Forms.*                                    FROM ASSEMBLY .
USING Consultingwerk.Windows.API.*                              FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.KendoUI.KendoUiGridWizard
    INHERITS BaseForm
    USE-WIDGET-POOL:

    { Consultingwerk/Util/TempTables/ttClassNames.i }

    DEFINE PRIVATE VARIABLE btnAdd AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnBack AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnFinish AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnNext AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnRemove AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE coGridBuilder AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE coPageTemplate AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE lvViewerFields AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE lvBusinessEntityFields AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE saveFileDialog1 AS System.Windows.Forms.SaveFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE tbDataSourceName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbPageTitle AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbOutputFileName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE treeView1 AS System.Windows.Forms.TreeView NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel6 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel7 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel8 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl3 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.

    DEFINE VARIABLE iPage    AS INTEGER NO-UNDO INIT 1.
    DEFINE VARIABLE hDataset AS HANDLE  NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Business Entity Name
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessEntityName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Business Entity Package
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessEntityPackage AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Label to be shown to the "Form Title" text box
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FormTitleLabel AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:ultraLabel8:Text .
    END GET.
    SET (arg AS CHARACTER):
        ASSIGN THIS-OBJECT:ultraLabel8:Text = arg .
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Logical value indicating if the code should be generated for the
                 generic interface SCL-819 (TRUE) or Service Interface resources were
                 generated using the REST Interface wizard (FALSE)
        Notes:   Default is to use the generic service interface
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UseGenericInterface AS LOGICAL NO-UNDO INIT TRUE
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartWindowWizard class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC KendoUiGridWizard ():

        DEFINE VARIABLE oImage AS System.Drawing.Image NO-UNDO .

        SUPER().

        InitializeComponent().

        THIS-OBJECT:StoreFormPosition = FALSE .

        ASSIGN oImage                                               = ImageHelper:Load ("Consultingwerk~\BusinessEntityDesigner~\UI~\Images~\table2_selection_column.png":U)
               lvBusinessEntityFields:ItemSettings:Appearance:Image = oImage
               lvViewerFields:ItemSettings:Appearance:Image         = oImage
                .

        NEW UltraListViewOrderController (lvBusinessEntityFields) .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnAdd
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnAdd_Click (sender AS System.Object,
                                      e AS System.EventArgs):

        DEFINE VARIABLE oItemsToRemove AS "System.Collections.Generic.List<Infragistics.Win.UltraWinListView.UltraListViewItem>":U NO-UNDO .

        oItemsToRemove = NEW "System.Collections.Generic.List<Infragistics.Win.UltraWinListView.UltraListViewItem>":U () .

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in lvBusinessEntityFields:SelectedItems}
            lvViewerFields:Items:Add (oItem:Key, oItem:Key) .
            oItemsToRemove:Add (oItem) .
        END.

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in oItemsToRemove nodefine}
            lvBusinessEntityFields:Items:Remove (oItem) .
        END.

        IF iPage = 2 THEN
            btnNext:Enabled = (lvViewerFields:Items:Count > 0) .

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnBack
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnBack_Click (sender AS System.Object,
                                       e AS System.EventArgs):

        ASSIGN iPage = iPage - 1 .

        THIS-OBJECT:InitializePage () .

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnFinish
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnFinish_Click (sender AS System.Object,
                                         e AS System.EventArgs):

        THIS-OBJECT:GeneratePage () .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnNext
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnNext_Click (sender AS System.Object,
                                       e AS System.EventArgs):

        ASSIGN iPage = iPage + 1 .

        THIS-OBJECT:InitializePage () .

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnRemove
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnRemove_Click (INPUT sender AS System.Object, INPUT e AS System.EventArgs):

        DEFINE VARIABLE oItemsToRemove AS "System.Collections.Generic.List<Infragistics.Win.UltraWinListView.UltraListViewItem>":U NO-UNDO .

        oItemsToRemove = NEW "System.Collections.Generic.List<Infragistics.Win.UltraWinListView.UltraListViewItem>":U () .

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in lvViewerFields:SelectedItems}
            lvBusinessEntityFields:Items:Add (oItem:Key, oItem:Key) .
            oItemsToRemove:Add (oItem) .
        END.

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in oItemsToRemove nodefine}
            lvViewerFields:Items:Remove (oItem) .
        END.

        IF iPage = 2 THEN
            btnNext:Enabled = (lvViewerFields:Items:Count > 0) .

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Verifies if the user can press finish
        Notes:
        @return
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL CheckFinish ():

        RETURN THIS-OBJECT:tbOutputFileName:Text > "":U AND
               THIS-OBJECT:tbDataSourceName:Text > "":U AND
               THIS-OBJECT:tbPageTitle:Text > "":U AND
               UNBOX (THIS-OBJECT:coGridBuilder:Value) > "":U AND
               UNBOX (THIS-OBJECT:coPageTemplate:Value) > "":U  .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Walk though the Dataset TopBuffers and add all of them with their
                 childs to the TreeView
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID FillTreeViewWithTableNames ():

        DEFINE VARIABLE oFactory AS TreeNodeFromDatasetStructureFactory NO-UNDO .

        IF THIS-OBJECT:treeView1:Nodes:Count > 0 THEN
            RETURN .

        oFactory = NEW TreeNodeFromDatasetStructureFactory () .

        oFactory:CreateTreeNodes (THIS-OBJECT:treeView1,
                                  hDataset) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Generates the SmartWindowForm based on the entered values
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GeneratePage ():

        DEFINE VARIABLE oBuilder   AS IKendoUiGridBuilder  NO-UNDO .
        DEFINE VARIABLE oGenerator AS KendoUiGridGenerator NO-UNDO .
        DEFINE VARIABLE cUrl       AS CHARACTER            NO-UNDO .

        DEFINE VARIABLE hBuffer    AS HANDLE               NO-UNDO .

        oBuilder = DYNAMIC-NEW (UNBOX (coGridBuilder:Value)) ().

        ASSIGN oBuilder:DataSourceName      = tbDataSourceName:Text
               oBuilder:DataSourceTableName = smartBusinessEntityBindingSource1:EntityTable
               oBuilder:OutputFileName      = tbOutputFileName:Text
               oBuilder:PageTitle           = tbPageTitle:Text
               oBuilder:TemplateFileName    = UNBOX (coPageTemplate:Value)

               hBuffer = hDataset:GET-BUFFER-HANDLE (smartBusinessEntityBindingSource1:EntityTable) .

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in lvViewerFields:Items}

            oBuilder:AddColumn (oItem:Key,
                                hBuffer:BUFFER-FIELD (oItem:Key):DATA-TYPE,
                                hBuffer:BUFFER-FIELD (oItem:Key):LABEL,
                                FormatHelper:GetFormatWidthPixels (hBuffer:BUFFER-FIELD (oItem:Key):FORMAT)) .

        END.

        oGenerator = NEW KendoUiGridGenerator () .
        oGenerator:Generate (oBuilder) .

        /* Mike Fechner, Consultingwerk Ltd. 05.05.2015
           Open in browser, when done */
        ASSIGN cUrl = SUBSTITUTE ("http://localhost:8980/KendoBackendService/static/&1":U,
                                  FileHelper:ShortFileName (oBuilder:OutputFileName)) .


        IF InputPromptHelper:PromptForCharacterValue ("Open the generated file in a browser?"{&TRAN},
                                                      "The html page was successfully created. Please choose OK to open it in the web browser.~n~nPlease deploy the file to your web server first."{&TRAN},
                                                      INPUT-OUTPUT cUrl)
        THEN
            Win32:ShellExecute ("open":U, cUrl) .
        ELSE DO:
            FILE-INFORMATION:FILE-NAME = oBuilder:OutputFileName .

            IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
                WindowsExplorerHelper:ShowFileInExplorer (FILE-INFORMATION:FULL-PATHNAME) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the first selected table to display in DataView and builds
                 a list of all selected child tables from the given selected node
        Notes:   Returns the name of the SelectedTopBuffer with a list of checked
                 nodes or returns the TreeNode object of the SelectedTopBuffer
        @param cCheckedNodes The list of all selected child tables from the given selected node
        @return The name of the first selected table to display in DataView
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER GetSelectedTopBuffer (OUTPUT cCheckedNodes AS CHARACTER):

        DEFINE VARIABLE oChildResult AS TreeNode NO-UNDO.

        oChildResult = RecurseTreeNodes (THIS-OBJECT:treeView1:Nodes).

        IF VALID-OBJECT (oChildResult) THEN DO:
            ASSIGN
                cCheckedNodes = RecurseTreeNodeNames (oChildResult:Nodes).

            RETURN oChildResult:Text.
        END.
        ELSE DO:
            cCheckedNodes = "":U.
            RETURN "":U.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the List of BindingSource Fields
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeBindingSourceFields ():

        DEFINE VARIABLE cEntityTable AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cEntityView  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cEntityJoin  AS CHARACTER NO-UNDO .

        DEFINE VARIABLE oColumnsToRemove AS "System.Collections.Generic.List<System.String>":U NO-UNDO .

        oColumnsToRemove = NEW "System.Collections.Generic.List<System.String>":U () .

        ASSIGN cEntityTable = THIS-OBJECT:GetSelectedTopBuffer (OUTPUT cEntityView)
               cEntityJoin  = TRIM (FILL ("YES,":U, NUM-ENTRIES (cEntityView)), ",":U)

               smartBusinessEntityBindingSource1:EntityName  = SUBSTITUTE ("&1.&2":U,
                                                                           THIS-OBJECT:BusinessEntityPackage,
                                                                           THIS-OBJECT:BusinessEntityName)
               smartBusinessEntityBindingSource1:EntityTable = cEntityTable
               smartBusinessEntityBindingSource1:EntityView  = cEntityView
               smartBusinessEntityBindingSource1:EntityJoin  = cEntityJoin
               .

        smartBusinessEntityBindingSource1:DesignerImportSchema (TRUE) .

        {Consultingwerk/foreach.i Progress.Data.ColumnPropDesc oColumn in smartBusinessEntityBindingSource1:TableSchema:Columns}

            IF NOT lvBusinessEntityFields:Items:Exists (oColumn:ColumnName) AND
                  NOT lvViewerFields:Items:Exists (oColumn:ColumnName) THEN DO:

                   lvBusinessEntityFields:Items:Add (oColumn:ColumnName, oColumn:ColumnName) .
               END.
           END.

        oColumnsToRemove:Clear () .

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in lvBusinessEntityFields:Items}

            IF NOT THIS-OBJECT:ValidColumn (oItem:Key) THEN
                oColumnsToRemove:Add (oItem:Key) .
        END.

        {Consultingwerk/foreach.i System.Object oObject in oColumnsToRemove}
            lvBusinessEntityFields:Items:Remove (lvBusinessEntityFields:Items[STRING (CAST (oObject, System.String))]) .
        END.

        oColumnsToRemove:Clear () .

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in lvViewerFields:Items nodefine}

            IF NOT THIS-OBJECT:ValidColumn (oItem:Key) THEN
                oColumnsToRemove:Add (oItem:Key) .
        END.

        {Consultingwerk/foreach.i System.Object oObject in oColumnsToRemove nodefine}
            lvViewerFields:Items:Remove (lvViewerFields:Items[STRING (CAST (oObject, System.String))]) .
        END.


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
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.Plugins.KendoUI.KendoUiGridWizard":U).
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:treeView1 = NEW System.Windows.Forms.TreeView().
        THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:btnRemove = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnAdd = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:lvViewerFields = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:lvBusinessEntityFields = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTabPageControl3 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraLabel7 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbDataSourceName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel6 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:coPageTemplate = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:coGridBuilder = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:tbPageTitle = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel8 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbOutputFileName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:btnCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnBack = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnNext = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:btnFinish = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:saveFileDialog1 = NEW System.Windows.Forms.SaveFileDialog().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        THIS-OBJECT:ultraTabPageControl2:SuspendLayout().
        CAST(THIS-OBJECT:lvViewerFields, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:lvBusinessEntityFields, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl3:SuspendLayout().
        CAST(THIS-OBJECT:tbDataSourceName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:coPageTemplate, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:coGridBuilder, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbPageTitle, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbOutputFileName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:treeView1).
        THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(570, 366).
        /*  */
        /* ultraLabel3 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel3:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel3:Appearance = appearance1.
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(3, 3).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(564, 23).
        THIS-OBJECT:ultraLabel3:TabIndex = 4.
        THIS-OBJECT:ultraLabel3:Text = "Please select those tables of the BindingSource that should be used for the Form's BindingSource:":U.
        /*  */
        /* treeView1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:treeView1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:treeView1:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
        THIS-OBJECT:treeView1:CheckBoxes = TRUE.
        THIS-OBJECT:treeView1:Location = NEW System.Drawing.Point(3, 32).
        THIS-OBJECT:treeView1:Name = "treeView1":U.
        THIS-OBJECT:treeView1:Size = NEW System.Drawing.Size(567, 334).
        THIS-OBJECT:treeView1:TabIndex = 1.
        THIS-OBJECT:treeView1:BeforeCheck:Subscribe(THIS-OBJECT:treeView1_BeforeCheck).
        /*  */
        /* ultraTabPageControl2 */
        /*  */
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:btnRemove).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:btnAdd).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:lvViewerFields).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:lvBusinessEntityFields).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:ultraTabPageControl2:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2":U.
        THIS-OBJECT:ultraTabPageControl2:Size = NEW System.Drawing.Size(570, 366).
        /*  */
        /* btnRemove */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:btnRemove:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnRemove:Location = NEW System.Drawing.Point(256, 169).
        THIS-OBJECT:btnRemove:Name = "btnRemove":U.
        THIS-OBJECT:btnRemove:Size = NEW System.Drawing.Size(58, 23).
        THIS-OBJECT:btnRemove:TabIndex = 6.
        THIS-OBJECT:btnRemove:Text = "<<":U.
        THIS-OBJECT:btnRemove:Click:Subscribe(THIS-OBJECT:btnRemove_Click).
        /*  */
        /* btnAdd */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:btnAdd:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnAdd:Location = NEW System.Drawing.Point(256, 140).
        THIS-OBJECT:btnAdd:Name = "btnAdd":U.
        THIS-OBJECT:btnAdd:Size = NEW System.Drawing.Size(58, 23).
        THIS-OBJECT:btnAdd:TabIndex = 5.
        THIS-OBJECT:btnAdd:Text = ">>":U.
        THIS-OBJECT:btnAdd:Click:Subscribe(THIS-OBJECT:btnAdd_Click).
        /*  */
        /* lvViewerFields */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:lvViewerFields:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:lvViewerFields:Location = NEW System.Drawing.Point(320, 32).
        THIS-OBJECT:lvViewerFields:Name = "lvViewerFields":U.
        THIS-OBJECT:lvViewerFields:Size = NEW System.Drawing.Size(250, 334).
        THIS-OBJECT:lvViewerFields:TabIndex = 4.
        THIS-OBJECT:lvViewerFields:Text = "ultraListView2":U.
        THIS-OBJECT:lvViewerFields:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
        THIS-OBJECT:lvViewerFields:ItemDoubleClick:Subscribe(THIS-OBJECT:lvViewerFields_ItemDoubleClick).
        /*  */
        /* lvBusinessEntityFields */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:lvBusinessEntityFields:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:lvBusinessEntityFields:Location = NEW System.Drawing.Point(0, 32).
        THIS-OBJECT:lvBusinessEntityFields:Name = "lvBusinessEntityFields":U.
        THIS-OBJECT:lvBusinessEntityFields:Size = NEW System.Drawing.Size(250, 334).
        THIS-OBJECT:lvBusinessEntityFields:TabIndex = 0.
        THIS-OBJECT:lvBusinessEntityFields:Text = "ultraListView1":U.
        THIS-OBJECT:lvBusinessEntityFields:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
        THIS-OBJECT:lvBusinessEntityFields:ItemDoubleClick:Subscribe(THIS-OBJECT:lvBusinessEntityFields_ItemDoubleClick).
        /*  */
        /* ultraLabel2 */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel2:Appearance = appearance2.
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(3, 3).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(564, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 3.
        THIS-OBJECT:ultraLabel2:Text = "Please select the fields that should be shown in the Grid Control:":U.
        /*  */
        /* ultraTabPageControl3 */
        /*  */
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:ultraLabel7).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:tbDataSourceName).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:ultraLabel6).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:coPageTemplate).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:coGridBuilder).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:tbPageTitle).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:ultraLabel8).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:tbOutputFileName).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:ultraTabPageControl3:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraTabPageControl3:Name = "ultraTabPageControl3":U.
        THIS-OBJECT:ultraTabPageControl3:Size = NEW System.Drawing.Size(570, 366).
        /*  */
        /* ultraLabel7 */
        /*  */
        THIS-OBJECT:ultraLabel7:Location = NEW System.Drawing.Point(16, 135).
        THIS-OBJECT:ultraLabel7:Name = "ultraLabel7":U.
        THIS-OBJECT:ultraLabel7:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel7:TabIndex = 15.
        THIS-OBJECT:ultraLabel7:Text = "Data Source:":U.
        /*  */
        /* tbDataSourceName */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDataSourceName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDataSourceName:Location = NEW System.Drawing.Point(122, 131).
        THIS-OBJECT:tbDataSourceName:Name = "tbDataSourceName":U.
        THIS-OBJECT:tbDataSourceName:Size = NEW System.Drawing.Size(445, 21).
        THIS-OBJECT:tbDataSourceName:TabIndex = 14.
        THIS-OBJECT:tbDataSourceName:TextChanged:Subscribe(THIS-OBJECT:tbOutputFileName_TextChanged).
        /*  */
        /* ultraLabel6 */
        /*  */
        THIS-OBJECT:ultraLabel6:Location = NEW System.Drawing.Point(16, 225).
        THIS-OBJECT:ultraLabel6:Name = "ultraLabel6":U.
        THIS-OBJECT:ultraLabel6:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel6:TabIndex = 13.
        THIS-OBJECT:ultraLabel6:Text = "Template:":U.
        /*  */
        /* ultraLabel5 */
        /*  */
        THIS-OBJECT:ultraLabel5:Location = NEW System.Drawing.Point(16, 196).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        THIS-OBJECT:ultraLabel5:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel5:TabIndex = 12.
        THIS-OBJECT:ultraLabel5:Text = "Grid Builder:":U.
        /*  */
        /* coPageTemplate */
        /*  */
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar8 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:coPageTemplate:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:coPageTemplate:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:coPageTemplate:Location = NEW System.Drawing.Point(122, 219).
        THIS-OBJECT:coPageTemplate:Name = "coPageTemplate":U.
        THIS-OBJECT:coPageTemplate:Size = NEW System.Drawing.Size(445, 21).
        THIS-OBJECT:coPageTemplate:TabIndex = 11.
        THIS-OBJECT:coPageTemplate:ValueChanged:Subscribe(THIS-OBJECT:tbOutputFileName_TextChanged).
        /*  */
        /* coGridBuilder */
        /*  */
        DEFINE VARIABLE nestedvar9 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar9 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:coGridBuilder:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar9, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:coGridBuilder:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:coGridBuilder:Location = NEW System.Drawing.Point(122, 192).
        THIS-OBJECT:coGridBuilder:Name = "coGridBuilder":U.
        THIS-OBJECT:coGridBuilder:Size = NEW System.Drawing.Size(445, 21).
        THIS-OBJECT:coGridBuilder:TabIndex = 10.
        THIS-OBJECT:coGridBuilder:ValueChanged:Subscribe(THIS-OBJECT:tbOutputFileName_TextChanged).
        /*  */
        /* tbPageTitle */
        /*  */
        DEFINE VARIABLE nestedvar10 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar10 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbPageTitle:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar10, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbPageTitle:Location = NEW System.Drawing.Point(122, 104).
        THIS-OBJECT:tbPageTitle:Name = "tbPageTitle":U.
        THIS-OBJECT:tbPageTitle:Size = NEW System.Drawing.Size(445, 21).
        THIS-OBJECT:tbPageTitle:TabIndex = 9.
        THIS-OBJECT:tbPageTitle:TextChanged:Subscribe(THIS-OBJECT:tbOutputFileName_TextChanged).
        /*  */
        /* ultraLabel8 */
        /*  */
        THIS-OBJECT:ultraLabel8:Location = NEW System.Drawing.Point(16, 108).
        THIS-OBJECT:ultraLabel8:Name = "ultraLabel8":U.
        THIS-OBJECT:ultraLabel8:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel8:TabIndex = 8.
        THIS-OBJECT:ultraLabel8:Text = "Page Title:":U.
        /*  */
        /* tbOutputFileName */
        /*  */
        DEFINE VARIABLE nestedvar11 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar11 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbOutputFileName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar11, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:tbOutputFileName:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tbOutputFileName:Location = NEW System.Drawing.Point(122, 77).
        THIS-OBJECT:tbOutputFileName:Name = "tbOutputFileName":U.
        THIS-OBJECT:tbOutputFileName:Size = NEW System.Drawing.Size(445, 21).
        THIS-OBJECT:tbOutputFileName:TabIndex = 2.
        THIS-OBJECT:tbOutputFileName:EditorButtonClick:Subscribe(THIS-OBJECT:tbOutputFileName_EditorButtonClick).
        THIS-OBJECT:tbOutputFileName:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        THIS-OBJECT:tbOutputFileName:TextChanged:Subscribe(THIS-OBJECT:tbOutputFileName_TextChanged).
        /*  */
        /* ultraLabel4 */
        /*  */
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(16, 82).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel4:TabIndex = 0.
        THIS-OBJECT:ultraLabel4:Text = "Output File Name:":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        DEFINE VARIABLE nestedvar12 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar12 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar12, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance3.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(564, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 2.
        THIS-OBJECT:ultraLabel1:Text = "Please select those tables of the BindingSource that should be used for the Viewer:":U.
        /*  */
        /* ultraTabControl1 */
        /*  */
        DEFINE VARIABLE nestedvar13 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar13 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar13, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl2).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl3).
        THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(570, 366).
        THIS-OBJECT:ultraTabControl1:Style = Infragistics.Win.UltraWinTabControl.UltraTabControlStyle:Wizard.
        THIS-OBJECT:ultraTabControl1:TabIndex = 0.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        ultraTab1:Text = "tab1":U.
        ultraTab2:TabPage = THIS-OBJECT:ultraTabPageControl2.
        ultraTab2:Text = "tab2":U.
        ultraTab3:TabPage = THIS-OBJECT:ultraTabPageControl3.
        ultraTab3:Text = "tab3":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 3 NO-UNDO.
        arrayvar0[1] = ultraTab1.
        arrayvar0[2] = ultraTab2.
        arrayvar0[3] = ultraTab3.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar0).
        THIS-OBJECT:ultraTabControl1:UseAppStyling = FALSE.
        THIS-OBJECT:ultraTabControl1:ViewStyle = Infragistics.Win.UltraWinTabControl.ViewStyle:Office2007.
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(570, 366).
        /*  */
        /* btnCancel */
        /*  */
        DEFINE VARIABLE nestedvar14 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar14 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar14, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(507, 384).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCancel:TabIndex = 1.
        THIS-OBJECT:btnCancel:Text = "&Close":U.
        /*  */
        /* btnBack */
        /*  */
        DEFINE VARIABLE nestedvar15 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar15 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnBack:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar15, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnBack:Location = NEW System.Drawing.Point(12, 384).
        THIS-OBJECT:btnBack:Name = "btnBack":U.
        THIS-OBJECT:btnBack:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnBack:TabIndex = 2.
        THIS-OBJECT:btnBack:Text = "< &Back":U.
        THIS-OBJECT:btnBack:Click:Subscribe(THIS-OBJECT:btnBack_Click).
        /*  */
        /* btnNext */
        /*  */
        DEFINE VARIABLE nestedvar16 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar16 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnNext:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar16, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnNext:Location = NEW System.Drawing.Point(93, 384).
        THIS-OBJECT:btnNext:Name = "btnNext":U.
        THIS-OBJECT:btnNext:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnNext:TabIndex = 3.
        THIS-OBJECT:btnNext:Text = "&Next >":U.
        THIS-OBJECT:btnNext:Click:Subscribe(THIS-OBJECT:btnNext_Click).
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = ?.
        /*  */
        /* btnFinish */
        /*  */
        DEFINE VARIABLE nestedvar17 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar17 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnFinish:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar17, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnFinish:Location = NEW System.Drawing.Point(174, 384).
        THIS-OBJECT:btnFinish:Name = "btnFinish":U.
        THIS-OBJECT:btnFinish:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnFinish:TabIndex = 4.
        THIS-OBJECT:btnFinish:Text = "&Finish":U.
        THIS-OBJECT:btnFinish:Click:Subscribe(THIS-OBJECT:btnFinish_Click).
        /*  */
        /* saveFileDialog1 */
        /*  */
        THIS-OBJECT:saveFileDialog1:DefaultExt = "html":U.
        THIS-OBJECT:saveFileDialog1:Filter = "HTML Files (*.html)|*.html|All Files (*.*)|*.*":U.
        THIS-OBJECT:saveFileDialog1:RestoreDirectory = TRUE.
        /*  */
        /* KendoUiGridWizard */
        /*  */
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnCancel.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(594, 419).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnFinish).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnNext).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnBack).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "KendoUiGridWizard":U.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
        THIS-OBJECT:Text = "Kendo UI Grid Page Wizard":U.
        THIS-OBJECT:WindowPositionRegistryKey = "KendoUiGridWizard":U.
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:lvViewerFields, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:lvBusinessEntityFields, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl3:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl3:PerformLayout().
        CAST(THIS-OBJECT:tbDataSourceName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:coPageTemplate, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:coGridBuilder, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbPageTitle, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbOutputFileName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown handler of the Text boxes with an
                 EditorButton (Lookup)
        Notes:   Maps F3 to EditorButtonClick
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID LookupKeyDownHandler (sender AS System.Object,
                                              e AS System.Windows.Forms.KeyEventArgs):

        DEFINE VARIABLE cControlName AS CHARACTER NO-UNDO.

        IF e:Handled = FALSE AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, SmartLookupSettings:ShortcutKey)
            AND THIS-OBJECT:Enabled THEN DO:

            ASSIGN cControlName = CAST (sender, System.Windows.Forms.Control):Name .

            DYNAMIC-INVOKE (THIS-OBJECT,
                            SUBSTITUTE ("&1_EditorButtonClick":U, cControlName),
                            sender,
                            NEW EditorButtonEventArgs (?, ?)) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes a Page
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializePage ():

        THIS-OBJECT:ultraTabControl1:SelectedTab = THIS-OBJECT:ultraTabControl1:Tabs[iPage - 1] .

        ASSIGN btnBack:Enabled = (iPage <> 1)
               btnNext:Enabled = (iPage <> 3) .

        IF iPage = 2 THEN DO:
            THIS-OBJECT:InitializeBindingSourceFields () .

            btnNext:Enabled = (lvViewerFields:Items:Count > 0) .
        END .

        IF iPage = 3 THEN DO:

            IF tbPageTitle:Text > "":U THEN .
            ELSE
                tbPageTitle:Text = THIS-OBJECT:InitialFormTitle () .

            btnFinish:Enabled = THIS-OBJECT:CheckFinish () .
        END.
        ELSE
            btnFinish:Enabled = FALSE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds the initial/default Form Title
        Notes:   Text Property of the generater Form
        @return The initial/detault Form Title
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER InitialFormTitle ():

        DEFINE VARIABLE cName AS CHARACTER NO-UNDO.

        ASSIGN cName = StringHelper:WellFormFromCamelCase (smartBusinessEntityBindingSource1:EntityTable) + " Maintenance":U .

        IF cName BEGINS BusinessEntityDesignerSettings:DefaultTablePrefix THEN
            cName = SUBSTRING (cName, LENGTH (BusinessEntityDesignerSettings:DefaultTablePrefix, "CHARACTER":U) + 1, -1, "CHARACTER":U) .

        RETURN TRIM (cName) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ItemDoubleClick event of the lvBusinessEntityFields
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ItemDoubleClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID lvBusinessEntityFields_ItemDoubleClick (sender AS System.Object,
                                                                e AS Infragistics.Win.UltraWinListView.ItemDoubleClickEventArgs):

        IF lvBusinessEntityFields:SelectedItems:Count > 0 THEN
            THIS-OBJECT:btnAdd_Click (btnAdd, System.EventArgs:Empty) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ItemDoubleClick event of the lvViewerFields
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ItemDoubleClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID lvViewerFields_ItemDoubleClick (INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinListView.ItemDoubleClickEventArgs):

        IF lvViewerFields:SelectedItems:Count > 0 THEN
            THIS-OBJECT:btnRemove_Click (btnAdd, System.EventArgs:Empty) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for the Shown event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        DEFINE VARIABLE cClassName       AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE oException       AS Exception               NO-UNDO .
        DEFINE VARIABLE oBuilderProvider AS IKendoUiBuilderProvider NO-UNDO .

        SUPER:OnShown (e) .

        IF Consultingwerk.CharacterType:IsNotNullOrEmpty (THIS-OBJECT:BusinessEntityPackage) THEN
            ASSIGN cClassName = SUBSTITUTE ("&1.&2":U,
                                            THIS-OBJECT:BusinessEntityPackage,
                                            THIS-OBJECT:BusinessEntityName) .
        ELSE
            ASSIGN cClassName = THIS-OBJECT:BusinessEntityName .

        DO ON ERROR UNDO, THROW:
            Consultingwerk.OERA.ServiceManager:StopAllBusinessServices() .
            Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:FetchDataset ("":U,
                                                                                    cClassName,
                                                                                    OUTPUT DATASET-HANDLE hDataset) .
            CATCH beerr AS Progress.Lang.Error :
                oException = NEW Exception (beerr,
                                            "The Business Entity Schema cannot be loaded."{&TRAN},
                                            0) .

                Consultingwerk.Util.ErrorHelper:ShowErrorMessage (oException) .

                THIS-OBJECT:Close () .
                RETURN .
            END CATCH.

        END.

        THIS-OBJECT:InitializePage () .
        THIS-OBJECT:FillTreeViewWithTableNames () .

        /* Mike Fechner, Consultingwerk Ltd. 30.05.2013
           By default, select first node */
        IF THIS-OBJECT:treeView1:Nodes:Count > 0 THEN
            THIS-OBJECT:treeView1:Nodes[0]:Checked = TRUE .

        /* Mike Fechner, Consultingwerk Ltd. 04.05.2015
           Fetch List of Templates and Builders */
        ASSIGN oBuilderProvider = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.KendoUI.IKendoUiBuilderProvider
                                                                "NEW KendoUiBuilderProvider ()"} .

        coGridBuilder:ValueList  = ValueListHelper:FromList (oBuilderProvider:GetAvailableGridBuilders ()) .

        IF coGridBuilder:ValueList:ValueListItems:Count > 0 THEN
            coGridBuilder:SelectedIndex = 0 .

        coPageTemplate:ValueList = ValueListHelper:FromList (oBuilderProvider:GetAvailableTemplates ()) .

        IF coPageTemplate:ValueList:ValueListItems:Count > 0 THEN
            coPageTemplate:SelectedIndex = 0 .

        /* Mike Fechner, Consultingwerk Ltd. 04.05.2015
           Data Source name */
        IF THIS-OBJECT:UseGenericInterface THEN
            THIS-OBJECT:tbDataSourceName:Text = SUBSTITUTE ("&1.&2":U,
                                                            THIS-OBJECT:BusinessEntityPackage,
                                                            THIS-OBJECT:BusinessEntityName) .
        ELSE DO:
            IF THIS-OBJECT:BusinessEntityName MATCHES "*BusinessEntity":U THEN
                ASSIGN THIS-OBJECT:tbDataSourceName:Text = SUBSTRING (THIS-OBJECT:BusinessEntityName,
                                                                      1,
                                                                      LENGTH (THIS-OBJECT:BusinessEntityName, "CHARACTER":U) - 14,
                                                                      "CHARACTER":U) .
            ELSE IF THIS-OBJECT:BusinessEntityName MATCHES "*Entity":U THEN
                ASSIGN THIS-OBJECT:tbDataSourceName:Text  = SUBSTRING (THIS-OBJECT:BusinessEntityName,
                                                                       1,
                                                                       LENGTH (THIS-OBJECT:BusinessEntityName, "CHARACTER":U) - 6,
                                                                       "CHARACTER":U) .
            ELSE
                ASSIGN THIS-OBJECT:tbDataSourceName:Text = THIS-OBJECT:BusinessEntityName .
        END.

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .

            THIS-OBJECT:Close () .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the first selected table to display in DataView
        Notes:
        @param oNodeCollection The collection of tree nodes
        @return The first selected TreeNode
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE TreeNode RecurseTreeNodes (oNodeCollection AS TreeNodeCollection):

        DEFINE VARIABLE i            AS INTEGER  NO-UNDO.
        DEFINE VARIABLE oChildResult AS TreeNode NO-UNDO.

        IF oNodeCollection:COUNT > 0 THEN
            DO i = 0 TO oNodeCollection:Count - 1:
                IF oNodeCollection:Item[i]:Checked THEN
                    RETURN oNodeCollection:Item[i].
                ELSE DO:
                    oChildResult = RecurseTreeNodes (oNodeCollection:Item[i]:Nodes).
                    IF VALID-OBJECT (oChildResult) THEN
                        RETURN oChildResult.
                END.
        END.
        RETURN ?.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Builds a list of all selected child tables from the given selected
                 node
        Notes:
        @param oNodeCollection The collection of tree nodes
        @return The comma delimited list of selected child tables of in the nodes collection
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER RecurseTreeNodeNames (oNodeCollection AS TreeNodeCollection):

        DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cEntityList AS CHARACTER NO-UNDO.

        DO i = 0 TO oNodeCollection:Count - 1:
            IF oNodeCollection:Item[i]:Checked THEN
                cEntityList = TRIM (cEntityList + ",":U + oNodeCollection:Item[i]:Text, ",":U).

            cEntityList = TRIM (cEntityList + ",":U + RecurseTreeNodeNames (oNodeCollection:Item[i]:Nodes), ",":U).
        END.

        RETURN cEntityList.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the tbOutputFileName
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbOutputFileName_EditorButtonClick (sender AS System.Object,
                                                         e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .

        IF THIS-OBJECT:tbOutputFileName:Text > "":U THEN
            saveFileDialog1:FileName = THIS-OBJECT:tbOutputFileName:Text .

        WAIT-FOR saveFileDialog1:ShowDialog (THIS-OBJECT) SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        THIS-OBJECT:tbOutputFileName:Text = saveFileDialog1:FileName .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the tbOutputFileName
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbOutputFileName_TextChanged (INPUT sender AS System.Object, INPUT e AS System.EventArgs):

        IF iPage <> 3 THEN
            RETURN .

        btnFinish:Enabled = THIS-OBJECT:CheckFinish() .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeforeCheck event of the treeView1
        Notes:   Implements single checked
        @param sender The reference to the object that raised the event
        @param e The TreeViewCancelEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID treeView1_BeforeCheck (sender AS System.Object,
                                               e AS System.Windows.Forms.TreeViewCancelEventArgs):

        THIS-OBJECT:treeView1:BeforeCheck:Unsubscribe(THIS-OBJECT:treeView1_BeforeCheck).

        THIS-OBJECT:UncheckNodes (treeView1:Nodes) .

        FINALLY:
            THIS-OBJECT:treeView1:BeforeCheck:Subscribe (THIS-OBJECT:treeView1_BeforeCheck).
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Unchecks the treenodes in the collection
        Notes:
        @param poNodes The TreeNodeCollection to uncheck nodes in
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UncheckNodes (poNodes AS TreeNodeCollection):

        {Consultingwerk/foreach.i TreeNode oNode in poNodes}

            oNode:Checked = FALSE .

            IF oNode:Nodes:Count > 0 THEN
                THIS-OBJECT:UncheckNodes (oNode:Nodes) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns true when the Column Name exists in the Column of the BindingSource Schema
        Notes:
        @param pcColumnName The name of the column to test for
        @return True when the Column Name exists in the Column of the BindingSource Schema
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ValidColumn (pcColumnName AS CHARACTER):

        {Consultingwerk/foreach.i Progress.Data.ColumnPropDesc oColumn in smartBusinessEntityBindingSource1:TableSchema:Columns}
            IF oColumn:ColumnName = pcColumnName THEN
                RETURN TRUE .
        END .

        RETURN FALSE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartWindowWizard class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC KendoUiGridWizard ():

        GarbageCollectorHelper:DeleteObject (hDataset) .

    END DESTRUCTOR .

END CLASS.
