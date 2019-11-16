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
    File        : SmartViewerWizard
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 20 23:16:18 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*                                          FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.*                              FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.*             FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.SmartViewerGenerator.* FROM PROPATH .
USING Consultingwerk.Forms.*                                               FROM PROPATH .
USING Consultingwerk.Framework.*                                           FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                               FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                                FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*                             FROM PROPATH .
USING Consultingwerk.Util.*                                                FROM PROPATH .
USING Consultingwerk.Windows.*                                             FROM PROPATH.
USING Consultingwerk.Windows.Controls.*                                    FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*                                  FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*                                   FROM ASSEMBLY .
USING Progress.Lang.*                                                      FROM PROPATH .
USING System.Windows.Forms.*                                               FROM ASSEMBLY .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.SmartViewerGenerator.SmartViewerWizard
    INHERITS BaseForm
    USE-WIDGET-POOL:

    DEFINE PRIVATE VARIABLE btnAdd AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnBack AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnFinish AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnNext AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnRemove AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE lvViewerFields AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE lvBusinessEntityFields AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE tbViewerPackage AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbViewerName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE treeView1 AS System.Windows.Forms.TreeView NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl3 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.

    DEFINE VARIABLE iPage    AS INTEGER NO-UNDO INIT 1.
    DEFINE VARIABLE hDataset AS HANDLE  NO-UNDO .

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }

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
        Purpose: Last used package name in the current ABL session
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY SavedPackageName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartViewerWizard class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartViewerWizard ():

        DEFINE VARIABLE oImage AS System.Drawing.Image NO-UNDO .

        SUPER().

        InitializeComponent().

        THIS-OBJECT:StoreFormPosition = FALSE .

        /* Mike Fechner, Consultingwerk Ltd. 05.06.2013
           Restore package from previous use */
        tbViewerPackage:Text = SmartViewerWizard:SavedPackageName .

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
        @param sender The object that raised the event
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
        @param sender The object that raised the event
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
        @param sender The object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnFinish_Click (sender AS System.Object,
                                         e AS System.EventArgs):

        THIS-OBJECT:GenerateViewer () .

        /* Mike Fechner, Consultingwerk Ltd. 05.06.2013
           Remember package for later use */
        SmartViewerWizard:SavedPackageName = tbViewerPackage:Text .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnNext
        Notes:
        @param sender The object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnNext_Click (INPUT sender AS System.Object, INPUT e AS System.EventArgs):

        ASSIGN iPage = iPage + 1 .

        THIS-OBJECT:InitializePage () .

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnRemove
        Notes:
        @param sender The object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnRemove_Click (sender AS System.Object,
                                         e AS System.EventArgs):

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
        Purpose: Generates the Viewer Control
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GenerateViewer ():

        DEFINE VARIABLE oBuilder    AS SmartViewerBuilder    NO-UNDO .
        DEFINE VARIABLE cFileName   AS CHARACTER             NO-UNDO .
        DEFINE VARIABLE oList       AS CharacterList         NO-UNDO .
        DEFINE VARIABLE oDictionary AS CharacterDictionary   NO-UNDO .

        DEFINE BUFFER b_eField FOR eField .

        oBuilder = NEW SmartViewerBuilder () .

        oBuilder:BindingSource = smartBusinessEntityBindingSource1 .
        oBuilder:ClassName = tbViewerName:Text .
        oBuilder:EntityJoin = smartBusinessEntityBindingSource1:EntityJoin .
        oBuilder:EntityName = smartBusinessEntityBindingSource1:EntityName .
        oBuilder:EntityTable = smartBusinessEntityBindingSource1:EntityTable .
        oBuilder:EntityView = smartBusinessEntityBindingSource1:EntityView .
        oBuilder:PackageName = tbViewerPackage:Text .

        smartBusinessEntityBindingSource1:Handle = SmartBusinessEntityQuerySupport:PrepareBindingQuery (smartBusinessEntityBindingSource1:EntityName,
                                                                                                        smartBusinessEntityBindingSource1:EntityTable,
                                                                                                        smartBusinessEntityBindingSource1:EntityView,
                                                                                                        smartBusinessEntityBindingSource1:EntityJoin) .

        oList = NEW CharacterList () .
        oDictionary = NEW CharacterDictionary () .

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in lvViewerFields:Items}
            oList:Add (oItem:Key) .
        END.

        FOR EACH b_eField WHERE (b_eField.TempTableName = smartBusinessEntityBindingSource1:EntityTable
                             OR LOOKUP (b_eField.TempTableName, smartBusinessEntityBindingSource1:EntityView) > 0)
                            AND b_eField.FieldCustomControlName > "":U ON ERROR UNDO, THROW:

            oDictionary:Add (SUBSTITUTE ("&1.&2":U, b_eField.TempTableName, b_eField.FieldName),
                             b_eField.FieldCustomControlName) .
        END.

        cFileName = oBuilder:Generate (oList, oDictionary) .

        IF cFileName > "":U THEN DO:
            IF BusinessEntityDesignerSettings:OpenFilesInIDE THEN
                THIS-OBJECT:OpenFilesInIDE (cFileName) .

            THIS-OBJECT:Close () .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Binds to the Business Entity Designer Dataset
        Notes:
        @param dsBusinessEntity The dataset to bind to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetDatasetHandle (DATASET dsBusinessEntity BIND):

        /* Noop */

    END METHOD.

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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.Plugins.SmartViewerGenerator.SmartViewerWizard":U).
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
        THIS-OBJECT:tbViewerName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbViewerPackage = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:btnCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnBack = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnNext = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:btnFinish = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        THIS-OBJECT:ultraTabPageControl2:SuspendLayout().
        CAST(THIS-OBJECT:lvViewerFields, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:lvBusinessEntityFields, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl3:SuspendLayout().
        CAST(THIS-OBJECT:tbViewerName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbViewerPackage, System.ComponentModel.ISupportInitialize):BeginInit().
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
        THIS-OBJECT:ultraLabel3:Text = "Please select those tables of the BindingSource that should be used for the Viewer:":U.
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
        /*  */
        /* ultraTabPageControl2 */
        /*  */
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:btnRemove).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:btnAdd).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:lvViewerFields).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:lvBusinessEntityFields).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:ultraTabPageControl2:Location = NEW System.Drawing.Point(0, 0).
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
        THIS-OBJECT:ultraLabel2:Text = "Please select those fields of the BindingSource that should be used for the Viewer:":U.
        /*  */
        /* ultraTabPageControl3 */
        /*  */
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:tbViewerName).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:tbViewerPackage).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:ultraTabPageControl3:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabPageControl3:Name = "ultraTabPageControl3":U.
        THIS-OBJECT:ultraTabPageControl3:Size = NEW System.Drawing.Size(570, 366).
        /*  */
        /* tbViewerName */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbViewerName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbViewerName:Location = NEW System.Drawing.Point(122, 107).
        THIS-OBJECT:tbViewerName:Name = "tbViewerName":U.
        THIS-OBJECT:tbViewerName:Size = NEW System.Drawing.Size(445, 21).
        THIS-OBJECT:tbViewerName:TabIndex = 3.
        THIS-OBJECT:tbViewerName:TextChanged:Subscribe(THIS-OBJECT:tbViewerPackage_TextChanged).
        /*  */
        /* tbViewerPackage */
        /*  */
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar8 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbViewerPackage:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:tbViewerPackage:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tbViewerPackage:Location = NEW System.Drawing.Point(122, 77).
        THIS-OBJECT:tbViewerPackage:Name = "tbViewerPackage":U.
        THIS-OBJECT:tbViewerPackage:Size = NEW System.Drawing.Size(445, 21).
        THIS-OBJECT:tbViewerPackage:TabIndex = 2.
        THIS-OBJECT:tbViewerPackage:EditorButtonClick:Subscribe(THIS-OBJECT:tbViewerPackage_EditorButtonClick).
        THIS-OBJECT:tbViewerPackage:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        THIS-OBJECT:tbViewerPackage:TextChanged:Subscribe(THIS-OBJECT:tbViewerPackage_TextChanged).
        /*  */
        /* ultraLabel5 */
        /*  */
        THIS-OBJECT:ultraLabel5:Location = NEW System.Drawing.Point(16, 111).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        THIS-OBJECT:ultraLabel5:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel5:TabIndex = 1.
        THIS-OBJECT:ultraLabel5:Text = "Viewer Name:":U.
        /*  */
        /* ultraLabel4 */
        /*  */
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(16, 82).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel4:TabIndex = 0.
        THIS-OBJECT:ultraLabel4:Text = "Package:":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        DEFINE VARIABLE nestedvar9 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar9 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar9, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
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
        DEFINE VARIABLE nestedvar10 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar10 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar10, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
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
        DEFINE VARIABLE nestedvar11 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar11 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar11, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(507, 384).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCancel:TabIndex = 1.
        THIS-OBJECT:btnCancel:Text = "&Cancel":U.
        /*  */
        /* btnBack */
        /*  */
        DEFINE VARIABLE nestedvar12 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar12 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnBack:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar12, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnBack:Location = NEW System.Drawing.Point(12, 384).
        THIS-OBJECT:btnBack:Name = "btnBack":U.
        THIS-OBJECT:btnBack:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnBack:TabIndex = 2.
        THIS-OBJECT:btnBack:Text = "< &Back":U.
        THIS-OBJECT:btnBack:Click:Subscribe(THIS-OBJECT:btnBack_Click).
        /*  */
        /* btnNext */
        /*  */
        DEFINE VARIABLE nestedvar13 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar13 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnNext:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar13, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnNext:Location = NEW System.Drawing.Point(93, 384).
        THIS-OBJECT:btnNext:Name = "btnNext":U.
        THIS-OBJECT:btnNext:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnNext:TabIndex = 3.
        THIS-OBJECT:btnNext:Text = "&Next >":U.
        THIS-OBJECT:btnNext:Click:Subscribe(THIS-OBJECT:btnNext_Click).
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:AddFields = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:ClearSchema = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:ImportSchema = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = ?.
        /*  */
        /* btnFinish */
        /*  */
        DEFINE VARIABLE nestedvar14 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar14 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnFinish:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar14, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnFinish:Location = NEW System.Drawing.Point(174, 384).
        THIS-OBJECT:btnFinish:Name = "btnFinish":U.
        THIS-OBJECT:btnFinish:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnFinish:TabIndex = 4.
        THIS-OBJECT:btnFinish:Text = "&Finish":U.
        THIS-OBJECT:btnFinish:Click:Subscribe(THIS-OBJECT:btnFinish_Click).
        /*  */
        /* SmartViewerWizard */
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
        THIS-OBJECT:Name = "SmartViewerWizard":U.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
        THIS-OBJECT:Text = "SmartViewerControl Wizard":U.
        THIS-OBJECT:WindowPositionRegistryKey = "SmartViewerWizard":U.
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:lvViewerFields, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:lvBusinessEntityFields, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl3:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl3:PerformLayout().
        CAST(THIS-OBJECT:tbViewerName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbViewerPackage, System.ComponentModel.ISupportInitialize):EndInit().
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

        DEFINE VARIABLE lFinish AS LOGICAL NO-UNDO.

        THIS-OBJECT:ultraTabControl1:SelectedTab = THIS-OBJECT:ultraTabControl1:Tabs[iPage - 1] .

        ASSIGN btnBack:Enabled = (iPage <> 1)
               btnNext:Enabled = (iPage <> 3) .

        IF iPage = 2 THEN DO:
            THIS-OBJECT:InitializeBindingSourceFields () .

            btnNext:Enabled = (lvViewerFields:Items:Count > 0) .
        END .

        IF iPage = 3 THEN DO:

            IF tbViewerName:Text > "":U THEN .
            ELSE
                tbViewerName:Text = THIS-OBJECT:InitialViewerName () .

            lFinish = (tbViewerName:Text > "":U AND
                       tbViewerPackage:Text > "":U) .

            btnFinish:Enabled = lFinish .
        END.
        ELSE
            btnFinish:Enabled = FALSE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds the initial/default Viewer Name
        Notes:
        @return The initial/detault Viewer Name
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER InitialViewerName ():

        DEFINE VARIABLE cName AS CHARACTER NO-UNDO.

        ASSIGN cName = DesignerHelper:DesignerSafeName (smartBusinessEntityBindingSource1:EntityTable + "ViewerControl":U) .

        IF cName BEGINS BusinessEntityDesignerSettings:DefaultTablePrefix THEN
            cName = SUBSTRING (cName, LENGTH (BusinessEntityDesignerSettings:DefaultTablePrefix, "CHARACTER":U) + 1, -1, "CHARACTER":U) .

        RETURN cName .

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

        DEFINE VARIABLE cClassName AS CHARACTER NO-UNDO .
        DEFINE VARIABLE oException AS Exception NO-UNDO .

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

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .

            THIS-OBJECT:Close () .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens the generated files in the IDE
        Notes:
        @param pcFileNames The list of file names
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OpenFilesInIDE (pcFileNames AS CHARACTER):

        DEFINE VARIABLE cCurrentPath  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cIDEConfig    AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cProject      AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iPortNumber   AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cFileName     AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cMessageTitle AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i             AS INTEGER   NO-UNDO .

        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:SetWaitState() .

        FILE-INFORMATION:FILE-NAME = ".":U .

        ASSIGN cMessageTitle = "Opening of generated files in {&ProgressIDE} failed"{&TRAN}
               cCurrentPath  = FILE-INFORMATION:FULL-PATHNAME
               cIDEConfig    = Consultingwerk.Framework.Registry:GetRegistryValue
                                        ("USER":U,
                                         "Software~\Consultingwerk Ltd.~\IDE Support":U,
                                         cCurrentPath) .

        IF cIDEConfig > "":U THEN .
        ELSE DO:
            Consultingwerk.Util.MessageFormHelper:ShowMessage ("Unable to read the IDE configuration from the registry"{&TRAN},
                                                               cMessageTitle,
                                                               MessageFormImages:ImageWarning) .
            RETURN .
        END.

        ASSIGN cProject    = ENTRY (1, cIDEConfig, " ":U)
               iPortNumber = INTEGER (ENTRY (2, cIDEConfig, " ":U))
               NO-ERROR .

        IF iPortNumber > 100 THEN .
        ELSE DO:
            Consultingwerk.Util.MessageFormHelper:ShowMessage (SUBSTITUTE ("Invalid IDE configuration: &1"{&TRAN}, cIDEConfig),
                                                               cMessageTitle,
                                                               MessageFormImages:ImageWarning) .
            RETURN .
        END.

        DO i = NUM-ENTRIES (pcFileNames) TO 1 BY -1 ON ERROR UNDO, THROW:
            ASSIGN cFileName = ENTRY (i, pcFileNames) .

            Consultingwerk.Util.OpenEdgeArchitectHelper:SendRequest
                     ("IDE openEditor ":U
                      + QUOTER(cProject) + " ":U
                      + QUOTER(cFileName) + " ":U
                      + QUOTER("":U), iPortNumber) .

            CATCH err AS Progress.Lang.Error :
                Consultingwerk.Util.MessageFormHelper:ShowMessage (SUBSTITUTE ("&1&2&3":U,
                                                                               err:GetMessage(1),
                                                                               System.Environment:NewLine,
                                                                               cIDEConfig),
                                                                   cMessageTitle,
                                                                   MessageFormImages:ImageError) .
                RETURN .
            END CATCH.
        END.

        FINALLY:
            IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
                FrameworkSettings:WaitStateManager:ClearWaitState() .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the first selected table to display in DataView
        Notes:
        @param oNodeCollection The collection of tree nodes
        @return The first selected TreeNode
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE TreeNode RecurseTreeNodes (oNodeCollection  AS TreeNodeCollection):

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
        Purpose: Event handler for the EditorButtonClick event of the tbViewerPackage
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbViewerPackage_EditorButtonClick (sender AS System.Object,
                                                           e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .
        DEFINE VARIABLE cRootFolder   AS CHARACTER                         NO-UNDO .

        DEFINE VARIABLE oFolderBrowserDialog AS Consultingwerk.Windows.Util.Forms.FolderBrowserForm NO-UNDO .

        IF BusinessEntityDesignerSettings:SourceCodeBaseFolderGui > "":U THEN DO:
            FILE-INFORMATION:FILE-NAME = BusinessEntityDesignerSettings:SourceCodeBaseFolderGui .

            IF FILE-INFORMATION:FULL-PATHNAME = ? THEN
                FILE-INFORMATION:FILE-NAME = ".":U .
        END.
        ELSE
            FILE-INFORMATION:FILE-NAME = ".":U .

        oFolderBrowserDialog = NEW Consultingwerk.Windows.Util.Forms.FolderBrowserForm () .

        ASSIGN cRootFolder                     = FILE-INFORMATION:FULL-PATHNAME
               oFolderBrowserDialog:RootFolder = cRootFolder .

        WAIT-FOR oFolderBrowserDialog:ShowDialog() SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        IF oFolderBrowserDialog:SelectedFolder BEGINS cRootFolder THEN DO:
            THIS-OBJECT:tbViewerPackage:Text =
              REPLACE (SUBSTRING (oFolderBrowserDialog:SelectedFolder, LENGTH (cRootFolder, "CHARACTER":U) + 2, -1, "CHARACTER":U), "~\":U, ".":U) .
        END.
        ELSE
            Consultingwerk.Util.MessageFormHelper:ShowMessage
                          ("Unable to translate path into package."{&TRAN},
                           "Business Entity Designer"{&TRAN},
                           "Path needs to be relative to working directory."{&TRAN},
                           MessageFormImages:ImageWarning) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the tbViewerPackage
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbViewerPackage_TextChanged (sender AS System.Object,
                                                     e AS System.EventArgs):

        IF iPage <> 3 THEN
            RETURN .

        btnFinish:Enabled = (tbViewerName:Text > "":U AND
                             tbViewerPackage:Text > "":U) .

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
        Purpose: Destructor for the SmartViewerWizard class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartViewerWizard ():

        GarbageCollectorHelper:DeleteObject (hDataset) .

    END DESTRUCTOR .

END CLASS.
