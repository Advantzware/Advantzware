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
    File        : OpenAccessSdkWizard
    Purpose     : Wizard for generating OpenAccess interface procedures
                  and JSON Catalog files
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 03 20:42:03 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*                                   FROM PROPATH .
USING Consultingwerk.Forms.*                                        FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.OpenAccessSdk.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                         FROM PROPATH .
USING Consultingwerk.Windows.*                                      FROM PROPATH .   
USING Consultingwerk.Windows.Util.Forms.*                           FROM PROPATH .
USING Consultingwerk.Util.*                                         FROM PROPATH.
USING Infragistics.Win.UltraWinEditors.*                            FROM ASSEMBLY.
USING Progress.Lang.*                                               FROM PROPATH .
USING System.Windows.Forms.*                                        FROM ASSEMBLY.

{Consultingwerk/products.i}

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.OpenAccessSdk.OpenAccessSdkWizard 
    INHERITS BaseForm
    USE-WIDGET-POOL: 

    DEFINE PRIVATE VARIABLE btnOk AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE cbStopAfter AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE cbBatchSize AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE folderBrowserDialog1 AS System.Windows.Forms.FolderBrowserDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbRelativeFolder AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbInterfaceName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbCatalogFolder AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbAppServerFolder AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tgOuterJoin AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE treeView1 AS System.Windows.Forms.TreeView NO-UNDO.

    DEFINE VARIABLE hDataset AS HANDLE  NO-UNDO .
    DEFINE PRIVATE VARIABLE ultraLabel6 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

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
        Purpose: Constructor for the OpenAccessSdkWizard class 
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC OpenAccessSdkWizard ():
		
        SUPER().
        
        InitializeComponent().
        
        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnOk 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID btnOk_Click (sender AS System.Object, 
	                                 e AS System.EventArgs):
		
        DEFINE VARIABLE oGenerator   AS OpenAccessSdkGenerator NO-UNDO . 
        DEFINE VARIABLE cEntityTable AS CHARACTER              NO-UNDO.
        DEFINE VARIABLE cEntityView  AS CHARACTER              NO-UNDO.
		
        DEFINE VARIABLE iBatchSize   AS INTEGER                NO-UNDO.
        DEFINE VARIABLE iStopAfter   AS INTEGER                NO-UNDO.
		
		DEFINE BUFFER bEntity FOR eBusinessEntity .          
		
		ASSIGN cEntityTable = THIS-OBJECT:GetSelectedTopBuffer (cEntityView) .
		
		FIND FIRST bEntity . 
		
		oGenerator = NEW OpenAccessSdkGenerator () . 
		
		ASSIGN iBatchSize = INTEGER (UNBOX(cbBatchSize:Value)) 
		       iStopAfter = INTEGER (UNBOX(cbStopAfter:Value)) . 
		
		oGenerator:Generate (TRIM (SUBSTITUTE ("&1.&2":U,
		                                       THIS-OBJECT:BusinessEntityPackage,
		                                       THIS-OBJECT:BusinessEntityName), ".":U),
		                     cEntityTable,
		                     cEntityView,
		                     tbInterfaceName:Text,
		                     tbAppServerFolder:Text,
		                     tbRelativeFolder:Text,
		                     tbCatalogFolder:Text,
		                     iBatchSize,
		                     iStopAfter,
		                     tgOuterJoin:Checked,
		                     hDataset,
		                     SUBSTITUTE ("&1/&2.i":U,
		                                 bEntity.DatasetPath,
		                                 bEntity.DatasetName)) .

        THIS-OBJECT:Close ().

        CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Walk though the Dataset TopBuffers and add all of them with their 
                 childs to the TreeView
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID FillTreeViewWithTableNames ():
        
        DEFINE VARIABLE oFactory AS TreeNodeFromDatasetStructureFactory NO-UNDO . 
        
        IF THIS-OBJECT:treeView1:Nodes:Count > 0 THEN RETURN . 
        
        oFactory = NEW TreeNodeFromDatasetStructureFactory () . 
        
        oFactory:CreateTreeNodes (THIS-OBJECT:treeView1,
                                  hDataset) .
                 
    END METHOD.
    
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
        Purpose: Initializes the Visual Design
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID InitializeComponent ():
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem6 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem6 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem7 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem7 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem8 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem8 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem9 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem9 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem10 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem10 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem11 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem11 = NEW Infragistics.Win.ValueListItem().
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
        DEFINE VARIABLE valueListItem4 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem4 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem5 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem5 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton3 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton3 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.Plugins.OpenAccessSdk.OpenAccessSdkWizard":U).
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:ultraLabel6 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:cbStopAfter = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:cbBatchSize = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:tgOuterJoin = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbRelativeFolder = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbAppServerFolder = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbCatalogFolder = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbInterfaceName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:treeView1 = NEW System.Windows.Forms.TreeView().
        THIS-OBJECT:btnOk = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:folderBrowserDialog1 = NEW System.Windows.Forms.FolderBrowserDialog().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:cbStopAfter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:cbBatchSize, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tgOuterJoin, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbRelativeFolder, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbAppServerFolder, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbCatalogFolder, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbInterfaceName, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel6).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:cbStopAfter).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:cbBatchSize).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tgOuterJoin).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbRelativeFolder).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbAppServerFolder).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbCatalogFolder).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbInterfaceName).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:treeView1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(652, 250).
        THIS-OBJECT:panel1:TabIndex = 0.
        /*  */
        /* ultraLabel6 */
        /*  */
        THIS-OBJECT:ultraLabel6:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel6:Location = NEW System.Drawing.Point(269, 88).
        THIS-OBJECT:ultraLabel6:Name = "ultraLabel6":U.
        THIS-OBJECT:ultraLabel6:Size = NEW System.Drawing.Size(79, 14).
        THIS-OBJECT:ultraLabel6:TabIndex = 6.
        THIS-OBJECT:ultraLabel6:Text = "STOP-AFTER:":U.
        /*  */
        /* ultraLabel5 */
        /*  */
        THIS-OBJECT:ultraLabel5:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel5:Location = NEW System.Drawing.Point(269, 60).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        THIS-OBJECT:ultraLabel5:Size = NEW System.Drawing.Size(144, 14).
        THIS-OBJECT:ultraLabel5:TabIndex = 4.
        THIS-OBJECT:ultraLabel5:Text = "Maximum Number of Rows:":U.
        /*  */
        /* cbStopAfter */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbStopAfter:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbStopAfter:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        valueListItem6:DataValue = "0":U.
        valueListItem6:DisplayText = "0 (off)":U.
        valueListItem7:DataValue = "1":U.
        valueListItem7:DisplayText = "1 second":U.
        valueListItem8:DataValue = "5":U.
        valueListItem8:DisplayText = "5 seconds":U.
        valueListItem9:DataValue = "15":U.
        valueListItem9:DisplayText = "15 seconds":U.
        valueListItem10:DataValue = "30":U.
        valueListItem10:DisplayText = "30 seconds":U.
        valueListItem11:DataValue = "60":U.
        valueListItem11:DisplayText = "1 minute":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.ValueListItem EXTENT 6 NO-UNDO.
        arrayvar0[1] = valueListItem6.
        arrayvar0[2] = valueListItem7.
        arrayvar0[3] = valueListItem8.
        arrayvar0[4] = valueListItem9.
        arrayvar0[5] = valueListItem10.
        arrayvar0[6] = valueListItem11.
        THIS-OBJECT:cbStopAfter:Items:AddRange(arrayvar0).
        THIS-OBJECT:cbStopAfter:Location = NEW System.Drawing.Point(452, 84).
        THIS-OBJECT:cbStopAfter:Margin = NEW System.Windows.Forms.Padding(3, 20, 3, 3).
        THIS-OBJECT:cbStopAfter:Name = "cbStopAfter":U.
        THIS-OBJECT:cbStopAfter:Size = NEW System.Drawing.Size(177, 21).
        THIS-OBJECT:cbStopAfter:TabIndex = 7.
        /*  */
        /* cbBatchSize */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbBatchSize:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbBatchSize:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        valueListItem1:DataValue = "0":U.
        valueListItem1:DisplayText = "0 (all rows)":U.
        valueListItem2:DataValue = "100":U.
        valueListItem2:DisplayText = "100":U.
        valueListItem3:DataValue = "200":U.
        valueListItem4:DataValue = "500":U.
        valueListItem4:DisplayText = "500":U.
        valueListItem5:DataValue = "1000":U.
        valueListItem5:DisplayText = "1000":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.ValueListItem EXTENT 5 NO-UNDO.
        arrayvar1[1] = valueListItem1.
        arrayvar1[2] = valueListItem2.
        arrayvar1[3] = valueListItem3.
        arrayvar1[4] = valueListItem4.
        arrayvar1[5] = valueListItem5.
        THIS-OBJECT:cbBatchSize:Items:AddRange(arrayvar1).
        THIS-OBJECT:cbBatchSize:Location = NEW System.Drawing.Point(452, 56).
        THIS-OBJECT:cbBatchSize:Margin = NEW System.Windows.Forms.Padding(3, 20, 3, 3).
        THIS-OBJECT:cbBatchSize:Name = "cbBatchSize":U.
        THIS-OBJECT:cbBatchSize:Size = NEW System.Drawing.Size(177, 21).
        THIS-OBJECT:cbBatchSize:TabIndex = 5.
        /*  */
        /* tgOuterJoin */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:tgOuterJoin:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tgOuterJoin:Location = NEW System.Drawing.Point(12, 217).
        THIS-OBJECT:tgOuterJoin:Name = "tgOuterJoin":U.
        THIS-OBJECT:tgOuterJoin:Size = NEW System.Drawing.Size(233, 20).
        THIS-OBJECT:tgOuterJoin:TabIndex = 1.
        THIS-OBJECT:tgOuterJoin:Text = "Outer join":U.
        /*  */
        /* ultraLabel4 */
        /*  */
        THIS-OBJECT:ultraLabel4:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(269, 203).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(177, 14).
        THIS-OBJECT:ultraLabel4:TabIndex = 12.
        THIS-OBJECT:ultraLabel4:Text = "Interface Procedure Relative Path:":U.
        /*  */
        /* ultraLabel3 */
        /*  */
        THIS-OBJECT:ultraLabel3:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(269, 176).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(120, 14).
        THIS-OBJECT:ultraLabel3:TabIndex = 10.
        THIS-OBJECT:ultraLabel3:Text = "AppServer Code Base:":U.
        /*  */
        /* tbRelativeFolder */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbRelativeFolder:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:tbRelativeFolder:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tbRelativeFolder:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tbRelativeFolder:Location = NEW System.Drawing.Point(452, 199).
        THIS-OBJECT:tbRelativeFolder:Name = "tbRelativeFolder":U.
        THIS-OBJECT:tbRelativeFolder:Size = NEW System.Drawing.Size(177, 21).
        THIS-OBJECT:tbRelativeFolder:TabIndex = 13.
        THIS-OBJECT:tbRelativeFolder:EditorButtonClick:Subscribe(THIS-OBJECT:tbRelativeFolder_EditorButtonClick).
        THIS-OBJECT:tbRelativeFolder:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* tbAppServerFolder */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbAppServerFolder:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton2:Text = "...":U.
        THIS-OBJECT:tbAppServerFolder:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:tbAppServerFolder:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tbAppServerFolder:Location = NEW System.Drawing.Point(452, 172).
        THIS-OBJECT:tbAppServerFolder:Margin = NEW System.Windows.Forms.Padding(3, 20, 3, 3).
        THIS-OBJECT:tbAppServerFolder:Name = "tbAppServerFolder":U.
        THIS-OBJECT:tbAppServerFolder:Size = NEW System.Drawing.Size(177, 21).
        THIS-OBJECT:tbAppServerFolder:TabIndex = 11.
        THIS-OBJECT:tbAppServerFolder:EditorButtonClick:Subscribe(THIS-OBJECT:tbCatalogFolder_EditorButtonClick).
        THIS-OBJECT:tbAppServerFolder:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(269, 132).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(115, 14).
        THIS-OBJECT:ultraLabel2:TabIndex = 8.
        THIS-OBJECT:ultraLabel2:Text = "JSON Catalog Folder:":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(269, 16).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(149, 14).
        THIS-OBJECT:ultraLabel1:TabIndex = 2.
        THIS-OBJECT:ultraLabel1:Text = "Interface Name (SQL Table):":U.
        /*  */
        /* tbCatalogFolder */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbCatalogFolder:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton3:Text = "...":U.
        THIS-OBJECT:tbCatalogFolder:ButtonsRight:Add(editorButton3).
        THIS-OBJECT:tbCatalogFolder:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tbCatalogFolder:Location = NEW System.Drawing.Point(452, 128).
        THIS-OBJECT:tbCatalogFolder:Margin = NEW System.Windows.Forms.Padding(3, 20, 3, 20).
        THIS-OBJECT:tbCatalogFolder:Name = "tbCatalogFolder":U.
        THIS-OBJECT:tbCatalogFolder:Size = NEW System.Drawing.Size(177, 21).
        THIS-OBJECT:tbCatalogFolder:TabIndex = 9.
        THIS-OBJECT:tbCatalogFolder:EditorButtonClick:Subscribe(THIS-OBJECT:tbCatalogFolder_EditorButtonClick).
        THIS-OBJECT:tbCatalogFolder:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* tbInterfaceName */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbInterfaceName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbInterfaceName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tbInterfaceName:Location = NEW System.Drawing.Point(452, 12).
        THIS-OBJECT:tbInterfaceName:Name = "tbInterfaceName":U.
        THIS-OBJECT:tbInterfaceName:Size = NEW System.Drawing.Size(177, 21).
        THIS-OBJECT:tbInterfaceName:TabIndex = 3.
        /*  */
        /* treeView1 */
        /*  */
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar8 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:treeView1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:treeView1:CheckBoxes = TRUE.
        THIS-OBJECT:treeView1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:treeView1:Name = "treeView1":U.
        THIS-OBJECT:treeView1:Size = NEW System.Drawing.Size(233, 199).
        THIS-OBJECT:treeView1:TabIndex = 0.
        THIS-OBJECT:treeView1:AfterCheck:Subscribe(THIS-OBJECT:treeView1_AfterCheck).
        /*  */
        /* btnOk */
        /*  */
        DEFINE VARIABLE nestedvar9 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar9 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnOk:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar9, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnOk:Location = NEW System.Drawing.Point(468, 256).
        THIS-OBJECT:btnOk:Name = "btnOk":U.
        THIS-OBJECT:btnOk:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnOk:TabIndex = 1.
        THIS-OBJECT:btnOk:Text = "&Generate":U.
        THIS-OBJECT:btnOk:Click:Subscribe(THIS-OBJECT:btnOk_Click).
        /*  */
        /* btnCancel */
        /*  */
        DEFINE VARIABLE nestedvar10 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar10 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar10, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(554, 256).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCancel:TabIndex = 2.
        THIS-OBJECT:btnCancel:Text = "&Cancel":U.
        /*  */
        /* folderBrowserDialog1 */
        /*  */
        THIS-OBJECT:folderBrowserDialog1:Description = "Select Folder":U.
        /*  */
        /* OpenAccessSdkWizard */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:btnOk.
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnCancel.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(649, 291).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnOk).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MinimumSize = NEW System.Drawing.Size(665, 330).
        THIS-OBJECT:Name = "OpenAccessSdkWizard":U.
        THIS-OBJECT:Text = "OpenAccess SDK Wizard":U.
        THIS-OBJECT:WindowPositionRegistryKey = "OpenAccessSdkWizard":U.
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        CAST(THIS-OBJECT:cbStopAfter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:cbBatchSize, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tgOuterJoin, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbRelativeFolder, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbAppServerFolder, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbCatalogFolder, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbInterfaceName, System.ComponentModel.ISupportInitialize):EndInit().
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

        THIS-OBJECT:FillTreeViewWithTableNames () . 
        
        THIS-OBJECT:cbBatchSize:Value  = THIS-OBJECT:cbBatchSize:ValueList:ValueListItems[0]:DataValue . 
        THIS-OBJECT:cbStopAfter:Value  = THIS-OBJECT:cbStopAfter:ValueList:ValueListItems[0]:DataValue . 

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
        Purpose: Restortes the Form Settings
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD OVERRIDE PROTECTED VOID RestoreWindowPosition ():

        DEFINE VARIABLE i AS INTEGER NO-UNDO.
		
		SUPER:RestoreWindowPosition().

        tbCatalogFolder:Text   = THIS-OBJECT:LoadSettings ("CatalogFolder":U) .
        tbAppServerFolder:Text = THIS-OBJECT:LoadSettings ("AppServerFolder":U) .
        tbRelativeFolder:Text  = THIS-OBJECT:LoadSettings ("RelativeFolder":U) .

        ASSIGN i = INTEGER (THIS-OBJECT:LoadSettings ("BatchSize":U)) NO-ERROR . 
        
        IF i > 0 THEN cbBatchSize:Value = BOX (i) .

        ASSIGN i = INTEGER (THIS-OBJECT:LoadSettings ("StopAfter":U)) NO-ERROR . 
        
        IF i > 0 THEN cbStopAfter:Value = BOX (i) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Stores Form settings in the Registry
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD OVERRIDE PROTECTED VOID StoreWindowPosition ():
		
		SUPER:StoreWindowPosition().

        THIS-OBJECT:StoreSettings (tbCatalogFolder:Text,"CatalogFolder":U) .
        THIS-OBJECT:StoreSettings (tbAppServerFolder:Text,"AppServerFolder":U) .
        THIS-OBJECT:StoreSettings (tbRelativeFolder:Text,"RelativeFolder":U) .

        THIS-OBJECT:StoreSettings (UNBOX (cbBatchSize:Value),"BatchSize":U) .
        THIS-OBJECT:StoreSettings (UNBOX (cbStopAfter:Value),"StopAfter":U) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the tbCatalogFolder
                 and tbAppServerFolder 
        Notes:   
        @param sender The reference to the control that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID tbCatalogFolder_EditorButtonClick (sender AS System.Object, 
	                                                       e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
		
		DEFINE VARIABLE oControl      AS UltraTextEditor NO-UNDO . 
		DEFINE VARIABLE oDialogResult AS DialogResult    NO-UNDO . 

        DEFINE VARIABLE cPath AS CHARACTER NO-UNDO.

        ASSIGN oControl = CAST (sender, UltraTextEditor) . 
    
        ASSIGN FILE-INFO:FILE-NAME = oControl:Text .
    
        IF FILE-INFO:FULL-PATHNAME > "":U AND FILE-INFO:FILE-TYPE MATCHES "*D*":U THEN 
            folderBrowserDialog1:SelectedPath = FILE-INFO:FULL-PATHNAME . 

        IF oControl = tbCatalogFolder THEN 
            folderBrowserDialog1:Description = "Select JSON Catalog Folder"{&TRAN} .
        ELSE  
            folderBrowserDialog1:Description = "Select AppServer Code base Folder"{&TRAN} .
            
        WAIT-FOR folderBrowserDialog1:ShowDialog () SET oDialogResult .
        
        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .
        
        ASSIGN oControl:Text = folderBrowserDialog1:SelectedPath .

        CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the tbRelativeFolder
        Notes:   
        @param sender The reference to the control that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID tbRelativeFolder_EditorButtonClick (sender AS System.Object, 
	                                                        e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
		
        DEFINE VARIABLE oDialog       AS FolderBrowserForm NO-UNDO . 
        DEFINE VARIABLE oDialogResult AS DialogResult      NO-UNDO . 
        
        oDialog = NEW FolderBrowserForm () . 
        
        FILE-INFO:FILE-NAME = tbAppServerFolder:Text .
        IF FILE-INFO:FULL-PATHNAME = ? THEN 
            UNDO, THROW NEW ExceptionWithTitle ("AppServer Code Base folder is not valid."{&TRAN}, 
                                                0, 
                                                THIS-OBJECT:Text) .           
        
        oDialog:RootFolder     = FILE-INFO:FULL-PATHNAME .

        WAIT-FOR oDialog:ShowDialog () SET oDialogResult . 

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .
        
        ASSIGN tbRelativeFolder:Text = oDialog:SelectedFolder .

        IF tbRelativeFolder:Text BEGINS tbAppServerFolder:Text + "~\":U THEN 
            tbRelativeFolder:Text = SUBSTRING (oDialog:SelectedFolder, LENGTH (tbAppServerFolder:Text) + 2) .   

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterCheck event of the treeView1 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The TreeViewEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID treeView1_AfterCheck (sender AS System.Object, 
	                                          e AS System.Windows.Forms.TreeViewEventArgs):
		
		DEFINE VARIABLE cTopBuffer   AS CHARACTER NO-UNDO.
		DEFINE VARIABLE cChildBuffer AS CHARACTER NO-UNDO.
		
		cTopBuffer = THIS-OBJECT:GetSelectedTopBuffer (OUTPUT cChildBuffer) .
		
        IF cChildBuffer > "":U THEN 
            THIS-OBJECT:tgOuterJoin:Enabled = TRUE . 
        ELSE 
            THIS-OBJECT:tgOuterJoin:Enabled = FALSE . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the OpenAccessSdkWizard class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC OpenAccessSdkWizard ():

        GarbageCollectorHelper:DeleteObject (hDataset) .

    END DESTRUCTOR .

END CLASS.
