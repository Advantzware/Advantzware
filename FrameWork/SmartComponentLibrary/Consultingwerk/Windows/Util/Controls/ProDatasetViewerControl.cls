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
    File        : ProDatasetViewerControl
    Purpose     : Viewer for a ProDataset
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Mon Aug 03 08:58:01 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                                FROM PROPATH .
USING Consultingwerk.Assertion.*                                      FROM PROPATH .
USING Consultingwerk.Exceptions.*                                     FROM PROPATH .
USING Consultingwerk.OERA.*                                           FROM PROPATH .
USING Consultingwerk.Util.*                                           FROM PROPATH .
USING Progress.Data.*                                                 FROM ASSEMBLY .
USING Progress.Lang.*                                                 FROM ASSEMBLY .
USING Progress.Windows.UserControl                                    FROM ASSEMBLY .
USING System.Windows.Forms.*                                          FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Util.Controls.ProDatasetViewerControl
    INHERITS UserControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE hQuery                        AS HANDLE                              NO-UNDO.
    DEFINE PRIVATE VARIABLE oBindingSource                AS Progress.Data.BindingSource         NO-UNDO.

    DEFINE PRIVATE VARIABLE splitContainer1               AS System.Windows.Forms.SplitContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE treeView1                     AS System.Windows.Forms.TreeView       NO-UNDO.
    DEFINE PRIVATE VARIABLE dataGrid1                     AS System.Windows.Forms.DataGrid       NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Dataset Handle this UserControl is visualizing
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DataSet AS HANDLE NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Initialize a new instance of the Class and subscribe to events.
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProDatasetViewerControl ():

        SUPER ().

        InitializeComponent ().

        THIS-OBJECT:treeView1:NodeMouseClick:Subscribe (NodeMouseClickHandler).

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Displays the provided DataSet in the Control
        Notes:
        @param phDataset The handle of the ProDataset
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisplayDataset (phDataset AS HANDLE):

        HandleAssert:WidgetType (phDataset, WidgetTypeEnum:Dataset) .

        THIS-OBJECT:DataSet = phDataset .

        THIS-OBJECT:FillTreeViewWithTableNames() .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Walk though the Dataset TopBuffers and add all of them with their
                 childs to the TreeView
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID FillTreeViewWithTableNames ():

        DEFINE VARIABLE i                AS INTEGER  NO-UNDO.
        DEFINE VARIABLE oCurrentTreeNode AS TreeNode NO-UNDO.

        DO i = 1 TO THIS-OBJECT:DataSet:NUM-TOP-BUFFERS:
            IF NOT RecurseFindTreeNodeName (THIS-OBJECT:DataSet:GET-TOP-BUFFER (i):NAME, THIS-OBJECT:treeView1:Nodes) THEN
                oCurrentTreeNode = RecurseDatasetRelations (THIS-OBJECT:DataSet, THIS-OBJECT:DataSet:GET-TOP-BUFFER (i), ?).
        END.

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
        THIS-OBJECT:splitContainer1 = NEW System.Windows.Forms.SplitContainer().
        THIS-OBJECT:treeView1 = NEW System.Windows.Forms.TreeView().
        THIS-OBJECT:dataGrid1 = NEW System.Windows.Forms.DataGrid().
        CAST(THIS-OBJECT:splitContainer1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:splitContainer1:Panel1:SuspendLayout().
        THIS-OBJECT:splitContainer1:Panel2:SuspendLayout().
        THIS-OBJECT:splitContainer1:SuspendLayout().
        CAST(THIS-OBJECT:dataGrid1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* splitContainer1 */
        /*  */
        THIS-OBJECT:splitContainer1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:splitContainer1:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
        THIS-OBJECT:splitContainer1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:splitContainer1:Name = "splitContainer1":U.
        /*  */
        /* splitContainer1.Panel1 */
        /*  */
        THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:treeView1).
        /*  */
        /* splitContainer1.Panel2 */
        /*  */
        THIS-OBJECT:splitContainer1:Panel2:Controls:Add(THIS-OBJECT:dataGrid1).
        THIS-OBJECT:splitContainer1:Size = NEW System.Drawing.Size(784, 515).
        THIS-OBJECT:splitContainer1:SplitterDistance = 261.
        THIS-OBJECT:splitContainer1:TabIndex = 2.
        /*  */
        /* treeView1 */
        /*  */
        THIS-OBJECT:treeView1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:treeView1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:treeView1:Name = "treeView1":U.
        THIS-OBJECT:treeView1:Size = NEW System.Drawing.Size(261, 515).
        THIS-OBJECT:treeView1:TabIndex = 0.
        /*  */
        /* dataGrid1 */
        /*  */
        THIS-OBJECT:dataGrid1:CaptionForeColor = System.Drawing.SystemColors:ActiveCaptionText.
        THIS-OBJECT:dataGrid1:DataMember = "":U.
        THIS-OBJECT:dataGrid1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:dataGrid1:HeaderForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:dataGrid1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:dataGrid1:Name = "dataGrid1":U.
        THIS-OBJECT:dataGrid1:ReadOnly = TRUE.
        THIS-OBJECT:dataGrid1:Size = NEW System.Drawing.Size(519, 515).
        THIS-OBJECT:dataGrid1:TabIndex = 1.
        /*  */
        /* ProDatasetViewerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainer1).
        THIS-OBJECT:Name = "ProDatasetViewerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(784, 515).
        THIS-OBJECT:splitContainer1:Panel1:ResumeLayout(FALSE).
        THIS-OBJECT:splitContainer1:Panel2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:splitContainer1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:splitContainer1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:dataGrid1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Switches the view to the table which is selected. Checks whether the
                 table is checked (data available).
        Notes:
        @param sender The reference to the object that raised the event
        @param e The TreeNodeMouseClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID NodeMouseClickHandler (sender AS System.Object,
                                              e      AS System.Windows.Forms.TreeNodeMouseClickEventArgs):

        DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO.

        treeView1:SelectedNode = e:Node.

        ASSIGN cTableName = e:Node:Name .

        IF    NOT Progress.Util.EnumHelper:AreEqual (System.Windows.Forms.MouseButtons:Left, e:Button)
           OR NOT VALID-HANDLE (THIS-OBJECT:DataSet) THEN
                RETURN .

        IF VALID-HANDLE (hQuery) THEN DO:
            hQuery:QUERY-CLOSE ().
            DELETE OBJECT hQuery.
        END.

        IF VALID-OBJECT (oBindingSource) THEN DO:
            oBindingSource:Dispose ().
            DELETE OBJECT oBindingSource.
        END.

        oBindingSource = NEW BindingSource ().

        hBuffer = THIS-OBJECT:DataSet:GET-BUFFER-HANDLE (cTableName).

        IF NOT VALID-HANDLE (hBuffer) THEN DO:
            MESSAGE SUBSTITUTE ("The Buffer &1 is not present in this Dataset!":U, cTableName)
                VIEW-AS ALERT-BOX INFORMATION TITLE THIS-OBJECT:Text.
            RETURN.
        END.

        CREATE QUERY hQuery.

        hQuery:SET-BUFFERS (hBuffer).

        hQuery:QUERY-PREPARE (SUBSTITUTE ("FOR EACH &1":U, cTableName)).
        hQuery:QUERY-OPEN.

        oBindingSource:Handle = hQuery:HANDLE.

        THIS-OBJECT:dataGrid1:DataSource = oBindingSource.

        /* Set the caption of the Grid */
        THIS-OBJECT:dataGrid1:CaptionText = cTableName.

        CATCH ple AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (ple, "Error while switching the displayed table!":U) .
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Add the DatasetRelations recursively to the tree
        Notes:
        @param phDataHandle The handle of the ProDataset
        @param phBuffer The current iteration of the parent buffer
        @param poParentTreeNode The current parent tree node
        @return The tree node for the current buffer
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE TreeNode RecurseDatasetRelations (phDataHandle     AS HANDLE,
                                                     phBuffer         AS HANDLE,
                                                     poParentTreeNode AS TreeNode):

        DEFINE VARIABLE i             AS INTEGER  NO-UNDO.
        DEFINE VARIABLE hRelation     AS HANDLE   NO-UNDO.
        DEFINE VARIABLE oCurrentNode  AS TreeNode NO-UNDO.

        oCurrentNode      = NEW System.Windows.Forms.TreeNode (phBuffer:NAME).
        oCurrentNode:Name = phBuffer:NAME.
        oCurrentNode:Text = phBuffer:NAME.

        IF VALID-OBJECT (poParentTreeNode) THEN
            poParentTreeNode:Nodes:Add (oCurrentNode).
        ELSE
            THIS-OBJECT:treeView1:Nodes:Add (oCurrentNode).

        DO i = 1 TO phDataHandle:NUM-RELATIONS:
            hRelation     = phDataHandle:GET-RELATION (i).
            IF hRelation:PARENT-BUFFER = phBuffer AND
               hRelation:ACTIVE        = TRUE AND
               hRelation:RECURSIVE     = FALSE THEN DO:

                /* Add ChildTable */
                RecurseDatasetRelations (phDataHandle, hRelation:CHILD-BUFFER, oCurrentNode).
            END.
        END.

        oCurrentNode:Expand() .

        RETURN oCurrentNode.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Find a TreeNode by Name
        Notes:   Returns TRUE if found
        @param cFindTreeNodeName The name of the treenode to find
        @param oNodeCollection The reference to the TreeNodeCollection
        @return Logical value indicating if the node name was found
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE LOGICAL RecurseFindTreeNodeName (cFindTreeNodeName AS CHARACTER,
                                                    oNodeCollection   AS TreeNodeCollection):

        DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cEntityList AS CHARACTER NO-UNDO.

        DEFINE VARIABLE oFoundNodeWithName AS TreeNode NO-UNDO.

        IF oNodeCollection:COUNT > 0 THEN
            DO i = 0 TO oNodeCollection:Count - 1:
                IF oNodeCollection:Item[i]:Name = cFindTreeNodeName THEN
                    RETURN TRUE.
                ELSE
                    IF RecurseFindTreeNodeName (cFindTreeNodeName, oNodeCollection:Item[i]:Nodes) THEN
                        RETURN TRUE.
        END.
        RETURN FALSE.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Builds a list of all selected child tables from the given selected
                 node
        Notes:
        @param oNodeCollection The reference to the TreeNodeCollection
        @return The comma delimited list of child table names
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER RecurseTreeNodeNames (oNodeCollection  AS TreeNodeCollection):

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
        Purpose: Returns the first selected table to display in DataView
        Notes:
        @param oNodeCollection The reference to the TreeNodeCollection
        @return The reference to the first selected TreeNode
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
        Purpose: Destructor for the OERABusinessEntityTesterUserControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ProDatasetViewerControl ():

        IF VALID-OBJECT (components) THEN DO:
            CAST (components, System.IDisposable):Dispose ().
        END.

    END DESTRUCTOR.

END CLASS.
