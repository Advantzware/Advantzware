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
    File        : OERABusinessEntityTesterUserControl
    Purpose     : Core user control of the Business Entity Tester and
                  Busisness Entity Browser tools
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Mon Aug 03 08:58:01 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                                FROM PROPATH .
USING Consultingwerk.Exceptions.*                                     FROM PROPATH .
USING Consultingwerk.OERA.*                                           FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*                 FROM PROPATH .
USING Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.* FROM PROPATH .
USING Consultingwerk.Util.*                                           FROM PROPATH .
USING Progress.Data.*                                                 FROM ASSEMBLY .
USING Progress.Lang.*                                                 FROM ASSEMBLY .
USING Progress.Windows.UserControl                                    FROM ASSEMBLY .
USING System.Windows.Forms.*                                          FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.OERABusinessEntityTesterUserControl
    INHERITS SmartUserControl :

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE hDataset                      AS HANDLE                              NO-UNDO.
    DEFINE PRIVATE VARIABLE hQuery                        AS HANDLE                              NO-UNDO.
    DEFINE PRIVATE VARIABLE oBindingSource                AS Progress.Data.BindingSource         NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityAdapter1   AS SmartBusinessEntityAdapter          NO-UNDO.

    DEFINE PRIVATE VARIABLE splitContainer1               AS System.Windows.Forms.SplitContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE treeView1                     AS System.Windows.Forms.TreeView       NO-UNDO.
    DEFINE PRIVATE VARIABLE dataGrid1                     AS System.Windows.Forms.DataGrid       NO-UNDO.

    DEFINE PRIVATE VARIABLE oOERABusinessEntityTesterForm AS IOERABusinessEntityTesterForm       NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraStatusBar1 AS Infragistics.Win.UltraWinStatusBar.UltraStatusBar NO-UNDO.

    DEFINE PRIVATE VARIABLE oQueryInfo                    AS DataSourceQueryInfo                 NO-UNDO.

    DEFINE VARIABLE cCustomParameterObject AS LONGCHAR NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the serialized form of the custom parameter object
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CustomParameterObject AS LONGCHAR NO-UNDO
    GET:
        RETURN cCustomParameterObject .
    END.
    SET (arg AS LONGCHAR):
        ASSIGN cCustomParameterObject = arg .
    END.

    /*------------------------------------------------------------------------------
        Purpose: ContextMenuStrip (MS Control) from the parent Form
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BatchSize AS INTEGER NO-UNDO INIT 100
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: CustomContext passed to SmartBusinessEntityAdapter
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CustomContext AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the text fo the StatusBar
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StatusBarText AS CHARACTER NO-UNDO
    GET:
        RETURN ultraStatusBar1:Text .
    END GET .
    SET (arg AS CHARACTER):
        ASSIGN ultraStatusBar1:Text = arg.
    END.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the visibility of the Status Bar
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StatusBarVisible AS LOGICAL NO-UNDO
    GET:
        RETURN ultraStatusBar1:Visible .
    END GET .
    SET (arg AS LOGICAL):
        ASSIGN ultraStatusBar1:Visible = arg.
    END.

    /*------------------------------------------------------------------------------
        Purpose: Value for StopAfter to be passed to the Service Interface
        Notes:   Allows limiting the execution time of queries executed on the backend
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StopAfter AS INTEGER INITIAL 0 NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: ContextMenuStrip (MS Control) from the parent Form
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TreeContextMenuStrip AS System.Windows.Forms.ContextMenuStrip NO-UNDO
    SET (arg AS System.Windows.Forms.ContextMenuStrip):
        ASSIGN THIS-OBJECT:treeView1:ContextMenuStrip = arg.
    END.

    /**
     * Purpose:
     * Notes:
     *
     * @return CHARACTER
     */
    DEFINE PUBLIC PROPERTY TreeView AS System.Windows.Forms.TreeView NO-UNDO
    GET:
        RETURN treeView1 .
    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Images used to show whether a filter is applied to a table.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ImageList AS System.Windows.Forms.ImageList NO-UNDO
    SET (arg AS System.Windows.Forms.ImageList):
        THIS-OBJECT:treeView1:ImageList = arg.
    END.

     /*------------------------------------------------------------------------------
        Purpose: The parent Form used for callback to en-/disable Tools.
        Notes:
    ------------------------------------------------------------------------------*/
     DEFINE PUBLIC PROPERTY OERABusinessEntityTesterForm AS IOERABusinessEntityTesterForm NO-UNDO
    SET (arg AS IOERABusinessEntityTesterForm):
        ASSIGN oOERABusinessEntityTesterForm = arg.
    END.

     /*------------------------------------------------------------------------------
        Purpose: Initialize a new instance of the Class and subscribe to events.
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC OERABusinessEntityTesterUserControl ():

        SUPER ().

        InitializeComponent ().

        THIS-OBJECT:treeView1:AfterCheck:Subscribe (AfterCheckHandler).
        THIS-OBJECT:treeView1:MouseDown:Subscribe (TreeViewMouseDownHandler) .
        THIS-OBJECT:treeView1:NodeMouseClick:Subscribe (NodeMouseClickHandler).
        THIS-OBJECT:treeView1:AfterSelect:Subscribe (AfterSelectHandler).

        FIX-CODEPAGE (cCustomParameterObject) = "utf-8":U .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterCheck event of the tree
        Notes:   Change settings after a table has been de-/selected
        @param sender The reference to the object that raised the event
        @param e The TreeViewEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID AfterCheckHandler (sender AS System.Object,
                                           e      AS System.Windows.Forms.TreeViewEventArgs):

        IF NOT e:Node:Checked AND e:Node:ImageIndex = 1 THEN
            ASSIGN
                e:Node:Tag                = "":U
                e:Node:ImageIndex         = 0
                e:Node:SelectedImageIndex = e:Node:ImageIndex
                .

        IF AtLeastOneNodeChecked (THIS-OBJECT:treeView1:Nodes) THEN
            oOERABusinessEntityTesterForm:EnableTool ("GetData":U).
        ELSE
            oOERABusinessEntityTesterForm:DisableTool ("GetData":U).

        AfterSelectHandler (sender, e).
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handle for the AfterSelect event of the tree
        Notes:   Set the Filter tools en-/disabled
        @param sender The reference to the object that raised the event
        @param e The TreeViewEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AfterSelectHandler (sender AS System.Object,
                                           e      AS System.Windows.Forms.TreeViewEventArgs):

        /* Enables the Filter Button if the SelectedNode is checked. */
        IF e:Node:Checked THEN
            oOERABusinessEntityTesterForm:EnableTool ("Filter":U).
        ELSE
            oOERABusinessEntityTesterForm:DisableTool ("Filter":U).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Recursivly go through all nodes and return True if at least one if
                 checked
        Notes:
        @param poNodes The reference to the TreeNodeCollection to verify
        @return Logical value indicating if at least one node was chaged
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL AtLeastOneNodeChecked (poNodes AS System.Windows.Forms.TreeNodeCollection):

        DEFINE VARIABLE i       AS INTEGER  NO-UNDO.

        DO i = 0 TO poNodes:Count - 1:
            IF poNodes[i]:Checked THEN
                RETURN TRUE .
            ELSE DO:
                IF poNodes[i]:Nodes:Count > 0 THEN DO:

                    /* Return TRUE if a child or grand child of this node
                       if selected */
                    IF AtLeastOneNodeChecked (poNodes[i]:Nodes) THEN
                        RETURN TRUE.
                END.
            END.
        END.

        RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Get the BusinessEntity Data from Backend
        Notes:   Only the first ChildTable is Joined at the moment.
        @param pcEntityName The name of the business entity to retrieve data from
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FetchDataFromBackend (pcEntityName AS CHARACTER):

        DEFINE VARIABLE cEntityName    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cEntityTable   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cEntityView    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cEntityJoin    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cQueryStrings  AS CHARACTER NO-UNDO.

        DEFINE VARIABLE cCurrentBuffer AS CHARACTER NO-UNDO.

        DEFINE VARIABLE dtStart        AS DATETIME  NO-UNDO.
        DEFINE VARIABLE dtEnd          AS DATETIME  NO-UNDO.
        DEFINE VARIABLE iReads         AS INT64     NO-UNDO EXTENT .
        DEFINE VARIABLE cReads         AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i              AS INTEGER   NO-UNDO.

        ASSIGN THIS-OBJECT:EntityName = pcEntityName .

        ASSIGN
            cEntityName  = pcEntityName
            cEntityTable = GetSelectedTopBuffer (OUTPUT cEntityView, OUTPUT cQueryStrings)
            cEntityJoin  = IF cEntityView > "":U THEN "YES":U + FILL (",NO":U, NUM-ENTRIES (cEntityView) - 1) ELSE "":U
            .

        ASSIGN
            hDataset                         = ?
            THIS-OBJECT:dataGrid1:DataSource = ?
            .

        IF VALID-HANDLE (hQuery) THEN DO:
            hQuery:QUERY-CLOSE ().
            DELETE OBJECT hQuery.
        END.

        IF VALID-OBJECT (oBindingSource) THEN DO:
            oBindingSource:Dispose ().
            DELETE OBJECT oBindingSource.
        END.

        IF cEntityTable = "":U THEN
            RETURN.

        oBindingSource = NEW BindingSource ().

        LaunchAdapter () .

        THIS-OBJECT:smartBusinessEntityAdapter1:NewFetchDataRequest:Subscribe (NewFetchDataRequestHandler) .

        ASSIGN
            THIS-OBJECT:smartBusinessEntityAdapter1:CustomContext               = THIS-OBJECT:CustomContext
            THIS-OBJECT:smartBusinessEntityAdapter1:EntityName                  = cEntityName
            THIS-OBJECT:smartBusinessEntityAdapter1:EntityTable                 = cEntityTable
            THIS-OBJECT:smartBusinessEntityAdapter1:EntityJoin                  = cEntityJoin
            THIS-OBJECT:smartBusinessEntityAdapter1:EntityView                  = cEntityView
            THIS-OBJECT:smartBusinessEntityAdapter1:QueryString                 = cQueryStrings
            THIS-OBJECT:smartBusinessEntityAdapter1:BatchSize                   = THIS-OBJECT:BatchSize
            THIS-OBJECT:smartBusinessEntityAdapter1:StopAfter                   = THIS-OBJECT:StopAfter
            THIS-OBJECT:smartBusinessEntityAdapter1:ThrowErrorsFromRetrieveData = TRUE
            .

        /* Start statistics */
        ASSIGN iReads  = Consultingwerk.Util.DatabaseHelper:GetRecordReads ()
               dtStart = NOW .

        DO ON ERROR UNDO, THROW:

            THIS-OBJECT:smartBusinessEntityAdapter1:RetrieveData ().

            FINALLY:
                /* End statistics */
                ASSIGN dtEnd  = NOW
                       iReads = Consultingwerk.Util.DatabaseHelper:GetRecordReads (iReads) .

                /* Statistics */
                DO i = 1 TO NUM-DBS:
                    IF i > 1 THEN
                        cReads = cReads + ", ":U .

                    ASSIGN cReads = cReads + SUBSTITUTE ("&1: &2":U,
                                                         LDBNAME (i),
                                                         iReads[i]) .
                END.

                ultraStatusBar1:Text = SUBSTITUTE ("Runtime: &1 msecs; DB Records Reads: &2",
                                                   INTERVAL (dtEnd, dtStart, "milliseconds":U),
                                                   cReads) .


            END FINALLY.
        END.

        ASSIGN hDataset       = THIS-OBJECT:smartBusinessEntityAdapter1:DataSet

               cCurrentBuffer = treeView1:SelectedNode:Text NO-ERROR .

        IF cCurrentBuffer = ? OR NOT ListHelper:EntryIsInList (cCurrentBuffer, TRIM (cEntityTable + ",":U + cEntityView, ",":U)) THEN

        cCurrentBuffer = cEntityTable .

        CREATE QUERY hQuery.

        hQuery:SET-BUFFERS (hDataset:GET-BUFFER-HANDLE (cCurrentBuffer)).

        hQuery:QUERY-PREPARE (SUBSTITUTE ("FOR EACH &1":U, cCurrentBuffer)).
        hQuery:QUERY-OPEN.

        oBindingSource:Handle = hQuery:HANDLE.

        THIS-OBJECT:dataGrid1:DataSource = oBindingSource .

        /* Set the caption of the Grid */
        THIS-OBJECT:dataGrid1:CaptionText = cCurrentBuffer .

        CATCH ple AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (ple, "Error while fetching data from Backend!") .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Get an empty ProDataset from a Backend business entity and populates
                 the Tree View
        Notes:
        @param pcEntityName The name of the Business Entity
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FetchEntity (pcEntityName AS CHARACTER):

        THIS-OBJECT:treeView1:Nodes:Clear ().

        ASSIGN THIS-OBJECT:EntityName = pcEntityName .

        FetchEntityFromBackend (pcEntityName).

        FillTreeViewWithTableNames ().

        /* Button will only be enabled, after user has checked a node */
        oOERABusinessEntityTesterForm:DisableTool ("GetData":U).

        IF THIS-OBJECT:treeView1:Nodes:Count > 0 THEN DO:
            THIS-OBJECT:treeView1:Nodes[0]:Checked = TRUE .
            THIS-OBJECT:treeView1:SelectedNode = THIS-OBJECT:treeView1:Nodes[0] .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Get an empty ProDataset from a Backend business entity
        Notes:
        @param pcEntityName The name of the Business Entity
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID FetchEntityFromBackend (pcEntityName AS CHARACTER):

        DEFINE VARIABLE cContext AS CHARACTER NO-UNDO.
        DEFINE VARIABLE oServiceAdapter AS Consultingwerk.OERA.IServiceAdapter NO-UNDO .
        DEFINE VARIABLE oClass AS Progress.Lang.Class NO-UNDO .

        ASSIGN hDataset = ? .

        LaunchAdapter() .

        oOERABusinessEntityTesterForm:DisableTool ("Filter":U).

        oClass = Progress.Lang.Class:GetClass ("Consultingwerk.SmartComponents.Implementation.ticEnterprise.ticBusinessEntityAdapter":U)
                    NO-ERROR .

        IF VALID-OBJECT (oClass) AND THIS-OBJECT:smartBusinessEntityAdapter1:GetClass():IsA (oClass) THEN DO:

            RUN adm2/retrieveDataset.p (pcEntityName,
                                        "DEFS":U,
                                        INPUT-OUTPUT cContext,
                                        INPUT-OUTPUT hDataset) .
        END .
        ELSE DO:
            oServiceAdapter = Consultingwerk.Framework.FrameworkSettings:ServiceAdapter .

            oServiceAdapter:FetchDataset ("":U, pcEntityName,
                                          OUTPUT DATASET-HANDLE hDataset BY-REFERENCE) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Walk though the Dataset TopBuffers and add all of them with their
                 childs to the TreeView
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID FillTreeViewWithTableNames ():

        DEFINE VARIABLE i                AS INTEGER  NO-UNDO.
        DEFINE VARIABLE oCurrentTreeNode AS TreeNode NO-UNDO.

        DO i = 1 TO hDataset:NUM-TOP-BUFFERS:
            IF NOT RecurseFindTreeNodeName (hDataset:GET-TOP-BUFFER (i):NAME, THIS-OBJECT:treeView1:Nodes) THEN
                oCurrentTreeNode = RecurseDatasetRelations (hDataset, hDataset:GET-TOP-BUFFER (i), ?).
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the top most buffer which is selected
        Notes:
        @return Tree reference to the topmost selected TreeNode
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE TreeNode GetSelectedTopBuffer ():

        RETURN RecurseTreeNodes (THIS-OBJECT:treeView1:Nodes).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the first selected table to display in DataView and builds
                 a list of all selected child tables from the given selected node
        Notes:   Returns the name of the SelectedTopBuffer with a list of checked
                 nodes or returns the TreeNode object of the SelectedTopBuffer
        @param cCheckedNodes OUTPUT Comma delimited list of checked node text
        @param cQueryStrings OUTPUT Delimited list of query strings of the selected nodes
        @return The name of the topmost selected buffer
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER GetSelectedTopBuffer (OUTPUT cCheckedNodes AS CHARACTER,
                                                   OUTPUT cQueryStrings AS CHARACTER):

        DEFINE VARIABLE oChildResult AS TreeNode NO-UNDO.

        oChildResult = RecurseTreeNodes (THIS-OBJECT:treeView1:Nodes).

        IF VALID-OBJECT (oChildResult) THEN DO:
            ASSIGN
                cCheckedNodes = RecurseTreeNodeNames (oChildResult:Nodes)
                cQueryStrings = RecurseTreeNodeQueries (THIS-OBJECT:treeView1:Nodes).

            RETURN oChildResult:Text.
        END.
        ELSE DO:
            cCheckedNodes = "":U.
            RETURN "":U.
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
        THIS-OBJECT:ultraStatusBar1 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusBar().
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
        THIS-OBJECT:splitContainer1:Name = "splitContainer1".
        /*  */
        /* splitContainer1.Panel1 */
        /*  */
        THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:treeView1).
        /*  */
        /* splitContainer1.Panel2 */
        /*  */
        THIS-OBJECT:splitContainer1:Panel2:Controls:Add(THIS-OBJECT:dataGrid1).
        THIS-OBJECT:splitContainer1:Size = NEW System.Drawing.Size(784, 492).
        THIS-OBJECT:splitContainer1:SplitterDistance = 261.
        THIS-OBJECT:splitContainer1:TabIndex = 2.
        /*  */
        /* treeView1 */
        /*  */
        THIS-OBJECT:treeView1:CheckBoxes = TRUE.
        THIS-OBJECT:treeView1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:treeView1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:treeView1:Name = "treeView1".
        THIS-OBJECT:treeView1:Size = NEW System.Drawing.Size(261, 492).
        THIS-OBJECT:treeView1:TabIndex = 0.
        /*  */
        /* dataGrid1 */
        /*  */
        THIS-OBJECT:dataGrid1:CaptionForeColor = System.Drawing.SystemColors:ActiveCaptionText.
        THIS-OBJECT:dataGrid1:DataMember = "".
        THIS-OBJECT:dataGrid1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:dataGrid1:HeaderForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:dataGrid1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:dataGrid1:Name = "dataGrid1".
        THIS-OBJECT:dataGrid1:ReadOnly = TRUE.
        THIS-OBJECT:dataGrid1:Size = NEW System.Drawing.Size(519, 492).
        THIS-OBJECT:dataGrid1:TabIndex = 1.
        /*  */
        /* ultraStatusBar1 */
        /*  */
        THIS-OBJECT:ultraStatusBar1:Location = NEW System.Drawing.Point(0, 492).
        THIS-OBJECT:ultraStatusBar1:Name = "ultraStatusBar1".
        THIS-OBJECT:ultraStatusBar1:Size = NEW System.Drawing.Size(784, 23).
        THIS-OBJECT:ultraStatusBar1:TabIndex = 1.
        THIS-OBJECT:ultraStatusBar1:ViewStyle = Infragistics.Win.UltraWinStatusBar.ViewStyle:Office2007.
        /*  */
        /* OERABusinessEntityTesterUserControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(6, 13).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainer1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraStatusBar1).
        THIS-OBJECT:Name = "OERABusinessEntityTesterUserControl".
        THIS-OBJECT:Size = NEW System.Drawing.Size(784, 515).
        THIS-OBJECT:splitContainer1:Panel1:ResumeLayout(FALSE).
        THIS-OBJECT:splitContainer1:Panel2:ResumeLayout(FALSE).
        THIS-OBJECT:splitContainer1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:dataGrid1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.


    /*------------------------------------------------------------------------------
        Purpose: Creates the BusinessEntityAdapter instance
        Notes:   Ability to specify a different SmartBusinessEntityAdapter derived
                 class for the data adapter using -param Adapter=.....
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID LaunchAdapter ():
        DEFINE VARIABLE cClassName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i          AS INTEGER   NO-UNDO.

        IF VALID-OBJECT (THIS-OBJECT:smartBusinessEntityAdapter1) THEN DO:
            DELETE OBJECT THIS-OBJECT:smartBusinessEntityAdapter1.
        END.

        IF SESSION:PARAMETER > "":U THEN DO:
            DO i = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
                IF ENTRY(i, SESSION:PARAMETER) BEGINS "Adapter=":U THEN
                    ASSIGN cClassName = ENTRY(2, ENTRY(i, SESSION:PARAMETER), "=":U) .
            END.

            IF cClassName > "":U THEN
                THIS-OBJECT:smartBusinessEntityAdapter1 = DYNAMIC-NEW (cClassName) ().
        END.

        IF NOT VALID-OBJECT (smartBusinessEntityAdapter1) THEN
            THIS-OBJECT:smartBusinessEntityAdapter1 = NEW SmartBusinessEntityAdapter ().

    END METHOD.


    /*------------------------------------------------------------------------------
       Purpose: Event handler for ther NewFetchDataRequest event of the
                SmartBusinessEntityAdapter instance
       Notes:   Assigns a DataSourceQueryInfo instance to the CustomParameter
                property of the IFechDataRequest instance
       @param sender The reference to the object that raised the event
       @param e The NewFetchDataRequestEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID NewFetchDataRequestHandler (sender AS Progress.Lang.Object,
                                                    e AS NewFetchDataRequestEventArgs):

        DEFINE VARIABLE oSerializable AS ISerializable NO-UNDO .
        DEFINE VARIABLE oException    AS Exception     NO-UNDO .

        IF THIS-OBJECT:CustomParameterObject > "":U THEN DO ON ERROR UNDO, THROW:
            oSerializable = Serializable:DeserializeInstance (THIS-OBJECT:CustomParameterObject) .

            ASSIGN e:FetchDataRequest:CustomParameter = oSerializable .

            oQueryInfo = ? .

            CATCH err AS Progress.Lang.Error:
                oException = NEW Exception (err, "Error deserializing the custom parameter object."{&TRAN}, 0) .

                IF err:NumMessages > 0 THEN
                   oException:AddMessage (err:GetMessage (1), err:GetMessageNum (1)) .

                UNDO, THROW oException .
            END CATCH.
        END.
        ELSE DO:
            oQueryInfo = NEW Consultingwerk.OERA.DataSourceQueryInfo () .

            ASSIGN e:FetchDataRequest:CustomParameter = oQueryInfo .
        END.

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

        DEFINE VARIABLE cEntityTable AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hBuffer      AS HANDLE    NO-UNDO.

        treeView1:SelectedNode = e:Node.

        IF    NOT Progress.Util.EnumHelper:AreEqual (System.Windows.Forms.MouseButtons:Left, e:Button)
           OR NOT e:Node:Checked
           OR THIS-OBJECT:dataGrid1:DataSource = ?
           OR NOT VALID-HANDLE (hDataset) THEN
                RETURN .

        ASSIGN cEntityTable = e:Node:Text.

        IF VALID-HANDLE (hQuery) THEN DO:
            hQuery:QUERY-CLOSE ().
            DELETE OBJECT hQuery.
        END.

        IF VALID-OBJECT (oBindingSource) THEN DO:
            oBindingSource:Dispose ().
            DELETE OBJECT oBindingSource.
        END.

        oBindingSource = NEW BindingSource ().

        hBuffer = hDataset:GET-BUFFER-HANDLE (cEntityTable).

        IF NOT VALID-HANDLE (hBuffer) THEN DO:
            MESSAGE SUBSTITUTE ("The Buffer &1 is not present in this Dataset!", cEntityTable)
                VIEW-AS ALERT-BOX INFORMATION TITLE THIS-OBJECT:Text.
            RETURN.
        END.

        CREATE QUERY hQuery.

        hQuery:SET-BUFFERS (hBuffer).

        hQuery:QUERY-PREPARE (SUBSTITUTE ("FOR EACH &1":U, cEntityTable)).
        hQuery:QUERY-OPEN.

        oBindingSource:Handle = hQuery:HANDLE.

        THIS-OBJECT:dataGrid1:DataSource = oBindingSource.

        /* Set the caption of the Grid */
        THIS-OBJECT:dataGrid1:CaptionText = cEntityTable.

        CATCH ple AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (ple, "Error while switching the displayed table!") .
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Selects all Tree nodes
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SelectAll ():

        SelectNodes (treeView1:Nodes, TRUE) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Selects or deselectes all Tree nodes
        Notes:
        @param poNodes The reference to the TreeNodeCollection
        @param plSelected Logical value indicating if the tree nodes should be selected or not
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SelectNodes (poNodes AS System.Windows.Forms.TreeNodeCollection,
                                       plSelected AS LOGICAL):

        {Consultingwerk/foreach.i System.Windows.Forms.TreeNode oNode in poNodes}
            oNode:Checked = plSelected .

            IF oNode:Nodes:Count > 0 THEN
                SelectNodes (oNode:Nodes, plSelected) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Deselects all Tree nodes
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SelectNone ():

        SelectNodes (treeView1:Nodes, FALSE) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Shows information about the Data source query strings from the previous
                request
       Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ShowDataSourceQueryInfo ():

        IF NOT VALID-OBJECT (oQueryInfo) THEN
            MessageFormHelper:ShowMessage ("No query information available."{&TRAN},
                                           Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageWarning) .

        ELSE

            MessageFormHelper:ShowMessage (oQueryInfo:PrepareQuery,
                                           "Query Information"{&TRAN},
                                           SUBSTITUTE ("Client Query: &1&4Source Default Query: &2&4Prepare Query: &3",
                                                       oQueryInfo:ClientQuery,
                                                       oQueryInfo:SourceDefaultQuery,
                                                       oQueryInfo:PrepareQuery,
                                                       System.Environment:NewLine),
                                           Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageInfo) .


    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Shuts down all locally running Business Entity instances using
                 the service adapter
        Notes:   Assumption is, that the Business Entity Tester is running the
                 Business Entities locally. Otherwise this will need to be implemented
                 as a call to the AppServer(s)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ShutDownBusinessEntities ():

        Consultingwerk.OERA.ServiceManager:StopAllBusinessServices
           (Progress.Lang.Class:GetClass ("Consultingwerk.OERA.IBusinessEntity":U)) .

        Consultingwerk.Util.MessageFormHelper:ShowMessage
            ("Shut down all locally running Business Entities",
             "Business Entity Tester") .

    END METHOD.

      /*------------------------------------------------------------------------------
        Purpose: Event handler for the MouseDown event of the Tree View
        Notes:   Enable/Disable the Remove Filter context-menu item when the user
                 right clicks on the tree view - before the context menu is opened
        @param sender The reference to the object that raised the event
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID TreeViewMouseDownHandler (sender AS System.Object,
                                                  e      AS System.Windows.Forms.MouseEventArgs):

        DEFINE VARIABLE oNode AS TreeNode NO-UNDO .

        IF NOT Progress.Util.EnumHelper:AreEqual (System.Windows.Forms.MouseButtons:RIGHT, e:Button) THEN
            RETURN .

        oNode = THIS-OBJECT:treeView1:GetNodeAt (e:X, e:Y) .

        IF VALID-OBJECT (oNode) AND
           (oNode:ImageIndex = 1 OR UNBOX (oNode:Tag) > "":U) THEN
            oOERABusinessEntityTesterForm:EnableTool ("RemoveFilter":U).
        ELSE
            oOERABusinessEntityTesterForm:DisableTool ("RemoveFilter":U).

        IF VALID-OBJECT (oNode) THEN
            THIS-OBJECT:treeView1:SelectedNode = oNode .

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

        DEFINE VARIABLE i AS INTEGER   NO-UNDO.

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
        Purpose: Build a list of all Queries stored in the Tag property of the
                 TreeNodes
        Notes:
        @param oNodeCollection The reference to the TreeNodeCollection
        @return The CHR(1) delimited list of query strings
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER RecurseTreeNodeQueries (oNodeCollection  AS TreeNodeCollection):

        DEFINE VARIABLE i          AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cQueryList AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cRecursion AS CHARACTER NO-UNDO.

        DO i = 0 TO oNodeCollection:Count - 1:
            IF oNodeCollection[i]:Checked THEN DO:
                IF UNBOX (oNodeCollection[i]:Tag) > "":U THEN
                    cQueryList = cQueryList + (IF LENGTH(cQueryList) > 0 THEN CHR(1) ELSE "":U) + UNBOX (oNodeCollection[i]:Tag).
                ELSE
                    cQueryList = cQueryList + (IF LENGTH(cQueryList) > 0 THEN CHR(1) ELSE "":U) + " ":U.
            END.

            IF oNodeCollection[i]:Nodes:Count > 0 THEN DO:
                ASSIGN cRecursion = RecurseTreeNodeQueries (oNodeCollection[i]:Nodes) .

                IF LENGTH(cRecursion) > 0 THEN
                    cQueryList = cQueryList + (IF LENGTH(cQueryList) > 0 THEN CHR(1) ELSE "":U) + cRecursion .
            END.
        END.

        RETURN cQueryList.

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
        Purpose: Removes the filter from a table.
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RemoveFilter ():

        DEFINE VARIABLE oTreeNode AS TreeNode NO-UNDO.

        IF NOT VALID-OBJECT (THIS-OBJECT:treeView1:SelectedNode) THEN
            RETURN.

        oTreeNode = THIS-OBJECT:treeView1:SelectedNode .

        IF oTreeNode:ImageIndex = 1
        OR UNBOX (oTreeNode:Tag) > "":U THEN
            ASSIGN
                oTreeNode:Tag                = "":U
                oTreeNode:ImageIndex         = 0
                oTreeNode:SelectedImageIndex = oTreeNode:ImageIndex
                .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Show the Filter Dialog and apply the settings
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ShowFilterDialog ():

        DEFINE VARIABLE oFilterDialog     AS OERABusinessEntityTesterFilterDialog NO-UNDO.
        DEFINE VARIABLE oResult           AS System.Windows.Forms.DialogResult    NO-UNDO.

        IF NOT VALID-OBJECT (THIS-OBJECT:treeView1:SelectedNode) THEN
            RETURN.

        IF NOT THIS-OBJECT:treeView1:SelectedNode:Checked THEN DO:
            MESSAGE "Please select a checked node to set a filter!"
                VIEW-AS ALERT-BOX INFORMATION TITLE THIS-OBJECT:Text.
            RETURN.
        END.

        oFilterDialog = NEW OERABusinessEntityTesterFilterDialog ().
        oFilterDialog:DatasetHandle = hDataset .

        ASSIGN oFilterDialog:EntityName  = THIS-OBJECT:EntityName
               oFilterDialog:EntityTable = THIS-OBJECT:treeView1:SelectedNode:Text .

        IF UNBOX (THIS-OBJECT:treeView1:SelectedNode:Tag) > "":U THEN
            oFilterDialog:QueryString = UNBOX (THIS-OBJECT:treeView1:SelectedNode:Tag) .
        ELSE
            oFilterDialog:QueryString = SUBSTITUTE ("FOR EACH &1 WHERE &1. = ":U, THIS-OBJECT:treeView1:SelectedNode:Text).

        WAIT-FOR oFilterDialog:ShowDialog (THIS-OBJECT) SET oResult.

        IF oResult:ToString () = "OK":U THEN DO:
            THIS-OBJECT:treeView1:SelectedNode:Tag = BOX (oFilterDialog:QueryString).

            /* Mark the filtered table in the tree */
            THIS-OBJECT:treeView1:SelectedNode:ImageIndex = IF UNBOX (THIS-OBJECT:treeView1:SelectedNode:Tag) > "":U THEN 1 ELSE 0.
            THIS-OBJECT:treeView1:SelectedNode:SelectedImageIndex = THIS-OBJECT:treeView1:SelectedNode:ImageIndex.
        END.

        FINALLY:
            IF VALID-OBJECT (oFilterDialog) THEN DO:
                oFilterDialog:Dispose ().
                DELETE OBJECT oFilterDialog.
            END.
        END FINALLY.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the OERABusinessEntityTesterUserControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC OERABusinessEntityTesterUserControl ():

        IF VALID-OBJECT (components) THEN DO:
            CAST (components, System.IDisposable):Dispose ().
        END.

    END DESTRUCTOR.

END CLASS.