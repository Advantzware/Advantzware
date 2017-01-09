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
    File        : DatasetRelationNodeParser
    Purpose     : Supports loading dataset buffers to a tree following 
                  their data relations
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 14 14:21:03 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Util.*  FROM PROPATH .
USING Progress.Lang.*        FROM PROPATH .
USING System.Windows.Forms.* FROM ASSEMBLY  .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.SmartComponents.Support.DatasetRelationNodeParser: 

    /*------------------------------------------------------------------------------
        Purpose: Walks though the Dataset TopBuffers and add all of them with their 
                 childs to the TreeView
        Notes:   
        @param phDataset The handle of the business entity dataset
        @param poRootNodes The TreeNodesCollection of the TreeView Control
        @param pcCheckedTables The names of the checked tables
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID FillTreeViewWithTableNames (phDataset       AS HANDLE,
                                                          poRootNodes     AS TreeNodeCollection,
                                                          pcCheckedTables AS CHARACTER):
                                                              
        DEFINE VARIABLE i                AS INTEGER  NO-UNDO.
        DEFINE VARIABLE oCurrentTreeNode AS TreeNode NO-UNDO.

        DO i = 1 TO phDataset:NUM-TOP-BUFFERS:
            IF NOT RecurseFindTreeNodeName (phDataset:GET-TOP-BUFFER (i):NAME, poRootNodes) THEN
             
                oCurrentTreeNode = RecurseDatasetRelations (phDataset, 
                                                            phDataset:GET-TOP-BUFFER (i), 
                                                            ?, 
                                                            poRootNodes,
                                                            pcCheckedTables).
        END.
         
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Add the DatasetRelations recursively to the tree
        Notes:   
        @param phDataHandle The handle of the business entity dataset
        @param phBuffer The handle of the current parent buffer
        @param poParentTreeNode The current parent tree node
        @param poRootNodes The TreeNodesCollection of the TreeView Control
        @param pcCheckedTables The names of the checked tables
        @return The reference to the TreeNode for the buffer
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE STATIC TreeNode RecurseDatasetRelations (phDataHandle     AS HANDLE,
                                                            phBuffer         AS HANDLE,
                                                            poParentTreeNode AS TreeNode,
                                                            poRootNodes      AS TreeNodeCollection,
                                                            pcCheckedTables AS CHARACTER):
                                                                 
        DEFINE VARIABLE i             AS INTEGER  NO-UNDO.
        DEFINE VARIABLE hRelation     AS HANDLE   NO-UNDO.
        DEFINE VARIABLE oCurrentNode  AS TreeNode NO-UNDO.

        oCurrentNode      = NEW System.Windows.Forms.TreeNode (phBuffer:NAME).
        oCurrentNode:Name = phBuffer:NAME.
        oCurrentNode:Text = phBuffer:NAME.

        IF ListHelper:EntryIsInList (phBuffer:NAME, pcCheckedTables) THEN 
            oCurrentNode:Checked = TRUE . 

        IF VALID-OBJECT (poParentTreeNode) THEN 
            poParentTreeNode:Nodes:Add (oCurrentNode).
        ELSE
            poRootNodes:Add (oCurrentNode).

        DO i = 1 TO phDataHandle:NUM-RELATIONS:
            hRelation     = phDataHandle:GET-RELATION (i).
            
            IF hRelation:PARENT-BUFFER = phBuffer AND 
    	       hRelation:ACTIVE        = TRUE AND
    	       hRelation:RECURSIVE     = FALSE THEN DO:
    	           
                /* Add ChildTable */
                RecurseDatasetRelations (phDataHandle, 
                                         hRelation:CHILD-BUFFER, 
                                         oCurrentNode, 
                                         poRootNodes,
                                         pcCheckedTables).                                                                  
            END.
        END.
        
        RETURN oCurrentNode.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Find a TreeNode by Name
        Notes:   Returns TRUE if found
        @param cFindTreeNodeName The name (text) of the tree node to find
        @param oNodeCollection The parent TreeNodeCollection
        @return Logical value indicating if the tree node is found
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE STATIC LOGICAL RecurseFindTreeNodeName (cFindTreeNodeName AS CHARACTER,
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

END CLASS.
