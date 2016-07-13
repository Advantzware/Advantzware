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
    File        : TreeNodeFromDatasetStructureFactory
    Purpose     : Generic factory method for nodes in the Microsoft 
                  TreeView Control that creates nodes visualizing the 
                  structure of a ProDataset
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Nov 08 06:53:44 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*           FROM PROPATH .
USING Consultingwerk.Assertion.* FROM PROPATH .
USING Consultingwerk.Windows.*   FROM PROPATH .  
USING Progress.Lang.*            FROM PROPATH .
USING System.Windows.Forms.*     FROM ASSEMBLY.

CLASS Consultingwerk.Windows.TreeNodeFromDatasetStructureFactory: 

    /*------------------------------------------------------------------------------
        Purpose: Creates treenodes in the provided Microsoft tree visualizing the 
                 structure of the provided ProDataset
        Notes:   
        @param poTreeView The reference to Microsoft TreeView Control
        @param phDataset The handle of the ProDataset
        @return Logical value indicating if Treenodes have been created or not
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL CreateTreeNodes (poTreeView AS TreeView,
	                                       phDataset AS HANDLE):
		
        DEFINE VARIABLE i         AS INTEGER  NO-UNDO.

		ObjectAssert:IsValid (poTreeView, "poTreeView":U) .
		{Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataset WidgetTypeEnum:Dataset} .

        DO i = 1 TO phDataset:NUM-TOP-BUFFERS:
                THIS-OBJECT:RecurseDatasetRelations (poTreeView, phDataset:GET-TOP-BUFFER (i), ?).
        END.


    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Add the DatasetRelations recursively to the tree
        Notes:
        @param poTreeView The reference to Microsoft TreeView Control
        @param phBuffer The handle of the current dataset buffer
        @param poParentTreeNode The TreeNode of the current parent node
        @return The new TreeNode         
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE TreeNode RecurseDatasetRelations (poTreeView       AS TreeView,
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
            poTreeView:Nodes:Add (oCurrentNode).

        DO i = 1 TO phBuffer:DATASET:NUM-RELATIONS:
            hRelation     = phBuffer:DATASET:GET-RELATION (i).
            
            IF hRelation:PARENT-BUFFER = phBuffer THEN DO:
                /* Add ChildTable */
                RecurseDatasetRelations (poTreeView, hRelation:CHILD-BUFFER, oCurrentNode).                                                                  
            END.
        END.
        
        oCurrentNode:Expand() .
        
        RETURN oCurrentNode.
        
    END METHOD.


END CLASS.
