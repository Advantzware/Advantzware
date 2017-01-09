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
    File        : TreeViewMenuLoader
    Purpose     : Populates the Nodes of an UltraTree based on the 
                  contents of the ttTreeViewMenu temp-table
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 16 13:54:09 CEST 2012
    Notes       : DEFINE TEMP-TABLE ttTreeViewMenu NO-UNDO 
                      FIELD NodeKey       AS CHARACTER 
                      FIELD ParentNodeKey AS CHARACTER
                      FIELD NodeSort      AS INTEGER  
                      FIELD NodeText      AS CHARACTER
                      FIELD NodeTag       AS CHARACTER 
                      FIELD Active        AS LOGICAL INIT TRUE 
                      FIELD ImageKey      AS CHARACTER 
                      INDEX NodeKey IS PRIMARY UNIQUE NodeKey 
                      INDEX ParentNodeKey ParentNodeKey NodeSort .
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Windows.*        FROM PROPATH . 
&IF DEFINED (Infragistics) NE 0 &THEN
USING Infragistics.Win.UltraWinTree.* FROM ASSEMBLY .
&ENDIF
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Windows.TreeViewMenuLoader: 

    {Consultingwerk/Windows/TempTables/ttTreeViewMenu.i &REFERENCE-ONLY=REFERENCE-ONLY}

&IF DEFINED (Infragistics) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Loads nodes into a TreeNodesCollection for a given parent node or root 
        Notes:   
        @param poNodes The reference to the TreeNodesCollection to load nodes into
        @param poImages The Dictionary used to look up images (optional)
        @param pcParentNodeKey The parent node key, pass "" for the root nodes 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CreateTreeViewNodes (poNodes AS TreeNodesCollection,
                                               poImages AS "System.Collections.Generic.Dictionary<CHARACTER, System.Drawing.Bitmap>":U,
                                               pcParentNodeKey AS CHARACTER):
        
        DEFINE VARIABLE oNode AS UltraTreeNode NO-UNDO .
         
        DEFINE BUFFER b_ttTreeViewMenu FOR ttTreeViewMenu .
        DEFINE BUFFER b2_ttTreeViewMenu FOR ttTreeViewMenu .
        
        FOR EACH b_ttTreeViewMenu WHERE b_ttTreeViewMenu.ParentNodeKey = pcParentNodeKey
                                    AND b_ttTreeViewMenu.Active        = TRUE ON ERROR UNDO, THROW:
            
            oNode = poNodes:Add (b_ttTreeViewMenu.NodeKey) .
            
            IF b_ttTreeViewMenu.NodeText > "":U THEN 
                ASSIGN oNode:Text = b_ttTreeViewMenu.NodeText . 

            IF b_ttTreeViewMenu.NodeTag > "":U THEN 
                ASSIGN oNode:Tag = BOX (b_ttTreeViewMenu.NodeTag) . 

            IF VALID-OBJECT (poImages) AND b_ttTreeViewMenu.ImageKey > "":U THEN DO:
                
                IF NOT poImages:ContainsKey (b_ttTreeViewMenu.ImageKey) THEN 
                    UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to find image ~"&1~" in the dictionary.", 
                                                          b_ttTreeViewMenu.ImageKey), 0) .
                
                oNode:LeftImages:Add (poImages [b_ttTreeViewMenu.ImageKey]) .
                
            END.
                
            IF CAN-FIND (FIRST b2_ttTreeViewMenu WHERE b2_ttTreeViewMenu.ParentNodeKey = b_ttTreeViewMenu.NodeKey) THEN 
                THIS-OBJECT:CreateTreeViewNodes (oNode:Nodes,
                                                 poImages,
                                                 b_ttTreeViewMenu.NodeKey) .                
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Loads nodes into a TreeNodesCollection 
        Notes:   Starts with records that have an empty ParentNodeKey value (EQ empty string)   
        @param poNodes The reference to the TreeNodesCollection to load nodes into
        @param poImages The Dictionary used to look up images (optional)
        @param ttTreeViewMenu The ttTreeViewMenu temp-table that contains the data to load the nodes from  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID LoadTreeViewNodes (poNodes AS TreeNodesCollection,
                                          poImages AS "System.Collections.Generic.Dictionary<CHARACTER, System.Drawing.Bitmap>":U,
                                          TABLE ttTreeViewMenu):
        
        Consultingwerk.Assertion.ObjectAssert:IsValid (poNodes, "poNodes":U) .
        
        THIS-OBJECT:CreateTreeViewNodes (poNodes, 
                                         poImages,
                                         "":U) . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Loads nodes into the root nodes collection of an UltraTree
        Notes:   
        @param poTreeView The reference to the UltraTree Control to load nodes into
        @param poImages The Dictionary used to look up images (optional)
        @param ttTreeViewMenu The ttTreeViewMenu temp-table that contains the data to load the nodes from  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID LoadTreeViewNodes (poTreeView AS UltraTree,
                                          poImages AS "System.Collections.Generic.Dictionary<CHARACTER, System.Drawing.Bitmap>":U,
                                          TABLE ttTreeViewMenu):
        
        Consultingwerk.Assertion.ObjectAssert:IsValid (poTreeView, "poTreeView":U) .
        
        THIS-OBJECT:LoadTreeViewNodes (poTreeView:Nodes,
                                       poImages,
                                       TABLE ttTreeViewMenu BY-REFERENCE) .

    END METHOD .
&ENDIF
END CLASS.
