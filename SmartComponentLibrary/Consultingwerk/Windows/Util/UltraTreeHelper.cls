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
    File        : UltraTreeHelper
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu May 22 13:20:21 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*      FROM PROPATH .
USING Consultingwerk.Windows.Util.*   FROM PROPATH .
USING Infragistics.Win.UltraWinTree.* FROM ASSEMBLY .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Windows.Util.UltraTreeHelper: 

    DEFINE VARIABLE oColumnResizer AS UltraTreeColumnResizeHelper NO-UNDO.
    
    /*------------------------------------------------------------------------------
        Purpose: Instance of a TreeColumnResizeHelper to do the resizing of the
                 columns like defined
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ColumnResizer AS UltraTreeColumnResizeHelper NO-UNDO 
	GET:
	    IF NOT VALID-OBJECT (oColumnResizer) THEN 
	       oColumnResizer = NEW UltraTreeColumnResizeHelper (THIS-OBJECT:Tree, 
	                                                         THIS-OBJECT:PreferredCellSize, 
	                                                         THIS-OBJECT:FirstColumnKey).
	    
	    RETURN oColumnResizer.
	END GET.

    /*------------------------------------------------------------------------------
        Purpose: Reference to first column of the Tree
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FirstColumnKey AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: The preferred cell size to be applied
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PreferredCellSize AS INTEGER NO-UNDO
    GET.
    PRIVATE SET. 


    /*------------------------------------------------------------------------------
        Purpose: Tree reference for this Helper
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Tree AS UltraTree NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UltraTreeHelper class
        Notes:   
        @param poTree An UltraTree instance which has to be valid.
        @param piPreferredCellSize The prefered cell size to be applied
        @param pcFirstColumnKey The reference to the first column of the tree
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UltraTreeHelper (poTree AS UltraTree, 
                                        piPreferredCellSize AS INTEGER,
                                        pcFirstColumnKey AS CHARACTER):
		SUPER ().
		
		Consultingwerk.Assertion.ObjectAssert:IsValid (poTree).
		
        ASSIGN 
            THIS-OBJECT:Tree              = poTree
            THIS-OBJECT:PreferredCellSize = piPreferredCellSize
            THIS-OBJECT:FirstColumnKey    = pcFirstColumnKey
            .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Creates a new column on one of the Node Level based ColumnSets
        Notes:   Always add the Column at the end of the list
        @param piNodeLevel The Level of the Node for which a column is created
        @param pcColumnKey The Key of the new column
        @param pcColumnLabel The Label of the new column
        @param pcDataType The DataType of the new column
        @return The created UltraTreeNodeColumn
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC UltraTreeNodeColumn AddColumnToNodeLevelColumnSet (piNodeLevel AS INTEGER,
                                                                     pcColumnKey AS CHARACTER,
                                                                     pcColumnLabel AS CHARACTER,
                                                                     pcDataType AS CHARACTER):
        RETURN AddColumnToNodeLevelColumnSet (piNodeLevel,
                                              pcColumnKey,
                                              pcColumnLabel,
                                              pcDataType,
                                              -1).
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Creates a new column on one of the Node Level based ColumnSets
        Notes:   Add the column at a specific position
        @param piNodeLevel The Level of the Node for which a column is created
        @param pcColumnKey The Key of the new column
        @param pcColumnLabel The Label of the new column
        @param pcDataType The DataType of the new column
        @param piPosition The Position of the new column
        @return The created UltraTreeNodeColumn
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC UltraTreeNodeColumn AddColumnToNodeLevelColumnSet (piNodeLevel AS INTEGER,
                	                                                 pcColumnKey AS CHARACTER,
                	                                                 pcColumnLabel AS CHARACTER,
                	                                                 pcDataType AS CHARACTER,
                	                                                 piPosition AS INTEGER):
        
        DEFINE VARIABLE oColumnSet           AS UltraTreeColumnSet  NO-UNDO.
        DEFINE VARIABLE oUltraTreeNodeColumn AS UltraTreeNodeColumn NO-UNDO.
        DEFINE VARIABLE lNewColumn           AS LOGICAL             NO-UNDO.
        
        oColumnSet = GetOrCreateNodeLevelBasedColumnSet (piNodeLevel).
        
        oUltraTreeNodeColumn = CreateColumnInColumnSet (oColumnSet, pcColumnKey, OUTPUT lNewColumn).
        
        IF lNewColumn THEN DO:
            oUltraTreeNodeColumn:Text = pcColumnLabel.
            
            oUltraTreeNodeColumn:DataType = Progress.Util.TypeHelper:GetType (pcDataType).
        END.
        
        RETURN oUltraTreeNodeColumn.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creates or returns a ColumnSet of the Tree
        Notes:   
        @param poColumnSet Description
        @param pcColumnKey The key of the new column
        @param plNewColumn Flags if the column was newly added or preexisting
        @return The new Infragistics.Win.UltraWinTree.UltraTreeNodeColumn
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED UltraTreeNodeColumn CreateColumnInColumnSet (poColumnSet AS UltraTreeColumnSet,
	                                                              pcColumnKey AS CHARACTER,
	                                                              OUTPUT plNewColumn AS LOGICAL):
		
		DEFINE VARIABLE oUltraTreeNodeColumn AS UltraTreeNodeColumn NO-UNDO.
		
        IF NOT poColumnSet:Columns:Exists (pcColumnKey) THEN 
            ASSIGN 
                oUltraTreeNodeColumn = poColumnSet:Columns:Add (pcColumnKey)
                plNewColumn          = TRUE
                .
        ELSE 
            ASSIGN 
                oUltraTreeNodeColumn = poColumnSet:Columns[pcColumnKey]
                plNewColumn          = FALSE 
                .

        RETURN oUltraTreeNodeColumn.
        
	END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Returns a list of all nodes in the tree 
	    Notes:   Recursively processes all nodes
	    @param poTree The UltraTree to return all Nodes from 
	    @return The List<UltraTreeNode> with all nodes of the tree 
	------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC "System.Collections.Generic.List<UltraTreeNode>" GetAllNodes (poTree AS UltraTree):
		
		DEFINE VARIABLE oList AS "System.Collections.Generic.List<UltraTreeNode>" NO-UNDO . 
		
		ObjectAssert:IsValid (poTree, "poTree":U) .
		
		oList = NEW "System.Collections.Generic.List<UltraTreeNode>" () . 
		
		UltraTreeHelper:GetAllNodes (poTree:Nodes, oList) .
		
		RETURN oList .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a list of all nodes in the tree 
        Notes:   Recursively processes all nodes
        @param poNodes The TreeNodesCollection to process 
        @param poList The List<UltraTreeNode> to add nodes to  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID GetAllNodes (poNodes AS TreeNodesCollection,
                                              poList AS "System.Collections.Generic.List<UltraTreeNode>"):
        
        {Consultingwerk/foreach.i UltraTreeNode oNode in poNodes}

            poList:Add (oNode) .
            
            IF oNode:Nodes:Count > 0 THEN 
                UltraTreeHelper:GetAllNodes (oNode:Nodes, poList) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates or returns a ColumnSet for the Tree and assigns it to a 
                 NodeLevelOverride
        Notes:   
        @param piNodeLevel The Level of the Node for which a column is created
        @return The reference to the Infragistics.Win.UltraWinTree.UltraTreeColumnSet
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC UltraTreeColumnSet GetOrCreateNodeLevelBasedColumnSet (piNodeLevel AS INTEGER):
        DEFINE VARIABLE oColumnSet    AS UltraTreeColumnSet NO-UNDO.
        DEFINE VARIABLE cColumnSetKey AS CHARACTER          NO-UNDO.
        
        ASSIGN cColumnSetKey = SUBSTITUTE ("TreeHelperColumnSet_&1":U, piNodeLevel).
        
        IF Tree:ColumnSettings:ColumnSets:Exists (cColumnSetKey) THEN 
            oColumnSet = Tree:ColumnSettings:ColumnSets[cColumnSetKey].
        ELSE DO: 
            oColumnSet = Tree:ColumnSettings:ColumnSets:Add (cColumnSetKey).
            
            Tree:NodeLevelOverrides[piNodeLevel]:ColumnSet = oColumnSet.
        END.

		RETURN oColumnSet.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns an existing node from the collection or adds a new one
        Notes:   
        @param poTreeNodesCollection The TreeNodesCollection to add the new UltraTreeNode to
        @param pcNodeKey The key of the new Node
        @param pcNodeLabel The label of the new Node
        @return The new UltraTreeNode instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Infragistics.Win.UltraWinTree.UltraTreeNode GetOrCreateNode (poTreeNodesCollection AS TreeNodesCollection,
                                                                               pcNodeKey AS CHARACTER,
                                                                               pcNodeLabel AS CHARACTER):
        
        DEFINE VARIABLE oNode AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.

        IF poTreeNodesCollection:Exists (pcNodeKey) THEN 
            oNode = poTreeNodesCollection:Item[pcNodeKey].
        ELSE 
            oNode = poTreeNodesCollection:Add (pcNodeKey, pcNodeLabel).
        
        RETURN oNode.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Hides the ColumnHeader of a given NodeLevel
        Notes:   
        @param piNodeLevel The INTEGER value of the NodeLevel to be changed
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID HideColumnHeaderForNodeLevel (piNodeLevel AS INTEGER):
		
        DEFINE VARIABLE oColumnSet AS UltraTreeColumnSet  NO-UNDO.
        
        oColumnSet = GetOrCreateNodeLevelBasedColumnSet (piNodeLevel).
		
		oColumnSet:LabelPosition = Infragistics.Win.UltraWinTree.NodeLayoutLabelPosition:None. 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Set Properties of the Tree
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ParameterizeTree ():
		
/*        Tree:ColumnSettings:AllowCellEdit = Infragistics.Win.UltraWinTree.AllowCellEdit:ReadOnly.              */
/*        Tree:ColumnSettings:AllowCellSizing = Infragistics.Win.UltraWinTree.LayoutSizing:Both.                 */
/*        Tree:ColumnSettings:AutoGenerateColumnSets = FALSE.                                                    */
/*        Tree:ColumnSettings:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.                    */
/*        Tree:ColumnSettings:BorderStyleColumnHeader = Infragistics.Win.UIElementBorderStyle:None.              */
/*        Tree:ColumnSettings:ColumnAutoSizeMode = Infragistics.Win.UltraWinTree.ColumnAutoSizeMode:VisibleNodes.*/
/*        Tree:ColumnSettings:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsVista.                           */
/*        Tree:ColumnSettings:LabelPosition = Infragistics.Win.UltraWinTree.NodeLayoutLabelPosition:None.        */
/*        Tree:DisplayStyle = Infragistics.Win.UltraWinTree.UltraTreeDisplayStyle:WindowsVista.                  */
/*        Tree:FullRowSelect = TRUE.                                                                             */
/*        Tree:HideSelection = FALSE.                                                                            */
/*        Tree:ImageTransparentColor = System.Drawing.Color:Transparent.                                         */
/*        Tree:NodeConnectorColor = System.Drawing.SystemColors:ControlDark.                                     */
        Tree:ViewStyle = Infragistics.Win.UltraWinTree.ViewStyle:Grid.
        Tree:ColumnSettings:AllowColMoving = Infragistics.Win.Layout.GridBagLayoutAllowMoving:None.
        Tree:ColumnSettings:AllowSorting = Infragistics.Win.DefaultableBoolean:False.

        /* Marko Rüterbories / Consultingwerk Ltd. 15.11.2013
           Enable multiline headers */
        Tree:ColumnSettings:ColumnHeaderWrapText = Infragistics.Win.DefaultableBoolean:True.
        
        Tree:SupportThemes = FALSE.
        Tree:UseOsThemes = Infragistics.Win.DefaultableBoolean:FALSE.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Replaces a column identified by the OldColumnKey on one of the Node 
                 Level based ColumnSets
        Notes:   
        @param piNodeLevel The Level of the Node for which a column is replaced
        @param pcOldColumnKey The Key of the column to be replaced
        @param pcNewColumnKey The new Key of the column
        @param pcNewColumnLabel The new Label of the column
        @param pcDataType The DataType of the column
        @return The created UltraTreeNodeColumn
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC UltraTreeNodeColumn ReplaceColumnInNodeLevelColumnSet (piNodeLevel AS INTEGER,
                	                                                     pcOldColumnKey AS CHARACTER,
                                                                         pcNewColumnKey AS CHARACTER,
                                                                         pcNewColumnLabel AS CHARACTER,
                                                                         pcDataType AS CHARACTER):
        
        DEFINE VARIABLE oColumnSet           AS UltraTreeColumnSet  NO-UNDO.
        DEFINE VARIABLE oUltraTreeNodeColumn AS UltraTreeNodeColumn NO-UNDO.
        DEFINE VARIABLE lNewColumn           AS LOGICAL             NO-UNDO.
        
        oColumnSet = GetOrCreateNodeLevelBasedColumnSet (piNodeLevel).

        IF oColumnSet:Columns:Exists (pcNewColumnKey) THEN 
            ASSIGN 
                oUltraTreeNodeColumn = oColumnSet:Columns[pcNewColumnKey]
                .
        ELSE DO:
            oUltraTreeNodeColumn = CreateColumnInColumnSet (oColumnSet, pcOldColumnKey, OUTPUT lNewColumn).
        
            oUltraTreeNodeColumn:Key  = pcNewColumnKey.
        END.
        
        oUltraTreeNodeColumn:Text = pcNewColumnLabel.
        
        oUltraTreeNodeColumn:DataType = Progress.Util.TypeHelper:GetType (pcDataType).
        
        RETURN oUltraTreeNodeColumn.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Resets a list of NodeLevelOverrides
        Notes:   
        @param piStartNodeLevel The start NodeLevel to process
        @param piStopNodeLevel The stop NodeLevel to process
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ResetNodeLevelOverride (piStartNodeLevel AS INTEGER,
                                               piStopNodeLevel AS INTEGER):

        DEFINE VARIABLE iLevel AS INTEGER NO-UNDO.
        
        DO iLevel = piStartNodeLevel TO piStartNodeLevel :
            ResetNodeLevelOverride (iLevel).
        END.
        
    END.

    /*------------------------------------------------------------------------------
        Purpose: Resets the node collection of a NodeLevelOverride
        Notes:   
        @param piNodeLevel The NodeLevel to process
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ResetNodeLevelOverride (piNodeLevel AS INTEGER):

        DEFINE VARIABLE oColumnSet           AS UltraTreeColumnSet  NO-UNDO.
        
        oColumnSet = GetOrCreateNodeLevelBasedColumnSet (piNodeLevel).
        
        oColumnSet:Columns:Clear ().
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Enable the NodeColumns to be editable
        Notes:   
        @param poTreeNode The Node which has to be editable
        @param piColumnOffset Index of the Column to start eddit in. The others won't be editable
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SetNodeColumnsEditable (poTreeNode AS UltraTreeNode, 
	                                           piColumnOffset AS INTEGER):
		
        DEFINE VARIABLE iCell AS INTEGER NO-UNDO.
        
        poTreeNode:Override:CellClickAction = Infragistics.Win.UltraWinTree.CellClickAction:EditCellSelectText.
        
        DO iCell = 0 TO poTreeNode:Cells:Count - 1 :
            IF iCell < piColumnOffset - 1 THEN 
                poTreeNode:Cells[iCell]:AllowEdit = Infragistics.Win.UltraWinTree.AllowCellEdit:ActivateOnly.
            ELSE
                poTreeNode:Cells[iCell]:AllowEdit = Infragistics.Win.UltraWinTree.AllowCellEdit:Full.
        END.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the UltraTreeHelper class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC UltraTreeHelper ():
        
        Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oColumnResizer).
        
    END DESTRUCTOR .

END CLASS.
