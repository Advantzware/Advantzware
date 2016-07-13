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
    File        : UltraTreeColumnResizeHelper
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu May 22 13:20:21 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.Util.*   FROM PROPATH .  
USING Infragistics.Win.UltraWinTree.* FROM ASSEMBLY .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Windows.Util.UltraTreeColumnResizeHelper: 

    DEFINE PRIVATE TEMP-TABLE ttNodeLevelSyncronization NO-UNDO 
        FIELD ChildNodeLevel     AS INTEGER 
        FIELD ParentColumnOffset AS INTEGER 
        FIELD ChildColumnOffset  AS INTEGER 
        INDEX idxMain IS PRIMARY UNIQUE ChildNodeLevel
        .
    
    /*------------------------------------------------------------------------------
        Purpose: Default column width to be used if nothing else is defined
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY DefaultColumnWidth AS INTEGER INITIAL 65 NO-UNDO 
	GET.
	SET. 

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
        Purpose: Constructor for the UltraTreeColumnResizeHelper class
        Notes:   
        @param poTree The UltraTree to be used for resizing
        @param piPreferredCellSize The prefered cell size to be applied
        @param pcFirstColumnKey The reference to the first column of the tree
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UltraTreeColumnResizeHelper (poTree AS UltraTree, 
	                                                piPreferredCellSize AS INTEGER,
	                                                pcFirstColumnKey AS CHARACTER):

        DEFINE VARIABLE oColumnSet      AS UltraTreeColumnSet  NO-UNDO.
        DEFINE VARIABLE oTreeNodeColumn AS UltraTreeNodeColumn NO-UNDO.

		SUPER ().
		
		Consultingwerk.Assertion.ObjectAssert:IsValid (poTree).
		
        ASSIGN 
            THIS-OBJECT:Tree               = poTree
            THIS-OBJECT:PreferredCellSize  = piPreferredCellSize
            THIS-OBJECT:FirstColumnKey     = pcFirstColumnKey
            .

        IF NOT THIS-OBJECT:Tree:ColumnSettings:ColumnSets:Count > 0 THEN 
            UNDO, THROW NEW AppError ("An initial ColumnSet is needed in order to initialize the resizing!":U, 0).
            
        oColumnSet = THIS-OBJECT:Tree:ColumnSettings:ColumnSets[0].

        IF NOT oColumnSet:Columns:Exists (THIS-OBJECT:FirstColumnKey) THEN 
            UNDO, THROW NEW AppError (SUBSTITUTE ("Need a Column (&1) to continue.":U, QUOTER (THIS-OBJECT:FirstColumnKey)), 0).
        
        oTreeNodeColumn = oColumnSet:Columns[THIS-OBJECT:FirstColumnKey].
        
        ASSIGN 
            oTreeNodeColumn:LayoutInfo:PreferredCellSize  = NEW System.Drawing.Size (THIS-OBJECT:PreferredCellSize, 0)
            oTreeNodeColumn:LayoutInfo:PreferredLabelSize = NEW System.Drawing.Size (THIS-OBJECT:PreferredCellSize, 0)
            .
		
		THIS-OBJECT:Tree:AfterNodeLayoutItemResize:Subscribe (AfterNodeLayoutItemResizeHandler).
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Occurs after a node layout item (i.e., a column header or cell) is 
                 resized by the end user.
        Notes:   
        @param sender The sending object
        @param e An AfterNodeLayoutItemResizeEventArgs that has the data of the event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID AfterNodeLayoutItemResizeHandler (sender AS System.Object, 
	                                                        e AS AfterNodeLayoutItemResizeEventArgs):
		
        DEFINE VARIABLE iCurrentNodeLevel AS INTEGER NO-UNDO.
        
		Tree:SuspendLayout ().
		Tree:BeginUpdate ().
		
		iCurrentNodeLevel = Tree:ColumnSettings:ColumnSets:IndexOf (e:Column:ColumnSet). 
		
        /* Parent-Resize */
        FIND FIRST ttNodeLevelSyncronization WHERE ChildNodeLevel = iCurrentNodeLevel + 1 NO-LOCK NO-ERROR.

        IF AVAILABLE (ttNodeLevelSyncronization) THEN DO:
            ParentResizeHandler (e:Column:ColumnSet, 
                                 Tree:ColumnSettings:ColumnSets[iCurrentNodeLevel + 1],
                                 ttNodeLevelSyncronization.ParentColumnOffset,
                                 ttNodeLevelSyncronization.ChildColumnOffset).
            
        END.
        
        /* Child-Resize */
        FIND FIRST ttNodeLevelSyncronization WHERE ChildNodeLevel = iCurrentNodeLevel NO-LOCK NO-ERROR.

        IF AVAILABLE (ttNodeLevelSyncronization) THEN DO:
            ChildResizeHandler (Tree:ColumnSettings:ColumnSets[iCurrentNodeLevel - 1],
                                e:Column:ColumnSet, 
                                ttNodeLevelSyncronization.ParentColumnOffset,
                                ttNodeLevelSyncronization.ChildColumnOffset).
        
        END.
            
        FINALLY:
            Tree:ResumeLayout ().
            Tree:EndUpdate ().
        END FINALLY.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets all columnsizes of a ColumnSet to a fixed width starting from 
                 the ColumnOffset
        Notes:   
        @param piNodeLevel The NodeLevel of the ColumnSet as INTEGER
        @param piColumnOffset The starting column as INTEGER
        @param piColumnWidth The width to be applied to all columns as INTEGER
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ApplyFixedColumnSizes (piNodeLevel AS INTEGER,
	                                          piColumnOffset AS INTEGER,
	                                          piColumnWidth AS INTEGER):
		DEFINE VARIABLE iColumns AS INTEGER NO-UNDO.
		DEFINE VARIABLE oColumnSet AS UltraTreeColumnSet NO-UNDO.
		
		oColumnSet = Tree:NodeLevelOverrides[piNodeLevel]:ColumnSet.
		
		Consultingwerk.Assertion.ObjectAssert:IsValid (oColumnSet).
		
		/* Marko Rüterbories / Consultingwerk Ltd. 03.12.2013
           A width smaller then 1 is not valid so we use the default width */
        IF piColumnWidth < 0 THEN 
            piColumnWidth = DefaultColumnWidth.
            
        DO iColumns = piColumnOffset - 1 TO oColumnSet:Columns:Count - 1:
            oColumnSet:Columns[iColumns]:LayoutInfo:PreferredCellSize = NEW System.Drawing.Size (piColumnWidth, 0).
            oColumnSet:Columns[iColumns]:LayoutInfo:PreferredLabelSize = NEW System.Drawing.Size (piColumnWidth, 0).
        END.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Resizes the Parent Collection after the Child has been resized
        Notes:   
        @param poParentColumnSet The Parent Node ColumnSet
        @param poChildColumnSet The Child Node ColumnSet
        @param piParentOffset The Parent Column Offset
        @param piChildOffset The Child Column Offset
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ChildResizeHandler (poParentColumnSet AS UltraTreeColumnSet,
                                              poChildColumnSet AS UltraTreeColumnSet,
                                              piParentOffset AS INTEGER,
                                              piChildOffset AS INTEGER):
		
        DEFINE VARIABLE iParentColumns    AS INTEGER NO-UNDO.
        DEFINE VARIABLE iChildWidth       AS INTEGER NO-UNDO.
        DEFINE VARIABLE iChildColumns     AS INTEGER NO-UNDO.
        DEFINE VARIABLE iCurrentNodeLevel AS INTEGER NO-UNDO.
        
        DO iChildColumns = 0 TO piChildOffset - 1:
            IF iChildColumns > poChildColumnSet:Columns:Count - 1 THEN LEAVE.

            iChildWidth = iChildWidth + poChildColumnSet:Columns[iChildColumns]:LayoutInfo:PreferredLabelSize:Width.
        END.
        
        iChildWidth = iChildWidth + 19.

        DO iParentColumns = 0 TO poParentColumnSet:Columns:Count - 1:

            IF iParentColumns - piChildOffset > poChildColumnSet:Columns:Count - 1 THEN LEAVE.
            
            IF iParentColumns > piParentOffset - 1 THEN 
                iChildWidth = poChildColumnSet:Columns[iParentColumns]:LayoutInfo:PreferredLabelSize:Width.
            
            IF iChildWidth < 0 THEN
                iChildWidth = 0.

            poParentColumnSet:Columns[iParentColumns]:LayoutInfo:PreferredCellSize = NEW System.Drawing.Size (iChildWidth, 0).
            poParentColumnSet:Columns[iParentColumns]:LayoutInfo:PreferredLabelSize = NEW System.Drawing.Size (iChildWidth, 0).
        END.

        iCurrentNodeLevel = Tree:ColumnSettings:ColumnSets:IndexOf (poParentColumnSet).

        /* Child-Resize */
        FIND FIRST ttNodeLevelSyncronization WHERE ChildNodeLevel = iCurrentNodeLevel NO-LOCK NO-ERROR.

        IF AVAILABLE (ttNodeLevelSyncronization) THEN DO:
            ChildResizeHandler (Tree:ColumnSettings:ColumnSets[iCurrentNodeLevel - 1],
                                poParentColumnSet,
                                ttNodeLevelSyncronization.ParentColumnOffset,
                                ttNodeLevelSyncronization.ChildColumnOffset).

        END.

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Clones every column of a NodeLevel based ColumnSet to another level
        Notes:   
        @param piSourceLevel The node level to use as source to copy the layout
        @param piTargetLevel The node level to copy the layout to
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID CloneColumnSet (piSourceLevel AS INTEGER,
                                        piTargetLevel AS INTEGER):
        
        DEFINE VARIABLE oColumnSet       AS UltraTreeColumnSet  NO-UNDO.
        DEFINE VARIABLE oCloneColumnSet  AS UltraTreeColumnSet  NO-UNDO.
        DEFINE VARIABLE oTreeNodeColumn  AS UltraTreeNodeColumn NO-UNDO.
        DEFINE VARIABLE iReducedCellSize AS INTEGER             NO-UNDO.
        
        IF NOT Tree:ColumnSettings:ColumnSets:Count >= piSourceLevel + 1 THEN 
            UNDO, THROW NEW AppError ("Source level ColumnSet does not exist!":U, 0).
        
        oColumnSet = Tree:ColumnSettings:ColumnSets[piSourceLevel].
        
        oCloneColumnSet = NEW UltraTreeColumnSet ().
        oCloneColumnSet:Key = SUBSTITUTE ("Level-&1":U, piTargetLevel).
        oCloneColumnSet:TipStyleCell = TipStyleCell:Show .

        {Consultingwerk/foreach.i UltraTreeNodeColumn oColumn in oColumnSet:Columns}
        
            oTreeNodeColumn = NEW UltraTreeNodeColumn ().

            IF oColumn:KEY = THIS-OBJECT:FirstColumnKey AND 
               oColumn:LayoutInfo:PreferredCellSize:Width > THIS-OBJECT:PreferredCellSize THEN 
                iReducedCellSize = 19. 
            ELSE 
                iReducedCellSize = 0. 

            ASSIGN
&IF NOT PROVERSION BEGINS "10" &THEN
                oTreeNodeColumn:ButtonDisplayStyle            = oColumn:ButtonDisplayStyle
&ENDIF                
                oTreeNodeColumn:CanShowExpansionIndicator     = oColumn:CanShowExpansionIndicator
                oTreeNodeColumn:CellWrapText                  = oColumn:CellWrapText
                oTreeNodeColumn:DataType                      = oColumn:DataType
                oTreeNodeColumn:Key                           = oColumn:Key
                oTreeNodeColumn:MaxLength                     = oColumn:MaxLength
                oTreeNodeColumn:LayoutInfo:PreferredCellSize  = NEW System.Drawing.Size (oColumn:LayoutInfo:PreferredCellSize:Width - iReducedCellSize, 0)
                oTreeNodeColumn:LayoutInfo:PreferredLabelSize = NEW System.Drawing.Size (oColumn:LayoutInfo:PreferredCellSize:Width - iReducedCellSize, 0)
                .
        
            oCloneColumnSet:Columns:Add (oTreeNodeColumn).
            
        END.

        Tree:ColumnSettings:ColumnSets:Add (oCloneColumnSet).
        
        Tree:NodeLevelOverrides[piSourceLevel]:ColumnSet = oColumnSet.
        Tree:NodeLevelOverrides[piTargetLevel]:ColumnSet = oCloneColumnSet.
        
    END METHOD.
        
    /*------------------------------------------------------------------------------
        Purpose: Checks if a NodeLevel based layout already exists. If not a new 
                 clone of the initial layout is created and assigned to that level.
        Notes:   
        @param piNodeLevel The level of the currently processed node
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeColumnResizing (piNodeLevel AS INTEGER):

        IF Tree:ColumnSettings:ColumnSets:Exists (SUBSTITUTE ("Level-&1":U, piNodeLevel)) THEN 
            RETURN.
         
        CloneColumnSet (0, piNodeLevel).
            
        RegisterNodeLevelsForSyncronization (piNodeLevel, 1, 1).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Resizes the Child Collection after the Parent has been resized
        Notes:   
        @param poParentColumnSet The Parent Node ColumnSet
        @param poChildColumnSet The Child Node ColumnSet
        @param piParentOffset The Parent Column Offset
        @param piChildOffset The Child Column Offset
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ParentResizeHandler (poParentColumnSet AS UltraTreeColumnSet,
	                                           poChildColumnSet AS UltraTreeColumnSet,
	                                           piParentOffset AS INTEGER,
	                                           piChildOffset AS INTEGER):
		
        DEFINE VARIABLE iParentColumns    AS INTEGER NO-UNDO.
        DEFINE VARIABLE iParentWidth      AS INTEGER NO-UNDO.
        DEFINE VARIABLE iChildColumns     AS INTEGER NO-UNDO.
        DEFINE VARIABLE iCurrentNodeLevel AS INTEGER NO-UNDO.
        
        DO iParentColumns = 0 TO piParentOffset - 1:
            /* Marko Rüterbories / Consultingwerk Ltd. 22.08.2013
               limit the iParentColumns to not run out of columns in the ColumnSet */
            IF iParentColumns > poParentColumnSet:Columns:Count - 1 THEN LEAVE.

            iParentWidth = iParentWidth + poParentColumnSet:Columns[iParentColumns]:LayoutInfo:PreferredLabelSize:Width.
        END.
        
        /* Marko Rüterbories / Consultingwerk Ltd. 20.08.2013
           Reduce the width by the amount of pixel for the indentation */
        iParentWidth = iParentWidth - 19.

        DO iChildColumns = 0 TO poChildColumnSet:Columns:Count - 1:
            IF iChildColumns > poParentColumnSet:Columns:Count - 1 OR 
               iChildColumns + piParentOffset - 1 > poParentColumnSet:Columns:Count - 1 THEN LEAVE.
            
            IF iChildColumns > piChildOffset - 1 THEN 
                iParentWidth = poParentColumnSet:Columns[iChildColumns + piParentOffset - 1]:LayoutInfo:PreferredLabelSize:Width.
            ELSE 
                IF iChildColumns < piChildOffset - 1 THEN NEXT.

            /* Marko Rüterbories / Consultingwerk Ltd. 03.12.2013
               A width smaller then 1 is not valid so we use the default width */
            IF iParentWidth < 0 THEN 
                iParentWidth = DefaultColumnWidth.

            poChildColumnSet:Columns[iChildColumns]:LayoutInfo:PreferredCellSize = NEW System.Drawing.Size (iParentWidth, 0).
            poChildColumnSet:Columns[iChildColumns]:LayoutInfo:PreferredLabelSize = NEW System.Drawing.Size (iParentWidth, 0).
        END.

        iCurrentNodeLevel = Tree:ColumnSettings:ColumnSets:IndexOf (poChildColumnSet). 
        
        /* Parent-Resize */
        FIND FIRST ttNodeLevelSyncronization WHERE ChildNodeLevel = iCurrentNodeLevel + 1 NO-LOCK NO-ERROR.

        IF AVAILABLE (ttNodeLevelSyncronization) THEN DO:
            ParentResizeHandler (poChildColumnSet, 
                                 Tree:ColumnSettings:ColumnSets[iCurrentNodeLevel + 1],
                                 ttNodeLevelSyncronization.ParentColumnOffset,
                                 ttNodeLevelSyncronization.ChildColumnOffset).
            
        END.

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds a new syncronization for NodeLevels
        Notes:   
        @param piChildNodeLevel An INTEGER value of the childs NodeLevel
        @param piParentColumnOffset First column to be syncronized as the base
        @param piChildColumnOffset First column to be syncronized with the parent offset
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID RegisterNodeLevelsForSyncronization (piChildNodeLevel AS INTEGER,
	                                                        piParentColumnOffset AS INTEGER,
	                                                        piChildColumnOffset AS INTEGER):
		
		FIND FIRST ttNodeLevelSyncronization WHERE ChildNodeLevel = piChildNodeLevel NO-LOCK NO-ERROR.
		
		IF NOT AVAILABLE (ttNodeLevelSyncronization) THEN
		    CREATE ttNodeLevelSyncronization.
		    
        ASSIGN
            ttNodeLevelSyncronization.ChildNodeLevel     = piChildNodeLevel
            ttNodeLevelSyncronization.ParentColumnOffset = piParentColumnOffset
            ttNodeLevelSyncronization.ChildColumnOffset  = piChildColumnOffset
            .
        
        RELEASE ttNodeLevelSyncronization. 
        
        ParentResizeHandler (Tree:ColumnSettings:ColumnSets[piChildNodeLevel - 1],
                             Tree:ColumnSettings:ColumnSets[piChildNodeLevel],
                             piParentColumnOffset,
                             piChildColumnOffset).
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Removes a registered syncronization
        Notes:   
        @param piChildNodeLevel An INTEGER value of the childs NodeLevel
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID UnregisterNodeLevelsForSyncronization (piChildNodeLevel AS INTEGER):
		
        FIND FIRST ttNodeLevelSyncronization WHERE ChildNodeLevel = piChildNodeLevel EXCLUSIVE-LOCK NO-ERROR.
        
        IF AVAILABLE (ttNodeLevelSyncronization) THEN 
            DELETE ttNodeLevelSyncronization.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the UltraTreeColumnResizeHelper class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC UltraTreeColumnResizeHelper ():
        
        IF VALID-OBJECT (Tree) THEN 
            THIS-OBJECT:Tree:AfterNodeLayoutItemResize:Unsubscribe (AfterNodeLayoutItemResizeHandler).
        
        EMPTY TEMP-TABLE ttNodeLevelSyncronization.
        
    END DESTRUCTOR .

END CLASS.
