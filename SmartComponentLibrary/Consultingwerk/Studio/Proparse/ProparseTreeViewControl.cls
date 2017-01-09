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
    File        : ProparseTreeViewControl
    Purpose     : Visualizes the propaser AST in a tree view
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jun 28 20:27:18 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (DotNetAccessible) NE 0 &THEN 
USING Consultingwerk.Windows.Util.*   FROM PROPATH.
USING Infragistics.Win.UltraWinTree.* FROM ASSEMBLY . 
USING org.prorefactor.refactor.*      FROM ASSEMBLY.
USING org.prorefactor.treeparser.*    FROM ASSEMBLY.
USING org.prorefactor.core.*          FROM ASSEMBLY.
USING com.joanju.proparse.NodeTypes   FROM ASSEMBLY .
USING Progress.Lang.*                 FROM PROPATH .
USING Progress.Windows.*              FROM ASSEMBLY .
&ENDIF

CLASS Consultingwerk.Studio.Proparse.ProparseTreeViewControl 
&IF DEFINED (DotNetAccessible) NE 0 &THEN 
    INHERITS UserControl
&ENDIF
    : 
    
&IF DEFINED (DotNetAccessible) NE 0 &THEN 
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTree1 AS Infragistics.Win.UltraWinTree.UltraTree NO-UNDO.
    
    DEFINE PRIVATE VARIABLE oUltraTreeColumnResizeHelper AS UltraTreeColumnResizeHelper NO-UNDO.
        
        
    /*------------------------------------------------------------------------------
        Purpose: Returns the Reference to the Dictionary of TreeNodes                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Nodes AS "System.Collections.Generic.Dictionary<System.Int32, org.prorefactor.core.JPNode>" NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ProparseTreeViewControl class                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProparseTreeViewControl ():

        DEFINE VARIABLE oColumnSet      AS UltraTreeColumnSet  NO-UNDO.
        DEFINE VARIABLE oTreeNodeColumn AS UltraTreeNodeColumn NO-UNDO.
        
        SUPER().
        InitializeComponent().
        
        THIS-OBJECT:Nodes = NEW "System.Collections.Generic.Dictionary<System.Int32, org.prorefactor.core.JPNode>" (). 
        
        Consultingwerk.Util.GarbageCollectorHelper:ClearUltraTreeNodes (THIS-OBJECT:ultraTree1) .

        THIS-OBJECT:ultraTree1:Nodes:Clear () .

        /* Initialize resizing for NodeColumnSets */
        oUltraTreeColumnResizeHelper = NEW UltraTreeColumnResizeHelper (THIS-OBJECT:ultraTree1,
                                                                        175,
                                                                        "TYPE":U).
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Key of the currently selected tree node                                                                        
        Notes:                                                      
        @return The Key of the currently selected tree node                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetCurrentNodeKey ():
        
        IF THIS-OBJECT:ultraTree1:SelectedNodes:Count > 0 THEN 
            RETURN THIS-OBJECT:ultraTree1:SelectedNodes[0]:Key .

        UNDO, THROW NEW AppError ("No node is currently selected."{&TRAN}, 0) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a JPNode by a node number                                                                       
        Notes:                                    
        @param piNodeNum The node number to return
        @return The refernce to the specified JPNode                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC JPNode GetJPNode (piNodeNum AS INTEGER):
        
        RETURN THIS-OBJECT:Nodes[piNodeNum] .
        
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
        DEFINE VARIABLE ultraTreeColumnSet1 AS Infragistics.Win.UltraWinTree.UltraTreeColumnSet NO-UNDO.
        ultraTreeColumnSet1 = NEW Infragistics.Win.UltraWinTree.UltraTreeColumnSet().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNodeColumn1 AS Infragistics.Win.UltraWinTree.UltraTreeNodeColumn NO-UNDO.
        ultraTreeNodeColumn1 = NEW Infragistics.Win.UltraWinTree.UltraTreeNodeColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNodeColumn2 AS Infragistics.Win.UltraWinTree.UltraTreeNodeColumn NO-UNDO.
        ultraTreeNodeColumn2 = NEW Infragistics.Win.UltraWinTree.UltraTreeNodeColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNodeColumn3 AS Infragistics.Win.UltraWinTree.UltraTreeNodeColumn NO-UNDO.
        ultraTreeNodeColumn3 = NEW Infragistics.Win.UltraWinTree.UltraTreeNodeColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNodeColumn4 AS Infragistics.Win.UltraWinTree.UltraTreeNodeColumn NO-UNDO.
        ultraTreeNodeColumn4 = NEW Infragistics.Win.UltraWinTree.UltraTreeNodeColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNodeColumn5 AS Infragistics.Win.UltraWinTree.UltraTreeNodeColumn NO-UNDO.
        ultraTreeNodeColumn5 = NEW Infragistics.Win.UltraWinTree.UltraTreeNodeColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNodeColumn6 AS Infragistics.Win.UltraWinTree.UltraTreeNodeColumn NO-UNDO.
        ultraTreeNodeColumn6 = NEW Infragistics.Win.UltraWinTree.UltraTreeNodeColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode1 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode1 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode2 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode2 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode3 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode3 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode4 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode4 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE m_override1 AS Infragistics.Win.UltraWinTree.Override NO-UNDO.
        m_override1 = NEW Infragistics.Win.UltraWinTree.Override().
        THIS-OBJECT:ultraTree1 = NEW Infragistics.Win.UltraWinTree.UltraTree().
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTree1 */
        /*  */
        THIS-OBJECT:ultraTree1:ColumnSettings:AllowCellEdit = Infragistics.Win.UltraWinTree.AllowCellEdit:ReadOnly.
        THIS-OBJECT:ultraTree1:ColumnSettings:AllowCellSizing = Infragistics.Win.UltraWinTree.LayoutSizing:Both.
        THIS-OBJECT:ultraTree1:ColumnSettings:AutoGenerateColumnSets = FALSE.
        THIS-OBJECT:ultraTree1:ColumnSettings:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:ultraTree1:ColumnSettings:BorderStyleColumnHeader = Infragistics.Win.UIElementBorderStyle:None.
        THIS-OBJECT:ultraTree1:ColumnSettings:ColumnAutoSizeMode = Infragistics.Win.UltraWinTree.ColumnAutoSizeMode:VisibleNodes.
        THIS-OBJECT:ultraTree1:ColumnSettings:ColumnHeaderWrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraTreeNodeColumn1:ButtonDisplayStyle = Infragistics.Win.UltraWinTree.ButtonDisplayStyle:Always.
        ultraTreeNodeColumn1:CanShowExpansionIndicator = Infragistics.Win.DefaultableBoolean:False.
        ultraTreeNodeColumn1:CellWrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraTreeNodeColumn1:DataType = Progress.Util.TypeHelper:GetType("System.String":U).
        ultraTreeNodeColumn1:Key = "TYPE":U.
        ultraTreeNodeColumn1:MaxLength = 500.
        ultraTreeNodeColumn2:ButtonDisplayStyle = Infragistics.Win.UltraWinTree.ButtonDisplayStyle:Always.
        ultraTreeNodeColumn2:Key = "SUBTYPE":U.
        ultraTreeNodeColumn3:ButtonDisplayStyle = Infragistics.Win.UltraWinTree.ButtonDisplayStyle:Always.
        ultraTreeNodeColumn3:CanShowExpansionIndicator = Infragistics.Win.DefaultableBoolean:False.
        ultraTreeNodeColumn3:CellWrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraTreeNodeColumn3:DataType = Progress.Util.TypeHelper:GetType("System.String":U).
        ultraTreeNodeColumn3:Key = "Statement":U.
        ultraTreeNodeColumn3:MaxLength = 500.
        ultraTreeNodeColumn4:ButtonDisplayStyle = Infragistics.Win.UltraWinTree.ButtonDisplayStyle:Always.
        ultraTreeNodeColumn4:CanShowExpansionIndicator = Infragistics.Win.DefaultableBoolean:False.
        ultraTreeNodeColumn4:CellWrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraTreeNodeColumn4:DataType = Progress.Util.TypeHelper:GetType("System.String":U).
        ultraTreeNodeColumn4:Key = "ToString":U.
        ultraTreeNodeColumn4:MaxLength = 500.
        ultraTreeNodeColumn5:ButtonDisplayStyle = Infragistics.Win.UltraWinTree.ButtonDisplayStyle:Always.
        ultraTreeNodeColumn5:DataType = Progress.Util.TypeHelper:GetType("System.Int32":U).
        ultraTreeNodeColumn5:Key = "Row":U.
        ultraTreeNodeColumn6:ButtonDisplayStyle = Infragistics.Win.UltraWinTree.ButtonDisplayStyle:Always.
        ultraTreeNodeColumn6:DataType = Progress.Util.TypeHelper:GetType("System.Int32":U).
        ultraTreeNodeColumn6:Key = "Column":U.
        ultraTreeColumnSet1:Columns:Add(ultraTreeNodeColumn1).
        ultraTreeColumnSet1:Columns:Add(ultraTreeNodeColumn2).
        ultraTreeColumnSet1:Columns:Add(ultraTreeNodeColumn3).
        ultraTreeColumnSet1:Columns:Add(ultraTreeNodeColumn4).
        ultraTreeColumnSet1:Columns:Add(ultraTreeNodeColumn5).
        ultraTreeColumnSet1:Columns:Add(ultraTreeNodeColumn6).
        ultraTreeColumnSet1:Key = "Level-0":U.
        THIS-OBJECT:ultraTree1:ColumnSettings:ColumnSets:Add(ultraTreeColumnSet1).
        THIS-OBJECT:ultraTree1:ColumnSettings:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsVista.
        THIS-OBJECT:ultraTree1:ColumnSettings:LabelPosition = Infragistics.Win.UltraWinTree.NodeLayoutLabelPosition:None.
        THIS-OBJECT:ultraTree1:DisplayStyle = Infragistics.Win.UltraWinTree.UltraTreeDisplayStyle:WindowsVista.
        THIS-OBJECT:ultraTree1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraTree1:FullRowSelect = TRUE.
        THIS-OBJECT:ultraTree1:HideSelection = FALSE.
        THIS-OBJECT:ultraTree1:ImageTransparentColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraTree1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraTree1:Name = "ultraTree1":U.
        THIS-OBJECT:ultraTree1:NodeConnectorColor = System.Drawing.SystemColors:ControlDark.
        ultraTreeNode3:Text = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTree.UltraTreeNode EXTENT 1 NO-UNDO.
        arrayvar0[1] = ultraTreeNode3.
        ultraTreeNode2:Nodes:AddRange(arrayvar0).
        ultraTreeNode2:Text = "":U.
        ultraTreeNode4:Text = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinTree.UltraTreeNode EXTENT 2 NO-UNDO.
        arrayvar1[1] = ultraTreeNode2.
        arrayvar1[2] = ultraTreeNode4.
        ultraTreeNode1:Nodes:AddRange(arrayvar1).
        ultraTreeNode1:Text = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinTree.UltraTreeNode EXTENT 1 NO-UNDO.
        arrayvar2[1] = ultraTreeNode1.
        THIS-OBJECT:ultraTree1:Nodes:AddRange(arrayvar2).
        m_override1:ColumnSetIndex = 0.
        THIS-OBJECT:ultraTree1:Override = m_override1.
        THIS-OBJECT:ultraTree1:Size = NEW System.Drawing.Size(223, 366).
        THIS-OBJECT:ultraTree1:TabIndex = 0.
        THIS-OBJECT:ultraTree1:ViewStyle = Infragistics.Win.UltraWinTree.ViewStyle:Grid.
        /*  */
        /* ProparseTreeViewControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTree1).
        THIS-OBJECT:Name = "ProparseTreeViewControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(223, 366).
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Loads the tree from a ParseUnit                                                                       
        Notes:                                  
        @param poParseUnit The ParseUnit to populate the tree from                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID LoadTree (poParseUnit AS ParseUnit):
        
        THIS-OBJECT:ultraTree1:BeginUpdate() .
        THIS-OBJECT:ultraTree1:SuspendLayout() .
        
        THIS-OBJECT:Nodes:Clear () .
        
        Consultingwerk.Util.GarbageCollectorHelper:ClearUltraTreeNodes (THIS-OBJECT:ultraTree1) .
        THIS-OBJECT:ultraTree1:Nodes:Clear() .

        WalkAST (poParseUnit:getTopNode():firstchild(), 
                 THIS-OBJECT:ultraTree1:Nodes).

        FINALLY:
            THIS-OBJECT:ultraTree1:EndUpdate () .
            THIS-OBJECT:ultraTree1:ResumeLayout (TRUE) .            
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Walks a level of the AST                                                                       
        Notes:                              
        @param poASTNode The current parent AST node reference
        @param poTreeNodes The current TreeNodesCollection to add the nodes to                                          
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID WalkAST (poASTNode AS JPNode, 
                                   poTreeNodes AS TreeNodesCollection):
    
        DEFINE VARIABLE oTreeNode AS UltraTreeNode NO-UNDO .

        IF VALID-OBJECT (poTreeNodes:ParentNode) THEN 
            oUltraTreeColumnResizeHelper:InitializeColumnResizing (poTreeNodes:ParentNode:Level + 1).
        
        DO WHILE VALID-OBJECT (poASTNode):
            oTreeNode = poTreeNodes:Add (STRING (poASTNode:getNodeNum ())) . 
            
            THIS-OBJECT:Nodes:Add (poASTNode:getNodeNum (), poASTNode) .
            
            oTreeNode:Cells ["TYPE":U]:Value = NodeTypes:getTypeName(poASTNode:getType()) .
            oTreeNode:Cells ["SUBTYPE":U]:Value = Consultingwerk.Studio.Proparse.ProparseHelper:GetSubtypeName (poASTNode:getSubTypeIndex()) .
            oTreeNode:Cells ["ToString":U]:Value = poASTNode:ToString() .
            oTreeNode:Cells ["Statement":U]:Value = poASTNode:getText() .
            oTreeNode:Cells ["Row":U]:Value = BOX(poASTNode:getLine ()) .
            oTreeNode:Cells ["Column":U]:Value = BOX (poASTNode:getColumn()) .
    
            IF VALID-OBJECT (poASTNode:firstChild()) THEN 
                WalkAST (poASTNode:firstChild(), oTreeNode:Nodes).
        
            poASTNode = poASTNode:nextSibling () .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ProparseTreeViewControl class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ProparseTreeViewControl ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.
&ENDIF
END CLASS.