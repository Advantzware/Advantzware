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
    File        : SmartMenuMaintenanceForm
    Purpose     : Maintenance of the Smart Menu Structure
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu Oct 11 01:47:52 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Enum.*                     FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.SmartWindowForm FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*               FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*     FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*         FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.*       FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.*                FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Consultingwerk.Windows.Framework.Menu.*             FROM PROPATH . 
USING Infragistics.Win.UltraWinToolbars.*                 FROM ASSEMBLY . 
USING Progress.Lang.*                                     FROM PROPATH .
USING System.*                                            FROM ASSEMBLY .
USING System.Collections.Generic.*                        FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Framework.Menu.SmartMenuMaintenanceForm 
    INHERITS SmartWindowForm
    IMPLEMENTS IDefaultActionSubscriber: 
    
    DEFINE PRIVATE VARIABLE ButtonUnAssignGroupFromMenu AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ButtonAssignGroupToMenu AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE MenuBindingSource AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE MenuAdapter AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE MenuGroupAdapter AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE MenuGroupBindingSource AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE MenuTree AS Infragistics.Win.UltraWinTree.UltraTree NO-UNDO.
    DEFINE PRIVATE VARIABLE PossibleGroupBindingSource AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE PossibleGroupTableAdapter AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDataBrowser3 AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDataBrowser2 AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartMenuViewer1 AS Consultingwerk.Windows.Framework.Menu.SmartMenuViewer NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.
    DEFINE PRIVATE VARIABLE SmartMenuMaintenanceForm_Fill_Panel AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel1 AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraSplitter1 AS Infragistics.Win.Misc.UltraSplitter NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.

    
    /* Marko Rüterbories, Consultingwerk Ltd. 11.10.2012
       This variable is used to be able to remove the TreeNode when canceling 
       the addition of a menu item. */
    DEFINE PRIVATE VARIABLE cAddingRecordMenuGuid AS CHARACTER NO-UNDO.
    
    /* Marko Rüterbories, Consultingwerk Ltd. 11.10.2012
       This variable is used to be able to remove the TreeNode when deleting 
       a menu item. */
    DEFINE PRIVATE VARIABLE cDeletingRecordMenuGuid AS CHARACTER NO-UNDO.

    DEFINE PRIVATE VARIABLE oMenuGroupDatasetController AS MenuGroupDatasetController NO-UNDO.
    DEFINE PRIVATE VARIABLE lPendingChangesInMenuGroupAssignement AS LOGICAL NO-UNDO INIT FALSE.
    
    /* Marko Rüterbories, Consultingwerk Ltd. 08.12.2012
       RootNode added to the Tree even if there is no Data in the SmartMenu table */
    DEFINE PRIVATE VARIABLE oRootNode AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
    
    /* Marko Rüterbories, Consultingwerk Ltd. 08.12.2012
       Set to TRUE if a Record is added by a Drop Action */
    DEFINE VARIABLE lDropActionAddingRecord AS LOGICAL NO-UNDO INIT FALSE.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartMenuMaintenanceForm
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartMenuMaintenanceForm ():
        
        SUPER ().
        InitializeComponent ().
        
        /* Marko Rüterbories, Consultingwerk Ltd. 16.01.2013
           Register the DefaultActionSubscriber to the SmartDataBrowser controls
           to enable the user to assign or unassign a group to/from a menu. */
        smartDataBrowser2:RegisterDefaultActionSubscriber (THIS-OBJECT).
        smartDataBrowser3:RegisterDefaultActionSubscriber (THIS-OBJECT).
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Creates a new Node in the MenuTree
        Notes:   
        @param poParentNode Object reference to the parent UltraTreeNode
        @param pcNodeText Text to be displayed by the Node
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID AddNodeForNewMenuRecord (poParentNode AS Infragistics.Win.UltraWinTree.UltraTreeNode,
                                                 pcNodeText   AS CHARACTER):
        DEFINE VARIABLE oNewNode         AS Infragistics.Win.UltraWinTree.UltraTreeNode       NO-UNDO.
        DEFINE VARIABLE oNodesCollection AS Infragistics.Win.UltraWinTree.TreeNodesCollection NO-UNDO.
        
        oNewNode = NEW Infragistics.Win.UltraWinTree.UltraTreeNode (GUID,
                                                                    pcNodeText).
        IF VALID-OBJECT (poParentNode) THEN DO: 
            IF poParentNode:HasNodes AND 
               NOT poParentNode:Expanded THEN 
                poParentNode:Expanded = TRUE.
            
            oNodesCollection = poParentNode:Nodes.
        END.
        ELSE
            oNodesCollection = MenuTree:Nodes.
         
        oNodesCollection:Add (oNewNode).

        THIS-OBJECT:MenuTree:SelectedNodes:Clear () . 

        PROCESS EVENTS . 

        oNewNode:BringIntoView (TRUE).
        oNewNode:Selected = TRUE.

        THIS-OBJECT:MenuTree:ActiveNode = oNewNode .

        cAddingRecordMenuGuid = oNewNode:Key.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds an assignement to a Menu for a Group
        Notes:   
        @param sender Reference to the calling Object
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ButtonAssignGroupToMenu_Click (sender AS System.Object, 
	                                                   e      AS System.EventArgs):
		
        IF NOT VALID-OBJECT (oMenuGroupDatasetController) THEN 
            UNDO, THROW NEW AppError ("No DatasteController registered for MenuGroups!":U, 0).
        
            /* Marko Rüterbories, Consultingwerk Ltd. 05.10.2013
           Do not try to assign from an empty list. */
        IF PossibleGroupBindingSource:Count = 0 THEN RETURN.

        oMenuGroupDatasetController:AssignGroupToMenu (PossibleGroupTableAdapter:GetFieldValues("MenuGroupGuid":U)).
        
        MenuGroupAdapter:SubmitChanges ().
        
        MenuGroupAdapter:QueryHandle:QUERY-OPEN ().
        MenuGroupAdapter:BindingSource:RefreshAll ().
        
        PossibleGroupTableAdapter:RetrieveData ().

        /* Marko Rüterbories, Consultingwerk Ltd. 12.10.2013
           Refresh Assignment button sensitivity */
        SetAssignmentButtonState ().

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
        FINALLY:
            lPendingChangesInMenuGroupAssignement = TRUE.
        END FINALLY.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Removes the assignement of a User to a Group
        Notes:   
        @param sender Reference to the calling Object
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ButtonUnAssignGroupFromMenu_Click (sender AS System.Object, 
	                                                       e      AS System.EventArgs):
		
        IF NOT VALID-OBJECT (oMenuGroupDatasetController) THEN 
            UNDO, THROW NEW AppError ("No DatasteController registered for MenuGroups!":U, 0).
        
            /* Marko Rüterbories, Consultingwerk Ltd. 05.10.2013
           Do not try to assign from an empty list. */
        IF MenuGroupBindingSource:Count = 0 THEN RETURN.

        oMenuGroupDatasetController:UnAssignGroupFromMenu ().
        
        MenuGroupAdapter:SubmitChanges ().
        
        MenuGroupAdapter:QueryHandle:QUERY-OPEN ().
        MenuGroupAdapter:BindingSource:RefreshAll ().
        
        PossibleGroupTableAdapter:RetrieveData ().

        /* Marko Rüterbories, Consultingwerk Ltd. 12.10.2013
           Refresh Assignment button sensitivity */
        SetAssignmentButtonState ().

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
        FINALLY:
            lPendingChangesInMenuGroupAssignement = TRUE.
        END FINALLY.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Copies the current MenuGuid to the system clipboard 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID CopyMenuGuid ():
		
		DEFINE VARIABLE cMenuGuid AS CHARACTER NO-UNDO.

        ASSIGN cMenuGuid = MenuAdapter:GetFieldValues ("MenuGuid":U) .
        
        IF cMenuGuid > "":U THEN 
            CLIPBOARD:VALUE = cMenuGuid .
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Add a dummy ChildNode to the given ParentNode
        Notes:   
        @param poParentNode Object reference to the parent UltraTreeNode
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CreateDummyChildNode (poParentNode AS Infragistics.Win.UltraWinTree.UltraTreeNode):
        
        DEFINE VARIABLE oNode AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        
        oNode = NEW Infragistics.Win.UltraWinTree.UltraTreeNode ("DummyNode":U + GUID, "":U).
        
        poParentNode:Nodes:Add (oNode).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds a RootNode to the MenuTree
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID CreateRootNode ():
		
        oRootNode = NEW Infragistics.Win.UltraWinTree.UltraTreeNode (GUID, "Menu Structure":U).
        
        MenuTree:Nodes:Add (oRootNode).

	END METHOD .

    /*------------------------------------------------------------------------------
       Purpose: Closes all Tree Nodes
       Notes:   Expands the root node
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CollapseAll ():
        
        MenuTree:CollapseAll() .
        MenuTree:Nodes[0]:Expanded = TRUE .  

        THIS-OBJECT:StoreSettings ("false":U, "Nodes Expanded":U) .

    END METHOD.

	/*------------------------------------------------------------------------------
	   Purpose: Expands all Tree Nodes
	   Notes:
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ExpandAll ():
		
        MenuTree:ExpandAll() .

        THIS-OBJECT:StoreSettings ("true":U, "Nodes Expanded":U) .

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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Menu.SmartMenuMaintenanceForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE contextualTabGroup1 AS Infragistics.Win.UltraWinToolbars.ContextualTabGroup NO-UNDO.
        contextualTabGroup1 = NEW Infragistics.Win.UltraWinToolbars.ContextualTabGroup("GroupAssignement":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE contextualTabGroup2 AS Infragistics.Win.UltraWinToolbars.ContextualTabGroup NO-UNDO.
        contextualTabGroup2 = NEW Infragistics.Win.UltraWinToolbars.ContextualTabGroup("Menu":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("start":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("navigation":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("maintenance":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool15 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool15 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool16 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool16 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool17 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool17 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool52 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool52 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Expand all":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool53 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool53 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Collapse all":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab2 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab2 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("GroupAssignement":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup4 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("Assignement":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab3 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab3 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("MenuTab":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup5 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup5 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("Sequence":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool48 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool48 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ReorderMenuItems":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup6 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup6 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool50 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool50 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("CopyMenuGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool20 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool20 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool21 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool21 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool22 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool22 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("NavigationToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool23 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool23 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool24 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool24 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool25 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool25 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool26 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool26 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TableIOToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool27 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool27 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool28 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool28 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool29 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool29 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool30 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool30 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool31 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool31 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool32 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool32 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar3 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar3 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TransactionToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool33 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool33 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool34 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool34 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool35 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool35 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
        appearance29 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool36 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool36 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
        appearance30 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool37 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool37 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
        appearance31 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool38 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool38 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
        appearance32 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool39 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool39 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
        appearance33 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool40 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool40 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
        appearance34 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool41 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool41 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance35 AS Infragistics.Win.Appearance NO-UNDO.
        appearance35 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool42 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool42 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance36 AS Infragistics.Win.Appearance NO-UNDO.
        appearance36 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool43 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool43 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance37 AS Infragistics.Win.Appearance NO-UNDO.
        appearance37 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool44 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool44 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance38 AS Infragistics.Win.Appearance NO-UNDO.
        appearance38 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool45 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool45 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance39 AS Infragistics.Win.Appearance NO-UNDO.
        appearance39 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool46 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool46 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance40 AS Infragistics.Win.Appearance NO-UNDO.
        appearance40 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance41 AS Infragistics.Win.Appearance NO-UNDO.
        appearance41 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool47 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool47 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance42 AS Infragistics.Win.Appearance NO-UNDO.
        appearance42 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance43 AS Infragistics.Win.Appearance NO-UNDO.
        appearance43 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool49 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool49 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ReorderMenuItems":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance44 AS Infragistics.Win.Appearance NO-UNDO.
        appearance44 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance45 AS Infragistics.Win.Appearance NO-UNDO.
        appearance45 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool51 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool51 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("CopyMenuGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance46 AS Infragistics.Win.Appearance NO-UNDO.
        appearance46 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance47 AS Infragistics.Win.Appearance NO-UNDO.
        appearance47 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool54 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool54 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Expand all":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance48 AS Infragistics.Win.Appearance NO-UNDO.
        appearance48 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance49 AS Infragistics.Win.Appearance NO-UNDO.
        appearance49 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool55 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool55 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Collapse all":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance50 AS Infragistics.Win.Appearance NO-UNDO.
        appearance50 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance51 AS Infragistics.Win.Appearance NO-UNDO.
        appearance51 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ePossibleGroup":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MenuGroupGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn6 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn6 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MenuGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn12 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn12 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("GroupGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn15 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn15 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("GroupName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn16 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn16 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LoginCompanyName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("ePossibleGroup":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand2 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand2 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("eSmartMenuGroup":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MenuGroupGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MenuGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn8 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn8 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("GroupGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("GroupName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LoginCompanyName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc2 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc2 = NEW Progress.Data.TableDesc("eSmartMenuGroup":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE m_override1 AS Infragistics.Win.UltraWinTree.Override NO-UNDO.
        m_override1 = NEW Infragistics.Win.UltraWinTree.Override().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
        appearance28 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc3 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc3 = NEW Progress.Data.TableDesc("eSmartMenu":U).
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartMenuViewer1 = NEW Consultingwerk.Windows.Framework.Menu.SmartMenuViewer().
        THIS-OBJECT:MenuAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter(THIS-OBJECT:components).
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraPanel1 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ButtonAssignGroupToMenu = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ButtonUnAssignGroupFromMenu = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:smartDataBrowser3 = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:PossibleGroupBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:PossibleGroupTableAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        THIS-OBJECT:smartDataBrowser2 = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:MenuGroupBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:MenuGroupAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter(THIS-OBJECT:components).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:MenuTree = NEW Infragistics.Win.UltraWinTree.UltraTree().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:MenuBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:ultraSplitter1 = NEW Infragistics.Win.Misc.UltraSplitter().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        CAST(THIS-OBJECT:smartMenuViewer1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:MenuAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl2:SuspendLayout().
        THIS-OBJECT:ultraPanel1:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel1:SuspendLayout().
        CAST(THIS-OBJECT:smartDataBrowser3, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:PossibleGroupBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:PossibleGroupTableAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartDataBrowser2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:MenuGroupBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:MenuGroupAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:MenuTree, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        CAST(THIS-OBJECT:MenuBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:AccessibleDescription = resources:GetString("ultraTabPageControl1.AccessibleDescription":U).
        THIS-OBJECT:ultraTabPageControl1:AccessibleName = resources:GetString("ultraTabPageControl1.AccessibleName":U).
        THIS-OBJECT:ultraTabPageControl1:AutoScroll = System.Convert:ToBoolean(resources:GetObject("ultraTabPageControl1.AutoScroll":U)).
        THIS-OBJECT:ultraTabPageControl1:BackgroundImageLayout = CAST(resources:GetObject("ultraTabPageControl1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartMenuViewer1).
        THIS-OBJECT:ultraTabPageControl1:Font = CAST(resources:GetObject("ultraTabPageControl1.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraTabPageControl1:ImeMode = CAST(resources:GetObject("ultraTabPageControl1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraTabPageControl1:Location = CAST(resources:GetObject("ultraTabPageControl1.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraTabPageControl1:MaximumSize = CAST(resources:GetObject("ultraTabPageControl1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        THIS-OBJECT:ultraTabPageControl1:RightToLeft = CAST(resources:GetObject("ultraTabPageControl1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraTabPageControl1:Size = CAST(resources:GetObject("ultraTabPageControl1.Size":U), System.Drawing.Size).
        /*  */
        /* smartMenuViewer1 */
        /*  */
        THIS-OBJECT:smartMenuViewer1:AccessibleDescription = resources:GetString("smartMenuViewer1.AccessibleDescription":U).
        THIS-OBJECT:smartMenuViewer1:AccessibleName = resources:GetString("smartMenuViewer1.AccessibleName":U).
        THIS-OBJECT:smartMenuViewer1:Anchor = CAST(resources:GetObject("smartMenuViewer1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartMenuViewer1:AutoScroll = System.Convert:ToBoolean(resources:GetObject("smartMenuViewer1.AutoScroll":U)).
        THIS-OBJECT:smartMenuViewer1:AutoSize = System.Convert:ToBoolean(resources:GetObject("smartMenuViewer1.AutoSize":U)).
        THIS-OBJECT:smartMenuViewer1:AutoSizeMode = CAST(resources:GetObject("smartMenuViewer1.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:smartMenuViewer1:BackgroundImage = CAST(resources:GetObject("smartMenuViewer1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:smartMenuViewer1:BackgroundImageLayout = CAST(resources:GetObject("smartMenuViewer1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:smartMenuViewer1:Dock = CAST(resources:GetObject("smartMenuViewer1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:smartMenuViewer1:FocusControlOnAdd = ?.
        THIS-OBJECT:smartMenuViewer1:FocusControlOnUpdate = ?.
        THIS-OBJECT:smartMenuViewer1:Font = CAST(resources:GetObject("smartMenuViewer1.Font":U), System.Drawing.Font).
        THIS-OBJECT:smartMenuViewer1:ImeMode = CAST(resources:GetObject("smartMenuViewer1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:smartMenuViewer1:LinkDataSource = THIS-OBJECT:MenuAdapter.
        THIS-OBJECT:smartMenuViewer1:LinkGroupAssignSource = ?.
        THIS-OBJECT:smartMenuViewer1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartMenuViewer1:Location = CAST(resources:GetObject("smartMenuViewer1.Location":U), System.Drawing.Point).
        THIS-OBJECT:smartMenuViewer1:MaximumSize = CAST(resources:GetObject("smartMenuViewer1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:smartMenuViewer1:Name = "smartMenuViewer1":U.
        THIS-OBJECT:smartMenuViewer1:RightToLeft = CAST(resources:GetObject("smartMenuViewer1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:smartMenuViewer1:SecurityKey = resources:GetString("smartMenuViewer1.SecurityKey":U).
        THIS-OBJECT:smartMenuViewer1:Size = CAST(resources:GetObject("smartMenuViewer1.Size":U), System.Drawing.Size).
        THIS-OBJECT:smartMenuViewer1:SmartTableIOState = resources:GetString("smartMenuViewer1.SmartTableIOState":U).
        THIS-OBJECT:smartMenuViewer1:TabIndex = System.Convert:ToInt32(resources:GetObject("smartMenuViewer1.TabIndex":U)).
        THIS-OBJECT:smartMenuViewer1:ViewerLogicClass = resources:GetString("smartMenuViewer1.ViewerLogicClass":U).
        THIS-OBJECT:smartMenuViewer1:AfterCancelUpdate:Subscribe(THIS-OBJECT:smartMenuViewer1_AfterCancelUpdate).
        THIS-OBJECT:smartMenuViewer1:AfterDeleteRecord:Subscribe(THIS-OBJECT:smartMenuViewer1_AfterDeleteRecord).
        THIS-OBJECT:smartMenuViewer1:AfterSaveChanges:Subscribe(THIS-OBJECT:smartMenuViewer1_AfterSaveChanges).
        THIS-OBJECT:smartMenuViewer1:BeforeDeleteRecord:Subscribe(THIS-OBJECT:smartMenuViewer1_BeforeDeleteRecord).
        THIS-OBJECT:smartMenuViewer1:SmartTableIOStateChanged:Subscribe(THIS-OBJECT:smartMenuViewer1_SmartTableIOStateChanged).
        /*  */
        /* MenuAdapter */
        /*  */
        THIS-OBJECT:MenuAdapter:BindingSourceExcludeFields = "ParentMenuGuid,MenuSequence":U.
        THIS-OBJECT:MenuAdapter:DatasetControllerType = "Consultingwerk.SmartFramework.Menu.MenuDatasetController":U.
        THIS-OBJECT:MenuAdapter:EntityJoin = "YES":U.
        THIS-OBJECT:MenuAdapter:EntityName = "Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U.
        THIS-OBJECT:MenuAdapter:EntityTable = "eSmartMenu":U.
        THIS-OBJECT:MenuAdapter:EntityView = "eSmartFunction":U.
        THIS-OBJECT:MenuAdapter:ForeignFields = "":U.
        THIS-OBJECT:MenuAdapter:LinkCommitSource = ?.
        THIS-OBJECT:MenuAdapter:LinkDataSource = ?.
        THIS-OBJECT:MenuAdapter:LinkNavigationSource = ?.
        THIS-OBJECT:MenuAdapter:NonUpdatableColumns = "":U.
        THIS-OBJECT:MenuAdapter:QuerySort = "":U.
        THIS-OBJECT:MenuAdapter:ThrowErrorsFromRetrieveData = FALSE.
        THIS-OBJECT:MenuAdapter:BeforeCreateRecord:Subscribe(THIS-OBJECT:MenuAdapter_BeforeCreateRecord).
        THIS-OBJECT:MenuAdapter:AfterRetrieveData:Subscribe(THIS-OBJECT:MenuAdapter_AfterRetrieveData).
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm":U).
        THIS-OBJECT:smartToolbarController1:Office2007UICompatibility = FALSE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar0).
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:KeyTip = resources:GetString("smartToolbarController1.Ribbon.ApplicationMenu.KeyTip":U).
        buttonTool5:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar1[1] = buttonTool2.
        arrayvar1[2] = buttonTool3.
        arrayvar1[3] = buttonTool4.
        arrayvar1[4] = buttonTool5.
        arrayvar1[5] = buttonTool6.
        arrayvar1[6] = buttonTool7.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar1).
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolTipText = resources:GetString("smartToolbarController1.Ribbon.ApplicationMenu.ToolTipText":U).
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolTipTextFormatted = resources:GetString("smartToolbarController1.Ribbon.ApplicationMenu.ToolTipTextFormatted":U).
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolTipTitle = resources:GetString("smartToolbarController1.Ribbon.ApplicationMenu.ToolTipTitle":U).
        contextualTabGroup1:Caption = resources:GetString("contextualTabGroup1.Caption":U).
        contextualTabGroup1:Key = "GroupAssignement":U.
        contextualTabGroup2:Caption = resources:GetString("contextualTabGroup2.Caption":U).
        contextualTabGroup2:Key = "Menu":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ContextualTabGroup EXTENT 2 NO-UNDO.
        arrayvar2[1] = contextualTabGroup1.
        arrayvar2[2] = contextualTabGroup2.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedContextualTabGroups:AddRange(arrayvar2).
        ribbonTab1:Caption = resources:GetString("ribbonTab1.Caption":U).
        ribbonGroup1:Caption = resources:GetString("ribbonGroup1.Caption":U).
        ribbonGroup1:KeyTip = resources:GetString("ribbonGroup1.KeyTip":U).
        buttonTool8:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool8:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool9:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool9:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool10:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool10:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool11:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool11:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar3[1] = buttonTool8.
        arrayvar3[2] = buttonTool9.
        arrayvar3[3] = buttonTool10.
        arrayvar3[4] = buttonTool11.
        ribbonGroup1:Tools:AddRange(arrayvar3).
        ribbonGroup2:Caption = resources:GetString("ribbonGroup2.Caption":U).
        ribbonGroup2:KeyTip = resources:GetString("ribbonGroup2.KeyTip":U).
        buttonTool12:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool12:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool13:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool13:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool14:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool14:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool15:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool16:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool17:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar4[1] = buttonTool12.
        arrayvar4[2] = buttonTool13.
        arrayvar4[3] = buttonTool14.
        arrayvar4[4] = buttonTool15.
        arrayvar4[5] = buttonTool16.
        arrayvar4[6] = buttonTool17.
        ribbonGroup2:Tools:AddRange(arrayvar4).
        ribbonGroup3:Caption = resources:GetString("ribbonGroup3.Caption":U).
        ribbonGroup3:KeyTip = resources:GetString("ribbonGroup3.KeyTip":U).
        buttonTool52:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool53:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar5[1] = buttonTool52.
        arrayvar5[2] = buttonTool53.
        ribbonGroup3:Tools:AddRange(arrayvar5).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 3 NO-UNDO.
        arrayvar6[1] = ribbonGroup1.
        arrayvar6[2] = ribbonGroup2.
        arrayvar6[3] = ribbonGroup3.
        ribbonTab1:Groups:AddRange(arrayvar6).
        ribbonTab1:KeyTip = resources:GetString("ribbonTab1.KeyTip":U).
        ribbonTab2:Caption = resources:GetString("ribbonTab2.Caption":U).
        ribbonTab2:ContextualTabGroupKey = "GroupAssignement":U.
        ribbonGroup4:Caption = resources:GetString("ribbonGroup4.Caption":U).
        ribbonGroup4:KeyTip = resources:GetString("ribbonGroup4.KeyTip":U).
        buttonTool18:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool19:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar7[1] = buttonTool18.
        arrayvar7[2] = buttonTool19.
        ribbonGroup4:Tools:AddRange(arrayvar7).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 1 NO-UNDO.
        arrayvar8[1] = ribbonGroup4.
        ribbonTab2:Groups:AddRange(arrayvar8).
        ribbonTab2:KeyTip = resources:GetString("ribbonTab2.KeyTip":U).
        ribbonTab3:Caption = resources:GetString("ribbonTab3.Caption":U).
        ribbonTab3:ContextualTabGroupKey = "Menu":U.
        ribbonGroup5:Caption = resources:GetString("ribbonGroup5.Caption":U).
        ribbonGroup5:KeyTip = resources:GetString("ribbonGroup5.KeyTip":U).
        buttonTool48:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar9[1] = buttonTool48.
        ribbonGroup5:Tools:AddRange(arrayvar9).
        ribbonGroup6:Caption = resources:GetString("ribbonGroup6.Caption":U).
        ribbonGroup6:KeyTip = resources:GetString("ribbonGroup6.KeyTip":U).
        buttonTool50:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar10[1] = buttonTool50.
        ribbonGroup6:Tools:AddRange(arrayvar10).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 2 NO-UNDO.
        arrayvar11[1] = ribbonGroup5.
        arrayvar11[2] = ribbonGroup6.
        ribbonTab3:Groups:AddRange(arrayvar11).
        ribbonTab3:KeyTip = resources:GetString("ribbonTab3.KeyTip":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 3 NO-UNDO.
        arrayvar12[1] = ribbonTab1.
        arrayvar12[2] = ribbonTab2.
        arrayvar12[3] = ribbonTab3.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar12).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar13[1] = buttonTool20.
        arrayvar13[2] = buttonTool21.
        arrayvar13[3] = buttonTool22.
        THIS-OBJECT:smartToolbarController1:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar13).
        THIS-OBJECT:smartToolbarController1:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        THIS-OBJECT:smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        ultraToolbar1:KeyTip = resources:GetString("ultraToolbar1.KeyTip":U).
        buttonTool23:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar14[1] = buttonTool23.
        arrayvar14[2] = buttonTool24.
        arrayvar14[3] = buttonTool25.
        arrayvar14[4] = buttonTool26.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar14).
        ultraToolbar1:Text = resources:GetString("ultraToolbar1.Text":U).
        ultraToolbar1:Visible = FALSE.
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        ultraToolbar2:KeyTip = resources:GetString("ultraToolbar2.KeyTip":U).
        buttonTool27:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar15 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar15[1] = buttonTool27.
        arrayvar15[2] = buttonTool28.
        arrayvar15[3] = buttonTool29.
        arrayvar15[4] = buttonTool30.
        arrayvar15[5] = buttonTool31.
        arrayvar15[6] = buttonTool32.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar15).
        ultraToolbar2:Text = resources:GetString("ultraToolbar2.Text":U).
        ultraToolbar2:Visible = FALSE.
        ultraToolbar3:DockedColumn = 0.
        ultraToolbar3:DockedRow = 0.
        ultraToolbar3:KeyTip = resources:GetString("ultraToolbar3.KeyTip":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar16 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar16[1] = buttonTool33.
        arrayvar16[2] = buttonTool34.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar16).
        ultraToolbar3:Text = resources:GetString("ultraToolbar3.Text":U).
        ultraToolbar3:Visible = FALSE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar17 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 3 NO-UNDO.
        arrayvar17[1] = ultraToolbar1.
        arrayvar17[2] = ultraToolbar2.
        arrayvar17[3] = ultraToolbar3.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar17).
        appearance29:Image = CAST(resources:GetObject("appearance29.Image":U), System.Object).
        appearance29:TextHAlignAsString = resources:GetString("appearance29.TextHAlignAsString":U).
        appearance29:TextVAlignAsString = resources:GetString("appearance29.TextVAlignAsString":U).
        buttonTool35:SharedPropsInternal:AppearancesSmall:Appearance = appearance29.
        buttonTool35:SharedPropsInternal:Caption = resources:GetString("resource.Caption4":U).
        buttonTool35:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption":U).
        buttonTool35:SharedPropsInternal:Enabled = FALSE.
        buttonTool35:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText":U).
        appearance30:Image = CAST(resources:GetObject("appearance30.Image":U), System.Object).
        appearance30:TextHAlignAsString = resources:GetString("appearance30.TextHAlignAsString":U).
        appearance30:TextVAlignAsString = resources:GetString("appearance30.TextVAlignAsString":U).
        buttonTool36:SharedPropsInternal:AppearancesSmall:Appearance = appearance30.
        buttonTool36:SharedPropsInternal:Caption = resources:GetString("resource.Caption5":U).
        buttonTool36:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption1":U).
        buttonTool36:SharedPropsInternal:Enabled = FALSE.
        buttonTool36:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText1":U).
        appearance31:Image = CAST(resources:GetObject("appearance31.Image":U), System.Object).
        appearance31:TextHAlignAsString = resources:GetString("appearance31.TextHAlignAsString":U).
        appearance31:TextVAlignAsString = resources:GetString("appearance31.TextVAlignAsString":U).
        buttonTool37:SharedPropsInternal:AppearancesSmall:Appearance = appearance31.
        buttonTool37:SharedPropsInternal:Caption = resources:GetString("resource.Caption6":U).
        buttonTool37:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption2":U).
        buttonTool37:SharedPropsInternal:Enabled = FALSE.
        buttonTool37:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText2":U).
        appearance32:Image = CAST(resources:GetObject("appearance32.Image":U), System.Object).
        appearance32:TextHAlignAsString = resources:GetString("appearance32.TextHAlignAsString":U).
        appearance32:TextVAlignAsString = resources:GetString("appearance32.TextVAlignAsString":U).
        buttonTool38:SharedPropsInternal:AppearancesSmall:Appearance = appearance32.
        buttonTool38:SharedPropsInternal:Caption = resources:GetString("resource.Caption7":U).
        buttonTool38:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption3":U).
        buttonTool38:SharedPropsInternal:Enabled = FALSE.
        buttonTool38:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText3":U).
        appearance33:Image = CAST(resources:GetObject("appearance33.Image":U), System.Object).
        appearance33:TextHAlignAsString = resources:GetString("appearance33.TextHAlignAsString":U).
        appearance33:TextVAlignAsString = resources:GetString("appearance33.TextVAlignAsString":U).
        buttonTool39:SharedPropsInternal:AppearancesSmall:Appearance = appearance33.
        buttonTool39:SharedPropsInternal:Caption = resources:GetString("resource.Caption8":U).
        buttonTool39:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption4":U).
        buttonTool39:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText4":U).
        appearance34:Image = CAST(resources:GetObject("appearance34.Image":U), System.Object).
        appearance34:TextHAlignAsString = resources:GetString("appearance34.TextHAlignAsString":U).
        appearance34:TextVAlignAsString = resources:GetString("appearance34.TextVAlignAsString":U).
        buttonTool40:SharedPropsInternal:AppearancesSmall:Appearance = appearance34.
        buttonTool40:SharedPropsInternal:Caption = resources:GetString("resource.Caption9":U).
        buttonTool40:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption5":U).
        buttonTool40:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText5":U).
        appearance35:Image = CAST(resources:GetObject("appearance35.Image":U), System.Object).
        appearance35:TextHAlignAsString = resources:GetString("appearance35.TextHAlignAsString":U).
        appearance35:TextVAlignAsString = resources:GetString("appearance35.TextVAlignAsString":U).
        buttonTool41:SharedPropsInternal:AppearancesSmall:Appearance = appearance35.
        buttonTool41:SharedPropsInternal:Caption = resources:GetString("resource.Caption10":U).
        buttonTool41:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption6":U).
        buttonTool41:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText6":U).
        appearance36:Image = CAST(resources:GetObject("appearance36.Image":U), System.Object).
        appearance36:TextHAlignAsString = resources:GetString("appearance36.TextHAlignAsString":U).
        appearance36:TextVAlignAsString = resources:GetString("appearance36.TextVAlignAsString":U).
        buttonTool42:SharedPropsInternal:AppearancesSmall:Appearance = appearance36.
        buttonTool42:SharedPropsInternal:Caption = resources:GetString("resource.Caption11":U).
        buttonTool42:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption7":U).
        buttonTool42:SharedPropsInternal:Enabled = FALSE.
        buttonTool42:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText7":U).
        appearance37:Image = CAST(resources:GetObject("appearance37.Image":U), System.Object).
        appearance37:TextHAlignAsString = resources:GetString("appearance37.TextHAlignAsString":U).
        appearance37:TextVAlignAsString = resources:GetString("appearance37.TextVAlignAsString":U).
        buttonTool43:SharedPropsInternal:AppearancesSmall:Appearance = appearance37.
        buttonTool43:SharedPropsInternal:Caption = resources:GetString("resource.Caption12":U).
        buttonTool43:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption8":U).
        buttonTool43:SharedPropsInternal:Enabled = FALSE.
        buttonTool43:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText8":U).
        appearance38:Image = CAST(resources:GetObject("appearance38.Image":U), System.Object).
        appearance38:TextHAlignAsString = resources:GetString("appearance38.TextHAlignAsString":U).
        appearance38:TextVAlignAsString = resources:GetString("appearance38.TextVAlignAsString":U).
        buttonTool44:SharedPropsInternal:AppearancesSmall:Appearance = appearance38.
        buttonTool44:SharedPropsInternal:Caption = resources:GetString("resource.Caption13":U).
        buttonTool44:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption9":U).
        buttonTool44:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText9":U).
        appearance39:Image = CAST(resources:GetObject("appearance39.Image":U), System.Object).
        appearance39:TextHAlignAsString = resources:GetString("appearance39.TextHAlignAsString":U).
        appearance39:TextVAlignAsString = resources:GetString("appearance39.TextVAlignAsString":U).
        buttonTool45:SharedPropsInternal:AppearancesSmall:Appearance = appearance39.
        buttonTool45:SharedPropsInternal:Caption = resources:GetString("resource.Caption14":U).
        buttonTool45:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance40:Image = CAST(resources:GetObject("appearance40.Image":U), System.Object).
        appearance40:TextHAlignAsString = resources:GetString("appearance40.TextHAlignAsString":U).
        appearance40:TextVAlignAsString = resources:GetString("appearance40.TextVAlignAsString":U).
        buttonTool46:SharedPropsInternal:AppearancesLarge:Appearance = appearance40.
        appearance41:Image = CAST(resources:GetObject("appearance41.Image":U), System.Object).
        appearance41:TextHAlignAsString = resources:GetString("appearance41.TextHAlignAsString":U).
        appearance41:TextVAlignAsString = resources:GetString("appearance41.TextVAlignAsString":U).
        buttonTool46:SharedPropsInternal:AppearancesSmall:Appearance = appearance41.
        buttonTool46:SharedPropsInternal:Caption = resources:GetString("resource.Caption15":U).
        buttonTool46:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption10":U).
        buttonTool46:SharedPropsInternal:Enabled = FALSE.
        buttonTool46:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText10":U).
        appearance42:Image = CAST(resources:GetObject("appearance42.Image":U), System.Object).
        appearance42:TextHAlignAsString = resources:GetString("appearance42.TextHAlignAsString":U).
        appearance42:TextVAlignAsString = resources:GetString("appearance42.TextVAlignAsString":U).
        buttonTool47:SharedPropsInternal:AppearancesLarge:Appearance = appearance42.
        appearance43:Image = CAST(resources:GetObject("appearance43.Image":U), System.Object).
        appearance43:TextHAlignAsString = resources:GetString("appearance43.TextHAlignAsString":U).
        appearance43:TextVAlignAsString = resources:GetString("appearance43.TextVAlignAsString":U).
        buttonTool47:SharedPropsInternal:AppearancesSmall:Appearance = appearance43.
        buttonTool47:SharedPropsInternal:Caption = resources:GetString("resource.Caption16":U).
        buttonTool47:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption11":U).
        buttonTool47:SharedPropsInternal:Enabled = FALSE.
        buttonTool47:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText11":U).
        appearance44:Image = CAST(resources:GetObject("appearance44.Image":U), System.Object).
        appearance44:TextHAlignAsString = resources:GetString("appearance44.TextHAlignAsString":U).
        appearance44:TextVAlignAsString = resources:GetString("appearance44.TextVAlignAsString":U).
        buttonTool49:SharedPropsInternal:AppearancesLarge:Appearance = appearance44.
        appearance45:Image = CAST(resources:GetObject("appearance45.Image":U), System.Object).
        appearance45:TextHAlignAsString = resources:GetString("appearance45.TextHAlignAsString":U).
        appearance45:TextVAlignAsString = resources:GetString("appearance45.TextVAlignAsString":U).
        buttonTool49:SharedPropsInternal:AppearancesSmall:Appearance = appearance45.
        buttonTool49:SharedPropsInternal:Caption = resources:GetString("resource.Caption17":U).
        appearance46:Image = CAST(resources:GetObject("appearance46.Image":U), System.Object).
        appearance46:TextHAlignAsString = resources:GetString("appearance46.TextHAlignAsString":U).
        appearance46:TextVAlignAsString = resources:GetString("appearance46.TextVAlignAsString":U).
        buttonTool51:SharedPropsInternal:AppearancesLarge:Appearance = appearance46.
        appearance47:Image = CAST(resources:GetObject("appearance47.Image":U), System.Object).
        appearance47:TextHAlignAsString = resources:GetString("appearance47.TextHAlignAsString":U).
        appearance47:TextVAlignAsString = resources:GetString("appearance47.TextVAlignAsString":U).
        buttonTool51:SharedPropsInternal:AppearancesSmall:Appearance = appearance47.
        buttonTool51:SharedPropsInternal:Caption = resources:GetString("resource.Caption18":U).
        appearance48:Image = CAST(resources:GetObject("appearance48.Image":U), System.Object).
        appearance48:TextHAlignAsString = resources:GetString("appearance48.TextHAlignAsString":U).
        appearance48:TextVAlignAsString = resources:GetString("appearance48.TextVAlignAsString":U).
        buttonTool54:SharedPropsInternal:AppearancesLarge:Appearance = appearance48.
        appearance49:Image = CAST(resources:GetObject("appearance49.Image":U), System.Object).
        appearance49:TextHAlignAsString = resources:GetString("appearance49.TextHAlignAsString":U).
        appearance49:TextVAlignAsString = resources:GetString("appearance49.TextVAlignAsString":U).
        buttonTool54:SharedPropsInternal:AppearancesSmall:Appearance = appearance49.
        buttonTool54:SharedPropsInternal:Caption = resources:GetString("resource.Caption19":U).
        appearance50:Image = CAST(resources:GetObject("appearance50.Image":U), System.Object).
        appearance50:TextHAlignAsString = resources:GetString("appearance50.TextHAlignAsString":U).
        appearance50:TextVAlignAsString = resources:GetString("appearance50.TextVAlignAsString":U).
        buttonTool55:SharedPropsInternal:AppearancesLarge:Appearance = appearance50.
        appearance51:Image = CAST(resources:GetObject("appearance51.Image":U), System.Object).
        appearance51:TextHAlignAsString = resources:GetString("appearance51.TextHAlignAsString":U).
        appearance51:TextVAlignAsString = resources:GetString("appearance51.TextVAlignAsString":U).
        buttonTool55:SharedPropsInternal:AppearancesSmall:Appearance = appearance51.
        buttonTool55:SharedPropsInternal:Caption = resources:GetString("resource.Caption20":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar18 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 17 NO-UNDO.
        arrayvar18[1] = buttonTool35.
        arrayvar18[2] = buttonTool36.
        arrayvar18[3] = buttonTool37.
        arrayvar18[4] = buttonTool38.
        arrayvar18[5] = buttonTool39.
        arrayvar18[6] = buttonTool40.
        arrayvar18[7] = buttonTool41.
        arrayvar18[8] = buttonTool42.
        arrayvar18[9] = buttonTool43.
        arrayvar18[10] = buttonTool44.
        arrayvar18[11] = buttonTool45.
        arrayvar18[12] = buttonTool46.
        arrayvar18[13] = buttonTool47.
        arrayvar18[14] = buttonTool49.
        arrayvar18[15] = buttonTool51.
        arrayvar18[16] = buttonTool54.
        arrayvar18[17] = buttonTool55.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar18).
        THIS-OBJECT:smartToolbarController1:ToolClick:Subscribe(THIS-OBJECT:smartToolbarController1_ToolClick).
        /*  */
        /* ultraTabPageControl2 */
        /*  */
        THIS-OBJECT:ultraTabPageControl2:AccessibleDescription = resources:GetString("ultraTabPageControl2.AccessibleDescription":U).
        THIS-OBJECT:ultraTabPageControl2:AccessibleName = resources:GetString("ultraTabPageControl2.AccessibleName":U).
        THIS-OBJECT:ultraTabPageControl2:AutoScroll = System.Convert:ToBoolean(resources:GetObject("ultraTabPageControl2.AutoScroll":U)).
        THIS-OBJECT:ultraTabPageControl2:BackgroundImageLayout = CAST(resources:GetObject("ultraTabPageControl2.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:ultraPanel1).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartDataBrowser3).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartDataBrowser2).
        THIS-OBJECT:ultraTabPageControl2:Font = CAST(resources:GetObject("ultraTabPageControl2.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraTabPageControl2:ImeMode = CAST(resources:GetObject("ultraTabPageControl2.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraTabPageControl2:Location = CAST(resources:GetObject("ultraTabPageControl2.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraTabPageControl2:MaximumSize = CAST(resources:GetObject("ultraTabPageControl2.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2":U.
        THIS-OBJECT:ultraTabPageControl2:RightToLeft = CAST(resources:GetObject("ultraTabPageControl2.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraTabPageControl2:Size = CAST(resources:GetObject("ultraTabPageControl2.Size":U), System.Drawing.Size).
        /*  */
        /* ultraPanel1 */
        /*  */
        THIS-OBJECT:ultraPanel1:AccessibleDescription = resources:GetString("ultraPanel1.AccessibleDescription":U).
        THIS-OBJECT:ultraPanel1:AccessibleName = resources:GetString("ultraPanel1.AccessibleName":U).
        THIS-OBJECT:ultraPanel1:Anchor = CAST(resources:GetObject("ultraPanel1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraPanel1:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraPanel1.AutoSize":U)).
        THIS-OBJECT:ultraPanel1:BackgroundImageLayout = CAST(resources:GetObject("ultraPanel1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        /*  */
        /* ultraPanel1.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel1:ClientArea:AccessibleDescription = resources:GetString("ultraPanel1.ClientArea.AccessibleDescription":U).
        THIS-OBJECT:ultraPanel1:ClientArea:AccessibleName = resources:GetString("ultraPanel1.ClientArea.AccessibleName":U).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ButtonAssignGroupToMenu).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ButtonUnAssignGroupFromMenu).
        THIS-OBJECT:ultraPanel1:ClientArea:Font = CAST(resources:GetObject("ultraPanel1.ClientArea.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraPanel1:Dock = CAST(resources:GetObject("ultraPanel1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraPanel1:Font = CAST(resources:GetObject("ultraPanel1.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraPanel1:ImeMode = CAST(resources:GetObject("ultraPanel1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraPanel1:Location = CAST(resources:GetObject("ultraPanel1.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraPanel1:MaximumSize = CAST(resources:GetObject("ultraPanel1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraPanel1:Name = "ultraPanel1":U.
        THIS-OBJECT:ultraPanel1:RightToLeft = CAST(resources:GetObject("ultraPanel1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraPanel1:Size = CAST(resources:GetObject("ultraPanel1.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraPanel1:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraPanel1.TabIndex":U)).
        /*  */
        /* ButtonAssignGroupToMenu */
        /*  */
        THIS-OBJECT:ButtonAssignGroupToMenu:AccessibleDescription = resources:GetString("ButtonAssignGroupToMenu.AccessibleDescription":U).
        THIS-OBJECT:ButtonAssignGroupToMenu:AccessibleName = resources:GetString("ButtonAssignGroupToMenu.AccessibleName":U).
        THIS-OBJECT:ButtonAssignGroupToMenu:Anchor = CAST(resources:GetObject("ButtonAssignGroupToMenu.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        appearance1:ImageHAlign = Infragistics.Win.HAlign:Center.
        appearance1:ImageVAlign = Infragistics.Win.VAlign:Middle.
        appearance1:TextHAlignAsString = resources:GetString("appearance1.TextHAlignAsString":U).
        appearance1:TextVAlignAsString = resources:GetString("appearance1.TextVAlignAsString":U).
        THIS-OBJECT:ButtonAssignGroupToMenu:Appearance = appearance1.
        THIS-OBJECT:ButtonAssignGroupToMenu:AutoSize = System.Convert:ToBoolean(resources:GetObject("ButtonAssignGroupToMenu.AutoSize":U)).
        THIS-OBJECT:ButtonAssignGroupToMenu:BackgroundImage = CAST(resources:GetObject("ButtonAssignGroupToMenu.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ButtonAssignGroupToMenu:BackgroundImageLayout = CAST(resources:GetObject("ButtonAssignGroupToMenu.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ButtonAssignGroupToMenu:Dock = CAST(resources:GetObject("ButtonAssignGroupToMenu.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ButtonAssignGroupToMenu:Font = CAST(resources:GetObject("ButtonAssignGroupToMenu.Font":U), System.Drawing.Font).
        THIS-OBJECT:ButtonAssignGroupToMenu:ImeMode = CAST(resources:GetObject("ButtonAssignGroupToMenu.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ButtonAssignGroupToMenu:Location = CAST(resources:GetObject("ButtonAssignGroupToMenu.Location":U), System.Drawing.Point).
        THIS-OBJECT:ButtonAssignGroupToMenu:MaximumSize = CAST(resources:GetObject("ButtonAssignGroupToMenu.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ButtonAssignGroupToMenu:Name = "ButtonAssignGroupToMenu":U.
        THIS-OBJECT:ButtonAssignGroupToMenu:RightToLeft = CAST(resources:GetObject("ButtonAssignGroupToMenu.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ButtonAssignGroupToMenu:Size = CAST(resources:GetObject("ButtonAssignGroupToMenu.Size":U), System.Drawing.Size).
        THIS-OBJECT:ButtonAssignGroupToMenu:TabIndex = System.Convert:ToInt32(resources:GetObject("ButtonAssignGroupToMenu.TabIndex":U)).
        THIS-OBJECT:ButtonAssignGroupToMenu:Click:Subscribe(THIS-OBJECT:ButtonAssignGroupToMenu_Click).
        /*  */
        /* ButtonUnAssignGroupFromMenu */
        /*  */
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:AccessibleDescription = resources:GetString("ButtonUnAssignGroupFromMenu.AccessibleDescription":U).
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:AccessibleName = resources:GetString("ButtonUnAssignGroupFromMenu.AccessibleName":U).
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:Anchor = CAST(resources:GetObject("ButtonUnAssignGroupFromMenu.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        appearance2:ImageHAlign = Infragistics.Win.HAlign:Center.
        appearance2:ImageVAlign = Infragistics.Win.VAlign:Middle.
        appearance2:TextHAlignAsString = resources:GetString("appearance2.TextHAlignAsString":U).
        appearance2:TextVAlignAsString = resources:GetString("appearance2.TextVAlignAsString":U).
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:Appearance = appearance2.
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:AutoSize = System.Convert:ToBoolean(resources:GetObject("ButtonUnAssignGroupFromMenu.AutoSize":U)).
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:BackgroundImage = CAST(resources:GetObject("ButtonUnAssignGroupFromMenu.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:BackgroundImageLayout = CAST(resources:GetObject("ButtonUnAssignGroupFromMenu.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:Dock = CAST(resources:GetObject("ButtonUnAssignGroupFromMenu.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:Font = CAST(resources:GetObject("ButtonUnAssignGroupFromMenu.Font":U), System.Drawing.Font).
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:ImeMode = CAST(resources:GetObject("ButtonUnAssignGroupFromMenu.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:Location = CAST(resources:GetObject("ButtonUnAssignGroupFromMenu.Location":U), System.Drawing.Point).
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:MaximumSize = CAST(resources:GetObject("ButtonUnAssignGroupFromMenu.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:Name = "ButtonUnAssignGroupFromMenu":U.
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:RightToLeft = CAST(resources:GetObject("ButtonUnAssignGroupFromMenu.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:Size = CAST(resources:GetObject("ButtonUnAssignGroupFromMenu.Size":U), System.Drawing.Size).
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:TabIndex = System.Convert:ToInt32(resources:GetObject("ButtonUnAssignGroupFromMenu.TabIndex":U)).
        THIS-OBJECT:ButtonUnAssignGroupFromMenu:Click:Subscribe(THIS-OBJECT:ButtonUnAssignGroupFromMenu_Click).
        /*  */
        /* smartDataBrowser3 */
        /*  */
        THIS-OBJECT:smartDataBrowser3:AccessibleDescription = resources:GetString("smartDataBrowser3.AccessibleDescription":U).
        THIS-OBJECT:smartDataBrowser3:AccessibleName = resources:GetString("smartDataBrowser3.AccessibleName":U).
        THIS-OBJECT:smartDataBrowser3:Anchor = CAST(resources:GetObject("smartDataBrowser3.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartDataBrowser3:BackgroundImage = CAST(resources:GetObject("smartDataBrowser3.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:smartDataBrowser3:BackgroundImageLayout = CAST(resources:GetObject("smartDataBrowser3.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:smartDataBrowser3:DataSource = THIS-OBJECT:PossibleGroupBindingSource.
        appearance3:BackColor = System.Drawing.SystemColors:Window.
        appearance3:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        appearance3:TextHAlignAsString = resources:GetString("appearance3.TextHAlignAsString":U).
        appearance3:TextVAlignAsString = resources:GetString("appearance3.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Appearance = appearance3.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ExtendLastColumn.
        ultraGridColumn5:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn5:Header:VisiblePosition = 2.
        ultraGridColumn5:Hidden = TRUE.
        ultraGridColumn6:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn6:Header:VisiblePosition = 3.
        ultraGridColumn6:Hidden = TRUE.
        ultraGridColumn12:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn12:Header:VisiblePosition = 4.
        ultraGridColumn12:Hidden = TRUE.
        ultraGridColumn15:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn15:Header:Caption = resources:GetString("resource.Caption":U).
        ultraGridColumn15:Header:VisiblePosition = 0.
        ultraGridColumn15:Width = 155.
        ultraGridColumn16:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn16:Header:Caption = resources:GetString("resource.Caption1":U).
        ultraGridColumn16:Header:VisiblePosition = 1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar19 AS System.Object EXTENT 5 NO-UNDO.
        arrayvar19[1] = ultraGridColumn5.
        arrayvar19[2] = ultraGridColumn6.
        arrayvar19[3] = ultraGridColumn12.
        arrayvar19[4] = ultraGridColumn15.
        arrayvar19[5] = ultraGridColumn16.
        ultraGridBand1:Columns:AddRange(arrayvar19).
        ultraGridBand1:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand1:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:True.
        appearance4:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance4:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance4:BorderColor = System.Drawing.SystemColors:Window.
        appearance4:TextHAlignAsString = resources:GetString("appearance4.TextHAlignAsString":U).
        appearance4:TextVAlignAsString = resources:GetString("appearance4.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:GroupByBox:Appearance = appearance4.
        appearance5:ForeColor = System.Drawing.SystemColors:GrayText.
        appearance5:TextHAlignAsString = resources:GetString("appearance5.TextHAlignAsString":U).
        appearance5:TextVAlignAsString = resources:GetString("appearance5.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:GroupByBox:BandLabelAppearance = appearance5.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance6:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance6:BackColor2 = System.Drawing.SystemColors:Control.
        appearance6:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance6:ForeColor = System.Drawing.SystemColors:GrayText.
        appearance6:TextHAlignAsString = resources:GetString("appearance6.TextHAlignAsString":U).
        appearance6:TextVAlignAsString = resources:GetString("appearance6.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:GroupByBox:PromptAppearance = appearance6.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:MaxRowScrollRegions = 1.
        appearance7:BackColor = System.Drawing.SystemColors:Window.
        appearance7:ForeColor = System.Drawing.SystemColors:ControlText.
        appearance7:TextHAlignAsString = resources:GetString("appearance7.TextHAlignAsString":U).
        appearance7:TextVAlignAsString = resources:GetString("appearance7.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:ActiveCellAppearance = appearance7.
        appearance8:BackColor = System.Drawing.SystemColors:Highlight.
        appearance8:ForeColor = System.Drawing.SystemColors:HighlightText.
        appearance8:TextHAlignAsString = resources:GetString("appearance8.TextHAlignAsString":U).
        appearance8:TextVAlignAsString = resources:GetString("appearance8.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:ActiveRowAppearance = appearance8.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance9:BackColor = System.Drawing.SystemColors:Window.
        appearance9:TextHAlignAsString = resources:GetString("appearance9.TextHAlignAsString":U).
        appearance9:TextVAlignAsString = resources:GetString("appearance9.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:CardAreaAppearance = appearance9.
        appearance10:BorderColor = System.Drawing.Color:Silver.
        appearance10:TextHAlignAsString = resources:GetString("appearance10.TextHAlignAsString":U).
        appearance10:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        appearance10:TextVAlignAsString = resources:GetString("appearance10.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:CellAppearance = appearance10.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:CellPadding = 0.
        appearance11:BackColor = System.Drawing.SystemColors:Control.
        appearance11:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance11:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance11:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance11:BorderColor = System.Drawing.SystemColors:Window.
        appearance11:TextHAlignAsString = resources:GetString("appearance11.TextHAlignAsString":U).
        appearance11:TextVAlignAsString = resources:GetString("appearance11.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:GroupByRowAppearance = appearance11.
        appearance12:TextHAlignAsString = resources:GetString("appearance12.TextHAlignAsString":U).
        appearance12:TextVAlignAsString = resources:GetString("appearance12.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:HeaderAppearance = appearance12.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance13:BackColor = System.Drawing.SystemColors:Window.
        appearance13:BorderColor = System.Drawing.Color:Silver.
        appearance13:TextHAlignAsString = resources:GetString("appearance13.TextHAlignAsString":U).
        appearance13:TextVAlignAsString = resources:GetString("appearance13.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:RowAppearance = appearance13.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance14:BackColor = System.Drawing.SystemColors:ControlLight.
        appearance14:TextHAlignAsString = resources:GetString("appearance14.TextHAlignAsString":U).
        appearance14:TextVAlignAsString = resources:GetString("appearance14.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:TemplateAddRowAppearance = appearance14.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:smartDataBrowser3:Dock = CAST(resources:GetObject("smartDataBrowser3.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:smartDataBrowser3:Font = CAST(resources:GetObject("smartDataBrowser3.Font":U), System.Drawing.Font).
        THIS-OBJECT:smartDataBrowser3:ImeMode = CAST(resources:GetObject("smartDataBrowser3.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:smartDataBrowser3:LinkDataSource = THIS-OBJECT:PossibleGroupTableAdapter.
        THIS-OBJECT:smartDataBrowser3:Location = CAST(resources:GetObject("smartDataBrowser3.Location":U), System.Drawing.Point).
        THIS-OBJECT:smartDataBrowser3:MaximumSize = CAST(resources:GetObject("smartDataBrowser3.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:smartDataBrowser3:Name = "smartDataBrowser3":U.
        THIS-OBJECT:smartDataBrowser3:RightToLeft = CAST(resources:GetObject("smartDataBrowser3.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:smartDataBrowser3:SettingsKey = resources:GetString("smartDataBrowser3.SettingsKey":U).
        THIS-OBJECT:smartDataBrowser3:Size = CAST(resources:GetObject("smartDataBrowser3.Size":U), System.Drawing.Size).
        THIS-OBJECT:smartDataBrowser3:TabIndex = System.Convert:ToInt32(resources:GetObject("smartDataBrowser3.TabIndex":U)).
        THIS-OBJECT:smartDataBrowser3:Text = resources:GetString("smartDataBrowser3.Text":U).
        THIS-OBJECT:smartDataBrowser3:VisibleGridColumns = resources:GetString("smartDataBrowser3.VisibleGridColumns":U).
        /*  */
        /* PossibleGroupBindingSource */
        /*  */
        THIS-OBJECT:PossibleGroupBindingSource:AddFields = FALSE.
        THIS-OBJECT:PossibleGroupBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:PossibleGroupBindingSource:ClearSchema = FALSE.
        THIS-OBJECT:PossibleGroupBindingSource:EntityName = "Consultingwerk.SmartFramework.Authorization.MenuGroupBusinessEntity":U.
        THIS-OBJECT:PossibleGroupBindingSource:EntityTable = "ePossibleGroup":U.
        THIS-OBJECT:PossibleGroupBindingSource:ImportSchema = FALSE.
        THIS-OBJECT:PossibleGroupBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:PossibleGroupBindingSource:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar20 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar20[1] = NEW Progress.Data.ColumnPropDesc("MenuGroupGuid":U, "MenuGroupGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar20[2] = NEW Progress.Data.ColumnPropDesc("MenuGuid":U, "MenuGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar20[3] = NEW Progress.Data.ColumnPropDesc("GroupGuid":U, "GroupGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar20[4] = NEW Progress.Data.ColumnPropDesc("GroupName":U, "GroupName":U, Progress.Data.DataType:CHARACTER).
        arrayvar20[5] = NEW Progress.Data.ColumnPropDesc("LoginCompanyName":U, "LoginCompanyName":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar20.
        THIS-OBJECT:PossibleGroupBindingSource:TableSchema = tableDesc1.
        /*  */
        /* PossibleGroupTableAdapter */
        /*  */
        THIS-OBJECT:PossibleGroupTableAdapter:AppServerPartition = resources:GetString("PossibleGroupTableAdapter.AppServerPartition":U).
        THIS-OBJECT:PossibleGroupTableAdapter:BindingSourceExcludeFields = resources:GetString("PossibleGroupTableAdapter.BindingSourceExcludeFields":U).
        THIS-OBJECT:PossibleGroupTableAdapter:BindingSourceIncludeFields = resources:GetString("PossibleGroupTableAdapter.BindingSourceIncludeFields":U).
        THIS-OBJECT:PossibleGroupTableAdapter:LinkDataSource = ?.
        THIS-OBJECT:PossibleGroupTableAdapter:LinkNavigationSource = ?.
        THIS-OBJECT:PossibleGroupTableAdapter:QuerySort = resources:GetString("PossibleGroupTableAdapter.QuerySort":U).
        THIS-OBJECT:PossibleGroupTableAdapter:QueryString = resources:GetString("PossibleGroupTableAdapter.QueryString":U).
        /*  */
        /* smartDataBrowser2 */
        /*  */
        THIS-OBJECT:smartDataBrowser2:AccessibleDescription = resources:GetString("smartDataBrowser2.AccessibleDescription":U).
        THIS-OBJECT:smartDataBrowser2:AccessibleName = resources:GetString("smartDataBrowser2.AccessibleName":U).
        THIS-OBJECT:smartDataBrowser2:Anchor = CAST(resources:GetObject("smartDataBrowser2.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartDataBrowser2:BackgroundImage = CAST(resources:GetObject("smartDataBrowser2.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:smartDataBrowser2:BackgroundImageLayout = CAST(resources:GetObject("smartDataBrowser2.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:smartDataBrowser2:DataSource = THIS-OBJECT:MenuGroupBindingSource.
        appearance15:BackColor = System.Drawing.SystemColors:Window.
        appearance15:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        appearance15:TextHAlignAsString = resources:GetString("appearance15.TextHAlignAsString":U).
        appearance15:TextVAlignAsString = resources:GetString("appearance15.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Appearance = appearance15.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ExtendLastColumn.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn1:Header:VisiblePosition = 2.
        ultraGridColumn1:Hidden = TRUE.
        ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn2:Header:VisiblePosition = 3.
        ultraGridColumn2:Hidden = TRUE.
        ultraGridColumn8:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn8:Header:VisiblePosition = 4.
        ultraGridColumn8:Hidden = TRUE.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn3:Header:Caption = resources:GetString("resource.Caption2":U).
        ultraGridColumn3:Header:VisiblePosition = 0.
        ultraGridColumn3:Width = 155.
        ultraGridColumn4:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn4:Header:Caption = resources:GetString("resource.Caption3":U).
        ultraGridColumn4:Header:VisiblePosition = 1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar21 AS System.Object EXTENT 5 NO-UNDO.
        arrayvar21[1] = ultraGridColumn1.
        arrayvar21[2] = ultraGridColumn2.
        arrayvar21[3] = ultraGridColumn8.
        arrayvar21[4] = ultraGridColumn3.
        arrayvar21[5] = ultraGridColumn4.
        ultraGridBand2:Columns:AddRange(arrayvar21).
        ultraGridBand2:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand2:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand2:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand2:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:BandsSerializer:Add(ultraGridBand2).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:True.
        appearance16:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance16:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance16:BorderColor = System.Drawing.SystemColors:Window.
        appearance16:TextHAlignAsString = resources:GetString("appearance16.TextHAlignAsString":U).
        appearance16:TextVAlignAsString = resources:GetString("appearance16.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:GroupByBox:Appearance = appearance16.
        appearance17:ForeColor = System.Drawing.SystemColors:GrayText.
        appearance17:TextHAlignAsString = resources:GetString("appearance17.TextHAlignAsString":U).
        appearance17:TextVAlignAsString = resources:GetString("appearance17.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance17.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance18:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance18:BackColor2 = System.Drawing.SystemColors:Control.
        appearance18:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance18:ForeColor = System.Drawing.SystemColors:GrayText.
        appearance18:TextHAlignAsString = resources:GetString("appearance18.TextHAlignAsString":U).
        appearance18:TextVAlignAsString = resources:GetString("appearance18.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:GroupByBox:PromptAppearance = appearance18.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:MaxRowScrollRegions = 1.
        appearance19:BackColor = System.Drawing.SystemColors:Window.
        appearance19:ForeColor = System.Drawing.SystemColors:ControlText.
        appearance19:TextHAlignAsString = resources:GetString("appearance19.TextHAlignAsString":U).
        appearance19:TextVAlignAsString = resources:GetString("appearance19.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:ActiveCellAppearance = appearance19.
        appearance20:BackColor = System.Drawing.SystemColors:Highlight.
        appearance20:ForeColor = System.Drawing.SystemColors:HighlightText.
        appearance20:TextHAlignAsString = resources:GetString("appearance20.TextHAlignAsString":U).
        appearance20:TextVAlignAsString = resources:GetString("appearance20.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:ActiveRowAppearance = appearance20.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance21:BackColor = System.Drawing.SystemColors:Window.
        appearance21:TextHAlignAsString = resources:GetString("appearance21.TextHAlignAsString":U).
        appearance21:TextVAlignAsString = resources:GetString("appearance21.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:CardAreaAppearance = appearance21.
        appearance22:BorderColor = System.Drawing.Color:Silver.
        appearance22:TextHAlignAsString = resources:GetString("appearance22.TextHAlignAsString":U).
        appearance22:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        appearance22:TextVAlignAsString = resources:GetString("appearance22.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:CellAppearance = appearance22.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:CellPadding = 0.
        appearance23:BackColor = System.Drawing.SystemColors:Control.
        appearance23:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance23:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance23:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance23:BorderColor = System.Drawing.SystemColors:Window.
        appearance23:TextHAlignAsString = resources:GetString("appearance23.TextHAlignAsString":U).
        appearance23:TextVAlignAsString = resources:GetString("appearance23.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:GroupByRowAppearance = appearance23.
        appearance24:TextHAlignAsString = resources:GetString("appearance24.TextHAlignAsString":U).
        appearance24:TextVAlignAsString = resources:GetString("appearance24.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:HeaderAppearance = appearance24.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance25:BackColor = System.Drawing.SystemColors:Window.
        appearance25:BorderColor = System.Drawing.Color:Silver.
        appearance25:TextHAlignAsString = resources:GetString("appearance25.TextHAlignAsString":U).
        appearance25:TextVAlignAsString = resources:GetString("appearance25.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:RowAppearance = appearance25.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance26:BackColor = System.Drawing.SystemColors:ControlLight.
        appearance26:TextHAlignAsString = resources:GetString("appearance26.TextHAlignAsString":U).
        appearance26:TextVAlignAsString = resources:GetString("appearance26.TextVAlignAsString":U).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:TemplateAddRowAppearance = appearance26.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:smartDataBrowser2:Dock = CAST(resources:GetObject("smartDataBrowser2.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:smartDataBrowser2:Font = CAST(resources:GetObject("smartDataBrowser2.Font":U), System.Drawing.Font).
        THIS-OBJECT:smartDataBrowser2:ImeMode = CAST(resources:GetObject("smartDataBrowser2.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:smartDataBrowser2:LinkDataSource = THIS-OBJECT:MenuGroupAdapter.
        THIS-OBJECT:smartDataBrowser2:Location = CAST(resources:GetObject("smartDataBrowser2.Location":U), System.Drawing.Point).
        THIS-OBJECT:smartDataBrowser2:MaximumSize = CAST(resources:GetObject("smartDataBrowser2.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:smartDataBrowser2:Name = "smartDataBrowser2":U.
        THIS-OBJECT:smartDataBrowser2:RightToLeft = CAST(resources:GetObject("smartDataBrowser2.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:smartDataBrowser2:SettingsKey = resources:GetString("smartDataBrowser2.SettingsKey":U).
        THIS-OBJECT:smartDataBrowser2:Size = CAST(resources:GetObject("smartDataBrowser2.Size":U), System.Drawing.Size).
        THIS-OBJECT:smartDataBrowser2:TabIndex = System.Convert:ToInt32(resources:GetObject("smartDataBrowser2.TabIndex":U)).
        THIS-OBJECT:smartDataBrowser2:Text = resources:GetString("smartDataBrowser2.Text":U).
        THIS-OBJECT:smartDataBrowser2:VisibleGridColumns = resources:GetString("smartDataBrowser2.VisibleGridColumns":U).
        /*  */
        /* MenuGroupBindingSource */
        /*  */
        THIS-OBJECT:MenuGroupBindingSource:AddFields = FALSE.
        THIS-OBJECT:MenuGroupBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:MenuGroupBindingSource:ClearSchema = FALSE.
        THIS-OBJECT:MenuGroupBindingSource:EntityName = "Consultingwerk.SmartFramework.Authorization.MenuGroupBusinessEntity":U.
        THIS-OBJECT:MenuGroupBindingSource:EntityTable = "eSmartMenuGroup":U.
        THIS-OBJECT:MenuGroupBindingSource:ImportSchema = FALSE.
        THIS-OBJECT:MenuGroupBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:MenuGroupBindingSource:NoLOBs = FALSE.
        tableDesc2:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar22 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar22[1] = NEW Progress.Data.ColumnPropDesc("MenuGroupGuid":U, "MenuGroupGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar22[2] = NEW Progress.Data.ColumnPropDesc("MenuGuid":U, "MenuGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar22[3] = NEW Progress.Data.ColumnPropDesc("GroupGuid":U, "GroupGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar22[4] = NEW Progress.Data.ColumnPropDesc("GroupName":U, "GroupName":U, Progress.Data.DataType:CHARACTER).
        arrayvar22[5] = NEW Progress.Data.ColumnPropDesc("LoginCompanyName":U, "LoginCompanyName":U, Progress.Data.DataType:CHARACTER).
        tableDesc2:Columns = arrayvar22.
        THIS-OBJECT:MenuGroupBindingSource:TableSchema = tableDesc2.
        /*  */
        /* MenuGroupAdapter */
        /*  */
        THIS-OBJECT:MenuGroupAdapter:AutoCommit = FALSE.
        THIS-OBJECT:MenuGroupAdapter:DatasetControllerType = "Consultingwerk.SmartFramework.Authorization.MenuGroupDatasetController":U.
        THIS-OBJECT:MenuGroupAdapter:EntityName = "Consultingwerk.SmartFramework.Authorization.MenuGroupBusinessEntity":U.
        THIS-OBJECT:MenuGroupAdapter:EntityTable = "eSmartMenuGroup":U.
        THIS-OBJECT:MenuGroupAdapter:ForeignFields = "MenuGuid,MenuGuid":U.
        THIS-OBJECT:MenuGroupAdapter:LinkCommitSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:MenuGroupAdapter:LinkDataSource = THIS-OBJECT:MenuAdapter.
        THIS-OBJECT:MenuGroupAdapter:LinkNavigationSource = ?.
        THIS-OBJECT:MenuGroupAdapter:NonUpdatableColumns = "":U.
        THIS-OBJECT:MenuGroupAdapter:QuerySort = "":U.
        THIS-OBJECT:MenuGroupAdapter:ThrowErrorsFromRetrieveData = FALSE.
        THIS-OBJECT:MenuGroupAdapter:AfterRejectTransaction:Subscribe(THIS-OBJECT:MenuGroupAdapter_AfterRejectTransaction).
        THIS-OBJECT:MenuGroupAdapter:SmartTransactionStateChanged:Subscribe(THIS-OBJECT:MenuGroupAdapter_SmartTransactionStateChanged).
        THIS-OBJECT:MenuGroupAdapter:AfterRetrieveData:Subscribe(THIS-OBJECT:MenuGroupAdapter_AfterRetrieveData).
        /*  */
        /* SmartMenuMaintenanceForm_Fill_Panel */
        /*  */
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:AccessibleDescription = resources:GetString("SmartMenuMaintenanceForm_Fill_Panel.AccessibleDescription":U).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:AccessibleName = resources:GetString("SmartMenuMaintenanceForm_Fill_Panel.AccessibleName":U).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:Anchor = CAST(resources:GetObject("SmartMenuMaintenanceForm_Fill_Panel.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:AutoSize = System.Convert:ToBoolean(resources:GetObject("SmartMenuMaintenanceForm_Fill_Panel.AutoSize":U)).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:BackgroundImageLayout = CAST(resources:GetObject("SmartMenuMaintenanceForm_Fill_Panel.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        /*  */
        /* SmartMenuMaintenanceForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:ClientArea:AccessibleDescription = resources:GetString("SmartMenuMaintenanceForm_Fill_Panel.ClientArea.AccessibleDescription":U).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:ClientArea:AccessibleName = resources:GetString("SmartMenuMaintenanceForm_Fill_Panel.ClientArea.AccessibleName":U).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraSplitter1).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:MenuTree).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:ClientArea:Font = CAST(resources:GetObject("SmartMenuMaintenanceForm_Fill_Panel.ClientArea.Font":U), System.Drawing.Font).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:Dock = CAST(resources:GetObject("SmartMenuMaintenanceForm_Fill_Panel.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:Font = CAST(resources:GetObject("SmartMenuMaintenanceForm_Fill_Panel.Font":U), System.Drawing.Font).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:ImeMode = CAST(resources:GetObject("SmartMenuMaintenanceForm_Fill_Panel.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:Location = CAST(resources:GetObject("SmartMenuMaintenanceForm_Fill_Panel.Location":U), System.Drawing.Point).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:MaximumSize = CAST(resources:GetObject("SmartMenuMaintenanceForm_Fill_Panel.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:Name = "SmartMenuMaintenanceForm_Fill_Panel":U.
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:RightToLeft = CAST(resources:GetObject("SmartMenuMaintenanceForm_Fill_Panel.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:Size = CAST(resources:GetObject("SmartMenuMaintenanceForm_Fill_Panel.Size":U), System.Drawing.Size).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:TabIndex = System.Convert:ToInt32(resources:GetObject("SmartMenuMaintenanceForm_Fill_Panel.TabIndex":U)).
        /*  */
        /* MenuTree */
        /*  */
        THIS-OBJECT:MenuTree:AccessibleDescription = resources:GetString("MenuTree.AccessibleDescription":U).
        THIS-OBJECT:MenuTree:AccessibleName = resources:GetString("MenuTree.AccessibleName":U).
        THIS-OBJECT:MenuTree:AllowDrop = TRUE.
        THIS-OBJECT:MenuTree:Anchor = CAST(resources:GetObject("MenuTree.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:MenuTree:BackgroundImageLayout = CAST(resources:GetObject("MenuTree.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:MenuTree:DisplayStyle = Infragistics.Win.UltraWinTree.UltraTreeDisplayStyle:WindowsVista.
        THIS-OBJECT:MenuTree:Dock = CAST(resources:GetObject("MenuTree.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:MenuTree:Font = CAST(resources:GetObject("MenuTree.Font":U), System.Drawing.Font).
        THIS-OBJECT:MenuTree:HideSelection = FALSE.
        THIS-OBJECT:MenuTree:ImeMode = CAST(resources:GetObject("MenuTree.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:MenuTree:Location = CAST(resources:GetObject("MenuTree.Location":U), System.Drawing.Point).
        THIS-OBJECT:MenuTree:MaximumSize = CAST(resources:GetObject("MenuTree.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:MenuTree:Name = "MenuTree":U.
        m_override1:SelectionType = Infragistics.Win.UltraWinTree.SelectType:Single.
        THIS-OBJECT:MenuTree:Override = m_override1.
        THIS-OBJECT:MenuTree:RightToLeft = CAST(resources:GetObject("MenuTree.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:MenuTree:Size = CAST(resources:GetObject("MenuTree.Size":U), System.Drawing.Size).
        THIS-OBJECT:MenuTree:TabIndex = System.Convert:ToInt32(resources:GetObject("MenuTree.TabIndex":U)).
        THIS-OBJECT:MenuTree:AfterSelect:Subscribe(THIS-OBJECT:MenuTree_AfterSelect).
        THIS-OBJECT:MenuTree:BeforeExpand:Subscribe(THIS-OBJECT:MenuTree_BeforeExpand).
        THIS-OBJECT:MenuTree:BeforeSelect:Subscribe(THIS-OBJECT:MenuTree_BeforeSelect).
        THIS-OBJECT:MenuTree:DragDrop:Subscribe(THIS-OBJECT:MenuTree_DragDrop).
        THIS-OBJECT:MenuTree:DragOver:Subscribe(THIS-OBJECT:MenuTree_DragOver).
        /*  */
        /* ultraTabControl1 */
        /*  */
        THIS-OBJECT:ultraTabControl1:AccessibleDescription = resources:GetString("ultraTabControl1.AccessibleDescription":U).
        THIS-OBJECT:ultraTabControl1:AccessibleName = resources:GetString("ultraTabControl1.AccessibleName":U).
        THIS-OBJECT:ultraTabControl1:Anchor = CAST(resources:GetObject("ultraTabControl1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:BackgroundImage = CAST(resources:GetObject("ultraTabControl1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraTabControl1:BackgroundImageLayout = CAST(resources:GetObject("ultraTabControl1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl2).
        THIS-OBJECT:ultraTabControl1:Dock = CAST(resources:GetObject("ultraTabControl1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraTabControl1:Font = CAST(resources:GetObject("ultraTabControl1.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraTabControl1:ImeMode = CAST(resources:GetObject("ultraTabControl1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraTabControl1:Location = CAST(resources:GetObject("ultraTabControl1.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraTabControl1:MaximumSize = CAST(resources:GetObject("ultraTabControl1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:RightToLeft = CAST(resources:GetObject("ultraTabControl1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        THIS-OBJECT:ultraTabControl1:Size = CAST(resources:GetObject("ultraTabControl1.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraTabControl1:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraTabControl1.TabIndex":U)).
        appearance27:Image = CAST(resources:GetObject("appearance27.Image":U), System.Object).
        appearance27:TextHAlignAsString = resources:GetString("appearance27.TextHAlignAsString":U).
        appearance27:TextVAlignAsString = resources:GetString("appearance27.TextVAlignAsString":U).
        ultraTab1:Appearance = appearance27.
        ultraTab1:Key = "Menu":U.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        ultraTab1:Text = resources:GetString("ultraTab1.Text":U).
        appearance28:Image = CAST(resources:GetObject("appearance28.Image":U), System.Object).
        appearance28:TextHAlignAsString = resources:GetString("appearance28.TextHAlignAsString":U).
        appearance28:TextVAlignAsString = resources:GetString("appearance28.TextVAlignAsString":U).
        ultraTab2:Appearance = appearance28.
        ultraTab2:Key = "GroupAssignement":U.
        ultraTab2:TabPage = THIS-OBJECT:ultraTabPageControl2.
        ultraTab2:Text = resources:GetString("ultraTab2.Text":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar23 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 2 NO-UNDO.
        arrayvar23[1] = ultraTab1.
        arrayvar23[2] = ultraTab2.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar23).
        THIS-OBJECT:ultraTabControl1:ViewStyle = Infragistics.Win.UltraWinTabControl.ViewStyle:Office2007.
        THIS-OBJECT:ultraTabControl1:SelectedTabChanged:Subscribe(THIS-OBJECT:ultraTabControl1_SelectedTabChanged).
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        THIS-OBJECT:ultraTabSharedControlsPage1:AccessibleDescription = resources:GetString("ultraTabSharedControlsPage1.AccessibleDescription":U).
        THIS-OBJECT:ultraTabSharedControlsPage1:AccessibleName = resources:GetString("ultraTabSharedControlsPage1.AccessibleName":U).
        THIS-OBJECT:ultraTabSharedControlsPage1:AutoScroll = System.Convert:ToBoolean(resources:GetObject("ultraTabSharedControlsPage1.AutoScroll":U)).
        THIS-OBJECT:ultraTabSharedControlsPage1:BackgroundImageLayout = CAST(resources:GetObject("ultraTabSharedControlsPage1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraTabSharedControlsPage1:Font = CAST(resources:GetObject("ultraTabSharedControlsPage1.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraTabSharedControlsPage1:ImeMode = CAST(resources:GetObject("ultraTabSharedControlsPage1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraTabSharedControlsPage1:Location = CAST(resources:GetObject("ultraTabSharedControlsPage1.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraTabSharedControlsPage1:MaximumSize = CAST(resources:GetObject("ultraTabSharedControlsPage1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        THIS-OBJECT:ultraTabSharedControlsPage1:RightToLeft = CAST(resources:GetObject("ultraTabSharedControlsPage1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = CAST(resources:GetObject("ultraTabSharedControlsPage1.Size":U), System.Drawing.Size).
        /*  */
        /* m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:AccessibleDescription = resources:GetString("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left.AccessibleDescription":U).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:AccessibleName = resources:GetString("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left.AccessibleName":U).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:Anchor = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:BackgroundImage = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:BackgroundImageLayout = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:ImeMode = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:Location = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left.Location":U), System.Drawing.Point).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:MaximumSize = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:Name = "m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:RightToLeft = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:Size = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left.Size":U), System.Drawing.Size).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:AccessibleDescription = resources:GetString("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right.AccessibleDescription":U).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:AccessibleName = resources:GetString("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right.AccessibleName":U).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:Anchor = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:BackgroundImage = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:BackgroundImageLayout = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:ImeMode = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:Location = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right.Location":U), System.Drawing.Point).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:MaximumSize = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:Name = "m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:RightToLeft = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:Size = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right.Size":U), System.Drawing.Size).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top:AccessibleDescription = resources:GetString("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top.AccessibleDescription":U).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top:AccessibleName = resources:GetString("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top.AccessibleName":U).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top:Anchor = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top:BackgroundImage = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top:BackgroundImageLayout = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top:ImeMode = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top:Location = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top.Location":U), System.Drawing.Point).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top:MaximumSize = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top:Name = "m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top:RightToLeft = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top:Size = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top.Size":U), System.Drawing.Size).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:AccessibleDescription = resources:GetString("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom.AccessibleDescription":U).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:AccessibleName = resources:GetString("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom.AccessibleName":U).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:Anchor = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:BackgroundImage = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:BackgroundImageLayout = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:ImeMode = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:Location = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom.Location":U), System.Drawing.Point).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:MaximumSize = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:Name = "m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:RightToLeft = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:Size = CAST(resources:GetObject("m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom.Size":U), System.Drawing.Size).
        THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* MenuBindingSource */
        /*  */
        THIS-OBJECT:MenuBindingSource:AddFields = FALSE.
        THIS-OBJECT:MenuBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:MenuBindingSource:ClearSchema = FALSE.
        THIS-OBJECT:MenuBindingSource:EntityName = "Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U.
        THIS-OBJECT:MenuBindingSource:EntityTable = "eSmartMenu":U.
        THIS-OBJECT:MenuBindingSource:ImportSchema = FALSE.
        THIS-OBJECT:MenuBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:MenuBindingSource:NoLOBs = FALSE.
        tableDesc3:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar24 AS Progress.Data.ColumnPropDesc EXTENT 12 NO-UNDO.
        arrayvar24[1] = NEW Progress.Data.ColumnPropDesc("MenuGuid":U, "MenuGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar24[2] = NEW Progress.Data.ColumnPropDesc("ParentMenuGuid":U, "ParentMenuGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar24[3] = NEW Progress.Data.ColumnPropDesc("MenuName":U, "MenuName":U, Progress.Data.DataType:CHARACTER).
        arrayvar24[4] = NEW Progress.Data.ColumnPropDesc("MenuStructureType":U, "MenuStructureType":U, Progress.Data.DataType:CHARACTER).
        arrayvar24[5] = NEW Progress.Data.ColumnPropDesc("MenuBeginsAGroup":U, "MenuBeginsAGroup":U, Progress.Data.DataType:LOGICAL).
        arrayvar24[6] = NEW Progress.Data.ColumnPropDesc("MenuSmallImage":U, "MenuSmallImage":U, Progress.Data.DataType:CHARACTER).
        arrayvar24[7] = NEW Progress.Data.ColumnPropDesc("MenuLargeImage":U, "MenuLargeImage":U, Progress.Data.DataType:CHARACTER).
        arrayvar24[8] = NEW Progress.Data.ColumnPropDesc("MenuSequence":U, "MenuSequence":U, Progress.Data.DataType:INTEGER).
        arrayvar24[9] = NEW Progress.Data.ColumnPropDesc("FunctionGuid":U, "FunctionGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar24[10] = NEW Progress.Data.ColumnPropDesc("HasChild":U, "HasChild":U, Progress.Data.DataType:LOGICAL).
        arrayvar24[11] = NEW Progress.Data.ColumnPropDesc("FunctionName":U, "FunctionName":U, Progress.Data.DataType:CHARACTER).
        arrayvar24[12] = NEW Progress.Data.ColumnPropDesc("MenuStyleGuid":U, "MenuStyleGuid":U, Progress.Data.DataType:CHARACTER).
        tableDesc3:Columns = arrayvar24.
        THIS-OBJECT:MenuBindingSource:TableSchema = tableDesc3.
        /*  */
        /* ultraSplitter1 */
        /*  */
        THIS-OBJECT:ultraSplitter1:AccessibleDescription = resources:GetString("ultraSplitter1.AccessibleDescription":U).
        THIS-OBJECT:ultraSplitter1:AccessibleName = resources:GetString("ultraSplitter1.AccessibleName":U).
        THIS-OBJECT:ultraSplitter1:Anchor = CAST(resources:GetObject("ultraSplitter1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraSplitter1:BackgroundImage = CAST(resources:GetObject("ultraSplitter1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraSplitter1:BackgroundImageLayout = CAST(resources:GetObject("ultraSplitter1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraSplitter1:Dock = CAST(resources:GetObject("ultraSplitter1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraSplitter1:Font = CAST(resources:GetObject("ultraSplitter1.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraSplitter1:ImeMode = CAST(resources:GetObject("ultraSplitter1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraSplitter1:Location = CAST(resources:GetObject("ultraSplitter1.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraSplitter1:MaximumSize = CAST(resources:GetObject("ultraSplitter1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraSplitter1:Name = "ultraSplitter1":U.
        THIS-OBJECT:ultraSplitter1:RestoreExtent = 0.
        THIS-OBJECT:ultraSplitter1:RightToLeft = CAST(resources:GetObject("ultraSplitter1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraSplitter1:Size = CAST(resources:GetObject("ultraSplitter1.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraSplitter1:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraSplitter1.TabIndex":U)).
        /*  */
        /* SmartMenuMaintenanceForm */
        /*  */
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ClientSize = CAST(resources:GetObject("$this.ClientSize":U), System.Drawing.Size).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartMenuMaintenanceForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:Name = "SmartMenuMaintenanceForm":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:RightToLeftLayout = System.Convert:ToBoolean(resources:GetObject("$this.RightToLeftLayout":U)).
        THIS-OBJECT:StartPosition = CAST(resources:GetObject("$this.StartPosition":U), System.Windows.Forms.FormStartPosition).
        THIS-OBJECT:Text = resources:GetString("$this.Text":U).
        THIS-OBJECT:WindowPositionRegistryKey = resources:GetString("$this.WindowPositionRegistryKey":U).
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartMenuViewer1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:MenuAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl2:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartDataBrowser3, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:PossibleGroupBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:PossibleGroupTableAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartDataBrowser2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:MenuGroupBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:MenuGroupAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:SmartMenuMaintenanceForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:MenuTree, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:MenuBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invokes the population of the tree after retrieving data.
        Notes:   
        @param sender The source of the event
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MenuAdapter_AfterRetrieveData (sender AS System.Object, 
                                                       e      AS System.EventArgs):
        
        Consultingwerk.Framework.FrameworkSettings:WaitStateManager:SetWaitState () .
        
        PopulateTree (oRootNode).

        SelectFirstMenuEntry ().

        FINALLY:
            Consultingwerk.Framework.FrameworkSettings:WaitStateManager:ClearWaitState () .
        END FINALLY.
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adding a new Node to the tree
        Notes:   
        @param sender The source of the event
        @param e An EventArgs that contains the event data
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID MenuAdapter_BeforeCreateRecord (sender AS System.Object, 
	                                                    e      AS BeforeCreateRecordEventArgs):
		
        DEFINE VARIABLE oNode AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        
        IF lDropActionAddingRecord THEN RETURN.
        
        IF MenuTree:SelectedNodes:Count > 0 THEN 
            oNode = MenuTree:SelectedNodes[0].
        ELSE 
            oNode = ?.
        
        AddNodeForNewMenuRecord (oNode, "New Menu Entry":U).
        
        IF VALID-OBJECT (oNode) AND 
           NOT oNode:Equals (oRootNode) THEN 
            e:BufferHandle::ParentMenuGuid = oNode:Key.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Refresh the Assignment button sensitivity after rejecting changes                                                                    
        Notes:                                                                        
        @param sender Reference to the calling Object
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MenuGroupAdapter_AfterRejectTransaction (sender AS System.Object,
                                                                 e      AS System.EventArgs):
        
        /* Marko Rüterbories, Consultingwerk Ltd. 12.10.2013
           Refresh Assignment button sensitivity */
        SetAssignmentButtonState ().

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the PossibleGroup Browser after new Data has been 
                 read by the MenuGroupAdapter
        Notes:   
        @param sender Reference to the calling Object
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MenuGroupAdapter_AfterRetrieveData (sender AS System.Object, 
                                                            e      AS System.EventArgs):
        
        IF NOT VALID-HANDLE (PossibleGroupTableAdapter:BufferHandle) AND  
           VALID-HANDLE (MenuGroupAdapter:DataSet) THEN 
            PossibleGroupTableAdapter:TempTableHandle = MenuGroupAdapter:DataSet::ePossibleGroup.
    
        PossibleGroupTableAdapter:RetrieveData ().

        /* Marko Rüterbories, Consultingwerk Ltd. 12.10.2013
           Refresh Assignment button sensitivity */
        SetAssignmentButtonState ().
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Makes sure that the PossibleAssignement Browser shows the correct
                 data
        Notes:   
        @param sender Reference to the calling Object
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MenuGroupAdapter_SmartTransactionStateChanged (sender AS System.Object,
                                                                       e      AS System.EventArgs):
        
        IF lPendingChangesInMenuGroupAssignement AND 
           MenuGroupAdapter:SmartTransactionState = Consultingwerk.SmartComponents.Enum.TransactionStateEnum:NoPendingChanges THEN 
            PossibleGroupTableAdapter:RetrieveData ().

        /* Marko Rüterbories, Consultingwerk Ltd. 12.10.2013
           Refresh Assignment button sensitivity */
        SetAssignmentButtonState ().

        FINALLY:
            lPendingChangesInMenuGroupAssignement = FALSE.
        END FINALLY.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Save or reject pending changes of the MenuGroupAdapter if required
        Notes:   
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event 
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID MenuTree_BeforeSelect (sender AS System.Object,
	                                           e      AS Infragistics.Win.UltraWinTree.BeforeSelectEventArgs):
		
		DEFINE VARIABLE oMessageProvider AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO.
		DEFINE VARIABLE oMessage         AS Consultingwerk.SmartFramework.Message          NO-UNDO.
		
		IF MenuGroupAdapter:SmartTransactionState <> TransactionStateEnum:PendingChanges THEN RETURN.
		    
        oMessageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IMessageProvider} .
        
        IF VALID-OBJECT (oMessageProvider) THEN 
        DO ON ERROR UNDO, THROW:
            oMessage = oMessageProvider:GetMessage ("SFR":U, 2) .
            
            IF MessageFormHelper:ShowMessage (oMessage) = DialogResultEnum:DialogResultYes THEN DO:
                MenuGroupAdapter:CommitTransaction ().
            END.
            ELSE DO:
                MenuGroupAdapter:RejectTransaction ().
            END.
            CATCH err AS Progress.Lang.Error :
                e:Cancel = TRUE.
                ErrorHelper:ShowErrorMessage (err) .    
            END CATCH.
        END.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creates a new Node in the MenuTree and a SmartMenu entry in the 
                 TEMP-TABLE eSmartMenu
        Notes:   
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MenuTree_DragDrop (sender AS System.Object, 
                                           e      AS System.Windows.Forms.DragEventArgs):
                                               
        DEFINE VARIABLE oPoint      AS System.Drawing.Point                        NO-UNDO.
        DEFINE VARIABLE oDictionary AS "Dictionary<String,String>":U               NO-UNDO.
        DEFINE VARIABLE oNode       AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        DEFINE VARIABLE hBuffer     AS HANDLE                                      NO-UNDO.
        
        oPoint = MenuTree:PointToClient (NEW System.Drawing.Point (e:X, e:Y)).
        
        oNode = MenuTree:GetNodeFromPoint (oPoint).
        
        oDictionary = NEW "Dictionary<String,String>":U ().
        
        IF e:Data:GetDataPresent (oDictionary:GetType ()) THEN DO:
            lDropActionAddingRecord = TRUE.
            
            oDictionary = CAST (e:Data:GetData (oDictionary:GetType ()), 
                                "Dictionary<String,String>":U).
            
            AddNodeForNewMenuRecord (oNode, oDictionary["eSmartFunction.FunctionName":U]).
            
            smartMenuViewer1:AddRecord ().
            
            hBuffer = MenuAdapter:Dataset::eSmartMenu.
            
            ASSIGN 
                hBuffer::MenuGuid          = cAddingRecordMenuGuid
                hBuffer::ParentMenuGuid    = oNode:Key
                hBuffer::MenuName          = oDictionary["eSmartFunction.FunctionName":U]
                hBuffer::FunctionGuid      = oDictionary["eSmartFunction.FunctionGuid":U]
                hBuffer::FunctionName      = oDictionary["eSmartFunction.FunctionName":U]
                hBuffer::MenuSmallImage    = oDictionary["eSmartFunction.FunctionSmallImage":U]
                hBuffer::MenuLargeImage    = oDictionary["eSmartFunction.FunctionLargeImage":U]
                hBuffer::MenuStructureType = "ITEM":U
                .
            
            MenuAdapter:BindingSource:Refresh ().
        END .

        FINALLY:
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oPoint).
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oDictionary).
            
            lDropActionAddingRecord = FALSE.
        END FINALLY.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Allows or disallows the drop for a drag & drop action
        Notes:   
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MenuTree_DragOver (sender AS System.Object, 
                                           e      AS System.Windows.Forms.DragEventArgs):
                                               
        DEFINE VARIABLE oPoint        AS System.Drawing.Point          NO-UNDO.
        DEFINE VARIABLE oDictionary   AS "Dictionary<String,String>":U NO-UNDO.
        DEFINE VARIABLE lAllowContent AS LOGICAL                       NO-UNDO INIT FALSE.

        oDictionary = NEW "Dictionary<String,String>":U ().
        
        IF e:Data:GetDataPresent (oDictionary:GetType ()) THEN DO:
            oDictionary = CAST (e:Data:GetData (oDictionary:GetType ()), 
                                "Dictionary<String,String>":U).
            IF oDictionary:ContainsKey ("eSmartFunction.FunctionGuid":U) THEN 
                ASSIGN lAllowContent = TRUE.
        END .
        
        IF NOT lAllowContent THEN DO:
            e:Effect = System.Windows.Forms.DragDropEffects:None.
            RETURN.
        END.
        
        oPoint = MenuTree:PointToClient (NEW System.Drawing.Point (e:X, e:Y)).
        
        IF VALID-OBJECT (MenuTree:GetNodeFromPoint (oPoint)) THEN 
            e:Effect = System.Windows.Forms.DragDropEffects:Link.

        FINALLY:
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oPoint).
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oDictionary).
        END FINALLY.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Override of the OnShown Method to first show the form and then 
                 initiate reading data.
        Notes:   
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnShown (e AS System.EventArgs):
    
        DEFINE VARIABLE cExpand AS CHARACTER NO-UNDO.
        
        SUPER:OnShown (e).

        PROCESS EVENTS . 

        CreateRootNode ().

        MenuAdapter:BatchSize = 0 . 
        MenuAdapter:RetrieveData ().
        
        IF VALID-OBJECT (MenuGroupAdapter:DatasetController) AND 
           TYPE-OF (MenuGroupAdapter:DatasetController, Consultingwerk.SmartFramework.Authorization.MenuGroupDatasetController) THEN 
            oMenuGroupDatasetController = CAST (MenuGroupAdapter:DatasetController, Consultingwerk.SmartFramework.Authorization.MenuGroupDatasetController).

        PROCESS EVENTS.
        
        /* Marko Rüterbories, Consultingwerk Ltd. 16.01.2013
           Expands all TreeNodes */
        ASSIGN cExpand = THIS-OBJECT:LoadSettings ("Nodes Expanded":U) .

        IF cExpand = "false":U THEN
            THIS-OBJECT:CollapseAll () .
        ELSE
            THIS-OBJECT:ExpandAll () .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates all child records of the given GUID to the tree.
        Notes:   
        @param poParentNode Object reference to the parent UltraTreeNode
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PopulateTree (poParentNode AS Infragistics.Win.UltraWinTree.UltraTreeNode):
        
        DEFINE VARIABLE hQuery          AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hBuffer         AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cParentMenuGuid AS CHARACTER NO-UNDO.
        
        DEFINE VARIABLE oParentCollection AS Infragistics.Win.UltraWinTree.TreeNodesCollection NO-UNDO.
        
        IF VALID-OBJECT (poParentNode) AND poParentNode:HasNodes THEN RETURN.
        
        hBuffer = MenuAdapter:DataSet::eSmartMenu.
        
        IF VALID-OBJECT (poParentNode) THEN 
            IF poParentNode:Equals(oRootNode) THEN 
                ASSIGN 
                    oParentCollection = poParentNode:Nodes
                    cParentMenuGuid   = "":U
                    .
            ELSE 
                ASSIGN 
                    oParentCollection = poParentNode:Nodes
                    cParentMenuGuid   = poParentNode:Key
                    .
        ELSE 
            ASSIGN 
                oParentCollection = MenuTree:Nodes
                cParentMenuGuid   = "":U
                .
        
        
        /* Mike Fechner, Consultingwerk Ltd. 01.12.2014
           SCL-555 : Ignore blank menu guid as this would cause an infinitive loop */
        hQuery = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (hBuffer, 
                                                                      SUBSTITUTE ("FOR EACH eSmartMenu WHERE eSmartMenu.ParentMenuGuid = '&1' AND eSmartMenu.MenuGuid <> ''":U, 
                                                                                  cParentMenuGuid)).

        DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:
            
            DEFINE VARIABLE oNode AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
            
            oNode = NEW Infragistics.Win.UltraWinTree.UltraTreeNode (hBuffer::MenuGuid, hBuffer::MenuName).
            
            IF hBuffer::HasChild THEN
                CreateDummyChildNode (oNode).
            
            oParentCollection:Add (oNode).
            
            hQuery:GET-NEXT ().
        END.
        
        FINALLY:
            IF VALID-HANDLE (hQuery) THEN 
                DELETE OBJECT hQuery.
        END FINALLY.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Removes the Node with the given key
        Notes:   
        @param pcNodeKey The key of the node to delete
        @return The returnvalue is TRUE if the node has been deleted
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE LOGICAL RemoveNodeWithKey (pcNodeKey AS CHARACTER):
        
        DEFINE VARIABLE oNode                 AS Infragistics.Win.UltraWinTree.UltraTreeNode       NO-UNDO.
        DEFINE VARIABLE oParentNodeCollection AS Infragistics.Win.UltraWinTree.TreeNodesCollection NO-UNDO.
        
        IF pcNodeKey > "":U THEN DO:  
            IF MenuTree:SelectedNodes:Count > 0 AND 
               MenuTree:SelectedNodes[0]:Key = pcNodeKey THEN 
                oNode = MenuTree:SelectedNodes[0].
            ELSE 
                oNode = MenuTree:GetNodeByKey (pcNodeKey).
            
            MenuTree:SelectedNodes:Clear ().

            UserInterfaceHelper:ProcessEvents () .

            IF VALID-OBJECT (oNode:Parent) THEN DO:
                oParentNodeCollection = oNode:Parent:Nodes.
                
                MenuTree:ActiveNode = oNode:Parent . 
                oNode:Parent:Selected = TRUE.
            END.
            ELSE DO:
                oParentNodeCollection = MenuTree:Nodes.
            END.
            
            oParentNodeCollection:Remove (oNode).
            
            RETURN TRUE.
        END.
        
        RETURN FALSE.
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Selects the first real Menu Entry if possible
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID SelectFirstMenuEntry ():
        
        MenuTree:SelectedNodes:Clear ().
        
        IF oRootNode:HasNodes THEN DO:
            oRootNode:Nodes[0]:BringIntoView ().
            oRootNode:Nodes[0]:Selected = TRUE.
            
            MenuTree:ActiveNode = oRootNode:Nodes[0].
        END.
        ELSE 
            oRootNode:Selected = TRUE.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Set Assignment buttons en or disabled according to the availability
                 of data to be assigned
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SetAssignmentButtonState ():
        
        ASSIGN 
            ButtonAssignGroupToMenu:Enabled     = PossibleGroupBindingSource:Count > 0
            ButtonUnAssignGroupFromMenu:Enabled = MenuGroupBindingSource:Count > 0
            .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Assign or unassign a Group from the Menu
        Notes:   
        @param sender The reference to the Instance of the SmartDataBrowser which has invoked the Method
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SmartDataBrowserDefaultActionHandler (sender AS SmartDataBrowser, 
                                                             e      AS System.EventArgs):

        CASE sender:
            WHEN smartDataBrowser2 THEN ButtonUnAssignGroupFromMenu_Click (sender, e).
            WHEN smartDataBrowser3 THEN ButtonAssignGroupToMenu_Click (sender, e).
        END CASE.        
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Remove the TreeNode when canceling the addition of a new menu record
        Notes:   
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartMenuViewer1_AfterCancelUpdate (sender AS System.Object, 
                                                            e      AS System.EventArgs):

        IF RemoveNodeWithKey (cAddingRecordMenuGuid) THEN 
            cAddingRecordMenuGuid = "".
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Remove the TreeNode when deleting a menu record
        Notes:   
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartMenuViewer1_AfterDeleteRecord (sender AS System.Object, 
                                                            e      AS System.EventArgs):
        
        IF RemoveNodeWithKey (cDeletingRecordMenuGuid) THEN 
            cDeletingRecordMenuGuid = "".

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets the NodeKey of a new menu record to the MenuGuid value assigned
                 from the create trigger
        Notes:   
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartMenuViewer1_AfterSaveChanges (sender AS System.Object, 
                                                           e      AS System.EventArgs):
        
        DEFINE VARIABLE oNode AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        
        IF cAddingRecordMenuGuid > "":U THEN DO:  
            IF MenuTree:SelectedNodes:Count > 0 AND 
               MenuTree:SelectedNodes[0]:Key = cAddingRecordMenuGuid THEN 
                oNode = MenuTree:SelectedNodes[0].
            ELSE 
                oNode = MenuTree:GetNodeByKey (cAddingRecordMenuGuid).

            oNode:Key  = MenuAdapter:GetFieldValues ("MenuGuid":U). 
        END.

        /* Marko Rüterbories, Consultingwerk Ltd. 12.10.2012
           When changing a prviously created record the oNode variable won't be
           valid at that time. */
        IF NOT VALID-OBJECT (oNode) AND 
           MenuTree:SelectedNodes:Count > 0 THEN 
            oNode = MenuTree:SelectedNodes[0].
        
        oNode:Text = MenuAdapter:GetFieldValues ("MenuName":U).

        cAddingRecordMenuGuid = "".
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Stores the MenuGuid of the menu record to be deleted in the variable
                 cDeletingRecordMenuGuid
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The CancelEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartMenuViewer1_BeforeDeleteRecord (sender AS System.Object, 
                                                             e      AS System.ComponentModel.CancelEventArgs):
        
        cDeletingRecordMenuGuid = MenuAdapter:GetFieldValues ("MenuGuid":U).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Disables the tree during update or add operations in the viewer
        Notes:   
        @param sender The source of the event
        @param e An SmartTableIOStateChangedEventArgs that contains the data for the event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartMenuViewer1_SmartTableIOStateChanged (sender AS System.Object, 
                                                                   e      AS Consultingwerk.SmartComponents.Base.SmartTableIOStateChangedEventArgs):
        
        IF e:SmartTableIOState = Consultingwerk.SmartComponents.Enum.TableIOStateEnum:ModifyingData THEN 
            MenuTree:Enabled = FALSE.
        ELSE 
            MenuTree:Enabled = TRUE.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Repositions the MenuAdapter to show the corresponding record
                 in the SmartMenuViewer
        Notes:   
        @param sender The source of the event
        @param e An SelectEventArgs that contains the data for the event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MenuTree_AfterSelect (sender AS System.Object, 
                                              e AS Infragistics.Win.UltraWinTree.SelectEventArgs):
        
        IF e:NewSelections:Count NE 1 THEN RETURN.

        /* Marko Rüterbories, Consultingwerk Ltd. 09.12.2013
           Avoid maintaining Groups on the root node by setting the Assignment tab enabled or disabled */
        IF e:NewSelections[0]:Level = 0 THEN DO:
            IF ultraTabControl1:ActiveTab:Key = "GroupAssignement":U AND 
               ultraTabControl1:Tabs:Exists("Menu":U) THEN 
                ASSIGN 
                    ultraTabControl1:SelectedTab = ultraTabControl1:Tabs["Menu":U]
                    ultraTabControl1:ActiveTab   = ultraTabControl1:Tabs["Menu":U]
                    .
            
            IF ultraTabControl1:Tabs:Exists("GroupAssignement":U) THEN 
                ultraTabControl1:Tabs["GroupAssignement":U]:Enabled = FALSE.
        END.
        ELSE 
            IF ultraTabControl1:Tabs:Exists("GroupAssignement":U) THEN 
                ultraTabControl1:Tabs["GroupAssignement":U]:Enabled = TRUE.
        
        MenuAdapter:FindRowWhere (SUBSTITUTE ("WHERE eSmartMenu.MenuGuid = '&1'":U, e:NewSelections[0]:Key)).

        MenuAdapter:QueryHandle:REPOSITION-TO-ROW (MenuAdapter:BindingSource:Position + 1) .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invokes the population of the tree after a node has been selected
                 and removes the DummyNode if nessesary.
        Notes:   
        @param sender The source of the event
        @param e A CancelableEventArgs that contains the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MenuTree_BeforeExpand (sender AS System.Object, 
                                                 e      AS Infragistics.Win.UltraWinTree.CancelableNodeEventArgs):
        
        DEFINE VARIABLE oNode AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        
        IF e:TreeNode:Nodes:Count = 1 THEN DO:

            IF e:TreeNode:Nodes[0]:Key BEGINS "DummyNode":U THEN DO: 
                oNode = e:TreeNode:Nodes[0].
                
                e:TreeNode:Nodes:Remove (oNode).
                oNode:Dispose ().
                DELETE OBJECT oNode.
            END.
            
            PopulateTree (e:TreeNode).
        
        END.
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Handler for the ToolClick Event of the smartToolbarController
        Notes:   
        @param sender The source of the event
        @param e A ToolClickEventArgs that contains the data for the event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID smartToolbarController1_ToolClick (sender AS System.Object, 
	                                                       e      AS ToolClickEventArgs):
        DEFINE VARIABLE cActiveNodeKey AS CHARACTER NO-UNDO.
		
		CASE e:Tool:Key:
		    WHEN "ReorderMenuItems":U THEN DO:
                IF VALID-OBJECT (MenuAdapter:DatasetController) AND 
                   TYPE-OF (MenuAdapter:DatasetController, MenuDatasetController) THEN DO ON ERROR UNDO, THROW: 
                    CAST (MenuAdapter:DatasetController, MenuDatasetController):ReorderItemsWithinLevel (THIS-OBJECT).
                
                    MenuTree:BeginUpdate ().

                    IF VALID-OBJECT (MenuTree:ActiveNode) THEN                     
                        cActiveNodeKey = MenuTree:ActiveNode:Key.
                    
                    MenuTree:SelectedNodes:Clear ().
                    oRootNode:Nodes:Clear ().
                    PopulateTree (oRootNode).
    
                    MenuTree:ExpandAll ().
                    
                    IF cActiveNodeKey > "":U THEN DO:
                        MenuTree:GetNodeByKey (cActiveNodeKey):Selected = TRUE.
                        MenuTree:ActiveNode = MenuTree:GetNodeByKey (cActiveNodeKey).
                    END.
                    
                    FINALLY:
                        MenuTree:EndUpdate ().
                    END FINALLY.
                END.
		    END.
		    WHEN "CopyMenuGuid":U THEN 
		        THIS-OBJECT:CopyMenuGuid () .
		    
		    WHEN "Expand all":U THEN 
		        THIS-OBJECT:ExpandAll () . 
		    
		    WHEN "Collapse all":U THEN 
		        THIS-OBJECT:CollapseAll () .
		    
		    
		END CASE.

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
                
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: When switching to the GroupAssignement tab the contextual tab for
                 this purpose gets shown
        Notes:   
        @param sender Reference to the calling Object
        @param e An EventArgs that contains the SelectedTabChanged event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTabControl1_SelectedTabChanged (sender AS System.Object, 
                                                             e      AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs):

        IF NOT VALID-OBJECT (smartToolbarController1:Ribbon) THEN RETURN.

        IF e:Tab:Key = "GroupAssignement":U THEN DO:
           IF NOT smartToolbarController1:Ribbon:ContextualTabGroups:Exists ("Menu":U) OR 
              NOT smartToolbarController1:Ribbon:ContextualTabGroups:Exists ("GroupAssignement":U) THEN RETURN.

            smartToolbarController1:Ribbon:ContextualTabGroups["Menu":U]:Visible = FALSE.
            smartToolbarController1:Ribbon:ContextualTabGroups["GroupAssignement":U]:Visible = TRUE.

            IF smartToolbarController1:Ribbon:Tabs:Exists ("GroupAssignement":U) THEN 
                smartToolbarController1:Ribbon:SelectedTab = smartToolbarController1:Ribbon:Tabs["GroupAssignement":U].
        END.
        ELSE DO:
           IF NOT smartToolbarController1:Ribbon:ContextualTabGroups:Exists ("Menu":U) OR 
              NOT smartToolbarController1:Ribbon:ContextualTabGroups:Exists ("GroupAssignement":U) THEN RETURN.

            smartToolbarController1:Ribbon:ContextualTabGroups["GroupAssignement":U]:Visible = FALSE.
            smartToolbarController1:Ribbon:ContextualTabGroups["Menu":U]:Visible = TRUE.

            IF smartToolbarController1:Ribbon:Tabs:Exists ("Menu":U) THEN 
                smartToolbarController1:Ribbon:SelectedTab = smartToolbarController1:Ribbon:Tabs["Menu":U].
        END.
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartMenuMaintenanceForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartMenuMaintenanceForm ():

    END DESTRUCTOR .

END CLASS.
