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
    File        : MenuTreeViewControl
    Purpose     : Menu Tree that is populated from an IMenuStructureProvider 
                  service and can start menu functions using the IMenuFunctionLauncher
                  service 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 07 16:56:15 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                      FROM PROPATH .
USING Consultingwerk.Util.*                 FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .
USING Consultingwerk.SmartFramework.*       FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.*  FROM PROPATH .
USING Consultingwerk.Windows.Controls.*     FROM PROPATH . 
USING Consultingwerk.Windows.Framework.*    FROM PROPATH .  
USING Infragistics.Win.UltraWinListView.*   FROM ASSEMBLY . 
USING Infragistics.Win.UltraWinTree.*       FROM ASSEMBLY .
USING Progress.Lang.*                       FROM PROPATH .    
USING Progress.Windows.UserControl          FROM ASSEMBLY .
USING System.Drawing.*                      FROM ASSEMBLY . 
USING System.Windows.Forms.*                FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Framework.Controls.MenuTreeViewControl 
    INHERITS UserControl: 

	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTree1 AS Infragistics.Win.UltraWinTree.UltraTree NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraListView1 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE timer1 AS System.Windows.Forms.Timer NO-UNDO.
    DEFINE PRIVATE VARIABLE oImageProvider AS Consultingwerk.Windows.Framework.IImageProvider NO-UNDO . 

    {Consultingwerk/Windows/Framework/ttMenuStructure.i}

    DEFINE VARIABLE oMenuProvider AS IMenuStructureProvider NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Event raised when the user selectes a TreeNode or the corresponding
                 item in the UltraListView                                                                   
        Notes:                            
        @param sender The reference to the object that raised the event
        @param e The TreeNodeSelectedEventArgs with the dat for this event                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT TreeNodeSelected SIGNATURE VOID 
       (sender AS System.Object, e AS TreeNodeSelectedEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the first level of TreeNodes (root nodes) should be 
                 expanded during OnLoad 
        Notes:   This is default behaviour
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ExpandFirstLevelAfterStart AS LOGICAL NO-UNDO INIT TRUE
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the second level of TreeNodes should be 
                 expanded during OnLoad 
        Notes:   By default only the Root Nodes will be expanded
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ExpandSecondLevelAfterStart AS LOGICAL NO-UNDO INIT FALSE
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the parent nodes (if present) Text should be included 
                 in the text shown for the search results 
        Notes:   When true, the ParentNodeTextSubstitute is used to compute the text
                 used in the search results, &1 for the parent nodes text, &2 for the
                 actual node  
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY IncludeParentNodeTextIntoSearchResult AS LOGICAL NO-UNDO INIT FALSE  
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the substitute pattern used for the search result texts 
                 when IncludeParentNodeTextIntoSearchResult is set to true
        Notes:   &1 for the parent nodes text, &2 for the
                 actual node  
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ParentNodeTextSubstitute AS CHARACTER NO-UNDO INIT "&1 / &2":U
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the root node's key
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RenderRootNode AS LOGICAL NO-UNDO INIT TRUE 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the root node's key
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RootNodeKey AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET. 
        
    /*------------------------------------------------------------------------------
        Purpose: Returns if the authorization checks should be skipps 
        Notes:   Allows to retrieve full menu structure to clients that perform 
                 authentication only after the first menu item has been selected 
                 etc.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SkipAuthorizationCheck AS LOGICAL NO-UNDO INIT FALSE  
    GET.
    SET. 
            
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the tree nodes should be sorted by the tree 
        Notes:   By default tree nodes are not sorted by the tree and thus use the 
                 order which is provided by the menu structure
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Sorted AS LOGICAL INITIAL FALSE NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the double click should be used for selecting items
                 in the UltraTree 
        Notes:   Previously the single click was used to select items. This property 
                 is to achieve backwards compatibility
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UseDoubleClickForItemSelect AS LOGICAL NO-UNDO INIT TRUE 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets the reference to the contained UltraTree                                                                     
        Notes:                                                                       
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UltraTree AS Infragistics.Win.UltraWinTree.UltraTree NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets the reference to the contained UltraListView                                                                     
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UltraListView AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO 
    GET.
    PROTECTED SET. 
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuTreeViewControl class                                                                     
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC MenuTreeViewControl ():
		
        SUPER().
        
        InitializeComponent().
        
        oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider} .
        
        THIS-OBJECT:UltraTree = ultraTree1 .
        THIS-OBJECT:UltraListView = ultraListView1.
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Expands or collapses the TreeNode with the given key                                                                     
        Notes:                                                      
        @param pcNodeKey The key of the node to process
        @param plExpand Logical value indicating if the Node should be expanded or collapsed
        @return Logical value indicating if the Node could be found               
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ExpandNode (pcNodeKey AS CHARACTER,
                                      plExpand AS LOGICAL):
        
        DEFINE VARIABLE oNode AS UltraTreeNode NO-UNDO . 
        
        oNode = ultraTree1:GetNodeByKey (pcNodeKey) .

        IF NOT VALID-OBJECT (oNode) THEN  
            RETURN FALSE .  
            
        oNode:Expanded = plExpand .

        /* Ensure, that all parent nodes of the expanded nodes
           are expanded */
        IF plExpand THEN DO:
            DO WHILE VALID-OBJECT (oNode):
                oNode:Expanded = TRUE .             
                oNode = oNode:Parent .
            END.
        END.

        RETURN TRUE . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fills the tree menu 
        Notes:   
        @param pcRootNodeKey The root node key to retrieve
        @param plRenderRootNode Render the Root Node as a Tree Node itself 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID FillMenuStructure (pcRootNodeKey AS CHARACTER,
	                                      plRenderRootNode AS LOGICAL):
		
		DEFINE VARIABLE cParentNodeKey AS CHARACTER NO-UNDO.
		
        oMenuProvider:GetMenuStructure (NEW GetMenuStructureParameter (pcRootNodeKey, 
                                                                       0,
                                                                       plRenderRootNode,
                                                                       THIS-OBJECT:SkipAuthorizationCheck),
                                        OUTPUT TABLE ttMenuStructure BY-REFERENCE) .
    
        GarbageCollectorHelper:ClearUltraTreeNodes (ultraTree1) .
        
        IF plRenderRootNode THEN DO:
            FIND ttMenuStructure WHERE ttMenuStructure.MenuKey = pcRootNodeKey NO-ERROR . 
            
            IF AVAILABLE ttMenuStructure THEN 
                cParentNodeKey = ttMenuStructure.ParentMenuKey . 
            ELSE 
                cParentNodeKey = "":U .  
            
            THIS-OBJECT:PopulateNodes (ultraTree1:Nodes, cParentNodeKey) .
        END.
        ELSE
            THIS-OBJECT:PopulateNodes (ultraTree1:Nodes, pcRootNodeKey) . 
    
        /* Mike Fechner, Consultingwerk Ltd. 07.01.2013
           Make first node level expanded by default */
        IF THIS-OBJECT:ExpandFirstLevelAfterStart THEN DO:
            {Consultingwerk/foreach.i UltraTreeNode oNode in ultraTree1:Nodes}
                oNode:Expanded = TRUE .
        
                IF THIS-OBJECT:ExpandSecondLevelAfterStart THEN DO:
                    {Consultingwerk/foreach.i UltraTreeNode oChildNode in oNode:Nodes}
                        oChildNode:Expanded = TRUE . 
                    END.     
                END.
            END.
        END.
        
        IF ultraTree1:Nodes:Count > 0 THEN 
            ultraTree1:ActiveNode = ultraTree1:Nodes[0] . 
        
        FINALLY:
            EMPTY TEMP-TABLE ttMenuStructure .		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Searches the Tree View Nodes for a Search String                                                                       
        Notes:   Populates the search results in the ultraListView1 instance, recursive method
        @param pcSearchString The string to search for
        @param poNodes The current iteration of the TreeNodesCollection                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FindNodes (pcSearchString AS CHARACTER,
                                     poNodes AS Infragistics.Win.UltraWinTree.TreeNodesCollection):
        
        DEFINE VARIABLE oEnumerator AS System.Collections.IEnumerator NO-UNDO .
        DEFINE VARIABLE oNode       AS UltraTreeNode                  NO-UNDO .
        DEFINE VARIABLE oItem       AS UltraListViewItem              NO-UNDO . 
        
        oEnumerator = poNodes:GetEnumerator() .
        oEnumerator:Reset() .
        
        DO WHILE oEnumerator:MoveNext():
            oNode = CAST (oEnumerator:Current, UltraTreeNode) .
    
            IF oNode:HasNodes THEN 
               FindNodes (pcSearchString, oNode:Nodes) . 
            ELSE IF oNode:Text MATCHES SUBSTITUTE ("*&1*":U, pcSearchString) THEN DO: 
                oItem = ultraListView1:Items:Add (oNode:Key) .
                
                IF THIS-OBJECT:IncludeParentNodeTextIntoSearchResult = TRUE AND VALID-OBJECT (oNode:Parent) THEN 
                    oItem:Value = SUBSTITUTE (THIS-OBJECT:ParentNodeTextSubstitute, 
                                              oNode:Parent:Text, oNode:Text) .  
                ELSE             
                    oItem:Value = oNode:Text . 
                
                IF oNode:LeftImages:Count > 0 THEN 
                   oItem:Appearance:Image = oNode:LeftImages[0] . 
                   
                oItem:Enabled = oNode:Enabled .     
                oItem:Tag = oNode .                   
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Puts the Focus into the Search Field
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FocusSearchField ():
        
        ultraTextEditor1:Focus () .
        ultraTextEditor1:SelectAll () .

    END METHOD .

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
        DEFINE VARIABLE ultraListViewItem1 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO.
        ultraListViewItem1 = NEW Infragistics.Win.UltraWinListView.UltraListViewItem("111":U, ?, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewItem2 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO.
        ultraListViewItem2 = NEW Infragistics.Win.UltraWinListView.UltraListViewItem("222":U, ?, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewItem3 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO.
        ultraListViewItem3 = NEW Infragistics.Win.UltraWinListView.UltraListViewItem("333":U, ?, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Controls.MenuTreeViewControl":U).
        THIS-OBJECT:ultraListView1 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTree1 = NEW Infragistics.Win.UltraWinTree.UltraTree().
        THIS-OBJECT:timer1 = NEW System.Windows.Forms.Timer(THIS-OBJECT:components).
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraListView1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraListView1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraListView1:BorderStyle = Infragistics.Win.UIElementBorderStyle:None.
        ultraListViewItem1:Key = "1":U.
        ultraListViewItem2:Key = "2":U.
        ultraListViewItem3:Key = "3":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinListView.UltraListViewItem EXTENT 3 NO-UNDO.
        arrayvar0[1] = ultraListViewItem1.
        arrayvar0[2] = ultraListViewItem2.
        arrayvar0[3] = ultraListViewItem3.
        THIS-OBJECT:ultraListView1:Items:AddRange(arrayvar0).
        THIS-OBJECT:ultraListView1:Location = NEW System.Drawing.Point(16, 0).
        THIS-OBJECT:ultraListView1:Margin = NEW System.Windows.Forms.Padding(0).
        THIS-OBJECT:ultraListView1:Name = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:Size = NEW System.Drawing.Size(168, 332).
        THIS-OBJECT:ultraListView1:TabIndex = 5.
        THIS-OBJECT:ultraListView1:Text = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
        THIS-OBJECT:ultraListView1:ViewSettingsList:MultiColumn = FALSE.
        THIS-OBJECT:ultraListView1:Visible = FALSE.
        THIS-OBJECT:ultraListView1:KeyDown:Subscribe(THIS-OBJECT:ultraListView1_KeyDown).
        THIS-OBJECT:ultraListView1:DoubleClick:Subscribe(THIS-OBJECT:ultraListView1_DoubleClick).
        THIS-OBJECT:ultraListView1:MouseDown:Subscribe(THIS-OBJECT:ultraListView1_MouseDown).
        /*  */
        /* ultraTextEditor1 */
        /*  */
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        appearance1:ImageHAlign = Infragistics.Win.HAlign:Center.
        appearance1:ImageVAlign = Infragistics.Win.VAlign:Middle.
        editorButton1:Appearance = appearance1.
        editorButton1:ButtonStyle = Infragistics.Win.UIElementButtonStyle:Office2007ScrollbarButton.
        editorButton1:Width = 21.
        THIS-OBJECT:ultraTextEditor1:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:ultraTextEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:ultraTextEditor1:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:ultraTextEditor1:Location = NEW System.Drawing.Point(0, 335).
        THIS-OBJECT:ultraTextEditor1:Margin = NEW System.Windows.Forms.Padding(0, 3, 0, 0).
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:Size = NEW System.Drawing.Size(184, 21).
        THIS-OBJECT:ultraTextEditor1:TabIndex = 4.
        THIS-OBJECT:ultraTextEditor1:KeyPress:Subscribe(THIS-OBJECT:ultraTextEditor1_KeyPress).
        THIS-OBJECT:ultraTextEditor1:KeyDown:Subscribe(THIS-OBJECT:ultraTextEditor1_KeyDown).
        THIS-OBJECT:ultraTextEditor1:EditorButtonClick:Subscribe(THIS-OBJECT:ultraTextEditor1_EditorButtonClick).
        /*  */
        /* ultraTree1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
	    THIS-OBJECT:ultraTree1:AfterExpand:Subscribe(THIS-OBJECT:ultraTree1_AfterExpand).
        THIS-OBJECT:ultraTree1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTree1:BorderStyle = Infragistics.Win.UIElementBorderStyle:None.
        THIS-OBJECT:ultraTree1:DisplayStyle = Infragistics.Win.UltraWinTree.UltraTreeDisplayStyle:WindowsVista.
        THIS-OBJECT:ultraTree1:FullRowSelect = TRUE.
        THIS-OBJECT:ultraTree1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraTree1:Margin = NEW System.Windows.Forms.Padding(0).
        THIS-OBJECT:ultraTree1:Name = "ultraTree1":U.
        THIS-OBJECT:ultraTree1:Size = NEW System.Drawing.Size(184, 332).
        THIS-OBJECT:ultraTree1:TabIndex = 3.
        THIS-OBJECT:ultraTree1:MouseClick:Subscribe(THIS-OBJECT:ultraTree1_MouseClick).
        THIS-OBJECT:ultraTree1:KeyDown:Subscribe(THIS-OBJECT:ultraTree1_KeyDown).
        THIS-OBJECT:ultraTree1:MouseDoubleClick:Subscribe(THIS-OBJECT:ultraTree1_MouseDoubleClick).
        /*  */
        /* timer1 */
        /*  */
        THIS-OBJECT:timer1:Interval = 750.
        THIS-OBJECT:timer1:Tick:Subscribe(THIS-OBJECT:timer1_Tick).
        /*  */
        /* MenuTreeViewControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraListView1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTree1).
        THIS-OBJECT:Name = "MenuTreeViewControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(184, 356).
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the TreeNodeSelected event if the given Node is enabled                                                                       
        Notes:                                                                 
        @param poNode The UltraTreeNode to raise the TreeNodeSelected event for       
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ItemSelected (poNode AS UltraTreeNode):
    
        IF poNode:Enabled THEN 
            OnTreeNodeSelected (NEW TreeNodeSelectedEventArgs (poNode:Key)) .    

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the ItemSelected event for the UltraTreeNode that correspondents 
                 to the currently selected ListViewItem                                                                        
        Notes:                                         
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ListViewItemSelected ():
        
        IF ultraListView1:SelectedItems:Count > 0 AND ultraListView1:SelectedItems[0]:Enabled THEN 
            ItemSelected (CAST (ultraListView1:SelectedItems[0]:Tag, UltraTreeNode)) .

    END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Loads the Menu Structure in the Tree  																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID LoadMenu ():
		
        oMenuProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IMenuStructureProvider} .

        IF VALID-OBJECT (oMenuProvider) THEN 
            THIS-OBJECT:FillMenuStructure (THIS-OBJECT:RootNodeKey,
                                           THIS-OBJECT:RenderRootNode) .        

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event 
        Notes:   Populates the Menu Structrur   
        @param e The System.EventArgs with the data for the Load event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
				
		SUPER:OnLoad (e) .

        IF THIS-OBJECT:Sorted THEN 
            ultraTree1:Override:Sort = Infragistics.Win.UltraWinTree.SortType:Ascending .

        THIS-OBJECT:LoadMenu () .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the TreeNodeSelected event                                                                        
        Notes:                                                                        
        @param e The TreeNodeSelectedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnTreeNodeSelected (e AS TreeNodeSelectedEventArgs):
        
        DEFINE VARIABLE oLauncher AS IMenuFunctionLauncher NO-UNDO . 
        DEFINE VARIABLE oNode     AS UltraTreeNode         NO-UNDO .
        
        IF NOT VALID-OBJECT (e) THEN 
            UNDO, THROW NEW AppError ("Unable to raise the TreeNodeSelected event without event argument."{&TRAN}, 0) .

        THIS-OBJECT:TreeNodeSelected:Publish (THIS-OBJECT, e) .

        oLauncher = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IMenuFunctionLauncher} .
        
        oNode = ultraTree1:GetNodeByKey (e:ItemKey) .
        
        IF VALID-OBJECT (oLauncher) THEN 
            oLauncher:LaunchMenuFunction (UNBOX (oNode:Tag), NEW MenuContext (UNBOX (oNode:Tag), oNode:Text, THIS-OBJECT)) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs the search                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformSearch ():
        
        DEFINE VARIABLE cText AS CHARACTER NO-UNDO.
        
        THIS-OBJECT:timer1:Enabled = FALSE .
        
        ASSIGN cText = ultraTextEditor1:Text .
        
        IF cText > "":U THEN DO:
            ultraListView1:Visible = TRUE .   
            ultraTree1:Visible = FALSE .          
            ultraListView1:Items:Clear () .
            
            FindNodes (cText, ultraTree1:Nodes) .
        END .
        ELSE DO:
            ultraListView1:Visible = FALSE .
            ultraTree1:Visible = TRUE .              
            ultraListView1:Items:Clear () .
        END . 
    
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Populates the Nodes in the Tree 
        Notes:   
        @param poNodes The current root node nodes collection
        @param pcParentMenuKey The Root Node Key for the current level
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulateNodes (poNodes AS TreeNodesCollection,
	                                     pcParentMenuKey AS CHARACTER):
		
        DEFINE VARIABLE oNode  AS UltraTreeNode NO-UNDO .
        DEFINE VARIABLE oImage AS Bitmap        NO-UNDO . 

		DEFINE BUFFER ttMenuStructure FOR ttMenuStructure . 
        DEFINE BUFFER b_ttMenuStructure FOR ttMenuStructure . 
		
		FOR EACH ttMenuStructure WHERE ttMenuStructure.ParentMenuKey = pcParentMenuKey
		                            BY ttMenuStructure.MenuSequence:
            
            oNode = poNodes:Add (ttMenuStructure.MenuKey, ttMenuStructure.MenuName) .
            oNode:Tag = BOX (ttMenuStructure.FunctionKey) .
	
	        IF ttMenuStructure.MenuSmallImage > "":U THEN DO:
	            IF VALID-OBJECT (oImageProvider) THEN 
	               oImage = oImageProvider:ImageForKey (ttMenuStructure.MenuSmallImage) . 
	            
	            IF VALID-OBJECT (oImage) THEN 
	               oNode:LeftImages:Add (oImage) .
	        END.
		    
		    IF ttMenuStructure.HasChild THEN DO:
		        IF CAN-FIND (FIRST b_ttMenuStructure WHERE b_ttMenuStructure.ParentMenuKey = ttMenuStructure.MenuKey) THEN 
    		        THIS-OBJECT:PopulateNodes (oNode:Nodes,
    		                                   ttMenuStructure.MenuKey) .
            	ELSE
            	   oNode:Nodes:Add (SUBSTITUTE ("dummy_&1":U, ttMenuStructure.MenuKey)) .
            END.            		                                   
		END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Expands or collapses the TreeNode with the given key                                                                     
        Notes:                                                      
        @param pcNodeKey The key of the node to process
        @return Logical value indicating if the Node could be found             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL SelectNode (pcNodeKey AS CHARACTER):
        
        DEFINE VARIABLE oNode AS UltraTreeNode NO-UNDO . 
        
        oNode = ultraTree1:GetNodeByKey (pcNodeKey) .

        IF NOT VALID-OBJECT (oNode) THEN  
            RETURN FALSE .  
            
        oNode:Selected = TRUE . 
        ultraTree1:ActiveNode = oNode .
            
        RETURN TRUE . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Eventhandler for the Tick Event of the timer Component                                                                        
        Notes:   Calls PerformSearch ()                                                                       
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID timer1_Tick (sender AS System.Object, 
                                     e AS System.EventArgs):
        
        PerformSearch () .

        CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DoubleClick Event of the ultraListView1
        Notes:                                                                          
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_DoubleClick (sender AS System.Object, 
                                                    e AS System.EventArgs):
                                                        
        IF ultraListView1:SelectedItems:Count > 0 THEN 
            ListViewItemSelected () .
            
        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
            
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown Event of the ultraListView1
        Notes:                                                                          
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_KeyDown (sender AS System.Object, 
                                                e AS System.Windows.Forms.KeyEventArgs):
        
        IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, Keys:Return) OR 
           Progress.Util.EnumHelper:AreEqual (e:KeyCode, Keys:Enter) THEN 
          
            ListViewItemSelected () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the MouseDown Event of the ultraListView1
        Notes:                                                                          
        @param sender The reference to the object that raised the event
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_MouseDown (sender AS System.Object, 
                                                  e AS System.Windows.Forms.MouseEventArgs):

        DEFINE VARIABLE oListViewItem AS UltraListViewItem NO-UNDO .
    
        IF (e:Button:Equals(System.Windows.Forms.MouseButtons:Right)) THEN DO:
            oListViewItem = ultraListView1:ItemFromPoint (e:Location) .
            IF oListViewItem:Enabled THEN DO:
                ultraListView1:SelectedItems:Clear ().
                ultraListView1:SelectedItems:Add (oListViewItem) .  
            END.
        END.
        
        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
        
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick Event of the ultraTextEditor1
        Notes:                                                                          
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTextEditor1_EditorButtonClick (sender AS System.Object, 
                                                            e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs ):
        
        ultraTextEditor1:Text = "":U .
        timer1:Enabled = FALSE . 
        PerformSearch() . 

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown Event of the ultraTextEditor1
        Notes:                                                                          
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTextEditor1_KeyDown (sender AS System.Object, 
                                                  e AS System.Windows.Forms.KeyEventArgs):
        
        IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Escape) THEN DO:
            
            ultraTextEditor1:Text = "":U .
            timer1:Enabled = FALSE .
            PerformSearch() .
            e:Handled = TRUE . 
            
            RETURN . 
        END. 
        
        IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Down) AND 
           ultraListView1:Visible AND 
           ultraListView1:Items:Count > 0 
            THEN DO:
            
            timer1:Enabled = FALSE .
            ultraListView1:SelectedItems:Clear() .
            ultraListView1:SelectedItems:Add (ultraListView1:Items[0]) . 
            ultraListView1:Items[0]:Activate() .
            ultraListView1:Focus() .
            e:Handled = TRUE .
            
            RETURN .  
        END.   

        IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Up) AND 
           ultraListView1:Visible AND 
           ultraListView1:Items:Count > 0 
            THEN DO:
            
            timer1:Enabled = FALSE .
            ultraListView1:SelectedItems:Clear() .
            ultraListView1:SelectedItems:Add (ultraListView1:Items[ultraListView1:Items:Count - 1]) . 
            ultraListView1:Items[ultraListView1:Items:Count - 1]:Activate() .
            ultraListView1:Focus() .
            e:Handled = TRUE .
            
            RETURN .  
        END.   
                
        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
                
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyPress Event of the ultraTextEditor1
        Notes:                                                                          
        @param sender The reference to the object that raised the event
        @param e The KeyPressEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTextEditor1_KeyPress (sender AS System.Object, 
                                                   e AS System.Windows.Forms.KeyPressEventArgs):
        
        /* Reset timer */
        timer1:Enabled = FALSE .
        timer1:Enabled = TRUE .        

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH . 
        
    END METHOD.

	/*------------------------------------------------------------------------------
	   Purpose: Event handler for the AfterExpand event handler of the ultraTree1 
	            instance
	   Notes:   Ensures that the expanded node and child nodes are visible
	   @param sender The reference to the object that raised this event
	   @param e The NodeEventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraTree1_AfterExpand (sender AS System.Object,   
	                                            e AS Infragistics.Win.UltraWinTree.NodeEventArgs):
	                                                
        /* Mike Fechner, Consultingwerk Ltd. 23.06.2014
           WINKIT-6: Bring the node child nodes into view when expanding a node */
        
        IF e:TreeNode:Expanded THEN
            e:TreeNode:BringIntoView (YES).

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown Event of the ultraTree1
        Notes:                                                                          
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTree1_KeyDown (sender AS System.Object, 
                                            e AS System.Windows.Forms.KeyEventArgs):
        
        IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, Keys:Return) OR 
           Progress.Util.EnumHelper:AreEqual (e:KeyCode, Keys:Enter) THEN DO: 

            IF VALID-OBJECT (ultraTree1:ActiveNode) AND NOT ultraTree1:ActiveNode:HasNodes AND ultraTree1:ActiveNode:Enabled THEN 
                
                ItemSelected (ultraTree1:ActiveNode) . 
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the MouseClick Event of the ultraTree1
        Notes:                                                                          
        @param sender The reference to the object that raised the event
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTree1_MouseClick (sender AS System.Object, 
                                               e AS System.Windows.Forms.MouseEventArgs):
        
        DEFINE VARIABLE oNode      AS UltraTreeNode              NO-UNDO .
        DEFINE VARIABLE oUIElement AS Infragistics.Win.UIElement NO-UNDO . 

        IF UseDoubleClickForItemSelect = TRUE THEN 
            RETURN . 

        /* Mike Fechner, Consultingwerk Ltd. 12.08.2014
           SCL-391: Disregard click on Expansion indicator 
                     http://www.infragistics.com/community/forums/t/72064.aspx */
        oUIElement = ultraTree1:UIElement:LastElementEntered .
  
        IF NOT VALID-OBJECT (oUIElement) THEN RETURN . 

        oUIElement = oUIElement:GetAncestor (Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinTree.NodeSelectableAreaUIElement":U)) .

        IF NOT VALID-OBJECT (oUIElement) THEN RETURN . 

        oNode = ultraTree1:GetNodeFromPoint (e:Location) .

        /* Mike Fechner, Consultingwerk Ltd. 23.06.2014
           Comment from S. Legian / Bravura:
           oNode simply validates that we double clicked on a tree node rather than somewhere in the Tree Control
           However the actual node that we will action will be the Active Node. These two will mostly be the same,
           the difference occurs when the double click expanded the node and by bringing the children nodes into view
           we moved the clicked node up. In this case the node at the current location is a child node rather than the
           original node we clicked on...  */

        IF VALID-OBJECT (oNode) AND NOT ultraTree1:ActiveNode:HasNodes AND ultraTree1:ActiveNode:Enabled THEN 
            ItemSelected (ultraTree1:ActiveNode) .        

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the MouseDoubleClick Event of the ultraTree1
        Notes:                                                                          
        @param sender The reference to the object that raised the event
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTree1_MouseDoubleClick (sender AS System.Object, 
                                                     e AS System.Windows.Forms.MouseEventArgs):
        
        DEFINE VARIABLE oNode AS UltraTreeNode NO-UNDO .

        IF UseDoubleClickForItemSelect = FALSE THEN 
            RETURN . 

        oNode = ultraTree1:GetNodeFromPoint (e:Location) .

        /* Mike Fechner, Consultingwerk Ltd. 23.06.2014
           Comment from S. Legian / Bravura:
           oNode simply validates that we double clicked on a tree node rather than somewhere in the Tree Control
           However the actual node that we will action will be the Active Node. These two will mostly be the same,
           the difference occurs when the double click expanded the node and by bringing the children nodes into view
           we moved the clicked node up. In this case the node at the current location is a child node rather than the
           original node we clicked on...  */

        IF VALID-OBJECT (oNode) AND NOT ultraTree1:ActiveNode:HasNodes AND ultraTree1:ActiveNode:Enabled THEN 
            ItemSelected (ultraTree1:ActiveNode) .        

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the MenuTreeViewControl class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC MenuTreeViewControl ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR .

END CLASS.
