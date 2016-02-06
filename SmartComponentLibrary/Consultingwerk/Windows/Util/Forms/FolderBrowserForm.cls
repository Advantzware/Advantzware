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
    File        : FolderBrowserForm
    Purpose     : Allows selection of a folder under a given (custom)
                  root folder
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu May 15 14:08:19 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Forms.*              FROM PROPATH .
USING Consultingwerk.Util.*               FROM PROPATH .
USING Consultingwerk.Windows.Util.*       FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH .  
USING Infragistics.Win.UltraWinTree.*     FROM ASSEMBLY . 
USING Progress.Lang.*                     FROM PROPATH .
USING Progress.Util.*                     FROM ASSEMBLY .
USING System.IO.*                         FROM ASSEMBLY . 
USING System.Windows.Forms.*              FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Util.Forms.FolderBrowserForm INHERITS BaseForm: 

    DEFINE PRIVATE VARIABLE button2 AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE button1 AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE button3 AS System.Windows.Forms.Button NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE timer1 AS System.Windows.Forms.Timer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTree1 AS Infragistics.Win.UltraWinTree.UltraTree NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the RootFolder this Dialog should be navigation on 
                 subfolders of 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY RootFolder AS CHARACTER NO-UNDO INIT ".":U 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the selected folder 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY SelectedFolder AS CHARACTER NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FolderBrowserForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC FolderBrowserForm ():
		
        SUPER().
        InitializeComponent().
        
        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF        
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the button3 
        Notes:   
        @param sender The reference to the object that raised this event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID button3_Click (sender AS System.Object, 
	                                   e AS System.EventArgs):
		
		DEFINE VARIABLE cFolderName AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE oDirectory  AS DirectoryInfo NO-UNDO . 
        DEFINE VARIABLE oDirectory2 AS DirectoryInfo NO-UNDO . 
        DEFINE VARIABLE oNode       AS UltraTreeNode NO-UNDO .
        DEFINE VARIABLE oParent     AS UltraTreeNode NO-UNDO .
        DEFINE VARIABLE oTempNode   AS UltraTreeNode NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER       NO-UNDO .
        DEFINE VARIABLE cEntry      AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE cFolder     AS CHARACTER     NO-UNDO .

        IF NOT InputPromptHelper:PromptForCharacterValue ({Consultingwerk/translate.i &string="'Create folder':U"},
                                                          {Consultingwerk/translate.i &string="'Please enter the name for the new folder to create':U"},
                                                          NEW Consultingwerk.Framework.NotEmptyInputValidator (),
                                                          INPUT-OUTPUT cFolderName) THEN RETURN . 

        IF NOT ultraTree1:SelectedNodes[0]:Expanded = TRUE THEN DO: 
            ultraTree1:SelectedNodes[0]:Expanded = TRUE .  
        END.
        
        oDirectory = NEW DirectoryInfo (ultraTree1:SelectedNodes[0]:Key) .

        FILE-INFO:FILE-NAME = oDirectory:FullName + "\":U + cFolderName .
        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN  
            UNDO, THROW NEW AppError ({Consultingwerk/translate.i &string="'A file or folder with that name does already exists!':U"}, 0) .            

        ASSIGN cFolderName = TRIM (REPLACE (cFolderName,  "/":U, "~\":U), "~\":U) .

        oDirectory2 = oDirectory:CreateSubdirectory (cFolderName) .

        ASSIGN oParent = ultraTree1:SelectedNodes[0] . 

        DO i = 1 TO NUM-ENTRIES (cFolderName, "~\":U):
            ASSIGN cEntry  = ENTRY (i, cFolderName, "~\":U) 
                   cFolder = oParent:Key + "~\":U + cEntry .

            IF NOT oParent:Expanded THEN 
                oParent:Expanded = TRUE . 
                
            IF NOT oParent:Nodes:Exists (cFolder) THEN DO:
                oNode = oParent:Nodes:Add (cFolder, cEntry) .
                oNode:LeftImages:Add (ImageHelper:Load ("Consultingwerk/Windows/Images/folder_16.png":U)) .
                
                IF i < NUM-ENTRIES (cFolderName, "~\":U) THEN DO: 
                    oTempNode = oNode:Nodes:Add (GUID) .
                    oTempNode:Tag = "temp":U .
                END.                
            END.
            ELSE 
                oNode = oParent:Nodes [cFolder] .

            oParent = oNode . 
        END.

        ultraTree1:SelectedNodes:Clear () .
        UserInterfaceHelper:ProcessEvents() . 
        ultraTree1:ActiveNode = oNode . 
        oNode:Selected = TRUE . 

        CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

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
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Util.Forms.FolderBrowserForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE m_override1 AS Infragistics.Win.UltraWinTree.Override NO-UNDO.
        m_override1 = NEW Infragistics.Win.UltraWinTree.Override().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:button1 = NEW System.Windows.Forms.Button().
        THIS-OBJECT:button2 = NEW System.Windows.Forms.Button().
        THIS-OBJECT:ultraTree1 = NEW Infragistics.Win.UltraWinTree.UltraTree().
        THIS-OBJECT:button3 = NEW System.Windows.Forms.Button().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:timer1 = NEW System.Windows.Forms.Timer(THIS-OBJECT:components).
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* button1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:button1, "button1":U).
        THIS-OBJECT:button1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:button1:Name = "button1":U.
        THIS-OBJECT:button1:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:button1:UseVisualStyleBackColor = TRUE.
        /*  */
        /* button2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:button2, "button2":U).
        THIS-OBJECT:button2:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:button2:Name = "button2":U.
        THIS-OBJECT:button2:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:button2:UseVisualStyleBackColor = TRUE.
        /*  */
        /* ultraTree1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTree1, "ultraTree1":U).
        THIS-OBJECT:ultraTree1:DisplayStyle = Infragistics.Win.UltraWinTree.UltraTreeDisplayStyle:WindowsVista.
        THIS-OBJECT:ultraTree1:HideSelection = FALSE.
        THIS-OBJECT:ultraTree1:Name = "ultraTree1":U.
        m_override1:SelectionType = Infragistics.Win.UltraWinTree.SelectType:Single.
        m_override1:Sort = Infragistics.Win.UltraWinTree.SortType:Ascending.
        THIS-OBJECT:ultraTree1:Override = m_override1.
        THIS-OBJECT:ultraTree1:ShowRootLines = FALSE.
        THIS-OBJECT:ultraTree1:AfterSelect:Subscribe(THIS-OBJECT:ultraTree1_AfterSelect).
        THIS-OBJECT:ultraTree1:BeforeExpand:Subscribe(THIS-OBJECT:ultraTree1_BeforeExpand).
        THIS-OBJECT:ultraTree1:MouseDoubleClick:Subscribe(THIS-OBJECT:ultraTree1_MouseDoubleClick).
        /*  */
        /* button3 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:button3, "button3":U).
        THIS-OBJECT:button3:Name = "button3":U.
        THIS-OBJECT:button3:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:button3:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:button3:Click:Subscribe(THIS-OBJECT:button3_Click).
        /*  */
        /* ultraTextEditor1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTextEditor1, "ultraTextEditor1":U).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        THIS-OBJECT:ultraTextEditor1:Appearance = appearance1.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        editorButton1:Appearance = appearance2.
        THIS-OBJECT:ultraTextEditor1:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:ultraTextEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:TextChanged:Subscribe(THIS-OBJECT:ultraTextEditor1_TextChanged).
        THIS-OBJECT:ultraTextEditor1:KeyDown:Subscribe(THIS-OBJECT:ultraTextEditor1_KeyDown).
        /*  */
        /* timer1 */
        /*  */
        THIS-OBJECT:timer1:Interval = 750.
        THIS-OBJECT:timer1:Tick:Subscribe(THIS-OBJECT:timer1_Tick).
        /*  */
        /* FolderBrowserForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:button1.
        THIS-OBJECT:CancelButton = THIS-OBJECT:button2.
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:button3).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTree1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:button2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:button1).
        THIS-OBJECT:Name = "FolderBrowserForm":U.
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Inserts the Root Node 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID InsertRootNode ():
		
        DEFINE VARIABLE oRootNode AS UltraTreeNode  NO-UNDO . 
        DEFINE VARIABLE oTempNode AS UltraTreeNode  NO-UNDO . 

        DEFINE VARIABLE oDirectory AS DirectoryInfo NO-UNDO . 
        DEFINE VARIABLE cDirectory AS CHARACTER     NO-UNDO .

        FILE-INFO:FILE-NAME = THIS-OBJECT:RootFolder . 
        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN DO:

            ASSIGN cDirectory = TRIM (FILE-INFO:FULL-PATHNAME, "~\":U) .

            oRootNode = ultraTree1:Nodes:Add (FILE-INFO:FULL-PATHNAME) .
            oRootNode:LeftImages:Add (ImageHelper:Load ("Consultingwerk/Windows/Images/workplace_16.png":U)) .
            
            oDirectory = NEW DirectoryInfo (cDirectory) .
            
            IF oDirectory:GetDirectories():Length > 0 THEN DO: 
                oTempNode = oRootNode:Nodes:Add (GUID) .
                oTempNode:Tag = "temp":U .
            END.                
        END.
        ELSE 
            MessageFormHelper:ShowMessage (SUBSTITUTE ("Root Folder &1 not found."{&TRAN}, QUOTER (THIS-OBJECT:RootFolder)), 
                                           MessageFormImages:ImageWarning) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event 
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):
		
		SUPER:OnShown (e) .
		
	    UserInterfaceHelper:ProcessEvents() . 
	    
	    THIS-OBJECT:InsertRootNode () . 	 

        UserInterfaceHelper:ProcessEvents() . 

        {Consultingwerk/foreach.i UltraTreeNode oNode in ultraTree1:Nodes}
            oNode:Expanded = TRUE . 
        END.
        
        IF ultraTree1:Nodes:Count > 0 THEN  
            ultraTree1:Nodes[0]:Selected = TRUE . 

        THIS-OBJECT:ultraTextEditor1:Focus () . 

        CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

	END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Performs the search based on the search text
	    Notes:
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PerformSearch ():
		
		DEFINE VARIABLE cSearch AS CHARACTER                                        NO-UNDO .
        DEFINE VARIABLE oList   AS "System.Collections.Generic.List<UltraTreeNode>" NO-UNDO . 
        DEFINE VARIABLE oParent AS UltraTreeNode                                    NO-UNDO .        	

		ASSIGN cSearch = ultraTextEditor1:Text .
		
		IF cSearch > "":U THEN DO:
		    ASSIGN cSearch = SUBSTITUTE ("*&1*":U, cSearch) .
		    
		    ultraTree1:ExpandAll (ExpandAllType:Always) .
		    THIS-OBJECT:ViewHideAllNodes (FALSE) .

            oList = UltraTreeHelper:GetAllNodes (ultraTree1) .
    
            {Consultingwerk/foreach.i UltraTreeNode oNode in oList}
    
                IF oNode:Text MATCHES cSearch THEN DO: 
                    oNode:Visible = TRUE .
                    
                    oParent = oNode:Parent .
                    
                    DO WHILE VALID-OBJECT (oParent):
                        oParent:Visible = TRUE . 
                        oParent = oParent:Parent .
                    END. 
                    
                END.
            END.    

            UserInterfaceHelper:ProcessEvents() .

            ultraTree1:SelectedNodes:Clear () . 

            IF ultraTree1:Nodes:Count > 0 THEN DO:  
                ultraTree1:Nodes[0]:Selected = TRUE .
                ultraTree1:ActiveNode = ultraTree1:Nodes[0] . 
            END. 
		END.
        ELSE DO:		
		    ultraTree1:CollapseAll() .
		    THIS-OBJECT:ViewHideAllNodes (TRUE) .

            {Consultingwerk/foreach.i UltraTreeNode oNode in ultraTree1:Nodes nodefine}
                oNode:Expanded = TRUE . 
            END.
            
            IF ultraTree1:Nodes:Count > 0 THEN  
                ultraTree1:Nodes[0]:Selected = TRUE . 
        END.

	END METHOD.

	/*------------------------------------------------------------------------------
        Purpose: Event handler for the Tick event of the timer1 
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID timer1_Tick (sender AS System.Object, e AS System.EventArgs):
		
		timer1:Enabled = FALSE . 
		
		THIS-OBJECT:PerformSearch () .

	END METHOD.

	/*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown event of the ultraTextEditor1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraTextEditor1_KeyDown (sender AS System.Object, 
	                                              e AS System.Windows.Forms.KeyEventArgs):
		
		IF EnumHelper:AreEqual (e:KeyCode, Keys:Down) THEN 
		    ultraTree1:Focus () .

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the TextChanged of the ultraTextEditor1
	    Notes:
	    @param sender The reference to the object that raised the event
	    @param e The System.EventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraTextEditor1_TextChanged (sender AS System.Object, 
	                                                  e AS System.EventArgs):
		
		timer1:Enabled = FALSE . 
        timer1:Enabled = TRUE . 

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterSelect event of the ultraTree1 
        Notes:   
        @param sender The reference to the object that raised this event
        @param e The SelectEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraTree1_AfterSelect (sender AS System.Object, 
	                                            e AS Infragistics.Win.UltraWinTree.SelectEventArgs):
		
		IF e:NewSelections:Count = 1 THEN 
		    ASSIGN button1:Enabled            = TRUE 
		           button3:Enabled            = TRUE
		           THIS-OBJECT:SelectedFolder = e:NewSelections[0]:Key . 
        		           
		ELSE
		    ASSIGN button1:Enabled            = FALSE 
		           button3:Enabled            = FALSE 
		           THIS-OBJECT:SelectedFolder = "":U . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeforeExport event of the ultraTree1 
        Notes:   Creates sub-nodes if required
        @param sender The reference to the object that raised this event
        @param e The CancelableNodeEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraTree1_BeforeExpand (sender AS System.Object, 
	                                             e AS Infragistics.Win.UltraWinTree.CancelableNodeEventArgs):
		
        DEFINE VARIABLE oCurrentDirectory AS DirectoryInfo NO-UNDO .
		
        DEFINE VARIABLE oNode             AS UltraTreeNode NO-UNDO . 
        DEFINE VARIABLE oTempNode         AS UltraTreeNode NO-UNDO . 
		
		IF e:TreeNode:Nodes:Count <> 1 OR 
		   UNBOX (e:TreeNode:Nodes[0]:Tag) <> "temp":U THEN 
		      RETURN . 
		      
        GarbageCollectorHelper:ClearUltraTreeNodes (e:TreeNode:Nodes) .

        oCurrentDirectory = NEW DirectoryInfo (e:TreeNode:Key) NO-ERROR . 
        
        IF NOT VALID-OBJECT (oCurrentDirectory) THEN 
            RETURN . 
                		      
        {Consultingwerk/foreach.i DirectoryInfo oDirectory in oCurrentDirectory:GetDirectories()}        		      

            oNode = e:TreeNode:Nodes:Add (oDirectory:FullName, oDirectory:Name) .
            oNode:LeftImages:Add (ImageHelper:Load ("Consultingwerk/Windows/Images/folder_16.png":U)) .
            
            IF oDirectory:GetDirectories():Length > 0 THEN DO: 
                oTempNode = oNode:Nodes:Add (GUID) .
                oTempNode:Tag = "temp":U .
            END.                
        END.        		      

	END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the MouseDoubleClick event of the ultraTree1
	    Notes:
	    @param sender The reference to the object that raised the event
	    @param e The MouseEventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraTree1_MouseDoubleClick (sender AS System.Object,
	                                                 e AS MouseEventArgs):
		
		DEFINE VARIABLE oNode AS UltraTreeNode NO-UNDO . 
		
		oNode = ultraTree1:GetNodeFromPoint (e:Location) .

        IF VALID-OBJECT (oNode) AND NOT oNode:HasNodes THEN DO:
            THIS-OBJECT:DialogResult = DialogResult:OK . 
            THIS-OBJECT:Close ().
        END.

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Views or hides all tree nodes
	    Notes:
	    @param plView Logical value indicating if the nodes should be shown
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ViewHideAllNodes (plView AS LOGICAL):
		
        DEFINE VARIABLE oList AS "System.Collections.Generic.List<UltraTreeNode>" NO-UNDO . 
		
		oList = UltraTreeHelper:GetAllNodes (ultraTree1) .
	
	    {Consultingwerk/foreach.i UltraTreeNode oNode in oList}
	
	       ASSIGN oNode:Visible = plView . 
	       
	    END.

	END METHOD.

END CLASS.
