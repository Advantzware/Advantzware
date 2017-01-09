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
    File        : BusinessEntityTreeViewControl
    Purpose     : Visualizes the list of available business entities in a 
                  tree view
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 14 22:53:13 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*            FROM PROPATH.
USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .
USING Consultingwerk.Util.*                 FROM PROPATH.
USING Infragistics.Win.UltraWinTree.*       FROM ASSEMBLY . 
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Tools.BusinessEntityBrowser.BusinessEntityTreeViewControl 
    INHERITS SmartUserControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTree1 AS Infragistics.Win.UltraWinTree.UltraTree NO-UNDO.

    {Consultingwerk/Util/TempTables/ttClassNames.i}

    DEFINE VARIABLE oImageFolder AS System.Drawing.Image NO-UNDO . 
    DEFINE VARIABLE oImageClass  AS System.Drawing.Image NO-UNDO .
        
    /*------------------------------------------------------------------------------
        Purpose: Raised when the SelectedClassName property changes
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SelectedClassNameChanged DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the currently selected class
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SelectedClassName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET (arg AS CHARACTER):
        THIS-OBJECT:SelectedClassName = arg . 
        
        THIS-OBJECT:OnSelectedClassNameChanged (System.EventArgs:Empty) .
    END SET . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessEntityTreeViewControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessEntityTreeViewControl ():
        
        
        SUPER().
        InitializeComponent().
        
        oImageFolder = Consultingwerk.Util.ImageHelper:Load ("Consultingwerk/SmartComponents/Tools/BusinessEntityBrowser/folder.png":U) .
        oImageClass  = Consultingwerk.Util.ImageHelper:Load ("Consultingwerk/SmartComponents/Tools/BusinessEntityBrowser/document_gear.png":U) .
        
        THIS-OBJECT:ultraTree1:Nodes:Clear () .
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Creates a node structure for a package (part of the classname)                                                                       
        Notes:  
        @param pcPackage The Package (part of the classname) to create the node structure for
        @return The Parent Nodes collection                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED TreeNodesCollection CreatePackageNodes (pcPackage AS CHARACTER):
        
        DEFINE VARIABLE oNodes    AS TreeNodesCollection NO-UNDO .
        DEFINE VARIABLE oNode     AS UltraTreeNode       NO-UNDO .
        DEFINE VARIABLE iLevel    AS INTEGER             NO-UNDO .
        DEFINE VARIABLE cSegments AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cSegment  AS CHARACTER           NO-UNDO .
    
        IF pcPackage = "":U THEN 
            RETURN THIS-OBJECT:ultraTree1:Nodes . 

        oNodes = THIS-OBJECT:ultraTree1:Nodes .

        DO iLevel = 1 TO NUM-ENTRIES (pcPackage, ".":U) ON ERROR UNDO, THROW:
    
            ASSIGN cSegment  = ENTRY (iLevel, pcPackage, ".":U) 
                   cSegments = cSegments + (IF cSegments > "":U THEN ".":U ELSE "":U) +
                               cSegment .
    
            IF oNodes:Exists (cSegments) THEN 
                oNodes = oNodes[cSegments]:Nodes .                        
    
            ELSE DO ON ERROR UNDO, THROW:
                oNode = oNodes:Add (cSegments, cSegment) .
                oNode:LeftImages:Add (oImageFolder) .
            
                oNodes = oNode:Nodes.
            END.
        END .

        RETURN oNodes . 

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
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraTreeNode1 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode1 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraTreeNode2 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode2 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraTreeNode3 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode3 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraTreeNode4 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode4 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraTreeNode5 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode5 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        THIS-OBJECT:ultraTree1 = NEW Infragistics.Win.UltraWinTree.UltraTree().
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTree1 */
        /*  */
        THIS-OBJECT:ultraTree1:BorderStyle = Infragistics.Win.UIElementBorderStyle:None.
        THIS-OBJECT:ultraTree1:DisplayStyle = Infragistics.Win.UltraWinTree.UltraTreeDisplayStyle:WindowsVista.
        THIS-OBJECT:ultraTree1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraTree1:ImageTransparentColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraTree1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraTree1:Name = "ultraTree1".
        THIS-OBJECT:ultraTree1:NodeConnectorColor = System.Drawing.SystemColors:ControlDark.
        ultraTreeNode4:Text = "Sports2000".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTree.UltraTreeNode EXTENT 1 NO-UNDO.
        arrayvar0[1] = ultraTreeNode4.
        ultraTreeNode3:Nodes:AddRange(arrayvar0).
        ultraTreeNode3:Text = "OERA".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinTree.UltraTreeNode EXTENT 1 NO-UNDO.
        arrayvar1[1] = ultraTreeNode3.
        ultraTreeNode2:Nodes:AddRange(arrayvar1).
        ultraTreeNode2:Text = "SmartComponentsDemo".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinTree.UltraTreeNode EXTENT 1 NO-UNDO.
        arrayvar2[1] = ultraTreeNode2.
        ultraTreeNode1:Nodes:AddRange(arrayvar2).
        ultraTreeNode1:Text = "Consultingwerk".
        ultraTreeNode5:Text = "Test".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinTree.UltraTreeNode EXTENT 2 NO-UNDO.
        arrayvar3[1] = ultraTreeNode1.
        arrayvar3[2] = ultraTreeNode5.
        THIS-OBJECT:ultraTree1:Nodes:AddRange(arrayvar3).
        THIS-OBJECT:ultraTree1:Size = NEW System.Drawing.Size(246, 494).
        THIS-OBJECT:ultraTree1:TabIndex = 0.
        THIS-OBJECT:ultraTree1:MouseDoubleClick:Subscribe(THIS-OBJECT:ultraTree1_MouseDoubleClick).
        /*  */
        /* BusinessEntityTreeViewControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(6, 13).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTree1).
        THIS-OBJECT:Name = "BusinessEntityTreeViewControl".
        THIS-OBJECT:Size = NEW System.Drawing.Size(246, 494).
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Loads the Business Entites and populates the TreeView                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID LoadBusinessEntities ():
        
        DEFINE VARIABLE oNodesCollection AS TreeNodesCollection NO-UNDO . 
        
        DEFINE VARIABLE cPackage         AS CHARACTER           NO-UNDO.
        DEFINE VARIABLE cClassName       AS CHARACTER           NO-UNDO.
        DEFINE VARIABLE iDot             AS INTEGER             NO-UNDO.
        
        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN 
            FrameworkSettings:WaitStateManager:SetWaitState ("Loading Business Entity Names":U) .
        
        Consultingwerk.Util.GarbageCollectorHelper:ClearUltraTreeNodes (THIS-OBJECT:ultraTree1) .
        
        THIS-OBJECT:ultraTree1:Nodes:Clear () .
        
        UserInterfaceHelper:ProcessEvents() .
        
        Consultingwerk.Util.ClassHelper:GetClassNamesInClassPath ("Consultingwerk.OERA.IBusinessEntity":U,
                                                                  OUTPUT TABLE ttClassNames BY-REFERENCE) .

        ultraTree1:BeginUpdate() .

        FOR EACH ttClassNames ON ERROR UNDO, THROW:
            ASSIGN iDot = R-INDEX (ttClassNames.ClassName, ".":U) . 
            
            IF iDot > 1 THEN 
                ASSIGN cPackage   = SUBSTRING (ttClassNames.ClassName, 1, iDot - 1) 
                       cClassName = SUBSTRING (ttClassNames.ClassName, iDot + 1).    
            ELSE 
                ASSIGN cPackage   = "":U
                       cClassName = ttClassNames.ClassName . 
            
            oNodesCollection = CreatePackageNodes (cPackage) .
            
            oNodesCollection:Add (ttClassNames.ClassName, cClassName):LeftImages:Add (oImageClass) .
        END.


        {Consultingwerk/foreach.i UltraTreeNode oNode in ultraTree1:Nodes}
            
            IF oNode:HasNodes THEN 
                oNode:Expanded = TRUE . 
        
        END.

        ultraTree1:EndUpdate() .

        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN 
            FrameworkSettings:WaitStateManager:ClearWaitState ("Loading Business Entity Names":U) .

        CATCH err AS Progress.Lang.Error :
            IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN 
                FrameworkSettings:WaitStateManager:ClearWaitState ("Loading Business Entity Names":U) .

            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD.


    /*------------------------------------------------------------------------------
        Purpose: Raises the SelectedClassNameChanged event                                                                       
        Notes:                   
        @param e The System.EventArgs with the data for this event                                                     
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnSelectedClassNameChanged (e AS System.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = System.EventArgs:Empty . 

        THIS-OBJECT:SelectedClassNameChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the MouseDoubleClick event of the ultraTree1 class                                                                        
        Notes:                                                                       
        @param sender The reference to the object that raised the event
        @param e The MouseEventArgs with the data for this event   
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTree1_MouseDoubleClick (sender AS System.Object, 
                                                     e AS System.Windows.Forms.MouseEventArgs):
        
        DEFINE VARIABLE oNode AS UltraTreeNode NO-UNDO .

        oNode = ultraTree1:GetNodeFromPoint (e:Location) .
        
        IF NOT VALID-OBJECT (oNode) THEN 
            RETURN . 
            
        IF oNode:HasNodes THEN 
            RETURN . 
            
        THIS-OBJECT:SelectedClassName = oNode:Key .            

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the BusinessEntityTreeViewControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC BusinessEntityTreeViewControl ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.