/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartObjectTreeModel
    Purpose     : Model for the Tree of the Smart Object Maintenance Form
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Oct 16 00:11:15 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                    FROM PROPATH .
USING Consultingwerk.Assertion.*                          FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*               FROM PROPATH .
USING Consultingwerk.Windows.Framework.Repository.Class.* FROM PROPATH .  
USING Infragistics.Win.UltraWinTree.*                     FROM ASSEMBLY .
USING Progress.Lang.*                                     FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH.
USING Consultingwerk.Util.* FROM PROPATH.
USING Consultingwerk.Framework.* FROM PROPATH.
USING Consultingwerk.SmartFramework.* FROM PROPATH.
USING Infragistics.Win.UltraWinToolbars.* FROM ASSEMBLY.

CLASS Consultingwerk.Windows.Framework.Repository.Class.SmartObjectTreeModel: 

    {Consultingwerk/SmartFramework/Repository/Class/dsObjectType.i &REFERENCE-ONLY=REFERENCE-ONLY}

    DEFINE VARIABLE oTree AS UltraTree NO-UNDO .

    DEFINE VARIABLE oObjectTypeAdapter   AS SmartDatasetAdapter             NO-UNDO . 
    DEFINE VARIABLE oObjectTypeViewer    AS SmartViewerControl              NO-UNDO .
    DEFINE VARIABLE oAttributesControl   AS ObjectTypeAttributesUserControl NO-UNDO . 

    DEFINE VARIABLE oAddingNode          AS UltraTreeNode                   NO-UNDO . 
                                                                            
    DEFINE VARIABLE oObjectTypeImage     AS System.Drawing.Bitmap           NO-UNDO . 
    DEFINE VARIABLE oContainerTypeImage  AS System.Drawing.Bitmap           NO-UNDO . 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets that the next record added should be a child to the 
                 current node, not a sibling 
        Notes:   True: Child, False: Sibling
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY AddChild AS LOGICAL NO-UNDO INIT FALSE 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartObjectTreeModel class
        Notes:
        @param phDatasetHandle The dataset of the Object Type Business Entity
        @param poTree The reference to the UltraTree Control
        @param poObjectTypeAdapter The reference to the SmartDatasetAdaptre
        @param poObjectTypeViewer The reference to the SmartViewerControl
        @param poAttributesControl The reference to the user control with the Attribute Values
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartObjectTreeModel (phDatasetHandle AS HANDLE,
                                             poTree AS UltraTree,
                                             poObjectTypeAdapter AS SmartDatasetAdapter,
                                             poObjectTypeViewer  AS SmartViewerControl,
                                             poAttributesControl AS ObjectTypeAttributesUserControl):
                                          
        SUPER ().
                        
        HandleAssert:WidgetType (phDatasetHandle, WidgetTypeEnum:Dataset) .
        ObjectAssert:IsValid (poTree, "poTree":U) .
        
        THIS-OBJECT:BindDataset (DATASET-HANDLE phDatasetHandle BIND) .
        
        ASSIGN oTree              = poTree 
               oObjectTypeAdapter = poObjectTypeAdapter   
               oObjectTypeViewer  = poObjectTypeViewer 
               oAttributesControl = poAttributesControl .
        
        THIS-OBJECT:SubscribeDataAdapterEvents (poObjectTypeAdapter) .
        
        oTree:AfterActivate:Subscribe (AfterActivateHandler) .
    
        ASSIGN oObjectTypeImage    = ImageHelper:Load ("Consultingwerk\Windows\Framework\Images\SmartFramework\objects_16.png":U) 
               oContainerTypeImage = ImageHelper:Load ("Consultingwerk\Windows\Framework\Images\SmartFramework\window_16.png":U)
               .
                            
        poObjectTypeViewer:SmartTableIOStateChanged:Subscribe (SmartTableIOStateChangedHandler) .                    
                            
    END CONSTRUCTOR.
 
    /*------------------------------------------------------------------------------
        Purpose: Event handle for the BeforeCreateRecordHandler event of the 
                 SmartDatasetAdapter
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The BeforeCreateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID BeforeCreateRecordHandler (sender AS System.Object,
                                                     e AS BeforeCreateRecordEventArgs):
		
		DEFINE VARIABLE oActiveNode AS UltraTreeNode NO-UNDO .
		
        oActiveNode = oTree:ActiveNode .

        IF THIS-OBJECT:AddChild = TRUE THEN DO:
            /* Add child */
            IF VALID-OBJECT (oActiveNode) THEN        
                ASSIGN e:BufferHandle::ExtendsObjectTypeGuid = oActiveNode:Key .
            ELSE DO: 
                /* Assuming child of root */
                ASSIGN e:BufferHandle::ExtendsObjectTypeGuid = "":U
                       THIS-OBJECT:AddChild                  = FALSE  .
            END.
        END.
        ELSE DO:
            /* Add sibling */
            IF VALID-OBJECT (oActiveNode) AND VALID-OBJECT (oActiveNode:Parent) THEN 		
    		    ASSIGN e:BufferHandle::ExtendsObjectTypeGuid = oActiveNode:Parent:Key .
        END.

        oAddingNode = NEW UltraTreeNode ("new":U) .
        oAddingNode:Tag = "eSmartObjectType":U .
        oAddingNode:Text = "<new>":U . 

        oAddingNode:LeftImages:Add (THIS-OBJECT:NodeImageForObjectType (e:BufferHandle)) .

        IF THIS-OBJECT:AddChild AND VALID-OBJECT (oActiveNode) THEN 
            oActiveNode:Nodes:Add (oAddingNode) .  
        ELSE IF VALID-OBJECT (oActiveNode) AND VALID-OBJECT (oActiveNode:Parent) THEN        
            oActiveNode:Parent:Nodes:Add (oAddingNode) .
        ELSE 
            oTree:Nodes:Add (oAddingNode) .

        oTree:ActiveNode = oAddingNode .
        oAddingNode:Selected = TRUE . 

        FINALLY:
            ASSIGN THIS-OBJECT:AddChild = FALSE . 
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Bind the dataset 
        Notes:   
        @param dsObjectType Dataset to bind to
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID BindDataset (DATASET dsObjectType BIND):
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterActivate event of the tree view control
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The NodeEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AfterActivateHandler (sender AS System.Object,
                                                e AS NodeEventArgs):
        
        DEFINE VARIABLE oNode AS UltraTreeNode NO-UNDO . 
        
        oNode = e:TreeNode .
    
        IF NOT VALID-OBJECT (oNode) THEN 
            RETURN . 

        oObjectTypeAdapter:FindRowWhere (SUBSTITUTE ("where ObjectTypeGuid = &1":U, QUOTER (oNode:Key))) .
        oAttributesControl:ShowObjectTypeAttributes (oNode:Key) .
    
        CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterCancelCreateRecord of the SmartDatasetAdapter 
        Notes:   
        @param sender The reference to the object that raised the evnet
        @param e The AfterCancelCreateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AfterCancelCreateRecordHandler (sender AS System.Object,
                                                          e AS AfterCancelCreateRecordEventArgs):
        
        IF VALID-OBJECT (oAddingNode) THEN DO:
            oAddingNode:ParentNodesCollection:Remove (oAddingNode) .
            
            GarbageCollectorHelper:DeleteObject (oAddingNode) .
            
            oAddingNode = ? .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterDeleteRecord event of the SmartDatasetAdapter
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The AfterDeleteRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AfterDeleteRecordHandler (sender AS System.Object,
                                                    e AS AfterDeleteRecordEventArgs):
        
        DEFINE VARIABLE oCurrentNode AS UltraTreeNode NO-UNDO .
        
        oCurrentNode = oTree:ActiveNode .
        
        IF NOT VALID-OBJECT (oCurrentNode) THEN 
            RETURN . 
        
        IF e:TableName = UNBOX (oCurrentNode:Tag) THEN DO:
            
            oCurrentNode:ParentNodesCollection:Remove (oCurrentNode) .
            
            GarbageCollectorHelper:DeleteObject (oCurrentNode) .
        END.

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handle for the AfterUpdateRecord event of the SmartDatasetAdapter  
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The AfterUpdateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AfterUpdateRecordHandler (sender AS System.Object,
                                                 e AS AfterUpdateRecordEventArgs) .

        DEFINE VARIABLE oActiveNode AS UltraTreeNode NO-UNDO . 

        oActiveNode = oTree:ActiveNode .

        CASE e:TableName:
            
            WHEN "eSmartObjectType":U THEN DO:
                ASSIGN oActiveNode:Text = THIS-OBJECT:NodeLabelForObjectType (e:BufferHandle) 
                       oActiveNode:Key  = e:BufferHandle::ObjectTypeGuid .
                
                oActiveNode:LeftImages:Clear() .
                oActiveNode:LeftImages:Add (NodeImageForObjectType (e:BufferHandle)) .
                
                IF oActiveNode:Key = "new":U THEN 
                    oActiveNode:Key = e:BufferHandle::ObjectTypeGuid .               
            END.

        END CASE . 

        ASSIGN oAddingNode = ? .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Disposes the object 
        Notes:   Unsubscribes from any event previously subscribed
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Dispose ():
		
        oObjectTypeAdapter:SmartNavigationTargetStateChanged:Unsubscribe (NavigationTargetStateChangedHandler) .
        oObjectTypeAdapter:AfterUpdateRecord:Unsubscribe (AfterUpdateRecordHandler) .
        oObjectTypeAdapter:AfterCancelCreateRecord:Unsubscribe (AfterCancelCreateRecordHandler) .
        oObjectTypeAdapter:AfterDeleteRecord:Unsubscribe (AfterDeleteRecordHandler) .
        oObjectTypeAdapter:BeforeCreateRecord:Unsubscribe (BeforeCreateRecordHandler) .
        
        oTree:AfterActivate:Unsubscribe (AfterActivateHandler) .
    
        oObjectTypeViewer:SmartTableIOStateChanged:Unsubscribe (SmartTableIOStateChangedHandler) .                    

        ASSIGN oObjectTypeAdapter = ? 
               oObjectTypeViewer  = ?           
               oAttributesControl = ? .
               
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event fired whenever the SmartDataSourceState property has changed
        Notes:   
        @param sender The reference to the object that raised the event 
        @param e The SmartNavigationTargetStateChangedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID NavigationTargetStateChangedHandler (sender AS System.Object, 
                                                               e AS SmartNavigationTargetStateChangedEventArgs):
        
        IF e:SmartNavigationTargetState = "Updating":U THEN 
            oTree:Enabled = FALSE . 
        ELSE 
            oTree:Enabled = TRUE .        

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds the Node Label based on an eSmartObjectType buffer handle
        Notes:   
        @param phBuffer The handle of the buffer to build the node label from
        @return The node label
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER NodeLabelForObjectType (phBuffer AS HANDLE):
		
		IF phBuffer::CustomObjectTypeName > "":U THEN 
		    RETURN SUBSTITUTE ("&1 (&2)":U, phBuffer::ObjectTypeName, phBuffer::CustomObjectTypeName) .
		ELSE 
		    RETURN phBuffer::ObjectTypeName .		

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds the Node Label based on an eSmartObjectType buffer handle
        Notes:   
        @param phBuffer The handle of the buffer to build the node label from
        @return The node label
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED System.Drawing.Bitmap NodeImageForObjectType (phBuffer AS HANDLE):
        
        IF phBuffer::ContainerType = TRUE THEN 
            RETURN oContainerTypeImage .
        ELSE
            RETURN oObjectTypeImage .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the Tree View after data has been loaded
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PopulateTreeView ():

        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN 
            FrameworkSettings:WaitStateManager:SetWaitState ("Populating Tree View":U) .

        oTree:BeginUpdate() .

        GarbageCollectorHelper:ClearUltraTreeNodes (oTree) .

        THIS-OBJECT:PopulateTreeView (oTree:Nodes, "":U) .

        UserInterfaceHelper:ProcessEvents (). 

        IF oTree:Nodes:Count > 0 THEN DO:
            oTree:ActiveNode = oTree:Nodes[0] . 
        
            oObjectTypeAdapter:FindRowWhere (SUBSTITUTE ("where ObjectTypeGuid= &1":U, QUOTER (oTree:ActiveNode:Key))) .
        END.

        oTree:EndUpdate (TRUE) .

        FINALLY:
            IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN 
                FrameworkSettings:WaitStateManager:ClearWaitState ("Populating Tree View":U) .
        END FINALLY.

    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Populates the Tree View after data has been loaded
        Notes:   
        @param poNodes The TreeNodesCollection to populate
        @param pcParentObjectTypeGuid The object type guid of the parent object type
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PopulateTreeView (poNodes AS TreeNodesCollection,
                                            pcParentObjectTypeGuid AS CHARACTER):

        DEFINE BUFFER b_eSmartObjectType FOR eSmartObjectType .

        DEFINE VARIABLE oNode AS UltraTreeNode NO-UNDO .

        FOR EACH b_eSmartObjectType WHERE b_eSmartObjectType.ExtendsObjectTypeGuid = pcParentObjectTypeGuid 
            BY b_eSmartObjectType.ObjectTypeName
            ON ERROR UNDO, THROW:
            
            oNode = NEW UltraTreeNode (b_eSmartObjectType.ObjectTypeGuid) .
            oNode:Tag = "eSmartObjectType":U .
            oNode:Text = THIS-OBJECT:NodeLabelForObjectType (BUFFER b_eSmartObjectType:HANDLE) . 
            oNode:LeftImages:Add (NodeImageForObjectType (BUFFER b_eSmartObjectType:HANDLE)) .
            
            poNodes:Add (oNode) .

            THIS-OBJECT:PopulateTreeView (oNode:Nodes, b_eSmartObjectType.ObjectTypeGuid) .
        END .

    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SmartTableIOStateChanged event of the viewer
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The SmartTableIOStateChangedEventArgs with the data for this call
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID SmartTableIOStateChangedHandler (sender AS System.Object, 
	                                                       e AS SmartTableIOStateChangedEventArgs):
        
		DEFINE VARIABLE oToolbar AS UltraToolbarsManager NO-UNDO . 

        oToolbar = CAST (CAST (oObjectTypeViewer:SmartTableIOSource, Progress.Lang.Object), 
                         UltraToolbarsManager) .

        IF oToolbar:Tools["TableIOAdd":U]:SharedProps:Enabled THEN        
            ASSIGN oToolbar:Tools["Add record":U]:SharedProps:Enabled = TRUE 
                   oToolbar:Tools["Add child":U]:SharedProps:Enabled  = TRUE . 
        ELSE 
            ASSIGN oToolbar:Tools["Add record":U]:SharedProps:Enabled = FALSE 
                   oToolbar:Tools["Add child":U]:SharedProps:Enabled  = FALSE . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Subscribes Event handles of the SmartDatasetAdapter
        Notes:   
        @param poAdapter The reference to the SmartDatasetAdapter to subscribe events from 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SubscribeDataAdapterEvents (poAdapter AS SmartDatasetAdapter):
        
        poAdapter:SmartNavigationTargetStateChanged:Subscribe (NavigationTargetStateChangedHandler) .
        poAdapter:AfterUpdateRecord:Subscribe (AfterUpdateRecordHandler) .
        poAdapter:AfterCancelCreateRecord:Subscribe (AfterCancelCreateRecordHandler) .
        poAdapter:AfterDeleteRecord:Subscribe (AfterDeleteRecordHandler) .
        poAdapter:BeforeCreateRecord:Subscribe (BeforeCreateRecordHandler) .
            
    END METHOD .

END CLASS.
