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
    File        : UltraListViewLeftRightController
    Purpose     : Controls two UltraListViews for move left/right
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 26 21:19:34 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*          FROM PROPATH .
USING Consultingwerk.Windows.Controls.*   FROM PROPATH .  
USING Infragistics.Win.Misc.*             FROM ASSEMBLY .
USING Infragistics.Win.UltraWinListView.* FROM ASSEMBLY.
USING Progress.Lang.*                     FROM PROPATH .

CLASS Consultingwerk.Windows.Controls.UltraListViewLeftRightController
    INHERITS System.EventArgs: 
 
    DEFINE VARIABLE oLeftListView AS UltraListView  NO-UNDO . 
    DEFINE VARIABLE oRightListView AS UltraListView NO-UNDO . 
    DEFINE VARIABLE oAddButton AS UltraButton       NO-UNDO . 
    DEFINE VARIABLE oRemoveButton AS UltraButton    NO-UNDO .  
 
    /*------------------------------------------------------------------------------
        Purpose: Raised when an Item is added to or removed from the left list view 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ItemsChanged DELEGATE System.EventHandler .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UltraListViewLeftRightController class
        Notes:   
        @param poLeftListView The left UltraListView
        @param poRightListView The right UltraListView
        @param poAddButton The button to add items to the right
        @param poRemoveButton The button to remove items from the right
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UltraListViewLeftRightController (poLeftListView AS UltraListView,
	                                                     poRightListView AS UltraListView,
	                                                     poAddButton AS UltraButton,
	                                                     poRemoveButton AS UltraButton):
		SUPER ().

        ObjectAssert:IsValid (poLeftListView, "poLeftListView":U) .
        ObjectAssert:IsValid (poRightListView, "poRightListView":U) .
        ObjectAssert:IsValid (poAddButton, "poAddButton":U) .
        ObjectAssert:IsValid (poRemoveButton, "poRemoveButton":U) .

        ASSIGN oLeftListView  = poLeftListView
               oRightListView = poRightListView
               oAddButton     = poAddButton
               oRemoveButton  = poRemoveButton .
		
		oLeftListView:ItemSelectionChanged:Subscribe (LeftItemSelectionChangedHandler) .
        oRightListView:ItemSelectionChanged:Subscribe (RightItemSelectionChangedHandler) .
		
		oAddButton:Click:Subscribe (AddButtonClickHandler) .
		oRemoveButton:Click:Subscribe (RemoveButtonClickHandler) .
		
		THIS-OBJECT:EvaluateButtons () . 
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the Add button 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with data for this event
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID AddButtonClickHandler (sender AS System.Object, 
	                                          e AS System.EventArgs):
		
        DEFINE VARIABLE oItemsToRemove AS "System.Collections.Generic.List<Infragistics.Win.UltraWinListView.UltraListViewItem>":U NO-UNDO .

        oItemsToRemove = NEW "System.Collections.Generic.List<Infragistics.Win.UltraWinListView.UltraListViewItem>":U () .
        
        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in oLeftListView:SelectedItems}
            oRightListView:Items:Add (oItem:Key, oItem:Value) .
            oItemsToRemove:Add (oItem) .        
        END.  

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in oItemsToRemove nodefine}
            oLeftListView:Items:Remove (oItem) .
        END.

        THIS-OBJECT:OnItemsChanged (System.EventArgs:Empty) .
        
        THIS-OBJECT:EvaluateButtons () .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Evaluates the Add/Remove Button state 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID EvaluateButtons ():
		
		ASSIGN oAddButton:Enabled    = oLeftListView:SelectedItems:Count > 0 
		       oRemoveButton:Enabled = oRightListView:SelectedItems:Count > 0 .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ItemSelectionChanged event of the left selection 
                 list
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The ItemSelectionChangedEventArgs with data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID LeftItemSelectionChangedHandler (sender AS System.Object,
	                                                       e AS ItemSelectionChangedEventArgs):
		
        THIS-OBJECT:EvaluateButtons () .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the ItemsChanged
        Notes:   
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnItemsChanged (e AS System.EventArgs):
            
        IF NOT VALID-OBJECT (e) THEN 
            e = System.EventArgs:Empty . 
    
        THIS-OBJECT:ItemsChanged:Publish (THIS-OBJECT, e) .
    
    END METHOD .
 
    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the Remove button 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RemoveButtonClickHandler (sender AS System.Object, 
                                                 e AS System.EventArgs):
        
        DEFINE VARIABLE oItemsToRemove AS "System.Collections.Generic.List<Infragistics.Win.UltraWinListView.UltraListViewItem>":U NO-UNDO .
    
        oItemsToRemove = NEW "System.Collections.Generic.List<Infragistics.Win.UltraWinListView.UltraListViewItem>":U () .
        
        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in oRightListView:SelectedItems}
            oLeftListView:Items:Add (oItem:Key, oItem:Value) .
            oItemsToRemove:Add (oItem) .        
        END.  

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in oItemsToRemove nodefine}
            oRightListView:Items:Remove (oItem) .
        END.

        THIS-OBJECT:OnItemsChanged (System.EventArgs:Empty) .

        THIS-OBJECT:EvaluateButtons () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ItemSelectionChanged event of the right selection 
                 list
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The ItemSelectionChangedEventArgs with data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RightItemSelectionChangedHandler (sender AS System.Object,
                                                            e AS ItemSelectionChangedEventArgs):
        
        THIS-OBJECT:EvaluateButtons () .

    END METHOD .

END CLASS.
