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
    File        : UltraListViewOrderController
    Purpose     : Adds a context menu with sort order to an UltraListView Control
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jun 05 10:46:06 CEST 2015
    Notes       :  
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*          FROM PROPATH .
USING Consultingwerk.Util.*               FROM PROPATH .
USING Consultingwerk.Windows.Controls.*   FROM PROPATH .  
USING Infragistics.Win.UltraWinListView.* FROM ASSEMBLY.
USING Infragistics.Win.UltraWinToolbars.* FROM ASSEMBLY.
USING Progress.Lang.*                     FROM PROPATH .
USING Progress.Util.*                     FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Controls.UltraListViewOrderController: 

    DEFINE VARIABLE lDropBownHandler AS LOGICAL NO-UNDO INIT FALSE .

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the UltraListView Control
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY UltraListView AS UltraListView NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the UltraToolbarsManager used to render the 
                 Context Menu
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY UltraToolbarsManager AS UltraToolbarsManager NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UltraListViewOrderController class
        Notes:   
        @param poUltraListView The reference to hte UltraListView control 
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UltraListViewOrderController (poUltraListView AS UltraListView):
		SUPER ().
		
        ObjectAssert:IsValid (poUltraListView, "poUltraListView":U) .		
		
		THIS-OBJECT:UltraListView = poUltraListView . 

        THIS-OBJECT:InitializeContextMenu () . 
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeforeToolDropdown event of the UltraToolbarsManager  
        Notes:   
        @param sender The reference to the object that raised the event 
        @param e The BeforeToolDropdownEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID BeforeToolDropdownHandler (sender AS System.Object,
	                                                 e AS BeforeToolDropdownEventArgs):
		
		ASSIGN lDropBownHandler = TRUE . 
		
		IF EnumHelper:AreEqual (THIS-OBJECT:UltraListView:MainColumn:Sorting,
		                        Sorting:Ascending) THEN DO: 
		    
		    UltraToolbarsHelper:SetStateButtonToolChecked (THIS-OBJECT:UltraToolbarsManager, "order":U, FALSE) .
            UltraToolbarsHelper:SetStateButtonToolChecked (THIS-OBJECT:UltraToolbarsManager, "alpha":U, TRUE) .
		END.
        ELSE DO:
            UltraToolbarsHelper:SetStateButtonToolChecked (THIS-OBJECT:UltraToolbarsManager, "order":U, TRUE) .
            UltraToolbarsHelper:SetStateButtonToolChecked (THIS-OBJECT:UltraToolbarsManager, "alpha":U, FALSE) .
        END.            		
		
		FINALLY:
            ASSIGN lDropBownHandler = FALSE .		
        END FINALLY.
		
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Context Menu 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID InitializeContextMenu ():
		
        DEFINE VARIABLE oSortTool1 AS StateButtonTool    NO-UNDO . 
        DEFINE VARIABLE oSortTool2 AS StateButtonTool    NO-UNDO . 
        DEFINE VARIABLE oPopupMenu AS PopupMenuTool NO-UNDO . 
		
		ASSIGN THIS-OBJECT:UltraToolbarsManager = NEW UltraToolbarsManager () 
		      
		       oSortTool1 = NEW StateButtonTool ("order":U)
		       oSortTool1:SharedProps:Caption                           = "By Order"{&tran}
		       oSortTool1:SharedProps:AppearancesSmall:Appearance:Image = ImageHelper:Load ("Consultingwerk/Windows/Images/sort_descending2_16.png":U)
		        
		       oSortTool2 = NEW StateButtonTool ("alpha":U) 
               oSortTool2:SharedProps:Caption                           = "By Alphabet"{&tran}
               oSortTool2:SharedProps:AppearancesSmall:Appearance:Image = ImageHelper:Load ("Consultingwerk/Windows/Images/sort_az_descending_16.png":U)
               
               oPopupMenu = NEW PopupMenuTool ("sortpopup":U) 
		       . 

        THIS-OBJECT:UltraToolbarsManager:Tools:Add (oSortTool1) .        
        THIS-OBJECT:UltraToolbarsManager:Tools:Add (oSortTool2) .        
        THIS-OBJECT:UltraToolbarsManager:Tools:Add (oPopupMenu) .
        
        oPopupMenu:Tools:AddTool ("order":U) .
        oPopupMenu:Tools:AddTool ("alpha":U) .

        THIS-OBJECT:UltraToolbarsManager:SetContextMenuUltra (THIS-OBJECT:UltraListView,
                                                              "sortpopup":U) .
                                                              
        THIS-OBJECT:UltraToolbarsManager:BeforeToolDropdown:Subscribe (BeforeToolDropdownHandler) .                                                              
        THIS-OBJECT:UltraToolbarsManager:ToolClick:Subscribe (ToolClickHandler) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Stores the initial order of items 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID StoreOrder ():
		
		DEFINE VARIABLE iOrder AS INTEGER NO-UNDO.
		
        THIS-OBJECT:UltraListView:SubItemColumns:Add ("order":U) .
    
        {Consultingwerk/foreach.i UltraListViewItem oItem in THIS-OBJECT:UltraListView:Items}
        
            oItem:SubItems["order":U]:Value = BOX (iOrder) . 
            iOrder = iOrder + 1 . 
        END.    

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the UltraToolbarsManager  
        Notes:   
        @param sender The reference to the object that raised the event 
        @param e The ToolEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ToolClickHandler (sender AS System.Object,
                                            e AS ToolEventArgs):
		
		IF lDropBownHandler THEN RETURN . 
		
		CASE e:Tool:Key:
		    WHEN "order":U THEN DO:
		        IF NOT THIS-OBJECT:UltraListView:SubItemColumns:Exists ("order":U) THEN 
		            THIS-OBJECT:StoreOrder () . 
		        
		        THIS-OBJECT:UltraListView:MainColumn:Sorting = Sorting:None .
                THIS-OBJECT:UltraListView:SubItemColumns["order":U]:Sorting = Sorting:Ascending .
		    END. 
            WHEN "alpha":U THEN DO:
                IF NOT THIS-OBJECT:UltraListView:SubItemColumns:Exists ("order":U) THEN
                    THIS-OBJECT:StoreOrder () . 
                
                THIS-OBJECT:UltraListView:MainColumn:Sorting = Sorting:Ascending .
                THIS-OBJECT:UltraListView:SubItemColumns["order":U]:Sorting = Sorting:None .          
            END. 
		END.
		
        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .		
        END CATCH.

	END METHOD .

END CLASS.
