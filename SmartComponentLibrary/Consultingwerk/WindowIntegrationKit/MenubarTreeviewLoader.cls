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
    File        : MenubarTreeviewLoader
    Purpose     : Parses an ABL Menu-Bar and populates the nodes of an 
                  UltraTree
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Mar 11 12:09:13 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                      FROM PROPATH .
USING Consultingwerk.Util.*                 FROM PROPATH .  
USING Consultingwerk.WindowIntegrationKit.* FROM PROPATH .
USING Infragistics.Win.UltraWinTree.*       FROM ASSEMBLY .   
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.MenubarTreeviewLoader: 

    /*------------------------------------------------------------------------------
        Purpose: Loads an ABL menu-bar into an UltraTree 
        Notes:   
        @param phMenubar The HANDLE of the ABL Menubar
        @param poTreeView The UltraTree Control to load the menu into
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Load (phMenubar AS HANDLE,
	                         poTreeView AS UltraTree):
		
		{Consultingwerk/Assertion/HandleAssert/WidgetType.i phMenubar WidgetTypeEnum:Menu} .
		                                                  
        Consultingwerk.Assertion.ObjectAssert:IsValid (poTreeView) .		                                                  

        THIS-OBJECT:Load (phMenubar,
                          poTreeView:Nodes) .

        /* Expand the Root Nodes */
        {Consultingwerk/foreach.i UltraTreeNode oNode in poTreeView:Nodes}
            oNode:Expanded = TRUE . 
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Loads an ABL menu into an UltraTree 
        Notes:   
        @param phMenu The HANDLE of the ABL Menu
        @param poNodes The TreeNodesCollection to load the menu into
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Load (phMenu AS HANDLE,
                             poNodes AS TreeNodesCollection):

        DEFINE VARIABLE hMenuItem AS HANDLE        NO-UNDO.
        DEFINE VARIABLE oNode     AS UltraTreeNode NO-UNDO . 

        ASSIGN hMenuItem = phMenu:FIRST-CHILD . 
        
        DO WHILE VALID-HANDLE (hMenuItem) ON ERROR UNDO, THROW:
            
            IF hMenuItem:TYPE = WidgetTypeEnum:MenuItem AND hMenuItem:SUBTYPE = "RULE":U THEN DO:
                hMenuItem = hMenuItem:NEXT-SIBLING .
                NEXT .    
            END.
            
            oNode = NEW UltraTreeNode (STRING (hMenuItem), 
                                       UltraToolbarsHelper:RemoveMenuAmpersand(hMenuItem:LABEL)) . 
            
            poNodes:Add (oNode) .
            
            IF hMenuItem:TYPE = WidgetTypeEnum:SubMenu THEN 
                THIS-OBJECT:Load (hMenuItem, oNode:Nodes) .

            hMenuItem = hMenuItem:NEXT-SIBLING . 
        END.

    END METHOD .

END CLASS.
