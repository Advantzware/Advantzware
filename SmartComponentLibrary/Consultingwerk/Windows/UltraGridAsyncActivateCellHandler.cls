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
    File        : UltraGridAsyncActivateCellHandler
    Purpose     : Uses a 1 msec timer to activate a cell in an UltraGrid
                  asynchronously
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 04 14:47:58 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.


USING Consultingwerk.WindowIntegrationKit.Controls.* FROM PROPATH .  
USING Infragistics.Win.UltraWinGrid.*                FROM ASSEMBLY . 
USING Progress.Lang.*                                FROM PROPATH .
USING System.Windows.Forms.*                         FROM ASSEMBLY . 

CLASS Consultingwerk.Windows.UltraGridAsyncActivateCellHandler: 

    DEFINE VARIABLE oGrid   AS UltraGrid NO-UNDO . 
    DEFINE VARIABLE cColumn AS CHARACTER NO-UNDO . 
    DEFINE VARIABLE oTimer  AS Timer     NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UltraGridAsyncActivateCellHandler class
        Notes:   
        @param poGrid The reference to the Grid 
        @param pcColumn The key of the Column that should be activated after a 1 msec Timer
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UltraGridAsyncActivateCellHandler (poGrid AS UltraGrid,
	                                                      pcColumn AS CHARACTER):
		SUPER ().

        ASSIGN oGrid   = poGrid
               cColumn = pcColumn . 

        oTimer = NEW Timer () .
        oTimer:Tick:Subscribe (TickHandler) .
        oTimer:Interval = 1 .
        oTimer:Enabled = TRUE .  
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Handler for the Tick event
        Notes:   Used for asynchronous operation
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID TickHandler (sender AS System.Object,
	                                   e AS System.EventArgs):
		
		oTimer:Enabled = FALSE . 
		
        oGrid:Focus () .
        oGrid:ActiveRow:Cells[cColumn]:Activate () .
        oGrid:PerformAction (UltraGridAction:EnterEditMode) .

        oTimer:Tick:Unsubscribe (TickHandler) .
        DELETE OBJECT oTimer .
        DELETE OBJECT THIS-OBJECT . 
        
	END METHOD .

END CLASS.
