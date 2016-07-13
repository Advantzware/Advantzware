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
    File        : BrowseWidgetHelper
    Purpose     : Helper routines for ABL browse widgets
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 23 14:29:52 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Util.BrowseWidgetHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Protected Constructor for the BrowseWidgetHelper class
        Notes:   Avoid instance creation
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PROTECTED BrowseWidgetHelper ():
		SUPER ().
		
	END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN       
    /*------------------------------------------------------------------------------
        Purpose: Avoids the appearance of the horizontal scrollbar in the browse
                 by making sure that all columns fit into the browse widget itself 
        Notes:   
        @param phBrowse The handle of the ABL browse widget
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID AvoidHorizontalScrollbar (phBrowse AS HANDLE):
		
		DEFINE VARIABLE deFactor    AS DECIMAL NO-UNDO .
		DEFINE VARIABLE iMax        AS INTEGER NO-UNDO .
		DEFINE VARIABLE i           AS INTEGER NO-UNDO .
		DEFINE VARIABLE hColumn     AS HANDLE  NO-UNDO .
		DEFINE VARIABLE hLastColumn AS HANDLE NO-UNDO.
		DEFINE VARIABLE iWidths     AS INTEGER NO-UNDO EXTENT .
		
		Consultingwerk.Windows.API.Win32:LockWindowUpdate (phBrowse:HANDLE:HWND) .

		{Consultingwerk/Assertion/HandleAssert/WidgetType.i phBrowse Consultingwerk.WidgetTypeEnum:Browse} .

        EXTENT (iWidths) = phBrowse:NUM-COLUMNS .
        DO i = 1 TO phBrowse:NUM-COLUMNS:
            ASSIGN iWidths[i]  = phBrowse:GET-BROWSE-COLUMN (i):WIDTH-PIXELS 
                   hLastColumn = phBrowse:GET-BROWSE-COLUMN (i) .
        END.

        ASSIGN iMax = phBrowse:WIDTH-PIXELS - 
                      System.Windows.Forms.SystemInformation:VerticalScrollBarWidth - 
                      4
               deFactor = (hLastColumn:X + hLastColumn:WIDTH-PIXELS) / iMax .

        DO i = 1 TO phBrowse:NUM-COLUMNS:
            phBrowse:GET-BROWSE-COLUMN (i):WIDTH-PIXELS = 1 .
        END.

        DO i = 1 TO phBrowse:NUM-COLUMNS:
            phBrowse:GET-BROWSE-COLUMN (i):WIDTH-PIXELS = ROUND (iWidths[i] / deFactor - .5, 0) .
        END.

        phBrowse:FIT-LAST-COLUMN = TRUE . 

        Consultingwerk.Windows.API.Win32:LockWindowUpdate (0) .
        PROCESS EVENTS . 
        
	END METHOD .
&ENDIF    

    /*------------------------------------------------------------------------------
        Purpose: Positions a field level widget above a Browse cell
        Notes:   The field level widget is supposed to be contained in the same frame
                 as the browse widget  
        @param phBrowse The handle of the ABL browse widget
        @param piColumn The column number 
        @param phWidget The handle of the widget to position on the browse cell
        @param piHeightOffset The offset in number of pixels from the top of the browse widget 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID PositionWidgetAboveBrowseCell (phBrowse       AS HANDLE, 
        	                                                 piColumn       AS INTEGER,
        	                                                 phWidget       AS HANDLE, 
        	                                                 piHeightOffset AS INTEGER):
		
		DEFINE VARIABLE hColumn AS HANDLE NO-UNDO.
		
		ASSIGN hColumn = phBrowse:GET-BROWSE-COLUMN (piColumn) .
		
		ASSIGN phWidget:Y             = phBrowse:Y + piHeightOffset - 1 
		       phWidget:HEIGHT-PIXELS = phBrowse:ROW-HEIGHT-PIXELS + 4
		       phWidget:WIDTH-PIXELS  = MAX (hColumn:WIDTH-PIXELS + 4, 1)
		       phWidget:X             = phBrowse:X + hColumn:X - 1
		       
		       NO-ERROR .
		
		phWidget:MOVE-TO-TOP () .

	END METHOD .

END CLASS.
