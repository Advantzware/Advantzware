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
    File        : IBrowseApi
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 11 18:33:12 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.WindowIntegrationKit.Controls.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.WindowIntegrationKit.Controls.IBrowseApi:  
    
    /*------------------------------------------------------------------------------
        Purpose: The value of the browse column that contains the current cell. This 
                 attribute moves focus to the cell in the specified column in the current 
                 row. 
        Notes:   For the browse, if the browse or a browse component currently has focus, 
                 then setting the attribute to another column causes the proper LEAVE 
                 and cell ENTRY events to happen.
                 If the setting of the CURRENT-COLUMN attribute happens when focus is 
                 outside of the browse, then the browse’s internal handle to the current 
                 column is updated so that it will become the current column when you tab 
                 back into the browse. Also if you apply "START-SEARCH" the search mode 
                 will now use this column to search on.
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY CURRENT-COLUMN AS HANDLE NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: The number of currently visible foreground iterations for a frame 
                 or the number of rows currently visible in a browse widget. 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY NUM-ITERATIONS AS INTEGER NO-UNDO 
	GET.

    /*------------------------------------------------------------------------------
        Purpose: The number of visible leading columns locked in a browse widget.  
        Notes:   If a locked column is hidden, the next visible non-locked column in 
                 the browse will then become locked.   
                 When you use the horizontal scrollbar to scroll columns in the browse, 
                 locked columns do not move. For example, if NUM-LOCKED-COLUMNS is 3, 
                 then the three leftmost columns in the browse are locked.
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY NUM-LOCKED-COLUMNS AS INTEGER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the number of rows currently selected in a browse widget. 
        Notes:   A browse can have more than one row selected only if the MULTIPLE 
                 attribute is TRUE. 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NUM-SELECTED-ROWS AS INTEGER NO-UNDO 
    GET .  

    /*------------------------------------------------------------------------------
        Purpose: Returns the Query used by the Browse 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY QUERY AS HANDLE NO-UNDO 
	GET.

    /*------------------------------------------------------------------------------
       Purpose: Applies entry to the browse widget
       Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ApplyEntry ().
    
    /*------------------------------------------------------------------------------
       Purpose: Applies entry to a cell of a browse widget
       Notes:
       @param pcColumnName The short name of the browse column
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ApplyEntryToCell (pcColumnName AS CHARACTER).

    /*------------------------------------------------------------------------------
       Purpose: Applies entry to a cell of a browse widget
       Notes:
       @param phColumn The handle of a browse column
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ApplyEntryToCell (phColumn AS HANDLE).
    
    /*------------------------------------------------------------------------------
        Purpose: Deletes the most recently selected row from a browse and the results list. 
        Notes:   This method does not delete the record from the database and has no effect 
                 on the database buffer. If you then want to delete the database record 
                 associated with the row, use the DELETE statement. 
        @return If the row is successfully deleted from the browse and results list, the method returns TRUE.
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL DELETE-CURRENT-ROW ().

    /*------------------------------------------------------------------------------
        Purpose: Deletes the nth selected row from a browse and the results list. 
        Notes:   This method does not delete the record from the database and has no 
                 effect on the database buffer. If you want to delete the database 
                 record associated with the row, use the DELETE statement. 
        @param piRow An integer expression that specifies a selected row within the browse.
        @return If the row is successfully deleted, the method returns TRUE. 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL DELETE-SELECTED-ROW (piRow AS INTEGER).

    /*------------------------------------------------------------------------------
        Purpose: Deletes all currently selected rows from a browse and the associated 
                 results list. 
        Notes:   Do not confuse the DELETE-SELECTED-ROW method (note the singular) 
                 with the DELETE-SELECTED-ROWS method (note the plural). 
                 This method does not delete the record from the database and has no 
                 effect on the database buffer. If you want to delete the database 
                 record associated with the row, use the DELETE statement. 
        @return If the row is successfully deleted, the method returns TRUE. 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL DELETE-SELECTED-ROWS ().

    /*------------------------------------------------------------------------------
        Purpose: Deselects the row with current focus. 
        Notes:   This method is ignored on single-select browse widgets, because 
                 focus follows selection. 
        @return Logical value, TRUE 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL DESELECT-FOCUSED-ROW ().

    /*------------------------------------------------------------------------------
        Purpose: Deselects all currently selected rows in the browse and clears the 
                 associated record buffer. 
        Notes:    
        @return If the rows are successfully deselected, the method returns TRUE. 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL DESELECT-ROWS ().

    /*------------------------------------------------------------------------------
        Purpose: Deselects the nth selected row in a browse 
        Notes:   The AVM maintains a numbered list of selected rows, starting at 1. 
                 When the DESELECT-SELECTED-ROW (n) method is encountered, the AVM 
                 searches this list to find the nth selected row.  
        @param piRow An integer expression that specifies a selected row within the browse.
        @return If the row is successfully deselected, the method returns TRUE. 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL DESELECT-SELECTED-ROW (piRow AS INTEGER).

    /*------------------------------------------------------------------------------
        Purpose: Fetches the piRow'th selected row in a browse and puts the row into 
                 the database buffer. In other words, this method specifies one row 
                 from the one-based index into all currently selected rows and puts 
                 that row into the record buffer. 
        Notes:   
        @param piRow An integer expression that specifies a selected row within the browse.
        @return Logical value indicating if the selected action was successful
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FETCH-SELECTED-ROW (piRow AS INTEGER) .

    /*------------------------------------------------------------------------------
        Purpose: Returns the row index of the browse viewport where the REPOSITION 
                 TO ROWID (or RECID) statement displays a repositioned record.
        Notes:   
        @return The row index of the browse viewport where the REPOSITION TO ROWID (or RECID) statement displays a repositioned record. 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC INTEGER GET-REPOSITIONED-ROW ().

    /*------------------------------------------------------------------------------
        Purpose: Returns TRUE if a specified row in the browse viewport is currently 
                 selected. 
        Notes:   The AVM maintains a numbered list of selected rows, starting at 1. 
        @param piRow An integer expression that specifies a selected row within the browse viewport.
        @return Logical value indicating if a specified row in the browse viewport is currently selected. 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL IS-ROW-SELECTED (piRow AS INTEGER).  

    /*------------------------------------------------------------------------------
        Purpose: Forces the AVM to refresh the display of the current rows in a browse.  
        Notes:   
        @return Logical value indicating if the selected action was successful
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL Refresh () .

	/*------------------------------------------------------------------------------
	    Purpose: Moves a query object’s result list pointer to the row corresponding 
	             to the specified sequence number.
	    Notes:   The REPOSITION-TO-ROW method corresponds to the REPOSITION statement 
	             with the TO ROW option.
	    @param piRow An integer expression representing the sequence number.
	    @return Logical value, always TRUE
	------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL REPOSITION-TO-ROW (piRow AS INTEGER).

    /*------------------------------------------------------------------------------
        Purpose: Scrolls a browse (if necessary) to bring a specified selected row 
                 into view
        Notes:   
        @param piRow An integer expression that specifies a selected row within the browse
        @return If the row is successfully scrolled into view (or if the scroll is unnecessary), the method returns TRUE 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL SCROLL-TO-SELECTED-ROW (piRow AS INTEGER).

    /*------------------------------------------------------------------------------
        Purpose: Selects all rows in a query connected to the browse. 
        Notes:   
        @return Logical value indicating if the selected action was successful
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL SELECT-ALL ().  

    /*------------------------------------------------------------------------------
        Purpose: Selects the row that currently has focus in a browse widget, even if 
                 it is not currently displayed. 
        Notes:   This method repositions the query to that row and copies the record 
                 into the database buffer. The browse automatically scrolls to the 
                 selected row. You can use this method after a REPOSITION statement 
                 to position a query to a selected row.   
        @return Logical value indicating if the selected action was successful
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL SELECT-FOCUSED-ROW ().

    /*------------------------------------------------------------------------------
        Purpose: Deselects all currently selected rows in a browse and selects the 
                 row after the deselected row. 
        Notes:   This method also repositions the query to the new row and copies the 
                 record into the database buffer. The browse automatically scrolls 
                 to the selected row if it is out of view. 
                 This method is intended for use with a browse that supports the 
                 selection of a single row at a time (MULTIPLE attribute is set to 
                 FALSE). If more than one row is selected when you execute this 
                 method, all of the selected rows are deselected and the record 
                 after the last selected row becomes the selected row. 
        @return Logical value indicating if the selected action was successful
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL SELECT-NEXT-ROW ().

    /*------------------------------------------------------------------------------
        Purpose: Deselects a currently selected row in a browse and selects the row 
                 before the deselected row. 
        Notes:   This method also repositions the query to the new row and copies 
                 the record into the database buffer. The browse automatically 
                 scrolls to the selected row if it is out of view. 
                 This method is intended for use with a browse that supports the 
                 selection of a single row at a time (MULTIPLE attribute is set to 
                 FALSE). If more than one row is selected when you execute this method, 
                 all of the selected rows are deselected and the record before the 
                 last selected row becomes the selected row. 
        @return Logical value indicating if the selected action was successful
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL SELECT-PREV-ROW ().

    /*------------------------------------------------------------------------------
        Purpose: Selects the specified row if it is currently in the browse viewport. 
                 In a single-select browse, the previously selected row is deselected.   
        Notes:   No rows are deselected in a multiple-select browse.   
        @param piRow An integer expression specifying the ordinal position of a row within the browse.
        @return Logical value indicating if the selected action was successful
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL SELECT-ROW (piRow AS INTEGER).

    /*------------------------------------------------------------------------------
        Purpose: Sets the row index where records positioned with the REPOSITION TO 
                 ROWID (or RECID) statement are displayed.
        Notes:   Currently not yet supports   
        @param piRowIndex Indicates the row number where the new record is displayed, 1 being the first row.
        @param pcRepositionMode ALWAYS or CONDITIONAL 
        @return Logical value indicating the success of the method
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL SET-REPOSITIONED-ROW (piRowIndex AS INTEGER, pcRepositionMode AS CHARACTER) .
  
END INTERFACE.
