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
    File        : RenderedBrowseControlApi
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 11 18:38:49 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.WindowIntegrationKit.Controls.* FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.*                FROM ASSEMBLY .
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.Controls.RenderedBrowseControlApi IMPLEMENTS IBrowseApi:

    DEFINE VARIABLE oGrid AS RenderedBrowseControl NO-UNDO .

    DEFINE TEMP-TABLE ttIndex NO-UNDO
        FIELD iIndex AS INTEGER
        INDEX iIndex IS UNIQUE iIndex .

    /*------------------------------------------------------------------------------
        Purpose: The value of the browse column that contains the current cell. This
                 attribute moves focus to the cell in the specified column in the current
                 row.
        Notes:   For the browse, if the browse or a browse component currently has focus,
                 then setting the attribute to another column causes the proper LEAVE
                 and cell ENTRY events to happen.
                 If the setting of the CURRENT-COLUMN attribute happens when focus is
                 outside of the browse, then the browses internal handle to the current
                 column is updated so that it will become the current column when you tab
                 back into the browse. Also if you apply "START-SEARCH" the search mode
                 will now use this column to search on.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CURRENT-COLUMN AS HANDLE NO-UNDO
    GET:
        RETURN oGrid:CURRENT-COLUMN .
    END GET .
    SET (arg AS HANDLE):
        ASSIGN oGrid:CURRENT-COLUMN = arg .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: The number of currently visible foreground iterations for a frame
                 or the number of rows currently visible in a browse widget.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NUM-ITERATIONS AS INTEGER NO-UNDO
    GET:
        RETURN oGrid:DisplayLayout:RowScrollRegions[0]:VisibleRows:Count .
    END.

    /*------------------------------------------------------------------------------
        Purpose: The number of visible leading columns locked in a browse widget.
        Notes:   If a locked column is hidden, the next visible non-locked column in
                 the browse will then become locked.
                 When you use the horizontal scrollbar to scroll columns in the browse,
                 locked columns do not move. For example, if NUM-LOCKED-COLUMNS is 3,
                 then the three leftmost columns in the browse are locked.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NUM-LOCKED-COLUMNS AS INTEGER NO-UNDO
    GET:
        RETURN oGrid:NUM-LOCKED-COLUMNS .
    END.
    SET (arg AS INTEGER):
        oGrid:NUM-LOCKED-COLUMNS = arg .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Returns the number of selected rows
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NUM-SELECTED-ROWS AS INTEGER NO-UNDO
    GET:
        IF oGrid:Selected:Rows:Count > 0 THEN
            RETURN oGrid:Selected:Rows:Count .
        ELSE
            IF VALID-OBJECT (oGrid:ActiveRow) THEN
                RETURN 1 .
            ELSE
                RETURN 0 .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Query used by the Browse
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY QUERY AS HANDLE NO-UNDO
    GET:
        RETURN oGrid:QUERY .
    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RenderedBrowseControlApi class
        Notes:
        @param poGrid The RenderedBrowseControl instance for this instance
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RenderedBrowseControlApi (poGrid AS RenderedBrowseControl):
        SUPER ().

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poGrid """RenderedBrowseControl"":U"} .

        ASSIGN oGrid = poGrid .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
       Purpose: Applies entry to the browse widget
       Notes:   Activates the Grid
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ApplyEntry ():

        oGrid:Focus() .

    END METHOD .

    /*------------------------------------------------------------------------------
       Purpose: Applies entry to a cell of a browse widget
       Notes:   Activates a grid cell
       @param pcColumnName The short name of the browse column
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ApplyEntryToCell (pcColumnName AS CHARACTER):

        DEFINE VARIABLE oColumn AS UltraGridColumn NO-UNDO .

        IF NOT VALID-OBJECT (oGrid:ActiveRow) THEN
            RETURN .

        IF oGrid:DisplayLayout:Bands[0]:Columns:Exists (REPLACE (pcColumnName, ".":U, ":":U)) THEN
            oColumn = oGrid:DisplayLayout:Bands[0]:Columns[REPLACE (pcColumnName, ".":U, ":":U)] .
        ELSE
           IF oGrid:DisplayLayout:Bands[0]:Columns:Exists (ENTRY (NUM-ENTRIES (pcColumnName, ".":U), pcColumnName, ".":U)) THEN
               oColumn = oGrid:DisplayLayout:Bands[0]:Columns[ENTRY (NUM-ENTRIES (pcColumnName, ".":U), pcColumnName, ".":U)] .
        ELSE
            RETURN .

        oGrid:ActiveCell = oGrid:ActiveRow:Cells [oColumn:Key] .

        oGrid:PerformAction (UltraGridAction:EnterEditMode) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Applies entry to a cell of a browse widget
       Notes:
       @param phColumn The handle of a browse column
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ApplyEntryToCell (phColumn AS HANDLE):

        DEFINE VARIABLE cColumnKey AS CHARACTER NO-UNDO.

        IF NOT VALID-OBJECT (oGrid:ActiveRow) THEN
            RETURN .

        cColumnKey = oGrid:ColumnKeyFromColumnHandle (phColumn) .

        IF cColumnKey > "":U THEN
            oGrid:ActiveCell = oGrid:ActiveRow:Cells [cColumnKey] .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deletes the most recently selected row from a browse and the results list.
        Notes:   This method does not delete the record from the database and has no effect
                 on the database buffer. If you then want to delete the database record
                 associated with the row, use the DELETE statement.
        @return If the row is successfully deleted from the browse and results list, the method returns TRUE.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL DELETE-CURRENT-ROW ():

        DEFINE VARIABLE iPosition AS INTEGER NO-UNDO.

        ASSIGN iPosition = oGrid:BindingSource:Position.

        oGrid:QUERY:DELETE-RESULT-LIST-ENTRY () .
        oGrid:BindingSource:RefreshAll () .
        oGrid:Selected:Rows:Clear () .

        IF oGrid:BindingSource:Count > iPosition THEN
            oGrid:BindingSource:Position = iPosition .
        ELSE
            oGrid:BindingSource:Position = oGrid:BindingSource:Count - 1 .

        RETURN TRUE .

        @SuppressUnusedWarnings.
        CATCH err AS Progress.Lang.Error :
            RETURN FALSE .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Deletes the nth selected row from a browse and the results list.
        Notes:   This method does not delete the record from the database and has no
                 effect on the database buffer. If you want to delete the database
                 record associated with the row, use the DELETE statement.
        @param piRow An integer expression that specifies a selected row within the browse.
        @return If the row is successfully deleted, the method returns TRUE.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL DELETE-SELECTED-ROW (piRow AS INTEGER):

        DEFINE VARIABLE iPosition AS INTEGER                                           NO-UNDO .
        DEFINE VARIABLE oSelected AS "System.Collections.Generic.List<System.Int32>":U NO-UNDO .
        @SuppressUnusedWarnings.
        DEFINE VARIABLE iIndex    AS INTEGER                                           NO-UNDO .
        @SuppressUnusedWarnings.
        DEFINE VARIABLE lResult   AS LOGICAL                                           NO-UNDO.
        DEFINE VARIABLE iRow      AS INTEGER                                           NO-UNDO.

        oSelected = NEW "System.Collections.Generic.List<System.Int32>":U () .

        ASSIGN iPosition = oGrid:BindingSource:Position
               iRow      = oGrid:Selected:Rows [piRow - 1]:Index NO-ERROR .

        {Consultingwerk/foreach.i UltraGridRow oRow in oGrid:Selected:Rows}
            oSelected:Add (oRow:Index) .
        END.

        IF THIS-OBJECT:FETCH-SELECTED-ROW (piRow) = TRUE THEN DO:
            lResult = THIS-OBJECT:DELETE-CURRENT-ROW () .

            IF lResult = FALSE THEN
                RETURN FALSE .

            {Consultingwerk/foreach.i System.Object oIndex in oSelected}
                iIndex = UNBOX (oIndex) .

                IF iIndex < iRow THEN
                    oGrid:Selected:Rows:Add (oGrid:Rows[iIndex]) .
                IF iIndex > iRow  THEN
                    oGrid:Selected:Rows:Add (oGrid:Rows[iIndex - 1]) .
            END.

            IF oGrid:BindingSource:Count > iPosition THEN
                oGrid:BindingSource:Position = iPosition .
            ELSE
                oGrid:BindingSource:Position = oGrid:BindingSource:Count - 1 .

            RETURN TRUE .
        END.
        ELSE
            RETURN FALSE .

    END METHOD .

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
    METHOD PUBLIC LOGICAL DELETE-SELECTED-ROWS ():

        DEFINE VARIABLE iPosition AS INTEGER      NO-UNDO .
        DEFINE VARIABLE i         AS INTEGER      NO-UNDO .

        EMPTY TEMP-TABLE ttIndex .

        ASSIGN iPosition = oGrid:BindingSource:Position.

        IF oGrid:Selected:Rows:Count > 0 THEN DO:
            DO i = oGrid:Selected:Rows:Count - 1 TO 0 BY -1:

                CREATE ttIndex .
                ASSIGN ttIndex.iIndex = oGrid:Selected:Rows[i]:Index .
            END.
        END.
        ELSE DO:
            CREATE ttIndex .
            ASSIGN ttIndex.iIndex = oGrid:ActiveRow:Index .
        END.

        FOR EACH ttIndex BY ttIndex.iIndex DESCENDING:
            oGrid:ActiveRow = oGrid:Rows[ttIndex.iIndex] .

            oGrid:QUERY:DELETE-RESULT-LIST-ENTRY () .
        END.

        oGrid:BindingSource:RefreshAll () .
        oGrid:Selected:Rows:Clear () .

        IF oGrid:BindingSource:Count > iPosition THEN
            oGrid:BindingSource:Position = iPosition .
        ELSE
            oGrid:BindingSource:Position = oGrid:BindingSource:Count - 1 .

        RETURN TRUE .

        @SuppressUnusedWarnings.
        CATCH err AS Progress.Lang.Error :
            RETURN FALSE .
        END CATCH.

        FINALLY:
            EMPTY TEMP-TABLE ttIndex .
        END FINALLY.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Deselects the row with current focus.
        Notes:   This method is ignored on single-select browse widgets, because
                 focus follows selection.
        @return Logical value, TRUE
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL DESELECT-FOCUSED-ROW ():

        IF oGrid:BrowseHandle:MULTIPLE = FALSE THEN
            RETURN FALSE .

        oGrid:ActiveRow:Selected = FALSE .

        RETURN TRUE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Deselects all currently selected rows in the browse and clears the
                 associated record buffer.
        Notes:
        @return If the rows are successfully deselected, the method returns TRUE.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL DESELECT-ROWS ():

        oGrid:Selected:Rows:Clear () .

        oGrid:QUERY:GET-BUFFER-HANDLE (1):BUFFER-RELEASE () .

        RETURN TRUE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Deselects the nth selected row in a browse
        Notes:   The AVM maintains a numbered list of selected rows, starting at 1.
                 When the DESELECT-SELECTED-ROW (n) method is encountered, the AVM
                 searches this list to find the nth selected row.
        @param piRow An integer expression that specifies a selected row within the browse.
        @return If the row is successfully deselected, the method returns TRUE.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL DESELECT-SELECTED-ROW (piRow AS INTEGER):

        IF oGrid:Selected:Rows:Count < piRow THEN
            RETURN FALSE .

        oGrid:Selected:Rows[piRow - 1]:Selected = FALSE .

        RETURN TRUE .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Fetches the piRow'th selected row in a browse and puts the row into
                 the database buffer. In other words, this method specifies one row
                 from the one-based index into all currently selected rows and puts
                 that row into the record buffer.
        Notes:
        @param piRow An integer expression that specifies a selected row within the browse.
        @return Logical value indicating if the selected action was successful
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FETCH-SELECTED-ROW (piRow AS INTEGER):

        DEFINE VARIABLE iRowIndex AS INTEGER NO-UNDO INIT -1.

        IF VALID-OBJECT (oGrid:ActiveRow) THEN
            ASSIGN iRowIndex = oGrid:ActiveRow:Index .

        IF piRow > THIS-OBJECT:NUM-SELECTED-ROWS THEN
            RETURN FALSE .

        IF oGrid:Selected:Rows:Count >= piRow THEN
            oGrid:ActiveRow = oGrid:Selected:Rows [piRow - 1] .
        /* no else - as the ActiveRow is by nature already active */

        /* Mike Fechner / Consultingwerk Ltd. / WinKit 3 Oct 2013
           When the Active Row was changed, the PositionChanged event of the
           BindingSource has already fetched the proper record. When it's not
           changing, we need to reposition to the row */
        IF VALID-OBJECT (oGrid:ActiveRow) AND oGrid:ActiveRow:Index = iRowIndex THEN DO:
          oGrid:QUERY:REPOSITION-TO-ROW (oGrid:ActiveRow:ListIndex + 1) .
          oGrid:QUERY:GET-NEXT () .
        END.

        RETURN TRUE .

        @SuppressUnusedWarnings.
        CATCH err AS Progress.Lang.Error :
            RETURN FALSE .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the row index of the browse viewport where the REPOSITION
                 TO ROWID (or RECID) statement displays a repositioned record.
        Notes:
        @return The row index of the browse viewport where the REPOSITION TO ROWID (or RECID) statement displays a repositioned record.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER GET-REPOSITIONED-ROW ():

        RETURN oGrid:GetRepositionedRow () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns TRUE if a specified row in the browse viewport is currently
                 selected.
        Notes:   The AVM maintains a numbered list of selected rows, starting at 1.
        @param piRow An integer expression that specifies a selected row within the browse viewport.
        @return Logical value indicating if a specified row in the browse viewport is currently selected.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IS-ROW-SELECTED (piRow AS INTEGER):

        IF oGrid:DisplayLayout:RowScrollRegions[0]:VisibleRows:Count < piRow THEN
            RETURN FALSE .

        RETURN oGrid:DisplayLayout:RowScrollRegions[0]:VisibleRows [piRow - 1]:Row:Selected .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Forces the AVM to refresh the display of the current rows in a browse.
        Notes:
        @return Logical value indicating if the selected action was successful
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL Refresh ():

        oGrid:BindingSource:RefreshAll () .

        RETURN TRUE .

        @SuppressUnusedWarnings.
        CATCH err AS Progress.Lang.Error :
            RETURN FALSE .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Moves a query objects result list pointer to the row corresponding
                 to the specified sequence number.
        Notes:   The REPOSITION-TO-ROW method corresponds to the REPOSITION statement
                 with the TO ROW option.
        @param piRow An integer expression representing the sequence number.
        @return Logical value, always TRUE
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL REPOSITION-TO-ROW (piRow AS INTEGER):

        RETURN oGrid:FetchRow (piRow) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Scrolls a browse (if necessary) to bring a specified selected row
                 into view
        Notes:
        @param piRow An integer expression that specifies a selected row within the browse
        @return If the row is successfully scrolled into view (or if the scroll is unnecessary), the method returns TRUE
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL SCROLL-TO-SELECTED-ROW (piRow AS INTEGER):

        IF piRow > THIS-OBJECT:NUM-SELECTED-ROWS THEN
            RETURN FALSE .

        oGrid:DisplayLayout:RowScrollRegions [0]:ScrollRowIntoView (oGrid:Selected:Rows [piRow - 1]) .

        RETURN TRUE .

        @SuppressUnusedWarnings.
        CATCH err AS Progress.Lang.Error :
            RETURN FALSE .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Selects all rows in a query connected to the browse.
        Notes:
        @return Logical value indicating if the selected action was successful
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL SELECT-ALL ():

        oGrid:Selected:Rows:AddRange (oGrid:Rows:GetAllNonGroupByRows()) .

        RETURN TRUE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Selects the row that currently has focus in a browse widget, even if
                 it is not currently displayed.
        Notes:   This method repositions the query to that row and copies the record
                 into the database buffer. The browse automatically scrolls to the
                 selected row. You can use this method after a REPOSITION statement
                 to position a query to a selected row.
        @return Logical value indicating if the selected action was successful
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL SELECT-FOCUSED-ROW ():

        oGrid:DisplayLayout:RowScrollRegions [0]:ScrollRowIntoView (oGrid:ActiveRow) .

        IF Progress.Util.EnumHelper:AreEqual(oGrid:DisplayLayout:Override:SelectTypeRow,
                                             Infragistics.Win.UltraWinGrid.SelectType:Extended) OR
           Progress.Util.EnumHelper:AreEqual(oGrid:DisplayLayout:Override:SelectTypeRow,
                                             Infragistics.Win.UltraWinGrid.SelectType:ExtendedAutoDrag) THEN DO:

            IF NOT oGrid:Selected:Rows:Contains (oGRid:ActiveRow) THEN
                oGrid:Selected:Rows:Add (oGRid:ActiveRow) .
        END.
        ELSE DO:
            oGrid:Selected:Rows:Clear () .
            oGrid:Selected:Rows:Add (oGRid:ActiveRow) .
        END.

        RETURN TRUE .

        @SuppressUnusedWarnings.
        CATCH err AS Progress.Lang.Error :
            RETURN FALSE .
        END CATCH.

    END METHOD .

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
    METHOD PUBLIC LOGICAL SELECT-NEXT-ROW ():

        DEFINE VARIABLE oRow AS UltraGridRow NO-UNDO .

        IF oGrid:Rows:Count = 0 THEN
            RETURN FALSE .

        IF oGrid:Selected:Rows:Count > 0 THEN
            oRow = oGrid:Selected:Rows [oGrid:Selected:Rows:Count - 1] .
        ELSE
            oRow = oGrid:ActiveRow .

        oGrid:Selected:Rows:Clear () .

        IF oRow:Index = oGrid:Rows:Count - 1 THEN
            RETURN FALSE .

        oRow = oGrid:Rows [oRow:Index + 1] .

        IF Progress.Util.EnumHelper:AreNotEqual (oGrid:DisplayLayout:Override:SelectTypeRow,
                                                 SelectType:None) AND
           Progress.Util.EnumHelper:AreNotEqual (oGrid:DisplayLayout:Bands[0]:Override:SelectTypeRow,
                                                 SelectType:None) THEN
            oGrid:Selected:Rows:Add (oRow) .

        oGrid:DisplayLayout:RowScrollRegions [0]:ScrollRowIntoView (oRow) .

        oGrid:ActiveRow = oRow .

        RETURN TRUE .

        @SuppressUnusedWarnings.
        CATCH err AS Progress.Lang.Error :
            RETURN FALSE .
        END CATCH.

    END METHOD .

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
    METHOD PUBLIC LOGICAL SELECT-PREV-ROW ():

        DEFINE VARIABLE oRow AS UltraGridRow NO-UNDO .

        IF oGrid:Rows:Count = 0 THEN
            RETURN FALSE .

        IF oGrid:Selected:Rows:Count > 0 THEN
            oRow = oGrid:Selected:Rows [oGrid:Selected:Rows:Count - 1] .
        ELSE
            oRow = oGrid:ActiveRow .

        oGrid:Selected:Rows:Clear () .

        IF oRow:Index <= 0 THEN
            RETURN FALSE .

        oRow = oGrid:Rows [oRow:Index - 1] .

        IF Progress.Util.EnumHelper:AreNotEqual (oGrid:DisplayLayout:Override:SelectTypeRow,
                                                 SelectType:None) AND
           Progress.Util.EnumHelper:AreNotEqual (oGrid:DisplayLayout:Bands[0]:Override:SelectTypeRow,
                                                 SelectType:None) THEN
            oGrid:Selected:Rows:Add (oRow) .

        oGrid:DisplayLayout:RowScrollRegions [0]:ScrollRowIntoView (oRow) .

        oGrid:ActiveRow = oRow .

        RETURN TRUE .

        @SuppressUnusedWarnings.
        CATCH err AS Progress.Lang.Error :
            RETURN FALSE .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Selects the specified row if it is currently in the browse viewport.
                 In a single-select browse, the previously selected row is deselected.
        Notes:   No rows are deselected in a multiple-select browse.
        @param piRow An integer expression specifying the ordinal position of a row within the browse.
        @return Logical value indicating if the selected action was successful
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL SELECT-ROW (piRow AS INTEGER):

        DEFINE VARIABLE oRow AS UltraGridRow NO-UNDO .

        oRow = oGrid:DisplayLayout:RowScrollRegions[0]:VisibleRows [piRow - 1]:Row .

        IF Progress.Util.EnumHelper:AreNotEqual(oGrid:DisplayLayout:Override:SelectTypeRow,
                                                Infragistics.Win.UltraWinGrid.SelectType:None) OR
           Progress.Util.EnumHelper:AreNotEqual(oGrid:DisplayLayout:Override:SelectTypeRow,
                                                Infragistics.Win.UltraWinGrid.SelectType:None) THEN

            oGrid:Selected:Rows:Add (oRow) .

        oGrid:ActiveRow = oRow .

        @SuppressUnusedWarnings.
        CATCH err AS Progress.Lang.Error :
            RETURN FALSE .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets the row index where records positioned with the REPOSITION TO
                 ROWID (or RECID) statement are displayed.
        Notes:   Currently not yet supports
        @param piRowIndex Indicates the row number where the new record is displayed, 1 being the first row.
        @param pcRepositionMode ALWAYS or CONDITIONAL
        @return Logical value indicating the success of the method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL SET-REPOSITIONED-ROW (piRowIndex AS INTEGER, pcRepositionMode AS CHARACTER) .

        oGrid:SetRepositionedRow (piRowIndex, pcRepositionMode) .

        RETURN TRUE .

        @SuppressUnusedWarnings.
        CATCH err AS Progress.Lang.Error:
            RETURN FALSE .
        END CATCH.

    END METHOD .

END CLASS.
