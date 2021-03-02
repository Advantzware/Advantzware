/* startSearch.i - rstark - 10.27.2020 */

IF VALID-HANDLE(hColumnLabel{1}) AND hColumnLabel{1}:LABEL-BGCOLOR EQ 30 THEN
ASSIGN
    hColumnLabel{1}:LABEL-BGCOLOR  = 14
    hColumnLabel{1}:SORT-ASCENDING = ?
    .
IF SELF:CURRENT-COLUMN:NAME NE ? THEN DO:
    ASSIGN
        hColumnLabel{1} = SELF:CURRENT-COLUMN
        cColumnLabel{1} = SELF:CURRENT-COLUMN:NAME
        .
    IF cColumnLabel{1} EQ cSaveLabel{1} THEN
    lAscending = NOT lAscending.
    cSaveLabel{1} = cColumnLabel{1}.
    &IF DEFINED(ScheduleBoard) NE 0 &THEN
    ascendingSort = lAscending.
    RUN reopenBrowse.
    &ELSEIF DEFINED(ScheduleBoardPending) NE 0 &THEN
    ascendingFlag = lAscending.
    RUN openQueryJobID.
    &ELSE
    RUN pReopenBrowse.
    &ENDIF
    &IF DEFINED(startSearchValueChanged) NE 0 &THEN
    APPLY "VALUE-CHANGED":U TO SELF.
    &ENDIF
    &IF DEFINED(sortButton) NE 0 &THEN
    &IF DEFINED(sortButtonFrame) EQ 0 &THEN
    &Scoped-define sortButtonFrame {&FRAME-NAME}
    &ENDIF
    btnSort:SENSITIVE IN FRAME {&sortButtonFrame} = VALID-HANDLE(hColumnLabel{1}).
    btnSort:LOAD-IMAGE("Graphics/32x32/"
        + IF lAscending THEN "sort_az_descending.png"
          ELSE "sort_az_descending2.png")
        .
    &ENDIF
END.
RETURN NO-APPLY.

&IF DEFINED(startSearchValueChanged) NE 0 &THEN
&Undefine startSearchValueChanged
&ENDIF

&IF DEFINED(sortButton) NE 0 &THEN
&Undefine sortButton
&ENDIF

&IF DEFINED(sortButtonFrame) NE 0 &THEN
&Undefine sortButtonFrame
&ENDIF

&IF DEFINED(ScheduleBoard) NE 0 &THEN
&Undefine ScheduleBoard
&ENDIF

&IF DEFINED(ScheduleBoardPending) NE 0 &THEN
&Undefine ScheduleBoardPending
&ENDIF
