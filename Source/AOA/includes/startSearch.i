/* startSearch.i - rstark - 10.27.2020 */

IF VALID-HANDLE(hColumnLabel) AND hColumnLabel:LABEL-BGCOLOR EQ 27 THEN
ASSIGN
    hColumnLabel:LABEL-BGCOLOR  = 22
    hColumnLabel:SORT-ASCENDING = ?
    .
IF SELF:CURRENT-COLUMN:NAME NE ? THEN DO:
    ASSIGN
        hColumnLabel = SELF:CURRENT-COLUMN
        cColumnLabel = SELF:CURRENT-COLUMN:NAME
        .
    IF cColumnLabel EQ cSaveLabel THEN
    lAscending = NOT lAscending.
    cSaveLabel = cColumnLabel.
    RUN pReopenBrowse.
    &IF DEFINED(startSearchValueChanged) NE 0 &THEN
    APPLY "VALUE-CHANGED":U TO SELF.
    &ENDIF
    &IF DEFINED(sortButton) NE 0 &THEN
    &IF DEFINED(sortButtonFrame) EQ 0 &THEN
    &Scoped-define sortButtonFrame {&FRAME-NAME}
    &ENDIF
    btnSort:SENSITIVE IN FRAME {&sortButtonFrame} = VALID-HANDLE(hColumnLabel).
    btnSort:LOAD-IMAGE("Graphics/32x32/"
        + IF lAscending THEN "sort_az_descending.ico"
          ELSE "sort_az_descending2.ico")
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
