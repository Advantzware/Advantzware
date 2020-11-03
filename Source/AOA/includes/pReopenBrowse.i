/* pReopenBrowse.i - rstark - 10.27.2020 */

IF VALID-HANDLE(hColumnLabel) AND hColumnLabel:LABEL-BGCOLOR EQ 22 THEN
ASSIGN
    hColumnLabel:LABEL-BGCOLOR  = 27
    hColumnLabel:SORT-ASCENDING = NOT lAscending
    .
