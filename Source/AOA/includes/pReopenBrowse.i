/* pReopenBrowse.i - rstark - 10.27.2020 */

IF VALID-HANDLE(hColumnLabel) AND hColumnLabel:LABEL-BGCOLOR EQ 14 THEN
ASSIGN
    hColumnLabel:LABEL-BGCOLOR  = 30
    hColumnLabel:SORT-ASCENDING = NOT lAscending
    .
