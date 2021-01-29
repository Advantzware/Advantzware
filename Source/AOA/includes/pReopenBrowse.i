/* pReopenBrowse.i - rstark - 10.27.2020 */

IF VALID-HANDLE(hColumnLabel{1}) AND hColumnLabel{1}:LABEL-BGCOLOR EQ 14 THEN
ASSIGN
    hColumnLabel{1}:LABEL-BGCOLOR  = 30
    hColumnLabel{1}:SORT-ASCENDING = NOT lAscending
    .
