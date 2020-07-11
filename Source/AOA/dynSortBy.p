/* dynSortBy.p - rstark - 4.9.2019 */

DEFINE PARAMETER BUFFER dynParamValue FOR dynParamValue.
DEFINE INPUT-OUTPUT PARAMETER iopcQueryStr AS CHARACTER NO-UNDO.

DEFINE VARIABLE idx AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttSortBy NO-UNDO
    FIELD ttOrder      AS INTEGER 
    FIELD ttSortBy     AS CHARACTER 
    FIELD ttDescending AS LOGICAL
        INDEX ttSortBy IS PRIMARY
            ttOrder
            .

FOR EACH dynValueColumn NO-LOCK
    WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
      AND dynValueColumn.user-id      EQ dynParamValue.user-id
      AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
      AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
      AND dynValueColumn.sortCol      GT 0
       BY dynValueColumn.sortOrder
    :
    CREATE ttSortBy.
    ASSIGN 
        ttSortBy.ttOrder      = dynValueColumn.sortCol
        ttSortBy.ttSortBy     = dynValueColumn.colName
        ttSortBy.ttDescending = dynValueColumn.sortDescending
        .
END. /* each dynvaluecolumn */

FOR EACH ttSortBy BY ttSortBy.ttOrder:
    iopcQueryStr = iopcQueryStr + " BY " + ttSortBy.ttSortBy
                 + IF ttSortBy.ttDescending THEN " DESCENDING" ELSE ""
                 .
END. /* each ttSortBy */
