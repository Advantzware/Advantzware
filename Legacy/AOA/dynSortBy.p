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

DO idx = 1 TO EXTENT(dynParamValue.colName):
    IF dynParamValue.colName[idx] EQ "" THEN LEAVE.
    IF dynParamValue.sortCol[idx] EQ 0  THEN NEXT.
    CREATE ttSortBy.        
    ASSIGN 
        ttSortBy.ttOrder      = dynParamValue.sortCol[idx]
        ttSortBy.ttSortBy     = dynParamValue.colName[idx]
        ttSortBy.ttDescending = dynParamValue.sortDescending[idx]
        .
END. /* do idx */

FOR EACH ttSortBy BY ttSortBy.ttOrder:
    iopcQueryStr = iopcQueryStr + " BY " + ttSortBy.ttSortBy
                 + IF ttSortBy.ttDescending THEN " DESCENDING" ELSE ""
                 .
END. /* each ttSortBy */
