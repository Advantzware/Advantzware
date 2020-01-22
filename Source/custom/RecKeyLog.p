/* RecKeyLog.p - rstark - 1.22.2020 */

DEFINE VARIABLE idx AS INTEGER NO-UNDO.

OUTPUT TO "blankRecKey.log" APPEND.
PUT UNFORMATTED
    "Table: {1} - "
    STRING(TODAY,"99/99/9999")
    " @ "
    STRING(TIME,"HH:MM:SS am")
    SKIP.
DO WHILE TRUE:
    PUT UNFORMATTED
        STRING(idx,"99") " - "
        PROGRAM-NAME(idx)
        SKIP.
    idx = idx + 1.
    IF PROGRAM-NAME(idx) EQ ? THEN LEAVE. 
END. /* do while true */
OUTPUT CLOSE.
