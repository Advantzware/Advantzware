/* ttSuperProc.i - rstark - 11.28.2019 */

DEFINE TEMP-TABLE ttSuperProc NO-UNDO
    FIELD procName     AS CHARACTER FORMAT "x(40)"  LABEL "Procedure File"
    FIELD internalProc AS CHARACTER FORMAT "x(40)"  LABEL "Internal Name"
    FIELD procParams   AS CHARACTER FORMAT "x(256)" LABEL "Parameter(s)"
    FIELD procType     AS CHARACTER FORMAT "x(12)"  LABEL "Type"
    FIELD returnType   AS CHARACTER FORMAT "x(12)"  LABEL "Return"
        INDEX ttSuperProc IS PRIMARY procName internalProc
        INDEX internalProc internalProc
        INDEX procType procType internalProc
        .
