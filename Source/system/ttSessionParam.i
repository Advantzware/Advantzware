/* ttSessionParam.i - rstark - 11.12.2021 */

DEFINE TEMP-TABLE ttSessionParam NO-UNDO
    FIELD sessionParam AS CHARACTER FORMAT "x(50)" LABEL "Session Parameter"
    FIELD sessionValue AS CHARACTER FORMAT "x(50)" LABEL "Session Value"
    FIELD stackTrace   AS CHARACTER
        INDEX sessionParam IS PRIMARY UNIQUE sessionParam
        .
