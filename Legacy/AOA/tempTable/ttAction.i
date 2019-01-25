/* ttAction.i - rstark - 1.25.2019 */

DEFINE TEMP-TABLE ttAction NO-UNDO
    FIELD paramWidget    AS HANDLE
    FIELD paramID        AS INTEGER
    FIELD actionParamID  AS INTEGER
    FIELD action         AS CHARACTER
    FIELD initializeProc AS CHARACTER
    FIELD validateProc   AS CHARACTER
        INDEX paramWidget IS PRIMARY paramWidget
        INDEX paramID paramID
        INDEX actionParamID actionParamID action
        .
