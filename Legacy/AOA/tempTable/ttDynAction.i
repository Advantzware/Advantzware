/* ttDynAction.i - rstark - 1.25.2019 */

DEFINE TEMP-TABLE ttDynAction NO-UNDO
    FIELD paramWidget     AS HANDLE
    FIELD paramName       AS CHARACTER FORMAT "x(30)"
    FIELD paramID         AS INTEGER   FORMAT ">>9"
    FIELD actionParamName AS CHARACTER FORMAT "X(30)"
    FIELD action          AS CHARACTER FORMAT "X(30)"
    FIELD initializeProc  AS CHARACTER FORMAT "X(30)"
    FIELD validateProc    AS CHARACTER FORMAT "X(30)"
    FIELD descriptionProc AS CHARACTER FORMAT "X(30)"
        INDEX paramWidget IS PRIMARY paramWidget
        INDEX paramName paramName
        INDEX paramID paramID
        INDEX actionParamName actionParamName action
        .
