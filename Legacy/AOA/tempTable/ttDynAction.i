/* ttDynAction.i - rstark - 1.25.2019 */

DEFINE TEMP-TABLE ttDynAction NO-UNDO
    FIELD paramWidget     AS HANDLE
    FIELD paramName       AS CHARACTER 
    FIELD paramID         AS INTEGER
    FIELD actionParamName AS CHARACTER 
    FIELD action          AS CHARACTER
    FIELD initializeProc  AS CHARACTER
    FIELD validateProc    AS CHARACTER
    FIELD descriptionProc AS CHARACTER
        INDEX paramWidget IS PRIMARY paramWidget
        INDEX paramName paramName
        INDEX paramID paramID
        INDEX actionParamName actionParamName action
        .
