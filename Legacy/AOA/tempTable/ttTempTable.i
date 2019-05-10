/* ttTempTable.i - rstark - 4.10.2019 */

DEFINE TEMP-TABLE ttTempTable NO-UNDO
    FIELD fieldName AS CHARACTER LABEL "Field Name" FORMAT "x(8)"
        INDEX fieldName IS PRIMARY fieldName
        .
