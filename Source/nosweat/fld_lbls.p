/* fld_lbls.p */

DEFINE INPUT  PARAMETER ipcTableName AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcFieldList AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcLabelList AS CHARACTER NO-UNDO.

DEFINE VARIABLE cFieldName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLabel AS CHARACTER NO-UNDO.
DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.

FIND FIRST dictdb._file NO-LOCK
     WHERE dictdb._file._file-name EQ ipcTableName
     NO-ERROR.
IF AVAILABLE dictdb._file THEN
DO idx = 1 TO NUM-ENTRIES(ipcFieldList):
    cFieldName = ENTRY(idx,ipcFieldList).
    IF INDEX(cFieldName,"[") NE 0 THEN
    cFieldName = SUBSTR(cFieldName,1,INDEX(cFieldName,"[") - 1).
    FIND FIRST dictdb._field OF dictdb._file NO-LOCK
         WHERE dictdb._field._field-name EQ cFieldName
         NO-ERROR.
    IF AVAILABLE dictdb._field THEN
    ASSIGN
        cFieldLabel  = IF dictdb._field._label NE ? THEN dictdb._field._label
                       ELSE dictdb._field._field-name
        opcLabelList = opcLabelList + cFieldLabel + ","
        .
END.
opcLabelList = TRIM(opcLabelList,",").
SESSION:SET-WAIT-STATE("").
