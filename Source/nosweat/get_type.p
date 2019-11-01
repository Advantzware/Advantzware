/* get_type.p */

DEFINE INPUT  PARAMETER ipcTableName AS CHARACTER.
DEFINE INPUT  PARAMETER ipcFieldName AS CHARACTER.
DEFINE OUTPUT PARAMETER opcDataType AS CHARACTER.

IF INDEX(ipcFieldName,"[") NE 0 THEN
ipcFieldName = SUBSTR(ipcFieldName,1,INDEX(ipcFieldName,"[") - 1).
FIND FIRST dictdb._file NO-LOCK
     WHERE dictdb._file._file-name EQ ipcTableName
     NO-ERROR.
IF AVAILABLE dictdb._file THEN DO:
    FIND FIRST dictdb._field OF dictdb._file NO-LOCK
         WHERE dictdb._field._field-name EQ ipcFieldName
         NO-ERROR.
    IF AVAILABLE dictdb._field THEN
    opcDataType = IF dictdb._field._data-type EQ "CHARACTER" THEN "STRING"
                ELSE dictdb._field._data-type.
    ELSE
    MESSAGE "Program 'get_type' could not find" SKIP(1)
        "Database:" LDBNAME("dictdb") SKIP
        "File:" ipcTableName SKIP
        "Field:" ipcFieldName 
            VIEW-AS ALERT-BOX ERROR.
END.
ELSE
MESSAGE "Program 'get_type' could not find" SKIP(1)
    "Database:" LDBNAME("dictdb") SKIP
    "File:" ipcTableName SKIP
        VIEW-AS ALERT-BOX ERROR.
SESSION:SET-WAIT-STATE("").
