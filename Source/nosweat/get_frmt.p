/* get_frmt.p */

DEFINE INPUT PARAMETER m-file-name AS CHARACTER.
DEFINE INPUT PARAMETER m-field-name AS CHARACTER.
DEFINE OUTPUT PARAMETER m-format AS CHARACTER.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

IF INDEX(m-field-name,"[") NE 0 THEN
m-field-name = SUBSTR(m-field-name,1,INDEX(m-field-name,"[") - 1).
FIND dictdb._file WHERE dictdb._file._file-name = m-file-name NO-LOCK NO-ERROR.
IF AVAILABLE dictdb._file THEN
DO:
  FIND dictdb._field OF dictdb._file
      WHERE dictdb._field._field-name = m-field-name NO-LOCK NO-ERROR.
  IF AVAILABLE dictdb._field THEN
  ASSIGN m-format = dictdb._field._format.
  ELSE
  MESSAGE "Program 'get_frmt' could not find" SKIP(1)
    "Database:" LDBNAME("dictdb") SKIP
    "File:" m-file-name SKIP
    "Field:" m-field-name 
      VIEW-AS ALERT-BOX ERROR.
END.
ELSE
MESSAGE "Program 'get_frmt could not find" SKIP(1)
  "Database:" LDBNAME("dictdb") SKIP
  "File:" m-file-name SKIP
    VIEW-AS ALERT-BOX ERROR.
ldummy = SESSION:SET-WAIT-STATE("").
