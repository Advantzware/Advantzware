/* fld_list.p */

DEFINE INPUT PARAMETER m_file_name AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER m_field_list AS CHARACTER NO-UNDO.

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.

FIND dictdb._file WHERE dictdb._file._file-name = m_file_name NO-LOCK.
FOR EACH dictdb._field OF dictdb._file NO-LOCK:
  j = IF dictdb._field._extent = 0 THEN 1
      ELSE dictdb._field._extent.
  DO i = 1 TO j:
    m_field_list = IF m_field_list = "" THEN dictdb._field._field-name
                   ELSE m_field_list + "," + dictdb._field._field-name.
    IF dictdb._field._extent NE 0 THEN
    m_field_list = m_field_list + "[" + STRING(i) + "]".
  END.
END.
ldummy = SESSION:SET-WAIT-STATE("").
