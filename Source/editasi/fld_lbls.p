/* fld_lbls.p */

DEFINE INPUT PARAMETER m_file_name AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER m_field_list AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER m_label_list AS CHARACTER NO-UNDO.

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

FIND dictdb._file WHERE dictdb._file._file-name = m_file_name NO-LOCK.
DO i = 1 TO NUM-ENTRIES(m_field_list):
  FIND dictdb._field OF dictdb._file
      WHERE dictdb._field._field-name = ENTRY(i,m_field_list) NO-LOCK.
  m_label_list = IF m_label_list = "" THEN dictdb._field._label
                 ELSE m_label_list + "," + dictdb._field._label.
END.
ldummy = SESSION:SET-WAIT-STATE("").
