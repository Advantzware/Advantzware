/* widxlist.p */

DEFINE INPUT PARAMETER m_file_name AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER m_index_list AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER m_field_list AS CHARACTER NO-UNDO.

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

FIND dictdb._file WHERE dictdb._file._file-name = m_file_name NO-LOCK.
FOR EACH dictdb._index WHERE dictdb._index._file-recid = RECID(dictdb._file) NO-LOCK:
  IF NOT dictdb._index._index-name BEGINS "wi-" THEN
  NEXT.
  FIND dictdb._index-field OF dictdb._index NO-LOCK.
  FIND dictdb._field
      WHERE RECID(dictdb._field) = dictdb._index-field._field-recid NO-LOCK.
  ASSIGN
    m_field_list = dictdb._field._field-name
    m_index_list = dictdb._index._index-name.
  LEAVE.
END.
ldummy = SESSION:SET-WAIT-STATE("").
