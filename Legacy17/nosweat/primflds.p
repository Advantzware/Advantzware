/* primflds.p */

DEFINE INPUT PARAMETER m_file_name AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER m_prim_flds AS CHARACTER NO-UNDO.

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

FIND dictdb._file WHERE dictdb._file._file-name = m_file_name NO-LOCK.
FIND dictdb._index WHERE RECID(dictdb._index) = dictdb._file._prime-index NO-LOCK.
FOR EACH dictdb._index-field OF dictdb._index NO-LOCK:
  FIND dictdb._field
      WHERE RECID(dictdb._field) = dictdb._index-field._field-recid NO-LOCK.
  m_prim_flds = IF m_prim_flds = "" THEN dictdb._field._field-name
                ELSE m_prim_flds + "," + dictdb._field._field-name.
END.
ldummy = SESSION:SET-WAIT-STATE("").
