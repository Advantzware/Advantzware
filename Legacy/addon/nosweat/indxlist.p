/* indxlist.p */

DEFINE INPUT PARAMETER m_file_name AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER m_index_list AS CHARACTER NO-UNDO.

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

FIND dictdb._file WHERE dictdb._file._file-name = m_file_name NO-LOCK.
FOR EACH dictdb._index WHERE dictdb._index._file-recid = RECID(dictdb._file) NO-LOCK:
  IF dictdb._index._index-name BEGINS "wi-" OR
     dictdb._index._index-name = "si-rec_key" THEN
  NEXT.
  m_index_list = IF m_index_list = "" THEN dictdb._index._index-name
                 ELSE m_index_list + "," + dictdb._index._index-name.
END.
ldummy = SESSION:SET-WAIT-STATE("").
