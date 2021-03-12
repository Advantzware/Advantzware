/* indxlist.p */

DEFINE INPUT  PARAMETER m_file_name  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER m_index_list AS CHARACTER NO-UNDO.

FIND FIRST dictdb._file NO-LOCK
     WHERE dictdb._file._file-name EQ m_file_name
     NO-ERROR.
IF AVAILABLE dictdb._file THEN
FOR EACH dictdb._index NO-LOCK
    WHERE dictdb._index._file-recid EQ RECID(dictdb._file)
    :
    IF dictdb._index._index-name BEGINS "wi-" OR
       dictdb._index._index-name EQ "si-rec_key" THEN
    NEXT.
    m_index_list = m_index_list + dictdb._index._index-name + ",".
END.
m_index_list = TRIM(m_index_list,",").
SESSION:SET-WAIT-STATE("").
