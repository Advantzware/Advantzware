/* filelist.p */

DEFINE OUTPUT PARAMETER m_file_list AS CHARACTER NO-UNDO.

FOR EACH dictdb._file
    WHERE dictdb._file._Tbl-type EQ "t"
    NO-LOCK
    :
    m_file_list = IF m_file_list = "" THEN dictdb._file._file-name
                  ELSE m_file_list + "," + dictdb._file._file-name.
END.
SESSION:SET-WAIT-STATE("").
