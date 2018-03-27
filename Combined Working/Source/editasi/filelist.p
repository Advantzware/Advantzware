/* filelist.p */

DEFINE OUTPUT PARAMETER m_file_list AS CHARACTER NO-UNDO.

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

FOR EACH dictdb._file WHERE NOT dictdb._file._file-name BEGINS "_" 
                        AND NOT dictdb._file._file-name BEGINS "SYS" NO-LOCK:
  m_file_list = IF m_file_list = "" THEN dictdb._file._file-name
                ELSE m_file_list + "," + dictdb._file._file-name.
END.
ldummy = SESSION:SET-WAIT-STATE("").
