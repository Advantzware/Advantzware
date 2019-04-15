/* primflds.p */

DEFINE INPUT  PARAMETER ipcFileName AS CHARACTER NO-UNDO.

DEFINE OUTPUT PARAMETER opcPrimFlds AS CHARACTER NO-UNDO.

FIND FIRST ASI._file NO-LOCK
     WHERE ASI._file._file-name EQ ipcFileName.
FIND FIRST ASI._index NO-LOCK
     WHERE RECID(ASI._index) EQ ASI._file._prime-index.
FOR EACH ASI._index-field OF ASI._index NO-LOCK:
  FIND FIRST ASI._field NO-LOCK
       WHERE RECID(ASI._field) EQ ASI._index-field._field-recid.
  opcPrimFlds = opcPrimFlds + ASI._field._field-name + ",".
END. /* each _index */
opcPrimFlds = TRIM(opcPrimFlds,",").
SESSION:SET-WAIT-STATE("").
