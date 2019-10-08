
DEF INPUT PARAMETER ipcTable AS CHAR NO-UNDO.
DEF INPUT PARAMETER iprTableRow AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER opcUpdUsr AS CHAR NO-UNDO.
def var a as int no-undo.
FIND FIRST dictdb._file WHERE dictdb._file._file-name = ipcTable 
    NO-LOCK NO-ERROR.
 IF AVAIL(dictdb._file) THEN
   a = dictdb._file._file-number .


FOR EACH dictdb._Lock: 
    IF dictdb._Lock._Lock-Usr = ? THEN LEAVE.
    IF dictdb._Lock._Lock-table = a THEN   do:  
      CASE ipcTable:
        WHEN "itemfg" THEN DO:                     
          FIND itemfg where recid(itemfg) = dictdb._lock._lock-recid no-lock no-error.            
          if AVAIL itemfg AND ROWID(itemfg) EQ iprTableRow THEN DO:              
              opcUpdUsr = dictdb._lock._Lock-name + "-" + STRING(dictdb._lock._lock-usr).
              LEAVE.
          END.         
        END.
        WHEN "oe-ord" THEN DO:                     
          FIND oe-ord where recid(oe-ord) = dictdb._lock._lock-recid no-lock no-error.            
          if AVAIL oe-ord AND ROWID(oe-ord) EQ iprTableRow THEN DO:              
              opcUpdUsr = dictdb._lock._Lock-name + "-" + STRING(dictdb._lock._lock-usr).
              LEAVE.
          END.         
        END.
        WHEN "job" THEN DO:                     
          FIND job where recid(job) = dictdb._lock._lock-recid no-lock no-error.            
          if AVAIL job AND ROWID(job) EQ iprTableRow THEN DO:              
              opcUpdUsr = dictdb._lock._Lock-name + "-" + STRING(dictdb._lock._lock-usr).
              LEAVE.
          END.         
        END.
        WHEN "job-hdr" THEN DO:                     
          FIND job-hdr where recid(job-hdr) = dictdb._lock._lock-recid no-lock no-error.            
          if AVAIL job-hdr AND ROWID(job-hdr) EQ iprTableRow THEN DO:              
              opcUpdUsr = dictdb._lock._Lock-name + "-" + STRING(dictdb._lock._lock-usr).
              LEAVE.
          END.         
        END.
      END CASE.
    end.
END.  
