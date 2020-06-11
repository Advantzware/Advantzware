DEF OUTPUT PARAMETER oplAuditLicensed AS LOG NO-UNDO.
DEF OUTPUT PARAMETER oplHasTables AS LOG NO-UNDO.

ASSIGN 
    oplAuditLicensed = FALSE.
FIND FIRST module NO-LOCK WHERE 
    module.module = "audit." OR
    module.module = "audit"
    NO-ERROR.
IF AVAIL module THEN ASSIGN 
    oplAuditLicensed = module.is-Used. 

IF CAN-FIND(FIRST _file WHERE _file._file-name EQ "dep-table") THEN ASSIGN 
    oplHasTables = TRUE.
    
OUTPUT TO c:\tmp\auditTbl.d.
FOR EACH auditTbl:
    EXPORT auditTbl.
END.  
OUTPUT CLOSE.


