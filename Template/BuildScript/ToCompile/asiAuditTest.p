DEF OUTPUT PARAMETER oplAuditLicensed AS LOG NO-UNDO.

FIND FIRST module NO-LOCK WHERE 
    module.module = "audit." OR
    module.module = "audit"
    NO-ERROR.
IF AVAIL module THEN ASSIGN 
    oplAuditLicensed = module.is-Used. 
