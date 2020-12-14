DEF VAR cAuditExceptionList AS CHAR NO-UNDO.

CONNECT -db TESTDEVELd -H EC2AMAZ-VE65OC7 -S 2522 -ld ASI -U mark -P mark.
CONNECT -db TESTDEVELa -H EC2AMAZ-VE65OC7 -S 2622 -ld AUDIT.

CREATE ALIAS DICTDB FOR DATABASE asi.
ASSIGN 
    cAuditExceptionList = "dynParamValue,report,tag,Task,taskEmail,taskResult,user-print".

FOR EACH _file NO-LOCK WHERE _file._Tbl-type EQ "T":
    IF CAN-FIND(FIRST AuditTbl WHERE 
        AuditTbl.AuditTable EQ _file._file-name) THEN NEXT.
    IF CAN-FIND(FIRST _field OF _file WHERE 
        _field._field-name EQ "rec_key") EQ NO THEN NEXT.
    IF CAN-FIND(FIRST _file-trig WHERE 
        _file-trig._file-recid EQ recid(_file) AND 
        _file-trig._event EQ "create") EQ NO THEN NEXT.
    IF CAN-DO(cAuditExceptionList,_file._file-name) THEN NEXT.

    IF NOT CAN-FIND(FIRST AuditTbl WHERE 
        AuditTbl.AuditTable EQ _file._file-name) THEN DO:
        CREATE AuditTbl.
        ASSIGN
            AuditTbl.AuditTable  = _file._file-name
            AuditTbl.AuditCreate = NO
            AuditTbl.AuditDelete = NO
            AuditTbl.AuditUpdate = NO
            AuditTbl.AuditStack  = NO
            AuditTbl.expireDays = 0
            AuditTbl.AuditCreateDefault = NO
            AuditTbl.AuditDeleteDefault = NO
            AuditTbl.AuditUpdateDefault = NO
            AuditTbl.AuditStackDefault  = NO
            AuditTbl.expireDaysDefault = 0
            .
    END.    
    FOR EACH _field NO-LOCK OF _file:
        IF NOT CAN-FIND(FIRST AuditFld WHERE 
            AuditFld.AuditTable EQ _file._file-name AND 
            AuditFld.auditField EQ _field._field-name) THEN DO:
            CREATE AuditFld.
            ASSIGN 
                auditFld.auditTable = _file._file-name
                auditFld.auditField = _field._field-name
                auditFld.audit = NO 
                auditFld.auditDefault = NO 
                .
        END.
    END.        
END.
