DEF VAR cAuditExceptionList AS CHAR NO-UNDO.

ASSIGN 
    cAuditExceptionList = "dynParamValue,report,tag,Task,taskEmail,taskResult,user-print".

FOR EACH _file NO-LOCK WHERE _file._Tbl-type EQ "T":
    IF CAN-FIND(FIRST AuditTbl WHERE 
        AuditTbl.AuditTable EQ _file._file-name) THEN NEXT.
    IF CAN-DO(cAuditExceptionList,_file._file-name) THEN NEXT.
    IF NOT CAN-FIND(FIRST _field OF _file WHERE 
        _field._field-name EQ "rec_key") THEN NEXT.
    IF NOT CAN-FIND(FIRST _file-trig WHERE 
        _file-trig._file-recid EQ recid(_file) AND 
        _file-trig._event EQ "create") THEN NEXT.

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
END.
FOR EACH AuditTbl NO-LOCK:
    FIND _file WHERE
        _file._file-name EQ AuditTbl.AuditTable
        NO-ERROR.
    IF AVAIL AuditTbl THEN DO:
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
END.
OUTPUT TO N:\Build\allfiles.SQL.
FOR EACH _file WHERE _file._file-num GT 0 AND _file._file-num LT 32000 NO-LOCK:
     PUT UNFORMATTED 'grant select on pub."' + _file._file-name + '" to public ~;' + CHR(10).
END.
PUT UNFORMATTED 'grant all on pub.dmiTrans to public ~;' + CHR(10).
PUT UNFORMATTED 'grant all on pub.job-mch to public ~;' + CHR(10).
PUT UNFORMATTED 'COMMIT;' + CHR(10).
output close.

