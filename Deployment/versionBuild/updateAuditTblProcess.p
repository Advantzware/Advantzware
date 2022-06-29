DEF VAR cAuditExceptionList AS CHAR NO-UNDO.

ASSIGN 
    cAuditExceptionList = "dynParamValue,report,tag,Task,taskEmail,taskResult,user-print".

FOR EACH _file NO-LOCK WHERE _file._Tbl-type EQ "T":

    /* Skip if auditTbl record already exists */
    IF CAN-FIND(FIRST AuditTbl WHERE 
        AuditTbl.AuditTable EQ _file._file-name) THEN NEXT.

    /* Skip if auditTbl is in the exception list */
    IF CAN-DO(cAuditExceptionList,_file._file-name) THEN NEXT.
    
    /* Skip if the base table doesn't have a rec_key field */
    IF NOT CAN-FIND(FIRST _field OF _file WHERE 
        _field._field-name EQ "rec_key") THEN NEXT.

    /* Skip if the base table doesn't have any triggers */
    IF NOT CAN-FIND(
        FIRST _file-trig WHERE 
            _file-trig._file-recid EQ recid(_file) AND 
            (_file-trig._event EQ "create" OR
             _file-trig._event EQ "write" OR
             _file-trig._event EQ "delete")
        )
        THEN NEXT.

    /* Otherwise create an auditTbl record */
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

/* Now do essentially the same for auditFld records */
FOR EACH AuditTbl NO-LOCK:
    FIND _file WHERE
        _file._file-name EQ AuditTbl.AuditTable
        NO-ERROR.
    IF AVAIL _File THEN DO:
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

/* This little piece has nothing to do with audit tables, but is needed for SQL permissions later */
DO:
    OUTPUT TO N:\Build\allfiles.SQL.
    FOR EACH _file WHERE _file._file-num GT 0 AND _file._file-num LT 32000 NO-LOCK:
        PUT UNFORMATTED 'GRANT SELECT ON PUB."' + _file._file-name + '" TO PUBLIC ~;' + CHR(10).
    END.
    PUT UNFORMATTED 'GRANT ALL ON PUB.dmiTrans TO amsuser ~;' + CHR(10).
    PUT UNFORMATTED 'GRANT ALL ON PUB.job-mch TO amsuser ~;' + CHR(10).
    PUT UNFORMATTED 'COMMIT;' + CHR(10).
    OUTPUT CLOSE.
END.

