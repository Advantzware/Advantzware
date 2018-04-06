/* audittrg.i */

DEFINE VARIABLE hTable      AS HANDLE    NO-UNDO EXTENT 2.
DEFINE VARIABLE iAuditIdx   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iExtent     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iExtentBase AS INTEGER   NO-UNDO.
DEFINE VARIABLE cIdxFields  AS CHARACTER NO-UNDO.

{methods/auditfunc.i}

&IF "{&ACTION}" = "UPDATE" &THEN
IF old-{&TABLENAME}.rec_key NE "" THEN DO:
    /* update */
    IF CAN-FIND(FIRST config WHERE CAN-DO(ENTRY(3,config.audit_tables,"|"),"{&TABLENAME}")) THEN DO:
        hTable[2] = BUFFER old-{&TABLENAME}:HANDLE.
        RUN pCreateAuditHdr ("UPDATE").
        RUN pAuditDetail ("UPDATE").
    END. /* if can-find */
END. /* if old-rec_key */
ELSE IF CAN-FIND(FIRST config WHERE CAN-DO(ENTRY(1,config.audit_tables,"|"),"{&TABLENAME}")) THEN DO:
    /* create */
    RUN pCreateAuditHdr ("CREATE").
    RUN pAuditDetail ("CREATE").
END. /* else */
&ELSE
IF CAN-FIND(FIRST config WHERE CAN-DO(ENTRY(2,config.audit_tables,"|"),"{&TABLENAME}")) THEN DO:
    /* delete */
    RUN pCreateAuditHdr ("DELETE").
    RUN pAuditDetail ("DELETE").
END.
&ENDIF

PROCEDURE pAuditDetail:
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    /* get primary index fields */
    RUN nosweat/primflds.p ("{&TABLENAME}", OUTPUT cIdxFields).

    ASSIGN
        hTable[1]         = BUFFER {&TABLENAME}:HANDLE
        AuditHdr.AuditKey = fAuditKey(hTable[1],cIdxFields)
        .
    
    DO iAuditIdx = 1 TO hTable[1]:NUM-FIELDS:
        iExtentBase = IF hTable[1]:BUFFER-FIELD(iAuditIdx):EXTENT GT 0 THEN 1 ELSE 0.
        DO iExtent = iExtentBase TO hTable[1]:BUFFER-FIELD(iAuditIdx):EXTENT:
            CASE ipcType:
                WHEN "CREATE" THEN
                IF CAN-DO(cIdxFields,hTable[1]:BUFFER-FIELD(iAuditIdx):NAME) THEN DO: 
                    RUN pCreateAuditDtl.
                    AuditDtl.AuditAfterValue = fFormatValue(hTable[1], hTable[1]:BUFFER-FIELD(iAuditIdx):NAME, iExtent).
                END. /* if before and after difference or primary index field */
                WHEN "DELETE" THEN DO:
                    RUN pCreateAuditDtl.
                    AuditDtl.AuditBeforeValue = fFormatValue(hTable[1], hTable[1]:BUFFER-FIELD(iAuditIdx):NAME, iExtent).
                END.
                WHEN "UPDATE" THEN 
                IF CAN-DO(cIdxFields,hTable[1]:BUFFER-FIELD(iAuditIdx):NAME) OR
                   hTable[1]:BUFFER-FIELD(iAuditIdx):BUFFER-VALUE(iExtent) NE
                   hTable[2]:BUFFER-FIELD(iAuditIdx):BUFFER-VALUE(iExtent) THEN DO: 
                    RUN pCreateAuditDtl.
                    ASSIGN 
                        AuditDtl.AuditBeforeValue = fFormatValue(hTable[2], hTable[2]:BUFFER-FIELD(iAuditIdx):NAME, iExtent)
                        AuditDtl.AuditAfterValue  = fFormatValue(hTable[1], hTable[1]:BUFFER-FIELD(iAuditIdx):NAME, iExtent)
                        .
                END. /* if before and after difference or primary index field */
            END CASE.
        END. /* do iextent */
    END. /* do */
END PROCEDURE.

PROCEDURE pCreateAuditDtl:
    CREATE AuditDtl.
    ASSIGN 
        AuditDtl.AuditID       = AuditHdr.AuditID
        AuditDtl.AuditField    = hTable[1]:BUFFER-FIELD(iAuditIdx):NAME
        AuditDtl.AuditExtent   = iExtent
        AuditDtl.AuditIdxField = CAN-DO(cIdxFields,hTable[1]:BUFFER-FIELD(iAuditIdx):NAME)
        .
END PROCEDURE.

PROCEDURE pCreateAuditHdr:
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO INITIAL 2.

    CREATE AuditHdr.
    ASSIGN
        AuditHdr.AuditID       = NEXT-VALUE(Audit_Seq,Audit)
        AuditHdr.AuditDateTime = NOW
        AuditHdr.AuditType     = ipcType
        AuditHdr.AuditDB       = "{&DBNAME}"
        AuditHdr.AuditTable    = "{&TABLENAME}"
        AuditHdr.AuditUser     = USERID("ASI")
        AuditHdr.AuditRecKey   = {&TABLENAME}.rec_key
        .
    DO WHILE TRUE:
        ASSIGN 
            AuditHdr.AuditStack = AuditHdr.AuditStack + PROGRAM-NAME(idx) + ","
            idx = idx + 1
            .
        IF PROGRAM-NAME(idx) EQ ? THEN LEAVE. 
    END. /* do while true */
    AuditHdr.AuditStack = TRIM(AuditHdr.AuditStack,",").
END PROCEDURE.
