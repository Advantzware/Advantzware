/* auditTrigProcs.p - rstark - 5.20.2021 */

&IF "{1}" NE "" &THEN
DEFINE TEMP-TABLE old-{&TABLENAME} LIKE {&TABLENAME}.
CREATE old-{&TABLENAME}.
&ENDIF

PROCEDURE pAudit{1}:
    FIND FIRST AuditTbl NO-LOCK 
         WHERE AuditTbl.AuditTable EQ "{&TABLENAME}"
         NO-ERROR.
    IF AVAILABLE AuditTbl THEN DO: 
        &IF "{&ACTION}" EQ "UPDATE" &THEN
        IF old-{&TABLENAME}.rec_key NE "" THEN DO:
            /* update */
            IF AuditTbl.AuditUpdate THEN DO:
                hTable[2] = BUFFER old-{&TABLENAME}:HANDLE.
                RUN pCreateAuditHdr{1} ("UPDATE").
                RUN pAuditDetail{1} ("UPDATE").
            END. /* if can-find */
        END. /* if old-rec_key */
        ELSE IF AuditTbl.AuditCreate THEN DO:
            /* create */
            RUN pCreateAuditHdr{1} ("CREATE").
            RUN pAuditDetail{1} ("CREATE").
        END. /* else */
        &ELSE
        IF AuditTbl.AuditDelete THEN DO:
            /* delete */
            RUN pCreateAuditHdr{1} ("DELETE").
            RUN pAuditDetail{1} ("DELETE").
        END.
        &ENDIF
    END. /* avail audittbl */
    // clear error-status so ADM1 does not fire adm-show-errors
    ELSE
    ERROR-STATUS:ERROR = NO.
END PROCEDURE.

PROCEDURE pAuditDetail{1}:
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFieldDiffs   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdBufferField AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cFormatField  AS CHARACTER NO-UNDO.
    
    /* get primary index fields */
    RUN nosweat/primflds.p ("{&TABLENAME}", OUTPUT cIdxFields).

    ASSIGN
        hTable[1]         = BUFFER {&TABLENAME}:HANDLE
        AuditHdr.AuditKey = fAuditKey(hTable[1],cIdxFields)
        .
    
    IF ipcType EQ "UPDATE" THEN DO: 
        &IF "{&ACTION}" EQ "UPDATE" &THEN
            BUFFER-COMPARE old-{&TABLENAME} TO {&TABLENAME} SAVE cFieldDiffs NO-LOBS.
        &ENDIF
        
        /* Create Audit records for all the primary index fields */
        DO iAuditIdx = 1 TO NUM-ENTRIES(cIdxFields):
            hdBufferField = hTable[1]:BUFFER-FIELD(ENTRY(iAuditIdx, cIdxFields)) NO-ERROR.
            IF NOT VALID-HANDLE(hdBufferField) THEN
                NEXT.

            iExtentBase = IF hdBufferField:EXTENT GT 0 THEN 1 ELSE 0.
            
            DO iExtent = iExtentBase TO hdBufferField:EXTENT:
                RUN pCreateAuditDtl{1} (hdBufferField:NAME).
                ASSIGN 
                    AuditDtl.AuditBeforeValue = fFormatValue(hTable[2], hTable[2]:BUFFER-FIELD(ENTRY(iAuditIdx, cIdxFields)):NAME, iExtent)
                    AuditDtl.AuditAfterValue  = fFormatValue(hTable[1], hdBufferField:NAME, iExtent)
                    .
            END.    
        END.
        
        /* Create Audit records for fields that have been modified */
        IF cFieldDiffs NE "" THEN DO:
            DO iAuditIdx = 1 TO NUM-ENTRIES(cFieldDiffs):
                /* Skip the fields already created from index fields */
                IF CAN-DO(cIdxFields, ENTRY(iAuditIdx, cFieldDiffs)) THEN
                    NEXT.
                
                IF NOT DYNAMIC-FUNCTION("sfAuditField", AuditTbl.AuditTable, ENTRY(iAuditIdx, cFieldDiffs)) THEN
                    NEXT.
                        
                hdBufferField = hTable[1]:BUFFER-FIELD(ENTRY(iAuditIdx, cFieldDiffs)) NO-ERROR.
                
                IF NOT VALID-HANDLE(hdBufferField) THEN
                    NEXT.
    
                iExtentBase = IF hdBufferField:EXTENT GT 0 THEN 1 ELSE 0.
                
                DO iExtent = iExtentBase TO hdBufferField:EXTENT:
                    /* Still have to validate for equality match as BUFFER-COMPARE cannot fetch differences in arrays at element level  */
                    IF hdBufferField:BUFFER-VALUE(iExtent) EQ hTable[2]:BUFFER-FIELD(ENTRY(iAuditIdx, cFieldDiffs)):BUFFER-VALUE(iExtent) THEN
                        NEXT.
    
                    RUN pCreateAuditDtl{1} (hdBufferField:NAME).
    
                    ASSIGN 
                        AuditDtl.AuditBeforeValue = fFormatValue(hTable[2], hTable[2]:BUFFER-FIELD(ENTRY(iAuditIdx, cFieldDiffs)):NAME, iExtent)
                        AuditDtl.AuditAfterValue  = fFormatValue(hTable[1], hdBufferField:NAME, iExtent)
                        .
                END.    
            END.
        END.        
    END.    
    ELSE DO iAuditIdx = 1 TO hTable[1]:NUM-FIELDS:    
        iExtentBase = IF hTable[1]:BUFFER-FIELD(iAuditIdx):EXTENT GT 0 THEN 1 ELSE 0.
        IF hTable[1]:BUFFER-FIELD(iAuditIdx):DATA-TYPE EQ "CLOB" OR
           hTable[1]:BUFFER-FIELD(iAuditIdx):DATA-TYPE EQ "BLOB" THEN
        NEXT.
        DO iExtent = iExtentBase TO hTable[1]:BUFFER-FIELD(iAuditIdx):EXTENT:
            CASE ipcType:
                WHEN "CREATE" THEN
                IF CAN-DO(cIdxFields,hTable[1]:BUFFER-FIELD(iAuditIdx):NAME) THEN DO: 
                    RUN pCreateAuditDtl{1} (hTable[1]:BUFFER-FIELD(iAuditIdx):NAME).
                    AuditDtl.AuditAfterValue = fFormatValue(hTable[1], hTable[1]:BUFFER-FIELD(iAuditIdx):NAME, iExtent).
                END. /* if before and after difference or primary index field */
                WHEN "DELETE" THEN DO:
                    /* If field value is equal to initial value of the field then skip it unless it is a primary index field */
                    IF NOT CAN-DO(cIdxFields,hTable[1]:BUFFER-FIELD(iAuditIdx):NAME) THEN DO:
                        /* Use a variable to avoid errors that might generate while converting the data into format that does not support */
                        cFormatField = STRING(hTable[1]:BUFFER-FIELD(iAuditIdx):BUFFER-VALUE(iExtent), hTable[1]:BUFFER-FIELD(iAuditIdx):FORMAT) NO-ERROR.
        
                        IF hTable[1]:BUFFER-FIELD(iAuditIdx):DATA-TYPE EQ "DATE" AND cFormatField EQ ? THEN
                            cFormatField = "".
                            
                        IF hTable[1]:BUFFER-FIELD(iAuditIdx):INITIAL EQ cFormatField THEN
                            NEXT.
                    END.
                          
                    RUN pCreateAuditDtl{1} (hTable[1]:BUFFER-FIELD(iAuditIdx):NAME).
                    AuditDtl.AuditBeforeValue = fFormatValue(hTable[1], hTable[1]:BUFFER-FIELD(iAuditIdx):NAME, iExtent).
                END.
            END CASE.
        END. /* do iextent */
    END. /* do */
END PROCEDURE.

PROCEDURE pCreateAuditDtl{1}:
    DEFINE INPUT  PARAMETER ipcFieldName AS CHARACTER NO-UNDO.
    
    CREATE AuditDtl.
    ASSIGN 
        AuditDtl.AuditID       = AuditHdr.AuditID
        AuditDtl.AuditField    = hTable[1]:BUFFER-FIELD(ipcFieldName):NAME
        AuditDtl.AuditExtent   = iExtent
        AuditDtl.AuditIdxField = CAN-DO(cIdxFields,hTable[1]:BUFFER-FIELD(ipcFieldName):NAME)
        .
END PROCEDURE.

PROCEDURE pCreateAuditHdr{1}:
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cStack AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx    AS INTEGER   NO-UNDO INITIAL 2.

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
    IF AuditTbl.AuditStack THEN DO:
        DO WHILE TRUE:
            ASSIGN 
                cStack = cStack + PROGRAM-NAME(idx) + ","
                idx = idx + 1
                .
            IF PROGRAM-NAME(idx) EQ ? THEN LEAVE. 
        END. /* do while true */
        cStack = TRIM(cStack,",").
        FIND FIRST AuditStack NO-LOCK
             WHERE AuditStack.AuditStack EQ cStack
             NO-ERROR.
        IF NOT AVAILABLE AuditStack THEN DO:
            CREATE AuditStack.
            ASSIGN
                AuditStack.AuditStackID = NEXT-VALUE(stack_trace) 
                AuditStack.AuditStack   = cStack
                .
        END. /* not avail */
        AuditHdr.AuditStackID  = AuditStack.AuditStackID.
    END. /* if auditstack */
END PROCEDURE.
