/* auditfunc.i */

&IF DEFINED(AuditFunctions) EQ 0 &THEN
&Global-define AuditFunctions
FUNCTION fFormatValue RETURNS CHARACTER (iphTable AS HANDLE, ipcField AS CHARACTER, ipiExtent AS INTEGER):
    DEFINE VARIABLE cStr AS CHARACTER NO-UNDO.
    
    IF iphTable:ROWID NE ? THEN DO:
        cStr = STRING(iphTable:BUFFER-FIELD(ipcField):BUFFER-VALUE(ipiExtent),
                      iphTable:BUFFER-FIELD(ipcField):FORMAT) NO-ERROR.
        /* error raised if invalid format for field value */
        IF ERROR-STATUS:NUM-MESSAGES NE 0 OR iphTable:BUFFER-FIELD(ipcField):DATA-TYPE EQ "CHARACTER" THEN 
        cStr = iphTable:BUFFER-FIELD(ipcField):BUFFER-VALUE(ipiExtent).
        /* cannot exceed index field limit of 1970 bytes */
        cStr = LEFT-TRIM(TRIM(SUBSTRING(cStr,1,1970))).
        IF cStr EQ ? THEN cStr = "".
    END. /* if rowid */
    
    RETURN cStr.
END FUNCTION.

FUNCTION fAuditKey RETURNS CHARACTER (iphTable AS HANDLE, ipcIdxFields AS CHARACTER):
    DEFINE VARIABLE cAuditKey AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
    
    DO idx = 1 TO NUM-ENTRIES(ipcIdxFields):
        cAuditKey = cAuditKey + fFormatValue(iphTable, ENTRY(idx,ipcIdxFields), 0) + "|".
    END. /* do idx */
    cAuditKey = TRIM(cAuditKey,"|").
    
    RETURN cAuditKey.
END FUNCTION.
&ENDIF
