/* auditfunc.i */

&IF DEFINED(AuditFunctions) EQ 0 &THEN
&Global-define AuditFunctions
FUNCTION fFormatValue RETURNS CHARACTER (iphTable AS HANDLE, ipcField AS CHARACTER, ipiExtent AS INTEGER):
    DEFINE VARIABLE cStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lDataType AS LOGICAL NO-UNDO.
     
    cStr = STRING(iphTable:BUFFER-FIELD(ipcField):BUFFER-VALUE(ipiExtent) ,
                  iphTable:BUFFER-FIELD(ipcField):FORMAT) NO-ERROR.
    /* error raised if invalid format for field value */
    lDataType = IF iphTable:BUFFER-FIELD(ipcField):DATA-TYPE EQ "CHARACTER" THEN TRUE ELSE FALSE NO-ERROR .
    IF ERROR-STATUS:NUM-MESSAGES NE 0 OR lDataType THEN 
    cStr = iphTable:BUFFER-FIELD(ipcField):BUFFER-VALUE(ipiExtent) NO-ERROR .
   
    cStr = LEFT-TRIM(TRIM(cStr)).
    
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
