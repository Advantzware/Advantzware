/* CallAudit.p */

&SCOPED-DEFINE AuditHistory

DEFINE INPUT PARAMETER ipcTable       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iphTable       AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER ipcType        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcProgramName AS CHARACTER NO-UNDO.

DEFINE VARIABLE cIdxFlds AS CHARACTER NO-UNDO.

{methods/auditfunc.i}

ipcProgramName = ENTRY(NUM-ENTRIES(ipcProgramName," "),ipcProgramName," ").
IF NOT VALID-HANDLE(iphTable) THEN DO:
    MESSAGE
        "Invalid Table Handle, Unable to Run Audit History" SKIP(1)
        "Called from " ipcType ipcProgramName
    VIEW-AS ALERT-BOX.
    RETURN.
END. /* not valid-handle */

RUN nosweat/primflds.p (ipcTable, OUTPUT cIdxFlds).

{system/Audit.w}
