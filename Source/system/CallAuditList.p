/* CallAuditList.p - rstark - 10.13.2020 */

&SCOPED-DEFINE AuditTableList

DEFINE INPUT PARAMETER ipcTable       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcType        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcProgramName AS CHARACTER NO-UNDO.

DEFINE VARIABLE cIdxFlds AS CHARACTER NO-UNDO.

{methods/auditfunc.i}

ipcProgramName = ENTRY(NUM-ENTRIES(ipcProgramName," "),ipcProgramName," ").
RUN nosweat/primflds.p (ipcTable, OUTPUT cIdxFlds).

{system/Audit.w}
