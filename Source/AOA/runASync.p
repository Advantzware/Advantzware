/* runASync.p - rstark - 6.10.2020 */

/* prowin.exe spawned by tasker.w monitor */

DEFINE VARIABLE hSession AS HANDLE NO-UNDO.
DEFINE VARIABLE rRowID   AS ROWID  NO-UNDO.

ASSIGN
    PROPATH = ENTRY(1,SESSION:PARAMETER,"+")
    rRowID  = TO-ROWID(ENTRY(2,SESSION:PARAMETER,"+"))
    .
RUN system/session.p PERSISTENT SET hSession.
SESSION:ADD-SUPER-PROCEDURE (hSession).

RUN AOA/runTask.p (rRowID).

DELETE PROCEDURE hSession.

QUIT.
