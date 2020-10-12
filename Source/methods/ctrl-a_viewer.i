/* ctrl-a_viewer.i - rstark - 7.9.2019 */

&IF DEFINED(sdQuery) EQ 0 &THEN
&Scoped-define sdQuery external_tables
&ENDIF

ON CTRL-A OF FRAME {&FRAME-NAME}
DO:
    DEFINE VARIABLE cTable AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hTable AS HANDLE    NO-UNDO.

    RUN AOA/AuditTable.w (QUERY {&sdQuery}:HANDLE, ?, OUTPUT cTable, OUTPUT hTable).
    IF VALID-HANDLE(hTable) THEN
    RUN system/CallAudit.p (cTable, hTable, "Viewer", PROGRAM-NAME(1)).
END.
