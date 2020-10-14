/* ctrl-a_browser.i - rstark - 7.9.2019 */

ON CTRL-A OF {&BROWSE-NAME} IN FRAME {&FRAME-NAME}
DO:
    RUN pCallAudit ("Browser").
END.

PROCEDURE pCallAudit:
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cTable    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hExternal AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTable    AS HANDLE    NO-UNDO.
    
    &IF DEFINED(EXTERNAL-TABLES) NE 0 &THEN
    hExternal = QUERY external_tables:HANDLE.
    &ENDIF

    RUN AOA/AuditTable.w (QUERY {&BROWSE-NAME}:HANDLE, hExternal, ?, OUTPUT cTable, OUTPUT hTable).
    IF VALID-HANDLE(hTable) THEN
    RUN system/CallAudit.p (cTable, hTable, ipcType, PROGRAM-NAME(1)).

END PROCEDURE.
