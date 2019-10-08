/* pSessionAuditKey.i - rstark - 7.9.2019 */

/* NOTES:                                                               */
/* used in methods/template/viewer.i in procedure local-row-available   */
/* may need to add {methods/auditfunc.i} if audit functions not present */
/* if query external_tables not present, provide in scop-def query name */
/* need to provide scop-def program name if cCuePrgmName not present    */

&IF DEFINED(sdQuery) EQ 0 &THEN
&Scoped-define sdQuery external_tables
&ENDIF

&IF DEFINED(sdPrgmName) EQ 0 &THEN
&Scoped-define sdPrgmName cCuePrgmName
&ENDIF

PROCEDURE pSessionAuditKey:
    DEFINE VARIABLE cIdxFields    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSessionParam AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSessionValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTable        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hQuery        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTable        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.
    
    /* set session values to use in audit field lookup (CTRL-F) */
    hQuery = QUERY {&sdQuery}:HANDLE.
    DO idx = 1 TO hQuery:NUM-BUFFERS:
        ASSIGN
            cTable = hQuery:GET-BUFFER-HANDLE(idx):NAME
            hTable = hQuery:GET-BUFFER-HANDLE(idx):HANDLE
            .
        RUN nosweat/primFlds.p (cTable, OUTPUT cIdxFields).
        ASSIGN
            cSessionParam = {&sdPrgmName} + "|" + cTable
            cSessionValue = fAuditKey(hTable, cIdxFields)
            .
        RUN spSetSessionParam (cSessionParam, cSessionValue).
    END. /* do idx */
END PROCEDURE.
