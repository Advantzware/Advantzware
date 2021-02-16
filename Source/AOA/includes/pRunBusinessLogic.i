/* pRunBusinessLogic.i - rstark - 4.9.2019 */

DEFINE VARIABLE hBusinessLogic      AS HANDLE NO-UNDO.
DEFINE VARIABLE hBusinessLogicTable AS HANDLE NO-UNDO.

PROCEDURE pRunBusinessLogic:
    DEFINE INPUT  PARAMETER iplProgressBar AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER ophQuery       AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER oplOK          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcError       AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cQueryStr    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hQuery       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lProgressBar AS LOGICAL   NO-UNDO.
    
    RUN VALUE(dynSubject.businessLogic) PERSISTENT SET hBusinessLogic.
    RUN pRunBusinessLogic IN hBusinessLogic (ROWID(dynParamValue), iplProgressBar, OUTPUT hBusinessLogicTable).
    CREATE QUERY hQuery.
    hQuery:ADD-BUFFER(hBusinessLogicTable:DEFAULT-BUFFER-HANDLE).
    cQueryStr = "FOR EACH " + hBusinessLogicTable:NAME.
    /* append sort by option to query */
    RUN AOA/dynSortBy.p (BUFFER dynParamValue, INPUT-OUTPUT cQueryStr).
    oplOK = hQuery:QUERY-PREPARE(cQueryStr) NO-ERROR.
    IF oplOK THEN
    ophQuery = hQuery:HANDLE.
    ELSE
    DO idx = 1 TO ERROR-STATUS:NUM-MESSAGES:
        opcError = opcError + ERROR-STATUS:GET-MESSAGE(idx) + CHR(10).
    END. /* do idx */
    RUN pDeleteProcedure IN hBusinessLogic.
END PROCEDURE.
