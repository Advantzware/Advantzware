/* dynRunBusinessLogicDefs.i - rstark - 4.10.2019 */

&IF DEFINED(ttTempTable) EQ 0 &THEN
&Scoped-define ttTempTable ttTempTable
&ENDIF

DEFINE VARIABLE cProgressBar  AS CHARACTER NO-UNDO.
DEFINE VARIABLE hAppServerBin AS HANDLE    NO-UNDO.
DEFINE VARIABLE lProgressBar  AS LOGICAL   NO-UNDO.

RUN AOA/appServer/aoaBin.p PERSISTENT SET hAppServerBin.
SESSION:ADD-SUPER-PROCEDURE (hAppServerBin).

&IF DEFINED(fGetTableHandle) EQ 0 &THEN
FUNCTION fGetTableHandle RETURNS HANDLE ():
    RETURN TEMP-TABLE {&ttTempTable}:HANDLE.
END FUNCTION.
&ENDIF

{AOA/includes/fGetDynParamValue.i}

PROCEDURE pRunBusinessLogic:
    DEFINE INPUT  PARAMETER iprRowID       AS ROWID   NO-UNDO.
    DEFINE INPUT  PARAMETER iplProgressBar AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER ophTempTable   AS HANDLE  NO-UNDO.
    
    lProgressBar = iplProgressBar.
    FIND FIRST dynParamValue NO-LOCK
         WHERE ROWID(dynParamValue) EQ iprRowID
         NO-ERROR.
    IF AVAILABLE dynParamValue THEN DO:
        cProgressBar = dynParamValue.paramTitle.
        RUN pAssignParamVariables.
        RUN pBusinessLogic.
    END. /* if avail */    
    ophTempTable = fGetTableHandle().
    DELETE PROCEDURE hAppServerBin.
    IF lProgressBar THEN
    RUN spProgressBar (?, ?, 100).
END PROCEDURE.

PROCEDURE pDeleteProcedure:
    IF VALID-HANDLE(hAppServerBin) THEN
    DELETE PROCEDURE hAppServerBin.
END PROCEDURE.
