/* dynRunBusinessLogicDefs.i - rstark - 4.10.2019 */

&IF DEFINED(ttTempTable) EQ 0 &THEN
&Scoped-define ttTempTable ttTempTable
&ENDIF

DEFINE VARIABLE hAppServerBin AS HANDLE NO-UNDO.

RUN AOA/appServer/aoaBin.p PERSISTENT SET hAppServerBin.
SESSION:ADD-SUPER-PROCEDURE (hAppServerBin).

FUNCTION fGetTableHandle RETURNS HANDLE ():
    RETURN TEMP-TABLE {&ttTempTable}:HANDLE.
END FUNCTION.

FUNCTION fGetDynParamValue RETURNS CHARACTER (ipcField AS CHARACTER):
    DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx          AS INTEGER   NO-UNDO.

    IF AVAILABLE dynParamValue THEN
    DO idx = 1 TO EXTENT(dynParamValue.paramName):
        IF dynParamValue.paramName[idx] EQ "" THEN LEAVE.
        IF TRIM(dynParamValue.paramName[idx]) EQ ipcField THEN DO:
            cReturnValue = dynParamValue.paramValue[idx].
            LEAVE.
        END. /* found parameter */
    END. /* do idx */

    RETURN cReturnValue.
END FUNCTION.

PROCEDURE pRunBusinessLogic:
    DEFINE INPUT  PARAMETER iprRowID     AS ROWID  NO-UNDO.
    DEFINE OUTPUT PARAMETER ophTempTable AS HANDLE NO-UNDO.
    
    FIND FIRST dynParamValue NO-LOCK
         WHERE ROWID(dynParamValue) EQ iprRowID
         NO-ERROR.
    IF AVAILABLE dynParamValue THEN DO:
        RUN pAssignParamVariables.
        RUN pBusinessLogic.
    END. /* if avail */    
    ophTempTable = fGetTableHandle().
END PROCEDURE.
