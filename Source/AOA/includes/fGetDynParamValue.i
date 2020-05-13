/* fGetDynParamValue.i */

FUNCTION fGetDynParamValue RETURNS CHARACTER (ipcField AS CHARACTER):
    DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx          AS INTEGER   NO-UNDO.

    IF AVAILABLE dynParamValue THEN DO:
        FIND FIRST dynValueParam NO-LOCK
             WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID
               AND dynValueParam.user-id      EQ dynParamValue.user-id
               AND dynValueParam.prgmName     EQ dynParamValue.prgmName
               AND dynValueParam.paramValueID EQ dynParamValue.paramValueID
               AND dynValueParam.paramName    EQ ipcField
             NO-ERROR.
        IF AVAILABLE dynValueParam THEN
        cReturnValue = dynValueParam.paramValue.
        RELEASE dynValueParam.
    END. /* if avail */
/*    /* rstark - remove when depricated */                         */
/*    IF AVAILABLE dynParamValue THEN                               */
/*    DO idx = 1 TO EXTENT(dynParamValue.paramName):                */
/*        IF dynParamValue.paramName[idx] EQ "" THEN LEAVE.         */
/*        IF TRIM(dynParamValue.paramName[idx]) EQ ipcField THEN DO:*/
/*            cReturnValue = dynParamValue.paramValue[idx].         */
/*            LEAVE.                                                */
/*        END. /* found parameter */                                */
/*    END. /* do idx */                                             */

    RETURN cReturnValue.
END FUNCTION.
