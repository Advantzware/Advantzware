/* fGetDynParamValue.i */

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
