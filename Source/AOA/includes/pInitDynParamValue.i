/* pInitDynParamValue.i - rstark - 10.30.2019 */

PROCEDURE pInitDynParamValue:
    DEFINE INPUT PARAMETER ipiSubjectID    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPrgmName     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiParamValueID AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcParamList    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcParamValue   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx AS INTEGER NO-UNDO.
    
    RUN pGetDynParamValue (ipiSubjectID, ipcUserID, ipcPrgmName, ipiParamValueID).
    IF NOT AVAILABLE dynParamValue THEN RETURN.
    DO TRANSACTION:
        FIND CURRENT dynParamValue EXCLUSIVE-LOCK.
        DO idx = 1 TO NUM-ENTRIES(ipcParamList,"|"):
            DO jdx = 1 TO EXTENT(dynParamValue.paramName):
                IF dynParamValue.paramName[jdx] EQ "" THEN LEAVE.
                IF dynParamValue.paramName[jdx] EQ ENTRY(idx,ipcParamList,"|") THEN
                dynParamValue.paramValue[jdx] = ENTRY(idx,ipcParamValue,"|").
            END. /* do jdx */
        END. /* do idx */
        RELEASE dynParamValue.
    END. /* do trans */

END PROCEDURE.
