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
    
    IF ipcUserID   EQ "" THEN ipcUserID   = "_default".
    IF ipcPrgmName EQ "" THEN ipcPrgmName = "dynSubjct.".
    RUN pGetDynParamValue (ipiSubjectID, ipcUserID, ipcPrgmName, ipiParamValueID).
    IF NOT AVAILABLE dynParamValue THEN RETURN.
    DO TRANSACTION:
        DO idx = 1 TO NUM-ENTRIES(ipcParamList,"|"):
            FIND FIRST dynValueParam EXCLUSIVE-LOCK
                 WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID
                   AND dynValueParam.user-id      EQ dynParamValue.user-id
                   AND dynValueParam.prgmName     EQ dynParamValue.prgmName
                   AND dynValueParam.paramValueID EQ dynParamValue.paramValueID
                   AND dynValueParam.paramName    EQ ENTRY(idx,ipcParamList,"|")
                 NO-ERROR.
            IF AVAILABLE dynValueParam THEN
            dynValueParam.paramValue = ENTRY(idx,ipcParamValue,"|").
            RELEASE dynValueParam.
        END. /* do idx */
    END. /* do trans */
END PROCEDURE.
