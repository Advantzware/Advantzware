/* pGetDynParamValue.i - rstark - 2.12.2019 */

PROCEDURE pGetDynParamValue:
    DEFINE INPUT PARAMETER ipiSubjectID    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPrgmName     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiParamValueID AS INTEGER   NO-UNDO.
    
    IF ipcUserID   EQ "" THEN ipcUserID   = "_default".
    IF ipcPrgmName EQ "" THEN ipcPrgmName = "dynSubjct.".
    FIND FIRST dynParamValue NO-LOCK
         WHERE dynParamValue.subjectID    EQ ipiSubjectID
           AND dynParamValue.user-id      EQ ipcUserID
           AND dynParamValue.prgmName     EQ ipcPrgmName
           AND dynParamValue.paramValueID EQ ipiParamValueID
         NO-ERROR.
    IF NOT AVAILABLE dynParamValue THEN
    RUN pSetDynParamValue (ipiSubjectID, ipcUserID, ipcPrgmName, ipiParamValueID).

END PROCEDURE.
