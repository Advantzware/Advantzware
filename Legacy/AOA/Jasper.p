/* Jasper.p - rstark - 2.12.2019 */

DEFINE INPUT PARAMETER ipiSubjectID    AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipcUserID       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcPrgmName     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiParamValueID AS INTEGER   NO-UNDO.

RUN pGetDynParamValue (ipiSubjectID, ipcUserID, ipcPrgmName, ipiParamValueID).
RUN AOA/dynRun.w (ipcPrgmName, ROWID(dynParamValue)).

{AOA/includes/pGetDynParamValue.i}
{AOA/includes/pSetDynParamValue.i "dyn"}
