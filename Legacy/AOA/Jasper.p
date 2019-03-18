/* Jasper.p - rstark - 2.12.2019 */

DEFINE INPUT PARAMETER ipiSubjectID    AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipcUserID       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcPrgmName     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiParamValueID AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER iplParameters   AS LOGICAL   NO-UNDO.

RUN pGetDynParamValue (ipiSubjectID, ipcUserID, ipcPrgmName, ipiParamValueID).
IF AVAILABLE dynParamValue THEN
RUN AOA/dynRun.w (ipcPrgmName, ROWID(dynParamValue), iplParameters).

{AOA/includes/pGetDynParamValue.i}
{AOA/includes/pSetDynParamValue.i "dyn"}
