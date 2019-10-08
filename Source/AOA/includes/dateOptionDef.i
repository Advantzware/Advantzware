/* dateOptionDef.i */

DEFINE VARIABLE hAppSrvBin AS HANDLE NO-UNDO.
DEFINE VARIABLE hContainer AS HANDLE NO-UNDO.

RUN AOA\appServer\aoaBin.p PERSISTENT SET hAppSrvBin.
SESSION:ADD-SUPER-PROCEDURE (hAppSrvBin).
hContainer = THIS-PROCEDURE.

/* function fDateOptions */
{AOA/includes/fDateOptions.i}
/* function fDateOptionValue */
{AOA/includes/fDateOptionValue.i}
