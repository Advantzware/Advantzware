/* subjectID116Defs.i - */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE lAllFGItem AS Logical NO-UNDO.
DEFINE VARIABLE cStartFGItem AS Character NO-UNDO.
DEFINE VARIABLE cEndFGItem AS Character NO-UNDO.
DEFINE VARIABLE dtStartShipDate AS Date NO-UNDO.
DEFINE VARIABLE dtEndShipDate AS Date NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lAllFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","allFGItems") EQ "YES"
        cStartFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItem")
        cEndFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItem")
        dtStartShipDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startShipDate"))
        dtEndShipDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endShipDate"))
        .
END PROCEDURE.
