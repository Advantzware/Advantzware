/* subjectID120Defs.i - auto generated 07.06.2021 @  7:17:22 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllRMItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartRMItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartRMItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndRMItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndRMItemDescriptn AS CHARACTER NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lAllRMItems = DYNAMIC-FUNCTION("fGetDynParamValue","allRMItems") EQ "YES"
        cStartRMItem = DYNAMIC-FUNCTION("fGetDynParamValue","startRMItem")
        cStartRMItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startRMItemDescriptn")
        cEndRMItem = DYNAMIC-FUNCTION("fGetDynParamValue","endRMItem")
        cEndRMItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endRMItemDescriptn")
        .
END PROCEDURE.
