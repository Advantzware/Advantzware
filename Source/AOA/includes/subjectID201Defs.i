/* subjectID201Defs.i - auto generated 04.14.2022 @  1:11:52 am */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllRMItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartRMItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartRMItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndRMItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndRMItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDeleteZeroQty AS LOGICAL NO-UNDO.
DEFINE VARIABLE lDeleteNegativeQty AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cLocation = DYNAMIC-FUNCTION("fGetDynParamValue","location")
        lAllRMItems = DYNAMIC-FUNCTION("fGetDynParamValue","allRMItems") EQ "YES"
        cStartRMItem = DYNAMIC-FUNCTION("fGetDynParamValue","startRMItem")
        cStartRMItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startRMItemDescriptn")
        cEndRMItem = DYNAMIC-FUNCTION("fGetDynParamValue","endRMItem")
        cEndRMItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endRMItemDescriptn")
        lDeleteZeroQty = DYNAMIC-FUNCTION("fGetDynParamValue","DeleteZeroQty") EQ "YES"
        lDeleteNegativeQty = DYNAMIC-FUNCTION("fGetDynParamValue","DeleteNegativeQty") EQ "YES"
        .
END PROCEDURE.
