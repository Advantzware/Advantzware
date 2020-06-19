/* subjectID60Defs.i -  */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE lAllRMItem AS Logical NO-UNDO.
DEFINE VARIABLE cStartRMItem AS Character NO-UNDO.
DEFINE VARIABLE cEndRMItem AS Character NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lAllRMItem = DYNAMIC-FUNCTION("fGetDynParamValue","allRMItems") EQ "YES"
        cStartRMItem = DYNAMIC-FUNCTION("fGetDynParamValue","startRMItem")
        cEndRMItem = DYNAMIC-FUNCTION("fGetDynParamValue","endRMItem")
        
        .
END PROCEDURE.
