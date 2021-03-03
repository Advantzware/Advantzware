/* subjectID163Defs.i - auto generated 02.23.2021 @ 12:50:30 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPostDelete AS CHARACTER NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cPostDelete = DYNAMIC-FUNCTION("fGetDynParamValue","PostDelete")
        .
END PROCEDURE.
