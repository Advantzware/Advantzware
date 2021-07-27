/* subjectID163Defs.i - auto generated 07.06.2021 @  7:19:55 pm */

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
