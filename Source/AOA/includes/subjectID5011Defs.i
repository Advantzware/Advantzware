/* subjectID5011Defs.i - auto generated 05.02.2019 @  7:01:16 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustNo AS CHARACTER NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","custNo")
        .
END PROCEDURE.
