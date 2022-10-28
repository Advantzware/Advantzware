/* subjectID138Defs.i - auto generated 10.25.2022 @ 10:33:55 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSbID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHTMLPage AS CHARACTER NO-UNDO.
DEFINE VARIABLE lUseDueDateOnly AS LOGICAL NO-UNDO.
DEFINE VARIABLE lLaunchHTMLPage AS LOGICAL NO-UNDO.
DEFINE VARIABLE lReload AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cSbID = DYNAMIC-FUNCTION("fGetDynParamValue","sbID")
        cHTMLPage = DYNAMIC-FUNCTION("fGetDynParamValue","HTMLPage")
        lUseDueDateOnly = DYNAMIC-FUNCTION("fGetDynParamValue","UseDueDateOnly") EQ "YES"
        lLaunchHTMLPage = DYNAMIC-FUNCTION("fGetDynParamValue","LaunchHTMLPage") EQ "YES"
        lReload = DYNAMIC-FUNCTION("fGetDynParamValue","Reload") EQ "YES"
        .
END PROCEDURE.
