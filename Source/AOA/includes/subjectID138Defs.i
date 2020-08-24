/* subjectID138Defs.i - auto generated 08.15.2020 @ 11:46:47 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE cSbID AS Character NO-UNDO.
DEFINE VARIABLE cHTMLPage AS Character NO-UNDO.
DEFINE VARIABLE lLaunchHTMLPage AS Logical NO-UNDO.
DEFINE VARIABLE lReload AS Logical NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cSbID = DYNAMIC-FUNCTION("fGetDynParamValue","sbID")
        cHTMLPage = DYNAMIC-FUNCTION("fGetDynParamValue","HTMLPage")
        lLaunchHTMLPage = DYNAMIC-FUNCTION("fGetDynParamValue","LaunchHTMLPage") EQ "YES"
        lReload = DYNAMIC-FUNCTION("fGetDynParamValue","Reload") EQ "YES"
        .
END PROCEDURE.
