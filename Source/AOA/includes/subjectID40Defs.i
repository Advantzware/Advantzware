/* subjectID40Defs.i - auto generated 06.25.2019 @  3:18:14 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartUserID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndUserID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartUserName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndUserName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllUserID AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cStartUserID = DYNAMIC-FUNCTION("fGetDynParamValue","startUserID")
        cEndUserID = DYNAMIC-FUNCTION("fGetDynParamValue","endUserID")
        cStartUserName = DYNAMIC-FUNCTION("fGetDynParamValue","startUserName")
        cEndUserName = DYNAMIC-FUNCTION("fGetDynParamValue","endUserName")
        lAllUserID = DYNAMIC-FUNCTION("fGetDynParamValue","allUserID") EQ "YES"
        .
END PROCEDURE.
