/* subjectID45Defs.i - auto generated 07.06.2021 @  7:14:36 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE lPurge AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        lPurge = DYNAMIC-FUNCTION("fGetDynParamValue","Purge") EQ "YES"
        .
END PROCEDURE.
