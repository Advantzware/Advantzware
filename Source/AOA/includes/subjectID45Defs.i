/* subjectID45Defs.i - auto generated 10.04.2019 @  3:31:13 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE lPurge AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        lPurge = DYNAMIC-FUNCTION("fGetDynParamValue","Purge") EQ "YES"
        .
END PROCEDURE.
