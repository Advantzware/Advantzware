/* subjectID53Defs.i - auto generated 11.06.2019 @ 12:27:54 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE lCorrectData AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        lCorrectData = DYNAMIC-FUNCTION("fGetDynParamValue","CorrectData") EQ "YES"
        .
END PROCEDURE.
