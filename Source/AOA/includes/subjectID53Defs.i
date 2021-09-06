/* subjectID53Defs.i - auto generated 07.06.2021 @  7:15:05 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE lCorrectData AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        lCorrectData = DYNAMIC-FUNCTION("fGetDynParamValue","CorrectData") EQ "YES"
        .
END PROCEDURE.
