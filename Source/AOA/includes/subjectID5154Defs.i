/* subjectID5154Defs.i - auto generated 11.17.2020 @  5:43:20 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE cLocation AS Character NO-UNDO.
DEFINE VARIABLE lCustomers AS Logical NO-UNDO.
DEFINE VARIABLE lFGItems AS Logical NO-UNDO.
DEFINE VARIABLE lTerms AS Logical NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cLocation = DYNAMIC-FUNCTION("fGetDynParamValue","location")
        lCustomers = DYNAMIC-FUNCTION("fGetDynParamValue","Customers") EQ "YES"
        lFGItems = DYNAMIC-FUNCTION("fGetDynParamValue","FGItems") EQ "YES"
        lTerms = DYNAMIC-FUNCTION("fGetDynParamValue","Terms") EQ "YES"
        .
END PROCEDURE.
