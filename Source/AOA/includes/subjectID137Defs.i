/* subjectID137Defs.i - auto generated 08.13.2020 @  1:05:53 am */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE lAllCustNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartCustNo AS Character NO-UNDO.
DEFINE VARIABLE cStartCustName AS Character NO-UNDO.
DEFINE VARIABLE cEndCustNo AS Character NO-UNDO.
DEFINE VARIABLE cEndCustName AS Character NO-UNDO.
DEFINE VARIABLE lOrderBalance AS Logical NO-UNDO.
DEFINE VARIABLE lARBalance AS Logical NO-UNDO.
DEFINE VARIABLE lPastGraceBalance AS Logical NO-UNDO.
DEFINE VARIABLE lCreditHold AS Logical NO-UNDO.
DEFINE VARIABLE lCustomerNotAged AS Logical NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lOrderBalance = DYNAMIC-FUNCTION("fGetDynParamValue","OrderBalance") EQ "YES"
        lARBalance = DYNAMIC-FUNCTION("fGetDynParamValue","ARBalance") EQ "YES"
        lPastGraceBalance = DYNAMIC-FUNCTION("fGetDynParamValue","PastGraceBalance") EQ "YES"
        lCreditHold = DYNAMIC-FUNCTION("fGetDynParamValue","CreditHold") EQ "YES"
        lCustomerNotAged = DYNAMIC-FUNCTION("fGetDynParamValue","CustomerNotAged") EQ "YES"
        .
END PROCEDURE.
