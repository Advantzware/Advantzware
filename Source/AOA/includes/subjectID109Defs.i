/* subjectID109Defs.i - auto generated 05.08.2020 @  3:21:56 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE dtPostDate AS Date NO-UNDO.
DEFINE VARIABLE lAllCustNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartCustNo AS Character NO-UNDO.
DEFINE VARIABLE cStartCustName AS Character NO-UNDO.
DEFINE VARIABLE cEndCustNo AS Character NO-UNDO.
DEFINE VARIABLE cEndCustName AS Character NO-UNDO.
DEFINE VARIABLE lPost AS Logical NO-UNDO.
DEFINE VARIABLE lAllInvNo AS Logical NO-UNDO.
DEFINE VARIABLE iStartInvNo AS Integer NO-UNDO.
DEFINE VARIABLE iEndInvNo AS Integer NO-UNDO.
DEFINE VARIABLE dtStartInvoiceDate AS Date NO-UNDO.
DEFINE VARIABLE dtEndInvoiceDate AS Date NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        dtPostDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","postDate"))
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lPost = DYNAMIC-FUNCTION("fGetDynParamValue","post") EQ "YES"
        lAllInvNo = DYNAMIC-FUNCTION("fGetDynParamValue","allInvNo") EQ "YES"
        iStartInvNo = DYNAMIC-FUNCTION("fGetDynParamValue","startInvNo")
        iEndInvNo = DYNAMIC-FUNCTION("fGetDynParamValue","endInvNo")
        dtStartInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startInvoiceDate"))
        dtEndInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endInvoiceDate"))
        .
END PROCEDURE.
