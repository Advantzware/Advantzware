/* subjectID7Defs.i - auto generated 07.06.2021 @  7:12:25 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartInvoiceDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndInvoiceDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lExceptionOnly AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        dtStartInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startInvoiceDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartInvoiceDate)
        dtEndInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endInvoiceDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndInvoiceDate)
        lExceptionOnly = DYNAMIC-FUNCTION("fGetDynParamValue","exceptionOnly") EQ "YES"
        .
END PROCEDURE.
