/* subjectID182Defs.i - auto generated 09.21.2021 @  4:03:46 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllPO AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartPONumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndPONumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartPoDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndPoDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartReceiptDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndReceiptDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-4 AS CHARACTER NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lAllPO = DYNAMIC-FUNCTION("fGetDynParamValue","allPO") EQ "YES"
        cStartPONumber = DYNAMIC-FUNCTION("fGetDynParamValue","StartPONumber")
        cEndPONumber = DYNAMIC-FUNCTION("fGetDynParamValue","endPONumber")
        dtStartPoDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startPoDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartPoDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartPoDate)
        dtEndPoDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endPoDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndPoDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndPoDate)
        dtStartReceiptDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startReceiptDate"))
        cDatePickList-3 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-3")
        dtStartReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-3,dtStartReceiptDate)
        dtEndReceiptDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endReceiptDate"))
        cDatePickList-4 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-4")
        dtEndReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-4,dtEndReceiptDate)
        .
END PROCEDURE.
