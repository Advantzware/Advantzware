/* subjectID167Defs.i - auto generated 07.06.2021 @  7:20:15 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtAsOfDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllAccountNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartAccountNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartAccountDesc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndAccountNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndAccountDesc AS CHARACTER NO-UNDO.
DEFINE VARIABLE lExcludeZero AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        dtAsOfDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","asOfDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtAsOfDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtAsOfDate)
        lAllAccountNo = DYNAMIC-FUNCTION("fGetDynParamValue","allAccountNo") EQ "YES"
        cStartAccountNo = DYNAMIC-FUNCTION("fGetDynParamValue","startAccountNo")
        cStartAccountDesc = DYNAMIC-FUNCTION("fGetDynParamValue","startAccountDesc")
        cEndAccountNo = DYNAMIC-FUNCTION("fGetDynParamValue","endAccountNo")
        cEndAccountDesc = DYNAMIC-FUNCTION("fGetDynParamValue","endAccountDesc")
        lExcludeZero = DYNAMIC-FUNCTION("fGetDynParamValue","ExcludeZero") EQ "YES"
        .
END PROCEDURE.
