/* subjectID95Defs.i - auto generated 11.30.2020 @  5:41:43 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtAsOfDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAsOfDateOption AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllFGItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartFGItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndFGItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIncludeZeroPricePer AS LOGICAL NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
DEFINE VARIABLE cProductCategoryList AS CHARACTER NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        dtAsOfDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","asOfDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtAsOfDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtAsOfDate)
        dtStartDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","StartDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtStartDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtStartDate)
        dtEndDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","EndDate"))
        cDatePickList-3 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-3")
        dtEndDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-3,dtEndDate)
        cAsOfDateOption = DYNAMIC-FUNCTION("fGetDynParamValue","AsOfDateOption")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lAllFGItems = DYNAMIC-FUNCTION("fGetDynParamValue","allFGItems") EQ "YES"
        cStartFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItem")
        cStartFGItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItemDescriptn")
        cEndFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItem")
        cEndFGItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItemDescriptn")
        lIncludeZeroPricePer = DYNAMIC-FUNCTION("fGetDynParamValue","IncludeZeroPricePer") EQ "YES"
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        cProductCategoryList = DYNAMIC-FUNCTION("fGetDynParamValue","ProductCategoryList")
        .
END PROCEDURE.
