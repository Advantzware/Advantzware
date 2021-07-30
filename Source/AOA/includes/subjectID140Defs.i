/* subjectID140Defs.i - auto generated 07.06.2021 @  7:18:52 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustTypes AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllGroups AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartSalesGroup AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartSalesGroupDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSalesGroup AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSalesGroupDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllProCats AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartProCat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartProCatDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCatDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartInvoiceDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndInvoiceDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIncludeFinanceCharges AS LOGICAL NO-UNDO.
DEFINE VARIABLE lShowDiscountedPrices AS LOGICAL NO-UNDO.
DEFINE VARIABLE cSummary-Details AS CHARACTER NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lAllCustTypes = DYNAMIC-FUNCTION("fGetDynParamValue","allCustTypes") EQ "YES"
        cStartCustType = DYNAMIC-FUNCTION("fGetDynParamValue","startCustType")
        cStartCustDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startCustDescription")
        cEndCustType = DYNAMIC-FUNCTION("fGetDynParamValue","endCustType")
        cEndCustDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endCustDescription")
        lAllGroups = DYNAMIC-FUNCTION("fGetDynParamValue","allGroups") EQ "YES"
        cStartSalesGroup = DYNAMIC-FUNCTION("fGetDynParamValue","StartSalesGroup")
        cStartSalesGroupDescription = DYNAMIC-FUNCTION("fGetDynParamValue","StartSalesGroupDescription")
        cEndSalesGroup = DYNAMIC-FUNCTION("fGetDynParamValue","EndSalesGroup")
        cEndSalesGroupDescription = DYNAMIC-FUNCTION("fGetDynParamValue","EndSalesGroupDescription")
        lAllProCats = DYNAMIC-FUNCTION("fGetDynParamValue","allProCats") EQ "YES"
        cStartProCat = DYNAMIC-FUNCTION("fGetDynParamValue","startProCat")
        cStartProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startProCatDescriptn")
        cEndProCat = DYNAMIC-FUNCTION("fGetDynParamValue","endProCat")
        cEndProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endProCatDescriptn")
        dtStartInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startInvoiceDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartInvoiceDate)
        dtEndInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endInvoiceDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndInvoiceDate)
        lIncludeFinanceCharges = DYNAMIC-FUNCTION("fGetDynParamValue","IncludeFinanceCharges") EQ "YES"
        lShowDiscountedPrices = DYNAMIC-FUNCTION("fGetDynParamValue","ShowDiscountedPrices") EQ "YES"
        cSummary-Details = DYNAMIC-FUNCTION("fGetDynParamValue","Summary-Details")
        .
END PROCEDURE.
