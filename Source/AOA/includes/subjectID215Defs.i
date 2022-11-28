/* subjectID215Defs.i - auto generated 10.21.2022 @ 11:27:13 am */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
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
DEFINE VARIABLE lAllShipToNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartShipToNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartShipToName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShipToNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShipToName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllSalesRep AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartSalesRep AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartSalesRepName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSalesRep AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSalesRepName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllFGItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartFGItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndFGItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartInvoiceDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndInvoiceDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIncludeFinanceCharges AS LOGICAL NO-UNDO.
DEFINE VARIABLE lShowDiscountedPrices AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
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
        lAllShipToNo = DYNAMIC-FUNCTION("fGetDynParamValue","allShipToNo") EQ "YES"
        cStartShipToNo = DYNAMIC-FUNCTION("fGetDynParamValue","startShipToNo")
        cStartShipToName = DYNAMIC-FUNCTION("fGetDynParamValue","startShipToName")
        cEndShipToNo = DYNAMIC-FUNCTION("fGetDynParamValue","endShipToNo")
        cEndShipToName = DYNAMIC-FUNCTION("fGetDynParamValue","endShipToName")
        lAllSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","allSalesRep") EQ "YES"
        cStartSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","startSalesRep")
        cStartSalesRepName = DYNAMIC-FUNCTION("fGetDynParamValue","startSalesRepName")
        cEndSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","endSalesRep")
        cEndSalesRepName = DYNAMIC-FUNCTION("fGetDynParamValue","endSalesRepName")
        lAllFGItems = DYNAMIC-FUNCTION("fGetDynParamValue","allFGItems") EQ "YES"
        cStartFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItem")
        cStartFGItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItemDescriptn")
        cEndFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItem")
        cEndFGItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItemDescriptn")
        dtStartInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startInvoiceDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartInvoiceDate)
        dtEndInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endInvoiceDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndInvoiceDate)
        lIncludeFinanceCharges = DYNAMIC-FUNCTION("fGetDynParamValue","IncludeFinanceCharges") EQ "YES"
        lShowDiscountedPrices = DYNAMIC-FUNCTION("fGetDynParamValue","ShowDiscountedPrices") EQ "YES"
        .
END PROCEDURE.
