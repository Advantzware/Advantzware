/* subjectID134Defs.i - auto generated 07.06.2021 @  7:18:10 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE lAllCompany AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllTerms AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartTerms AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartTermsDescr AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndTerms AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndTermsDescr AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllARClass AS LOGICAL NO-UNDO.
DEFINE VARIABLE iStartARClass AS INTEGER NO-UNDO.
DEFINE VARIABLE cStartARClassDescr AS CHARACTER NO-UNDO.
DEFINE VARIABLE iEndARClass AS INTEGER NO-UNDO.
DEFINE VARIABLE cEndARClassDescr AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllSalesRep AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartSalesRep AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartSalesRepName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSalesRep AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSalesRepName AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtAsOfDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE iRecentTrendDays AS INTEGER NO-UNDO.
DEFINE VARIABLE cAgedBy AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPeriodDays1 AS INTEGER NO-UNDO.
DEFINE VARIABLE iPeriodDays2 AS INTEGER NO-UNDO.
DEFINE VARIABLE iPeriodDays3 AS INTEGER NO-UNDO.
DEFINE VARIABLE iPeriodDays4 AS INTEGER NO-UNDO.
DEFINE VARIABLE lIncludePaidInvoices AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIncludeFuelSurcharges AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIncludeFactoredFGItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSeparateFinanceCharges AS LOGICAL NO-UNDO.
DEFINE VARIABLE lInactiveCustomers AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAllCurrency AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCurrency AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCurrencyDscr AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCurrency AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCurrencyDscr AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartInvoiceDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndInvoiceDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        lAllCompany = DYNAMIC-FUNCTION("fGetDynParamValue","allCompany") EQ "YES"
        cStartCompany = DYNAMIC-FUNCTION("fGetDynParamValue","startCompany")
        cStartDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startDescription")
        cEndCompany = DYNAMIC-FUNCTION("fGetDynParamValue","endCompany")
        cEndDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endDescription")
        lAllTerms = DYNAMIC-FUNCTION("fGetDynParamValue","AllTerms") EQ "YES"
        cStartTerms = DYNAMIC-FUNCTION("fGetDynParamValue","startTerms")
        cStartTermsDescr = DYNAMIC-FUNCTION("fGetDynParamValue","startTermsDescr")
        cEndTerms = DYNAMIC-FUNCTION("fGetDynParamValue","endTerms")
        cEndTermsDescr = DYNAMIC-FUNCTION("fGetDynParamValue","endTermsDescr")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lAllARClass = DYNAMIC-FUNCTION("fGetDynParamValue","AllARClass") EQ "YES"
        iStartARClass = DYNAMIC-FUNCTION("fGetDynParamValue","startARClass")
        cStartARClassDescr = DYNAMIC-FUNCTION("fGetDynParamValue","startARClassDescr")
        iEndARClass = DYNAMIC-FUNCTION("fGetDynParamValue","endARClass")
        cEndARClassDescr = DYNAMIC-FUNCTION("fGetDynParamValue","endARClassDescr")
        lAllSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","allSalesRep") EQ "YES"
        cStartSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","startSalesRep")
        cStartSalesRepName = DYNAMIC-FUNCTION("fGetDynParamValue","startSalesRepName")
        cEndSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","endSalesRep")
        cEndSalesRepName = DYNAMIC-FUNCTION("fGetDynParamValue","endSalesRepName")
        dtAsOfDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","asOfDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtAsOfDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtAsOfDate)
        iRecentTrendDays = DYNAMIC-FUNCTION("fGetDynParamValue","RecentTrendDays")
        cAgedBy = DYNAMIC-FUNCTION("fGetDynParamValue","AgedBy")
        iPeriodDays1 = DYNAMIC-FUNCTION("fGetDynParamValue","PeriodDays1")
        iPeriodDays2 = DYNAMIC-FUNCTION("fGetDynParamValue","PeriodDays2")
        iPeriodDays3 = DYNAMIC-FUNCTION("fGetDynParamValue","PeriodDays3")
        iPeriodDays4 = DYNAMIC-FUNCTION("fGetDynParamValue","PeriodDays4")
        lIncludePaidInvoices = DYNAMIC-FUNCTION("fGetDynParamValue","IncludePaidInvoices") EQ "YES"
        lIncludeFuelSurcharges = DYNAMIC-FUNCTION("fGetDynParamValue","IncludeFuelSurcharges") EQ "YES"
        lIncludeFactoredFGItems = DYNAMIC-FUNCTION("fGetDynParamValue","IncludeFactoredFGItems") EQ "YES"
        lSeparateFinanceCharges = DYNAMIC-FUNCTION("fGetDynParamValue","SeparateFinanceCharges") EQ "YES"
        lInactiveCustomers = DYNAMIC-FUNCTION("fGetDynParamValue","InactiveCustomers") EQ "YES"
        lAllCurrency = DYNAMIC-FUNCTION("fGetDynParamValue","AllCurrency") EQ "YES"
        cStartCurrency = DYNAMIC-FUNCTION("fGetDynParamValue","startCurrency")
        cStartCurrencyDscr = DYNAMIC-FUNCTION("fGetDynParamValue","startCurrencyDscr")
        cEndCurrency = DYNAMIC-FUNCTION("fGetDynParamValue","endCurrency")
        cEndCurrencyDscr = DYNAMIC-FUNCTION("fGetDynParamValue","endCurrencyDscr")
        dtStartInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startInvoiceDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtStartInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtStartInvoiceDate)
        dtEndInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endInvoiceDate"))
        cDatePickList-3 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-3")
        dtEndInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-3,dtEndInvoiceDate)
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        .
END PROCEDURE.
