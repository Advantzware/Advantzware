/* subjectID134Defs.i - auto generated 07.23.2020 @ 11:23:37 am */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE lAllCompany AS Logical NO-UNDO.
DEFINE VARIABLE cStartCompany AS Character NO-UNDO.
DEFINE VARIABLE cStartDescription AS Character NO-UNDO.
DEFINE VARIABLE cEndCompany AS Character NO-UNDO.
DEFINE VARIABLE cEndDescription AS Character NO-UNDO.
DEFINE VARIABLE lAllTerms AS Logical NO-UNDO.
DEFINE VARIABLE cStartTerms AS Character NO-UNDO.
DEFINE VARIABLE cStartTermsDescr AS Character NO-UNDO.
DEFINE VARIABLE cEndTerms AS Character NO-UNDO.
DEFINE VARIABLE cEndTermsDescr AS Character NO-UNDO.
DEFINE VARIABLE lAllCustNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartCustNo AS Character NO-UNDO.
DEFINE VARIABLE cStartCustName AS Character NO-UNDO.
DEFINE VARIABLE cEndCustNo AS Character NO-UNDO.
DEFINE VARIABLE cEndCustName AS Character NO-UNDO.
DEFINE VARIABLE lAllARClass AS Logical NO-UNDO.
DEFINE VARIABLE iStartARClass AS Integer NO-UNDO.
DEFINE VARIABLE cStartARClassDescr AS Character NO-UNDO.
DEFINE VARIABLE iEndARClass AS Integer NO-UNDO.
DEFINE VARIABLE cEndARClassDescr AS Character NO-UNDO.
DEFINE VARIABLE lAllSalesRep AS Logical NO-UNDO.
DEFINE VARIABLE cStartSalesRep AS Character NO-UNDO.
DEFINE VARIABLE cStartSalesRepName AS Character NO-UNDO.
DEFINE VARIABLE cEndSalesRep AS Character NO-UNDO.
DEFINE VARIABLE cEndSalesRepName AS Character NO-UNDO.
DEFINE VARIABLE dtAsOfDate AS Date NO-UNDO.
DEFINE VARIABLE lCustList AS Logical NO-UNDO.
DEFINE VARIABLE iRecentTrendDays AS Integer NO-UNDO.
DEFINE VARIABLE cAgedBy AS Character NO-UNDO.
DEFINE VARIABLE iPeriodDays1 AS Integer NO-UNDO.
DEFINE VARIABLE iPeriodDays2 AS Integer NO-UNDO.
DEFINE VARIABLE iPeriodDays3 AS Integer NO-UNDO.
DEFINE VARIABLE iPeriodDays4 AS Integer NO-UNDO.
DEFINE VARIABLE lIncludePaidInvoices AS Logical NO-UNDO.
DEFINE VARIABLE lIncludeFuelSurcharges AS Logical NO-UNDO.
DEFINE VARIABLE lIncludeFactoredFGItems AS Logical NO-UNDO.
DEFINE VARIABLE lSeparateFinanceCharges AS Logical NO-UNDO.
DEFINE VARIABLE lInactiveCustomers AS Logical NO-UNDO.
DEFINE VARIABLE lAllCurrency AS Logical NO-UNDO.
DEFINE VARIABLE cStartCurrency AS Character NO-UNDO.
DEFINE VARIABLE cStartCurrencyDscr AS Character NO-UNDO.
DEFINE VARIABLE cEndCurrency AS Character NO-UNDO.
DEFINE VARIABLE cEndCurrencyDscr AS Character NO-UNDO.
DEFINE VARIABLE dtStartInvoiceDate AS Date NO-UNDO.
DEFINE VARIABLE dtEndInvoiceDate AS Date NO-UNDO.

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
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
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
        dtEndInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endInvoiceDate"))
        .
END PROCEDURE.
