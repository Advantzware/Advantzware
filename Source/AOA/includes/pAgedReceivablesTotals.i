/* pAgedReceivablesTotals.i - auto generated 08.11.2020 @  7:18:12 pm from AOA/aoaParam.w */

    {AOA/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE lAllARClass AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iEndARClass AS INTEGER NO-UNDO.
    DEFINE VARIABLE iStartARClass AS INTEGER NO-UNDO.
    DEFINE VARIABLE lAllCompany AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllSalesRep AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartSalesRep AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndSalesRep AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllTerms AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartTerms AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndTerms AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSort1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSort2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lInactiveCustomers AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCurrency AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCurrency AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCurrency AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtAsOfDate AS DATE NO-UNDO.
    DEFINE VARIABLE cAsOfDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartInvoiceDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartInvoiceDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndInvoiceDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndInvoiceDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPeriodDays1 AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPeriodDays2 AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPeriodDays3 AS INTEGER NO-UNDO.
    DEFINE VARIABLE iRecentTrendDays AS INTEGER NO-UNDO.
    DEFINE VARIABLE lIncludePaidInvoices AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIncludeFuelSurcharges AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIncludeFactoredFGItems AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSeparateFinanceCharges AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cRecipients AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetUserPrint (ipcCompany, "agedtot.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        lAllARClass = DYNAMIC-FUNCTION("fGetParamValue","svAllARClass") EQ "yes"
        iEndARClass = DYNAMIC-FUNCTION("fGetParamValue","svEndARClass")
        iStartARClass = DYNAMIC-FUNCTION("fGetParamValue","svStartARClass")
        lAllCompany = DYNAMIC-FUNCTION("fGetParamValue","svAllCompany") EQ "yes"
        cStartCompany = DYNAMIC-FUNCTION("fGetParamValue","svStartCompany")
        cEndCompany = DYNAMIC-FUNCTION("fGetParamValue","svEndCompany")
        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svAllSalesRep") EQ "yes"
        cStartSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svStartSalesRep")
        cEndSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svEndSalesRep")
        lAllTerms = DYNAMIC-FUNCTION("fGetParamValue","svAllTerms") EQ "yes"
        cStartTerms = DYNAMIC-FUNCTION("fGetParamValue","svStartTerms")
        cEndTerms = DYNAMIC-FUNCTION("fGetParamValue","svEndTerms")
        cType = DYNAMIC-FUNCTION("fGetParamValue","svType")
        cSort1 = DYNAMIC-FUNCTION("fGetParamValue","svSort1")
        cSort2 = DYNAMIC-FUNCTION("fGetParamValue","svSort2")
        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        lInactiveCustomers = DYNAMIC-FUNCTION("fGetParamValue","svInactiveCustomers") EQ "yes"
        lAllCurrency = DYNAMIC-FUNCTION("fGetParamValue","svAllCurrency") EQ "yes"
        cStartCurrency = DYNAMIC-FUNCTION("fGetParamValue","svStartCurrency")
        cEndCurrency = DYNAMIC-FUNCTION("fGetParamValue","svEndCurrency")
        dtAsOfDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svAsOfDate"))
        cAsOfDateOption = DYNAMIC-FUNCTION("fGetParamValue","svAsOfDateOption")
        dtAsOfDate = DYNAMIC-FUNCTION("fDateOptionDate",cAsOfDateOption,dtAsOfDate)
        dtStartInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartInvoiceDate"))
        cStartInvoiceDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartInvoiceDateOption")
        dtStartInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartInvoiceDateOption,dtStartInvoiceDate)
        dtEndInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndInvoiceDate"))
        cEndInvoiceDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndInvoiceDateOption")
        dtEndInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndInvoiceDateOption,dtEndInvoiceDate)
        iPeriodDays1 = DYNAMIC-FUNCTION("fGetParamValue","svPeriodDays1")
        iPeriodDays2 = DYNAMIC-FUNCTION("fGetParamValue","svPeriodDays2")
        iPeriodDays3 = DYNAMIC-FUNCTION("fGetParamValue","svPeriodDays3")
        iRecentTrendDays = DYNAMIC-FUNCTION("fGetParamValue","svRecentTrendDays")
        lIncludePaidInvoices = DYNAMIC-FUNCTION("fGetParamValue","svIncludePaidInvoices") EQ "yes"
        lIncludeFuelSurcharges = DYNAMIC-FUNCTION("fGetParamValue","svIncludeFuelSurcharges") EQ "yes"
        lIncludeFactoredFGItems = DYNAMIC-FUNCTION("fGetParamValue","svIncludeFactoredFGItems") EQ "yes"
        lSeparateFinanceCharges = DYNAMIC-FUNCTION("fGetParamValue","svSeparateFinanceCharges") EQ "yes"
        cRecipients = DYNAMIC-FUNCTION("fGetParamValue","svRecipients")
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttAgedReceivablesTotals:HANDLE, cAvailableColumns, cSelectedColumns).

    IF lAllARClass THEN
    ASSIGN
        iStartARClass = 0
        iEndARClass   = 99999999
        .

    IF lAllCompany THEN
    ASSIGN
        cStartCompany = CHR(32)
        cEndCompany   = CHR(254)
        .

    IF lAllSalesRep THEN
    ASSIGN
        cStartSalesRep = CHR(32)
        cEndSalesRep   = CHR(254)
        .

    IF lAllTerms THEN
    ASSIGN
        cStartTerms = CHR(32)
        cEndTerms   = CHR(254)
        .

    IF lAllCustNo THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(254)
        .

    IF lAllCurrency THEN
    ASSIGN
        cStartCurrency = CHR(32)
        cEndCurrency   = CHR(254)
        .

    IF lCustList THEN
    RUN pBuildCustList (ipcCompany, "AR51", OUTPUT cStartCustNo, OUTPUT cEndCustNo, OUTPUT lCustList).