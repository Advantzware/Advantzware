/* pTemplate.i - auto generated 11.02.2016 @  7:29:35 pm from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllLoc AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dtStartTranDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartTranDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartLoc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndTranDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndTranDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndLoc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartReceiptDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartReceiptDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllCompany AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllMachine AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dtEndReceiptDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndReceiptDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartMachine AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartInvoiceDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartInvoiceDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndMachine AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndInvoiceDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndInvoiceDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllSalesRep AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCurrency AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dtStartOrderDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartOrderDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartSalesRep AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartCurrency AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndOrderDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndOrderDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndSalesRep AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCurrency AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartDueDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartDueDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllPONumber AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCAD AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllTerms AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dtEndDueDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndDueDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartPONumber AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartCAD AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartTerms AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndPONumber AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCAD AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndTerms AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartShipDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartShipDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndShipDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndShipDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllUserID AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartUserID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartBOLDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartBOLDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndUserID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndBOLDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndBOLDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllItemNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dtAsOfDate AS DATE NO-UNDO.
    DEFINE VARIABLE cAsOfDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSort AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSubRpt_SubReportName AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllJobNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllOrderNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllBOL AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllLocBin AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllInvNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartJobNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStartJobNo2 AS INTEGER NO-UNDO.
    DEFINE VARIABLE iStartOrderNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE iStartBOL AS INTEGER NO-UNDO.
    DEFINE VARIABLE cStartLocBin AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStartInvNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE cEndJobNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iEndJobNo2 AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEndOrderNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEndBOL AS INTEGER NO-UNDO.
    DEFINE VARIABLE cEndLocBin AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iEndInvNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE lAllProdCategory AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartProdCategory AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndProdCategory AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllShift AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iStartShift AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEndShift AS INTEGER NO-UNDO.
    DEFINE VARIABLE lAllDept AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartDept AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndDept AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "template.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        cLocation = DYNAMIC-FUNCTION("fGetParamValue","svLocation")
        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllLoc = DYNAMIC-FUNCTION("fGetParamValue","svAllLoc") EQ "yes"
        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"
        dtStartTranDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartTranDate"))
        cStartTranDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartTranDateOption")
        dtStartTranDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartTranDateOption,dtStartTranDate)
        cStartLoc = DYNAMIC-FUNCTION("fGetParamValue","svStartLoc")
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        dtEndTranDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndTranDate"))
        cEndTranDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndTranDateOption")
        dtEndTranDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndTranDateOption,dtEndTranDate)
        cEndLoc = DYNAMIC-FUNCTION("fGetParamValue","svEndLoc")
        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        dtStartReceiptDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartReceiptDate"))
        cStartReceiptDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartReceiptDateOption")
        dtStartReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartReceiptDateOption,dtStartReceiptDate)
        lAllCompany = DYNAMIC-FUNCTION("fGetParamValue","svAllCompany") EQ "yes"
        lAllMachine = DYNAMIC-FUNCTION("fGetParamValue","svAllMachine") EQ "yes"
        dtEndReceiptDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndReceiptDate"))
        cEndReceiptDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndReceiptDateOption")
        dtEndReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndReceiptDateOption,dtEndReceiptDate)
        cStartCompany = DYNAMIC-FUNCTION("fGetParamValue","svStartCompany")
        cStartMachine = DYNAMIC-FUNCTION("fGetParamValue","svStartMachine")
        dtStartInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartInvoiceDate"))
        cStartInvoiceDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartInvoiceDateOption")
        dtStartInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartInvoiceDateOption,dtStartInvoiceDate)
        cEndCompany = DYNAMIC-FUNCTION("fGetParamValue","svEndCompany")
        cEndMachine = DYNAMIC-FUNCTION("fGetParamValue","svEndMachine")
        dtEndInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndInvoiceDate"))
        cEndInvoiceDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndInvoiceDateOption")
        dtEndInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndInvoiceDateOption,dtEndInvoiceDate)
        lAllSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svAllSalesRep") EQ "yes"
        lAllCurrency = DYNAMIC-FUNCTION("fGetParamValue","svAllCurrency") EQ "yes"
        dtStartOrderDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartOrderDate"))
        cStartOrderDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartOrderDateOption")
        dtStartOrderDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartOrderDateOption,dtStartOrderDate)
        cStartSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svStartSalesRep")
        cStartCurrency = DYNAMIC-FUNCTION("fGetParamValue","svStartCurrency")
        dtEndOrderDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndOrderDate"))
        cEndOrderDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndOrderDateOption")
        dtEndOrderDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndOrderDateOption,dtEndOrderDate)
        cEndSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svEndSalesRep")
        cEndCurrency = DYNAMIC-FUNCTION("fGetParamValue","svEndCurrency")
        dtStartDueDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartDueDate"))
        cStartDueDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartDueDateOption")
        dtStartDueDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartDueDateOption,dtStartDueDate)
        lAllPONumber = DYNAMIC-FUNCTION("fGetParamValue","svAllPONumber") EQ "yes"
        lAllCAD = DYNAMIC-FUNCTION("fGetParamValue","svAllCAD") EQ "yes"
        lAllTerms = DYNAMIC-FUNCTION("fGetParamValue","svAllTerms") EQ "yes"
        dtEndDueDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndDueDate"))
        cEndDueDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndDueDateOption")
        dtEndDueDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndDueDateOption,dtEndDueDate)
        cStartPONumber = DYNAMIC-FUNCTION("fGetParamValue","svStartPONumber")
        cStartCAD = DYNAMIC-FUNCTION("fGetParamValue","svStartCAD")
        cStartTerms = DYNAMIC-FUNCTION("fGetParamValue","svStartTerms")
        cEndPONumber = DYNAMIC-FUNCTION("fGetParamValue","svEndPONumber")
        cEndCAD = DYNAMIC-FUNCTION("fGetParamValue","svEndCAD")
        cEndTerms = DYNAMIC-FUNCTION("fGetParamValue","svEndTerms")
        dtStartShipDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartShipDate"))
        cStartShipDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartShipDateOption")
        dtStartShipDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartShipDateOption,dtStartShipDate)
        dtEndShipDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndShipDate"))
        cEndShipDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndShipDateOption")
        dtEndShipDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndShipDateOption,dtEndShipDate)
        lAllUserID = DYNAMIC-FUNCTION("fGetParamValue","svAllUserID") EQ "yes"
        cStartUserID = DYNAMIC-FUNCTION("fGetParamValue","svStartUserID")
        dtStartBOLDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartBOLDate"))
        cStartBOLDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartBOLDateOption")
        dtStartBOLDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartBOLDateOption,dtStartBOLDate)
        cEndUserID = DYNAMIC-FUNCTION("fGetParamValue","svEndUserID")
        dtEndBOLDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndBOLDate"))
        cEndBOLDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndBOLDateOption")
        dtEndBOLDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndBOLDateOption,dtEndBOLDate)
        lAllItemNo = DYNAMIC-FUNCTION("fGetParamValue","svAllItemNo") EQ "yes"
        dtAsOfDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svAsOfDate"))
        cAsOfDateOption = DYNAMIC-FUNCTION("fGetParamValue","svAsOfDateOption")
        dtAsOfDate = DYNAMIC-FUNCTION("fDateOptionDate",cAsOfDateOption,dtAsOfDate)
        cStartItemNo = DYNAMIC-FUNCTION("fGetParamValue","svStartItemNo")
        cSort = DYNAMIC-FUNCTION("fGetParamValue","svSort")
        cEndItemNo = DYNAMIC-FUNCTION("fGetParamValue","svEndItemNo")
        lSubRpt_SubReportName = DYNAMIC-FUNCTION("fGetParamValue","svSubRpt_SubReportName") EQ "yes"
        lAllJobNo = DYNAMIC-FUNCTION("fGetParamValue","svAllJobNo") EQ "yes"
        lAllOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svAllOrderNo") EQ "yes"
        lAllBOL = DYNAMIC-FUNCTION("fGetParamValue","svAllBOL") EQ "yes"
        lAllLocBin = DYNAMIC-FUNCTION("fGetParamValue","svAllLocBin") EQ "yes"
        lAllInvNo = DYNAMIC-FUNCTION("fGetParamValue","svAllInvNo") EQ "yes"
        cStartJobNo = DYNAMIC-FUNCTION("fGetParamValue","svStartJobNo")
        iStartJobNo2 = DYNAMIC-FUNCTION("fGetParamValue","svStartJobNo2")
        iStartOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svStartOrderNo")
        iStartBOL = DYNAMIC-FUNCTION("fGetParamValue","svStartBOL")
        cStartLocBin = DYNAMIC-FUNCTION("fGetParamValue","svStartLocBin")
        iStartInvNo = DYNAMIC-FUNCTION("fGetParamValue","svStartInvNo")
        cEndJobNo = DYNAMIC-FUNCTION("fGetParamValue","svEndJobNo")
        iEndJobNo2 = DYNAMIC-FUNCTION("fGetParamValue","svEndJobNo2")
        iEndOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svEndOrderNo")
        iEndBOL = DYNAMIC-FUNCTION("fGetParamValue","svEndBOL")
        cEndLocBin = DYNAMIC-FUNCTION("fGetParamValue","svEndLocBin")
        iEndInvNo = DYNAMIC-FUNCTION("fGetParamValue","svEndInvNo")
        lAllProdCategory = DYNAMIC-FUNCTION("fGetParamValue","svAllProdCategory") EQ "yes"
        cStartProdCategory = DYNAMIC-FUNCTION("fGetParamValue","svStartProdCategory")
        cEndProdCategory = DYNAMIC-FUNCTION("fGetParamValue","svEndProdCategory")
        lAllShift = DYNAMIC-FUNCTION("fGetParamValue","svAllShift") EQ "yes"
        iStartShift = DYNAMIC-FUNCTION("fGetParamValue","svStartShift")
        iEndShift = DYNAMIC-FUNCTION("fGetParamValue","svEndShift")
        lAllDept = DYNAMIC-FUNCTION("fGetParamValue","svAllDept") EQ "yes"
        cStartDept = DYNAMIC-FUNCTION("fGetParamValue","svStartDept")
        cEndDept = DYNAMIC-FUNCTION("fGetParamValue","svEndDept")
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttTemplate:HANDLE,
                     cAvailableColumns,
                     cSelectedColumns
                     ).

    IF lAllLoc THEN
    ASSIGN
        cStartLoc = CHR(32)
        cEndLoc   = CHR(254)
        .

    IF lAllCustNo THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(254)
        .

    IF lAllCompany THEN
    ASSIGN
        cStartCompany = CHR(32)
        cEndCompany   = CHR(254)
        .

    IF lAllMachine THEN
    ASSIGN
        cStartMachine = CHR(32)
        cEndMachine   = CHR(254)
        .

    IF lAllSalesRep THEN
    ASSIGN
        cStartSalesRep = CHR(32)
        cEndSalesRep   = CHR(254)
        .

    IF lAllCurrency THEN
    ASSIGN
        cStartCurrency = CHR(32)
        cEndCurrency   = CHR(254)
        .

    IF lAllPONumber THEN
    ASSIGN
        cStartPONumber = CHR(32)
        cEndPONumber   = CHR(254)
        .

    IF lAllCAD THEN
    ASSIGN
        cStartCAD = CHR(32)
        cEndCAD   = CHR(254)
        .

    IF lAllTerms THEN
    ASSIGN
        cStartTerms = CHR(32)
        cEndTerms   = CHR(254)
        .

    IF lAllUserID THEN
    ASSIGN
        cStartUserID = CHR(32)
        cEndUserID   = CHR(254)
        .

    IF lAllItemNo THEN
    ASSIGN
        cStartItemNo = CHR(32)
        cEndItemNo   = CHR(254)
        .

    IF lAllJobNo THEN
    ASSIGN
        cStartJobNo = CHR(32)
        cEndJobNo   = CHR(254)
        .

    IF lAllOrderNo THEN
    ASSIGN
        iStartOrderNo = 0
        iEndOrderNo   = 99999999
        .

    IF lAllBOL THEN
    ASSIGN
        iStartBOL = 0
        iEndBOL   = 99999999
        .

    IF lAllLocBin THEN
    ASSIGN
        cStartLocBin = CHR(32)
        cEndLocBin   = CHR(254)
        .

    IF lAllInvNo THEN
    ASSIGN
        iStartInvNo = 0
        iEndInvNo   = 99999999
        .

    IF lAllProdCategory THEN
    ASSIGN
        cStartProdCategory = CHR(32)
        cEndProdCategory   = CHR(254)
        .

    IF lAllShift THEN
    ASSIGN
        iStartShift = 0
        iEndShift   = 99999999
        .

    IF lAllDept THEN
    ASSIGN
        cStartDept = CHR(32)
        cEndDept   = CHR(254)
        .

    RUN pBuildCustList (ipcCompany, lCustList, cStartCustNo, cEndCustNo, "").
