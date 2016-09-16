/* pTemplate.i - auto generated 06.15.2016 @  8:23:02 pm from aoa/aoaParam.w */

    {aoaAppSrv/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dtStartTranDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartTranDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndTranDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndTranDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartReceiptDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartReceiptDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllMachine AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dtEndReceiptDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndReceiptDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartMachine AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartInvoiceDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartInvoiceDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndMachine AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndInvoiceDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndInvoiceDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllSalesRep AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dtStartOrderDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartOrderDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartSalesRep AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndOrderDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndOrderDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndSalesRep AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartDueDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartDueDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllPONumber AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCAD AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dtEndDueDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndDueDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartPONumber AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartCAD AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndPONumber AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCAD AS CHARACTER NO-UNDO.
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
    DEFINE VARIABLE cSort AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSubRpt_SubReportName AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cEndItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllJobNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllOrderNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllBOL AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartJobNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStartJobNo2 AS INTEGER NO-UNDO.
    DEFINE VARIABLE iStartOrderNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE iStartBOL AS INTEGER NO-UNDO.
    DEFINE VARIABLE cEndJobNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iEndJobNo2 AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEndOrderNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEndBOL AS INTEGER NO-UNDO.
    DEFINE VARIABLE lAllProdCategory AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartProdCategory AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndProdCategory AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllShift AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iStartShift AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEndShift AS INTEGER NO-UNDO.
    DEFINE VARIABLE lAllDept AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartDept AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndDept AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "template.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        cLocation = DYNAMIC-FUNCTION("fGetParamValue","svLocation")
        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"
        dtStartTranDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartTranDate"))
        cStartTranDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartTranDateOption")
        dtStartTranDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartTranDateOption,dtStartTranDate)
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        dtEndTranDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndTranDate"))
        cEndTranDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndTranDateOption")
        dtEndTranDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndTranDateOption,dtEndTranDate)
        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        dtStartReceiptDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartReceiptDate"))
        cStartReceiptDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartReceiptDateOption")
        dtStartReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartReceiptDateOption,dtStartReceiptDate)
        lAllMachine = DYNAMIC-FUNCTION("fGetParamValue","svAllMachine") EQ "yes"
        dtEndReceiptDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndReceiptDate"))
        cEndReceiptDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndReceiptDateOption")
        dtEndReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndReceiptDateOption,dtEndReceiptDate)
        cStartMachine = DYNAMIC-FUNCTION("fGetParamValue","svStartMachine")
        dtStartInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartInvoiceDate"))
        cStartInvoiceDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartInvoiceDateOption")
        dtStartInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartInvoiceDateOption,dtStartInvoiceDate)
        cEndMachine = DYNAMIC-FUNCTION("fGetParamValue","svEndMachine")
        dtEndInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndInvoiceDate"))
        cEndInvoiceDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndInvoiceDateOption")
        dtEndInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndInvoiceDateOption,dtEndInvoiceDate)
        lAllSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svAllSalesRep") EQ "yes"
        dtStartOrderDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartOrderDate"))
        cStartOrderDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartOrderDateOption")
        dtStartOrderDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartOrderDateOption,dtStartOrderDate)
        cStartSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svStartSalesRep")
        dtEndOrderDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndOrderDate"))
        cEndOrderDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndOrderDateOption")
        dtEndOrderDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndOrderDateOption,dtEndOrderDate)
        cEndSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svEndSalesRep")
        dtStartDueDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartDueDate"))
        cStartDueDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartDueDateOption")
        dtStartDueDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartDueDateOption,dtStartDueDate)
        lAllPONumber = DYNAMIC-FUNCTION("fGetParamValue","svAllPONumber") EQ "yes"
        lAllCAD = DYNAMIC-FUNCTION("fGetParamValue","svAllCAD") EQ "yes"
        dtEndDueDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndDueDate"))
        cEndDueDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndDueDateOption")
        dtEndDueDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndDueDateOption,dtEndDueDate)
        cStartPONumber = DYNAMIC-FUNCTION("fGetParamValue","svStartPONumber")
        cStartCAD = DYNAMIC-FUNCTION("fGetParamValue","svStartCAD")
        cEndPONumber = DYNAMIC-FUNCTION("fGetParamValue","svEndPONumber")
        cEndCAD = DYNAMIC-FUNCTION("fGetParamValue","svEndCAD")
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
        cSort = DYNAMIC-FUNCTION("fGetParamValue","svSort")
        cStartItemNo = DYNAMIC-FUNCTION("fGetParamValue","svStartItemNo")
        lSubRpt_SubReportName = DYNAMIC-FUNCTION("fGetParamValue","svSubRpt_SubReportName") EQ "yes"
        cEndItemNo = DYNAMIC-FUNCTION("fGetParamValue","svEndItemNo")
        lAllJobNo = DYNAMIC-FUNCTION("fGetParamValue","svAllJobNo") EQ "yes"
        lAllOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svAllOrderNo") EQ "yes"
        lAllBOL = DYNAMIC-FUNCTION("fGetParamValue","svAllBOL") EQ "yes"
        cStartJobNo = DYNAMIC-FUNCTION("fGetParamValue","svStartJobNo")
        iStartJobNo2 = DYNAMIC-FUNCTION("fGetParamValue","svStartJobNo2")
        iStartOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svStartOrderNo")
        iStartBOL = DYNAMIC-FUNCTION("fGetParamValue","svStartBOL")
        cEndJobNo = DYNAMIC-FUNCTION("fGetParamValue","svEndJobNo")
        iEndJobNo2 = DYNAMIC-FUNCTION("fGetParamValue","svEndJobNo2")
        iEndOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svEndOrderNo")
        iEndBOL = DYNAMIC-FUNCTION("fGetParamValue","svEndBOL")
        lAllProdCategory = DYNAMIC-FUNCTION("fGetParamValue","svAllProdCategory") EQ "yes"
        cStartProdCategory = DYNAMIC-FUNCTION("fGetParamValue","svStartProdCategory")
        cEndProdCategory = DYNAMIC-FUNCTION("fGetParamValue","svEndProdCategory")
        lAllShift = DYNAMIC-FUNCTION("fGetParamValue","svAllShift") EQ "yes"
        iStartShift = DYNAMIC-FUNCTION("fGetParamValue","svStartShift")
        iEndShift = DYNAMIC-FUNCTION("fGetParamValue","svEndShift")
        lAllDept = DYNAMIC-FUNCTION("fGetParamValue","svAllDept") EQ "yes"
        cStartDept = DYNAMIC-FUNCTION("fGetParamValue","svStartDept")
        cEndDept = DYNAMIC-FUNCTION("fGetParamValue","svEndDept")
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttTemplate:HANDLE,
                     cAvailableColumns,
                     cSelectedColumns
                     ).

    IF lAllCustNo THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(255)
        .

    IF lAllMachine THEN
    ASSIGN
        cStartMachine = CHR(32)
        cEndMachine   = CHR(255)
        .

    IF lAllSalesRep THEN
    ASSIGN
        cStartSalesRep = CHR(32)
        cEndSalesRep   = CHR(255)
        .

    IF lAllPONumber THEN
    ASSIGN
        cStartPONumber = CHR(32)
        cEndPONumber   = CHR(255)
        .

    IF lAllCAD THEN
    ASSIGN
        cStartCAD = CHR(32)
        cEndCAD   = CHR(255)
        .

    IF lAllUserID THEN
    ASSIGN
        cStartUserID = CHR(32)
        cEndUserID   = CHR(255)
        .

    IF lAllItemNo THEN
    ASSIGN
        cStartItemNo = CHR(32)
        cEndItemNo   = CHR(255)
        .

    IF lAllJobNo THEN
    ASSIGN
        cStartJobNo = CHR(32)
        cEndJobNo   = CHR(255)
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

    IF lAllProdCategory THEN
    ASSIGN
        cStartProdCategory = CHR(32)
        cEndProdCategory   = CHR(255)
        .

    IF lAllShift THEN
    ASSIGN
        iStartShift = 0
        iEndShift   = 99999999
        .

    IF lAllDept THEN
    ASSIGN
        cStartDept = CHR(32)
        cEndDept   = CHR(255)
        .

    RUN pBuildCustList (ipcCompany, lCustList, cStartCustNo, cEndCustNo, "XX99").
