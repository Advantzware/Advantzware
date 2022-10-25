/*------------------------------------------------------------------------
    File        : api/PrintInvoice.p
    Purpose     : Returns the request data for invoice business forms

    Syntax      :

    Description : Returns the request data for invoice business forms

    Author(s)   : DEVA$!
    Created     : Mon May 09 07:33:22 EDT 2022
    Notes       :
  ----------------------------------------------------------------------*/

{api/ttArgs.i}
{api/CommonAPIProcs.i}
{ar/ttInvoice.i}
{api/ttInvoice.i}

DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdInvoiceProcs AS HANDLE NO-UNDO.

DEFINE VARIABLE oAttribute    AS system.Attribute NO-UNDO.
DEFINE VARIABLE mptrTTInvoice AS MEMPTR   NO-UNDO.
DEFINE VARIABLE hdTTHandle    AS HANDLE   NO-UNDO.

DEFINE VARIABLE lcReportHeader           AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcReportFooter           AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPageHeader             AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPageFooter             AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcInvoiceGroupHeader     AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcInvoiceGroupFooter     AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcInvoice                AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcInvoiceHeader          AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcInvoiceLineGroupHeader AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcInvoiceLineGroupFooter AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcInvoiceLine            AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcInvoiceMiscGroupHeader AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcInvoiceMiscGroupFooter AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcInvoiceMisc            AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcTaxDetailByTaxGroup    AS LONGCHAR NO-UNDO.

DEFINE VARIABLE lcInvoiceData             AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcInvoiceLineData         AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcInvoiceMiscData         AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcTaxDetailByTaxGroupData AS LONGCHAR NO-UNDO.

DEFINE VARIABLE lcConcatInvoice              AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcConcatInvoiceLine          AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcConcatInvoiceMisc          AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcConcatTaxDetailByTaxGroup  AS LONGCHAR NO-UNDO.

DEFINE VARIABLE lRecFound               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cCompany                AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNumLinesInPage         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cDisplayCompanyAddress  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBusinessFormLogo       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustomerPO             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPrintInstructions      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lPrintSetComponents     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lBatchEmail             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE dTotalTaxByTaxGroup     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iLineCount              AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMiscCount              AS INTEGER   NO-UNDO.
DEFINE VARIABLE lLastInvoice            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFirstInvoice           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE riInvoice               AS ROWID     NO-UNDO.
DEFINE VARIABLE iInvoiceID              AS INTEGER   NO-UNDO.
DEFINE VARIABLE cReportARMiscAsLine     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lReportARMiscAsLine     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lPrintAllQty            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValid                  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage                AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-APIOutbound FOR APIOutbound.

RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).
RUN pGetNumLinesInPage(ipiAPIOutboundID, OUTPUT iNumLinesInPage).
RUN pGetContentValue(ipiAPIOutboundID, "ReportARMiscAsLine", OUTPUT cReportARMiscAsLine).

lReportARMiscAsLine = LOGICAL(cReportARMiscAsLine) NO-ERROR.

IF lReportARMiscAsLine EQ ? THEN
    lReportARMiscAsLine = NO.
    
RUN spGetSessionParam("Company", OUTPUT cCompany).
 
IF ipcRequestHandler NE "" THEN DO:
    RUN VALUE(ipcRequestHandler) (
        INPUT TABLE ttArgs,
        INPUT ipiAPIOutboundID,
        INPUT ipiAPIOutboundTriggerID,
        INPUT-OUTPUT ioplcRequestData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).
    
    RETURN.
END.

RUN ar/InvoiceProcs.p PERSISTENT SET hdInvoiceProcs.

FIND FIRST bf-APIOutbound NO-LOCK
     WHERE bf-APIOutbound.apiOutboundID EQ ipiAPIOutboundID
     NO-ERROR.
IF NOT AVAILABLE bf-APIOutbound THEN DO:
    ASSIGN
        opcMessage = "No APIOutbound record found"
        oplSuccess = FALSE
        .
    RETURN.        
END. 

FIND FIRST ttArgs
     WHERE ttArgs.argType  = "ROWID"
       AND ttArgs.argKey   = "CurrentInvoiceRowID"
     NO-ERROR.
IF AVAILABLE ttArgs THEN
    riInvoice = TO-ROWID(ttArgs.argValue).

FIND FIRST ttArgs
     WHERE ttArgs.argType  = "ROWID"
       AND ttArgs.argKey   = "TTInvoiceHandle"
     NO-ERROR.
IF NOT AVAILABLE ttArgs THEN DO:
    ASSIGN
        oplSuccess = FALSE
        opcMessage = "Invalid temp-table handle"
        .
    
    RETURN.
END.

hdTTHandle = HANDLE(ttArgs.argValue) NO-ERROR.

IF NOT VALID-HANDLE (hdTTHandle) THEN DO:
    ASSIGN
        oplSuccess = FALSE
        opcMessage = "Invalid temp-table handle"
        .
    
    RETURN.        
END.

/* Code to send data from dynamic temp-table handle to static temp-table */
hdTTHandle:WRITE-XML("MEMPTR", mptrTTInvoice).

TEMP-TABLE ttInvoice:READ-XML("MEMPTR", mptrTTInvoice, "EMPTY", ?, FALSE) NO-ERROR.

oAttribute = NEW system.Attribute().
oAttribute:RequestDataType = gcRequestDataType.

FOR EACH ttInvoice
    WHERE ttInvoice.sourceRowID EQ riInvoice OR riInvoice EQ ?
    BREAK BY ttInvoice.company:
    
    ASSIGN
        lFirstInvoice = FIRST-OF(ttInvoice.company)
        lLastInvoice  = LAST-OF(ttInvoice.company)
        iInvoiceID    = ttInvoice.invoiceID
        .
        
    EMPTY TEMP-TABLE ttInv.
    EMPTY TEMP-TABLE ttInvLine.
    EMPTY TEMP-TABLE ttTaxDetail.
        
    RUN BuildData IN hdInvoiceProcs (
        INPUT  ttInvoice.sourceRowID,
        OUTPUT TABLE ttInv BY-REFERENCE,
        OUTPUT TABLE ttInvLine BY-REFERENCE,
        OUTPUT TABLE ttTaxDetail BY-REFERENCE
        ).
    
    IF NOT TEMP-TABLE ttInv:HAS-RECORDS THEN
        NEXT.
         
    RUN sys/ref/nk1look.p (cCompany, "INVPRINT", "L" , YES, YES, "" , "", OUTPUT cDisplayCompanyAddress, OUTPUT lRecFound).
    RUN FileSys_GetBusinessFormLogo(cCompany, "" /* cust */ , "" /* Location */ , OUTPUT cBusinessFormLogo, OUTPUT lValid, OUTPUT cMessage).
    
    ASSIGN
        lPrintInstructions  = LOGICAL(system.SharedConfig:Instance:GetValue("PrintInvoice_PrintInstructions"))
        lPrintSetComponents = LOGICAL(system.SharedConfig:Instance:GetValue("PrintInvoice_PrintSetComponents"))
        lBatchEmail         = LOGICAL(system.SharedConfig:Instance:GetValue("PrintInvoice_BatchEmail"))
        lPrintAllQty        = LOGICAL(system.SharedConfig:Instance:GetValue("PrintInvoice_PrintQtyAll"))
        NO-ERROR.
    
    RUN pGetRequestData ("PrintInvoice", "Invoice", OUTPUT lcInvoiceData).
    RUN pGetRequestData ("PrintInvoice", "InvoiceHeader", OUTPUT lcInvoiceHeader).
    RUN pGetRequestData ("PrintInvoice", "InvoiceGroupHeader", OUTPUT lcInvoiceGroupHeader).
    RUN pGetRequestData ("PrintInvoice", "InvoiceGroupFooter", OUTPUT lcInvoiceGroupFooter).
    
    RUN pGetRequestData ("PrintInvoice", "InvoiceLine", OUTPUT lcInvoiceLineData).
    RUN pGetRequestData ("PrintInvoice", "InvoiceLineGroupHeader", OUTPUT lcInvoiceLineGroupHeader).
    RUN pGetRequestData ("PrintInvoice", "InvoiceLineGroupFooter", OUTPUT lcInvoiceLineGroupFooter).
    
    RUN pGetRequestData ("PrintInvoice", "InvoiceMisc", OUTPUT lcInvoiceMiscData).
    RUN pGetRequestData ("PrintInvoice", "InvoiceMiscGroupHeader", OUTPUT lcInvoiceMiscGroupHeader).
    RUN pGetRequestData ("PrintInvoice", "InvoiceMiscGroupFooter", OUTPUT lcInvoiceMiscGroupFooter).
    
    RUN pGetRequestData ("PrintInvoice", "PageFooter", OUTPUT lcPageFooter).
    RUN pGetRequestData ("PrintInvoice", "PageHeader", OUTPUT lcPageHeader).
    
    RUN pGetRequestData ("PrintInvoice", "TaxDetailByTaxGroup", OUTPUT lcTaxDetailByTaxGroupData).
    
    FOR EACH ttInv:
        ASSIGN
            iLineCount = 0
            iMiscCount = 0
            .
            
        IF ttInv.sourceTable EQ "INVHEAD" THEN
            FIND FIRST inv-head NO-LOCK
                 WHERE ROWID(inv-head) EQ ttInv.sourceRowID
                 NO-ERROR.
        ELSE IF ttInv.sourceTable EQ "ARINV" THEN
            FIND FIRST ar-inv NO-LOCK
                 WHERE ROWID(ar-inv) EQ ttInv.sourceRowID
                 NO-ERROR.        
        
        IF NOT AVAILABLE inv-head AND NOT AVAILABLE ar-inv THEN
            NEXT.
        
        ASSIGN
            lcConcatInvoiceLine         = ""
            lcConcatInvoiceMisc         = ""
            lcConcatTaxDetailByTaxGroup = "".
            lcInvoice                   = lcInvoiceData
            .
            
        FOR EACH ttInvLine NO-LOCK
            WHERE ttInvLine.linkerID EQ ttInv.linkerID
            BY ttInvLine.isMisc:
    
            IF ttInvLine.isMisc AND NOT (lReportARMiscAsLine AND ttInv.sourceTable = "ARINV") THEN DO:
                IF ttInvLine.sourceTable EQ "INVMISC" THEN
                    FIND FIRST inv-misc NO-LOCK
                         WHERE ROWID(inv-misc) EQ ttInvLine.sourceRowID
                         NO-ERROR.
    
                IF ttInvLine.sourceTable EQ "ARINVL" THEN
                    FIND FIRST ar-invl NO-LOCK
                         WHERE ROWID(ar-invl) EQ ttInvLine.sourceRowID
                         NO-ERROR.
                
                IF NOT AVAILABLE inv-misc AND NOT AVAILABLE ar-invl THEN
                    NEXT.
                
                ASSIGN
                    iMiscCount    = iMiscCount + 1
                    lcInvoiceMisc = lcInvoiceMiscData
                    .
                
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcInvoiceLine, "InvoiceLinePricePerUOM", STRING(ttInvLine.pricePerUOM)).
                
                IF ttInvLine.sourceTable EQ "INVMISC" THEN
                    lcInvoiceMisc = oAttribute:ReplaceAttributes(lcInvoiceMisc, BUFFER inv-misc:HANDLE).
                ELSE IF ttInvLine.sourceTable EQ "ARINVL" THEN
                    lcInvoiceMisc = oAttribute:ReplaceAttributes(lcInvoiceMisc, BUFFER ar-invl:HANDLE).
    
                lcConcatInvoiceMisc = lcConcatInvoiceMisc + lcInvoiceMisc.
            END.
            ELSE DO:
                IF ttInvLine.sourceTable EQ "INVLINE" THEN
                    FIND FIRST inv-line NO-LOCK
                         WHERE ROWID(inv-line) EQ ttInvLine.sourceRowID
                         NO-ERROR.
    
                IF ttInvLine.sourceTable EQ "ARINVL" THEN
                    FIND FIRST ar-invl NO-LOCK
                         WHERE ROWID(ar-invl) EQ ttInvLine.sourceRowID
                         NO-ERROR.
    
                IF NOT AVAILABLE inv-line AND NOT AVAILABLE ar-invl THEN
                    NEXT.
                
                ASSIGN
                    iLineCount    = iLineCount + 1
                    lcInvoiceLine = lcInvoiceLineData
                    .
                
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcInvoiceLine, "InvoiceLinePricePerUOM", STRING(ttInvLine.pricePerUOM)).
                
                IF ttInvLine.sourceTable EQ "INVLINE" THEN
                    lcInvoiceLine = oAttribute:ReplaceAttributes(lcInvoiceLine, BUFFER inv-line:HANDLE).
                ELSE IF ttInvLine.sourceTable EQ "ARINVL" THEN
                    lcInvoiceLine = oAttribute:ReplaceAttributes(lcInvoiceLine, BUFFER ar-invl:HANDLE).
                
                lcConcatInvoiceLine = lcConcatInvoiceLine + lcInvoiceLine.
            END.
    
            IF cCustomerPO EQ "" AND ttInvLine.customerPONo NE "" THEN
                cCustomerPO = ttInvLine.customerPONo.
        END.
        
        FOR EACH ttTaxDetail
            WHERE ttTaxDetail.company   EQ ttInv.company
              AND ttTaxDetail.invoiceNo EQ ttInv.invoiceID
            BREAK BY ttTaxDetail.taxGroup:
            
            dTotalTaxByTaxGroup = dTotalTaxByTaxGroup + ttTaxDetail.taxCodeTaxAmount.
            
            IF LAST-OF(ttTaxDetail.taxGroup) THEN DO:
                lcTaxDetailByTaxGroup = lcTaxDetailByTaxGroupData.
                
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcTaxDetailByTaxGroup, "TaxCode", ttTaxDetail.taxGroup).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcTaxDetailByTaxGroup, "TaxAmountByTaxGroup", STRING(dTotalTaxByTaxGroup)).
                
                lcConcatTaxDetailByTaxGroup = lcConcatTaxDetailByTaxGroup + lcTaxDetailByTaxGroup.
                
                dTotalTaxByTaxGroup = 0.
            END.        
            
        END.
    
        lcInvoice = REPLACE(lcInvoice, "$InvoiceLines$", lcConcatInvoiceLine).
        lcInvoice = REPLACE(lcInvoice, "$InvoiceMiscs$", lcConcatInvoiceMisc).
        
        RUN pAssignCommonData(INPUT-OUTPUT lcInvoice).
        RUN pAssignCommonData(INPUT-OUTPUT lcInvoiceHeader).        
        
        lcInvoice = REPLACE(lcInvoice, "$InvoiceLineGroupHeader$", lcInvoiceLineGroupHeader).
        lcInvoice = REPLACE(lcInvoice, "$InvoiceLineGroupFooter$", lcInvoiceLineGroupFooter).
        lcInvoice = REPLACE(lcInvoice, "$InvoiceMiscGroupHeader$", lcInvoiceMiscGroupHeader).
        lcInvoice = REPLACE(lcInvoice, "$InvoiceMiscGroupFooter$", lcInvoiceMiscGroupFooter).
        lcInvoice = REPLACE(lcInvoice, "$TaxDetailByTaxGroup$", lcConcatTaxDetailByTaxGroup).

        oAttribute:UpdateRequestData(INPUT-OUTPUT lcInvoice, "LineCount", STRING(iLineCount)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcInvoice, "MiscCount", STRING(iMiscCount)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcInvoice, "FirstInvoice", STRING(lFirstInvoice)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcInvoice, "LastInvoice", STRING(lLastInvoice)).
    
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ ttInv.company
               AND cust.cust-no EQ ttInv.customerID
             NO-ERROR.
        
        ASSIGN
            lcInvoice       = oAttribute:ReplaceAttributes(lcInvoice, BUFFER cust:HANDLE)
            lcInvoiceHeader = oAttribute:ReplaceAttributes(lcInvoiceHeader, BUFFER cust:HANDLE)
            .
        
        IF ttInv.sourceTable EQ "INVHEAD" THEN DO:
            ASSIGN
                lcInvoice       = oAttribute:ReplaceAttributes(lcInvoice, BUFFER inv-head:HANDLE)
                lcInvoiceHeader = oAttribute:ReplaceAttributes(lcInvoiceHeader, BUFFER inv-head:HANDLE).
                .
        END.
        ELSE IF ttInv.sourceTable EQ "ARINV" THEN DO:
            ASSIGN
                lcInvoice       = oAttribute:ReplaceAttributes(lcInvoice, BUFFER ar-inv:HANDLE)
                lcInvoiceHeader = oAttribute:ReplaceAttributes(lcInvoiceHeader, BUFFER ar-inv:HANDLE)
                .
        END.
    
        RUN pInsertPageHeaderFooter (INPUT iNumLinesInPage, INPUT-OUTPUT lcInvoice, INPUT lcPageHeader + lcInvoiceHeader, INPUT lcPageFooter).
        
        RUN pUpdateDelimiterWithoutTrim(INPUT-OUTPUT lcInvoice, INPUT gcRequestDataType).
                
        lcConcatInvoice = lcConcatInvoice + lcInvoice.
    END.
END.

RUN pGetRequestData(INPUT "PrintInvoice", INPUT "InvoiceGroupHeader", OUTPUT lcInvoiceGroupHeader).
RUN pGetRequestData(INPUT "PrintInvoice", INPUT "InvoiceGroupFooter", OUTPUT lcInvoiceGroupFooter).
RUN pGetRequestData(INPUT "PrintInvoice", INPUT "ReportHeader", OUTPUT lcReportHeader).
RUN pGetRequestData(INPUT "PrintInvoice", INPUT "ReportFooter", OUTPUT lcReportFooter).

ioplcRequestData = REPLACE(ioplcRequestData, "$Invoices$", lcConcatInvoice).
ioplcRequestData = REPLACE(ioplcRequestData, "$InvoiceGroupHeader$", lcInvoiceGroupHeader).
ioplcRequestData = REPLACE(ioplcRequestData, "$InvoiceGroupFooter$", lcInvoiceGroupFooter).
ioplcRequestData = REPLACE(ioplcRequestData, "$ReportHeader$", lcReportHeader).
ioplcRequestData = REPLACE(ioplcRequestData, "$ReportFooter$", lcReportFooter).

oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceID", STRING(iInvoiceID)).
oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "DisplayCompanyAddress", cDisplayCompanyAddress).
oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "BatchEmail", STRING(lBatchEmail)).
oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "PrintAllQty", STRING(lPrintAllQty)).

ASSIGN
    opcMessage = ""
    oplSuccess = TRUE
    .

FINALLY:
    IF VALID-HANDLE(hdInvoiceProcs) THEN
        DELETE PROCEDURE hdInvoiceProcs.
    
    IF VALID-OBJECT(oAttribute) THEN
        DELETE OBJECT oAttribute.
END FINALLY.



/* **********************  Internal Procedures  *********************** */


PROCEDURE pAssignCommonData PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR NO-UNDO.

    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceLineTotalAmount", STRING(ttInv.amountTotalLines)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceFreightAmount", STRING(ttInv.amountTotalFreight)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "BOLID", STRING(ttInv.bolID)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "BOLDate", STRING(ttInv.bolDate)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "CarrierDescription", STRING(ttInv.carrier)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "FOB", STRING(ttInv.fob)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerPO", STRING(cCustomerPO)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalPallets", STRING(ttInv.totalPallets)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "PrintInstructions", STRING(lPrintInstructions)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "PrintSetComponenets", STRING(lPrintSetComponents)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "BusinessFormLogo", STRING(cBusinessFormLogo)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "SalesPerson1", ttInv.salesPerson[1]).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "SalesPerson2", ttInv.salesPerson[2]).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "SalesPerson3", ttInv.salesPerson[3]).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceShiptoName", ttInv.shiptoName).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceShiptoAddress1", ttInv.shiptoAddress1).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceShiptoAddress2", ttInv.shiptoAddress2).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceShiptoCity", ttInv.shiptoCity).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceShiptoState", ttInv.shiptoState).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceShiptoZip", ttInv.shiptoPostalCode).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoicePC", ttInv.invoicePC).
    
    FIND FIRST sman NO-LOCK 
         WHERE sman.company EQ ttInv.company
         AND sman.sman      EQ ttInv.salesPerson[1]
         NO-ERROR.     
    ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER sman:HANDLE).
   
END PROCEDURE.

PROCEDURE pGetRequestData:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcParentID     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDetailID     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcRequestData AS LONGCHAR  NO-UNDO.

    DEFINE VARIABLE lcHeader AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFooter AS LONGCHAR NO-UNDO.
        
    DEFINE BUFFER bf-APIOutboundDetail FOR apiOutboundDetail.
        
    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ ipcDetailID
           AND bf-APIOutboundDetail.parentID      EQ ipcParentID
         NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN
        oplcRequestData = bf-APIOutboundDetail.data.

    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ ipcDetailID + "Header"
           AND bf-APIOutboundDetail.parentID      EQ ipcParentID
         NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN DO:
        lcHeader = bf-APIOutboundDetail.data.
        oplcRequestData = REPLACE(oplcRequestData, "$" + ipcDetailID + "Header" + "$", lcHeader).
    END.
    
    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ ipcDetailID + "Footer"
           AND bf-APIOutboundDetail.parentID      EQ ipcParentID
         NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN DO:
        lcFooter = bf-APIOutboundDetail.data.
        oplcRequestData = REPLACE(oplcRequestData, "$" + ipcDetailID + "Footer" + "$", lcFooter).
    END.    
END PROCEDURE.
    