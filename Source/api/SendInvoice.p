/*------------------------------------------------------------------------
    File        : api/SendInvoice.p
    Purpose     : Returns the request data for X12 Invoice (810)

    Syntax      :

    Description : Returns the request data for X12 Invoice (810)

    Author(s)   : DEVA$!
    Created     : Wed Jan 27 07:33:22 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

{api/ttArgs.i}
{api/CommonAPIProcs.i}
{ar/ttInvoice.i}

DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.

/* Variables to store invoice line's request data */
DEFINE VARIABLE lcLineItemsData       AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatLineItemsData AS LONGCHAR  NO-UNDO.

/* Variables to store invoice surcharge request data */
DEFINE VARIABLE lcSurchargeData       AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcConcatSurchargeData AS LONGCHAR NO-UNDO.

DEFINE VARIABLE lcTaxData        AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcTaxLineData    AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE cRequestDataType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iInvoiceID       AS INTEGER   NO-UNDO.
DEFINE VARIABLE riInvoice        AS ROWID     NO-UNDO.
DEFINE VARIABLE iSECount         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotalLineCount  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLineCount       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMiscLineCount   AS INTEGER   NO-UNDO.
DEFINE VARIABLE hdInvoiceProcs   AS HANDLE    NO-UNDO.
DEFINE VARIABLE cCalcMethod      AS CHARACTER NO-UNDO.
DEFINE VARIABLE dFreightTaxPct   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lFirstItemLine   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFirstMiscLine   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cPartQualifier   AS CHARACTER NO-UNDO.

DEFINE VARIABLE cCXMLIdentity        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCXMLDeploymentMode  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCXMLShipToPrefix    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCXMLIdentityCust    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCXMLPayloadID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCXMLSharedSecret    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustomerPONoBlank   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPurchaseOrder       AS INTEGER   NO-UNDO.

DEFINE BUFFER bf-APIOutbound                FOR APIOutbound.
DEFINE BUFFER bf-line-APIOutboundDetail     FOR APIOutboundDetail.
DEFINE BUFFER bf-misc-APIOutboundDetail     FOR APIOutboundDetail.
DEFINE BUFFER bf-tax-APIOutboundDetail      FOR APIOutboundDetail.
DEFINE BUFFER bf-charge-APIOutboundDetail   FOR APIOutboundDetail.
DEFINE BUFFER bf-tax-line-APIOutboundDetail FOR APIOutboundDetail.

RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).

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
     WHERE ttArgs.argType  EQ "ROWID"
       AND ttArgs.argKey   EQ "inv-head"
    NO-ERROR.
IF NOT AVAIL ttArgs THEN 
    FIND FIRST ttArgs
         WHERE ttArgs.argType  EQ "ROWID"
           AND ttArgs.argKey   EQ "ar-inv"
         NO-ERROR.
                      
IF NOT AVAILABLE ttArgs THEN DO:
    ASSIGN
        opcMessage = "No valid inv-head or ar-inv record passed to handler"
        oplSuccess = FALSE
        .
    RETURN.
END.
    
FIND FIRST inv-head NO-LOCK
     WHERE ROWID(inv-head) EQ TO-ROWID(ttArgs.argValue)
     NO-ERROR.
IF NOT AVAILABLE  inv-head THEN 
    FIND FIRST ar-inv NO-LOCK
         WHERE ROWID(ar-inv) EQ TO-ROWID(ttArgs.argValue)
         NO-ERROR.        
IF NOT AVAILABLE inv-head AND NOT AVAILABLE ar-inv THEN DO:
    ASSIGN
        opcMessage = "Invalid inv-head or ar-inv ROWID passed to handler"
        oplSuccess = FALSE
        .
    RETURN.
END.
           
IF AVAILABLE inv-head THEN DO:
    IF NOT CAN-FIND(FIRST inv-line
                    WHERE inv-line.r-no EQ inv-head.r-no) AND
       NOT CAN-FIND(FIRST inv-misc
                    WHERE inv-misc.r-no EQ inv-head.r-no) THEN DO:
        ASSIGN
            opcMessage = "No inv-line/inv-misc records available for invoice [ " + STRING(inv-head.inv-no) + " ]"
            oplSuccess = FALSE
            .
        RETURN.
    END.
END. 
ELSE DO:
    IF NOT CAN-FIND(FIRST ar-invl
                    WHERE ar-invl.x-no EQ ar-inv.x-no) THEN DO:
        ASSIGN
            opcMessage = "No ar-invl records available for invoice [ " + STRING(ar-inv.inv-no) + " ]"
            oplSuccess = FALSE
            .
        RETURN.
    END.            
END.

IF AVAILABLE inv-head THEN
    ASSIGN
        cCompany   = inv-head.company
        iInvoiceID = inv-head.inv-no
        riInvoice  = ROWID(inv-head)
        .
ELSE
    ASSIGN
        cCompany   = ar-inv.company
        iInvoiceID = ar-inv.inv-no
        riInvoice  = ROWID(ar-inv)
        .

RUN Tax_GetCalcMethod (
    INPUT  cCompany,
    OUTPUT cCalcMethod
    ).
         
FIND FIRST bf-line-APIOutboundDetail NO-LOCK
     WHERE bf-line-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
       AND bf-line-APIOutboundDetail.detailID      EQ "ItemDetail"
       AND bf-line-APIOutboundDetail.parentID      EQ bf-APIOutbound.apiID
     NO-ERROR.

FIND FIRST bf-misc-APIOutboundDetail NO-LOCK
     WHERE bf-misc-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
       AND bf-misc-APIOutboundDetail.detailID      EQ "MiscDetail"
       AND bf-misc-APIOutboundDetail.parentID      EQ bf-APIOutbound.apiID
     NO-ERROR.
   
FIND FIRST bf-charge-APIOutboundDetail NO-LOCK
     WHERE bf-charge-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
       AND bf-charge-APIOutboundDetail.detailID      EQ "SurchargeDetail"
       AND bf-charge-APIOutboundDetail.parentID      EQ bf-APIOutbound.apiID
     NO-ERROR.
                 
FIND FIRST bf-tax-APIOutboundDetail NO-LOCK
     WHERE bf-tax-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
       AND bf-tax-APIOutboundDetail.detailID      EQ "TaxDetail"
       AND bf-tax-APIOutboundDetail.parentID      EQ bf-APIOutbound.apiID
     NO-ERROR.

FIND FIRST bf-tax-line-APIOutboundDetail NO-LOCK
     WHERE bf-tax-line-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
       AND bf-tax-line-APIOutboundDetail.detailID      EQ "TaxDetailLineItem"
       AND bf-tax-line-APIOutboundDetail.parentID      EQ bf-APIOutbound.apiID
     NO-ERROR.
    
cRequestDataType = bf-APIOutbound.requestDataType. 

RUN BuildData IN hdInvoiceProcs (
    INPUT  riInvoice,
    OUTPUT TABLE ttInv BY-REFERENCE,
    OUTPUT TABLE ttinvLine BY-REFERENCE,
    OUTPUT TABLE ttTaxDetail BY-REFERENCE
    ).

DELETE PROCEDURE hdInvoiceProcs.

FOR EACH ttInv:
    RUN pUpdateCXMLSettings(
        INPUT ttInv.company,
        INPUT ttInv.customerID,
        INPUT ttInv.shiptoID
        ).                                                           
    
    FOR EACH ttInvLine NO-LOCK:
        IF ttInvLine.quantity EQ 0 OR ttInvLine.priceTotal EQ 0 THEN
            NEXT.
        
        IF NOT ttInvLine.billable THEN
            NEXT.

        iTotalLineCount = iTotalLineCount + 1.
        
        IF ttInvLine.isMisc THEN
            iMiscLineCount = iMiscLineCount + 1.            
        ELSE
            iLineCount = iLineCount + 1.
                                    
        IF AVAILABLE bf-line-APIOutboundDetail AND NOT ttInvLine.isMisc THEN
            lcLineItemsData = bf-line-APIOutboundDetail.data.
        ELSE IF AVAILABLE bf-misc-APIOutboundDetail AND ttInvLine.isMisc THEN
            lcLineItemsData = bf-misc-APIOutboundDetail.data.
        /* No corresponding data available to populate */
        ELSE
            LEAVE.
        
        lcTaxLineData = "".
        
        IF AVAILABLE bf-tax-line-APIOutboundDetail AND NOT ttInvLine.taxable THEN
            lcTaxLineData = bf-tax-line-APIOutboundDetail.data.
        
        cPartQualifier = IF ttInvLine.customerPartID NE "" THEN 
                             "BP" 
                         ELSE 
                             "".
        ASSIGN
            lFirstItemLine = iLineCount EQ 1
            lFirstMiscLine = iMiscLineCount EQ 1
            .
        cCustomerPONoBlank = ttInvLine.customerPONoNoBlank.
        IF cCustomerPONoBlank EQ "" THEN
            cCustomerPONoBlank = "No " + STRING(ttInvLine.isMisc, "Misc/Line") + " PO".
            
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "FirstLineItem", STRING(lFirstItemLine)).  
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "FirstMiscItem", STRING(lFirstMiscLine)).                   
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemLineID", STRING(ttInvLine.lineNo)).        
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "LineNumber", STRING(ttInvLine.lineNo)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemQuantity", STRING(ttInvLine.quantity)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemUOM", STRING(ttInvLine.quantityUOM)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemPriceEach", STRING(ttInvLine.pricePerEach)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "BuyerPart", STRING(ttInvLine.customerPartID)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "PONum", STRING(ttInvLine.customerPONo)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "PONumNoBlank", cCustomerPONoBlank).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "OrderLineNumber",STRING(ttInvLine.orderLine)). 
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "OrderLineNumberOverride",STRING(ttInvLine.orderLineOverride)). 
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "OrderLineNumberOverridden",STRING(ttInvLine.orderLineOverridden)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemPriceUOM",ttInvLine.priceUOM).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemPrice", STRING(ttInvLine.pricePerUOM)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemID",ttInvLine.itemID).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemName",ttInvLine.itemName).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemDescription",ttInvLine.itemDescription).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "LineSubTotalAmount",STRING(ttInvLine.priceTotal)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "LineTotal",STRING(ttInvLine.priceTotal)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "TotalShippingAmount", STRING(ttInv.amountTotalFreight)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "PartQualifier", cPartQualifier).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "BOLID", STRING(ttInvLine.bolID)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "EDIPrice", STRING(ttInvLine.ediPrice)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "EDIPriceUOM", ttInvLine.ediPriceUOM).
                                            
        IF ttInvLine.isMisc THEN
            RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemDescription", STRING(ttInvLine.chargeDescription)).
        ELSE
            RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemDescription", STRING(ttInvLine.itemDescription)).
        
        lcLineItemsData = REPLACE(lcLineItemsData, "$TaxDetailLineItem$", lcTaxLineData).
                   
        lcConcatLineItemsData = lcConcatLineItemsData + lcLineItemsData.
                                   
        IF ttInvLine.orderID NE 0 THEN
        iPurchaseOrder = ttInvLine.orderID.
    END.

    lcTaxData = "".
               
    /* IF Freight is bill and and freight amount is not 0 */
    IF ttInv.billFreight AND ttInv.amountTotalFreight NE 0 THEN DO:
        IF AVAILABLE bf-charge-APIOutboundDetail THEN DO:   
            ASSIGN
                lcSurchargeData = bf-charge-APIOutboundDetail.data.
            
            RUN updateRequestData(INPUT-OUTPUT lcSurchargeData, "SurchargeID", "C").   /* "A" - Allowance, "C" - Charge */
            RUN updateRequestData(INPUT-OUTPUT lcSurchargeData, "SurchargeCode", "D240").  /* "D240" - Freight */
                       
            IF ttInv.amountTotalFreight EQ ttInv.amountTotalTaxableFreight THEN
                RUN updateRequestData(INPUT-OUTPUT lcSurchargeData, "SurchargeAmount", STRING(ttInv.amountTotalFreight)).
            ELSE
                RUN updateRequestData(INPUT-OUTPUT lcSurchargeData, "SurchargeAmount", STRING(ttInv.amountTotalFreight - ttInv.amountTotalTaxableFreight)).
            
            /* Do not write a tax line if freight amount and freight taxable amount is different. Handling in a different section */
            IF AVAILABLE bf-tax-APIOutboundDetail AND ttInv.amountTotalFreight EQ ttInv.amountTotalTaxableFreight THEN DO:
                lcTaxData = bf-tax-APIOutboundDetail.data.
                
                dFreightTaxPct = ttInv.frtTaxRate.
                
                IF cCalcMethod EQ "API" THEN
                    dFreightTaxPct = dFreightTaxPct * 100.
                    
                RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxType", "ST").
                RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxPercent", STRING(dFreightTaxPct)).
                RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TotalTaxDollars", STRING(ttInv.amountTotalTaxFreight)).
            END.
            
            lcSurchargeData = REPLACE(lcSurchargeData, "$TaxDetail$", lcTaxData).
            
            lcConcatSurchargeData = lcConcatSurchargeData + lcSurchargeData.
        END.
    END.

    /* Add another segment if freight taxable amount is different than total freight */
    IF ttInv.billFreight AND ttInv.amountTotalFreight NE ttInv.amountTotalTaxableFreight AND ttInv.amountTotalTaxableFreight NE 0 THEN DO:
        IF AVAILABLE bf-charge-APIOutboundDetail THEN DO:
            ASSIGN
                lcSurchargeData = bf-charge-APIOutboundDetail.data.
            
            RUN updateRequestData(INPUT-OUTPUT lcSurchargeData, "SurchargeID", "C").   /* "A" - Allowance, "C" - Charge */
            RUN updateRequestData(INPUT-OUTPUT lcSurchargeData, "SurchargeCode", "D240").  /* "D240" - Freight */
            RUN updateRequestData(INPUT-OUTPUT lcSurchargeData, "SurchargeAmount", STRING(ttInv.amountTotalTaxableFreight)).
            
            IF AVAILABLE bf-tax-APIOutboundDetail AND ttInv.amountTotalTaxableFreight NE 0 THEN DO:
                lcTaxData = bf-tax-APIOutboundDetail.data.

                dFreightTaxPct = ttInv.frtTaxRate.
                
                IF cCalcMethod EQ "API" THEN
                    dFreightTaxPct = dFreightTaxPct * 100.

                RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxType", "ST").
                RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxPercent", STRING(dFreightTaxPct)).
                RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TotalTaxDollars", STRING(ttInv.amountTotalTaxFreight)).
            END.
            
            lcSurchargeData = REPLACE(lcSurchargeData, "$TaxDetail$", lcTaxData).
            
            lcConcatSurchargeData = lcConcatSurchargeData + lcSurchargeData.
        END.
    END.

    lcTaxData = "".
    
    IF ttInv.amountTotalTaxExFreight NE 0 THEN DO:        
        IF AVAILABLE bf-tax-APIOutboundDetail THEN DO:
            lcTaxData = bf-tax-APIOutboundDetail.data.
            
            RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxType", "ST").
            RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxPercent", STRING(ROUND((ttInv.amountTotalTaxExFreight / ttInv.amountTotalTaxableExFreight) * 100, 2))).
            RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TotalTaxDollars", STRING(ttInv.amountTotalTaxExFreight)).
        END.
    END.
    
    ioplcRequestData = REPLACE(ioplcRequestData, "$ItemDetail$", lcConcatLineItemsData).
    ioplcRequestData = REPLACE(ioplcRequestData, "$TaxDetail$", lcTaxData).
    ioplcRequestData = REPLACE(ioplcRequestData, "$SurchargeDetail$", lcConcatSurchargeData).
    
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceDate", STRING(ttInv.invoiceDate)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceNum", STRING(ttInv.invoiceID)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Currency", ttInv.currency).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PayloadID", TRIM(ttInv.payloadID)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerName", ttInv.customerName).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerStreetAddress1", ttInv.customerAddress1).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerStreetAddress2", ttInv.customerAddress2).    
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerCity", ttInv.customerCity).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerState", ttInv.customerState).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerPostalCode", ttInv.customerPostalCode).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerEmail", ttInv.customerEmail).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BillToName", ttInv.customerName).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BillToStreetAddress1", ttInv.customerAddress1).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BillToStreetAddress2", ttInv.customerAddress2).    
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BillToCity", ttInv.customerCity).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BillToState", ttInv.customerState).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BillToPostalCode", ttInv.customerPostalCode).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BillToEmail", ttInv.customerEmail).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToID",ttInv.shiptoID).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToName", ttInv.shiptoName).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToStreetAddress1", ttInv.shiptoAddress1).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToStreetAddress2", ttInv.shiptoAddress2).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToCity", ttInv.shiptoCity).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToState", ttInv.shiptoState).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToPostalCode", ttInv.shiptoPostalCode).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SiteID",ttInv.siteID).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TermNetDays", STRING(ttInv.termNetDays)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TermDiscountDays", STRING(ttInv.termDiscountDays)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TermNetDueDate", STRING(ttInv.termNetDueDate)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TermDiscountDueDate", STRING(ttInv.termDiscountDueDate)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TermDiscountPercent", STRING(ttInv.termDiscountPercent)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TermDiscountAmount", STRING(ttInv.termDiscountAmount)).            
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalAmount", STRING(ttInv.amountTotal)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "OrderID", ttInv.customerPONo).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cXMLPayloadID", cCXMLPayloadID).    
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "IdentityCust", cCXMLIdentityCust).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Identity", cCXMLIdentity).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SharedSecret", cCXMLSharedSecret).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "DeploymentMode", cCXMLDeploymentMode).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SubTotalAmount", STRING(ttInv.amountTotalLines)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalTax", STRING(ttInv.amountTotalTax)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalSalesTaxableAmount", STRING(ttInv.amountTotalTaxableExFreight)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalSalesTax", STRING(ttInv.amountTotalTaxExFreight)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalShippingAmount", STRING(ttInv.amountTotalFreight)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalShippingTaxableAmount", STRING(ttInv.amountTotalTaxableFreight)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalShippingTaxAmount", STRING(ttInv.amountTotalTaxFreight)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalGrossAmt", STRING(ttInv.amountTotal)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalNetAmt", STRING(ttInv.amountTotal)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceDueDate", STRING(ttInv.invoiceDueDate)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceNotes", ttInv.invoiceNotes).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceType", ttInv.invoiceType).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalLineCount", iTotalLineCount).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "MiscLineCount", iMiscLineCount).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "LineCount", iLineCount). 
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PurchaseOrder", STRING(iPurchaseOrder)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "AreaCode", ttInv.areaCode).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Phone", ttInv.phone).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "FaxAreaCode", SUBSTRING(ttInv.fax,1,3)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Fax", SUBSTRING(ttInv.fax,4)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Country", ttInv.country).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CountryName", ttInv.countryName).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TermsDescription", ttInv.termsDesc).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "FreightPayCode", ttInv.frtPay).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "FobCode", ttInv.fob).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "FobCodeDescription", ttInv.fob).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TermsDiscountDue", ttInv.amountTotal - ttInv.termDiscountAmount ).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "IsEDIOrder", STRING(ttInv.isEDIOrder)).
    
        
    ASSIGN 
        iSECount = NUM-ENTRIES(ioplcRequestData, "~n") - 1   
        /* Subtract lines before ST and after SE segments */
        iSECount = iSECount - 4
        .
        
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SECount", STRING(iSECount)).
END.

RUN pUpdateDelimiter(
    INPUT-OUTPUT ioplcRequestData,
    INPUT        cRequestDataType
    ).                

/* To remove any unwanted blank lines */
ioplcRequestData = REPLACE(ioplcRequestData, "~n~n", "~n").

ASSIGN
    opcMessage = ""
    oplSuccess = TRUE
    .

PROCEDURE pUpdateCXMLSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipToID   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

    RUN sys/ref/nk1look.p(
        INPUT  ipcCompany,
        INPUT  "cXMLIdentity",
        INPUT  "C",           /* Logical */
        INPUT  NO,           /* check by cust */
        INPUT  NO,           /* use cust not vendor */
        INPUT  ipcCustomerID, /* cust */
        INPUT  ipcShipToID,   /* ship-to*/
        OUTPUT cReturn,
        OUTPUT lFound
        ).
        
    cCXMLIdentityCust = cReturn.
      
    RUN sys/ref/nk1look.p(
        INPUT  ipcCompany,
        INPUT  "cXMLIdentity", 
        INPUT  "L",      /* Logical */
        INPUT  NO,      /* check by cust */
        INPUT  NO,      /* use cust not vendor */
        INPUT  "",      /* cust */
        INPUT  "",      /* ship-to*/
        OUTPUT cReturn,
        OUTPUT lFound
        ).
        
    cCXMLDeploymentMode = IF cReturn EQ "YES" THEN "production" ELSE "test".
    
    RUN sys/ref/nk1look.p(
        INPUT ipcCompany,
        INPUT "cXMLShipToPrefix",
        INPUT  "C",            /* Logical */
        INPUT  YES,            /* check by cust */ 
        INPUT  YES,            /* use cust not vendor */
        INPUT  ipcCustomerID,  /* cust */
        INPUT  ipcShipToID,    /* ship-to*/
        OUTPUT cReturn, 
        OUTPUT lFound
        ).
    cCXMLShipToPrefix = TRIM(cReturn).
    
    RUN sys/ref/nk1look.p(
        INPUT  ipcCompany,
        INPUT  "cXMLSecret", 
        INPUT  "C",      /* Logical */
        INPUT  NO,      /* check by cust */
        INPUT  NO,      /* use cust not vendor */
        INPUT  "",      /* cust */
        INPUT  "",      /* ship-to*/
        OUTPUT cReturn,
        OUTPUT lFound
        ).
    cCXMLSharedSecret = cReturn.  
      
    FIND FIRST sys-ctrl-shipto NO-LOCK
         WHERE sys-ctrl-shipto.company      EQ ipcCompany
           AND sys-ctrl-shipto.name         EQ "cXMLInvoice"
           AND sys-ctrl-shipto.cust-vend    EQ YES
           AND sys-ctrl-shipto.cust-vend-no EQ ipcCustomerID
         NO-ERROR.   
         
    IF AVAILABLE sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN 
        cCXMLIdentity  = sys-ctrl-shipto.char-fld.  

    ASSIGN
        cCXMLPayloadID = STRING(NOW)
        cCXMLPayloadID = REPLACE(cCXMLPayloadID,'/','')
        cCXMLPayloadID = REPLACE(cCXMLPayloadID,' ','')
        cCXMLPayloadID = REPLACE(cCXMLPayloadID,' ','')
        cCXMLPayloadID = REPLACE(cCXMLPayloadID,':','')
        cCXMLPayloadID = REPLACE(cCXMLPayloadID,'-','')
        cCXMLPayloadID = REPLACE(cCXMLPayloadID,'.','')
        cCXMLPayloadID = cCXMLPayloadID + ''
        cCXMLPayloadID = cCXMLPayloadID + '.' + STRING(RANDOM(1000,9999),'9999') 
        .
END PROCEDURE.