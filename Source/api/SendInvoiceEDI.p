/*------------------------------------------------------------------------
    File        : api/SendInvoiceEDI.p
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
DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.

/* Variables to store invoice line's request data */
DEFINE VARIABLE lcLineItemsData       AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatLineItemsData AS LONGCHAR  NO-UNDO.

/* Variables to store invoice surcharge request data */
DEFINE VARIABLE lcSurchargeData       AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcConcatSurchargeData AS LONGCHAR NO-UNDO.

DEFINE VARIABLE lcTaxData AS LONGCHAR NO-UNDO.

DEFINE VARIABLE cRequestDataType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iInvoiceID       AS INTEGER   NO-UNDO.
DEFINE VARIABLE riInvoice        AS ROWID     NO-UNDO.
DEFINE VARIABLE iSECount         AS INTEGER   NO-UNDO.
DEFINE VARIABLE hdInvoiceProcs   AS HANDLE    NO-UNDO.

DEFINE BUFFER bf-APIOutbound              FOR APIOutbound.
DEFINE BUFFER bf-line-APIOutboundDetail   FOR APIOutboundDetail.
DEFINE BUFFER bf-misc-APIOutboundDetail   FOR APIOutboundDetail.
DEFINE BUFFER bf-tax-APIOutboundDetail    FOR APIOutboundDetail.
DEFINE BUFFER bf-charge-APIOutboundDetail FOR APIOutboundDetail.

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
                    WHERE inv-line.r-no EQ inv-head.r-no) THEN DO:
        ASSIGN
            opcMessage = "No inv-line records available for invoice [ " + STRING(inv-head.inv-no) + " ]"
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
    
cRequestDataType = bf-APIOutbound.requestDataType. 

RUN BuildData IN hdInvoiceProcs (
    INPUT  riInvoice,
    OUTPUT TABLE ttInv,
    OUTPUT TABLE ttinvLine,
    OUTPUT TABLE ttTaxDetail
    ).

FOR EACH ttInv:
    FOR EACH ttInvLine NO-LOCK:
        IF ttInvLine.quantity EQ 0 OR ttInvLine.priceTotal EQ 0 THEN
            NEXT.
        
        IF NOT ttInvLine.billable THEN
            NEXT.
            
        IF AVAILABLE bf-line-APIOutboundDetail AND NOT ttInvLine.isMisc THEN
            lcLineItemsData = bf-line-APIOutboundDetail.data.
        ELSE IF AVAILABLE bf-misc-APIOutboundDetail AND ttInvLine.isMisc THEN
            lcLineItemsData = bf-misc-APIOutboundDetail.data.
        /* No corresponding data available to populate */
        ELSE
            LEAVE.
        
        lcTaxData = "".
        
        IF AVAILABLE bf-tax-APIOutboundDetail AND NOT ttInvLine.taxable THEN DO:
            lcTaxData = bf-tax-APIOutboundDetail.data.

            RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxType", "ZZ").
            RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxPercent", "").
            RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TotalTaxDollars", "").
        END.
        
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemLineID", STRING(ttInvLine.lineNo)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemQuantity", STRING(ttInvLine.quantity)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemUOM", STRING(ttInvLine.quantityUOM)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemPrice", STRING(ttInvLine.pricePerEach)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "BuyerPart", STRING(ttInvLine.customerPartID)).
        RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "PONum", STRING(ttInvLine.customerPONo)).
        
        IF ttInvLine.isMisc THEN
            RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemDescription", STRING(ttInvLine.chargeDescription)).
        ELSE
            RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemDescription", STRING(ttInvLine.itemName)).
        
        lcLineItemsData = REPLACE(lcLineItemsData, "$TaxDetail$", lcTaxData).
                   
        lcConcatLineItemsData = lcConcatLineItemsData + lcLineItemsData.
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
            
            /* Do not write a tax line is freight amount freight taxable amount is different. Handling in a different section */
            IF AVAILABLE bf-tax-APIOutboundDetail AND ttInv.amountTotalFreight EQ ttInv.amountTotalTaxableFreight THEN DO:
                lcTaxData = bf-tax-APIOutboundDetail.data.

                RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxType", "ST").
                RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxPercent", TRIM(STRING(ROUND((ttInv.amountTotalTaxFreight / ttInv.amountTotalTaxableFreight) * 100, 2), ">>9.99<<"))).
                RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TotalTaxDollars", STRING(ttInv.amountTotalTaxFreight)).
            END.
            
            lcSurchargeData = REPLACE(lcSurchargeData, "$TaxDetail$", lcTaxData).
            
            lcConcatSurchargeData = lcConcatSurchargeData + lcSurchargeData.
        END.
    END.

    /* Add another segment if freight taxable amount is different than total freigt */
    IF ttInv.billFreight AND ttInv.amountTotalFreight NE ttInv.amountTotalTaxableFreight AND ttInv.amountTotalTaxableFreight NE 0 THEN DO:
        IF AVAILABLE bf-charge-APIOutboundDetail THEN DO:
            ASSIGN
                lcSurchargeData = bf-charge-APIOutboundDetail.data.
            
            RUN updateRequestData(INPUT-OUTPUT lcSurchargeData, "SurchargeID", "C").   /* "A" - Allowance, "C" - Charge */
            RUN updateRequestData(INPUT-OUTPUT lcSurchargeData, "SurchargeCode", "D240").  /* "D240" - Freight */
            RUN updateRequestData(INPUT-OUTPUT lcSurchargeData, "SurchargeAmount", STRING(ttInv.amountTotalTaxableFreight)).

            IF AVAILABLE bf-tax-APIOutboundDetail AND ttInv.amountTotalTaxableFreight NE 0 THEN DO:
                lcTaxData = bf-tax-APIOutboundDetail.data.

                RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxType", "ST").
                RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxPercent", TRIM(STRING(ROUND((ttInv.amountTotalTaxFreight / ttInv.amountTotalTaxableFreight) * 100, 2), ">>9.99<<"))).
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
            RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxPercent", TRIM(STRING(ROUND((ttInv.amountTotalTaxExFreight / ttInv.amountTotalTaxableExFreight) * 100, 2), ">>9.99<<"))).
            RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TotalTaxDollars", STRING(ttInv.amountTotalTaxExFreight)).
        END.
    END.
    
    ioplcRequestData = REPLACE(ioplcRequestData, "$ItemDetail$", lcConcatLineItemsData).
    ioplcRequestData = REPLACE(ioplcRequestData, "$TaxDetail$", lcTaxData).
    ioplcRequestData = REPLACE(ioplcRequestData, "$SurchargeDetail$", lcConcatSurchargeData).
    
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceDate", STRING(ttInv.invoiceDate)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceNum", STRING(ttInv.invoiceID)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Currency", "USD").
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerName", ttInv.customerName).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BillToStreetData", ttInv.customerAddress1 + " " + ttInv.customerAddress2).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerCity", ttInv.customerCity).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerState", ttInv.customerState).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerPostalCode", ttInv.customerPostalCode).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CompanyName", ttInv.companyName).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CompanyStreetData", ttInv.companyAddress1 + " " + ttInv.companyAddress2).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CompanyCity", ttInv.companyCity).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CompanyState", ttInv.companyState).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CompanyPostalCode", ttInv.companyPostalCode).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShiptoName", ttInv.shiptoName).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToStreetData", ttInv.shiptoAddress1 + " " + ttInv.shiptoAddress2).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShiptoCity", ttInv.shiptoCity).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShiptoState", ttInv.shiptoState).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShiptoPostalCode", ttInv.shiptoPostalCode).        
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalAmount", REPLACE(TRIM(STRING(ttInv.amountTotal, "->>>>>>>>>.99")), ".", "")).
    
    ASSIGN 
        iSECount = NUM-ENTRIES(ioplcRequestData, "~~") - 1   
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
