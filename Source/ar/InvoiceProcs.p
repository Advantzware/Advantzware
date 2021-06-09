
/*------------------------------------------------------------------------
    File        : InvoiceProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : DEVA$!
    Created     : Wed Jan 27 11:09:04 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ar/ttInvoice.i}

/* ********************  Preprocessor Definitions  ******************** */

/* **********************  Internal Procedures  *********************** */

PROCEDURE BuildData:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriInvoice  AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttInv.
    DEFINE OUTPUT PARAMETER TABLE FOR ttInvLine.
    DEFINE OUTPUT PARAMETER TABLE FOR ttTaxDetail.
    
    RUN pBuildData(
        INPUT ipriInvoice
        ).
END PROCEDURE.

PROCEDURE pAssignCommonHeaderData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given primary inputs, assign common fields to temp-table header
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttInv FOR ttInv.

    DEFINE VARIABLE iDueOnMonth AS INTEGER NO-UNDO.
    DEFINE VARIABLE iDueOnDay   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNetDays    AS INTEGER NO-UNDO.
    DEFINE VARIABLE dDiscPct    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iDiscDays   AS DECIMAL NO-UNDO. 
    DEFINE VARIABLE lError      AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-cust    FOR cust.
    DEFINE BUFFER bf-shipto  FOR shipto.
    DEFINE BUFFER bf-notes   FOR notes.

    FIND FIRST bf-cust NO-LOCK 
        WHERE bf-cust.company EQ ipbf-ttInv.company
          AND bf-cust.cust-no EQ ipbf-ttInv.customerID
        NO-ERROR.
    IF AVAILABLE bf-cust THEN 
        ipbf-ttInv.customerEmail = bf-cust.email.
        
    FIND FIRST bf-shipto NO-LOCK 
         WHERE bf-shipto.company EQ ipbf-ttInv.company
           AND bf-shipto.cust-no EQ ipbf-ttInv.customerID
           AND bf-shipto.ship-id EQ ipbf-ttInv.shipToID
         NO-ERROR.
    IF AVAILABLE bf-shipto THEN 
        ASSIGN 
            ipbf-ttInv.shipToID         = bf-shipto.ship-id
            ipbf-ttInv.shiptoName       = bf-shipto.ship-name
            ipbf-ttInv.shiptoAddress1   = bf-shipto.ship-addr[1]
            ipbf-ttInv.shiptoAddress2   = bf-shipto.ship-addr[2]
            ipbf-ttInv.shiptoCity       = bf-shipto.ship-city
            ipbf-ttInv.shiptoState      = bf-shipto.ship-state
            ipbf-ttInv.shiptoPostalCode = bf-shipto.ship-zip
            ipbf-ttInv.siteID           =  bf-shipto.siteId
            .

    RUN Credit_GetTerms (
        INPUT  ipbf-ttInv.company,
        INPUT  ipbf-ttInv.terms,
        OUTPUT iDueOnMonth,
        OUTPUT iDueOnDay,
        OUTPUT iNetDays, 
        OUTPUT dDiscPct,  
        OUTPUT iDiscDays, 
        OUTPUT lError         
        ).
    IF NOT lError THEN
        ASSIGN
            ipbf-ttInv.termNetDays         = iNetDays
            ipbf-ttInv.termNetDueDate      = ipbf-ttInv.invoiceDate + ipbf-ttInv.termNetDays
            ipbf-ttInv.termDiscountDays    = iDiscDays
            ipbf-ttInv.termDiscountDueDate = ipbf-ttInv.invoiceDate + ipbf-ttInv.termDiscountDays
            ipbf-ttInv.termDiscountPercent = dDiscPct
            ipbf-ttInv.termDiscountAmount  = ipbf-ttInv.amountTotal * (ipbf-ttInv.termDiscountPercent / 100)
            .
    
    FOR EACH bf-notes NO-LOCK
        WHERE bf-notes.rec_key EQ ipbf-ttInv.invoiceRecKey:
        ipbf-ttInv.invoiceNotes = ipbf-ttInv.invoiceNotes + bf-notes.note_text.
    END.
                
END PROCEDURE.

PROCEDURE pAssignCommonLineData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given buffers and primary inputs, assign common fields to temp-table header
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttInv     FOR ttInv.
    DEFINE PARAMETER BUFFER ipbf-ttInvLine FOR ttInvLine.
    
    DEFINE BUFFER bf-oe-ord  FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
    
    DEFINE VARIABLE lError        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
    
    IF ipbf-ttInv.customerPONo EQ "" THEN 
        ipbf-ttInv.customerPONo = ipbf-ttInvLine.customerPONo.

    FIND FIRST bf-oe-bolh NO-LOCK 
         WHERE bf-oe-bolh.b-no EQ ipbf-ttInvLine.bNo 
         NO-ERROR.
    IF AVAILABLE bf-oe-bolh THEN
        ipbf-ttInvLine.bolID = bf-oe-bolh.bol-no.
        
    FIND FIRST bf-oe-ord NO-LOCK
         WHERE bf-oe-ord.company EQ ipbf-ttInv.company
           AND bf-oe-ord.ord-no EQ ipbf-ttInvLine.orderID
         NO-ERROR.
    IF AVAILABLE bf-oe-ord THEN DO:
        IF ipbf-ttInv.customerPONo EQ "" THEN 
            ipbf-ttInv.customerPONo EQ bf-oe-ord.po-no.
            
        IF ipbf-ttInv.payloadID EQ "" THEN 
            ipbf-ttInv.payloadID = bf-oe-ord.spare-char-3.
            
        FIND FIRST bf-oe-ordl NO-LOCK 
             WHERE bf-oe-ordl.company EQ bf-oe-ord.company
               AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
               AND bf-oe-ordl.i-no    EQ ipbf-ttInvLine.itemID
               AND bf-oe-ordl.line    EQ ipbf-ttInvLine.orderLine
             NO-ERROR.
        IF AVAILABLE bf-oe-ordl THEN DO: 
            ASSIGN 
                ipbf-ttInvLine.quantityOrderOriginal    = bf-oe-ordl.spare-dec-1
                ipbf-ttInvLine.quantityOrderOriginalUOM = bf-oe-ordl.spare-char-2
                .
            
            IF ipbf-ttInvLine.quantityOrderOriginalUOM NE ""
                AND ipbf-ttInvLine.quantityOrderOriginalUOM NE ipbf-ttInvLine.quantityInvoicedUOM THEN DO: 
                RUN Conv_QuantityFromUOMtoUOM(
                    INPUT  bf-oe-ordl.company, 
                    INPUT  bf-oe-ordl.i-no, 
                    INPUT  "FG", 
                    INPUT  ipbf-ttInvLine.quantityInvoiced, 
                    INPUT  ipbf-ttInvLine.quantityInvoicedUOM, 
                    INPUT  ipbf-ttInvLine.quantityOrderOriginalUOM, 
                    INPUT  0, 
                    INPUT  0, 
                    INPUT  0, 
                    INPUT  0, 
                    INPUT  bf-oe-ordl.cas-cnt, 
                    OUTPUT ipbf-ttInvLine.quantity, 
                    OUTPUT lError, 
                    OUTPUT cErrorMessage
                    ).
                
                IF ipbf-ttInvLine.quantity EQ 0 THEN 
                    ipbf-ttInvLine.quantity = ipbf-ttInvLine.quantityInvoiced.
                
                ipbf-ttInvLine.quantityUOM = ipbf-ttInvLine.quantityOrderOriginalUOM.
            END.    
        END.
    END.
    IF ipbf-ttInvLine.quantity EQ 0 THEN 
        ASSIGN 
            ipbf-ttInvLine.quantity    = ipbf-ttInvLine.quantityInvoiced
            ipbf-ttInvLine.quantityUOM = ipbf-ttInvLine.quantityInvoicedUOM
            .
/*    IF ttInvLine.taxable THEN DO:                                                                                                  */
/*        ASSIGN                                                                                                                     */
/*            ipbf-ttInvLine.amountTax               = ipbf-ttInvLine.amountTaxExFreight + ipbf-ttInvLine.amountFreightTax           */
/*            ipbf-ttInvLine.amountTaxable           = ipbf-ttInvLine.amountTaxableExFreight + ipbf-ttInvLine.amountTaxableFreight   */
/*            ipbf-ttInv.amountTotalTaxableExFreight = ipbf-ttInv.amountTotalTaxableExFreight + ipbf-ttInvLine.amountTaxableExFreight*/
/*            ipbf-ttInv.amountTotalTaxExFreight     = ipbf-ttInv.amountTotalTaxExFreight + ipbf-ttInvLine.amountTaxExFreight        */
/*            ipbf-ttInv.amountTotalTaxable          = ipbf-ttInv.amountTotalTaxable + ipbf-ttInvLine.amountTaxable                  */
/*            .                                                                                                                      */
/*    END.                                                                                                                           */
END PROCEDURE.

PROCEDURE pBuildData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given ROWID - determine ROWID type and build data for a given data type
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriInvoice AS ROWID NO-UNDO.
    
    DEFINE BUFFER bf-inv-head FOR inv-head.
    DEFINE BUFFER bf-ar-inv   FOR ar-inv.
    
    EMPTY TEMP-TABLE ttInv.
    EMPTY TEMP-TABLE ttInvLIne.
    
    FIND FIRST bf-ar-inv NO-LOCK
         WHERE ROWID(bf-ar-inv) EQ ipriInvoice
         NO-ERROR.
    IF NOT AVAILABLE bf-ar-inv THEN
        FIND FIRST bf-inv-head NO-LOCK
             WHERE ROWID(bf-inv-head) EQ ipriInvoice
             NO-ERROR.
    IF AVAILABLE bf-inv-head THEN 
        RUN pBuildDataForUnposted(
            BUFFER bf-inv-head
            ).
    ELSE 
        RUN pBuildDataForPosted(
            BUFFER bf-ar-inv
            ).

END PROCEDURE.

PROCEDURE pBuildDataForPosted PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given ar-inv buffer - Initialize and process the buffer to create the cXML 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ar-inv FOR ar-inv.
    
    DEFINE BUFFER bf-ar-invl  FOR ar-invl.
    DEFINE BUFFER bf-oe-ordl  FOR oe-ordl.
    DEFINE BUFFER bf-oe-ordm  FOR oe-ordm.
    DEFINE BUFFER bf-oe-ord   FOR oe-ord.
             
    DEFINE VARIABLE dTaxTotal        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvoiceTotal    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvoiceSubTotal AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFirst           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFirstLine       AS LOGICAL   NO-UNDO.
    
    IF AVAILABLE ipbf-ar-inv THEN DO:
        CREATE ttInv.
        ASSIGN             
            ttInv.invoiceRecKey      = ipbf-ar-inv.rec_key
            ttInv.invoiceDate        = ipbf-ar-inv.inv-date 
            ttInv.invoiceID          = ipbf-ar-inv.inv-no
            ttInv.customerID         = ipbf-ar-inv.cust-no
            ttInv.customerName       = ipbf-ar-inv.cust-name
            ttInv.customerAddress1   = ipbf-ar-inv.addr[1]
            ttInv.customerAddress2   = ipbf-ar-inv.addr[2]
            ttInv.customerCity       = ipbf-ar-inv.city
            ttInv.customerState      = ipbf-ar-inv.state
            ttInv.customerPostalCode = ipbf-ar-inv.zip
            ttInv.company            = ipbf-ar-inv.company
            ttInv.taxGroup           = ipbf-ar-inv.tax-code
            ttInv.amountTotal        = ipbf-ar-inv.t-sales 
            ttInv.billFreight        = ipbf-ar-inv.f-bill
            ttInv.amountTotalFreight = ipbf-ar-inv.freight 
            ttInv.amountTotalTax     = ipbf-ar-inv.tax-amt
            ttInv.shiptoID           = ipbf-ar-inv.ship-id
            ttInv.terms              = ipbf-ar-inv.terms            
            ttInv.invoiceDueDate     = ipbf-ar-inv.due-date
            ttInv.customerPONo       = ipbf-ar-inv.po-no
            . 

        RUN Tax_CalculateForARInvWithDetail(
            INPUT  ROWID(ipbf-ar-inv),
            INPUT  ipbf-ar-inv.loc,
            INPUT  "QUOTATION",
            INPUT  NO,
            INPUT  "GetTaxAmount",
            OUTPUT dTaxTotal,
            OUTPUT dInvoiceTotal,
            OUTPUT dInvoiceSubTotal,
            OUTPUT TABLE ttTaxDetail,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).

        RUN pAssignCommonHeaderData(
            BUFFER ttInv
            ).
        
        lFirstLine = TRUE.
             
        FOR EACH bf-ar-invl NO-LOCK
            WHERE bf-ar-invl.x-no EQ ipbf-ar-inv.x-no:
                
            CREATE ttInvLine.
            ASSIGN 
                ttInvLine.invoiceID              = ttInv.invoiceID
                ttInvLine.company                = ttInv.company
                ttInvLine.lineNo                 = bf-ar-invl.line
                ttInvLine.orderID                = bf-ar-invl.ord-no
                ttInvLine.orderLineOverride      = bf-ar-invl.e-num
                ttInvLine.quantityInvoiced       = bf-ar-invl.inv-qty
                ttInvLine.quantityInvoicedUOM    = IF bf-ar-invl.pr-qty-uom NE "" THEN
                                                       bf-ar-invl.pr-qty-uom
                                                   ELSE
                                                       "EA"
                ttInvLine.pricePerUOM            = bf-ar-invl.unit-pr * (1 - (bf-ar-invl.disc / 100))
                ttInvLine.priceUOM               = bf-ar-invl.pr-uom
                ttInvLine.customerPartID         = bf-ar-invl.part-no
                ttInvLine.itemID                 = bf-ar-invl.i-no
                ttInvLine.itemName               = bf-ar-invl.i-name
                ttInvLine.itemDescription        = bf-ar-invl.i-dscr
                ttInvLine.priceTotal             = bf-ar-invl.amt
                ttInvLine.taxable                = bf-ar-invl.tax
                ttInvLine.billable               = TRUE
                ttInvLine.amountTaxableExFreight = ttInvLine.priceTotal
                ttInvLine.amountFreight          = bf-ar-invl.t-freight
                ttInvLine.customerPONo           = bf-ar-invl.po-no
                ttInvLine.taxGroup               = ttInv.taxGroup
                ttInvLine.isMisc                 = bf-ar-invl.misc
                ttInvLine.bNo                    = bf-ar-invl.b-no
                .
            
            IF ttInvLine.isMisc THEN
                ttInvLine.pricePerEach = IF bf-ar-invl.inv-qty EQ 0 THEN 
                                             bf-ar-invl.amt
                                         ELSE
                                             (bf-ar-invl.amt / bf-ar-invl.inv-qty).
            ELSE
                ttInvLine.pricePerEach = (bf-ar-invl.amt / bf-ar-invl.inv-qty).
            
            IF ttInvLine.isMisc THEN
                ttInvLine.billable = bf-ar-invl.billable.
                           
            IF NOT ttInvLine.billable THEN 
                NEXT.
                            
            lFirst = TRUE.
            
            FOR EACH ttTaxDetail
                WHERE ttTaxDetail.invoiceLineType   EQ "ARINVL"
                  AND ttTaxDetail.invoiceLineRecKey EQ bf-ar-invl.rec_key
                  AND NOT ttTaxDetail.isFreight:
                ASSIGN
                    ttInvLine.amountTaxExFreight      = ttInvLine.amountTaxExFreight + ttTaxDetail.taxCodeTaxAmount
                    ttInvLine.taxRate                 = ttInvLine.taxRate + ttTaxDetail.taxCodeRate
                    ttInv.amountTotalTaxExFreight     = ttInv.amountTotalTaxExFreight + ttTaxDetail.taxCodeTaxAmount
                    ttInvLine.amountTax               = ttInvLine.amountTax + ttTaxDetail.taxCodeTaxAmount
                    .
                
                IF lFirst THEN
                    ASSIGN
                        ttInvLine.amountTaxable           = ttInvLine.amountTaxable + ttTaxDetail.taxCodeTaxableAmount
                        ttInv.amountTotalTaxable          = ttInv.amountTotalTaxable + ttTaxDetail.taxCodeTaxableAmount
                        ttInv.amountTotalTaxableExFreight = ttInv.amountTotalTaxableExFreight + ttTaxDetail.taxCodeTaxableAmount
                        lFirst                            = FALSE
                        .
            END.
            
            lFirst = TRUE.
            
            FOR EACH ttTaxDetail
                WHERE ttTaxDetail.invoiceLineType   EQ "ARINVL"
                  AND ttTaxDetail.invoiceLineRecKey EQ bf-ar-invl.rec_key
                  AND ttTaxDetail.isFreight:
                /* Populate rate only for the first item. Rate stays same for all the lines */
                IF lFirstLine THEN
                    ttInv.frtTaxRate = ttInv.frtTaxRate + ttTaxDetail.taxCodeRate.
                    
                ASSIGN
                    ttInv.amountTotalTaxFreight     = ttInv.amountTotalTaxFreight + ttTaxDetail.taxCodeTaxAmount
                    ttInvLine.amountTax             = ttInvLine.amountTax + ttTaxDetail.taxCodeTaxAmount
                    .
                
                IF lFirst THEN
                    ASSIGN
                        ttInvLine.amountTaxableFreight  = ttInvLine.amountTaxableFreight + ttTaxDetail.taxCodeTaxableAmount
                        ttInvLine.amountTaxable         = ttInvLine.amountTaxable + ttTaxDetail.taxCodeTaxableAmount
                        ttInv.amountTotalTaxableFreight = ttInv.amountTotalTaxableFreight + ttTaxDetail.taxCodeTaxableAmount
                        ttInv.amountTotalTaxable        = ttInv.amountTotalTaxable + ttTaxDetail.taxCodeTaxableAmount
                        lFirst                          = FALSE
                        .
            END.
                        
            ttInv.amountTotalLines        = ttInv.amountTotalLines + bf-ar-invl.amt.
            
            IF NOT ttInvLine.isMisc THEN DO:
                FIND FIRST bf-oe-ordl NO-LOCK
                     WHERE bf-oe-ordl.company EQ bf-ar-invl.company
                       AND bf-oe-ordl.i-no    EQ bf-ar-invl.i-no
                       AND bf-oe-ordl.ord-no  EQ bf-ar-invl.ord-no
                     NO-ERROR.
    
                IF AVAILABLE bf-oe-ordl THEN DO:
                    ttInvLine.orderLine = bf-oe-ordl.line.

                    IF ttInvLine.customerPONo EQ "" THEN
                        ttInvLine.customerPONo = bf-oe-ordl.po-no.                
                END.
            END.
            ELSE DO:
                FIND FIRST bf-oe-ordm NO-LOCK
                     WHERE bf-oe-ordm.company EQ bf-ar-invl.company
                       AND bf-oe-ordm.charge  EQ bf-ar-invl.prep-charge
                       AND bf-oe-ordm.ord-no  EQ bf-ar-invl.ord-no
                     NO-ERROR.            
                IF AVAILABLE bf-oe-ordm THEN DO:
                    ttInvLine.orderLine = bf-oe-ordm.line.
                    
                    IF ttInvLine.customerPONo EQ "" THEN
                        ttInvLine.customerPONo = bf-oe-ordm.po-no.
                END.            
            END.
            
            ttInvLine.orderLineOverridden = IF ttInvLine.orderLineOverride GT 0 THEN
                                                ttInvLine.orderLineOverride
                                            ELSE
                                                ttInvLine.orderLine.  

            IF ttInvLine.customerPONo EQ "" THEN DO:
                FIND FIRST bf-oe-ord NO-LOCK
                     WHERE bf-oe-ord.company EQ bf-ar-invl.company                       
                       AND bf-oe-ord.ord-no  EQ bf-ar-invl.ord-no
                     NO-ERROR.     
                IF AVAILABLE bf-oe-ord THEN
                    ttInvLine.customerPONo = bf-oe-ord.po-no. 
            END.
   
            RUN pAssignCommonLineData(
                BUFFER ttInv, 
                BUFFER ttInvLine
                ).            
            
            /* Set this to false only if a line found that is not misc, taxable and invoice's freight is billable  */
            IF ttInv.billFreight AND NOT ttInvLine.isMisc AND ttInvLine.taxable AND ttInvLine.amountFreight GT 0 THEN
                lFirstLine = FALSE.
        END.
        
        ttInv.amountTotal = ttInv.amountTotalLines + ttInv.amountTotalTax.
        
        IF ttInv.billFreight THEN 
            ttInv.amountTotal = ttInv.amountTotal + ttInv.amountTotalFreight.
        
        IF ttInv.amountTotal LT 0 THEN
            ttInv.invoiceType = "CR".

    END.
END PROCEDURE.

PROCEDURE pBuildDataForUnposted PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given inv-head buffer - Initialize and process the buffer to create the cXML 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-inv-head FOR inv-head.
    
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER bf-inv-misc FOR inv-misc.
    DEFINE BUFFER bf-oe-ordl  FOR oe-ordl.
    DEFINE BUFFER bf-oe-ordm  FOR oe-ordm.
    DEFINE BUFFER bf-oe-ord   FOR oe-ord.
    
    DEFINE VARIABLE dTaxTotal        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvoiceTotal    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvoiceSubTotal AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFirst           AS LOGICAL   NO-UNDO.
    
    IF AVAILABLE ipbf-inv-head THEN DO:
        CREATE ttInv.
        ASSIGN             
            ttInv.invoiceRecKey      = ipbf-inv-head.rec_key
            ttInv.invoiceDate        = ipbf-inv-head.inv-date 
            ttInv.invoiceID          = ipbf-inv-head.inv-no
            ttInv.customerID         = ipbf-inv-head.cust-no
            ttInv.customerName       = ipbf-inv-head.cust-name
            ttInv.customerAddress1   = ipbf-inv-head.addr[1]
            ttInv.customerAddress2   = ipbf-inv-head.addr[2]
            ttInv.customerCity       = ipbf-inv-head.city
            ttInv.customerState      = ipbf-inv-head.state
            ttInv.customerPostalCode = ipbf-inv-head.zip
            ttInv.company            = ipbf-inv-head.company
            ttInv.taxGroup           = ipbf-inv-head.tax-gr
            ttInv.amountTotal        = ipbf-inv-head.t-inv-rev
            ttInv.billFreight        = ipbf-inv-head.f-bill
            ttInv.amountTotalFreight = ipbf-inv-head.t-inv-freight 
            ttInv.amountTotalTax     = ipbf-inv-head.t-inv-tax
            ttInv.shiptoID           = ipbf-inv-head.sold-no
            ttInv.terms              = ipbf-inv-head.terms            
            ttInv.invoiceDueDate     = DYNAMIC-FUNCTION("GetInvDueDate", ttInv.invoiceDate, ttInv.company ,ttInv.terms).            
            . 

        RUN Tax_CalculateForInvHeadWithDetail(
            INPUT  ROWID(ipbf-inv-head),
            INPUT  "",
            INPUT  "QUOTATION",
            INPUT  NO,
            INPUT  "GetTaxAmount",
            OUTPUT dTaxTotal,
            OUTPUT dInvoiceTotal,
            OUTPUT dInvoiceSubTotal,
            OUTPUT TABLE ttTaxDetail,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).

        RUN pAssignCommonHeaderData(
            BUFFER ttInv
            ).
                         
        FOR EACH bf-inv-line NO-LOCK
            WHERE bf-inv-line.r-no EQ ipbf-inv-head.r-no:

            CREATE ttInvLine.
            ASSIGN 
                ttInvLine.invoiceID              = ttInv.invoiceID
                ttInvLine.company                = ttInv.company
                ttInvLine.lineNo                 = bf-inv-line.line
                ttInvLine.orderLineOverride      = bf-inv-line.e-num
                ttInvLine.orderID                = bf-inv-line.ord-no
                ttInvLine.orderLine              = bf-inv-line.line
                ttInvLine.quantityInvoiced       = bf-inv-line.inv-qty
                ttInvLine.quantityInvoicedUOM    = IF bf-inv-line.pr-qty-uom NE "" THEN
                                                       bf-inv-line.pr-qty-uom
                                                   ELSE
                                                       "EA"
                ttInvLine.pricePerUOM            = bf-inv-line.price * (1 - (bf-inv-line.disc / 100))
                ttInvLine.pricePerEach           = bf-inv-line.t-price / bf-inv-line.inv-qty
                ttInvLine.priceUOM               = bf-inv-line.pr-uom
                ttInvLine.customerPartID         = bf-inv-line.part-no
                ttInvLine.itemID                 = bf-inv-line.i-no
                ttInvLine.itemName               = bf-inv-line.i-name
                ttInvLine.itemDescription        = bf-inv-line.i-dscr
                ttInvLine.priceTotal             = bf-inv-line.t-price
                ttInvLine.taxable                = bf-inv-line.tax
                ttInvLine.billable               = TRUE
                ttInvLine.customerPONo           = bf-inv-line.po-no
                ttInvLine.taxGroup               = ttInv.taxGroup
                ttInvLine.isMisc                 = FALSE
                ttInvLine.bNo                    = bf-inv-line.b-no
                .

            lFirst = TRUE.
            
            FOR EACH ttTaxDetail
                WHERE ttTaxDetail.invoiceLineType   EQ "INVLINE"
                  AND ttTaxDetail.invoiceLineRecKey EQ bf-inv-line.rec_key:
                ASSIGN
                    ttInvLine.amountTaxExFreight      = ttInvLine.amountTaxExFreight + ttTaxDetail.taxCodeTaxAmount
                    ttInvLine.taxRate                 = ttInvLine.taxRate + ttTaxDetail.taxCodeRate
                    ttInv.amountTotalTaxExFreight     = ttInv.amountTotalTaxExFreight + ttTaxDetail.taxCodeTaxAmount
                    ttInvLine.amountTax               = ttInvLine.amountTax + ttTaxDetail.taxCodeTaxAmount
                    .
                
                IF lFirst THEN
                    ASSIGN
                        ttInvLine.amountTaxable           = ttInvLine.amountTaxable + ttTaxDetail.taxCodeTaxableAmount
                        ttInv.amountTotalTaxable          = ttInv.amountTotalTaxable + ttTaxDetail.taxCodeTaxableAmount
                        ttInv.amountTotalTaxableExFreight = ttInv.amountTotalTaxableExFreight + ttTaxDetail.taxCodeTaxableAmount
                        lFirst                            = FALSE
                        .
            END.
            
            ttInv.amountTotalLines        = ttInv.amountTotalLines + bf-inv-line.t-price.

            FIND FIRST bf-oe-ordl NO-LOCK
                 WHERE bf-oe-ordl.company EQ bf-inv-line.company
                   AND bf-oe-ordl.i-no    EQ bf-inv-line.i-no
                   AND bf-oe-ordl.ord-no  EQ bf-inv-line.ord-no
                NO-ERROR.

            IF AVAILABLE bf-oe-ordl THEN DO:
                ttInvLine.orderLine = bf-oe-ordl.line.
                
                IF ttInvLine.customerPONo EQ "" THEN
                    ttInvLine.customerPONo = bf-oe-ordl.po-no.                
            END.
            
            ttInvLine.orderLineOverridden = IF ttInvLine.orderLineOverride GT 0 THEN
                                                ttInvLine.orderLineOverride
                                            ELSE
                                                ttInvLine.orderLine. 

            IF ttInvLine.customerPONo EQ "" THEN DO:
                FIND FIRST bf-oe-ord NO-LOCK
                     WHERE bf-oe-ord.company EQ bf-inv-line.company                       
                       AND bf-oe-ord.ord-no  EQ bf-inv-line.ord-no
                     NO-ERROR.     
                IF AVAILABLE bf-oe-ord THEN
                    ttInvLine.customerPONo = bf-oe-ord.po-no. 
            END.

            RUN pAssignCommonLineData(
                BUFFER ttInv, 
                BUFFER ttInvLine
                ).            
                
        END.
        FOR EACH bf-inv-misc NO-LOCK
            WHERE bf-inv-misc.r-no EQ ipbf-inv-head.r-no:
                
            CREATE ttInvLine.
            ASSIGN
                ttInvLine.invoiceID              = ttInv.invoiceID
                ttInvLine.company                = ttInv.company
                ttInvLine.lineNo                 = bf-inv-misc.line
                ttInvLine.orderLine              = bf-inv-misc.line
                ttInvLine.orderLineOverride      = bf-inv-misc.spare-int-4                
                ttInvLine.quantityInvoiced       = 1
                ttInvLine.quantityInvoicedUOM    = "EA"
                ttInvLine.pricePerUOM            = bf-inv-misc.amt
                ttInvLine.priceUOM               = "EA"
                ttInvLine.pricePerEach           = bf-inv-misc.amt
                ttInvLine.itemID                 = bf-inv-misc.inv-i-no
                ttInvLine.charge                 = bf-inv-misc.charge
                ttInvLine.chargeDescription      = bf-inv-misc.dscr
                ttInvLine.priceTotal             = bf-inv-misc.amt
                ttInvLine.taxable                = bf-inv-misc.tax
                ttInvLine.billable               = bf-inv-misc.bill EQ "Y"
                ttInvLine.customerPONo           = bf-inv-misc.po-no
                ttInvLine.taxGroup               = ttInv.taxGroup
                ttInvLine.isMisc                 = TRUE
                .
            
            IF NOT ttInvLine.billable THEN
                NEXT.
                
            lFirst = TRUE.
             
            FOR EACH ttTaxDetail
                WHERE ttTaxDetail.invoiceLineType EQ "INVMISC"
                AND ttTaxDetail.invoiceLineRecKey EQ bf-inv-misc.rec_key:
                ASSIGN
                    ttInvLine.amountTaxExFreight      = ttInvLine.amountTaxExFreight + ttTaxDetail.taxCodeTaxAmount
                    ttInvLine.amountTax               = ttInvLine.amountTax + ttTaxDetail.taxCodeTaxAmount
                    ttInvLine.taxRate                 = ttInvLine.taxRate + ttTaxDetail.taxCodeRate
                    ttInv.amountTotalTaxExFreight     = ttInv.amountTotalTaxExFreight + ttTaxDetail.taxCodeTaxAmount
                    .
                
                IF lFirst THEN
                    ASSIGN
                        ttInvLine.amountTaxable           = ttInvLine.amountTaxable + ttTaxDetail.taxCodeTaxableAmount
                        ttInv.amountTotalTaxableExFreight = ttInv.amountTotalTaxableExFreight + ttTaxDetail.taxCodeTaxableAmount
                        ttInv.amountTotalTaxable          = ttInv.amountTotalTaxable + ttTaxDetail.taxCodeTaxableAmount
                        lFirst                            = FALSE
                        .    
            END.
            
            ttInv.amountTotalLines        = ttInv.amountTotalLines + bf-inv-misc.amt.
            
            FIND FIRST bf-oe-ordm NO-LOCK
                 WHERE bf-oe-ordm.company EQ bf-inv-misc.company
                   AND bf-oe-ordm.charge  EQ bf-inv-misc.charge
                   AND bf-oe-ordm.ord-no  EQ bf-inv-misc.ord-no
                 NO-ERROR.            
            IF AVAILABLE bf-oe-ordm THEN DO:
                ttInvLine.orderLine = bf-oe-ordm.line.
                
                IF ttInvLine.customerPONo EQ "" THEN
                    ttInvLine.customerPONo = bf-oe-ordm.po-no.
            END.
            
            ttInvLine.orderLineOverridden = IF ttInvLine.orderLineOverride GT 0 THEN
                                                ttInvLine.orderLineOverride
                                            ELSE
                                                ttInvLine.orderLine. 
                
            IF ttInvLine.customerPONo EQ "" THEN DO:
                FIND FIRST bf-oe-ord NO-LOCK
                     WHERE bf-oe-ord.company EQ bf-inv-misc.company                       
                       AND bf-oe-ord.ord-no  EQ bf-inv-misc.ord-no
                     NO-ERROR.     
                IF AVAILABLE bf-oe-ord THEN
                    ttInvLine.customerPONo = bf-oe-ord.po-no. 
            END.    
            
            RUN pAssignCommonLineData (
                BUFFER ttInv, 
                BUFFER ttInvLine
                ).            
                
        END.

        lFirst = TRUE.
        
        FOR EACH ttTaxDetail
            WHERE ttTaxDetail.invoiceLineType   EQ "INVHEAD"
              AND ttTaxDetail.invoiceLineRecKey EQ ipbf-inv-head.rec_key:
            IF NOT ttTaxDetail.isFreight THEN
                NEXT.
                
            ASSIGN
                ttInv.frtTaxRate                = ttInv.frtTaxRate + ttTaxDetail.taxCodeRate
                ttInv.amountTotalTaxFreight     = ttInv.amountTotalTaxFreight + ttTaxDetail.taxCodeTaxAmount
                .
            
            IF lFirst THEN
                ASSIGN
                    ttInv.amountTotalTaxableFreight = ttInv.amountTotalTaxableFreight + ttTaxDetail.taxCodeTaxableAmount
                    ttInv.amountTotalTaxable        = ttInv.amountTotalTaxable + ttTaxDetail.taxCodeTaxableAmount
                    lFirst                          = FALSE
                    .
        END.

        ttInv.amountTotal = ttInv.amountTotalLines + ttInv.amountTotalTax.
        
        IF ttInv.billFreight THEN 
            ttInv.amountTotal = ttInv.amountTotal + ttInv.amountTotalFreight.

        IF ttInv.amountTotal LT 0 THEN
            ttInv.invoiceType = "CR".        
    END.
    
END PROCEDURE.
