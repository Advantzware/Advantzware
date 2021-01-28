
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
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipToID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTermsCode AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-company FOR company.
    DEFINE BUFFER bf-cust    FOR cust.
    DEFINE BUFFER bf-shipto  FOR shipto.
    DEFINE BUFFER bf-terms   FOR terms.
    
    ASSIGN 
        ipbf-ttInv.invoiceIDString   = STRING(ipbf-ttInv.invoiceID)
        ipbf-ttInv.invoiceDateString = STRING(YEAR(ipbf-ttInv.invoiceDate),'9999')
                                     + '-'
                                     + STRING(MONTH(ipbf-ttInv.invoiceDate),'99')
                                     + '-'
                                     + STRING(DAY(ipbf-ttInv.invoiceDate),'99')
                                     + 'T'
                                     + STRING(0,'hh:mm:ss')
                                     + '-05:00'.
       .
       
    FIND FIRST bf-company NO-LOCK
        WHERE bf-company.company EQ ipcCompany
        NO-ERROR.
    IF AVAILABLE bf-company THEN 
        ASSIGN
            ipbf-ttInv.companyName       = bf-company.name
            ipbf-ttInv.companyAddress1   = bf-company.addr[1]
            ipbf-ttInv.companyAddress2   = bf-company.addr[2]
            ipbf-ttInv.companyCity       = bf-company.city
            ipbf-ttInv.companyState      = bf-company.state
            ipbf-ttInv.companyPostalCode = bf-company.zip 
            .
    FIND FIRST bf-cust NO-LOCK 
        WHERE bf-cust.company EQ ipcCompany
          AND bf-cust.cust-no EQ ipcCustomerID
        NO-ERROR.
    FIND FIRST bf-shipto NO-LOCK 
        WHERE bf-shipto.company EQ ipcCompany
          AND bf-shipto.cust-no EQ ipcCustomerID
          AND bf-shipto.ship-id EQ ipcShipToID
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
            .
    FIND FIRST bf-terms NO-LOCK 
         WHERE bf-terms.company EQ ipcCompany
           AND bf-terms.t-code EQ ipcTermsCode
         NO-ERROR.
    IF AVAILABLE bf-terms THEN 
        ipbf-ttInv.termsDays = bf-terms.net-days.
    
END PROCEDURE.

PROCEDURE pAssignCommonLineData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given buffers and primary inputs, assign common fields to temp-table header
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttInv     FOR ttInv.
    DEFINE PARAMETER BUFFER ipbf-ttInvLine FOR ttInvLine.
    
    DEFINE           BUFFER bf-oe-ord      FOR oe-ord.
    DEFINE           BUFFER bf-oe-ordl     FOR oe-ordl.
    
    DEFINE VARIABLE lError        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
    
    IF ipbf-ttInv.customerPO EQ "" THEN 
        ipbf-ttInv.customerPO = ipbf-ttInvLine.customerPO.
    FIND FIRST bf-oe-ord NO-LOCK
         WHERE bf-oe-ord.company EQ ipbf-ttInv.company
           AND bf-oe-ord.ord-no EQ ipbf-ttInvLine.orderID
         NO-ERROR.
    IF AVAILABLE bf-oe-ord THEN DO:
        IF ipbf-ttInv.customerPO EQ "" THEN 
            ipbf-ttInv.customerPO EQ bf-oe-ord.po-no.
            
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
    
    DEFINE           BUFFER bf-ar-invl  FOR ar-invl.
    DEFINE           BUFFER bf-oe-ordl  FOR oe-ordl.
    
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
            BUFFER ttInv, 
            INPUT  ipbf-ar-inv.company, 
            INPUT  ipbf-ar-inv.cust-no, 
            INPUT  ipbf-ar-inv.ship-id,
            INPUT  ipbf-ar-inv.terms
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
                ttInvLine.orderLine              = bf-ar-invl.ord-line
                ttInvLine.quantityInvoiced       = bf-ar-invl.inv-qty
                ttInvLine.quantityInvoicedUOM    = "EA"
                ttInvLine.pricePerUOM            = bf-ar-invl.unit-pr * (1 - (bf-ar-invl.disc / 100))
                ttInvLine.priceUOM               = bf-ar-invl.pr-uom
                ttInvLine.customerPartID         = bf-ar-invl.part-no
                ttInvLine.itemID                 = bf-ar-invl.i-no
                ttInvLine.itemName               = bf-ar-invl.i-name
                ttInvLine.priceTotal             = bf-ar-invl.amt
                ttInvLine.taxable                = bf-ar-invl.tax
                ttInvLine.amountTaxableExFreight = ttInvLine.priceTotal
                ttInvLine.amountFreight          = bf-ar-invl.t-freight
                ttInvLine.customerPONo           = bf-ar-invl.po-no
                ttInvLine.taxGroup               = ttInv.taxGroup
                ttInvLine.isMisc                 = bf-ar-invl.misc
                .
            
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

            FIND FIRST bf-oe-ordl NO-LOCK
                 WHERE bf-oe-ordl.company EQ bf-ar-invl.company
                   AND bf-oe-ordl.i-no    EQ bf-ar-invl.i-no
                   AND bf-oe-ordl.ord-no  EQ bf-ar-invl.ord-no
                 NO-ERROR.

            IF AVAILABLE bf-oe-ordl THEN 
                ttInvLine.orderLine = bf-oe-ordl.line.
            
            RUN pAssignCommonLineData(
                BUFFER ttInv, 
                BUFFER ttInvLine
                ).            
            
            lFirstLine = FALSE.
        END.
        
        ttInv.amountTotal = ttInv.amountTotalLines + ttInv.amountTotalTax.
        
        IF ttInv.billFreight THEN 
            ttInv.amountTotal = ttInv.amountTotal + ttInv.amountTotalFreight.
            
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
    
    DEFINE VARIABLE dTaxTotal        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvoiceTotal    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvoiceSubTotal AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFirst           AS LOGICAL   NO-UNDO.
    
    IF AVAILABLE ipbf-inv-head THEN DO:
        CREATE ttInv.
        ASSIGN             
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
            BUFFER ttInv, 
            INPUT  ipbf-inv-head.company, 
            INPUT  ipbf-inv-head.cust-no, 
            INPUT  ipbf-inv-head.sold-no,
            INPUT  ipbf-inv-head.terms
            ).
                         
        FOR EACH bf-inv-line NO-LOCK
            WHERE bf-inv-line.r-no EQ ipbf-inv-head.r-no:

            CREATE ttInvLine.
            ASSIGN 
                ttInvLine.invoiceID              = ttInv.invoiceID
                ttInvLine.company                = ttInv.company
                ttInvLine.lineNo                 = bf-inv-line.line
                ttInvLine.orderID                = bf-inv-line.ord-no
                ttInvLine.orderLine              = bf-inv-line.line
                ttInvLine.quantityInvoiced       = bf-inv-line.inv-qty
                ttInvLine.quantityInvoicedUOM    = bf-inv-line.pr-qty-uom
                ttInvLine.pricePerUOM            = bf-inv-line.price * (1 - (bf-inv-line.disc / 100))
                ttInvLine.priceUOM               = bf-inv-line.pr-uom
                ttInvLine.customerPartID         = bf-inv-line.part-no
                ttInvLine.itemID                 = bf-inv-line.i-no
                ttInvLine.itemName               = bf-inv-line.i-name
                ttInvLine.priceTotal             = bf-inv-line.t-price
                ttInvLine.taxable                = bf-inv-line.tax
                ttInvLine.customerPONo           = bf-inv-line.po-no
                ttInvLine.taxGroup               = ttInv.taxGroup
                ttInvLine.isMisc                 = FALSE
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

            IF AVAILABLE bf-oe-ordl THEN 
                ttInvLine.orderLine = bf-oe-ordl.line.
            
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
                ttInvLine.quantityInvoiced       = 1
                ttInvLine.quantityInvoicedUOM    = "EA"
                ttInvLine.pricePerUOM            = bf-inv-misc.amt
                ttInvLine.priceUOM               = "EA"
                ttInvLine.itemID                 = bf-inv-misc.inv-i-no
                ttInvLine.charge                 = bf-inv-misc.charge
                ttInvLine.chargeDescription      = bf-inv-misc.dscr
                ttInvLine.priceTotal             = bf-inv-misc.amt
                ttInvLine.taxable                = bf-inv-misc.tax
                ttInvLine.customerPONo           = bf-inv-misc.po-no
                ttInvLine.taxGroup               = ttInv.taxGroup
                ttInvLine.isMisc                 = TRUE
                .
            
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

            IF AVAILABLE bf-oe-ordl THEN
                ttInvLine.orderLine = bf-oe-ordl.line.
            
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
        
    END.
    
END PROCEDURE.

