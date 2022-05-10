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

/* ************************  Function Prototypes ********************** */
    
FUNCTION fGetNewInvoice RETURNS INTEGER 
    (ipcCompany AS CHARACTER) FORWARD. 

/* ********************  Preprocessor Definitions  ******************** */

/* **********************  Internal Procedures  *********************** */

FUNCTION fAddressBlock RETURNS CHARACTER PRIVATE (
    ipcAddress1   AS CHARACTER,
    ipcAddress2   AS CHARACTER,
    ipcCity       AS CHARACTER,
    ipcState      AS CHARACTER,
    ipcPostalCode AS CHARACTER
    ): 
    RETURN (IF ipcAddress1 NE "" THEN ipcAddress1 + CHR(10) ELSE "")
         + (IF ipcAddress2 NE "" THEN ipcAddress2 + CHR(10) ELSE "")
         + (IF ipcCity NE "" THEN ipcCity + ", " ELSE "")
         + (IF ipcState NE "" THEN ipcState + " " ELSE "")
         + (IF ipcPostalCode NE "" THEN ipcPostalCode ELSE "")
         .

END FUNCTION.

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

PROCEDURE invoice_CreateInterCompanyBilling:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriOeBolh  AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSoldID   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    RUN pCreateInterCompanyBilling(
                  INPUT ipriOeBolh,
                  INPUT ipcSoldID,
                  OUTPUT oplError,
                  OUTPUT opcMessage
                  ).
END PROCEDURE.

PROCEDURE invoice_pCreateInvoiceLineTax:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipriInvoiceLine AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER TABLE FOR ttTaxDetail.
        
    RUN pCreateInvoiceLineTax(
                  INPUT ipcRecKey,
                  INPUT ipriInvoiceLine /*,
                  INPUT TABLE ttTaxDetail*/ 
                  ).
END PROCEDURE.

PROCEDURE pAssignCommonHeaderData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given primary inputs, assign common fields to temp-table header
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttInv FOR ttInv.

    DEFINE VARIABLE iDueOnMonth AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iDueOnDay   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNetDays    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dDiscPct    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iDiscDays   AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE cTermsDesc  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-cust    FOR cust.
    DEFINE BUFFER bf-shipto  FOR shipto.
    DEFINE BUFFER bf-notes   FOR notes.
    DEFINE BUFFER bf-country FOR country.
    
    FIND FIRST company NO-LOCK
         WHERE company.company EQ ipbf-ttInv.company
         NO-ERROR.
    IF AVAILABLE company THEN
         ASSIGN 
             ipbf-ttInv.country  = company.countryCode
             ipbf-ttInv.currency = company.curr-code.    

    FIND FIRST bf-cust NO-LOCK 
        WHERE bf-cust.company EQ ipbf-ttInv.company
          AND bf-cust.cust-no EQ ipbf-ttInv.customerID
        NO-ERROR.
    IF AVAILABLE bf-cust THEN
        ASSIGN
            ipbf-ttInv.customerEmail = bf-cust.email
            ipbf-ttInv.areaCode      = bf-cust.area-code
            ipbf-ttInv.phone         = bf-cust.phone 
            ipbf-ttInv.fax           = bf-cust.fax
            ipbf-ttInv.country       = IF bf-cust.fax-country NE "" THEN bf-cust.fax-country ELSE ipbf-ttInv.country
            ipbf-ttInv.currency      = IF bf-cust.curr-code NE "" THEN bf-cust.curr-code ELSE ipbf-ttInv.currency.
        
    FIND FIRST bf-shipto NO-LOCK 
         WHERE bf-shipto.company EQ ipbf-ttInv.company
           AND bf-shipto.cust-no EQ ipbf-ttInv.customerID
           AND bf-shipto.ship-id EQ ipbf-ttInv.shipToID
         NO-ERROR.
    IF AVAILABLE bf-shipto THEN 
        ASSIGN 
            ipbf-ttInv.shipToID           = bf-shipto.ship-id
            ipbf-ttInv.shiptoName         = bf-shipto.ship-name
            ipbf-ttInv.shiptoAddress1     = bf-shipto.ship-addr[1]
            ipbf-ttInv.shiptoAddress2     = bf-shipto.ship-addr[2]
            ipbf-ttInv.shiptoCity         = bf-shipto.ship-city
            ipbf-ttInv.shiptoState        = bf-shipto.ship-state
            ipbf-ttInv.shiptoPostalCode   = bf-shipto.ship-zip
            ipbf-ttInv.shiptoAddressBlock = fAddressBlock (
                                                ipbf-ttInv.shiptoAddress1,
                                                ipbf-ttInv.shiptoAddress2,
                                                ipbf-ttInv.shiptoCity,
                                                ipbf-ttInv.shiptoState,
                                                ipbf-ttInv.shiptoPostalCode
                                                )
            ipbf-ttInv.siteID             = bf-shipto.siteId
            .
    
    FIND FIRST bf-country NO-LOCK
         WHERE bf-country.countryCode EQ ipbf-ttInv.country
         NO-ERROR.
    IF AVAILABLE bf-country THEN
        ASSIGN
            ipbf-ttInv.countryName = bf-country.Description.
            
    RUN Credit_GetTerms (
        INPUT  ipbf-ttInv.company,
        INPUT  ipbf-ttInv.terms,
        OUTPUT iDueOnMonth,
        OUTPUT iDueOnDay,
        OUTPUT iNetDays, 
        OUTPUT dDiscPct,  
        OUTPUT iDiscDays,
        OUTPUT cTermsDesc,
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
            ipbf-ttInv.termsDesc           = cTermsDesc
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
        ipbf-ttInv.isEDIOrder = bf-oe-ord.ediSubmitted EQ 1.
        
        IF ipbf-ttInv.customerPONo EQ "" THEN 
            ipbf-ttInv.customerPONo = bf-oe-ord.po-no.
        
        IF ipbf-ttInvLine.customerPONoNoBlank EQ "" THEN
            ipbf-ttInvLine.customerPONoNoBlank = bf-oe-ord.po-no.
            
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
             
    DEFINE VARIABLE dTaxTotal        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvoiceTotal    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvoiceSubTotal AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFirst           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFirstLine       AS LOGICAL   NO-UNDO.
    
    IF AVAILABLE ipbf-ar-inv THEN DO:
        FIND FIRST carrier NO-LOCK
             WHERE carrier.company EQ ipbf-ar-inv.company
               AND carrier.carrier EQ ipbf-ar-inv.carrier
             NO-ERROR.
        CREATE ttInv.
        ASSIGN             
            ttInv.invoiceRecKey        = ipbf-ar-inv.rec_key
            ttInv.invoiceDate          = ipbf-ar-inv.inv-date 
            ttInv.invoiceID            = ipbf-ar-inv.inv-no
            ttInv.customerID           = ipbf-ar-inv.cust-no
            ttInv.customerName         = ipbf-ar-inv.cust-name
            ttInv.customerAddress1     = ipbf-ar-inv.addr[1]
            ttInv.customerAddress2     = ipbf-ar-inv.addr[2]
            ttInv.customerCity         = ipbf-ar-inv.city
            ttInv.customerState        = ipbf-ar-inv.state
            ttInv.customerPostalCode   = ipbf-ar-inv.zip
            ttInv.customerAddressBlock = fAddressBlock (
                                             ttInv.customerAddress1,
                                             ttInv.customerAddress2,
                                             ttInv.customerCity,
                                             ttInv.customerState,
                                             ttInv.customerPostalCode
                                             )
            ttInv.company              = ipbf-ar-inv.company
            ttInv.taxGroup             = ipbf-ar-inv.tax-code
            ttInv.amountTotal          = ipbf-ar-inv.t-sales 
            ttInv.billFreight          = ipbf-ar-inv.f-bill
            ttInv.amountTotalFreight   = ipbf-ar-inv.freight 
            ttInv.amountTotalTax       = ipbf-ar-inv.tax-amt
            ttInv.shiptoID             = ipbf-ar-inv.ship-id
            ttInv.terms                = ipbf-ar-inv.terms            
            ttInv.invoiceDueDate       = ipbf-ar-inv.due-date
            ttInv.customerPONo         = ipbf-ar-inv.po-no
            ttInv.fob                  = IF ipbf-ar-inv.fob BEGINS "ORIG" THEN "Origin" ELSE "Destination"
            ttInv.carrier              = IF AVAILABLE carrier THEN carrier.dscr ELSE "" 
            ttInv.frtPay               = ipbf-ar-inv.frt-pay
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
                ttInvLine.ediPrice               = bf-ar-invl.ediPrice
                ttInvLine.ediPriceUOM            = bf-ar-invl.ediPriceUOM
                ttInvLine.customerPartID         = bf-ar-invl.part-no
                ttInvLine.itemID                 = bf-ar-invl.inv-i-no
                ttInvLine.itemName               = IF bf-ar-invl.misc THEN
                                                       bf-ar-invl.prep-charge
                                                   ELSE
                                                       bf-ar-invl.i-name
                ttInvLine.itemDescription        = bf-ar-invl.i-dscr
                ttInvLine.charge                 = bf-ar-invl.prep-charge
                ttInvLine.chargeDescription      = bf-ar-invl.i-dscr                
                ttInvLine.priceTotal             = bf-ar-invl.amt
                ttInvLine.taxable                = bf-ar-invl.tax
                ttInvLine.billable               = TRUE
                ttInvLine.amountTaxableExFreight = ttInvLine.priceTotal
                ttInvLine.amountFreight          = bf-ar-invl.t-freight
                ttInvLine.customerPONo           = bf-ar-invl.po-no
                ttInvLine.customerPONoNoBlank    = IF ttInvLine.customerPONo EQ "" THEN
                                                       ipbf-ar-inv.po-no
                                                   ELSE
                                                       ttInvLine.customerPONo
                ttInvLine.taxGroup               = ttInv.taxGroup
                ttInvLine.isMisc                 = bf-ar-invl.misc
                ttInvLine.bNo                    = bf-ar-invl.b-no
                .
            
            IF ttInvLine.isMisc THEN
                ASSIGN
                    ttInvLine.quantityInvoiced = 1
                    ttInvLine.pricePerEach     = IF bf-ar-invl.inv-qty EQ 0 THEN 
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

                    IF ttInvLine.customerPONoNoBlank EQ "" THEN
                        ttInvLine.customerPONoNoBlank = bf-oe-ordl.po-no.                
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
                    
                    IF ttInvLine.customerPONoNoBlank EQ "" THEN
                        ttInvLine.customerPONoNoBlank = bf-oe-ordm.po-no.
                END.            
            END.
            
            ttInvLine.orderLineOverridden = IF ttInvLine.orderLineOverride GT 0 THEN
                                                ttInvLine.orderLineOverride
                                            ELSE
                                                ttInvLine.orderLine.  

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
    
    DEFINE VARIABLE dTaxTotal        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvoiceTotal    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvoiceSubTotal AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFirst           AS LOGICAL   NO-UNDO.
    
    IF AVAILABLE ipbf-inv-head THEN DO:
        FIND FIRST carrier NO-LOCK
             WHERE carrier.company EQ inv-head.company
               AND carrier.carrier EQ inv-head.carrier
             NO-ERROR.
        CREATE ttInv.
        ASSIGN             
            ttInv.invoiceRecKey        = ipbf-inv-head.rec_key
            ttInv.invoiceDate          = ipbf-inv-head.inv-date 
            ttInv.invoiceID            = ipbf-inv-head.inv-no
            ttInv.customerID           = ipbf-inv-head.cust-no
            ttInv.customerName         = ipbf-inv-head.cust-name
            ttInv.customerAddress1     = ipbf-inv-head.addr[1]
            ttInv.customerAddress2     = ipbf-inv-head.addr[2]
            ttInv.customerCity         = ipbf-inv-head.city
            ttInv.customerState        = ipbf-inv-head.state
            ttInv.customerPostalCode   = ipbf-inv-head.zip
            ttInv.customerAddressBlock = fAddressBlock (
                                             ttInv.customerAddress1,
                                             ttInv.customerAddress2,
                                             ttInv.customerCity,
                                             ttInv.customerState,
                                             ttInv.customerPostalCode
                                             )
            ttInv.company              = ipbf-inv-head.company
            ttInv.taxGroup             = ipbf-inv-head.tax-gr
            ttInv.amountTotal          = ipbf-inv-head.t-inv-rev
            ttInv.billFreight          = ipbf-inv-head.f-bill
            ttInv.amountTotalFreight   = ipbf-inv-head.t-inv-freight 
            ttInv.amountTotalTax       = ipbf-inv-head.t-inv-tax
            ttInv.shiptoID             = ipbf-inv-head.sold-no
            ttInv.terms                = ipbf-inv-head.terms            
            ttInv.invoiceDueDate       = DYNAMIC-FUNCTION("GetInvDueDate", ttInv.invoiceDate, ttInv.company ,ttInv.terms)
            ttInv.fob                  = IF ipbf-inv-head.fob BEGINS "ORIG" THEN "Origin" ELSE "Destination"
            ttInv.carrier              = IF AVAILABLE carrier THEN carrier.dscr ELSE ""
            ttInv.frtPay               = ipbf-inv-head.frt-pay
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
                ttInvLine.ediPrice               = bf-inv-line.ediPrice
                ttInvLine.ediPriceUOM            = bf-inv-line.ediPriceUOM
                ttInvLine.customerPartID         = bf-inv-line.part-no
                ttInvLine.itemID                 = bf-inv-line.i-no
                ttInvLine.itemName               = bf-inv-line.i-name
                ttInvLine.itemDescription        = bf-inv-line.i-dscr
                ttInvLine.priceTotal             = bf-inv-line.t-price
                ttInvLine.taxable                = bf-inv-line.tax
                ttInvLine.billable               = TRUE
                ttInvLine.customerPONo           = bf-inv-line.po-no
                ttInvLine.customerPONoNoBlank    = ttInvLine.customerPONo                
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
                
                IF ttInvLine.customerPONoNoBlank EQ "" THEN
                    ttInvLine.customerPONoNoBlank = bf-oe-ordl.po-no.                
            END.
            
            ttInvLine.orderLineOverridden = IF ttInvLine.orderLineOverride GT 0 THEN
                                                ttInvLine.orderLineOverride
                                            ELSE
                                                ttInvLine.orderLine. 

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
                ttInvLine.orderID                = bf-inv-misc.ord-no
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
                ttInvLine.customerPONoNoBlank    = bf-inv-misc.po-no
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
                
                IF ttInvLine.customerPONoNoBlank EQ "" THEN
                    ttInvLine.customerPONoNoBlank = bf-oe-ordm.po-no.
            END.
            
            ttInvLine.orderLineOverridden = IF ttInvLine.orderLineOverride GT 0 THEN
                                                ttInvLine.orderLineOverride
                                            ELSE
                                                ttInvLine.orderLine. 
                
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

PROCEDURE pCreateInterCompanyBilling PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriOeBolh  AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSoldID   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-inv-head FOR inv-head.
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER bf-inv-misc FOR inv-misc.
    DEFINE BUFFER bf-ar-inv   FOR ar-inv.
    DEFINE BUFFER bf-ar-invl  FOR ar-invl.
    DEFINE BUFFER bf-period   FOR period.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-cust     FOR cust.
    DEFINE BUFFER bf-shipto   FOR shipto.
    
    DEFINE VARIABLE iLine         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTransCompany AS CHARACTER INIT "002" NO-UNDO.
    /*used for Terms procedures*/
    DEFINE VARIABLE iDueOnMonth   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iDueOnDay     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNetDays      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lError        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCustExist    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cCustomer     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipto       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iInterCompanyBilling AS INTEGER NO-UNDO.
    DEFINE VARIABLE iAsc          AS INTEGER NO-UNDO.
    DEFINE VARIABLE cChar         AS CHARACTER NO-UNDO.     
    DEFINE VARIABLE lSoldUsed     AS LOGICAL NO-UNDO.
    
    FIND FIRST oe-bolh NO-LOCK 
        WHERE ROWID(oe-bolh) EQ ipriOeBolh NO-ERROR.
    IF NOT AVAILABLE oe-bolh THEN RETURN.
     
    RUN pGetNk1TransCompany(
        INPUT oe-bolh.company,
        INPUT oe-bolh.cust-no,
        INPUT-OUTPUT cTransCompany,
        OUTPUT iInterCompanyBilling
        ). 
        
    IF iInterCompanyBilling EQ 1 THEN
    DO:
        lSoldUsed = NO.
        cChar =  SUBSTRING(oe-bolh.ship-id,1,1).
        iAsc = ASC(cChar).
        IF iAsc GE 65 AND iAsc LE 122 THEN
        lSoldUsed = YES.         
    END.    
        
    ASSIGN
        cCustomer = IF iInterCompanyBilling EQ 1 AND lSoldUsed THEN ipcSoldID
               ELSE IF iInterCompanyBilling EQ 1 THEN oe-bolh.ship-id
               ELSE oe-bolh.cust-no   
        cShipto   = oe-bolh.ship-id
        .
     
    FIND FIRST bf-period NO-LOCK 
        WHERE bf-period.company EQ oe-bolh.company
        AND bf-period.pst LE TODAY
        AND bf-period.pend GE TODAY
        NO-ERROR.
                
    FIND FIRST bf-cust NO-LOCK
        WHERE bf-cust.company EQ cTransCompany 
        AND bf-cust.cust-no EQ cCustomer NO-ERROR.
                
    FOR EACH bf-inv-head NO-LOCK
        WHERE bf-inv-head.company EQ oe-bolh.company
        AND bf-inv-head.cust-no EQ oe-bolh.cust-no
        AND bf-inv-head.bol-no EQ oe-bolh.bol-no 
        : 
        CREATE bf-ar-inv.
        ASSIGN
            bf-ar-inv.company        = cTransCompany
            bf-ar-inv.inv-date       = TODAY
            bf-ar-inv.inv-no         = fGetNewInvoice(cTransCompany)                     
            bf-ar-inv.ord-no         = oe-bolh.ord-no                                       
            bf-ar-inv.inv-date       = bf-inv-head.inv-date
            bf-ar-inv.prod-date      = TODAY
            bf-ar-inv.period         = bf-period.pnum       
            bf-ar-inv.posted         = NO 
            bf-ar-inv.printed        = NO        
            bf-ar-inv.cust-no        = bf-cust.cust-no 
            bf-ar-inv.cust-name      = bf-cust.NAME 
            bf-ar-inv.ship-id        = cShipto
            bf-ar-inv.addr[1]        = bf-cust.addr[1]
            bf-ar-inv.addr[2]        = bf-cust.addr[2]
            bf-ar-inv.state          = bf-cust.state
            bf-ar-inv.zip            = bf-cust.zip
            bf-ar-inv.city           = bf-cust.city
            bf-ar-inv.bill-to        = bf-inv-head.bill-to
            bf-ar-inv.sold-id        = ipcSoldID
            bf-ar-inv.sold-name      = bf-inv-head.sold-name
            bf-ar-inv.sold-addr[1]   = bf-inv-head.sold-addr[1]
            bf-ar-inv.sold-addr[2]   = bf-inv-head.sold-addr[2]
            bf-ar-inv.sold-city      = bf-inv-head.sold-city
            bf-ar-inv.sold-state     = bf-inv-head.sold-state
            bf-ar-inv.sold-zip       = bf-inv-head.sold-zip
            bf-ar-inv.contact        = bf-cust.contact
            bf-ar-inv.terms          = bf-cust.terms
            bf-ar-inv.frt-pay        = bf-cust.frt-pay
            bf-ar-inv.fob-code       = bf-cust.fob-code
            bf-ar-inv.carrier        = bf-cust.carrier                      
            bf-ar-inv.bill-i[1]      = bf-inv-head.bill-i[1]
            bf-ar-inv.bill-i[2]      = bf-inv-head.bill-i[2]
            bf-ar-inv.bill-i[3]      = bf-inv-head.bill-i[3]
            bf-ar-inv.bill-i[4]      = bf-inv-head.bill-i[4]
            bf-ar-inv.ship-i[1]      = bf-inv-head.ship-i[1]
            bf-ar-inv.ship-i[2]      = bf-inv-head.ship-i[2]
            bf-ar-inv.ship-i[3]      = bf-inv-head.ship-i[3]
            bf-ar-inv.ship-i[4]      = bf-inv-head.ship-i[4]
            bf-ar-inv.f-bill         = bf-inv-head.f-bill
            bf-ar-inv.STAT           = bf-inv-head.STAT
            bf-ar-inv.TAX-code       = bf-cust.TAX-GR
            bf-ar-inv.t-comm         = bf-inv-head.t-comm
            bf-ar-inv.t-weight       = bf-inv-head.t-inv-weight   /* total weight shipped */
            bf-ar-inv.freight        = bf-inv-head.t-inv-freight  /* total freight Invoiced */
            bf-ar-inv.tax-amt        = bf-inv-head.t-inv-tax      /* total tax Invoiced */
            bf-ar-inv.t-cost         = bf-inv-head.t-inv-cost     /* total cost invoiced */
            bf-ar-inv.due            = IF bf-inv-head.terms EQ "CASH" THEN 0 ELSE bf-inv-head.t-inv-rev        
            /* total invoiced amount */
            bf-ar-inv.gross          = bf-inv-head.t-inv-rev /*+ v-inv-disc   total invoiced + disc */ 
            bf-ar-inv.disc-taken     = 0
            bf-ar-inv.paid           = 0        
            /* total invoiced - freight - misc - tax */
            bf-ar-inv.t-sales        = bf-inv-head.t-inv-rev - bf-inv-head.t-inv-tax
            bf-ar-inv.net            = bf-ar-inv.t-sales
            bf-ar-inv.curr-code[1]   = bf-inv-head.curr-code[1]
                    
            bf-ar-inv.postedDate     = TODAY                           
            bf-ar-inv.invoiceComment = bf-inv-head.spare-char-1
            bf-ar-inv.glYear         = YEAR(TODAY)
            . 
        FIND FIRST currency NO-LOCK 
            WHERE currency.company EQ cTransCompany
            AND currency.c-code EQ bf-inv-head.curr-code[1]
            AND currency.ex-rate GT 0 
            NO-ERROR.   
        IF AVAILABLE currency THEN
            ASSIGN                     
                bf-ar-inv.ex-rate = currency.ex-rate .
                 
        IF bf-inv-head.f-bill THEN /*Exclude Freight billed from total true sales*/ 
            ASSIGN 
                bf-ar-inv.t-sales = bf-ar-inv.t-sales - bf-inv-head.t-inv-freight.  
        FIND FIRST terms WHERE terms.t-code = bf-ar-inv.terms NO-LOCK NO-ERROR.
        IF AVAILABLE terms THEN
            ASSIGN  
                bf-ar-inv.terms-d   = terms.dscr
                bf-ar-inv.disc-%    = terms.disc-rate
                bf-ar-inv.disc-days = terms.disc-days.
                         
        bf-ar-inv.due-date  =  DYNAMIC-FUNCTION("GetInvDueDate", DATE(bf-ar-inv.inv-date), cTransCompany, bf-inv-head.terms).  /*From CreditProcs*/
                 
        iLine = 1.
        FIND FIRST ar-ctrl NO-LOCK  
            WHERE ar-ctrl.company EQ cTransCompany
            NO-ERROR.
        FOR EACH bf-inv-line OF bf-inv-head NO-LOCK:
            CREATE bf-ar-invl.
            ASSIGN 
                bf-ar-invl.x-no               = bf-ar-inv.x-no
                bf-ar-invl.line               = iLine
                bf-ar-invl.company            = cTransCompany
                bf-ar-invl.b-no               = bf-inv-line.b-no                        
                bf-ar-invl.actnum             = IF AVAILABLE ar-ctrl THEN ar-ctrl.sales ELSE ""
                bf-ar-invl.inv-no             = bf-ar-inv.inv-no
                bf-ar-invl.bol-no             = oe-bolh.bol-no                         
                bf-ar-invl.ord-no             = bf-inv-line.ord-no
                bf-ar-invl.cust-no            = bf-cust.cust-no
                bf-ar-invl.est-no             = bf-inv-line.est-no
                bf-ar-invl.est-type           = bf-inv-line.est-type
                bf-ar-invl.form-no            = bf-inv-line.form-no
                bf-ar-invl.blank-no           = bf-inv-line.blank-no
                bf-ar-invl.job-no             = bf-inv-line.job-no
                bf-ar-invl.job-no2            = bf-inv-line.job-no2
                bf-ar-invl.part-no            = bf-inv-line.part-no
                bf-ar-invl.i-no               = bf-inv-line.i-no
                bf-ar-invl.i-name             = bf-inv-line.i-name
                bf-ar-invl.i-dscr             = bf-inv-line.i-dscr
                bf-ar-invl.po-no              = bf-inv-line.po-no
                bf-ar-invl.req-code           = bf-inv-line.req-code
                bf-ar-invl.req-date           = bf-inv-line.req-date
                bf-ar-invl.prom-code          = bf-inv-line.prom-code
                bf-ar-invl.prom-date          = bf-inv-line.prom-date
                bf-ar-invl.part-dscr1         = bf-inv-line.part-dscr1
                bf-ar-invl.part-dscr2         = bf-inv-line.part-dscr2
                bf-ar-invl.po-no-po           = bf-inv-line.po-no-po
                bf-ar-invl.cas-cnt            = bf-inv-line.cas-cnt
                bf-ar-invl.pr-uom             = bf-inv-line.pr-uom
                bf-ar-invl.unit-pr            = bf-inv-line.price
                bf-ar-invl.tax                = bf-inv-line.tax
                bf-ar-invl.disc               = bf-inv-line.disc
                bf-ar-invl.amt                = bf-inv-line.t-price   /* total price of invoiced item */
                bf-ar-invl.t-weight           = bf-inv-line.t-weight  /* total weight of invoiced item */
                bf-ar-invl.t-freight          = bf-inv-line.t-freight /* total freight of invoiced item */
                bf-ar-invl.ship-qty           = bf-inv-line.ship-qty
                bf-ar-invl.inv-qty            = bf-inv-line.inv-qty
                bf-ar-invl.qty                = bf-inv-line.qty                         
                bf-ar-invl.sman[1]            = bf-cust.sman                 
                bf-ar-invl.s-pct[1]           = bf-inv-line.s-pct[1]
                bf-ar-invl.s-pct[2]           = bf-inv-line.s-pct[2]
                bf-ar-invl.s-pct[3]           = bf-inv-line.s-pct[3]
                bf-ar-invl.s-comm[1]          = bf-inv-line.s-comm[1]
                bf-ar-invl.s-comm[2]          = bf-inv-line.s-comm[2]
                bf-ar-invl.s-comm[3]          = bf-inv-line.s-comm[3]                                  
                bf-ar-invl.s-commbasis[1]     = bf-inv-line.s-commbasis[1]
                bf-ar-invl.s-commbasis[2]     = bf-inv-line.s-commbasis[2]
                bf-ar-invl.s-commbasis[3]     = bf-inv-line.s-commbasis[3]
                bf-ar-invl.misc               = NO 
                bf-ar-invl.posted             = NO 
                bf-ar-invl.pr-qty-uom         = bf-inv-line.pr-uom
                bf-ar-invl.cost               = bf-inv-line.cost
                bf-ar-invl.t-cost             = bf-ar-invl.cost * (bf-ar-invl.inv-qty / 1000)
                bf-ar-invl.dscr[1]            = "M"
                bf-ar-invl.std-tot-cost       = bf-inv-line.cost                  
                bf-ar-invl.lot-no             = bf-inv-line.lot-no
                bf-ar-invl.e-num              = bf-inv-line.e-num
                bf-ar-invl.inv-date           = bf-inv-head.inv-date
                bf-ar-invl.costStdFreight     = bf-inv-line.costStdFreight
                bf-ar-invl.costStdWarehouse   = bf-inv-line.costStdWarehouse
                bf-ar-invl.costStdDeviation   = bf-inv-line.costStdDeviation
                bf-ar-invl.costStdManufacture = bf-inv-line.costStdManufacture                        
                bf-ar-invl.amt-msf            = (bf-ar-invl.inv-qty * bf-ar-invl.sf-sht) / 1000
                bf-ar-invl.taxGroup           = bf-inv-line.taxGroup
                .    
            
            FIND FIRST bf-itemfg NO-LOCK
                WHERE bf-itemfg.company EQ bf-inv-line.company
                AND bf-itemfg.i-no EQ bf-inv-line.i-no
                NO-ERROR.
            IF AVAILABLE bf-itemfg THEN 
            DO:     
                RUN fg\GetFGArea.p (ROWID(bf-itemfg), "SF", OUTPUT bf-ar-invl.sf-sht).
                bf-ar-invl.spare-dec-1        = bf-itemfg.spare-dec-1.
            END.    
            iLine = iLine + 1.
                 
        END.
        FOR EACH bf-inv-misc WHERE bf-inv-misc.r-no EQ bf-inv-head.r-no:
            CREATE bf-ar-invl.
            ASSIGN
                bf-ar-invl.x-no           = bf-ar-inv.x-no
                bf-ar-invl.line           = iLine
                bf-ar-invl.company        = cTransCompany
                bf-ar-invl.INV-NO         = bf-ar-inv.inv-no
                bf-ar-invl.ord-no         = bf-inv-misc.ord-no
                bf-ar-invl.bol-no         = oe-bolh.bol-no
                bf-ar-invl.cust-no        = bf-cust.cust-no
                bf-ar-invl.est-no         = bf-inv-misc.est-no
                bf-ar-invl.tax            = bf-inv-misc.tax
                bf-ar-invl.actnum         = bf-inv-misc.actnum
                bf-ar-invl.prep-amt       = bf-inv-misc.amt
                bf-ar-invl.qty            = 1
                bf-ar-invl.unit-pr        = bf-inv-misc.amt
                bf-ar-invl.amt            = bf-inv-misc.amt
                bf-ar-invl.t-cost         = bf-inv-misc.cost
                bf-ar-invl.cost           = bf-ar-invl.t-cost / 1000
                bf-ar-invl.dscr[1]        = "M"
                bf-ar-invl.prep-charge    = bf-inv-misc.charge
                bf-ar-invl.prep-cost      = bf-inv-misc.cost
                bf-ar-invl.prep-dscr      = bf-inv-misc.dscr
                bf-ar-invl.i-name         = bf-inv-misc.charge
                bf-ar-invl.i-dscr         = bf-inv-misc.dscr
                bf-ar-invl.po-no          = bf-inv-misc.po-no
                bf-ar-invl.po-no-po       = bf-inv-misc.po-no-po
                bf-ar-invl.sman[1]        = bf-cust.sman                
                bf-ar-invl.s-pct[1]       = bf-inv-misc.s-pct[1]
                bf-ar-invl.s-pct[2]       = bf-inv-misc.s-pct[2]
                bf-ar-invl.s-pct[3]       = bf-inv-misc.s-pct[3]
                bf-ar-invl.s-comm[1]      = bf-inv-misc.s-comm[1]
                bf-ar-invl.s-comm[2]      = bf-inv-misc.s-comm[2]
                bf-ar-invl.s-comm[3]      = bf-inv-misc.s-comm[3]
                bf-ar-invl.s-commbasis[1] = bf-inv-misc.s-commbasis[1]
                bf-ar-invl.s-commbasis[2] = bf-inv-misc.s-commbasis[2]
                bf-ar-invl.s-commbasis[3] = bf-inv-misc.s-commbasis[3]
                bf-ar-invl.inv-i-no       = bf-inv-misc.inv-i-no
                bf-ar-invl.inv-line       = bf-inv-misc.inv-line
                bf-ar-invl.misc           = YES
                bf-ar-invl.billable       = bf-inv-misc.bill EQ "Y"
                bf-ar-invl.spare-char-1   = bf-inv-misc.spare-char-1
                bf-ar-invl.posted         = NO
                bf-ar-invl.inv-date       = bf-inv-head.inv-date
                bf-ar-invl.e-num          = bf-inv-misc.spare-int-4
                bf-ar-invl.taxGroup       = bf-inv-misc.spare-char-1
                .

            IF NOT bf-ar-invl.billable THEN bf-ar-invl.amt = 0. 
            iLine = iLine + 1.
        END.
    END.
       
    
END PROCEDURE. 

PROCEDURE pCreateInvoiceLineTax PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  create GL InvoiceLineTax
     Notes:          
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipriInvoiceLine AS ROWID NO-UNDO. 
       
    DEFINE BUFFER bf-InvoiceLineTax FOR InvoiceLineTax.
    DEFINE BUFFER bf-ar-invl FOR ar-invl.
    
    FIND FIRST bf-ar-invl NO-LOCK
         WHERE ROWID(bf-ar-invl) EQ ipriInvoiceLine NO-ERROR.    
               
    FOR EACH ttTaxDetail NO-LOCK
        WHERE ttTaxDetail.invoiceLineRecKey EQ ipcRecKey:         
                     
        CREATE bf-InvoiceLineTax.
        ASSIGN            
            bf-InvoiceLineTax.invoiceLineRecKey  = bf-ar-invl.rec_key             
            bf-InvoiceLineTax.isFreight          = ttTaxDetail.isFreight
            bf-InvoiceLineTax.isTaxOnFreight     = ttTaxDetail.isTaxOnFreight
            bf-InvoiceLineTax.isTaxOnTax         = ttTaxDetail.isTaxOnTax
            bf-InvoiceLineTax.taxCode            = ttTaxDetail.taxCode
            bf-InvoiceLineTax.taxCodeAccount     = ttTaxDetail.taxCodeAccount
            bf-InvoiceLineTax.taxCodeDescription = ttTaxDetail.taxCodeDescription
            bf-InvoiceLineTax.taxCodeRate        = ttTaxDetail.taxCodeRate 
            bf-InvoiceLineTax.taxableAmount      = ttTaxDetail.taxCodeTaxableAmount 
            bf-InvoiceLineTax.taxAmount          = ttTaxDetail.taxCodeTaxAmount 
            bf-InvoiceLineTax.taxGroup           = ttTaxDetail.taxGroup
            bf-InvoiceLineTax.taxGroupLine       = ttTaxDetail.taxGroupLine 
            bf-InvoiceLineTax.company            = bf-ar-invl.company
            bf-InvoiceLineTax.invoiceNo          = bf-ar-invl.inv-no
            bf-InvoiceLineTax.invoiceLineID      = bf-ar-invl.LINE
            bf-InvoiceLineTax.taxGroupTaxAmountLimit = ttTaxDetail.taxGroupTaxAmountLimit
            .
        RELEASE bf-InvoiceLineTax.
    END.
    
END PROCEDURE.

PROCEDURE pGetNk1TransCompany:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcReturnValue AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiInterCompanyBilling AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.    
    
    RUN sys/ref/nk1look.p (
                    INPUT ipcCompany,
                    INPUT "InterCompanyBilling",
                    INPUT "C" /* Logical */,
                    INPUT YES /* check by cust */,
                    INPUT YES /* use cust not vendor */, 
                    INPUT ipcCustomer /* cust */,
                    INPUT "" /* ship-to*/,
                    OUTPUT cReturnValue,
                    OUTPUT lRecFound).    
    iopcReturnValue = IF cReturnValue NE "" THEN cReturnValue ELSE iopcReturnValue.
    
    RUN sys/ref/nk1look.p (
                    INPUT ipcCompany,
                    INPUT "InterCompanyBilling",
                    INPUT "I" /* Logical */,
                    INPUT YES /* check by cust */,
                    INPUT YES /* use cust not vendor */, 
                    INPUT ipcCustomer /* cust */,
                    INPUT "" /* ship-to*/,
                    OUTPUT cReturnValue,
                    OUTPUT lRecFound).    
    opiInterCompanyBilling = INTEGER(cReturnValue) NO-ERROR.     
    
END PROCEDURE.

FUNCTION fGetNewInvoice RETURNS INTEGER 
     (ipcCompany AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE iReturnValue AS INTEGER NO-UNDO.
    DEFINE BUFFER b-ar-inv FOR ar-inv.
    iReturnValue = 0.

    FIND FIRST ar-ctrl WHERE ar-ctrl.company = ipcCompany NO-LOCK NO-ERROR.
    iReturnValue = IF AVAIL ar-ctrl THEN ar-ctrl.last-inv + 1 ELSE 1.

    DO WHILE TRUE:
        FIND FIRST b-ar-inv
            WHERE b-ar-inv.company EQ ipcCompany
              AND b-ar-inv.inv-no  EQ iReturnValue
            NO-LOCK NO-ERROR.
        FIND FIRST inv-head
            WHERE inv-head.company EQ ipcCompany
              AND inv-head.inv-no  EQ iReturnValue
            NO-LOCK NO-ERROR.
        IF NOT AVAIL b-ar-inv AND NOT AVAIL inv-head THEN LEAVE.

        iReturnValue = iReturnValue + 1.
    END.
    RETURN iReturnValue.
END FUNCTION.
