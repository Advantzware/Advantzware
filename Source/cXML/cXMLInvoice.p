
/*------------------------------------------------------------------------
    File        : cXMLInvoice.p
    Purpose     : Built to handle generation from posted invoices

    Syntax      :

    Description : Generates an invoice for a given invoice RowID

    Author(s)   : BV
    Created     : Fri Jul 24 13:25:27 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipriInv AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipdtInvDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipcInvSuffix AS CHARACTER NO-UNDO.

{system/TaxProcs.i}

DEFINE TEMP-TABLE ttInv NO-UNDO 
    FIELD invoiceID                   AS INTEGER
    FIELD invoiceIDString             AS CHARACTER
    FIELD deploymentMode              AS CHARACTER
    FIELD invoiceDate                 AS DATE
    FIELD invoiceDateString           AS CHARACTER
    FIELD customerID                  AS CHARACTER
    FIELD customerName                AS CHARACTER
    FIELD customerAddress1            AS CHARACTER
    FIELD customerAddress2            AS CHARACTER 
    FIELD customerCity                AS CHARACTER 
    FIELD customerState               AS CHARACTER 
    FIELD customerPostalCode          AS CHARACTER
    FIELD company                     AS CHARACTER
    FIELD companyName                 AS CHARACTER
    FIELD companyAddress1             AS CHARACTER
    FIELD companyAddress2             AS CHARACTER 
    FIELD companyCity                 AS CHARACTER 
    FIELD companyState                AS CHARACTER 
    FIELD companyPostalCode           AS CHARACTER 
    FIELD shiptoID                    AS CHARACTER
    FIELD shiptoName                  AS CHARACTER
    FIELD shiptoAddress1              AS CHARACTER
    FIELD shiptoAddress2              AS CHARACTER 
    FIELD shiptoCity                  AS CHARACTER 
    FIELD shiptoState                 AS CHARACTER 
    FIELD shiptoPostalCode            AS CHARACTER 
    FIELD termsDays                   AS INTEGER
    FIELD customerPO                  AS CHARACTER
    FIELD payloadID                   AS CHARACTER
    FIELD amountTotalLines            AS DECIMAL 
    FIELD amountTotalTax              AS DECIMAL 
    FIELD amountTotalTaxable          AS DECIMAL 
    FIELD amountTotalFreight          AS DECIMAL
    FIELD amountTotalTaxableFreight   AS DECIMAL 
    FIELD amountTotalTaxFreight       AS DECIMAL 
    FIELD amountTotalTaxExFreight     AS DECIMAL
    FIELD amountTotalTaxableExFreight AS DECIMAL
    FIELD amountTotal                 AS DECIMAL 
    FIELD taxGroup                    AS CHARACTER
    FIELD billFreight                 AS LOGICAL
    FIELD frtTaxRate                  AS DECIMAL
    .

DEFINE TEMP-TABLE ttInvLine NO-UNDO     
    FIELD invoiceID                AS INTEGER
    FIELD company                  AS CHARACTER
    FIELD lineNo                   AS INTEGER
    FIELD quantity                 AS INTEGER
    FIELD quantityUOM              AS CHARACTER 
    FIELD quantityInvoiced         AS INTEGER
    FIELD quantityInvoicedUOM      AS CHARACTER 
    FIELD quantityOrderOriginal    AS INTEGER
    FIELD quantityOrderOriginalUOM AS CHARACTER
    FIELD pricePerUOM              AS DECIMAL
    FIELD priceUOM                 AS CHARACTER 
    FIELD priceTotal               AS DECIMAL 
    FIELD customerPartID           AS CHARACTER
    FIELD itemID                   AS CHARACTER 
    FIELD itemName                 AS CHARACTER 
    FIELD amountTax                AS DECIMAL 
    FIELD amountTaxExFreightTax    AS DECIMAL
    FIELD amountFreightTax         AS DECIMAL 
    FIELD amountTaxable            AS DECIMAL
    FIELD amountTaxableExFreight   AS DECIMAL 
    FIELD amountTaxableFreight     AS DECIMAL 
    FIELD taxRate                  AS DECIMAL
    FIELD amountFreight            AS DECIMAL 
    FIELD taxable                  AS LOGICAL
    FIELD taxGroup                 AS CHARACTER 
    FIELD orderID                  AS INTEGER 
    FIELD orderLine                AS INTEGER 
    FIELD taxRateFreight           AS DECIMAL 
    FIELD customerPONo             AS CHARACTER
    FIELD isMisc                   AS LOGICAL 
    .
    
DEFINE VARIABLE gcCXMLIdentity       AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCXMLDeploymentMode AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCXMLShipToPrefix   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFirstLine           AS LOG       NO-UNDO.
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

&SCOPED-DEFINE sysCtrlcXML cXMLInvoice
{XMLOutput/XMLOutput.i &cXMLOutput={&sysCtrlcXML} &Company=ipcCompany &c=c}

RUN pBuildData(ipriInv).
RUN pGenerateCXML.

/* **********************  Internal Procedures  *********************** */

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
        ipbf-ttInv.deploymentMode    = gcCXMLDeploymentMode
        ipbf-ttInv.invoiceIDString   = STRING(ipbf-ttInv.invoiceID) + ipcInvSuffix
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
            ipbf-ttInv.shipToID         = gcCXMLShipToPrefix + bf-shipto.ship-id
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
        ASSIGN 
            ipbf-ttInv.termsDays = bf-terms.net-days
            .
    

    
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
    IF AVAILABLE bf-oe-ord THEN 
    DO:
        IF ipbf-ttInv.customerPO EQ "" THEN 
            ipbf-ttInv.customerPO EQ bf-oe-ord.po-no.
        IF ipbf-ttInv.payloadID EQ "" THEN 
            ipbf-ttInv.payloadID = bf-oe-ord.spare-char-3.
        FIND FIRST bf-oe-ordl NO-LOCK 
            WHERE bf-oe-ordl.company EQ bf-oe-ord.company
            AND bf-oe-ordl.ord-no EQ bf-oe-ord.ord-no
            AND bf-oe-ordl.i-no EQ ipbf-ttInvLine.itemID
            AND bf-oe-ordl.line EQ ipbf-ttInvLine.orderLine
            NO-ERROR.
        IF AVAILABLE bf-oe-ordl THEN 
        DO: 
            ASSIGN 
                ipbf-ttInvLine.quantityOrderOriginal    = bf-oe-ordl.spare-dec-1
                ipbf-ttInvLine.quantityOrderOriginalUOM = bf-oe-ordl.spare-char-2
                .
            
            IF ipbf-ttInvLine.quantityOrderOriginalUOM NE ""
                AND ipbf-ttInvLine.quantityOrderOriginalUOM NE ipbf-ttInvLine.quantityInvoicedUOM THEN 
            DO: 
                RUN Conv_QuantityFromUOMtoUOM(bf-oe-ordl.company, bf-oe-ordl.i-no, "FG", 
                    ipbf-ttInvLine.quantityInvoiced, ipbf-ttInvLine.quantityInvoicedUOM, ipbf-ttInvLine.quantityOrderOriginalUOM, 
                    0, 0, 0, 0, bf-oe-ordl.cas-cnt, 
                    OUTPUT ipbf-ttInvLine.quantity, OUTPUT lError, OUTPUT cErrorMessage).
                
                IF ipbf-ttInvLine.quantity EQ 0 THEN 
                    ASSIGN 
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
    IF ttInvLine.taxable THEN 
    DO:                                 
        /*IF ttInv.billFreight THEN 
        DO: 
            EMPTY TEMP-TABLE ttTaxDetail.                     
            RUN Tax_CalculateWithDetail  (
                INPUT  ipbf-ttInv.company,
                INPUT  ipbf-ttInv.taxGroup,
                INPUT  TRUE,   /* Is this freight */
                INPUT  ipbf-ttInvLine.amountTaxableFreight,
                OUTPUT ipbf-ttInvLine.amountFreightTax,
                OUTPUT TABLE ttTaxDetail
                ).
        END.
        ELSE 
            ttInvLine.amountTaxableFreight = 0.
        */
        ASSIGN     
            ipbf-ttInvLine.amountTax               = ipbf-ttInvLine.amountTaxExFreight + ipbf-ttInvLine.amountFreightTax
            ipbf-ttInvLine.amountTaxable           = ipbf-ttInvLine.amountTaxableExFreight + ipbf-ttInvLine.amountTaxableFreight
            ipbf-ttInv.amountTotalTaxableExFreight = ipbf-ttInv.amountTotalTaxableExFreight + ipbf-ttInvLine.amountTaxableExFreight
            ipbf-ttInv.amountTotalTaxExFreight     = ipbf-ttInv.amountTotalTaxExFreight + ipbf-ttInvLine.amountTaxExFreight
            ipbf-ttInv.amountTotalTaxable          = ipbf-ttInv.amountTotalTaxable + ipbf-ttInvLine.amountTaxable
            .
    END.
END PROCEDURE.

PROCEDURE pBuildData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given ROWID - determine ROWID type and build data for a given data type
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriInv AS ROWID NO-UNDO.
    DEFINE BUFFER bf-inv-head FOR inv-head.
    DEFINE BUFFER bf-ar-inv   FOR ar-inv.
    
    EMPTY TEMP-TABLE ttInv.
    EMPTY TEMP-TABLE ttInvLIne.
    
    FIND FIRST bf-ar-inv NO-LOCK
        WHERE ROWID(bf-ar-inv) EQ ipriInv
        NO-ERROR.
    IF NOT AVAILABLE bf-ar-inv THEN 
    DO: 
        FIND FIRST bf-inv-head NO-LOCK 
            WHERE ROWID(bf-inv-head) EQ ipriInv
            NO-ERROR.
        IF AVAILABLE bf-inv-head THEN 
            RUN pBuildDataForUnposted(BUFFER bf-inv-head).
    END.
    ELSE 
        RUN pBuildDataForPosted(BUFFER bf-ar-inv).
          

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
    DEFINE VARIABLE dLineTaxAmt      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dLineTaxRate     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dFrtTaxRate      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dFrtTaxAmt       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lFirst           AS LOGICAL   NO-UNDO.
    
    IF AVAILABLE ipbf-ar-inv THEN 
    DO:
        EMPTY TEMP-TABLE ttTaxDetail.
        ASSIGN 
            dFrtTaxRate = 0
            dFrtTaxAmt  = 0
            .
        RUN pGetSettings(ipbf-ar-inv.company, ipbf-ar-inv.cust-no, ipbf-ar-inv.ship-id).
             
        CREATE ttInv.
        ASSIGN             
            ttInv.invoiceDate        = ipdtInvDate 
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
            ttInv.amountTotalFreight = IF ttInv.billFreight THEN ipbf-ar-inv.freight ELSE 0
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
        RUN pAssignCommonHeaderData(BUFFER ttInv, 
            ipbf-ar-inv.company, 
            ipbf-ar-inv.cust-no, 
            ipbf-ar-inv.ship-id,
            ipbf-ar-inv.terms).
            
        lFirst = YES.   
         
        FOR EACH bf-ar-invl NO-LOCK
            WHERE bf-ar-invl.x-no EQ ipbf-ar-inv.x-no:
                
            ASSIGN
                dLineTaxAmt  = 0
                dLineTaxRate = 0 
                .             
            CREATE ttInvLine.
            ASSIGN 
                ttInvLine.invoiceID              = ttInv.invoiceID
                ttInvLine.company                = ttInv.company
                ttInvLine.lineNo                 = bf-ar-invl.line
                ttInvLine.orderID                = bf-ar-invl.ord-no
                ttInvLine.orderLine              = bf-ar-invl.ord-line
                ttInvLine.quantityInvoiced       = bf-ar-invl.inv-qty
                ttInvLine.quantityInvoicedUOM    = bf-ar-invl.pr-qty-uom
                ttInvLine.pricePerUOM            = bf-ar-invl.unit-pr * (1 - (bf-ar-invl.disc / 100))
                ttInvLine.priceUOM               = bf-ar-invl.pr-uom
                ttInvLine.customerPartID         = bf-ar-invl.part-no
                ttInvLine.itemID                 = bf-ar-invl.i-no
                ttInvLine.itemName               = bf-ar-invl.i-name
                ttInvLine.priceTotal             = bf-ar-invl.amt
                ttInvLine.taxable                = bf-ar-invl.tax
                ttInvLine.amountTaxableExFreight = ttInvLine.priceTotal
                ttInvLine.amountTaxableFreight   = bf-ar-invl.t-freight
                ttInvLine.amountFreight          = bf-ar-invl.t-freight
                ttInvLine.customerPONo           = bf-ar-invl.po-no
                ttInvLine.isMisc                 = bf-ar-invl.misc
                ttInv.amountTotalLines           = ttInv.amountTotalLines + bf-ar-invl.amt
                .
                
            FOR EACH ttTaxDetail
                WHERE ttTaxDetail.invoiceLineType   EQ "ARINVL"
                AND ttTaxDetail.invoiceLineRecKey EQ bf-ar-invl.rec_key:
                IF NOT ttTaxDetail.isFreight THEN 
                    ASSIGN 
                        dLineTaxAmt  = dLineTaxAmt  + ttTaxDetail.taxCodeTaxAmount
                        dLineTaxRate = dLineTaxRate + ttTaxDetail.taxCodeRate
                        .  
                IF ttTaxDetail.isFreight THEN 
                DO:
                    IF lFirst THEN 
                    DO:
                        ASSIGN
                            dFrtTaxRate = dFrtTaxRate + ttTaxDetail.taxCodeRate
                            lFirst      = NO
                            .
                    END.    
                    dFrtTaxAmt  = dFrtTaxAmt  + ttTaxDetail.taxCodeTaxAmount.
                END.                                           
            END.              
            ASSIGN 
                ttInvLine.amountTaxExFreight = dLineTaxAmt
                ttInvLine.taxRate            = dLineTaxRate 
                .
            FIND FIRST bf-oe-ordl NO-LOCK
                WHERE bf-oe-ordl.company EQ bf-ar-invl.company
                AND bf-oe-ordl.i-no    EQ bf-ar-invl.i-no
                AND bf-oe-ordl.ord-no  EQ bf-ar-invl.ord-no
                NO-ERROR.

            IF AVAILABLE bf-oe-ordl THEN 
                ttInvLine.orderLine = bf-oe-ordl.line.
            
            RUN pAssignCommonLineData(BUFFER ttInv, 
                BUFFER ttInvLine).            
                
        END.
        ASSIGN 
            ttInv.amountTotal           = ttInv.amountTotalLines + ttInv.amountTotalTax + ttInv.amountTotalFreight
            ttInv.frtTaxRate            = dFrtTaxRate  
            ttInv.amountTotalTaxFreight = dFrtTaxAmt
            .
            
    END.
END PROCEDURE.

PROCEDURE pBuildDataForUnposted PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given inv-head buffer - Initialize and process the buffer to create the cXML 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-inv-head FOR inv-head.
    
    DEFINE           BUFFER bf-inv-line   FOR inv-line.
    DEFINE           BUFFER bf-inv-misc   FOR inv-misc.
    DEFINE           BUFFER bf-oe-ordl    FOR oe-ordl.
    
    DEFINE VARIABLE dTaxTotal        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvoiceTotal    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvoiceSubTotal AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dLineTaxAmt      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dLineTaxRate     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dFrtTaxRate      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dFrtTaxAmt       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lFirst           AS LOGICAL   NO-UNDO.
    
    IF AVAILABLE ipbf-inv-head THEN 
    DO:
        RUN pGetSettings(ipbf-inv-head.company, ipbf-inv-head.cust-no, "").
        EMPTY TEMP-TABLE ttTaxDetail.
        ASSIGN 
            dFrtTaxRate = 0
            dFrtTaxAmt  = 0
            .         
        CREATE ttInv.
        ASSIGN             
            ttInv.invoiceDate        = ipdtInvDate 
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
            ttInv.amountTotalFreight = IF ttInv.billFreight THEN ipbf-inv-head.t-inv-freight ELSE 0
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
        RUN pAssignCommonHeaderData(BUFFER ttInv, 
            ipbf-inv-head.company, 
            ipbf-inv-head.cust-no, 
            ipbf-inv-head.sold-no,
            ipbf-inv-head.terms).
            
        lFirst = YES.   
         
        FOR EACH bf-inv-line NO-LOCK
            WHERE bf-inv-line.r-no EQ ipbf-inv-head.r-no:
                
            ASSIGN
                dLineTaxAmt  = 0
                dLineTaxRate = 0 
                .             
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
                ttInvLine.amountTaxableExFreight = ttInvLine.priceTotal
                ttInvLine.amountTaxableFreight   = bf-inv-line.t-freight
                ttInvLine.amountFreight          = bf-inv-line.t-freight
                ttInvLine.customerPONo           = bf-inv-line.po-no
                ttInvLine.isMisc                 = NO
                ttInv.amountTotalLines           = ttInv.amountTotalLines + bf-inv-line.t-price
                .
                
            FOR EACH ttTaxDetail
                WHERE ttTaxDetail.invoiceLineType   EQ "OEINVL"
                AND ttTaxDetail.invoiceLineRecKey EQ bf-inv-line.rec_key:
                IF NOT ttTaxDetail.isFreight THEN 
                    ASSIGN 
                        dLineTaxAmt  = dLineTaxAmt  + ttTaxDetail.taxCodeTaxAmount
                        dLineTaxRate = dLineTaxRate + ttTaxDetail.taxCodeRate
                        .  
                IF ttTaxDetail.isFreight THEN 
                DO:
                    IF lFirst THEN 
                    DO:
                        ASSIGN
                            dFrtTaxRate = dFrtTaxRate + ttTaxDetail.taxCodeRate
                            lFirst      = NO
                            .
                    END.    
                    dFrtTaxAmt  = dFrtTaxAmt  + ttTaxDetail.taxCodeTaxAmount.
                END.                                           
            END.              
            ASSIGN 
                ttInvLine.amountTaxExFreight = dLineTaxAmt
                ttInvLine.taxRate            = dLineTaxRate 
                .
            FIND FIRST bf-oe-ordl NO-LOCK
                WHERE bf-oe-ordl.company EQ bf-inv-line.company
                AND bf-oe-ordl.i-no    EQ bf-inv-line.i-no
                AND bf-oe-ordl.ord-no  EQ bf-inv-line.ord-no
                NO-ERROR.

            IF AVAILABLE bf-oe-ordl THEN 
                ttInvLine.orderLine = bf-oe-ordl.line.
            
            RUN pAssignCommonLineData(BUFFER ttInv, 
                BUFFER ttInvLine).            
                
        END.
        FOR EACH bf-inv-misc NO-LOCK
            WHERE bf-inv-misc.r-no EQ ipbf-inv-head.r-no:
                
            ASSIGN
                dLineTaxAmt  = 0
                dLineTaxRate = 0 
                .             
/*            CREATE ttInvLine.                                                                        */
/*            ASSIGN                                                                                   */
/*                ttInvLine.invoiceID              = ttInv.invoiceID                                   */
/*                ttInvLine.company                = ttInv.company                                     */
/*                ttInvLine.lineNo                 = bf-inv-misc.line                                  */
/*                ttInvLine.orderID                = bf-inv-misc.ord-no                                */
/*                ttInvLine.orderLine              = bf-inv-misc.line                                  */
/*                ttInvLine.quantityInvoiced       = bf-inv-misc.                                      */
/*                ttInvLine.quantityInvoicedUOM    = bf-inv-misc.pr-qty-uom                            */
/*                ttInvLine.pricePerUOM            = bf-inv-misc.price * (1 - (bf-inv-misc.disc / 100))*/
/*                ttInvLine.priceUOM               = bf-inv-misc.pr-uom                                */
/*                ttInvLine.customerPartID         = bf-inv-misc.part-no                               */
/*                ttInvLine.itemID                 = bf-inv-misc.i-no                                  */
/*                ttInvLine.itemName               = bf-inv-misc.i-name                                */
/*                ttInvLine.priceTotal             = bf-inv-misc.t-price                               */
/*                ttInvLine.taxable                = bf-inv-misc.tax                                   */
/*                ttInvLine.amountTaxableExFreight = ttInvLine.priceTotal                              */
/*                ttInvLine.amountTaxableFreight   = bf-inv-misc.t-freight                             */
/*                ttInvLine.amountFreight          = bf-inv-misc.t-freight                             */
/*                ttInvLine.customerPONo           = bf-inv-misc.po-no                                 */
/*                ttInvLine.isMisc                 = NO                                                */
/*                ttInv.amountTotalLines           = ttInv.amountTotalLines + bf-inv-misc.amt          */
/*                .                                                                                    */
                
            FOR EACH ttTaxDetail
                WHERE ttTaxDetail.invoiceLineType   EQ "OEINVL"
                AND ttTaxDetail.invoiceLineRecKey EQ bf-inv-misc.rec_key:
                IF NOT ttTaxDetail.isFreight THEN 
                    ASSIGN 
                        dLineTaxAmt  = dLineTaxAmt  + ttTaxDetail.taxCodeTaxAmount
                        dLineTaxRate = dLineTaxRate + ttTaxDetail.taxCodeRate
                        .  
                IF ttTaxDetail.isFreight THEN 
                DO:
                    IF lFirst THEN 
                    DO:
                        ASSIGN
                            dFrtTaxRate = dFrtTaxRate + ttTaxDetail.taxCodeRate
                            lFirst      = NO
                            .
                    END.    
                    dFrtTaxAmt  = dFrtTaxAmt  + ttTaxDetail.taxCodeTaxAmount.
                END.                                           
            END.              
            ASSIGN 
                ttInvLine.amountTaxExFreight = dLineTaxAmt
                ttInvLine.taxRate            = dLineTaxRate 
                .
/*            FIND FIRST bf-oe-ordl NO-LOCK                      */
/*                WHERE bf-oe-ordl.company EQ bf-inv-misc.company*/
/*                AND bf-oe-ordl.i-no    EQ bf-inv-misc.i-no     */
/*                AND bf-oe-ordl.ord-no  EQ bf-inv-misc.ord-no   */
/*                NO-ERROR.                                      */
/*                                                               */
/*            IF AVAILABLE bf-oe-ordl THEN                       */
/*                ttInvLine.orderLine = bf-oe-ordl.line.         */
            
            RUN pAssignCommonLineData(BUFFER ttInv, 
                BUFFER ttInvLine).            
                
        END.
        ASSIGN 
            ttInv.amountTotal           = ttInv.amountTotalLines + ttInv.amountTotalTax + ttInv.amountTotalFreight
            ttInv.frtTaxRate            = dFrtTaxRate  
            ttInv.amountTotalTaxFreight = dFrtTaxAmt
            .    
        
    END.
    
END PROCEDURE.

PROCEDURE pGenerateCXML PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes the 
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH ttInv:
    {XMLOutput/cXMLCust.i
        &cXMLSysCtrl={&sysCtrlcXML}
        &Company=ttInv.company
        &Customer=ttInv.customerID}
        RUN cXMLOutput (clXMLOutput,'Request deploymentMode="' + ttInv.deploymentMode + '"','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailRequest','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailRequestHeader ' 
            + 'invoiceDate="' + ttInv.invoiceDateString + '" '
            + 'invoiceID="' + ttInv.invoiceIDString + '" '
            + 'operation="new" purpose="standard"','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailHeaderIndicator/','','Row').  
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailLineIndicator isShippingInLine="yes" isAccountingInLine="yes" isTaxInLine="yes" /','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoicePartner','','Row').
        RUN cXMLOutput (clXMLOutput,'Contact role="billTo"','','Row').
        RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
        RUN cXMLOutput (clXMLOutput,'',ttInv.customerName,'Col').
        RUN cXMLOutput (clXMLOutput,'/Name','','Row').
        RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'Street',ttInv.customerAddress1,'Col').
        IF ttInv.customerAddress2 NE '' THEN 
            RUN cXMLOutput (clXMLOutput,'Street',ttInv.customerAddress2,'Col').
        RUN cXMLOutput (clXMLOutput,'City',ttInv.customerCity,'Col').
        RUN cXMLOutput (clXMLOutput,'State',ttInv.customerState,'Col').
        RUN cXMLOutput (clXMLOutput,'PostalCode',ttInv.customerPostalCode,'Col').
        RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','US','Col').
        RUN cXMLOutput (clXMLOutput,'/Country','','Row').
        RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
        RUN cXMLOutput (clXMLOutput,'/InvoicePartner','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoicePartner','','Row').
        RUN cXMLOutput (clXMLOutput,'Contact addressID="Premier Packaging" role="remitTo"','','Row').
        RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','Premier Packaging','Col').
        RUN cXMLOutput (clXMLOutput,'/Name','','Row').
        RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'Street','3254 Reliable Pkwy','Col').
        RUN cXMLOutput (clXMLOutput,'City','Chicago','Col').
        RUN cXMLOutput (clXMLOutput,'State','IL','Col').
        RUN cXMLOutput (clXMLOutput,'PostalCode','60686','Col').
        RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','US','Col').
        RUN cXMLOutput (clXMLOutput,'/Country','','Row').
        RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
        RUN cXMLOutput (clXMLOutput,'/InvoicePartner','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailShipping','','Row').
        RUN cXMLOutput (clXMLOutput,'Contact addressID="' + ttInv.shiptoID + '" role="shipTo"','','Row').
        RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
        RUN cXMLOutput (clXMLOutput,'',ttInv.shiptoName,'Col').
        RUN cXMLOutput (clXMLOutput,'/Name','','Row').
        RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'Street',ttInv.shiptoAddress1,'Col').
        IF ttInv.shiptoAddress2 NE "" AND ttInv.shiptoAddress2 NE '345 Court Street' THEN
            RUN cXMLOutput (clXMLOutput,'Street',ttInv.shiptoAddress2,'Col').
        RUN cXMLOutput (clXMLOutput,'City',ttInv.shiptoCity,'Col').
        RUN cXMLOutput (clXMLOutput,'State',ttInv.shiptoState,'Col').
        RUN cXMLOutput (clXMLOutput,'PostalCode',ttInv.shiptoPostalCode,'Col').
        RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','US','Col').
        RUN cXMLOutput (clXMLOutput,'/Country','','Row').
        RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
        RUN cXMLOutput (clXMLOutput,'Contact role="shipFrom"','','Row').
        RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','Premier Packaging','Col').
        RUN cXMLOutput (clXMLOutput,'/Name','','Row').
        RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'Street','3900 Produce Road','Col').
        RUN cXMLOutput (clXMLOutput,'City','Louisville','Col').
        RUN cXMLOutput (clXMLOutput,'State','KY','Col').
        RUN cXMLOutput (clXMLOutput,'PostalCode','40218','Col').
        RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','US','Col').
        RUN cXMLOutput (clXMLOutput,'/Country','','Row').
        RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
        RUN cXMLOutput (clXMLOutput,'/InvoiceDetailShipping','','Row'). 
        RUN cXMLOutput (clXMLOutput,'PaymentTerm payInNumberOfDays="' + STRING(ttInv.termsDays)  + '" /','','Row').
        /*         RUN cXMLOutput (clXMLOutput,'Extrinsic name="invoiceSubmissionMethod"','','Row'). */
        /*         RUN cXMLOutput (clXMLOutput,'','CSVUpload','Col').                                */
        /*         RUN cXMLOutput (clXMLOutput,'/Extrinsic','','Row').                               */
        RUN cXMLOutput (clXMLOutput,'/InvoiceDetailRequestHeader ','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailOrder','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailOrderInfo','','Row').
        RUN cXMLOutput (clXMLOutput,'OrderReference orderID="' + ttInv.customerPO + '"','','Row').
        RUN cXMLOutput (clXMLOutput,'DocumentReference payloadID="' + ttInv.payloadID + '"','','Row').
        RUN cXMLOutput (clXMLOutput,'/DocumentReference','','Row').
        RUN cXMLOutput (clXMLOutput,'/OrderReference','','Row').
        RUN cXMLOutput (clXMLOutput,'/InvoiceDetailOrderInfo','','Row').
        
        lFirstLine = TRUE.
        FOR EACH ttInvLine
            WHERE ttInvLine.isMisc EQ NO:
            RUN cXMLOutput (clXMLOutput,'InvoiceDetailItem invoiceLineNumber="' + STRING(ttInvLine.orderLine) 
                + '" quantity="' + STRING(ttInvLine.quantity) + '"','','Row').
            RUN cXMLOutput (clXMLOutput,'UnitOfMeasure',ttInvLine.priceUOM,'Col').
            RUN cXMLOutput (clXMLOutput,'UnitPrice','','Row'). 
            RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
            RUN cXMLOutput (clXMLOutput,'',STRING(ttInvLine.pricePerUOM),'Col').
            RUN cXMLOutput (clXMLOutput,'/Money','','Row').
            RUN cXMLOutput (clXMLOutput,'/UnitPrice','','Row'). 
            RUN cXMLOutput (clXMLOutput,'InvoiceDetailItemReference lineNumber="' + STRING(ttInvLine.orderLine) + '"','','Row').
            RUN cXMLOutput (clXMLOutput,'ItemID','','Row'). 
            RUN cXMLOutput (clXMLOutput,'SupplierPartID',ttInvLine.customerPartID,'Col').
            RUN cXMLOutput (clXMLOutput,'/ItemID','','Row'). 
            RUN cXMLOutput (clXMLOutput,'Description xml:lang="en-US"','','Row').
            RUN cXMLOutput (clXMLOutput,'',ttInvLine.itemName,'Col').
            RUN cXMLOutput (clXMLOutput,'/Description','','Row').
            RUN cXMLOutput (clXMLOutput,'/InvoiceDetailItemReference','','Row').
            RUN cXMLOutput (clXMLOutput,'SubtotalAmount','','Row'). 
            RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
            RUN cXMLOutput (clXMLOutput,'',STRING(ttInvLine.priceTotal),'Col').
            RUN cXMLOutput (clXMLOutput,'/Money','','Row').
            RUN cXMLOutput (clXMLOutput,'/SubtotalAmount','','Row').                      
            RUN cXMLOutput (clXMLOutput,'InvoiceDetailLineShipping','','Row').
            RUN cXMLOutput (clXMLOutput,'InvoiceDetailShipping','','Row').
            RUN cXMLOutput (clXMLOutput,'Contact addressID="' + ttInv.shiptoID + '" role="shipTo"','','Row').
            RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
            RUN cXMLOutput (clXMLOutput,'',ttInv.shiptoName,'Col').
            RUN cXMLOutput (clXMLOutput,'/Name','','Row').
            RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
            RUN cXMLOutput (clXMLOutput,'Street',ttInv.shiptoAddress1,'Col').
            IF ttInv.shiptoAddress2 NE "" AND ttInv.shiptoAddress2 NE '345 Court Street' THEN 
                RUN cXMLOutput (clXMLOutput,'Street',ttInv.shiptoAddress2,'Col').
            RUN cXMLOutput (clXMLOutput,'City',ttInv.shiptoCity,'Col').
            RUN cXMLOutput (clXMLOutput,'State',ttInv.shiptoState,'Col').
            RUN cXMLOutput (clXMLOutput,'PostalCode',ttInv.shiptoPostalCode,'Col').
            RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
            RUN cXMLOutput (clXMLOutput,'','US','Col').
            RUN cXMLOutput (clXMLOutput,'/Country','','Row').
            RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
            RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
            RUN cXMLOutput (clXMLOutput,'Contact role="shipFrom"','','Row').
            RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
            RUN cXMLOutput (clXMLOutput,'','Premier Packaging','Col').
            RUN cXMLOutput (clXMLOutput,'/Name','','Row').
            RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
            RUN cXMLOutput (clXMLOutput,'Street','3900 Produce Road','Col').
            RUN cXMLOutput (clXMLOutput,'City','Louisville','Col').
            RUN cXMLOutput (clXMLOutput,'State','KY','Col').
            RUN cXMLOutput (clXMLOutput,'PostalCode','40218','Col').
            RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
            RUN cXMLOutput (clXMLOutput,'','US','Col').
            RUN cXMLOutput (clXMLOutput,'/Country','','Row').
            RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
            RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
            RUN cXMLOutput (clXMLOutput,'/InvoiceDetailShipping','','Row').
            RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
            /* Assign total freight to first detail line since line-level freight not supported */
            IF lFirstLine THEN 
            DO: 
                RUN cXMLOutput (clXMLOutput,'',STRING(ttInv.amountTotalFreight),'Col').
                lFirstLine = NO.
            END.
            ELSE
                RUN cXMLOutput (clXMLOutput,'','0','Col').            RUN cXMLOutput (clXMLOutput,'/Money','','Row').
            RUN cXMLOutput (clXMLOutput,'/InvoiceDetailLineShipping','','Row').
            RUN cXMLOutput (clXMLOutput,'/InvoiceDetailItem','','Row'). 
        END.
        
        RUN cXMLOutput (clXMLOutput,'/InvoiceDetailOrder','','Row'). 
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailSummary','','Row').
        RUN cXMLOutput (clXMLOutput,'SubtotalAmount','','Row').
        RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
        RUN cXMLOutput (clXMLOutput,'',STRING(ttInv.amountTotalLines),'Col').
        RUN cXMLOutput (clXMLOutput,'/Money','','Row').
        RUN cXMLOutput (clXMLOutput,'/SubtotalAmount','','Row').
        RUN cXMLOutput (clXMLOutput,'Tax','','Row').
        RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
        RUN cXMLOutput (clXMLOutput,'',STRING(ttInv.amountTotalTax),'Col').
        RUN cXMLOutput (clXMLOutput,'/Money','','Row').
        RUN cXMLOutput (clXMLOutput,'Description xml:lang="en-US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','Sales Tax','Col').
        RUN cXMLOutput (clXMLOutput,'/Description','','Row').
        RUN cXMLOutput (clXMLOutput,'TaxDetail category="SalesTax"'+
            ' percentageRate="0"','','Row').
        RUN cXMLOutput (clXMLOutput,'TaxableAmount','','Row').    
        RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
        RUN cXMLOutput (clXMLOutput,'',STRING(ttInv.amountTotalTaxableExFreight),'Col').
        RUN cXMLOutput (clXMLOutput,'/Money','','Row').
        RUN cXMLOutput (clXMLOutput,'/TaxableAmount','','Row').    
        RUN cXMLOutput (clXMLOutput,'TaxAmount','','Row').    
        RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
        RUN cXMLOutput (clXMLOutput,'',STRING(ttInv.amountTotalTaxExFreight),'Col').
        RUN cXMLOutput (clXMLOutput,'/Money','','Row').
        RUN cXMLOutput (clXMLOutput,'/TaxAmount','','Row').    
        RUN cXMLOutput (clXMLOutput,'/TaxDetail','','Row'). 
        /* Seperate section for handling shipping Tax */
        IF ttInv.billFreight AND ttInv.amountTotalFreight NE 0 
            AND ttInv.frtTaxRate NE 0 THEN DO:       
            RUN cXMLOutput (clXMLOutput,'TaxDetail purpose="shippingTax" category="sales"' + ' percentageRate="' + STRING(ttInv.frtTaxRate) + '"','','Row').             
            RUN cXMLOutput (clXMLOutput,'TaxableAmount','','Row').
            RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').              
            RUN cXMLOutput (clXMLOutput,'',STRING(ttInv.amountTotalFreight),'Col').
            RUN cXMLOutput (clXMLOutput,'/Money','','Row').             
            RUN cXMLOutput (clXMLOutput,'/TaxableAmount','','Row').
            RUN cXMLOutput (clXMLOutput,'TaxAmount','','Row').
            RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').              
            RUN cXMLOutput (clXMLOutput,'',STRING(ttInv.amountTotalTaxFreight),'Col').
            RUN cXMLOutput (clXMLOutput,'/Money','','Row').             
            RUN cXMLOutput (clXMLOutput,'/TaxAmount','','Row').
            RUN cXMLOutput (clXMLOutput,'Description xml:lang="en-US"','','Row').
            RUN cXMLOutput (clXMLOutput,'/Description','','Row').
            RUN cXMLOutput (clXMLOutput,'/TaxDetail','','Row').
        END.           
//Good from here               
        RUN cXMLOutput (clXMLOutput,'/Tax','','Row').
        RUN cXMLOutput (clXMLOutput,'SpecialHandlingAmount','','Row').
        RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
        RUN cXMLOutput (clXMLOutput,'','0','Col').
        RUN cXMLOutput (clXMLOutput,'/Money','','Row').
        RUN cXMLOutput (clXMLOutput,'/SpecialHandlingAmount','','Row').
        RUN cXMLOutput (clXMLOutput,'ShippingAmount','','Row').
        RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
        RUN cXMLOutput (clXMLOutput,'',STRING(ttInv.amountTotalFreight),'Col').
        RUN cXMLOutput (clXMLOutput,'/Money','','Row').
        RUN cXMLOutput (clXMLOutput,'/ShippingAmount','','Row').
        RUN cXMLOutput (clXMLOutput,'GrossAmount','','Row').
        RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
        RUN cXMLOutput (clXMLOutput,'',STRING(ttInv.amountTotal),'Col').
        RUN cXMLOutput (clXMLOutput,'/Money','','Row').
        RUN cXMLOutput (clXMLOutput,'/GrossAmount','','Row').
        RUN cXMLOutput (clXMLOutput,'NetAmount','','Row').
        RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
        RUN cXMLOutput (clXMLOutput,'',STRING(ttInv.amountTotal),'Col').
        RUN cXMLOutput (clXMLOutput,'/Money','','Row').
        RUN cXMLOutput (clXMLOutput,'/NetAmount','','Row').
        RUN cXMLOutput (clXMLOutput,'/InvoiceDetailSummary','','Row').
        RUN cXMLOutput (clXMLOutput,'/InvoiceDetailRequest','','Row').
        RUN cXMLOutput (clXMLOutput,'/Request','','Row').
        /* rstark 05291402 */
    
        {XMLOutput/XMLOutput.i &c=c &XMLClose} /* rstark 05291402 */
    END.

END PROCEDURE.

PROCEDURE pGetSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the global settings for this this procedure from NK1s
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipToID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

    cXMLDTD = 'http://xml.cxml.org/schemas/cXML/1.2.025/InvoiceDetail.dtd'.
    RUN sys/ref/nk1look.p (ipcCompany, "cXMLIdentity", "C" /* Logical */, YES /* check by cust */, 
        INPUT YES /* use cust not vendor */, ipcCustomerID /* cust */, ipcShipToID /* ship-to*/,
        OUTPUT cReturn, OUTPUT lFound).
    gcCXMLIdentity = cReturn.  
    RUN sys/ref/nk1look.p (ipcCompany, "cXMLIdentity", "L" /* Logical */, NO /* check by cust */, 
        INPUT NO /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturn, OUTPUT lFound).
    gcCXMLDeploymentMode = IF cReturn EQ "YES" THEN "production" ELSE "test".
    RUN sys/ref/nk1look.p (ipcCompany, "cXMLShipToPrefix", "C" /* Logical */, YES /* check by cust */, 
        INPUT YES /* use cust not vendor */, ipcCustomerID /* cust */, ipcShipToID /* ship-to*/,
        OUTPUT cReturn, OUTPUT lFound).
    gcCXMLShipToPrefix = TRIM(cReturn).  


END PROCEDURE.
