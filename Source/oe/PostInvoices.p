
/*------------------------------------------------------------------------
    File        : oe/PostInvoices.p
    Purpose     : 

    Syntax      :

    Description : Given Range of OE Invoice #s, 
            Post Invoices from Order Processing to AR		

    Author(s)   : BV
    Created     : Thu Apr 30 14:34:07 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiInvNoStart AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipiInvNoEnd AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipdtInvDateStart AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipdtInvDateEnd AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipcCustomerIDStart AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCustomerIDEnd AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdtPostDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipcOptions AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttPostingMaster
    FIELD company           AS CHARACTER
    FIELD blockZeroCost     AS LOGICAL
    FIELD invoiceStart      AS INTEGER 
    FIELD invoiceEnd        AS INTEGER 
    FIELD invoiceDateStart  AS DATE 
    FIELD invoiceDateEnd    AS DATE 
    FIELD customerIDStart   AS CHARACTER 
    FIELD customerIDEnd     AS CHARACTER
    FIELD postDate          AS DATE
    FIELD periodID          AS INTEGER 
    FIELD periodDateStart   AS DATE
    FIELD periodDateEnd     AS DATE 
    FIELD accountAR         AS CHARACTER
    FIELD accountARFreight  AS CHARACTER
    FIELD accountARSales    AS CHARACTER
    FIELD accountARSalesTax AS CHARACTER
    FIELD accountARDiscount AS CHARACTER
    FIELD accountARCash     AS CHARACTER 
    .
    
DEFINE TEMP-TABLE ttInvoiceToPost
    FIELD riInvHead                    AS ROWID
    FIELD isOKToPost                   AS LOGICAL
    FIELD rNo                          AS INTEGER
    FIELD riCust                       AS ROWID
    FIELD company                      AS CHARACTER
    FIELD postDate                     AS DATE  
    FIELD postDatePeriod               AS INTEGER 
    FIELD customerID                   AS CHARACTER 
    FIELD invoiceID                    AS INTEGER 
    FIELD invoiceDate                  AS DATE    
    FIELD isFactored                   AS LOGICAL
    FIELD problemMessage               AS CHARACTER 
    FIELD orderID                      AS INTEGER
    FIELD orderDate                    AS DATE
    FIELD quantityTotal                AS INTEGER
    FIELD quantityTotalWeight          AS DECIMAL
    FIELD currencyCode                 AS CHARACTER 
    FIELD currencyExRate               AS DECIMAL
    FIELD currencyARAccount            AS CHARACTER
    FIELD accountAR                    AS CHARACTER
    FIELD accountARFreight             AS CHARACTER
    FIELD accountARSales               AS CHARACTER
    FIELD accountARSalesTax            AS CHARACTER
    FIELD accountARDiscount            AS CHARACTER
    FIELD accountARCash                AS CHARACTER    
    FIELD amountBilled                 AS DECIMAL
    FIELD amountBilledIncDiscount      AS DECIMAL
    FIELD amountBilledExTax            AS DECIMAL 
    FIELD amountBilledTax              AS DECIMAL
    FIELD amountBilledFreight          AS DECIMAL
    FIELD amountBilledMiscOnly         AS DECIMAL
    FIELD amountBilledLineOnly         AS DECIMAL
    FIELD amountCommission             AS DECIMAL 
    FIELD amountDiscount               AS DECIMAL
    FIELD amountCost                   AS DECIMAL 
    FIELD isCashTerms                  AS LOGICAL
    FIELD isFreightBillable            AS LOGICAL
    FIELD isInvoiceDateInCurrentPeriod AS LOGICAL     
    .
    
DEFINE TEMP-TABLE ttInvoiceLineToPost
    FIELD riInvLine               AS ROWID
    FIELD rNo                     AS INTEGER 
    FIELD rNoOld                  AS INTEGER
    FIELD company                 AS CHARACTER 
    FIELD invoiceID               AS INTEGER
    FIELD isOKToPost              AS LOGICAL
    FIELD problemMessage          AS CHARACTER  
    FIELD orderID                 AS INTEGER
    FIELD itemID                  AS CHARACTER                        
    FIELD itemName                AS CHARACTER                     
    FIELD quantityOrdered         AS DECIMAL
    FIELD quantityInvoiced        AS DECIMAL 
    FIELD quantityShipped         AS DECIMAL
    FIELD pricePerUOM             AS DECIMAL
    FIELD priceUOM                AS CHARACTER
    FIELD costPerUOM              AS DECIMAL 
    FIELD costUOM                 AS CHARACTER
    FIELD costTotal               AS DECIMAL 
    FIELD costDirectLabor         AS DECIMAL 
    FIELD costFixedOverhead       AS DECIMAL 
    FIELD costVariableOverhead    AS DECIMAL 
    FIELD costDirectMaterial      AS DECIMAL 
    FIELD costSource              AS CHARACTER
    FIELD costStdFreight          AS DECIMAL  
    FIELD costStdWarehouse        AS DECIMAL  
    FIELD costStdDeviation        AS DECIMAL  
    FIELD costStdManufacture      AS DECIMAL 
    FIELD costFull                AS DECIMAL 
    FIELD quantityInvoicedWeight  AS DECIMAL 
    FIELD weightUOM               AS CHARACTER 
    FIELD accountAR               AS CHARACTER
    FIELD accountARFreight        AS CHARACTER
    FIELD accountARSales          AS CHARACTER
    FIELD accountARSalesTax       AS CHARACTER
    FIELD accountARDiscount       AS CHARACTER
    FIELD accountARCash           AS CHARACTER 
    FIELD quantityPerSubUnit      AS DECIMAL
    FIELD amountDiscount          AS DECIMAL
    FIELD amountBilled            AS DECIMAL
    FIELD amountBilledIncDiscount AS DECIMAL
    FIELD amountCommission        AS DECIMAL 
    FIELD locationID              AS CHARACTER
    FIELD bolID                   AS INTEGER
    FIELD squareFeetPerEA         AS DECIMAL
    .    
    
DEFINE TEMP-TABLE ttInvoiceMiscToPost
    FIELD riInvMisc      AS ROWID
    FIELD rNo            AS INTEGER 
    FIELD rNoOld         AS INTEGER
    FIELD company        AS CHARACTER 
    FIELD invoiceID      AS INTEGER
    FIELD isOKToPost     AS LOGICAL
    FIELD problemMessage AS CHARACTER  
    FIELD orderID        AS INTEGER
    FIELD itemID         AS CHARACTER                        
    FIELD itemName       AS CHARACTER
    FIELD accountARSales AS CHARACTER   
    FIELD isBillable     AS LOGICAL 
    FIELD chargeID       AS CHARACTER 
    FIELD isTaxable      AS LOGICAL        
    FIELD amount         AS DECIMAL          
    .
    
DEFINE TEMP-TABLE ttGLTransaction
    FIELD company           AS CHARACTER 
    FIELD transactionType   AS CHARACTER
    FIELD transactionDate   AS DATE 
    FIELD transactionDesc   AS CHARACTER 
    FIELD transactionPeriod AS INTEGER 
    FIELD account           AS CHARACTER
    FIELD amount            AS DECIMAL
    FIELD currencyCode      AS CHARACTER 
    FIELD currencyExRate    AS DECIMAL
    FIELD itemID            AS CHARACTER  
    FIELD quantityWeight    AS DECIMAL
    FIELD invoiceID         AS INTEGER
    FIELD journalNote       AS CHARACTER
    .

DEFINE TEMP-TABLE ttARLedgerTransaction
    FIELD company       AS CHARACTER
    FIELD customerID    AS CHARACTER
    FIELD amount        AS DECIMAL 
    FIELD referenceDesc AS CHARACTER
    FIELD referenceDate AS DATE
    FIELD runID         AS INTEGER 
    FIELD postDate      AS DATE
    .
    
DEFINE TEMP-TABLE ttOrderToUpdate
    FIELD riOeOrd AS ROWID 
    FIELD company AS CHARACTER 
    FIELD orderID AS INTEGER
    .
    
DEFINE TEMP-TABLE ttBolLineToUpdate
    FIELD riOeBoll  AS ROWID
    FIELD riOeBolh  AS ROWID
    FIELD company   AS CHARACTER
    FIELD bolID     AS INTEGER
    FIELD invoiceID AS INTEGER
    .
    
DEFINE TEMP-TABLE ttOrderLineToUpdate
    FIELD riOeOrdl            AS ROWID 
    FIELD company             AS CHARACTER 
    FIELD orderID             AS INTEGER
    FIELD orderLine           AS INTEGER 
    FIELD itemID              AS CHARACTER
    FIELD newQuantityInvoiced AS DECIMAL 
    FIELD newQuantityShipped  AS DECIMAL
    .    

DEFINE TEMP-TABLE ttCustomerToUpdate 
    FIELD riCust                 AS ROWID
    FIELD company                AS CHARACTER 
    FIELD customerID             AS CHARACTER 
    FIELD orderBalanceReduction  AS DECIMAL
    FIELD periodID               AS INTEGER
    FIELD postDate               AS DATE
    FIELD salesTotalExTax        AS DECIMAL
    FIELD salesPTDExTax          AS DECIMAL
    FIELD salesTotal             AS DECIMAL
    FIELD salesPTD               AS DECIMAL 
    FIELD costTotal              AS DECIMAL
    FIELD costPTD                AS DECIMAL 
    FIELD commissionTotal        AS DECIMAL
    FIELD commissionPTD          AS DECIMAL
    FIELD lastPayAmount          AS DECIMAL 
    FIELD lastPayDate            AS DATE
    FIELD accountBalanceIncrease AS DECIMAL 
    FIELD lastInvoiceDate        AS DATE       
    .
    
/*Program-level Handles for persistent procs*/
DEFINE VARIABLE ghNotesProcs AS HANDLE  NO-UNDO.

/*Program-level variables*/
DEFINE VARIABLE giRunID      AS INTEGER NO-UNDO.
DEFINE VARIABLE gdtPostDate  AS DATE    NO-UNDO.
DEFINE VARIABLE giPeriod     AS INTEGER NO-UNDO.



    
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetNextRun RETURNS INTEGER PRIVATE
    (ipcCompany AS CHARACTER) FORWARD.

FUNCTION fGetNextXNo RETURNS INTEGER PRIVATE
    (  ) FORWARD.

FUNCTION fGetTransactionDescription RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER,
    ipcCustomer AS CHARACTER,
    ipiInvoiceID AS INTEGER) FORWARD.

FUNCTION fIsFactored RETURNS LOGICAL PRIVATE
    (iplCustFactored AS LOGICAL,
    ipiInvHeadRNo AS INTEGER) FORWARD.

FUNCTION fIsWritable RETURNS LOGICAL PRIVATE
    (ipriInvHead AS ROWID) FORWARD.


/* ***************************  Main Block  *************************** */

RUN "sys/NotesProcs.p" PERSISTENT SET ghNotesProcs.

/*Create the ttPostingMaster record and fill it with initial values*/
RUN pInitialize(ipcCompany, 
    ipiInvNoStart, ipiInvNoEnd, 
    ipdtInvDateStart, ipdtInvDateEnd, 
    ipcCustomerIDStart, ipcCustomerIDEnd, 
    ipdtPostDate,
    OUTPUT oplError, OUTPUT opcMessage).

IF NOT oplError THEN
    /*Build the master list of invoices based on ttPostingMaster*/
    RUN pBuildInvoicesToPost(OUTPUT oplError, OUTPUT opcMessage).

IF NOT oplError THEN
    /*Process the master list of invoices for reporting and/or posting*/
    RUN pProcessInvoicesToPost(OUTPUT oplError, OUTPUT opcMessage).

IF NOT oplError AND LOOKUP("Export",ipcOptions) GT 0 THEN
    RUN pExportAllTempTables.

IF NOT oplError AND LOOKUP("Post",ipcOptions) GT 0 THEN
    RUN pPostInvoices (ipcCompany).


DELETE OBJECT ghNotesProcs.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pAddARLedgerTransaction PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttInvoiceToPost FOR ttInvoiceToPost.
    
    DEFINE VARIABLE cReferenceDesc AS CHARACTER NO-UNDO.
    
    cReferenceDesc = "INV# " + STRING(ipbf-ttInvoiceToPost.invoiceID).
    FIND ttARLedgerTransaction 
        WHERE ttARLedgerTransaction.company EQ ipbf-ttInvoiceToPost.company
        AND ttARLedgerTransaction.customerID EQ ipbf-ttInvoiceToPost.customerID
        AND ttARLedgerTransaction.referenceDate EQ ipbf-ttInvoiceToPost.invoiceDate
        AND ttARLedgerTransaction.referenceDesc EQ cReferenceDesc
        NO-ERROR.
    IF NOT AVAILABLE ttARLedgerTransaction THEN 
    DO:
        CREATE ttARLedgerTransaction.
        ASSIGN 
            ttARLedgerTransaction.company       = ipbf-ttInvoiceToPost.company
            ttARLedgerTransaction.customerID    = ipbf-ttInvoiceToPost.customerID
            ttARLedgerTransaction.referenceDate = ipbf-ttInvoiceToPost.invoiceDate
            ttARLedgerTransaction.referenceDesc = cReferenceDesc
            
            .
    END.
    ttARLedgerTransaction.amount = ttARLedgerTransaction.amount - ROUND(ipbf-ttInvoiceToPost.amountBilled, 2).
    
END PROCEDURE.


PROCEDURE pAddInvoiceLineToPost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given posting invoice line, invoice header, 
     build the ttInvoiceLineToPost record base (non-calculated fields)
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttPostingMaster FOR ttPostingMaster.
    DEFINE PARAMETER BUFFER ipbf-inv-head        FOR inv-head.
    DEFINE PARAMETER BUFFER ipbf-inv-line        FOR inv-line.
    DEFINE INPUT PARAMETER ipiRNo AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-itemfg FOR itemfg.
    DEFINE BUFFER bf-fgcat  FOR fgcat.
    
    DEFINE VARIABLE lAccountError        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cAccountErrorMessage AS CHARACTER NO-UNDO.
    
    CREATE ttInvoiceLineToPost.
    ASSIGN 
        ttInvoiceLineToPost.riInvLine         = ROWID(ipbf-inv-line)
        ttInvoiceLineToPost.rNo               = ipiRNo
        ttInvoiceLineToPost.rNoOld            = ipbf-inv-line.r-no
        ttInvoiceLineToPost.company           = ipbf-inv-line.company
        ttInvoiceLineToPost.invoiceID         = ipbf-inv-head.inv-no
        ttInvoiceLineToPost.orderID           = ipbf-inv-line.ord-no
        ttInvoiceLineToPost.itemID            = ipbf-inv-line.i-no
        ttInvoiceLineToPost.itemName          = ipbf-inv-line.i-name
        ttInvoiceLineToPost.quantityOrdered   = ipbf-inv-line.qty
        ttInvoiceLineToPost.quantityInvoiced  = ipbf-inv-line.inv-qty
        ttInvoiceLineToPost.quantityShipped   = ipbf-inv-line.ship-qty
        ttInvoiceLineToPost.pricePerUOM       = ipbf-inv-line.price
        ttInvoiceLineToPost.priceUOM          = ipbf-inv-line.pr-uom
        ttInvoiceLineToPost.amountBilled      = ipbf-inv-line.t-price
        ttInvoiceLineToPost.costPerUOM        = ipbf-inv-line.cost
        ttInvoiceLineToPost.isOKToPost        = YES
        ttInvoiceLineToPost.accountAR         = ipbf-ttPostingMaster.accountAR
        ttInvoiceLineToPost.accountARFreight  = ipbf-ttPostingMaster.accountARFreight
        ttInvoiceLineToPost.accountARSales    = ipbf-ttPostingMaster.accountARSales
        ttInvoiceLineToPost.accountARSalesTax = ipbf-ttPostingMaster.accountARSalesTax
        ttInvoiceLineToPost.accountARDiscount = ipbf-ttPostingMaster.accountARDiscount
        ttInvoiceLineToPost.accountARCash     = ipbf-ttPostingMaster.accountARCash
        ttInvoiceLineToPost.bol               = ipbf-inv-head.bol-no
        .

    /*FG Dependent fields*/
    IF ipbf-inv-line.i-no NE "" THEN 
        FIND FIRST bf-itemfg NO-LOCK
            WHERE bf-itemfg.company EQ ipbf-inv-line.company
            AND bf-itemfg.i-no EQ ipbf-inv-line.i-no
            NO-ERROR.
    IF NOT AVAILABLE bf-itemfg THEN 
    DO:
        ASSIGN 
            ttInvoiceLineToPost.isOKToPost     = NO
            ttInvoiceLineToPost.problemMessage = "Invalid FG Item " + ipbf-inv-line.i-no
            oplError                           = YES
            opcMessage                         = ttInvoiceLineToPost.problemMessage
            .
        RETURN.
    END.            
    ASSIGN 
        ttInvoiceLineToPost.quantityInvoicedWeight = ipbf-inv-line.inv-qty * bf-itemfg.weight-100 / 100  /*Refactor - changed from .qty to .inv-qty*/
        ttInvoiceLineToPost.weightUOM              = "LB"
        ttInvoiceLineToPost.quantityPerSubUnit     = MAX(bf-itemfg.case-count, 1)
        ttInvoiceLineToPost.costFull               = bf-itemfg.spare-dec-1
        ttInvoiceLineToPost.squareFeetPerEA        = bf-itemfg.t-sqft
        .
        
    /*Product Category Dependencies*/
    FIND FIRST bf-fgcat NO-LOCK
        WHERE bf-fgcat.company EQ bf-itemfg.company
        AND bf-fgcat.procat  EQ bf-itemfg.procat
        NO-ERROR.
    IF AVAILABLE bf-fgcat THEN 
    DO:
        /*Override default GL account for sales*/
        RUN pCheckAccount(bf-fgcat.company, bf-fgcat.glacc, "FG Category of " + bf-fgcat.procat, "GL Account", OUTPUT lAccountError, OUTPUT cAccountErrorMessage). 
        IF NOT lAccountError THEN
            ttInvoiceLineToPost.accountARSales = bf-fgcat.glacc.
        ELSE 
        DO:
            ASSIGN 
                ttInvoiceLineToPost.isOKToPost     = NO
                ttInvoiceLineToPost.problemMessage = cAccountErrorMessage
                oplError                           = YES
                opcMessage                         = ttInvoiceLineToPost.problemMessage
                .
            RETURN.
        END.
    END. /*AVailb bf-fgcat - FGCat dependencies*/
        
    RUN pGetBOLInfoForInvoiceLine(BUFFER ipbf-inv-line, OUTPUT ttInvoiceLineToPost.bolID, OUTPUT ttInvoiceLineToPost.locationID).
    RUN pGetOrderInfoForInvoiceLine(BUFFER ipbf-inv-line, OUTPUT ttInvoiceLineToPost.quantityPerSubUnit).

    IF ipbf-inv-line.cas-cnt NE 0 THEN 
        ttInvoiceLineToPost.quantityPerSubUnit = ipbf-inv-line.cas-cnt.
                            
    RUN oe/GetCostInvl.p (ROWID(ipbf-inv-line),
        OUTPUT ttInvoiceLineToPost.costDirectLabor, OUTPUT ttInvoiceLineToPost.costFixedOverhead,
        OUTPUT ttInvoiceLineToPost.costVariableOverhead, OUTPUT ttInvoiceLineToPost.costDirectMaterial,
        OUTPUT ttInvoiceLineToPost.costPerUOM, OUTPUT ttInvoiceLineToPost.costUOM, 
        OUTPUT ttInvoiceLineToPost.costTotal, OUTPUT ttInvoiceLineToPost.costSource,
        OUTPUT ttInvoiceLineToPost.costStdFreight, 
        OUTPUT ttInvoiceLineToPost.costStdWarehouse, 
        OUTPUT ttInvoiceLineToPost.costStdDeviation, 
        OUTPUT ttInvoiceLineToPost.costStdManufacture).
    
    IF ttPostingMaster.blockZeroCost AND ipbf-inv-line.inv-qty NE 0 AND ttInvoiceLineToPost.costTotal EQ 0 THEN 
    DO:
        ASSIGN 
            ttInvoiceLineToPost.isOKToPost     = NO
            ttInvoiceLineToPost.problemMessage = "Zero Cost"
            oplError                           = YES
            opcMessage                         = ttInvoiceLineToPost.problemMessage
            .
    END.  /*Check 0 cost*/

    IF ipbf-inv-line.t-price NE 0 THEN 
    DO:
        ASSIGN 
            ttInvoiceLineToPost.amountBilledIncDiscount = ipbf-inv-line.t-price
            ttInvoiceLineToPost.amountBilled            = ipbf-inv-line.t-price
            .
        IF ipbf-inv-line.disc NE 0 THEN 
        DO:
            RUN Conv_CalcTotalPrice(ipbf-inv-line.company, 
                ipbf-inv-line.i-no, 
                ipbf-inv-line.inv-qty,
                ipbf-inv-line.price,
                ipbf-inv-line.pr-uom,
                0,
                ttInvoiceLineToPost.quantityPerSubUnit,
                OUTPUT ttInvoiceLineToPost.amountBilledIncDiscount).
                    
            ASSIGN
                ttInvoiceLineToPost.amountDiscount = ttInvoiceLineToPost.amountBilledIncDiscount - ttInvoiceLineToPost.amountBilled
                .
        END.                
    END.            
        
        
END PROCEDURE.

PROCEDURE pAddInvoiceMiscToPost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given posting invoice line, invoice header, 
     build the ttInvoiceLineToPost record base (non-calculated fields)
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttPostingMaster FOR ttPostingMaster.
    DEFINE PARAMETER BUFFER ipbf-inv-head        FOR inv-head.
    DEFINE PARAMETER BUFFER ipbf-inv-misc        FOR inv-misc.
    DEFINE INPUT PARAMETER ipiRNo AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
       
    DEFINE VARIABLE lAccountError        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cAccountErrorMessage AS CHARACTER NO-UNDO.
    
    CREATE ttInvoiceMiscToPost.
    ASSIGN 
        ttInvoiceMiscToPost.riInvMisc      = ROWID(ipbf-inv-misc)
        ttInvoiceMiscToPost.rNo            = ipiRNo
        ttInvoiceMiscToPost.rNoOld         = ipbf-inv-misc.r-no
        ttInvoiceMiscToPost.company        = ipbf-inv-misc.company
        ttInvoiceMiscToPost.isOKToPost     = YES
        ttInvoiceMiscToPost.invoiceID      = ipbf-inv-head.inv-no
        ttInvoiceMiscToPost.orderID        = ipbf-inv-misc.ord-no
        ttInvoiceMiscToPost.itemID         = ipbf-inv-misc.inv-i-no
        ttInvoiceMiscToPost.chargeID       = ipbf-inv-misc.charge
        ttInvoiceMiscToPost.isTaxable      = ipbf-inv-misc.tax
        ttInvoiceMiscToPost.isBillable     = ipbf-inv-misc.bill EQ "Y"
        ttInvoiceMiscToPost.accountARSales = ipbf-ttPostingMaster.accountARSales
        ttInvoiceMiscToPost.amount         = ipbf-inv-misc.amt
        .
    IF ipbf-inv-misc.actnum NE "" THEN 
    DO:
        RUN pCheckAccount(ipbf-inv-misc.company, ipbf-inv-misc.actnum, "Misc Invoice Line", "Misc GL Account", OUTPUT lAccountError, OUTPUT cAccountErrorMessage). 
        IF NOT lAccountError THEN 
            ttInvoiceMiscToPost.accountARSales =  ipbf-inv-misc.actnum.
        ELSE 
        DO:
            ASSIGN 
                ttInvoiceMiscToPost.isOKToPost     = NO
                ttInvoiceMiscToPost.problemMessage = cAccountErrorMessage
                oplError                           = YES
                opcMessage                         = ttInvoiceMiscToPost.problemMessage
                .
            RETURN.
        END.
    END.
        
            
END PROCEDURE.

PROCEDURE pAddInvoiceToPost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given posting master, invoice header and customer, 
     build the ttInvoiceToPost record base (non-calculated fields)
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttPostingMaster FOR ttPostingMaster.
    DEFINE PARAMETER BUFFER ipbf-inv-head        FOR inv-head.
    DEFINE PARAMETER BUFFER ipbf-cust            FOR cust.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttInvoiceToPost FOR ttInvoiceToPost.
    
    CREATE opbf-ttInvoiceToPost.
    ASSIGN 
        opbf-ttInvoiceToPost.riInvHead                    = ROWID(ipbf-inv-head)
        opbf-ttInvoiceToPost.riCust                       = ROWID(ipbf-cust)
        opbf-ttInvoiceToPost.rNo                          = ipbf-inv-head.r-no
        opbf-ttInvoiceToPost.company                      = ipbf-inv-head.company        
        opbf-ttInvoiceToPost.invoiceID                    = ipbf-inv-head.inv-no
        opbf-ttInvoiceToPost.customerID                   = ipbf-inv-head.cust-no
        opbf-ttInvoiceToPost.isOKToPost                   = YES
        opbf-ttInvoiceToPost.invoiceDate                  = ipbf-inv-head.inv-date                
        opbf-ttInvoiceToPost.postDate                     = ipbf-ttPostingMaster.postDate  
        opbf-ttInvoiceToPost.postDatePeriod               = ipbf-ttPostingMaster.periodID 
        opbf-ttInvoiceToPost.isFactored                   = fIsFactored(ipbf-cust.factored, ipbf-inv-head.r-no)
        opbf-ttInvoiceToPost.amountBilled                 = ipbf-inv-head.t-inv-rev
        opbf-ttInvoiceToPost.amountBilledTax              = ipbf-inv-head.t-inv-tax
        opbf-ttInvoiceToPost.amountBilledExTax            = opbf-ttInvoiceToPost.amountBilled - opbf-ttInvoiceToPost.amountBilledTax
        opbf-ttInvoiceToPost.amountCommission             = ipbf-inv-head.t-comm
        opbf-ttInvoiceToPost.amountCost                   = ipbf-inv-head.t-inv-cost
        opbf-ttInvoiceToPost.isFreightBillable            = ipbf-inv-head.f-bill
        opbf-ttInvoiceToPost.isCashTerms                  = ipbf-inv-head.terms EQ "CASH"
        opbf-ttInvoiceToPost.accountAR                    = ipbf-ttPostingMaster.accountAR
        opbf-ttInvoiceToPost.accountARFreight             = ipbf-ttPostingMaster.accountARFreight
        opbf-ttInvoiceToPost.accountARSales               = ipbf-ttPostingMaster.accountARSales
        opbf-ttInvoiceToPost.accountARSalesTax            = ipbf-ttPostingMaster.accountARSalesTax
        opbf-ttInvoiceToPost.accountArDiscount            = ipbf-ttPostingMaster.accountARDiscount
        opbf-ttInvoiceToPost.accountARCash                = ipbf-ttPostingMaster.accountARCash
        opbf-ttInvoiceToPost.isInvoiceDateInCurrentPeriod = (opbf-ttInvoiceToPost.invoiceDate GE ipbf-ttPostingMaster.periodDateStart 
                                                        AND opbf-ttInvoiceToPost.invoiceDate LE ipbf-ttPostingMaster.periodDateEnd)       
        .
    IF opbf-ttInvoiceToPost.isFreightBillable THEN 
        opbf-ttInvoiceToPost.amountBilledFreight = ipbf-inv-head.t-inv-freight.
        
    RUN pGetCurrencyCodeAndRate(BUFFER ipbf-inv-head, BUFFER ipbf-cust, OUTPUT opbf-ttInvoiceToPost.currencyCode, OUTPUT opbf-ttInvoiceToPost.currencyExRate, OUTPUT opbf-ttInvoiceToPost.currencyARAccount,
        OUTPUT oplError, OUTPUT opcMessage).
        
    IF oplError THEN 
        ASSIGN 
            opbf-ttInvoiceToPost.isOKToPost = NO
            opbf-ttInvoiceToPost.problemMessage = opcMessage
            .
    
END PROCEDURE.

PROCEDURE pAlignMultiInvoiceLinesWithMaster PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a multi-invoice, align the child invoice lines with the master invoice
     Notes:  includes the former "create-save-line" procedure that used to be a disaster 
     with reftable read/write
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttPostingMaster FOR ttPostingMaster.
    DEFINE PARAMETER BUFFER ipbf-master-inv-head FOR inv-head.
    DEFINE PARAMETER BUFFER ipbf-ttInvoiceToPost FOR ttInvoiceToPost.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-child-inv-head FOR inv-head.
    DEFINE BUFFER bf-child-inv-line FOR inv-line.
    DEFINE BUFFER bf-child-inv-misc FOR inv-misc.
    
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    ASSIGN 
        oplError = YES
        opcMessage = "Invalid Master Invoice"
        .
    
    headblock:
    FOR EACH bf-child-inv-head NO-LOCK
        WHERE bf-child-inv-head.company  EQ ipbf-master-inv-head.company
        AND bf-child-inv-head.cust-no       EQ ipbf-master-inv-head.cust-no
        AND bf-child-inv-head.inv-no        EQ ipbf-master-inv-head.inv-no
        AND bf-child-inv-head.multi-invoice EQ NO:
            
        FOR EACH bf-child-inv-line EXCLUSIVE-LOCK 
            WHERE bf-child-inv-line.r-no EQ bf-child-inv-head.r-no:
            
            /*Clear procedure error if at least one found*/
            ASSIGN 
                oplError = NO
                opcMessage = ""
                .   
                
            RUN pAddInvoiceLineToPost(BUFFER ipbf-ttPostingMaster, BUFFER bf-child-inv-head, BUFFER bf-child-inv-line, ipbf-master-inv-head.r-no, OUTPUT lError, OUTPUT cMessage).
            IF lError THEN 
            DO:  /*Flag invoice as bad but continue*/ 
                ASSIGN
                    ipbf-ttInvoiceToPost.isOKToPost     = NO
                    ipbf-ttInvoiceToPost.problemMessage = cMessage
                    .
                NEXT headblock. 
            END.
            
            
        END.

        FOR EACH bf-child-inv-misc EXCLUSIVE-LOCK 
            WHERE bf-child-inv-misc.r-no EQ bf-child-inv-head.r-no:
            
            /*Clear procedure error if at least one found*/
            ASSIGN 
                oplError   = NO
                opcMessage = ""
                .
            
            RUN pAddInvoiceMiscToPost(BUFFER ipbf-ttPostingMaster, BUFFER bf-child-inv-head, BUFFER bf-child-inv-misc, ipbf-master-inv-head.r-no, OUTPUT lError, OUTPUT cMessage).
            
            IF lError THEN 
            DO: 
                ASSIGN
                    ipbf-ttInvoiceToPost.isOKToPost     = NO
                    ipbf-ttInvoiceToPost.problemMessage = cMessage
                    .
                NEXT headblock. 
            END.
            
               
        END.
    END.
    
END PROCEDURE.

PROCEDURE pBuildInvoicesToPost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given Criteria Range, build invoices to post
     Notes:  Will process multi-invoices and "link-up" the inv-lines to one master
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
        
    DEFINE BUFFER bf-inv-head                   FOR inv-head.
    DEFINE BUFFER bf-inv-line                   FOR inv-line.
    DEFINE BUFFER bf-inv-misc                   FOR inv-misc.
    DEFINE BUFFER bf-cust                       FOR cust.
    DEFINE BUFFER bf-MultiInvoiceChild-inv-head FOR inv-head.
    DEFINE BUFFER bf-ttInvoiceToPost            FOR ttInvoiceToPost.
    DEFINE BUFFER bf-ttInvoiceLineToPost        FOR ttInvoiceLineToPost.
    DEFINE BUFFER bf-ttInvoiceMiscToPost        FOR ttInvoiceMiscToPost.
    
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

    FIND FIRST ttPostingMaster NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttPostingMaster THEN 
    DO: 
        ASSIGN 
            oplError   = YES
            opcMessage = "Posting Master Not available"
            .
        RETURN.
    END.    
    ASSIGN 
        oplError = YES
        opcMessage = "No Invoices Available for Posting"
        .
    FOR EACH bf-inv-head NO-LOCK
        WHERE bf-inv-head.company  EQ ttPostingMaster.company
        AND bf-inv-head.printed  EQ YES
        AND bf-inv-head.inv-no   GT 0
        AND bf-inv-head.inv-no   GE ttPostingMaster.invoiceStart
        AND bf-inv-head.inv-no   LE ttPostingMaster.invoiceEnd
        AND bf-inv-head.inv-date GE ttPostingMaster.invoiceDateStart
        AND bf-inv-head.inv-date LE ttPostingMaster.invoiceDateEnd
        AND bf-inv-head.cust-no  GE ttPostingMaster.customerIDStart
        AND bf-inv-head.cust-no  LE ttPostingMaster.customerIDEnd    
        AND (CAN-FIND(FIRST bf-inv-line WHERE bf-inv-line.r-no EQ bf-inv-head.r-no)
        OR CAN-FIND(FIRST bf-inv-misc WHERE bf-inv-misc.r-no = bf-inv-head.r-no )
        OR bf-inv-head.multi-invoice)
        AND bf-inv-head.stat     NE "H"
        USE-INDEX prnt,
        FIRST bf-cust NO-LOCK
        WHERE bf-cust.company EQ bf-inv-head.company
        AND bf-cust.cust-no EQ bf-inv-head.cust-no
        AND ((bf-cust.inv-meth EQ ? AND bf-inv-head.multi-invoice) OR (bf-cust.inv-meth NE ? AND NOT bf-inv-head.multi-invoice))  /*Filter multi-invoices correctly based on customer*/
        :
        /*Add CustomerList Exclusions*/
        /*TBD*/
        
        IF fIsWritable(ROWID(bf-inv-head)) THEN 
        DO:
            ASSIGN 
                oplError = NO
                opcMessage = ""
                .
            RUN pAddInvoiceToPost(BUFFER ttPostingMaster, BUFFER bf-inv-head, BUFFER bf-cust, OUTPUT lError, OUTPUT cMessage, BUFFER bf-ttInvoiceToPost).
            IF lError THEN NEXT.
                
            FOR EACH bf-inv-line NO-LOCK
                WHERE bf-inv-line.r-no EQ bf-inv-head.r-no
                USE-INDEX r-no:
                RUN pAddInvoiceLineToPost(BUFFER ttPostingMaster, BUFFER bf-inv-head, BUFFER bf-inv-line, bf-inv-head.r-no, OUTPUT lError, OUTPUT cMessage). 
            END.
            IF lError THEN DO: 
                ASSIGN
                    bf-ttInvoiceToPost.isOKToPost = NO
                    bf-ttInvoiceToPost.problemMessage = cMessage
                    .
                NEXT. 
            END.
            FOR EACH bf-inv-misc NO-LOCK
                WHERE bf-inv-misc.r-no EQ bf-inv-head.r-no
                USE-INDEX r-no:
                RUN pAddInvoiceMiscToPost(BUFFER ttPostingMaster, BUFFER bf-inv-head, BUFFER bf-inv-misc, bf-inv-head.r-no, OUTPUT lError, OUTPUT cMessage). 
            END.
            IF lError THEN 
            DO: 
                ASSIGN
                    bf-ttInvoiceToPost.isOKToPost     = NO
                    bf-ttInvoiceToPost.problemMessage = cMessage
                    .
                NEXT. 
            END.
            
            /*Manage Multi Invoices*/
            IF bf-inv-head.multi-invoice THEN 
            DO:             
                RUN pAlignMultiInvoiceLinesWithMaster(BUFFER ttPostingMaster, BUFFER bf-inv-head, BUFFER bf-ttInvoiceToPost, OUTPUT lError, OUTPUT cMessage).
                IF lError THEN DO:
                    ASSIGN
                        bf-ttInvoiceToPost.isOKToPost     = NO
                        bf-ttInvoiceToPost.problemMessage = cMessage
                        .
                    NEXT.
                END.
            END. /*Multi-invoice header*/
            
            FIND FIRST bf-ttInvoiceLineToPost NO-LOCK
                WHERE bf-ttInvoiceLineToPost.rNo EQ bf-ttInvoiceToPost.rNo
                AND NOT bf-ttInvoiceLineToPost.isOKToPost
                NO-ERROR.
            IF AVAILABLE bf-ttInvoiceLineToPost THEN DO:
                ASSIGN 
                    bf-ttInvoiceToPost.isOKToPost = NO
                    bf-ttInvoiceToPost.problemMessage = bf-ttInvoiceLineToPost.problemMessage
                    .
                NEXT.                
            END.
            FIND FIRST bf-ttInvoiceMiscToPost NO-LOCK
                WHERE bf-ttInvoiceMiscToPost.rNo EQ bf-ttInvoiceToPost.rNo
                AND NOT bf-ttInvoiceMiscToPost.isOKToPost
                NO-ERROR.
            IF AVAILABLE bf-ttInvoiceMiscToPost THEN 
            DO:
                ASSIGN 
                    bf-ttInvoiceToPost.isOKToPost     = NO
                    bf-ttInvoiceToPost.problemMessage = bf-ttInvoiceMiscToPost.problemMessage
                    .
                NEXT.                
            END.
            
            FIND FIRST ttCustomerToUpdate NO-LOCK
                WHERE ttCustomerToUpdate.riCust EQ ROWID(bf-cust)
                NO-ERROR.
            IF NOT AVAILABLE ttCustomerToUpdate THEN 
            DO:
                CREATE ttCustomerToUpdate.
                ASSIGN 
                    ttCustomerToUpdate.riCust      = ROWID(bf-cust)
                    ttCustomerToUpdate.company     = bf-cust.company
                    ttCustomerToUpdate.customerID  = bf-cust.cust-no
                    ttCustomerToUpdate.periodID    = ttPostingMaster.periodID
                    .  
            END.
        END. /*inv-head is writable*/
    END.  /*Each Inv-head that meets range criteria*/
    
END PROCEDURE.

PROCEDURE pCalcArInvTotals PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a rowid , recalculate ar-inv totals based on lines
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriArInv AS ROWID NO-UNDO.

    DEFINE BUFFER bf-ar-inv  FOR ar-inv.
    DEFINE BUFFER bf-ar-invl FOR ar-invl.
    
    FIND bf-ar-inv EXCLUSIVE-LOCK 
        WHERE ROWID(bf-ar-inv) EQ ipriArInv
        NO-ERROR.
    IF AVAILABLE bf-ar-inv THEN 
    DO:
        bf-ar-inv.t-cost = 0.
        FOR EACH bf-ar-invl NO-LOCK
            WHERE bf-ar-invl.x-no EQ bf-ar-inv.x-no:
            bf-ar-inv.t-cost = bf-ar-inv.t-cost + bf-ar-invl.t-cost.
        END.
    END.

END PROCEDURE.

PROCEDURE pCheckAccount PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a company and account, output error if not valid
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAccount AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAccountSource AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAccountDesc AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    IF ipcAccount EQ "" THEN 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = ipcAccountSource + " has a blank " + ipcAccountDesc
            .
        RETURN.  
    END.
    ELSE 
    DO: 
        FIND FIRST account NO-LOCK
            WHERE account.company EQ ipcCompany
            AND account.actnum  EQ ipcAccount
            NO-ERROR.
        IF NOT AVAILABLE account THEN 
        DO:
            ASSIGN 
                oplError   = YES
                opcMessage = ipcAccountSource + " has an invalid " + ipcAccountDesc + " of " + ipcAccount
                . 
            RETURN.
        END.
    END. /*account not-blank*/
    

END PROCEDURE.

PROCEDURE pCopyNotesFromInvHeadToArInv PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Copies Notes from inv-head to ar-inv
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-inv-head FOR inv-head.
    DEFINE INPUT PARAMETER ipcARInvRecKey AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-child-inv-head FOR inv-head.
    
    RUN CopyNotes IN ghNotesProcs (ipbf-inv-head.rec_key, ipcARInvRecKey, "", "").
    /*copy notes for Group By Date (multi-invoice)*/
    IF ipbf-inv-head.multi-invoice THEN 
    DO:
        
        FOR EACH bf-child-inv-head 
            WHERE bf-child-inv-head.company EQ ipbf-inv-head.company
            AND bf-child-inv-head.inv-no EQ ipbf-inv-head.inv-no
            AND bf-child-inv-head.cust-no EQ ipbf-inv-head.cust-no
            AND NOT bf-child-inv-head.multi-invoice 
            NO-LOCK:
            RUN CopyNotes IN ghNotesProcs (bf-child-inv-head.rec_key, ipcARInvRecKey, "", "").
        END.
    END.

END PROCEDURE.

PROCEDURE pCreateARInvHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  given an inv-head buffer, create ar-inv and return writeable buffer  
     Notes:  Replaces invhpost.i
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-inv-head        FOR inv-head.
    DEFINE PARAMETER BUFFER ipbf-ttInvoiceToPost FOR ttInvoiceToPost.
    DEFINE OUTPUT PARAMETER opriArInv AS ROWID.
    
    DEFINE BUFFER bf-ar-inv FOR ar-inv.
      
    DEFINE VARIABLE iNextXNo    AS INTEGER NO-UNDO.
    
    /*used for Terms procedures*/
    DEFINE VARIABLE iDueOnMonth AS INTEGER NO-UNDO.
    DEFINE VARIABLE iDueOnDay   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNetDays    AS INTEGER NO-UNDO.
    DEFINE VARIABLE lError      AS LOGICAL NO-UNDO.
    
    iNextXNO = fGetNextXNo().
    
    CREATE bf-ar-inv.

    ASSIGN
        bf-ar-inv.x-no         = iNextXNO
        bf-ar-inv.company      = ipbf-inv-head.company
        bf-ar-inv.ord-no       = ipbf-ttInvoiceToPost.orderID
        bf-ar-inv.ord-date     = ipbf-ttInvoiceToPost.orderDate
        bf-ar-inv.inv-no       = ipbf-inv-head.inv-no
        bf-ar-inv.inv-date     = ipbf-inv-head.inv-date

        bf-ar-inv.prod-date    = ipbf-ttInvoiceToPost.postDate /* using prod-date as posted date #53205, pass in tran-date or dtPostDate */
        bf-ar-inv.period       = ipbf-ttInvoiceToPost.postDatePeriod
        
        bf-ar-inv.posted       = YES 
        bf-ar-inv.printed      = YES
        
        bf-ar-inv.cust-no      = ipbf-inv-head.cust-no
        bf-ar-inv.cust-name    = ipbf-inv-head.cust-name
        bf-ar-inv.ship-id      = ipbf-inv-head.sold-no /* RLL */
        bf-ar-inv.addr[1]      = ipbf-inv-head.addr[1]
        bf-ar-inv.addr[2]      = ipbf-inv-head.addr[2]
        bf-ar-inv.state        = ipbf-inv-head.state
        bf-ar-inv.zip          = ipbf-inv-head.zip
        bf-ar-inv.city         = ipbf-inv-head.city
        bf-ar-inv.bill-to      = ipbf-inv-head.bill-to
        bf-ar-inv.sold-id      = ipbf-inv-head.sold-no
        bf-ar-inv.sold-name    = ipbf-inv-head.sold-name
        bf-ar-inv.sold-addr[1] = ipbf-inv-head.sold-addr[1]
        bf-ar-inv.sold-addr[2] = ipbf-inv-head.sold-addr[2]
        bf-ar-inv.sold-city    = ipbf-inv-head.sold-city
        bf-ar-inv.sold-state   = ipbf-inv-head.sold-state
        bf-ar-inv.sold-zip     = ipbf-inv-head.sold-zip
        bf-ar-inv.contact      = ipbf-inv-head.contact
        bf-ar-inv.terms        = ipbf-inv-head.terms
        bf-ar-inv.frt-pay      = ipbf-inv-head.frt-pay
        bf-ar-inv.fob-code     = ipbf-inv-head.fob-code
        bf-ar-inv.carrier      = ipbf-inv-head.carrier
        bf-ar-inv.terms-d      = ipbf-inv-head.terms-d
        bf-ar-inv.bill-i[1]    = ipbf-inv-head.bill-i[1]
        bf-ar-inv.bill-i[2]    = ipbf-inv-head.bill-i[2]
        bf-ar-inv.bill-i[3]    = ipbf-inv-head.bill-i[3]
        bf-ar-inv.bill-i[4]    = ipbf-inv-head.bill-i[4]
        bf-ar-inv.ship-i[1]    = ipbf-inv-head.ship-i[1]
        bf-ar-inv.ship-i[2]    = ipbf-inv-head.ship-i[2]
        bf-ar-inv.ship-i[3]    = ipbf-inv-head.ship-i[3]
        bf-ar-inv.ship-i[4]    = ipbf-inv-head.ship-i[4]
        bf-ar-inv.f-bill       = ipbf-inv-head.f-bill
        bf-ar-inv.STAT         = ipbf-inv-head.STAT
        bf-ar-inv.TAX-code     = ipbf-inv-head.TAX-GR
        bf-ar-inv.t-comm       = ipbf-inv-head.t-comm
        bf-ar-inv.t-weight     = ipbf-inv-head.t-inv-weight   /* total weight shipped */
        bf-ar-inv.freight      = ipbf-inv-head.t-inv-freight  /* total freight Invoiced */
        bf-ar-inv.tax-amt      = ipbf-inv-head.t-inv-tax      /* total tax Invoiced */
        bf-ar-inv.t-cost       = ipbf-inv-head.t-inv-cost     /* total cost invoiced */
        bf-ar-inv.due          = IF ipbf-inv-head.terms EQ "CASH" THEN 0 ELSE ipbf-inv-head.t-inv-rev
        
        /* total invoiced amount */
        bf-ar-inv.gross        = ipbf-inv-head.t-inv-rev /*+ v-inv-disc   total invoiced + disc */ 
        bf-ar-inv.disc-taken   = 0
        bf-ar-inv.paid         = 0
        
        /* total invoiced - freight - misc - tax */
        bf-ar-inv.t-sales      = ipbf-inv-head.t-inv-rev - ipbf-inv-head.t-inv-tax
        bf-ar-inv.net          = bf-ar-inv.t-sales
        .
    
    IF ipbf-inv-head.f-bill THEN /*Exclude Freight billed from total true sales*/ 
        ASSIGN 
            bf-ar-inv.t-sales = bf-ar-inv.t-sales - ipbf-inv-head.t-inv-freight
            .
    
    RUN Credit_GetTerms(ipbf-inv-head.company,ipbf-inv-head.terms, 
        OUTPUT iDueOnMonth, OUTPUT iDueOnDay, OUTPUT iNetDays, 
        OUTPUT bf-ar-inv.disc-%, OUTPUT bf-ar-inv.disc-days,  
        OUTPUT lError) .
    IF NOT lError THEN         
        bf-ar-inv.due-date  =  DYNAMIC-FUNCTION("GetInvDueDate", DATE(bf-ar-inv.inv-date), ipbf-inv-head.company, ipbf-inv-head.terms).  /*From CreditProcs*/
        
    
    RUN pGetCurrencyCodeAndRate(bf-ar-inv.company, bf-ar-inv.cust-no, OUTPUT bf-ar-inv.curr-code[1], OUTPUT bf-ar-inv.ex-rate).
    

END PROCEDURE.


PROCEDURE pCreateARInvLine PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  given inv-line buffers and line numbers, create ar-invl and return writeable buffer  
     Notes:  Replaces oe/invlpost.i
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-inv-head            FOR inv-head.
    DEFINE PARAMETER BUFFER ipbf-inv-line            FOR inv-line.
    DEFINE PARAMETER BUFFER ipbf-ttInvoiceLineToPost FOR ttInvoiceLineToPost.
    DEFINE INPUT PARAMETER ipiXNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiLine AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opriArInvl AS ROWID.
    
    DEFINE BUFFER bf-ar-invl FOR ar-invl.
    
    CREATE bf-ar-invl.
    ASSIGN 
        bf-ar-invl.x-no               = ipiXNo
        bf-ar-invl.line               = ipiLine
        bf-ar-invl.actnum             = ipbf-ttInvoiceLineToPost.accountARSales
        bf-ar-invl.inv-no             = ipbf-inv-head.inv-no
        bf-ar-invl.bol-no             = ipbf-ttInvoiceLineToPost.bolID
        bf-ar-invl.b-no               = ipbf-inv-line.b-no
        bf-ar-invl.company            = ipbf-inv-line.company
        bf-ar-invl.ord-no             = ipbf-inv-line.ord-no
        bf-ar-invl.cust-no            = ipbf-inv-line.cust-no
        bf-ar-invl.est-no             = ipbf-inv-line.est-no
        bf-ar-invl.est-type           = ipbf-inv-line.est-type
        bf-ar-invl.form-no            = ipbf-inv-line.form-no
        bf-ar-invl.blank-no           = ipbf-inv-line.blank-no
        bf-ar-invl.job-no             = ipbf-inv-line.job-no
        bf-ar-invl.job-no2            = ipbf-inv-line.job-no2
        bf-ar-invl.part-no            = ipbf-inv-line.part-no
        bf-ar-invl.i-no               = ipbf-inv-line.i-no
        bf-ar-invl.i-name             = ipbf-inv-line.i-name
        bf-ar-invl.i-dscr             = ipbf-inv-line.i-dscr
        bf-ar-invl.po-no              = ipbf-inv-line.po-no
        bf-ar-invl.req-code           = ipbf-inv-line.req-code
        bf-ar-invl.req-date           = ipbf-inv-line.req-date
        bf-ar-invl.prom-code          = ipbf-inv-line.prom-code
        bf-ar-invl.prom-date          = ipbf-inv-line.prom-date
        bf-ar-invl.part-dscr1         = ipbf-inv-line.part-dscr1
        bf-ar-invl.part-dscr2         = ipbf-inv-line.part-dscr2
        bf-ar-invl.po-no-po           = ipbf-inv-line.po-no-po
        bf-ar-invl.cas-cnt            = ipbf-inv-line.cas-cnt
        bf-ar-invl.pr-uom             = ipbf-inv-line.pr-uom
        bf-ar-invl.unit-pr            = ipbf-inv-line.price
        bf-ar-invl.tax                = ipbf-inv-line.tax
        bf-ar-invl.disc               = ipbf-inv-line.disc
        bf-ar-invl.amt                = ipbf-inv-line.t-price   /* total price of invoiced item */
        bf-ar-invl.t-weight           = ipbf-inv-line.t-weight  /* total weight of invoiced item */
        bf-ar-invl.t-freight          = ipbf-inv-line.t-freight /* total freight of invoiced item */
        bf-ar-invl.ship-qty           = ipbf-inv-line.ship-qty
        bf-ar-invl.inv-qty            = ipbf-inv-line.inv-qty
        bf-ar-invl.qty                = ipbf-inv-line.qty
        bf-ar-invl.spare-dec-1        = ttInvoiceLineToPost.costFull
        bf-ar-invl.sman[1]            = ipbf-inv-line.sman[1]
        bf-ar-invl.sman[2]            = ipbf-inv-line.sman[2]
        bf-ar-invl.sman[3]            = ipbf-inv-line.sman[3]
        bf-ar-invl.s-pct[1]           = ipbf-inv-line.s-pct[1]
        bf-ar-invl.s-pct[2]           = ipbf-inv-line.s-pct[2]
        bf-ar-invl.s-pct[3]           = ipbf-inv-line.s-pct[3]
        bf-ar-invl.s-comm[1]          = ipbf-inv-line.s-comm[1]
        bf-ar-invl.s-comm[2]          = ipbf-inv-line.s-comm[2]
        bf-ar-invl.s-comm[3]          = ipbf-inv-line.s-comm[3]
        bf-ar-invl.sname[1]           = ipbf-inv-line.sname[1]
        bf-ar-invl.sname[2]           = ipbf-inv-line.sname[2]
        bf-ar-invl.sname[3]           = ipbf-inv-line.sname[3]
        bf-ar-invl.s-commbasis[1]     = ipbf-inv-line.s-commbasis[1]
        bf-ar-invl.s-commbasis[2]     = ipbf-inv-line.s-commbasis[2]
        bf-ar-invl.s-commbasis[3]     = ipbf-inv-line.s-commbasis[3]
        bf-ar-invl.misc               = NO 
        bf-ar-invl.posted             = YES 
        bf-ar-invl.pr-qty-uom         = ipbf-inv-line.pr-uom
        bf-ar-invl.cost               = ipbf-inv-line.cost
        bf-ar-invl.t-cost             = bf-ar-invl.cost * (bf-ar-invl.inv-qty / 1000)
        bf-ar-invl.dscr[1]            = "M"
        bf-ar-invl.std-tot-cost       = ipbf-inv-line.cost
        bf-ar-invl.std-lab-cost       = ttInvoiceLineToPost.costDirectLabor
        bf-ar-invl.std-fix-cost       = ttInvoiceLineToPost.costFixedOverhead
        bf-ar-invl.std-var-cost       = ttInvoiceLineToPost.costVariableOverhead
        bf-ar-invl.std-mat-cost       = ttInvoiceLineToPost.costDirectMaterial
        bf-ar-invl.loc                = ttInvoiceLineToPost.locationID
        bf-ar-invl.lot-no             = ipbf-inv-line.lot-no
        bf-ar-invl.e-num              = ipbf-inv-line.e-num
        bf-ar-invl.inv-date           = ipbf-inv-head.inv-date
        bf-ar-invl.costStdFreight     = ipbf-inv-line.costStdFreight
        bf-ar-invl.costStdWarehouse   = ipbf-inv-line.costStdWarehouse
        bf-ar-invl.costStdDeviation   = ipbf-inv-line.costStdDeviation
        bf-ar-invl.costStdManufacture = ipbf-inv-line.costStdManufacture
        bf-ar-invl.sf-sht             = ttInvoiceLineToPost.squareFeetPerEA
        bf-ar-invl.amt-msf            = (bf-ar-invl.inv-qty * bf-ar-invl.sf-sht) / 1000
        .

    IF bf-ar-invl.ord-no EQ 0 THEN 
        bf-ar-invl.s-pct[1] = 100. 
        
    opriArInvl = ROWID(bf-ar-invl).

END PROCEDURE.

PROCEDURE pCreateARInvMisc PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  given inv-misc buffers and line numbers, create ar-invl and return writeable buffer  
     Notes:  Replaces oe/invmpost.i
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-inv-head            FOR inv-head.
    DEFINE PARAMETER BUFFER ipbf-inv-misc            FOR inv-misc.
    DEFINE PARAMETER BUFFER ipbf-ttInvoiceMiscToPost FOR ttInvoiceMiscToPost.
    DEFINE INPUT PARAMETER ipiXNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiLine AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opriArInvl AS ROWID.
    
    DEFINE BUFFER bf-ar-invl FOR ar-invl.
    
    CREATE bf-ar-invl.
    ASSIGN
        bf-ar-invl.x-no           = ipiXNo
        bf-ar-invl.line           = ipiLine
        bf-ar-invl.company        = ipbf-inv-misc.company
        bf-ar-invl.INV-NO         = ipbf-inv-head.inv-no
        bf-ar-invl.ord-no         = ipbf-inv-misc.ord-no
        bf-ar-invl.cust-no        = ipbf-inv-head.cust-no
        bf-ar-invl.est-no         = ipbf-inv-misc.est-no
        bf-ar-invl.tax            = ipbf-inv-misc.tax
        bf-ar-invl.actnum         = ipbf-inv-misc.actnum
        bf-ar-invl.prep-amt       = ipbf-inv-misc.amt
        bf-ar-invl.qty            = 1
        bf-ar-invl.unit-pr        = ipbf-inv-misc.amt
        bf-ar-invl.amt            = ipbf-inv-misc.amt
        bf-ar-invl.t-cost         = ipbf-inv-misc.cost
        bf-ar-invl.cost           = bf-ar-invl.t-cost / 1000
        bf-ar-invl.dscr[1]        = "M"
        bf-ar-invl.prep-charge    = ipbf-inv-misc.charge
        bf-ar-invl.prep-cost      = ipbf-inv-misc.cost
        bf-ar-invl.prep-dscr      = ipbf-inv-misc.dscr
        bf-ar-invl.i-name         = ipbf-inv-misc.charge
        bf-ar-invl.i-dscr         = ipbf-inv-misc.dscr
        bf-ar-invl.po-no          = ipbf-inv-misc.po-no
        bf-ar-invl.po-no-po       = ipbf-inv-misc.po-no-po
        bf-ar-invl.sman[1]        = ipbf-inv-misc.s-man[1]
        bf-ar-invl.sman[2]        = ipbf-inv-misc.s-man[2]
        bf-ar-invl.sman[3]        = ipbf-inv-misc.s-man[3]
        bf-ar-invl.s-pct[1]       = ipbf-inv-misc.s-pct[1]
        bf-ar-invl.s-pct[2]       = ipbf-inv-misc.s-pct[2]
        bf-ar-invl.s-pct[3]       = ipbf-inv-misc.s-pct[3]
        bf-ar-invl.s-comm[1]      = ipbf-inv-misc.s-comm[1]
        bf-ar-invl.s-comm[2]      = ipbf-inv-misc.s-comm[2]
        bf-ar-invl.s-comm[3]      = ipbf-inv-misc.s-comm[3]
        bf-ar-invl.s-commbasis[1] = ipbf-inv-misc.s-commbasis[1]
        bf-ar-invl.s-commbasis[2] = ipbf-inv-misc.s-commbasis[2]
        bf-ar-invl.s-commbasis[3] = ipbf-inv-misc.s-commbasis[3]
        bf-ar-invl.inv-i-no       = ipbf-inv-misc.inv-i-no
        bf-ar-invl.inv-line       = ipbf-inv-misc.inv-line
        bf-ar-invl.misc           = YES
        bf-ar-invl.billable       = ipbf-inv-misc.bill EQ "Y"
        bf-ar-invl.spare-char-1   = ipbf-inv-misc.spare-char-1
        bf-ar-invl.posted         = YES
        bf-ar-invl.inv-date       = ipbf-inv-head.inv-date
        bf-ar-invl.e-num          = ipbf-inv-misc.spare-int-4.

    IF NOT bf-ar-invl.billable THEN bf-ar-invl.amt = 0.
    
        
    opriArInvl = ROWID(bf-ar-invl).

END PROCEDURE.

PROCEDURE pCreateGLTrans PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given temp-table buffer, create GL transaction
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttGLTransaction FOR ttGLTransaction.
    DEFINE INPUT PARAMETER ipdTransactionAmount AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiRun AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opriGLTrans AS ROWID NO-UNDO.
     
    DEFINE BUFFER bf-gltrans FOR gltrans.
     
    CREATE bf-gltrans.
    ASSIGN
        opriGLTrans        = ROWID(bf-gltrans)
        bf-gltrans.company = ipbf-ttGLTransaction.company
        bf-gltrans.actnum  = ipbf-ttGLTransaction.account
        bf-gltrans.jrnl    = ipbf-ttGLTransaction.journalNote
        bf-gltrans.tr-dscr = ipbf-ttGLTransaction.transactionDesc
        bf-gltrans.tr-date = ipbf-ttGLTransaction.transactionDate
        bf-gltrans.tr-amt  = ipdTransactionAmount
        bf-gltrans.period  = ipbf-ttGLTransaction.transactionPeriod
        bf-gltrans.trnum   = ipiRun
        .
    
    RELEASE bf-gltrans.

END PROCEDURE.

PROCEDURE pExportAllTempTables PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Exports all TempTables to file
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE hdOutput    AS HANDLE.
    DEFINE VARIABLE cTempFolder AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
    
    RUN system\OutputProcs.p PERSISTENT SET hdOutput.
    THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hdOutput).
    
    RUN FileSys_GetTempDirectory (OUTPUT cTempFolder).
    
    RUN Output_TempTableToCSV(TEMP-TABLE ttInvoiceToPost:HANDLE, cTempFolder + "\InvoiceHeaders.csv", YES).
    RUN Output_TempTableToCSV(TEMP-TABLE ttInvoiceLineToPost:HANDLE, cTempFolder + "\InvoiceLines.csv", YES).
    RUN Output_TempTableToCSV(TEMP-TABLE ttInvoiceMiscToPost:HANDLE, cTempFolder + "\InvoiceMiscs.csv", YES).
    RUN Output_TempTableToCSV(TEMP-TABLE ttGLTransaction:HANDLE, cTempFolder + "\GLTransactions.csv", YES).
    RUN Output_TempTableToCSV(TEMP-TABLE ttCustomerToUpdate:HANDLE, cTempFolder + "\Customers.csv", YES).
    RUN Output_TempTableToCSV(TEMP-TABLE ttOrderToUpdate:HANDLE, cTempFolder + "\OrderHeaders.csv", YES).
    RUN Output_TempTableToCSV(TEMP-TABLE ttOrderLineToUpdate:HANDLE, cTempFolder + "\OrderLines.csv", YES).
    RUN Output_TempTableToCSV(TEMP-TABLE ttBolLineToUpdate:HANDLE, cTempFolder + "\BOLLines.csv", YES).
    
    
    DELETE OBJECT hdOutput.

END PROCEDURE.

PROCEDURE pGetAccountDefaults PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a company, return the default AR accounts
     Notes: Replaces oe/getacct.p
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAccountAR AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAccountARFreight AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAccountARSales AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAccountARSalesTax AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAccountARDiscount AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAccountARCash AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cAccountSource AS CHARACTER NO-UNDO.
    
    cAccountSource = "A/R Control File".
    FIND FIRST ar-ctrl NO-LOCK  
        WHERE ar-ctrl.company EQ ipcCompany
        NO-ERROR.

    IF NOT AVAILABLE ar-ctrl THEN 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "A/R Control File does not exist for company " + ipcCompany
            . 
    END.
    ELSE 
    DO:
        ASSIGN 
            opcAccountAR         = ar-ctrl.receivables
            opcAccountARFreight  = ar-ctrl.freight
            opcAccountARSales    = ar-ctrl.sales
            opcAccountARSalesTax = ar-ctrl.stax
            opcAccountARDiscount = ar-ctrl.discount
            opcAccountARCash     = ar-ctrl.cash-act
            .

        RUN pCheckAccount(ipcCompany, opcAccountAR, cAccountSource, "Receivables Account", OUTPUT oplError, OUTPUT opcMessage).
        IF NOT oplError THEN 
            RUN pCheckAccount(ipcCompany, opcAccountARFreight, cAccountSource, "Freight Account", OUTPUT oplError, OUTPUT opcMessage).
        IF NOT oplError THEN 
            RUN pCheckAccount(ipcCompany, opcAccountARSales, cAccountSource, "Sales Account", OUTPUT oplError, OUTPUT opcMessage).
        IF NOT oplError THEN 
            RUN pCheckAccount(ipcCompany, opcAccountARSalesTax, cAccountSource, "Sales Tax Account", OUTPUT oplError, OUTPUT opcMessage).            
        IF NOT oplError THEN 
            RUN pCheckAccount(ipcCompany, opcAccountARDiscount, cAccountSource, "Discount Account", OUTPUT oplError, OUTPUT opcMessage).
        IF NOT oplError THEN 
            RUN pCheckAccount(ipcCompany, opcAccountARCash, cAccountSource, "Cash Account", OUTPUT oplError, OUTPUT opcMessage).     
                   
    END.  /*valid ar-ctrl*/
    
END PROCEDURE.

PROCEDURE pGetBOLInfoForInvoiceLine PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  given an invoice line buffer, create a BOL Line to update and
     return the BOL information for the invoice line
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-inv-line FOR inv-line.
    DEFINE OUTPUT PARAMETER opiBOLID AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcLocationID AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-oe-boll FOR oe-boll.
    DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
    
    /*BOL Dependent Fields*/
    FOR EACH bf-oe-boll NO-LOCK 
        WHERE bf-oe-boll.company EQ ipbf-inv-line.company
        AND bf-oe-boll.b-no EQ ipbf-inv-line.b-no
        AND bf-oe-boll.i-no EQ ipbf-inv-line.i-no
        AND bf-oe-boll.ord-no EQ ipbf-inv-line.ord-no
        AND bf-oe-boll.line EQ ipbf-inv-line.line
        AND bf-oe-boll.po-no EQ ipbf-inv-line.po-no,
        FIRST bf-oe-bolh NO-LOCK  
        WHERE bf-oe-bolh.b-no EQ bf-oe-bolh.b-no
        BREAK BY bf-oe-bolh.bol-no: 
                
        FIND FIRST ttBolLineToUpdate
            WHERE ttBolLineToUpdate.riOeBoll EQ ROWID(bf-oe-boll)
            NO-ERROR.
        IF NOT AVAILABLE ttBolLineToUpdate THEN 
        DO:
            CREATE ttBolLineToUpdate.
            ASSIGN 
                ttBolLineToUpdate.riOeBoll  = ROWID(bf-oe-boll)
                ttBollineToUpdate.invoiceID = ipbf-inv-line.inv-no
                ttBolLineToUpdate.riOeBolh  = ROWID(bf-oe-bolh)
                .
        END. /*create new bol line to update*/
        IF FIRST-OF(bf-oe-bolh.bol-no) THEN 
            ASSIGN
                opiBOLID      = bf-oe-bolh.bol-no
                opcLocationID = bf-oe-boll.loc
                .
    END.  /*each bf-oe-boll - BOL dependencies*/

END PROCEDURE.

PROCEDURE pGetCurrencyCodeAndRate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given customer , get applicable currency code and rate
     Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-inv-head FOR inv-head.
    DEFINE PARAMETER BUFFER ipbf-cust     FOR cust.
    DEFINE OUTPUT PARAMETER opcCurrencyCode AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCurrencyExchangeRate AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCurrencyARAccount AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-company  FOR company.
    DEFINE BUFFER bf-currency FOR currency.
    
    opcCurrencyCode = ipbf-inv-head.curr-code[1].
    IF opcCurrencyCode EQ "" AND AVAILABLE ipbf-cust THEN 
        ASSIGN 
            opcCurrencyCode = ipbf-cust.curr-code
            .
    IF opcCurrencyCode EQ "" THEN 
    DO:
        FIND FIRST bf-company NO-LOCK    
            WHERE bf-company.company EQ ipbf-inv-head.company 
            NO-ERROR.
        IF AVAILABLE bf-company THEN 
            opcCurrencyCode = bf-company.curr-code.
    END.            
    IF opcCurrencyCode NE "" THEN 
        FIND bf-currency NO-LOCK 
            WHERE bf-currency.company EQ ipbf-inv-head.company
            AND bf-currency.c-code EQ opcCurrencyCode
//            AND bf-currency.ar-ast-acct NE ""
            AND bf-currency.ex-rate GT 0 
            NO-ERROR.
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "No valid currency code for invoice, customer or company (" + ipbf-inv-head.company + ")"
            .
        
    IF AVAILABLE bf-currency THEN 
        ASSIGN 
            opdCurrencyExchangeRate = bf-currency.ex-rate 
            opcCurrencyARAccount    = bf-currency.ar-ast-acct
            .
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid currency code " + opcCurrencyCode
            .    

END PROCEDURE.

PROCEDURE pGetOrderInfoForInvoiceLine PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: GIven an invoice line, register the order/line to update
     and return key order specific data to invoice line 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-inv-line FOR inv-line.
    DEFINE OUTPUT PARAMETER iopdQuantityPerSubUnit AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-oe-ord  FOR oe-ord.
    
    /*Order Dependencies*/
    IF ipbf-inv-line.ord-no NE 0 THEN 
        FIND FIRST bf-oe-ordl NO-LOCK
            WHERE bf-oe-ordl.company EQ ipbf-inv-line.company
            AND bf-oe-ordl.ord-no  EQ ipbf-inv-line.ord-no
            AND bf-oe-ordl.line    EQ ipbf-inv-line.line
            AND bf-oe-ordl.i-no    EQ ipbf-inv-line.i-no
            USE-INDEX ord-no 
            NO-ERROR.
    IF ipbf-inv-line.ord-no NE 0 AND AVAILABLE bf-oe-ordl THEN 
    DO:
        FIND FIRST bf-oe-ord NO-LOCK
            WHERE bf-oe-ord.company EQ bf-oe-ordl.company
            AND bf-oe-ord.ord-no  EQ bf-oe-ordl.ord-no
            NO-ERROR.
        IF bf-oe-ordl.cas-cnt NE 0 THEN 
            iopdQuantityPerSubUnit = bf-oe-ordl.cas-cnt.
             
        FIND FIRST ttOrderLineToUpdate EXCLUSIVE-LOCK
            WHERE ttOrderLineToUpdate.riOeOrdl EQ ROWID(bf-oe-ordl)
            NO-ERROR.
        IF NOT AVAILABLE ttOrderLineToUpdate THEN 
        DO:
            CREATE ttOrderLineToUpdate.
            ASSIGN 
                ttOrderLineToUpdate.riOeOrdl  = ROWID(bf-oe-ordl)
                ttOrderLineToUpdate.company   = bf-oe-ordl.company
                ttOrderLineToUpdate.orderID   = bf-oe-ordl.ord-no
                ttOrderLineToUpdate.itemID    = bf-oe-ordl.i-no
                ttOrderLineToUpdate.orderLine = bf-oe-ordl.line
                .
        END.
        ASSIGN 
            ttOrderLineToUpdate.newQuantityInvoiced = ttOrderLineToUpdate.newQuantityInvoiced + ipbf-inv-line.inv-qty
            ttOrderLineToUpdate.newQuantityShipped  = ttOrderLineToUpdate.newQuantityShipped + ipbf-inv-line.ship-qty /*Refactor - remove?*/
            .
    END. /*available bf-oe-ordl - Order Line dependencies*/
            
    IF AVAILABLE bf-oe-ord THEN 
    DO:
        FIND FIRST ttOrderToUpdate NO-LOCK
            WHERE ttOrderToUpdate.riOeOrd EQ ROWID(bf-oe-ord)
            NO-ERROR.
        IF NOT AVAILABLE ttOrderToUpdate THEN 
        DO: 
            CREATE ttOrderToUpdate.
            ASSIGN 
                ttOrderToUpdate.riOeOrd = ROWID(bf-oe-ord)
                ttOrderToUpdate.company = bf-oe-ord.company
                ttOrderToUpdate.orderID = bf-oe-ord.ord-no
                .
        END.
    END. /*Avail bf-oe-ord*/

END PROCEDURE.

PROCEDURE pGetSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Gets all required NK1 settings for posting run
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplBlockZeroCost AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (ipcCompany, "INVPOST", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN oplBlockZeroCost = cReturn EQ "YES".
    

END PROCEDURE.

PROCEDURE pInitialize PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Initializes the post
        Empties all temp-tables
        Builds the postingMaster control record
        Fills it with settings and global values for the posting process
     
     Notes:
         RUN pInitialize(ipcCompany, 
                        ipiInvNoStart, ipiInvNoEnd, 
                        ipdtInvDateStart, ipdtInvDateEnd, 
                        ipcCustomerIDStart, ipcCustomerIDEnd, 
                        ipdtPostDate,
                        OUTPUT oplError, OUTPUT opcMessage).
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiInvNoStart AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiInvNoEnd AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtInvDateStart AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipdtInvDateEnd AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerIDStart AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerIDEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtPostDate AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-period FOR period.
    
    EMPTY TEMP-TABLE ttPostingMaster.
    EMPTY TEMP-TABLE ttInvoiceToPost.
    EMPTY TEMP-TABLE ttInvoiceLineToPost.
    
    CREATE ttPostingMaster.
    ASSIGN 
        ttPostingMaster.company          = ipcCompany
        ttPostingMaster.invoiceStart     = ipiInvNoStart
        ttPostingMaster.invoiceEnd       = ipiInvNoEnd
        ttPostingMaster.invoiceDateStart = ipdtInvDateStart
        ttPostingMaster.invoiceDateEnd   = ipdtInvDateEnd
        ttPostingMaster.customerIDStart  = ipcCustomerIDStart
        ttPostingMaster.customerIDEnd    = ipcCustomerIDEnd
        ttPostingMaster.postDate         = ipdtPostDate 
        .
    
    RUN pGetSettings(ipcCompany, OUTPUT ttPostingMaster.blockZeroCost).
    
    FIND FIRST bf-period NO-LOCK 
        WHERE bf-period.company EQ ipcCompany
        AND bf-period.pst LE ipdtPostDate
        AND bf-period.pend GE ipdtPostDate
        NO-ERROR.
    IF AVAILABLE bf-period THEN 
        ASSIGN 
            ttPostingMaster.periodID        = bf-period.pnum
            ttPostingMaster.periodDateStart = bf-period.pst
            ttPostingMaster.periodDateEnd   = bf-period.pend
            .                    
    ELSE 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "No valid period for " + STRING(ipdtPostDate) + " for company " + ipcCompany
            .
        RETURN.
    END.
    RUN pGetAccountDefaults(ipcCompany, 
        OUTPUT ttPostingMaster.accountAR, 
        OUTPUT ttPostingMaster.accountARFreight, 
        OUTPUT ttPostingMaster.accountARSales, 
        OUTPUT ttPostingMaster.accountARSalesTax, 
        OUTPUT ttPostingMaster.accountARDiscount,
        OUTPUT ttPostingMaster.accountARCash,
        OUTPUT oplError, OUTPUT opcMessage).
        

END PROCEDURE.

PROCEDURE pPostGL PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Posts all pending GL Transactions
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttPostingMaster FOR ttPostingMaster.
    
    DEFINE VARIABLE riGLTrans    AS ROWID   NO-UNDO.
    DEFINE VARIABLE iRunID       AS INTEGER NO-UNDO.
    DEFINE VARIABLE cAccountDesc LIKE gltrans.tr-dscr NO-UNDO. 
   
    iRunID = fGetNextRun(ipcCompany).    
    FOR EACH ttGLTransaction
        WHERE ttGLTransaction.transactionType EQ 'work-line'
        BREAK BY ttGLTransaction.account
        BY ttGLTransaction.invoiceID:
        
        ACCUMULATE ttGlTransaction.amount (TOTAL BY ttGLTransaction.invoiceID).
        
        IF LAST-OF(ttGlTransaction.invoiceID) THEN 
            RUN pCreateGLTrans(BUFFER ttGLTransaction, 
                (ACCUMULATE TOTAL BY ttGLTransaction.invoiceID ttGlTransaction.amount),
                iRunID,
                OUTPUT riGLTrans).
                           
    END. /* each work-line ttGLTransaction */
    
    FOR EACH ttGLTransaction
        WHERE ttGLTransaction.transactionType EQ 'work-misc'
        BREAK BY ttGLTransaction.account
        BY ttGLTransaction.invoiceID:
        
        ACCUMULATE ttGlTransaction.amount (TOTAL BY ttGLTransaction.invoiceID).
        
        IF LAST-OF(ttGlTransaction.invoiceID) THEN 
            RUN pCreateGLTrans(BUFFER ttGLTransaction, 
                (ACCUMULATE TOTAL BY ttGLTransaction.invoiceID ttGlTransaction.amount),
                iRunID,
                OUTPUT riGLTrans).
                           
    END. /* each work-misc ttGLTransaction */
    
    FOR EACH ttGLTransaction
        WHERE ttGLTransaction.transactionType EQ 'work-tax'
        BREAK BY ttGLTransaction.account
        BY ttGLTransaction.invoiceID:
        
        ACCUMULATE ttGlTransaction.amount (TOTAL BY ttGLTransaction.invoiceID).
        
        IF LAST-OF(ttGlTransaction.invoiceID) THEN 
            RUN pCreateGLTrans(BUFFER ttGLTransaction, 
                (ACCUMULATE TOTAL BY ttGLTransaction.invoiceID ttGlTransaction.amount),
                iRunID,
                OUTPUT riGLTrans).
                           
    END. /* each work-tax ttGLTransaction */

    FOR EACH ttGLTransaction
        WHERE ttGLTransaction.transactionType EQ 'work-freight'
        BREAK BY ttGLTransaction.account
        BY ttGLTransaction.invoiceID:
        
        ACCUMULATE ttGlTransaction.amount (TOTAL BY ttGLTransaction.invoiceID).
        
        IF LAST-OF(ttGlTransaction.invoiceID) THEN 
            RUN pCreateGLTrans(BUFFER ttGLTransaction, 
                (ACCUMULATE TOTAL BY ttGLTransaction.invoiceID ttGlTransaction.amount),
                iRunID,
                OUTPUT riGLTrans).
                           
    END. /* each work-freight ttGLTransaction */
    
    FOR EACH ttGLTransaction
        WHERE ttGLTransaction.transactionType EQ 'work-disc'
        BREAK BY ttGLTransaction.account
        BY ttGLTransaction.invoiceID:
        
        ACCUMULATE ttGlTransaction.amount (TOTAL BY ttGLTransaction.invoiceID).
        
        IF LAST-OF(ttGlTransaction.invoiceID) THEN 
            RUN pCreateGLTrans(BUFFER ttGLTransaction, 
                (ACCUMULATE TOTAL BY ttGLTransaction.invoiceID ttGlTransaction.amount),
                iRunID,
                OUTPUT riGLTrans).
                           
    END. /* each work-disc ttGLTransaction */
    FOR EACH ttGLTransaction
        WHERE ttGLTransaction.transactionType EQ 'work-cash'
        BREAK BY ttGLTransaction.account
        BY ttGLTransaction.invoiceID:
        
        ACCUMULATE ttGlTransaction.amount (TOTAL BY ttGLTransaction.invoiceID).
        
        IF LAST-OF(ttGlTransaction.invoiceID) THEN 
            RUN pCreateGLTrans(BUFFER ttGLTransaction, 
                (ACCUMULATE TOTAL BY ttGLTransaction.invoiceID ttGlTransaction.amount),
                iRunID,
                OUTPUT riGLTrans).
                           
    END. /* each work-cash ttGLTransaction */
    FOR EACH ttGLTransaction
        WHERE ttGLTransaction.transactionType EQ 'work-ar'
        BREAK BY ttGLTransaction.account
        BY ttGLTransaction.invoiceID:
        
        ACCUMULATE ttGlTransaction.amount (TOTAL BY ttGLTransaction.invoiceID).
        
        IF LAST-OF(ttGlTransaction.invoiceID) THEN 
            RUN pCreateGLTrans(BUFFER ttGLTransaction, 
                (ACCUMULATE TOTAL BY ttGLTransaction.invoiceID ttGlTransaction.amount),
                iRunID,
                OUTPUT riGLTrans).
                           
    END. /* each work-ar ttGLTransaction */
/*                                                                                                  */
/*    /** POST CURRENCY TO G/L TRANS **/                                                            */
/*    FOR EACH tt-report NO-LOCK                                                                    */
/*        WHERE tt-report.term-id EQ ""                                                             */
/*        AND tt-report.key-01  EQ "work-curr"                                                      */
/*                                                                                                  */
/*        BREAK BY tt-report.key-02:                                                                */
/*                                                                                                  */
/*        ACCUMULATE dec(tt-report.key-05) (TOTAL BY tt-report.key-02).                             */
/*                                                                                                  */
/*        IF LAST-OF(tt-report.key-02) THEN                                                         */
/*        DO:                                                                                       */
/*            CREATE tt-gl.                                                                         */
/*            CREATE gltrans.                                                                       */
/*            ASSIGN                                                                                */
/*                tt-gl.row-id    = ROWID(gltrans)                                                  */
/*                gltrans.company = cocode                                                          */
/*                gltrans.actnum  = tt-report.key-02                                                */
/*                gltrans.jrnl    = "OEINV"                                                         */
/*                gltrans.tr-dscr = "ORDER ENTRY INVOICE CURRENCY GAIN/LOSS"                        */
/*                gltrans.tr-date = dtPostDate                                                      */
/*                gltrans.tr-amt  = - (ACCUMULATE TOTAL BY tt-report.key-02 dec(tt-report.key-05))  */
/*                gltrans.period  = tran-period                                                     */
/*                gltrans.trnum   = v-trnum                                                         */
/*                .                                                                                 */
/*                                                                                                  */
/*            RELEASE gltrans.                                                                      */
/*        END. /* last actnum */                                                                    */
/*    END. /* each work-curr */                                                                      */
/*                                                                                                  */
/*    FOR EACH tmp-work-job                                                                         */
/*        BREAK BY tmp-work-job.fg                                                                  */
/*        BY tmp-work-job.actnum                                                                    */
/*        BY tmp-work-job.inv-no:                                                                   */
/*                                                                                                  */
/*        ACCUMULATE tmp-work-job.amt (TOTAL BY tmp-work-job.inv-no).                               */
/*                                                                                                  */
/*        IF LAST-OF(tmp-work-job.inv-no) THEN                                                      */
/*        DO:                                                                                       */
/*            RUN get-tr-dscr (tmp-work-job.inv-no, OUTPUT cAccountDesc).                           */
/*                                                                                                  */
/*            CREATE tt-gl.                                                                         */
/*            CREATE gltrans.                                                                       */
/*            ASSIGN                                                                                */
/*                tt-gl.row-id    = ROWID(gltrans)                                                  */
/*                gltrans.company = cocode                                                          */
/*                gltrans.actnum  = tmp-work-job.actnum                                             */
/*                gltrans.jrnl    = "OEINV"                                                         */
/*                gltrans.tr-date = dtPostDate                                                      */
/*                gltrans.period  = tran-period                                                     */
/*                gltrans.trnum   = v-trnum                                                         */
/*                .                                                                                 */
/*                                                                                                  */
/*            IF tmp-work-job.fg THEN                                                               */
/*                ASSIGN                                                                            */
/*                    gltrans.tr-amt  = - (ACCUMULATE TOTAL BY tmp-work-job.inv-no tmp-work-job.amt)*/
/*                    gltrans.tr-dscr = TRIM(cAccountDesc) + " FG".                                 */
/*            ELSE                                                                                  */
/*                ASSIGN                                                                            */
/*                    gltrans.tr-amt  = (ACCUMULATE TOTAL BY tmp-work-job.inv-no tmp-work-job.amt)  */
/*                    gltrans.tr-dscr = TRIM(cAccountDesc) + " COGS".                               */
/*                                                                                                  */
/*            RELEASE gltrans.                                                                      */
/*        END.                                                                                      */
/*    END. /* each work-job */                                                                      */
/*                                                                                                  */
        

END PROCEDURE.

PROCEDURE pPostInvoices PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes all ttInvoiceToPost for session.
        Transactions for updating the DB
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER.
    
    DEFINE BUFFER bf-inv-head       FOR inv-head.
    DEFINE BUFFER bf-inv-line       FOR inv-line.
    DEFINE BUFFER bf-inv-misc       FOR inv-misc.
    DEFINE BUFFER bf-ar-inv         FOR ar-inv.
    DEFINE BUFFER bf-ar-invl        FOR ar-invl.
    DEFINE BUFFER bf-oe-ordl        FOR oe-ordl.
    DEFINE BUFFER bf-child-inv-head FOR inv-head.
    
    DEFINE VARIABLE iLine    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iXno     AS INTEGER NO-UNDO.
    DEFINE VARIABLE riArInv  AS ROWID   NO-UNDO.
    DEFINE VARIABLE riArInvl AS ROWID   NO-UNDO.
    DEFINE VARIABLE iRun     AS INTEGER NO-UNDO.
          
    /*    DISABLE TRIGGERS FOR LOAD OF inv-head.*/
    /*    DISABLE TRIGGERS FOR LOAD OF inv-line.*/
    /*    DISABLE TRIGGERS FOR LOAD OF oe-ord.  */
    /*    DISABLE TRIGGERS FOR LOAD OF oe-ordl. */
    /*    DISABLE TRIGGERS FOR LOAD OF itemfg.  */
    /*    DISABLE TRIGGERS FOR LOAD OF oe-relh. */
    /*    DISABLE TRIGGERS FOR LOAD OF oe-rell. */
    
    FOR EACH ttInvoiceToPost NO-LOCK
        WHERE ttInvoiceToPost.isOKToPost,
        FIRST bf-inv-head EXCLUSIVE-LOCK 
        WHERE ROWID(bf-inv-head) EQ ttInvoiceToPost.riInvHead 
        :
        /*Create ar-inv based on inv-head and return writeable buffer*/
        RUN pCreateARInvHeader(BUFFER bf-inv-head, BUFFER ttInvoiceToPost, OUTPUT riArInv).  
        FIND FIRST bf-ar-inv NO-LOCK
            WHERE ROWID(bf-ar-inv) EQ riArInv
            NO-ERROR.
        IF AVAILABLE bf-ar-inv THEN 
        DO: 
            ASSIGN 
                iXNo = bf-ar-inv.x-no
                . 
            RUN pCopyNotesFromInvHeadToArInv(BUFFER bf-inv-head, bf-ar-inv.rec_key).
        END.            
        iLine = 1.
        FOR EACH ttInvoiceLineToPost NO-LOCK
            WHERE ttInvoiceLineToPost.rNo EQ ttInvoiceToPost.rNo 
            AND ttInvoiceLineToPost.isOKToPost,
            FIRST bf-inv-line EXCLUSIVE-LOCK 
            WHERE ROWID(bf-inv-line) EQ ttInvoiceLineToPost.riInvLine:
                
            RUN pCreateARInvLIne(BUFFER bf-inv-head, BUFFER bf-inv-line, BUFFER ttInvoiceLineToPost, iXno, iLine, OUTPUT riArInvl).
            iLine = iLine + 1.
            /* Create eddoc for invoice if required */
            /*        RUN ed/asi/o810hook.p (RECID(bf-inv-head), NO, NO).                */
            /*        FIND FIRST edmast NO-LOCK                                          */
            /*            WHERE edmast.cust EQ bf-inv-head.cust-no                       */
            /*            NO-ERROR.                                                      */
            /*        IF AVAILABLE edmast THEN                                           */
            /*        DO:                                                                */
            /*            FIND FIRST edcode NO-LOCK                                      */
            /*                WHERE edcode.partner EQ edmast.partner                     */
            /*                NO-ERROR.                                                  */
            /*            IF NOT AVAILABLE edcode THEN                                   */
            /*                FIND FIRST edcode NO-LOCK                                  */
            /*                    WHERE edcode.partner EQ edmast.partnerGrp              */
            /*                    NO-ERROR.                                              */
            /*        END.                                                               */
            /*                                                                           */
            /*        IF AVAILABLE edcode AND edcode.sendFileOnPrint THEN                */
            /*            RUN ed/asi/write810.p (INPUT cocode, INPUT bf-inv-head.inv-no).*/
            /*                                                                           */
            /*        /* {oe/r-inve&pb.i} */                                             */
            /*                                                                           */
            /*        fDebugMsg("list-post-inv invoice # " + string(bf-inv-head.inv-no)).*/
        
            /*Refactor?*/
            /*            RUN oe/invposty.p (bf-inv-head.inv-no, bf-inv-line.i-no, bf-inv-line.inv-qty,*/
            /*                ttInvoiceLineToPost.costUOM,                                             */
            /*                ttInvoiceLineToPost.costDirectLabor,                                     */
            /*                ttInvoiceLineToPost.costFixedOverhead,                                   */
            /*                ttInvoiceLineToPost.costVariableOverhead,                                */
            /*                ttInvoiceLineToPost.costDirectMaterial).                                 */
            
            /*Refactor*/
            /*            ASSIGN                                      */
            /*                    v-invline = RECID(inv-line)         */
            /*                    v-invhead = RECID(bf-inv-head).     */
            /*            RUN oe/invpost3.p (dtPostDate, tran-period).*/
            
            /*Removed "Sonoco" and other export logic*/
            
            DELETE bf-inv-line.
            DELETE ttInvoiceLineToPost.
            
        END. /*each invoice line*/
        FOR EACH ttInvoiceMiscToPost NO-LOCK
            WHERE ttInvoiceMiscToPost.isOKToPost,
            FIRST bf-inv-misc NO-LOCK
            WHERE ROWID(bf-inv-misc) EQ ttInvoiceMiscToPost.riInvMisc:
         
            RUN pCreateARInvMisc(BUFFER bf-inv-head, BUFFER bf-inv-misc, BUFFER ttInvoiceMiscToPost, iXno, iLine, OUTPUT riArInvl).
            iLine = iLine + 1.
            
            DELETE bf-inv-misc.
            DELETE ttInvoiceMiscToPost. 
        END. /*each invoice misc*/
        
        RUN pCalcArInvTotals(riArInv).
        
        IF bf-inv-head.multi-invoice THEN
            FOR EACH bf-child-inv-head
                WHERE bf-child-inv-head.company       EQ bf-inv-head.company
                AND bf-child-inv-head.cust-no       EQ bf-inv-head.cust-no
                AND bf-child-inv-head.inv-no        EQ bf-inv-head.inv-no
                AND bf-child-inv-head.multi-invoice EQ NO:

                DELETE bf-child-inv-head.
            END.

        DELETE bf-inv-head.
        DELETE ttInvoiceToPost.
    END. /*each invoice to post*/
    
    /*Create GL Records*/
    RUN pPostGL(ipcCompany).       
    
    RUN pUpdateOrders.
    RUN pUpdateBOLs.
    RUN pUpdateCustomers.
    
END PROCEDURE.

PROCEDURE pProcessInvoicesToPost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  For each InvoiceToPost, sum up totals for posting
     and create temp-tables for GL account creation
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
     
    DEFINE BUFFER bf-inv-head FOR inv-head.
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER bf-inv-misc FOR inv-misc.
    DEFINE BUFFER bf-oe-ordl  FOR oe-ordl.
    DEFINE BUFFER bf-oe-ord   FOR oe-ord.
    DEFINE BUFFER bf-oe-bolh  FOR oe-bolh.
    DEFINE BUFFER bf-oe-boll  FOR oe-boll.
    
    DEFINE VARIABLE dDiscountAmount AS DECIMAL.
    DEFINE VARIABLE dInvoiceAmount  AS DECIMAL.

    FIND FIRST ttPostingMaster NO-ERROR.
    IF NOT AVAILABLE ttPostingMaster THEN 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Posting Master not available"
            .
        RETURN.
    END.

    InvoiceBlock:
    FOR EACH ttInvoiceToPost
        WHERE ttInvoiceToPost.isOKToPost,    
        FIRST bf-inv-head NO-LOCK 
        WHERE ROWID(bf-inv-head) EQ ttInvoiceToPost.riInvHead
        ,
        FIRST ttCustomerToUpdate
        WHERE ttCustomerToUpdate.riCust EQ ttInvoiceToPost.riCust
        BY ttInvoiceToPost.invoiceID:
         
        LineBlock:
        FOR EACH ttInvoiceLineToPost
            WHERE ttInvoiceLineToPost.rNo EQ ttInvoiceToPost.rNo
            AND ttInvoiceLineToPost.isOKToPost,
            FIRST bf-inv-line NO-LOCK 
            WHERE ROWID(bf-inv-line) EQ ttInvoiceLineToPost.riInvLine
            BREAK BY ttInvoiceLineToPost.orderID:
            
            ASSIGN 
                ttInvoiceToPost.quantityTotal       = ttInvoiceToPost.quantityTotal + ttInvoiceLineToPost.quantityInvoiced
                ttInvoiceToPost.quantityTotalWeight = ttInvoiceToPost.quantityTotalWeight + ttInvoiceLineToPost.quantityInvoicedWeight
                ttInvoiceToPost.amountDiscount      = ttInvoiceToPost.amountDiscount + ttInvoiceLineToPost.amountDiscount
                .

            IF FIRST(ttInvoiceLineToPost.orderID) THEN
                ASSIGN
                    ttInvoiceToPost.orderID   = bf-inv-line.ord-no
                    ttInvoiceToPost.orderDate = bf-inv-line.ord-date
                    .
            
            IF ttInvoiceLineToPost.amountBilledIncDiscount NE 0 THEN 
            DO:                
                CREATE ttGLTransaction.
                ASSIGN 
                    ttGLTransaction.transactionType   = "work-line"
                    ttGLTransaction.journalNote       = "OEINV"
                    ttGLTransaction.transactionDate   = ttPostingMaster.postDate
                    ttGLTransaction.transactionPeriod = ttPostingMaster.periodID
                    ttGLTransaction.transactionDesc   = fGetTransactionDescription(ttInvoiceToPost.company, ttInvoiceToPost.customerID, ttInvoiceToPost.invoiceID) + " LINE"
                    ttGLTransaction.company           = ttInvoiceLineToPost.company
                    ttGLTransaction.account           = ttInvoiceLineToPost.accountARSales
                    ttGLTransaction.invoiceID         = ttInvoiceLineToPost.invoiceID
                    ttGLTransaction.amount            = - ttInvoiceLineToPost.amountBilledIncDiscount
                    ttGLTransaction.currencyCode      = ttInvoiceToPost.currencyCode
                    ttGLTransaction.currencyExRate    = ttInvoiceToPost.currencyExRate
                    ttGLTransaction.quantityWeight    = ttInvoiceLineToPost.quantityInvoicedWeight
                    ttGLTransaction.itemID            = ttInvoiceLineToPost.itemID
                    .
            END.
        END. /* each inv-line */

        FOR EACH ttInvoiceMiscToPost NO-LOCK
            WHERE ttInvoiceMiscToPost.rNo EQ ttInvoiceToPost.rNo 
            AND ttInvoiceMiscToPost.isOKToPost,
            FIRST bf-inv-misc
            WHERE ROWID(bf-inv-misc) EQ ttInvoiceMiscToPost.riInvMisc
            :
             
            
            IF ttInvoiceMiscToPost.isBillable AND ttInvoiceMiscToPost.amount NE 0 THEN 
            DO:
                CREATE ttGLTransaction.
                ASSIGN 
                    ttGLTransaction.transactionType   = "work-misc"
                    ttGLTransaction.journalNote       = "OEINV"
                    ttGLTransaction.transactionDate   = ttPostingMaster.postDate
                    ttGLTransaction.transactionPeriod = ttPostingMaster.periodID
                    ttGLTransaction.transactionDesc   = fGetTransactionDescription(ttInvoiceToPost.company, ttInvoiceToPost.customerID, ttInvoiceToPost.invoiceID) + " MISC"
                    ttGLTransaction.company           = ttInvoiceToPost.company
                    ttGLTransaction.account           = ttInvoiceMiscToPost.accountARSales
                    ttGLTransaction.invoiceID         = ttInvoiceToPost.invoiceID
                    ttGLTransaction.amount            = - ttInvoiceMiscToPost.amount
                    ttGLTransaction.currencyCode      = ttInvoiceToPost.currencyCode
                    ttGLTransaction.currencyExRate    = ttInvoiceToPost.currencyExRate
                    ttGLTransaction.itemID            = ttInvoiceMiscToPost.chargeID
                    .
                
                ASSIGN 
                    ttInvoiceToPost.amountBilledMisc = ttInvoiceToPost.amountBilledMisc + ttInvoiceMiscToPost.amount
                    .                    
                
            END. /*Billable misc*/
            
            IF bf-inv-misc.ord-no NE 0 THEN 
                FIND FIRST bf-oe-ord NO-LOCK 
                    WHERE bf-oe-ord.company EQ bf-inv-misc.company
                    AND bf-oe-ord.ord-no EQ bf-inv-misc.ord-no
                    NO-ERROR.
            IF AVAILABLE bf-oe-ord THEN 
            DO:
                FIND FIRST ttOrderToUpdate NO-LOCK
                    WHERE ttOrderToUpdate.riOeOrd EQ ROWID(bf-oe-ord)
                    NO-ERROR.
                IF NOT AVAILABLE ttOrderToUpdate THEN 
                DO: 
                    CREATE ttOrderToUpdate.
                    ASSIGN 
                        ttOrderToUpdate.riOeOrd = ROWID(bf-oe-ord)
                        ttOrderToUpdate.company = bf-oe-ord.company
                        ttOrderToUpdate.orderID = bf-oe-ord.ord-no
                        .
                END.
            END.            
                
        /*            IF v-post THEN                                                                               */
        /*            DO:                                                                                          */
        /*                FIND FIRST oe-ordm                                                                       */
        /*                    WHERE oe-ordm.company EQ inv-misc.company                                            */
        /*                    AND oe-ordm.ord-no  EQ inv-misc.ord-no                                               */
        /*                    AND oe-ordm.line    EQ inv-misc.line                                                 */
        /*                    AND oe-ordm.charge  EQ inv-misc.charge                                               */
        /*                    NO-ERROR.                                                                            */
        /*                IF AVAILABLE oe-ordm THEN                                                                */
        /*                DO:                                                                                      */
        /*                    IF oe-ordm.bill EQ "P" THEN oe-ordm.bill = IF inv-misc.bill EQ "Y" THEN "I" ELSE "Y".*/
        /*                                                                                                         */
        /*                                                                                                         */
        /*                                                                                                         */
        /*                    IF oe-ordm.miscType EQ 1 THEN                                                        */
        /*                        FOR EACH est-prep                                                                */
        /*                            WHERE est-prep.company EQ oe-ordm.company                                    */
        /*                            AND est-prep.est-no  EQ oe-ordm.est-no                                       */
        /*                            AND est-prep.eqty    EQ oe-ordm.estPrepEqty                                  */
        /*                            AND est-prep.line    EQ oe-ordm.estPrepLine                                  */
        /*                            AND est-prep.code    EQ oe-ordm.charge                                       */
        /*                            AND est-prep.simon   EQ "S"                                                  */
        /*                            AND est-prep.amtz    EQ 100:                                                 */
        /*                            IF oeprep-log THEN DELETE est-prep.                                          */
        /*                            ELSE est-prep.simon = "N".                                                   */
        /*                        END.                                                                             */
        /*                                                                                                         */
        /*                                                                                                         */
        /*                                                                                                         */
        /*                END.                                                                                     */
        /*                                                                                                         */
        /*                IF inv-misc.bill EQ "Y" THEN                                                             */
        /*                DO:                                                                                      */
        /*                    RUN ar/calctax2.p (bf-inv-head.tax-gr,                                               */
        /*                        NO,                                                                              */
        /*                        inv-misc.amt,                                                                    */
        /*                        inv-misc.company,                                                                */
        /*                        inv-misc.inv-i-no,                                                               */
        /*                        OUTPUT dTax).                                                                    */
        /*                                                                                                         */
        /*                    v-reduce-ord-bal = v-reduce-ord-bal + inv-misc.amt +                                 */
        /*                        (IF inv-misc.tax THEN dTax ELSE 0).                                              */
        /*                END.                                                                                     */
        /*                                                                                                         */
        /*                                                                                                         */
        /*            END. /* v-post */                                                                            */
                    
        END. /* each inv-misc */

        /******************* DISCOUNT ITEMS ****************************************/
        /*        ASSIGN                                         */
        /*            v-post-disc   = v-post-disc   + dInvDisc   */
        /*            v-post-disc-w = v-post-disc-w + dInvDisc-w.*/
        
        

        

        /*        ASSIGN                                                                          */
        /*            v-post-total   = v-post-total   + bf-inv-head.t-inv-rev                     */
        /*            v-post-total-w = v-post-total-w + dLineTot-w.                               */
        /*                                                                                        */
        /*        IF AVAILABLE currency THEN                                                      */
        /*        DO:                                                                             */
        /*            CREATE tt-report.                                                           */
        /*            ASSIGN                                                                      */
        /*                tt-report.term-id = ""                                                  */
        /*                tt-report.key-01  = "work-curr"                                         */
        /*                tt-report.key-02  = currency.ar-ast-acct                                */
        /*                tt-report.key-05  = STRING(((bf-inv-head.t-inv-rev * currency.ex-rate) -*/
        /*                                       bf-inv-head.t-inv-rev) * -1).                    */
        /*        END.                                                                            */
        /*                                                                                        */

        /*                                                                                        */
       
        
        IF ttInvoiceToPost.isInvoiceDateInCurrentPeriod THEN 
            ASSIGN 
                ttCustomerToUpdate.salesPTDExTax = ttCustomerToUpdate.salesPTDExTax + ttInvoiceToPost.amountBilledExTax
                ttCustomerToUpdate.salesPTD      = ttCustomerToUpdate.salesPTD + ttInvoiceToPost.amountBilledExTax
                ttCustomerToUpdate.costPTD       = ttCustomerToUpdate.costPTD + ttInvoiceToPost.amountCost
                ttCustomerToUpdate.commissionPTD = ttCustomerToUpdate.commissionPTD + ttInvoiceToPost.amountCommission
                .
        ASSIGN 
            ttCustomerToUpdate.salesTotalExTax       = ttCustomerToUpdate.salesTotalExTax + ttInvoiceToPost.amountBilledExTax
            ttCustomerToUpdate.salesTotal            = ttCustomerToUpdate.salesTotal + ttInvoiceToPost.amountBilled
            ttCustomerToUpdate.costTotal             = ttCustomerToUpdate.costTotal + ttInvoiceToPost.amountCost 
            ttCustomerToUpdate.commissionTotal       = ttCustomerToUpdate.commissionTotal + ttInvoiceToPost.amountCommission
            ttCustomerToUpdate.orderBalanceReduction = ttCustomerToUpdate.orderBalanceReduction +  ttInvoiceToPost.amountBilled
            .
            
        IF ttInvoiceToPost.isCashTerms THEN 
        DO:
            IF ttInvoiceToPost.invoiceDate GT ttCustomerToUpdate.lastPayDate OR ttCustomerToUpdate.lastPayDate EQ ? THEN 
                ASSIGN 
                    ttCustomerToUpdate.lastPayDate   = ttInvoiceToPost.invoiceDate
                    ttCustomerToUpdate.lastPayAmount = ttInvoiceToPost.amountBilled
                    .
        END.
        ELSE 
            ttCustomerToUpdate.accountBalanceIncrease = ttCustomerToUpdate.accountBalanceIncrease + ttInvoiceToPost.amountBilled.
        

        IF ttInvoiceToPost.invoiceDate GT ttCustomerToUpdate.lastInvoiceDate OR ttCustomerToUpdate.lastInvoiceDate EQ ? THEN 
            ttCustomerToUpdate.lastInvoiceDate = ttInvoiceToPost.invoiceDate.
        
         /*Add discount per invoice*/
        IF ttInvoiceToPost.amountDiscount NE 0 THEN 
        DO:
            CREATE ttGLTransaction.
            ASSIGN 
                ttGLTransaction.transactionType   = "work-disc"
                ttGLTransaction.journalNote       = "OEINV"
                ttGLTransaction.transactionDate   = ttPostingMaster.postDate
                ttGLTransaction.transactionPeriod = ttPostingMaster.periodID
                ttGLTransaction.transactionDesc   = fGetTransactionDescription(ttInvoiceToPost.company, ttInvoiceToPost.customerID, ttInvoiceToPost.invoiceID) + " DISC"
                ttGLTransaction.company           = ttInvoiceToPost.company
                ttGLTransaction.account           = ttInvoiceToPost.accountARDiscount
                ttGLTransaction.invoiceID         = ttInvoiceToPost.invoiceID
                ttGLTransaction.amount            = ttInvoiceToPost.amountDiscount
                ttGLTransaction.currencyCode      = ttInvoiceToPost.currencyCode
                ttGLTransaction.currencyExRate    = ttInvoiceToPost.currencyExRate
                ttGLTransaction.itemID            = ""
                .
        END.
        IF ttInvoiceToPost.amountBilledFreight NE 0 THEN 
        DO:
            CREATE ttGLTransaction.
            ASSIGN 
                ttGLTransaction.transactionType   = "work-freight"
                ttGLTransaction.journalNote       = "OEINV"
                ttGLTransaction.transactionDate   = ttPostingMaster.postDate
                ttGLTransaction.transactionPeriod = ttPostingMaster.periodID
                ttGLTransaction.transactionDesc   = fGetTransactionDescription(ttInvoiceToPost.company, ttInvoiceToPost.customerID, ttInvoiceToPost.invoiceID) + " FRT"
                ttGLTransaction.company           = ttInvoiceToPost.company
                ttGLTransaction.account           = ttInvoiceToPost.accountARFreight
                ttGLTransaction.invoiceID         = ttInvoiceToPost.invoiceID
                ttGLTransaction.amount            = - ttInvoiceToPost.amountBilledFreight
                ttGLTransaction.currencyCode      = ttInvoiceToPost.currencyCode
                ttGLTransaction.currencyExRate    = ttInvoiceToPost.currencyExRate
                ttGLTransaction.itemID            = ""
                .
        END.
        IF ttInvoiceToPost.amountBilledTax NE 0 THEN 
        DO:
            CREATE ttGLTransaction.
            ASSIGN 
                ttGLTransaction.transactionType   = "work-tax"
                ttGLTransaction.journalNote       = "OEINV"
                ttGLTransaction.transactionDate   = ttPostingMaster.postDate
                ttGLTransaction.transactionPeriod = ttPostingMaster.periodID
                ttGLTransaction.transactionDesc   = fGetTransactionDescription(ttInvoiceToPost.company, ttInvoiceToPost.customerID, ttInvoiceToPost.invoiceID) + " TAX"
                ttGLTransaction.company           = ttInvoiceToPost.company
                ttGLTransaction.account           = ttInvoiceToPost.accountARSalesTax
                ttGLTransaction.invoiceID         = ttInvoiceToPost.invoiceID
                ttGLTransaction.amount            = - ttInvoiceToPost.amountBilledTax
                ttGLTransaction.currencyCode      = ttInvoiceToPost.currencyCode
                ttGLTransaction.currencyExRate    = ttInvoiceToPost.currencyExRate
                ttGLTransaction.itemID            = ""
                .
        END.
        
        IF ttInvoiceToPost.isCashTerms AND ttInvoiceToPost.amountBilled NE 0 THEN 
        DO:
            CREATE ttGLTransaction.
            ASSIGN 
                ttGLTransaction.transactionType   = "work-cash"
                ttGLTransaction.journalNote       = "OEINV"
                ttGLTransaction.transactionDate   = ttPostingMaster.postDate
                ttGLTransaction.transactionPeriod = ttPostingMaster.periodID
                ttGLTransaction.transactionDesc   = fGetTransactionDescription(ttInvoiceToPost.company, ttInvoiceToPost.customerID, ttInvoiceToPost.invoiceID) + " CASH"
                ttGLTransaction.company           = ttInvoiceToPost.company
                ttGLTransaction.account           = ttInvoiceToPost.accountARSalesTax
                ttGLTransaction.invoiceID         = ttInvoiceToPost.invoiceID
                ttGLTransaction.amount            = ttInvoiceToPost.amountBilled
                ttGLTransaction.currencyCode      = ttInvoiceToPost.currencyCode
                ttGLTransaction.currencyExRate    = ttInvoiceToPost.currencyExRate
                ttGLTransaction.itemID            = ""
                .
        END.
        ELSE 
        DO:
            CREATE ttGLTransaction.
            ASSIGN 
                ttGLTransaction.transactionType   = "work-ar"
                ttGLTransaction.journalNote       = "OEINV"
                ttGLTransaction.transactionDate   = ttPostingMaster.postDate
                ttGLTransaction.transactionPeriod = ttPostingMaster.periodID
                ttGLTransaction.transactionDesc   = fGetTransactionDescription(ttInvoiceToPost.company, ttInvoiceToPost.customerID, ttInvoiceToPost.invoiceID) + " AR"
                ttGLTransaction.company           = ttInvoiceToPost.company
                ttGLTransaction.account           = ttInvoiceToPost.accountAR
                ttGLTransaction.invoiceID         = ttInvoiceToPost.invoiceID
                ttGLTransaction.amount            = ttInvoiceToPost.amountBilled
                ttGLTransaction.currencyCode      = ttInvoiceToPost.currencyCode
                ttGLTransaction.currencyExRate    = ttInvoiceToPost.currencyExRate
                ttGLTransaction.itemID            = ""
                .
        END.

        RUN pAddARLedgerTransaction (BUFFER ttInvoiceToPost).
            
     
    END. /*Each ttInvoiceToPost*/    
    

END PROCEDURE.

PROCEDURE pUpdateBOLs PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Updates db for BOLs in ttBOLLineToUpdate temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-oe-boll FOR oe-boll.
    DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
    
    FOR EACH ttBOLlineToUpdate NO-LOCK,
        FIRST bf-oe-boll EXCLUSIVE-LOCK
        WHERE ROWID(bf-oe-boll) EQ ttBolLineToUpdate.riOeBoll,
        FIRST bf-oe-bolh EXCLUSIVE-LOCK 
        WHERE ROWID(bf-oe-bolh) EQ ttBolLineToUpdate.riOeBolh:
        ASSIGN 
            bf-oe-boll.inv-no = ttBolLineToUpdate.invoiceID
            bf-oe-bolh.inv-no = ttBolLineToUpdate.invoiceID
            .
    /*Update loadtag.sts to "Completed" - May be unnecessary*/
    END.

END PROCEDURE.

PROCEDURE pUpdateCustomers PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Updates db for customers in ttCustomerToUpdate temp-table
     Notes: 
     
     Replaces oe/invcust.p
    ------------------------------------------------------------------------------*/
    
    DEFINE BUFFER bf-cust   FOR cust.  
    
    FOR EACH ttCustomerToUpdate NO-LOCK,
        FIRST bf-cust EXCLUSIVE-LOCK
        WHERE ROWID(bf-cust) EQ ttCustomerToUpdate.riCust:
        ASSIGN 
            bf-cust.sales[ttCustomerToUpdate.periodID] = bf-cust.sales[ttCustomerToUpdate.periodID] + ttCustomerToUpdate.salesPTDExTax
            bf-cust.cost[1]                            = bf-cust.cost[1] + ttCustomerToUpdate.costPTD       /* PTD */
            bf-cust.comm[1]                            = bf-cust.comm[1] + ttCustomerToUpdate.commissionPTD /* PTD */     
            bf-cust.ord-bal                            = bf-cust.ord-bal - ttCustomerToUpdate.orderBalanceReduction
            bf-cust.ytd-sales                          = bf-cust.ytd-sales + ttCustomerToUpdate.salesTotalExTax
            bf-cust.cost[5]                            = bf-cust.cost[5] + ttCustomerToUpdate.costTotal
            bf-cust.comm[5]                            = bf-cust.comm[5] + ttCustomerToUpdate.commissionTotal
            bf-cust.acc-bal                            = bf-cust.acc-bal + ttCustomerToUpdate.accountBalanceIncrease
            bf-cust.lpay                               = ttCustomerToUpdate.lastPayAmount
            bf-cust.lpay-date                          = ttCustomerToUpdate.lastPayDate
            .

        IF bf-cust.acc-bal GE bf-cust.hibal THEN 
            ASSIGN 
                bf-cust.hibal      = bf-cust.acc-bal
                bf-cust.hibal-date = ttCustomerToUpdate.lastInvoiceDate
                .
    END.
   
END PROCEDURE.

PROCEDURE pUpdateOrders PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Updates DB for order lines and orders given ttOrderLineToUpdate temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    
    FOR EACH ttOrderLineToUpdate NO-LOCK,
        FIRST bf-oe-ordl EXCLUSIVE-LOCK 
        WHERE ROWID(bf-oe-ordl) EQ ttOrderLineToUpdate.riOeOrdl:
        
        ASSIGN 
            bf-oe-ordl.t-inv-qty  = bf-oe-ordl.t-inv-qty + ttOrderLineToUpdate.newQuantityInvoiced
            bf-oe-ordl.t-ship-qty = bf-oe-ordl.t-ship-qty + ttOrderLineToUpdate.newQuantityShipped
            . 
    END.

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fGetNextRun RETURNS INTEGER PRIVATE
    ( ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  Gets the next available run # for a company
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE iRun AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-gl-ctrl FOR gl-ctrl.
    
    DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
        REPEAT:
            FIND FIRST bf-gl-ctrl EXCLUSIVE-LOCK
                WHERE bf-gl-ctrl.company EQ ipcCompany
                NO-ERROR NO-WAIT.
            IF AVAILABLE bf-gl-ctrl THEN 
            DO:
                ASSIGN 
                    iRun             = bf-gl-ctrl.trnum + 1 
                    bf-gl-ctrl.trnum = iRun.
                FIND CURRENT bf-gl-ctrl NO-LOCK.
                LEAVE.
            END. /* IF AVAIL bf-gl-ctrl */
        END. /* REPEAT */
    END.
    RETURN iRun.
		
END FUNCTION.

FUNCTION fGetTransactionDescription RETURNS CHARACTER PRIVATE
    ( ipcCompany AS CHARACTER, ipcCustomer AS CHARACTER, ipiInvoiceID AS INTEGER ):
    /*------------------------------------------------------------------------------
     Purpose: Given customer/invoice, return the standard transaction description
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE cTransactionDesc AS CHARACTER NO-UNDO.
    
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ ipcCompany
        AND cust.cust-no EQ ipcCustomer
        NO-ERROR.
    IF AVAILABLE cust THEN 
        cTransactionDesc = cust.name.
    ELSE 
        cTransactionDesc = "Cust not on file". 
    
    cTransactionDesc =  cTransactionDesc + " Inv# " + STRING(ipiInvoiceID,"99999999").
    
    RETURN cTransactionDesc.
		
END FUNCTION.

FUNCTION fGetNextXNo RETURNS INTEGER PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: returns the next xNO for ar-inv creation
     Notes:
    ------------------------------------------------------------------------------*/	
    
    DEFINE VARIABLE iLastXNo AS INTEGER NO-UNDO.
    
    iLastXNo = 1.
    FIND LAST ar-inv NO-LOCK 
        USE-INDEX x-no  NO-ERROR.
    IF AVAILABLE ar-inv THEN                
        ASSIGN
            iLastXNo = ar-inv.x-no.
    
    RETURN iLastXNo + 1.
		
END FUNCTION.

FUNCTION fIsFactored RETURNS LOGICAL PRIVATE
    (iplCustFactored AS LOGICAL, ipiInvHeadRNo AS INTEGER ):
    /*------------------------------------------------------------------------------
     Purpose:  Given an inv-head.r-no determine if invoice containes factored
        item
     Notes:
    ------------------------------------------------------------------------------*/	
    
    DEFINE VARIABLE lIsFactored AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    
    IF iplCustFactored THEN
        FOR EACH bf-inv-line NO-LOCK 
            WHERE bf-inv-line.r-no EQ ipiInvHeadRNo,
            FIRST bf-itemfg NO-LOCK 
            WHERE bf-itemfg.company  EQ bf-inv-line.company
            AND bf-itemfg.i-no     EQ bf-inv-line.i-no
            AND bf-itemfg.factored = YES:
           
            lIsFactored = YES.
            LEAVE.
        END.        
    
    RETURN lIsFactored.
		
END FUNCTION.

FUNCTION fIsWritable RETURNS LOGICAL PRIVATE
    (ipriInvHead AS ROWID):
    /*------------------------------------------------------------------------------
     Purpose:  Returns YES if the invoice headers is not currently locked
     Notes:  REFACTOR - Is this reliable?
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lWritable AS LOGICAL NO-UNDO.
	
    DEFINE BUFFER bf-inv-head FOR inv-head.
	
    FIND FIRST bf-inv-head EXCLUSIVE-LOCK 
        WHERE ROWID(bf-inv-head) EQ ipriInvHead
        NO-WAIT NO-ERROR.
    lWritable = AVAILABLE bf-inv-head.
	
    RETURN lWritable.
		
END FUNCTION.

