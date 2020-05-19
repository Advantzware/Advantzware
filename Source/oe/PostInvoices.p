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

DEFINE TEMP-TABLE ttPostingMaster NO-UNDO 
    FIELD company            AS CHARACTER
    FIELD blockZeroCost      AS LOGICAL
    FIELD deleteEstPrep      AS LOGICAL
    FIELD invoiceStart       AS INTEGER 
    FIELD invoiceEnd         AS INTEGER 
    FIELD invoiceDateStart   AS DATE 
    FIELD invoiceDateEnd     AS DATE 
    FIELD customerIDStart    AS CHARACTER 
    FIELD customerIDEnd      AS CHARACTER
    FIELD postDate           AS DATE
    FIELD periodID           AS INTEGER 
    FIELD periodDateStart    AS DATE
    FIELD periodDateEnd      AS DATE 
    FIELD accountAR          AS CHARACTER
    FIELD accountARFreight   AS CHARACTER
    FIELD accountARSales     AS CHARACTER
    FIELD accountARSalesTax  AS CHARACTER
    FIELD accountARDiscount  AS CHARACTER
    FIELD accountARCash      AS CHARACTER
    FIELD accountARCurrency  AS CHARACTER
    FIELD accountCOGS        AS CHARACTER
    FIELD accountFG          AS CHARACTER 
    FIELD journalNote        AS CHARACTER
    FIELD consolidateAR      AS LOGICAL
    FIELD consolidateFG      AS LOGICAL
    FIELD consolidateCOGS    AS LOGICAL
    FIELD consolidateLine    AS LOGICAL
    FIELD consolidateMisc    AS LOGICAL 
    FIELD consolidateTax     AS LOGICAL
    FIELD consolidateDisc    AS LOGICAL 
    FIELD consolidateFreight AS LOGICAL  
    FIELD consolidateCash    AS LOGICAL
    FIELD currencyCode       AS CHARACTER 
    FIELD currencyExRate     AS DECIMAL
    FIELD exportPath         AS CHARACTER
    FIELD runID              AS INTEGER
    .
    
DEFINE TEMP-TABLE ttInvoiceToPost NO-UNDO 
    FIELD riInvHead                    AS ROWID
    FIELD isOKToPost                   AS LOGICAL
    FIELD rNo                          AS INTEGER
    FIELD riCust                       AS ROWID
    FIELD company                      AS CHARACTER
    FIELD postDate                     AS DATE  
    FIELD periodID                     AS INTEGER 
    FIELD customerID                   AS CHARACTER 
    FIELD invoiceID                    AS INTEGER 
    FIELD invoiceDate                  AS DATE    
    FIELD isFactored                   AS LOGICAL
    FIELD problemMessage               AS CHARACTER 
    FIELD orderID                      AS INTEGER
    FIELD orderDate                    AS DATE
    FIELD quantityTotal                AS INTEGER
    FIELD quantityTotalWeight          AS DECIMAL
    FIELD quantityTotalMSF             AS DECIMAL
    FIELD currencyCode                 AS CHARACTER 
    FIELD currencyExRate               AS DECIMAL
    FIELD accountARCurrency            AS CHARACTER
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
    FIELD bolID                        AS INTEGER
    .
    
DEFINE TEMP-TABLE ttInvoiceLineToPost NO-UNDO 
    FIELD riInvLine               AS ROWID
    FIELD rNo                     AS INTEGER 
    FIELD rNoOld                  AS INTEGER
    FIELD company                 AS CHARACTER 
    FIELD invoiceID               AS INTEGER
    FIELD customerID              AS CHARACTER
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
    FIELD quantityInvoicedMSF     AS DECIMAL
    FIELD weightUOM               AS CHARACTER 
    FIELD accountAR               AS CHARACTER
    FIELD accountARFreight        AS CHARACTER
    FIELD accountARSales          AS CHARACTER
    FIELD accountARSalesTax       AS CHARACTER
    FIELD accountARDiscount       AS CHARACTER
    FIELD accountARCash           AS CHARACTER
    FIELD accountDLCogs           AS CHARACTER
    FIELD accountDLFG             AS CHARACTER
    FIELD accountVOCogs           AS CHARACTER
    FIELD accountVOFG             AS CHARACTER
    FIELD accountFOCogs           AS CHARACTER
    FIELD accountFOFG             AS CHARACTER
    FIELD accountDMCogs           AS CHARACTER
    FIELD accountDMFG             AS CHARACTER 
    FIELD quantityPerSubUnit      AS DECIMAL
    FIELD amountDiscount          AS DECIMAL
    FIELD amountBilled            AS DECIMAL
    FIELD amountBilledIncDiscount AS DECIMAL
    FIELD amountCommission        AS DECIMAL 
    FIELD locationID              AS CHARACTER
    FIELD bolID                   AS INTEGER
    FIELD squareFeetPerEA         AS DECIMAL
    FIELD productCategory         AS CHARACTER
    FIELD currencyCode            AS CHARACTER 
    FIELD currencyExRate          AS DECIMAL  
    FIELD periodID                AS INTEGER
    .    
    
DEFINE TEMP-TABLE ttInvoiceMiscToPost NO-UNDO 
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
    FIELD costTotal      AS DECIMAL          
    .
    
DEFINE TEMP-TABLE ttGLTransaction NO-UNDO 
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

DEFINE TEMP-TABLE ttARLedgerTransaction NO-UNDO 
    FIELD company        AS CHARACTER
    FIELD customerID     AS CHARACTER
    FIELD amount         AS DECIMAL 
    FIELD referenceDesc  AS CHARACTER
    FIELD referenceDate  AS DATE
    FIELD runID          AS INTEGER
    FIELD accountAR      AS CHARACTER 
    FIELD postDate       AS DATE
    FIELD periodID       AS INTEGER
    FIELD currencyCode   AS CHARACTER 
    FIELD currencyExRate AS DECIMAL 
    .
    
DEFINE TEMP-TABLE ttFGItemToUpdate NO-UNDO
    FIELD riItemfg                 AS ROWID
    FIELD company                  AS CHARACTER 
    FIELD itemID                   AS CHARACTER
    FIELD itemName                 AS CHARACTER
    FIELD quantityInvoicedTotal    AS DECIMAL
    FIELD quantityInvoicedPTD      AS DECIMAL
    FIELD quantityShippedTotal     AS DECIMAL
    FIELD quantityShippedPTD       AS DECIMAL
    FIELD quantityAllocatedTotal   AS DECIMAL
    FIELD quantityAllocatedPTD     AS DECIMAL
    FIELD quantityInvoicedMSFTotal AS DECIMAL
    FIELD quantityInvoicedMSFPTD   AS DECIMAL
    FIELD periodID                 AS INTEGER
    . 
        
DEFINE TEMP-TABLE ttOrderToUpdate NO-UNDO 
    FIELD riOeOrd AS ROWID 
    FIELD company AS CHARACTER 
    FIELD orderID AS INTEGER
    .
    
DEFINE TEMP-TABLE ttBolLineToUpdate NO-UNDO 
    FIELD riOeBoll  AS ROWID
    FIELD riOeBolh  AS ROWID
    FIELD company   AS CHARACTER
    FIELD bolID     AS INTEGER
    FIELD invoiceID AS INTEGER
    FIELD orderID   AS INTEGER
    FIELD orderLine AS INTEGER
    FIELD itemID    AS CHARACTER
    .
    
DEFINE TEMP-TABLE ttOrderLineToUpdate NO-UNDO 
    FIELD riOeOrdl            AS ROWID 
    FIELD company             AS CHARACTER 
    FIELD orderID             AS INTEGER
    FIELD orderLine           AS INTEGER 
    FIELD itemID              AS CHARACTER
    FIELD newQuantityInvoiced AS DECIMAL 
    FIELD newQuantityShipped  AS DECIMAL
    .    

DEFINE TEMP-TABLE ttOrderMiscToUpdate NO-UNDO 
    FIELD riOeOrdm   AS ROWID 
    FIELD company    AS CHARACTER 
    FIELD orderID    AS INTEGER
    FIELD orderLine  AS INTEGER 
    FIELD itemID     AS CHARACTER
    FIELD billStatus AS CHARACTER
    FIELD riEstPrep  AS ROWID 
    .

DEFINE TEMP-TABLE ttCustomerToUpdate NO-UNDO 
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
    FIELD msfPTD                 AS DECIMAL
    FIELD msfTotal               AS DECIMAL
    .
    
DEFINE TEMP-TABLE ttEstPrepToUpdate NO-UNDO
    FIELD riEstPrep     AS ROWID 
    FIELD deleteEstPrep AS LOGICAL
    FIELD newSimon      AS CHARACTER
    .
    
DEFINE TEMP-TABLE ttException NO-UNDO
    FIELD exceptionReason        AS CHARACTER
    FIELD recordRowid            AS ROWID
    FIELD company                AS CHARACTER
    FIELD recordTable            AS CHARACTER
    FIELD recordTableDescription AS CHARACTER
    FIELD recordKeyDescription   AS CHARACTER
    FIELD recordField            AS CHARACTER 
    FIELD recordFieldDescription AS CHARACTER
    FIELD recordFieldValue       AS CHARACTER  
    .
    
{custom/globdefs.i}    
{sys/inc/var.i SHARED}
/*Program-level Handles for persistent procs*/

DEFINE VARIABLE ghNotesProcs AS HANDLE NO-UNDO.
    
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetFilePath RETURNS CHARACTER PRIVATE
    (ipcFolder AS CHARACTER,
     ipcFileBase AS CHARACTER,
     ipcFileUnique AS CHARACTER,
     ipcFileExt AS CHARACTER) FORWARD.

FUNCTION fGetGLTransactionHandle RETURNS HANDLE 
    (  ) FORWARD.

FUNCTION fGetInvoiceLineToPostHandle RETURNS HANDLE 
    (  ) FORWARD.

FUNCTION fGetInvoiceToPostHandle RETURNS HANDLE 
    (  ) FORWARD.

FUNCTION fGetNextRun RETURNS INTEGER PRIVATE
    (ipcCompany AS CHARACTER,
    iplUpdateControl AS LOGICAL) FORWARD.

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
/* Shared Vars needed for 810 invoices */
RUN rc/genrcvar.p.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pAddARLedgerTransaction PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  given an invoice to post, create the ttARLedgerTransaction
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
            ttARLedgerTransaction.company        = ipbf-ttInvoiceToPost.company
            ttARLedgerTransaction.customerID     = ipbf-ttInvoiceToPost.customerID
            ttARLedgerTransaction.referenceDate  = ipbf-ttInvoiceToPost.invoiceDate
            ttARLedgerTransaction.referenceDesc  = cReferenceDesc
            ttARLedgerTransaction.accountAR      = ipbf-ttInvoiceToPost.accountAR
            ttARLedgerTransaction.currencyCode   = ipbf-ttInvoiceToPost.currencyCode
            ttARLedgerTransaction.currencyExRate = ipbf-ttInvoiceToPost.currencyExRate
            ttARLedgerTransaction.periodID       = ipbf-ttInvoiceToPost.periodID
            ttARLedgerTransaction.postDate       = ipbf-ttInvoiceToPost.postDate
            .
    END.
    ttARLedgerTransaction.amount = ttARLedgerTransaction.amount - ipbf-ttInvoiceToPost.amountBilled.
    
END PROCEDURE.


PROCEDURE pAddException PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Adds a ttException based on 
     Notes:  
     RUN pAddException(ttInvoiceToPost.riInvHead, ttInvoiceToPost.problemMessage,
        ttInvoiceToPost.company, "inv-head","Invoice Header", cKey,
        cField, cFieldDesc , cFieldValue).           
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriRecord AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipcReason AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTable AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTableDesc AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcKeyDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcField AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFieldDesc AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFieldValue AS CHARACTER NO-UNDO.
    
    CREATE ttException.
    ASSIGN 
        ttException.recordRowid            = ipriRecord
        ttException.company                = ipcCompany
        ttException.exceptionReason        = ipcReason
        
        ttException.recordTable            = ipcTable
        ttException.recordTableDescription = ipcTableDesc
        ttException.recordKeyDescription   = ipcKeyDescription
        
        ttException.recordField            = ipcField
        ttException.recordFieldDescription = ipcFieldDesc
        ttException.recordFieldValue       = ipcFieldValue 
        .

END PROCEDURE.

PROCEDURE pAddFGItemToUpdate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given invoice line to post, add FG ITem to update or add
        sums if it already exists
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItemfg AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iplInPeriod AS LOGICAL NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-ttInvoiceLineToPost FOR ttInvoiceLineToPost.

    FIND FIRST ttFGItemToUpdate
        WHERE ttFGITemToUpdate.riItemfg EQ ipriItemfg
        NO-ERROR.
    IF NOT AVAILABLE ttFGItemToUpdate THEN 
    DO: 
        CREATE ttFGItemToUpdate.
        ASSIGN 
            ttFGItemToUpdate.riItemfg = ipriItemFG
            ttFGItemToUpdate.company  = ipbf-ttInvoiceLineToPost.company
            ttFGItemToUpdate.itemID   = ipbf-ttInvoiceLineToPost.itemID
            ttFGItemToUpdate.itemName = ipbf-ttInvoiceLineToPost.itemName
            ttFGItemToUpdate.periodID = ipbf-ttInvoiceLIneToPost.periodID
            .
    END.
    IF iplInPeriod THEN 
        ASSIGN 
            ttFGItemToUpdate.quantityInvoicedPTD    = ttFGItemToUpdate.quantityInvoicedPTD + ipbf-ttInvoiceLIneToPost.quantityInvoiced
            ttFGItemToUpdate.quantityShippedPTD     = ttFGItemToUpdate.quantityShippedPTD + ipbf-ttInvoiceLineToPost.quantityShipped
            ttFGItemToUpdate.quantityAllocatedPTD   = ttFGItemToUpdate.quantityAllocatedPTD + ipbf-ttInvoiceLineToPost.quantityShipped
            ttFGItemToUpdate.quantityInvoicedMSFPTD = ttFGItemToUpdate.quantityInvoicedMSFPTD + ipbf-ttInvoiceLineToPost.quantityInvoicedMSF
            .
    ASSIGN 
        ttFGItemToUpdate.quantityInvoicedTotal    = ttFGItemToUpdate.quantityInvoicedTotal + ipbf-ttInvoiceLIneToPost.quantityInvoiced
        ttFGItemToUpdate.quantityShippedTotal     = ttFGItemToUpdate.quantityShippedTotal + ipbf-ttInvoiceLineToPost.quantityShipped
        ttFGItemToUpdate.quantityAllocatedTotal   = ttFGItemToUpdate.quantityAllocatedTotal + ipbf-ttInvoiceLineToPost.quantityShipped
        ttFGItemToUpdate.quantityInvoicedMSFTotal = ttFGItemToUpdate.quantityInvoicedMSFTotal + ipbf-ttInvoiceLineToPost.quantityInvoicedMSF
        .
        
END PROCEDURE.

PROCEDURE pAddGLTransaction PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  GIven input buffers, transaction type, account and amount,
        add a new ttGLTransaction
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttPostingMaster FOR ttPostingMaster.
    DEFINE PARAMETER BUFFER ipbf-ttInvoiceToPost FOR ttInvoiceToPost.
    DEFINE INPUT PARAMETER ipcTransactionType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAccount AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdAmount AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    
    CREATE ttGLTransaction.
    ASSIGN 
        ttGLTransaction.transactionType   = ipcTransactionType
        ttGLTransaction.account           = ipcAccount
        ttGLTransaction.amount            = ipdAmount
        ttGLTransaction.itemID            = ipcItemID
        ttGLTransaction.transactionDesc   = fGetTransactionDescription(ttInvoiceToPost.company, ttInvoiceToPost.customerID, ttInvoiceToPost.invoiceID) + " " + ipcTransactionType
        ttGLTransaction.journalNote       = ipbf-ttPostingMaster.journalNote
        ttGLTransaction.transactionDate   = ipbf-ttPostingMaster.postDate
        ttGLTransaction.transactionPeriod = ipbf-ttPostingMaster.periodID
        ttGLTransaction.company           = ipbf-ttInvoiceToPost.company
        ttGLTransaction.invoiceID         = ipbf-ttInvoiceToPost.invoiceID
        ttGLTransaction.currencyCode      = ipbf-ttInvoiceToPost.currencyCode
        ttGLTransaction.currencyExRate    = ipbf-ttInvoiceToPost.currencyExRate
        .
    

END PROCEDURE.

PROCEDURE pAddGLTransactionsForFG PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a FG Item, Add the appropriate COGS and FG Inventory
        amounts to GL accounts in Product Line table
     Notes: replaces oe/invposty.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttPostingMaster     FOR ttPostingMaster.
    DEFINE PARAMETER BUFFER ipbf-ttInvoiceLineToPost FOR ttInvoiceLineToPost.
    
    RUN pAddGLTransactionsForFGDetail (BUFFER ipbf-ttPostingMaster, BUFFER ipbf-ttInvoiceLineToPost,
        "COGS", ipbf-ttInvoiceLineToPost.accountDLCogs, ipbf-ttInvoiceLineToPost.costDirectLabor, ipbf-ttInvoiceLineToPost.costUOM).
    RUN pAddGLTransactionsForFGDetail (BUFFER ipbf-ttPostingMaster, BUFFER ipbf-ttInvoiceLineToPost,
        "FG", ipbf-ttInvoiceLineToPost.accountDLFG, - ipbf-ttInvoiceLineToPost.costDirectLabor, ipbf-ttInvoiceLineToPost.costUOM).

    RUN pAddGLTransactionsForFGDetail (BUFFER ipbf-ttPostingMaster, BUFFER ipbf-ttInvoiceLineToPost,
        "COGS", ipbf-ttInvoiceLineToPost.accountFOCogs, ipbf-ttInvoiceLineToPost.costFixedOverhead, ipbf-ttInvoiceLineToPost.costUOM).
    RUN pAddGLTransactionsForFGDetail (BUFFER ipbf-ttPostingMaster, BUFFER ipbf-ttInvoiceLineToPost,
        "FG", ipbf-ttInvoiceLineToPost.accountFOFG, - ipbf-ttInvoiceLineToPost.costFixedOverhead, ipbf-ttInvoiceLineToPost.costUOM).

    RUN pAddGLTransactionsForFGDetail (BUFFER ipbf-ttPostingMaster, BUFFER ipbf-ttInvoiceLineToPost,
        "COGS", ipbf-ttInvoiceLineToPost.accountVOCogs, ipbf-ttInvoiceLineToPost.costVariableOverhead, ipbf-ttInvoiceLineToPost.costUOM).
    RUN pAddGLTransactionsForFGDetail (BUFFER ipbf-ttPostingMaster, BUFFER ipbf-ttInvoiceLineToPost,
        "FG", ipbf-ttInvoiceLineToPost.accountVOFG, - ipbf-ttInvoiceLineToPost.costVariableOverhead, ipbf-ttInvoiceLineToPost.costUOM).

    RUN pAddGLTransactionsForFGDetail (BUFFER ipbf-ttPostingMaster, BUFFER ipbf-ttInvoiceLineToPost,
        "COGS", ipbf-ttInvoiceLineToPost.accountDMCogs, ipbf-ttInvoiceLineToPost.costDirectMaterial, ipbf-ttInvoiceLineToPost.costUOM).
    RUN pAddGLTransactionsForFGDetail (BUFFER ipbf-ttPostingMaster, BUFFER ipbf-ttInvoiceLineToPost,
        "FG", ipbf-ttInvoiceLineToPost.accountDMFG, - ipbf-ttInvoiceLineToPost.costDirectMaterial, ipbf-ttInvoiceLineToPost.costUOM).


END PROCEDURE.

PROCEDURE pAddGLTransactionsForFGDetail PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Creates the pending GL Transactions to process for each account
                and specific cost
     Notes:  replaces oe/invpostx.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttPostingMaster     FOR ttPostingMaster.
    DEFINE PARAMETER BUFFER ipbf-ttInvoiceLineToPost FOR ttInvoiceLineToPost.
    DEFINE INPUT PARAMETER ipcTransactionType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAccount AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcCostUOM AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dCostPerEA AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostTotal AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    
    IF ipbf-ttInvoiceLineToPost.costUOM NE "EA" THEN 
        RUN Conv_ValueFromUOMtoUOM(ipbf-ttInvoiceLineToPost.company, ipbf-ttInvoiceLineToPost.itemID, "FG", 
            ipdCostPerUOM, ipbf-ttInvoiceLineToPost.costUOM, "EA", 
            0, 0, 0, 0, ipbf-ttInvoiceLineToPost.quantityPerSubUnit,
            OUTPUT dCostPerEA, OUTPUT lError, OUTPUT cMessage).
    ELSE 
        dCostPerEA = ipdCostPerUOM.
    
    dCostTotal = ROUND(dCostPerEA * ipbf-ttInvoiceLineToPost.quantityInvoiced, 2).
    
    CREATE ttGLTransaction.
    ASSIGN 
        ttGLTransaction.transactionType   = ipcTransactionType
        ttGLTransaction.account           = ipcAccount
        ttGLTransaction.amount            = dCostTotal
        ttGLTransaction.itemID            = ipbf-ttInvoiceLineToPost.itemID
        ttGLTransaction.transactionDesc   = fGetTransactionDescription(ipbf-ttInvoiceLineToPost.company, ipbf-ttInvoiceLineToPost.customerID, ipbf-ttInvoiceLineToPost.invoiceID) + " " + ipcTransactionType
        ttGLTransaction.journalNote       = ipbf-ttPostingMaster.journalNote
        ttGLTransaction.transactionDate   = ipbf-ttPostingMaster.postDate
        ttGLTransaction.transactionPeriod = ipbf-ttPostingMaster.periodID
        ttGLTransaction.company           = ipbf-ttInvoiceLineToPost.company
        ttGLTransaction.invoiceID         = ipbf-ttInvoiceLineToPost.invoiceID
        ttGLTransaction.currencyCode      = ipbf-ttInvoiceLineToPost.currencyCode
        ttGLTransaction.currencyExRate    = ipbf-ttInvoiceLineToPost.currencyExRate
        .


END PROCEDURE.

PROCEDURE pAddInvoiceLineToPost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given posting invoice line, invoice header, 
     build the ttInvoiceLineToPost record base (non-calculated fields)
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttPostingMaster FOR ttPostingMaster.
    DEFINE PARAMETER BUFFER ipbf-ttInvoiceToPost FOR ttInvoiceToPost.
    DEFINE PARAMETER BUFFER ipbf-inv-line        FOR inv-line.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
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
        ttInvoiceLineToPost.invoiceID         = ipbf-ttInvoiceToPost.invoiceID
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
        ttInvoiceLineToPost.bolID             = ipbf-ttInvoiceToPost.bolID
        ttInvoiceLineToPost.customerID        = ipbf-ttInvoiceToPost.customerID
        ttInvoiceLineToPost.periodID          = ipbf-ttInvoiceToPost.periodID
        ttInvoiceLineToPost.currencyCode      = ipbf-ttInvoiceToPost.currencyCode
        ttInvoiceLineToPost.currencyExRate    = ipbf-ttInvoiceToPost.currencyExRate
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
    RUN fg\GetFGArea.p (ROWID(bf-itemfg), "SF", OUTPUT ttInvoiceLineToPost.squareFeetPerEA).            
    ASSIGN 
        ttInvoiceLineToPost.quantityInvoicedWeight = ipbf-inv-line.inv-qty * bf-itemfg.weight-100 / 100  /*changed from .qty to .inv-qty*/
        ttInvoiceLineToPost.quantityInvoicedMSF    = ipbf-inv-line.inv-qty * ttInvoiceLineToPost.squareFeetPerEA / 1000
        ttInvoiceLineToPost.weightUOM              = "LB"
        ttInvoiceLineToPost.quantityPerSubUnit     = MAX(bf-itemfg.case-count, 1)
        ttInvoiceLineToPost.costFull               = bf-itemfg.spare-dec-1
        ttInvoiceLineToPost.productCategory        = bf-itemfg.procat
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

    RUN pGetAccountProductLine (BUFFER ipbf-ttPostingMaster, BUFFER ttInvoiceLineToPost, OUTPUT oplError, OUTPUT opcMessage).
                
    IF NOT oplError THEN /*Do not create FGs, Bols and orders to update unless all is ok*/
    DO:
        RUN pAddFGItemToUpdate(ROWID(bf-itemfg), ipbf-ttInvoiceToPost.isInvoiceDateInCurrentPeriod, BUFFER ttInvoiceLineToPost).
        RUN pAddBOLToUpdate(BUFFER ipbf-inv-line, ttInvoiceLineToPost.invoiceID, OUTPUT ttInvoiceLineToPost.bolID, OUTPUT ttInvoiceLineToPost.locationID).
        RUN pAddOrderToUpdate(BUFFER ipbf-inv-line, OUTPUT ttInvoiceLineToPost.quantityPerSubUnit).

        IF ipbf-inv-line.cas-cnt NE 0 THEN 
            ttInvoiceLineToPost.quantityPerSubUnit = ipbf-inv-line.cas-cnt.
         
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
       
    DEFINE BUFFER bf-oe-ord   FOR oe-ord.
    DEFINE BUFFER bf-oe-ordm  FOR oe-ordm.
    DEFINE BUFFER bf-est-prep FOR est-prep.
    
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
        ttInvoiceMiscToPost.costTotal      = ipbf-inv-misc.cost
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
        END. /*error asssignment*/
    END.  /*inv-misc.actnum ne ""*/
    IF NOT oplError THEN 
    DO:
        IF ipbf-inv-misc.ord-no NE 0 THEN 
            FIND FIRST bf-oe-ord NO-LOCK 
                WHERE bf-oe-ord.company EQ ipbf-inv-misc.company
                AND bf-oe-ord.ord-no EQ ipbf-inv-misc.ord-no
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
            END. /*Not avail ttOrderToUpdate*/
            FIND FIRST bf-oe-ordm NO-LOCK 
                WHERE bf-oe-ordm.company EQ ipbf-inv-misc.company
                AND bf-oe-ordm.ord-no EQ ipbf-inv-misc.ord-no
                AND bf-oe-ordm.line EQ ipbf-inv-misc.line
                AND bf-oe-ordm.charge EQ ipbf-inv-misc.charge
                NO-ERROR.
            IF AVAILABLE bf-oe-ordm THEN 
            DO:
                FIND FIRST ttOrderMiscToUpdate NO-LOCK 
                    WHERE ttORderMiscToUpdate.riOeOrdm EQ ROWID(bf-oe-ordm)
                    NO-ERROR.
                IF NOT AVAILABLE ttOrderMiscToUpdate THEN 
                DO:
                    CREATE ttOrderMiscToUpdate.
                    ASSIGN 
                        ttOrderMiscToUpdate.riOeOrdm   = ROWID(bf-oe-ordm)
                        ttOrderMiscToUpdate.company    = bf-oe-ordm.company
                        ttOrderMiscToUpdate.itemID     = bf-oe-ordm.charge
                        ttOrderMiscToUpdate.orderID    = bf-oe-ordm.ord-no
                        ttOrderMiscToUpdate.orderLine  = bf-oe-ordm.line
                        ttOrderMiscToUpdate.billStatus = IF ipbf-inv-misc.bill EQ "Y" THEN "I" ELSE "Y"
                        .
                END. /*not avail ttOrderMiscToUpdate*/
                IF bf-oe-ordm.miscType EQ 1 THEN 
                    FOR EACH bf-est-prep NO-LOCK 
                        WHERE bf-est-prep.company EQ bf-oe-ordm.company
                        AND bf-est-prep.est-no EQ bf-oe-ordm.est-no
                        AND bf-est-prep.eqty EQ bf-oe-ordm.estPrepEqty
                        AND bf-est-prep.line EQ bf-oe-ordm.estPrepLine
                        AND bf-est-prep.code EQ bf-oe-ordm.charge
                        AND bf-est-prep.simon EQ "S"
                        AND bf-est-prep.amtz EQ 100
                        AND NOT CAN-FIND(FIRST ttEstPrepToUpdate WHERE ttEstPrepToUpdate.riEstPrep EQ ROWID(bf-est-prep)):
                            
                        CREATE ttEstPrepToUpdate.
                        ASSIGN 
                            ttEstPrepToUpdate.riEstPrep     = ROWID(bf-est-prep)
                            ttEstPrepToUpdate.deleteEstPrep = ipbf-ttPostingMaster.deleteEstPrep
                            ttEstPrepToUpdate.newSimon      = "N"
                            .
                        
                    END.  /*Each bf-est-prep*/
            END. /*avail bf-oe-ordm*/
        END.  /*avail bf-oe-ord*/                              
    END. /*Not oplError*/    
            
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
        opbf-ttInvoiceToPost.periodID                     = ipbf-ttPostingMaster.periodID 
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
        opbf-ttInvoiceToPost.bolID                        = ipbf-inv-head.bol-no
        .
    IF opbf-ttInvoiceToPost.isFreightBillable THEN 
        opbf-ttInvoiceToPost.amountBilledFreight = ipbf-inv-head.t-inv-freight.
        
    RUN pGetCurrencyCodeAndRate(ipbf-inv-head.company, ipbf-inv-head.curr-code[1], ipbf-cust.curr-code, ipbf-ttPostingMaster.currencyCode, 
        OUTPUT opbf-ttInvoiceToPost.currencyCode, OUTPUT opbf-ttInvoiceToPost.currencyExRate, OUTPUT opbf-ttInvoiceToPost.accountARCurrency,
        OUTPUT oplError, OUTPUT opcMessage).
        
    IF oplError THEN 
        ASSIGN 
            opbf-ttInvoiceToPost.isOKToPost     = NO
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
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-child-inv-head FOR inv-head.
    DEFINE BUFFER bf-child-inv-line FOR inv-line.
    DEFINE BUFFER bf-child-inv-misc FOR inv-misc.
    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    ASSIGN 
        oplError   = YES
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
                oplError   = NO
                opcMessage = ""
                .   
                
            RUN pAddInvoiceLineToPost(BUFFER ipbf-ttPostingMaster, BUFFER ipbf-ttInvoiceToPost, BUFFER bf-child-inv-line, ipcCompany, ipbf-master-inv-head.r-no, OUTPUT lError, OUTPUT cMessage).
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
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiProcessed AS INTEGER NO-UNDO.
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
    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
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
        oplError   = YES
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
        opiProcessed = opiProcessed + 1.
        RUN pAddInvoiceToPost(BUFFER ttPostingMaster, BUFFER bf-inv-head, BUFFER bf-cust, OUTPUT lError, OUTPUT cMessage, BUFFER bf-ttInvoiceToPost).
        IF fIsWritable(ROWID(bf-inv-head)) THEN 
        DO:
            ASSIGN 
                oplError   = NO
                opcMessage = ""
                .
            IF lError THEN NEXT.
                
            FOR EACH bf-inv-line NO-LOCK
                WHERE bf-inv-line.r-no EQ bf-inv-head.r-no
                USE-INDEX r-no:
                RUN pAddInvoiceLineToPost(BUFFER ttPostingMaster, BUFFER bf-ttInvoiceToPost, BUFFER bf-inv-line, ipcCompany, bf-inv-head.r-no, OUTPUT lError, OUTPUT cMessage). 
            END. /*each bf-inv-line*/
            IF lError THEN 
            DO: 
                ASSIGN
                    bf-ttInvoiceToPost.isOKToPost     = NO
                    bf-ttInvoiceToPost.problemMessage = cMessage
                    .
                NEXT. 
            END.
            FOR EACH bf-inv-misc NO-LOCK
                WHERE bf-inv-misc.r-no EQ bf-inv-head.r-no
                USE-INDEX r-no:
                RUN pAddInvoiceMiscToPost(BUFFER ttPostingMaster, BUFFER bf-inv-head, BUFFER bf-inv-misc, bf-inv-head.r-no, OUTPUT lError, OUTPUT cMessage). 
            END. /*each bf-inv-misc*/
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
                RUN pAlignMultiInvoiceLinesWithMaster(BUFFER ttPostingMaster, BUFFER bf-inv-head, BUFFER bf-ttInvoiceToPost, ipcCompany, OUTPUT lError, OUTPUT cMessage).
                IF lError THEN 
                DO:
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
            IF AVAILABLE bf-ttInvoiceLineToPost THEN 
            DO:
                ASSIGN 
                    bf-ttInvoiceToPost.isOKToPost     = NO
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
            
            IF bf-ttInvoiceToPost.isOKToPost THEN 
            DO:
                FIND FIRST ttCustomerToUpdate NO-LOCK
                    WHERE ttCustomerToUpdate.riCust EQ ROWID(bf-cust)
                    NO-ERROR.
                IF NOT AVAILABLE ttCustomerToUpdate THEN 
                DO:
                    CREATE ttCustomerToUpdate.
                    ASSIGN 
                        ttCustomerToUpdate.riCust     = ROWID(bf-cust)
                        ttCustomerToUpdate.company    = bf-cust.company
                        ttCustomerToUpdate.customerID = bf-cust.cust-no
                        ttCustomerToUpdate.periodID   = ttPostingMaster.periodID
                        .  
                END.
            END.
        END. /*inv-head is writable*/
        ELSE 
            ASSIGN 
                bf-ttInvoiceToPost.isOKToPost     = NO
                bf-ttInvoiceToPost.problemMessage = "Invoice is locked"
                .
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
        ASSIGN 
            oplError   = YES
            opcMessage = ipcAccountSource + " has a blank " + ipcAccountDesc
            .
    ELSE 
    DO: 
        FIND FIRST account NO-LOCK
            WHERE account.company EQ ipcCompany
            AND account.actnum  EQ ipcAccount
            NO-ERROR.
        IF NOT AVAILABLE account THEN 
            ASSIGN 
                oplError   = YES
                opcMessage = ipcAccountSource + " has an invalid " + ipcAccountDesc + " of " + ipcAccount
                . 
        
        ELSE IF account.inactive THEN 
                ASSIGN
                    oplError   = YES
                    opcMessage = ipcAccountSource + " has an inactive " + ipcAccountDesc + " of " + ipcAccount
                    .
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
    DISABLE TRIGGERS FOR LOAD OF ar-inv.
    ASSIGN
        bf-ar-inv.x-no         = iNextXNO
        bf-ar-inv.company      = ipbf-inv-head.company
        bf-ar-inv.ord-no       = ipbf-ttInvoiceToPost.orderID
        bf-ar-inv.ord-date     = ipbf-ttInvoiceToPost.orderDate
        bf-ar-inv.inv-no       = ipbf-inv-head.inv-no
        bf-ar-inv.inv-date     = ipbf-inv-head.inv-date

        bf-ar-inv.prod-date    = ipbf-ttInvoiceToPost.postDate /* using prod-date as posted date #53205, pass in tran-date or dtPostDate */
        bf-ar-inv.period       = ipbf-ttInvoiceToPost.periodID
        
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
        bf-ar-inv.curr-code[1] = ipbf-ttInvoiceToPost.currencyCode
        bf-ar-inv.ex-rate      = ipbf-ttInvoiceToPost.currencyExRate
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
        
    opriArinv = ROWID(bf-ar-inv).

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


PROCEDURE pCreateEDI PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given an inv-header buffer, executes EDI (810) procedure
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-inv-head FOR inv-head.
    
    FIND FIRST edCode NO-LOCK
        WHERE  edcode.setid EQ "810"
        NO-ERROR.
    IF AVAIL edCode THEN 
    DO: 
        RUN ed/asi/o810hook.p (RECID(ipbf-inv-head), NO, NO).     
        FIND FIRST edmast NO-LOCK
            WHERE edmast.cust EQ ipbf-inv-head.cust-no
            NO-ERROR.
        IF AVAILABLE edmast THEN 
        DO: 
            FIND FIRST edcode NO-LOCK
                WHERE edcode.partner EQ edmast.partner
                NO-ERROR.
            IF NOT AVAILABLE edcode THEN 
                FIND FIRST edcode NO-LOCK
                    WHERE edcode.partner EQ edmast.partnerGrp
                    NO-ERROR.
            IF AVAILABLE edcode AND edcode.sendFileOnPrint THEN    
                RUN ed/asi/write810.p (ipbf-inv-head.company, ipbf-inv-head.inv-no).    
        END.  /*ED master is available*/
    END.  /*ED Code available*/

END PROCEDURE.

PROCEDURE pCreateGLTransFromTransaction PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given temp-table buffer, create GL transaction
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttPostingMaster FOR ttPostingMaster.
    DEFINE PARAMETER BUFFER ipbf-ttGLTransaction FOR ttGLTransaction.
    DEFINE INPUT PARAMETER ipdTransactionAmount AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiRun AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opriGLTrans AS ROWID NO-UNDO.
     
    RUN pCreateGLTrans(BUFFER ipbf-ttPostingMaster, ipdTransactionAmount, ipbf-ttGLTransaction.account, ipbf-ttGLTransaction.transactionDesc, ipiRun, ipbf-ttGLTransaction.transactionDate, 
        ipbf-ttGLTransaction.transactionPeriod, 
        ipbf-ttGLTransaction.currencyCode, ipbf-ttGLTransaction.currencyExRate, OUTPUT opriGLTrans).

END PROCEDURE.

PROCEDURE pCreateGLTrans PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given temp-table buffer, create GL transaction
     Notes:
         RUN pCreateGLTrans(BUFFER ipbf-ttPostingMaster, dAmount, cAccount, cDescription, iRun, 
         dtTransDate, iPeriod, ipcCurrCode, ipdExRate, OUTPUT riGLTrans). 
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttPostingMaster FOR ttPostingMaster.
    DEFINE INPUT PARAMETER ipdTransactionAmount AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcAccount AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiRun AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtTransactionDate AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipiTransactionPeriod AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCurrCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdExRate AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opriGLTrans AS ROWID NO-UNDO.
     
    DEFINE BUFFER bf-gltrans FOR gltrans.
     
    IF ipdTransactionAmount NE 0 THEN 
    DO:
        CREATE bf-gltrans.
        ASSIGN
            opriGLTrans          = ROWID(bf-gltrans)
            bf-gltrans.company   = ipbf-ttPostingMaster.company
            bf-gltrans.actnum    = ipcAccount
            bf-gltrans.jrnl      = ipbf-ttPostingMaster.journalNote
            bf-gltrans.tr-dscr   = ipcDescription
            bf-gltrans.tr-amt    = ipdTransactionAmount
            bf-gltrans.period    = IF ipiTransactionPeriod EQ 0 THEN ipbf-ttPostingMaster.periodID ELSE ipiTransactionPeriod
            bf-gltrans.tr-date   = IF ipdtTransactionDate EQ ? THEN ipbf-ttPostingMaster.postDate ELSE ipdtTransactionDate
            bf-gltrans.trnum     = ipiRun
            bf-gltrans.curr-code = ipcCurrCode
            bf-gltrans.ex-rate   = ipdExRate
            .
    
        RELEASE bf-gltrans.
    END.
    
END PROCEDURE.


PROCEDURE pExportAllTempTables PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Exports all TempTables to file
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdOutput    AS HANDLE NO-UNDO.
    DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdTempTable AS HANDLE NO-UNDO.
    
    FIND FIRST ttPostingMaster NO-ERROR.
    IF AVAILABLE ttPostingMaster AND ttPostingMaster.exportPath NE "" THEN 
    DO:
    
        RUN system\OutputProcs.p PERSISTENT SET hdOutput.
        THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hdOutput).
        
        ASSIGN 
            cFile = fGetFilePath(ttPostingMaster.exportPath, "InvoiceHeaders", TRIM(STRING(ttPostingMaster.runID,">>>>>>>>>>>9")), "csv")
            hdTempTable = TEMP-TABLE ttInvoiceToPost:HANDLE.
        RUN Output_TempTableToCSV(hdTempTable, cFile, YES).
        
        ASSIGN 
            cFile       = fGetFilePath(ttPostingMaster.exportPath, "InvoiceLines", TRIM(STRING(ttPostingMaster.runID,">>>>>>>>>>>9")), "csv")
            hdTempTable = TEMP-TABLE ttInvoiceLineToPost:HANDLE.
        RUN Output_TempTableToCSV(hdTempTable, cFile, YES).
        
        ASSIGN 
            cFile       = fGetFilePath(ttPostingMaster.exportPath, "InvoiceMiscs", TRIM(STRING(ttPostingMaster.runID,">>>>>>>>>>>9")), "csv")
            hdTempTable = TEMP-TABLE ttInvoiceMiscToPost:HANDLE.
        RUN Output_TempTableToCSV(hdTempTable, cFile, YES).
        
        ASSIGN 
            cFile       = fGetFilePath(ttPostingMaster.exportPath, "GLTransactions", TRIM(STRING(ttPostingMaster.runID,">>>>>>>>>>>9")), "csv")
            hdTempTable = TEMP-TABLE ttGLTransaction:HANDLE.
        RUN Output_TempTableToCSV(hdTempTable, cFile, YES).
        
        ASSIGN 
            cFile       = fGetFilePath(ttPostingMaster.exportPath, "ARLedgerTransactions", TRIM(STRING(ttPostingMaster.runID,">>>>>>>>>>>9")), "csv")
            hdTempTable = TEMP-TABLE ttARLedgerTransaction:HANDLE.
        RUN Output_TempTableToCSV(hdTempTable, cFile, YES).
        
        ASSIGN 
            cFile       = fGetFilePath(ttPostingMaster.exportPath, "Customers", TRIM(STRING(ttPostingMaster.runID,">>>>>>>>>>>9")), "csv")
            hdTempTable = TEMP-TABLE ttCustomerToUpdate:HANDLE.
        RUN Output_TempTableToCSV(hdTempTable, cFile, YES).
        
        ASSIGN 
            cFile       = fGetFilePath(ttPostingMaster.exportPath, "OrderHeaders", TRIM(STRING(ttPostingMaster.runID,">>>>>>>>>>>9")), "csv")
            hdTempTable = TEMP-TABLE ttOrderToUpdate:HANDLE.
        RUN Output_TempTableToCSV(hdTempTable, cFile, YES).
        
        ASSIGN 
            cFile       = fGetFilePath(ttPostingMaster.exportPath, "OrderLines", TRIM(STRING(ttPostingMaster.runID,">>>>>>>>>>>9")), "csv")
            hdTempTable = TEMP-TABLE ttOrderLineToUpdate:HANDLE.
        RUN Output_TempTableToCSV(hdTempTable, cFile, YES).
        
        ASSIGN 
            cFile       = fGetFilePath(ttPostingMaster.exportPath, "BOLLines", TRIM(STRING(ttPostingMaster.runID,">>>>>>>>>>>9")), "csv")
            hdTempTable = TEMP-TABLE ttBOLLineToUpdate:HANDLE.
        RUN Output_TempTableToCSV(hdTempTable, cFile, YES).
        
        ASSIGN 
            cFile       = fGetFilePath(ttPostingMaster.exportPath, "FGItems", TRIM(STRING(ttPostingMaster.runID,">>>>>>>>>>>9")), "csv")
            hdTempTable = TEMP-TABLE ttFGItemToUpdate:HANDLE.
        RUN Output_TempTableToCSV(hdTempTable, cFile, YES).        
     
        DELETE OBJECT hdOutput.
    
    END. /*PostingMaster has export path*/

END PROCEDURE.

PROCEDURE pExportExceptions PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Exports all exceptions to file
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE hdOutput    AS HANDLE NO-UNDO.
    DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdTempTable AS HANDLE NO-UNDO.
    
    FIND FIRST ttPostingMaster NO-ERROR.
    IF AVAILABLE ttPostingMaster AND ttPostingMaster.exportPath NE "" THEN 
    DO:
        RUN system\OutputProcs.p PERSISTENT SET hdOutput.
        THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hdOutput).
    
        ASSIGN 
            cFile       = fGetFilePath(ttPostingMaster.exportPath, "Exceptions", TRIM(STRING(ttPostingMaster.runID,">>>>>>>>>>>9")), "csv")
            hdTempTable = TEMP-TABLE ttException:HANDLE.
        RUN Output_TempTableToCSV(hdTempTable, cFile, YES).       
        
        DELETE OBJECT hdOutput.
        
    END. /*PostingMaster has export path*/
    
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
    DEFINE OUTPUT PARAMETER opcAccountCogs AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAccountFG AS CHARACTER NO-UNDO.
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
            opcAccountCogs       = ar-ctrl.discount  /*REFACTOR - Assign Appropriate Default*/
            opcAccountFG         = ar-ctrl.discount  /*REFACTOR - Assign Appropriate Default*/
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
        IF NOT oplError THEN 
            RUN pCheckAccount(ipcCompany, opcAccountCOGS, cAccountSource, "COGS Default Account", OUTPUT oplError, OUTPUT opcMessage).
        IF NOT oplError THEN 
            RUN pCheckAccount(ipcCompany, opcAccountFG, cAccountSource, "FG Default Account", OUTPUT oplError, OUTPUT opcMessage).                   
    END.  /*valid ar-ctrl*/
    
    
END PROCEDURE.

PROCEDURE pGetAccountProductLine PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  given a product category, validate and return the 8 accounts for 
     COGS and FG
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttPostingMaster     FOR ttPostingMaster.
    DEFINE PARAMETER BUFFER ipbf-ttInvoiceLineToPost FOR ttInvoiceLineToPost.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-prodl FOR prodl.
    DEFINE BUFFER bf-prod  FOR prod.
        
    DEFINE VARIABLE cAccountSource AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
    
    ASSIGN 
        ipbf-ttInvoiceLineToPost.accountDLCogs = ipbf-ttPostingMaster.accountCOGS
        ipbf-ttInvoiceLineToPost.accountVOCogs = ipbf-ttPostingMaster.accountCOGS
        ipbf-ttInvoiceLineToPost.accountFOCogs = ipbf-ttPostingMaster.accountCOGS
        ipbf-ttInvoiceLineToPost.accountDMCogs = ipbf-ttPostingMaster.accountCOGS
        ipbf-ttInvoiceLineToPost.accountDLFG   = ipbf-ttPostingMaster.accountFG
        ipbf-ttInvoiceLineToPost.accountVOFG   = ipbf-ttPostingMaster.accountFG
        ipbf-ttInvoiceLineToPost.accountFOFG   = ipbf-ttPostingMaster.accountFG
        ipbf-ttInvoiceLineToPost.accountDMFG   = ipbf-ttPostingMaster.accountFG
        .
        
    FOR EACH bf-prodl
        WHERE bf-prodl.company EQ ipbf-ttInvoiceLineToPost.company
        AND bf-prodl.procat  EQ ipbf-ttInvoiceLineToPost.productCategory
        NO-LOCK,
        FIRST bf-prod
        WHERE bf-prod.company EQ bf-prodl.company
        AND bf-prod.prolin  EQ bf-prodl.prolin
        NO-LOCK:
        ASSIGN 
            cAccountSource = "Product Line File for " + bf-prod.prolin
            .
        IF bf-prod.cgs-dl NE "" THEN ipbf-ttInvoiceLineToPost.accountDLCogs = bf-prod.cgs-dl.
        IF bf-prod.fg-lab NE "" THEN ipbf-ttInvoiceLineToPost.accountDLFG = bf-prod.fg-lab.
        IF bf-prod.cgs-vo NE "" THEN ipbf-ttInvoiceLineToPost.accountVOCogs = bf-prod.cgs-vo.             
        IF bf-prod.fg-vo NE "" THEN ipbf-ttInvoiceLineToPost.accountVOFG = bf-prod.fg-vo.
        IF bf-prod.cgs-fo NE "" THEN ipbf-ttInvoiceLineToPost.accountFOCogs = bf-prod.cgs-fo.
        IF bf-prod.fg-fo NE "" THEN ipbf-ttInvoiceLineToPost.accountFOFG   = bf-prod.fg-fo.
        IF bf-prod.cgs-mat NE "" THEN ipbf-ttInvoiceLineToPost.accountDMCogs = bf-prod.cgs-mat.
        IF bf-prod.fg-mat NE "" THEN ipbf-ttInvoiceLineToPost.accountDMFG   = bf-prod.fg-mat.
               
        LEAVE.
    END.    
    
    RUN pCheckAccount(ipbf-ttInvoiceLineToPost.company, ipbf-ttInvoiceLineToPost.accountDLCogs, cAccountSource, "COGS Direct Labor Account", OUTPUT lError, OUTPUT cMessage).
    IF NOT lError THEN 
        RUN pCheckAccount(ipbf-ttInvoiceLineToPost.company, ipbf-ttInvoiceLineToPost.accountDLFG, cAccountSource, "FG Direct Labor Account", OUTPUT lError, OUTPUT cMessage).
    IF NOT lError THEN 
        RUN pCheckAccount(ipbf-ttInvoiceLineToPost.company, ipbf-ttInvoiceLineToPost.accountVOCogs, cAccountSource, "COGS Variable Overhead Account", OUTPUT lError, OUTPUT cMessage).
    IF NOT lError THEN 
        RUN pCheckAccount(ipbf-ttInvoiceLineToPost.company, ipbf-ttInvoiceLineToPost.accountVOFG, cAccountSource, "FG Variable Overhead Account", OUTPUT lError, OUTPUT cMessage).
    IF NOT lError THEN 
        RUN pCheckAccount(ipbf-ttInvoiceLineToPost.company, ipbf-ttInvoiceLineToPost.accountFOCogs, cAccountSource, "COGS Fixed Overhead Account", OUTPUT lError, OUTPUT cMessage).
    IF NOT lError THEN 
        RUN pCheckAccount(ipbf-ttInvoiceLineToPost.company, ipbf-ttInvoiceLineToPost.accountFOFG, cAccountSource, "FG Fixed Overhead Account", OUTPUT lError, OUTPUT cMessage).
    IF NOT lError THEN 
        RUN pCheckAccount(ipbf-ttInvoiceLineToPost.company, ipbf-ttInvoiceLineToPost.accountDMCogs, cAccountSource, "COGS Direct Material Account", OUTPUT lError, OUTPUT cMessage).
    IF NOT lError THEN 
        RUN pCheckAccount(ipbf-ttInvoiceLineToPost.company, ipbf-ttInvoiceLineToPost.accountDMFG, cAccountSource, "FG Direct Material Account", OUTPUT lError, OUTPUT cMessage).
    
    IF lError THEN 
        ASSIGN
            oplError                                = YES
            opcMessage                              = cMessage 
            ipbf-ttInvoiceLineToPost.isOKToPost     = NO
            ipbf-ttInvoiceLineToPost.problemMessage = cMessage
            .

END PROCEDURE.

PROCEDURE pAddBOLToUpdate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  given an invoice line buffer, create a BOL Line to update and
     return the BOL information for the invoice line
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-inv-line FOR inv-line.
    DEFINE INPUT PARAMETER ipiInvoiceID AS INTEGER NO-UNDO.
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
        WHERE bf-oe-bolh.company EQ bf-oe-boll.company
        AND bf-oe-bolh.b-no EQ bf-oe-boll.b-no
        BREAK BY bf-oe-bolh.bol-no:
                
        FIND FIRST ttBolLineToUpdate
            WHERE ttBolLineToUpdate.riOeBoll EQ ROWID(bf-oe-boll)
            NO-ERROR.
        IF NOT AVAILABLE ttBolLineToUpdate THEN 
        DO:
            CREATE ttBolLineToUpdate.
            ASSIGN 
                ttBolLineToUpdate.riOeBoll  = ROWID(bf-oe-boll)
                ttBollineToUpdate.invoiceID = ipiInvoiceID
                ttBolLineToUpdate.bolID     = bf-oe-bolh.bol-no
                ttBolLineToUpdate.company   = bf-oe-bolh.company
                ttBolLineToUpdate.orderID   = bf-oe-boll.ord-no
                ttBolLineToUpdate.orderLine = bf-oe-boll.line
                ttBOLLineToUpdate.itemID    = bf-oe-boll.i-no      
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
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCurrCodeInvoice AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCurrCodeCust AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCurrCodeMaster AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCurrencyCode AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCurrencyExchangeRate AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAccountARCurrency AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-company  FOR company.
    DEFINE BUFFER bf-currency FOR currency.
    
    opcCurrencyCode = ipcCurrCodeInvoice.
    IF opcCurrencyCode EQ "" THEN 
        opcCurrencyCode = ipcCurrCodeCust.
    IF opcCurrencyCode EQ "" THEN 
    DO:
        opcCurrencyCode = ipcCurrCodeMaster.
    END.            
    IF opcCurrencyCode NE "" THEN 
        FIND bf-currency NO-LOCK 
            WHERE bf-currency.company EQ ipcCompany
            AND bf-currency.c-code EQ opcCurrencyCode
//            AND bf-currency.ar-ast-acct NE ""
            AND bf-currency.ex-rate GT 0 
            NO-ERROR.
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Currency code is blank for invoice, customer and company (" + ipcCompany + ")"
            .
        
    IF AVAILABLE bf-currency THEN 
        ASSIGN 
            opdCurrencyExchangeRate = bf-currency.ex-rate 
            opcAccountARCurrency    = bf-currency.ar-ast-acct
            .
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid currency code " + opcCurrencyCode
            .    

END PROCEDURE.

PROCEDURE pAddOrderToUpdate PRIVATE:
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
    DEFINE PARAMETER BUFFER ipbf-ttPostingMaster FOR ttPostingMaster.
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (ipbf-ttPostingMaster.company, "INVPOST", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN ipbf-ttPostingMaster.blockZeroCost = cReturn EQ "YES".
    
    RUN sys/ref/nk1look.p (ipbf-ttPostingMaster.company, "OEPREP", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN ipbf-ttPostingMaster.deleteEstPrep = cReturn EQ "YES".
    
    RUN sys/ref/nk1look.p (ipbf-ttPostingMaster.company, "UseNewInvoicePost", "C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN ipbf-ttPostingMaster.exportPath = cReturn.
    
    
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
    
    DEFINE BUFFER bf-period  FOR period.
    DEFINE BUFFER bf-company FOR company.
    
    DEFINE VARIABLE lValid AS LOGICAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttPostingMaster.
    EMPTY TEMP-TABLE ttInvoiceToPost.
    EMPTY TEMP-TABLE ttInvoiceLineToPost.
    
    
    CREATE ttPostingMaster.
    ASSIGN 
        ttPostingMaster.company            = ipcCompany
        ttPostingMaster.invoiceStart       = ipiInvNoStart
        ttPostingMaster.invoiceEnd         = ipiInvNoEnd
        ttPostingMaster.invoiceDateStart   = ipdtInvDateStart
        ttPostingMaster.invoiceDateEnd     = ipdtInvDateEnd
        ttPostingMaster.customerIDStart    = ipcCustomerIDStart
        ttPostingMaster.customerIDEnd      = ipcCustomerIDEnd
        ttPostingMaster.postDate           = ipdtPostDate 
        ttPostingMaster.consolidateAR      = YES
        ttPostingMaster.consolidateDisc    = YES
        ttPostingMaster.consolidateFreight = YES
        ttPostingMaster.consolidateCash    = YES
        ttPostingMaster.consolidateLine    = NO
        ttPostingMaster.consolidateMisc    = NO
        ttPostingMaster.consolidateTax     = NO
        ttPostingMaster.consolidateCOGS    = NO
        ttPostingMaster.consolidateFG      = NO
        ttPostingMaster.journalNote        = "OEINV"
        ttPostingMaster.runID              = fGetNextRun(ttPostingMaster.company, NO)
        .
    
    FIND FIRST bf-company NO-LOCK    
        WHERE bf-company.company EQ ipcCompany 
        NO-ERROR.
    IF AVAILABLE bf-company THEN        
        ttPostingMaster.currencyCode = bf-company.curr-code.
    ELSE 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid company " + ipcCompany
            .
        RETURN.
    END.        
    
    RUN pGetSettings(BUFFER ttPostingMaster).
    
    IF ttPostingMaster.exportPath NE "" THEN DO: 
        RUN FileSys_GetFilePath(ttPostingMaster.exportPath, OUTPUT ttPostingMaster.exportPath, OUTPUT lValid, OUTPUT opcMessage). 
        IF NOT lValid THEN DO:
            ASSIGN 
                oplError = YES.
            RETURN.
        END.
    END.
    
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
        OUTPUT ttPostingMaster.accountCogs,
        OUTPUT ttPostingMaster.accountFG,
        OUTPUT oplError, OUTPUT opcMessage).
        
    RUN pGetCurrencyCodeAndRate(ipcCompany, "", "", ttPostingMaster.currencyCode, 
        OUTPUT ttPostingMaster.currencyCode, OUTPUT ttPostingMaster.currencyExRate, OUTPUT ttPostingMaster.accountARCurrency,
        OUTPUT oplError, OUTPUT opcMessage).
        

END PROCEDURE.

PROCEDURE PostInvoices:
    /*------------------------------------------------------------------------------
     Purpose:  Main Public Procedure for Post Invoices
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiInvNoStart AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiInvNoEnd AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtInvDateStart AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipdtInvDateEnd AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerIDStart AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerIDEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtPostDate AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipcOptions AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiCountProcessed AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiCountValid AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiCountPosted AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lExceptions AS LOGICAL NO-UNDO.
    
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
        RUN pBuildInvoicesToPost(ipcCompany, OUTPUT opiCountProcessed, OUTPUT oplError, OUTPUT opcMessage).

    IF NOT oplError THEN
        /*Process the master list of invoices for reporting and/or posting*/
        RUN pProcessInvoicesToPost(OUTPUT opiCountValid, OUTPUT oplError, OUTPUT opcMessage).

    IF NOT oplError AND LOOKUP("Export",ipcOptions) GT 0 THEN
        RUN pExportAllTempTables.

    IF NOT oplError AND LOOKUP("Post",ipcOptions) GT 0 THEN
        RUN pPostAll (OUTPUT opiCountPosted, OUTPUT lExceptions, OUTPUT oplError, OUTPUT opcMessage).

    IF NOT oplError AND lExceptions AND LOOKUP("ExportExceptions",ipcOptions) GT 0 THEN
        RUN pExportExceptions.

    DELETE OBJECT ghNotesProcs.

END PROCEDURE.

PROCEDURE pPostARLedger PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Process the AR ledger list and creates the ar-ledger transactions
     in the DB
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiRun AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-ar-ledger FOR ar-ledger.

    FOR EACH ttARLedgerTransaction:
        FIND FIRST bf-ar-ledger EXCLUSIVE-LOCK 
            WHERE bf-ar-ledger.company EQ ttArLedgerTransaction.company
            AND bf-ar-ledger.cust-no EQ ttARLedgerTransaction.customerID
            AND bf-ar-ledger.ref-date EQ ttARLedgerTransaction.referenceDate
            AND bf-ar-ledger.ref-num EQ ttARLedgerTransaction.referenceDesc
            NO-ERROR.
        IF NOT AVAILABLE bf-ar-ledger THEN
        DO: 
            CREATE bf-ar-ledger.
            ASSIGN 
                bf-ar-ledger.company   = ttARLedgerTransaction.company
                bf-ar-ledger.cust-no   = ttARLedgerTransaction.customerID
                bf-ar-ledger.amt       = ttArLedgerTransaction.amount
                bf-ar-ledger.ref-num   = ttARLedgerTransaction.referenceDesc
                bf-ar-ledger.ref-date  = ttARLedgerTransaction.referenceDate
                bf-ar-ledger.tr-num    = ipiRun
                bf-ar-ledger.tr-date   = ttARLedgerTransaction.postDate
                bf-ar-ledger.curr-code = ttARLedgerTransaction.currencyCode
                bf-ar-ledger.ex-rate   = ttARLedgerTransaction.currencyExRate
                .
        END.
        ELSE 
            bf-ar-ledger.amt = bf-ar-ledger.amt + ttArLedgerTransaction.amount.
            
        DELETE ttARLedgerTransaction.
    END.
    
END PROCEDURE.

PROCEDURE pPostGL PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Posts all pending GL Transactions
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttPostingMaster FOR ttPostingMaster.
    DEFINE INPUT PARAMETER iplCreateGL AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iRunID              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dRunningBalance     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dAmount             AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE riGLTrans           AS ROWID     NO-UNDO.
    DEFINE VARIABLE cConsolidateMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cConsolidateAccount AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cConsolidateDesc    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTransactionType    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lConsolidate        AS LOGICAL   NO-UNDO.
    
    ASSIGN 
        iRunID                     = fGetNextRun(ttPostingMaster.company, iplCreateGL)
        ipbf-ttPostingMaster.runID = iRunID
        dRunningBalance            = 0
        cConsolidateMessage        = "ORDER ENTRY INVOICE"
        .
    
    
    ASSIGN 
        cTransactionType    = "LINE"
        lConsolidate        = ipbf-ttPostingMaster.consolidateLine
        cConsolidateAccount = ipbf-ttPostingMaster.accountARSales
        .  
    RUN pPostGLType(BUFFER ipbf-ttPostingMaster, iplCreateGL, cTransactionType, iRunID, 
        lConsolidate, cConsolidateAccount, cConsolidateMessage + " " + cTransactionType, INPUT-OUTPUT dRunningBalance).

    ASSIGN 
        cTransactionType    = "MISC"
        lConsolidate        = ipbf-ttPostingMaster.consolidateMisc
        cConsolidateAccount = ipbf-ttPostingMaster.accountARSales
        cConsolidateDesc    = cConsolidateMessage + " " + cTransactionType
        .  
    RUN pPostGLType(BUFFER ipbf-ttPostingMaster, iplCreateGL, cTransactionType, iRunID, 
        lConsolidate, cConsolidateAccount, cConsolidateMessage + " " + cTransactionType, INPUT-OUTPUT dRunningBalance).
    
    ASSIGN 
        cTransactionType    = "TAX"
        lConsolidate        = ipbf-ttPostingMaster.consolidateTax
        cConsolidateAccount = ipbf-ttPostingMaster.accountARSalesTax
        cConsolidateDesc    = cConsolidateMessage + " " + cTransactionType
        .  
    RUN pPostGLType(BUFFER ipbf-ttPostingMaster, iplCreateGL, cTransactionType, iRunID, 
        lConsolidate, cConsolidateAccount, cConsolidateMessage + " " + cTransactionType, INPUT-OUTPUT dRunningBalance).
            
    ASSIGN 
        cTransactionType    = "FG"
        lConsolidate        = ipbf-ttPostingMaster.consolidateFG
        cConsolidateAccount = ipbf-ttPostingMaster.accountFG
        cConsolidateDesc    = cConsolidateMessage + " " + cTransactionType
        .  
    RUN pPostGLType(BUFFER ipbf-ttPostingMaster, iplCreateGL, cTransactionType, iRunID, 
        lConsolidate, cConsolidateAccount, cConsolidateMessage + " " + cTransactionType, INPUT-OUTPUT dRunningBalance).    
    
    ASSIGN 
        cTransactionType    = "COGS"
        lConsolidate        = ipbf-ttPostingMaster.consolidateCogs
        cConsolidateAccount = ipbf-ttPostingMaster.accountCogs
        .  
    RUN pPostGLType(BUFFER ipbf-ttPostingMaster, iplCreateGL, cTransactionType, iRunID, 
        lConsolidate, cConsolidateAccount, cConsolidateMessage + " " + cTransactionType, INPUT-OUTPUT dRunningBalance).    
    
    ASSIGN 
        cTransactionType    = "FREIGHT"
        lConsolidate        = ipbf-ttPostingMaster.consolidateFreight
        cConsolidateAccount = ipbf-ttPostingMaster.accountARFreight
        .  
    RUN pPostGLType(BUFFER ipbf-ttPostingMaster, iplCreateGL, cTransactionType, iRunID, 
        lConsolidate, cConsolidateAccount, cConsolidateMessage + " " + cTransactionType, INPUT-OUTPUT dRunningBalance). 
        
    ASSIGN 
        cTransactionType    = "DISC"
        lConsolidate        = ipbf-ttPostingMaster.consolidateDisc
        cConsolidateAccount = ipbf-ttPostingMaster.accountARDiscount
        .  
    RUN pPostGLType(BUFFER ipbf-ttPostingMaster, iplCreateGL, cTransactionType, iRunID, 
        lConsolidate, cConsolidateAccount, cConsolidateMessage + " " + cTransactionType, INPUT-OUTPUT dRunningBalance).
    
    ASSIGN 
        cTransactionType    = "CASH"
        lConsolidate        = ipbf-ttPostingMaster.consolidateCash
        cConsolidateAccount = ipbf-ttPostingMaster.accountAR
        .  
    RUN pPostGLType(BUFFER ipbf-ttPostingMaster, iplCreateGL, cTransactionType, iRunID, 
        lConsolidate, cConsolidateAccount, cConsolidateMessage + " " + cTransactionType, INPUT-OUTPUT dRunningBalance).
    
    ASSIGN 
        cTransactionType    = "AR"
        lConsolidate        = ipbf-ttPostingMaster.consolidateAR
        cConsolidateAccount = ipbf-ttPostingMaster.accountAR
        .  
    RUN pPostGLType(BUFFER ipbf-ttPostingMaster, iplCreateGL, cTransactionType, iRunID, 
        lConsolidate, cConsolidateAccount, cConsolidateMessage + " " + cTransactionType, INPUT-OUTPUT dRunningBalance).
                
    IF dRunningBalance NE 0 THEN 
        ASSIGN 
            oplError   = YES
            opcMessage = "Run out of balance by " + STRING(dRunningBalance, ">>>,>>>,>>9.99")
            .
    
    IF NOT oplError AND iplCreateGL THEN 
        RUN pPostARLedger (iRunID).

END PROCEDURE.

PROCEDURE pPostGLAccumulateAndAdd PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a GL trans type, accumulate the total balance by invoice
     and create the gltrans
     Notes:  RUN pPostGLAccumulateAndAdd("work-line", iRunID, INPUT-OUTPUT dRunningBalance).
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttPostingMaster FOR ttPostingMaster.
    DEFINE INPUT PARAMETER ipcTransactionType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiRunID AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplCreateGL AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplConsolidateOnly AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdAmountConsolidated AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdRunningBalance AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dAmountToPost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE riGlTrans     AS ROWID   NO-UNDO.
     
    FOR EACH ttGLTransaction
        WHERE ttGLTransaction.transactionType EQ ipcTransactionType
        BREAK BY ttGLTransaction.account
        BY ttGLTransaction.invoiceID:
        
        ASSIGN 
            dAmountToPost      = dAmountToPost + ttGLTransaction.amount
            iopdRunningBalance = iopdRunningBalance + ttGLTransaction.amount
            .
        
        IF LAST-OF(ttGlTransaction.invoiceID) AND NOT iplConsolidateOnly THEN 
        DO:
            IF iplCreateGL THEN 
                RUN pCreateGLTransFromTransaction(BUFFER ipbf-ttPostingMaster, BUFFER ttGLTransaction, dAmountToPost, ipiRunID, OUTPUT riGLTrans).
            dAmountToPost = 0.
        END.                           
        
        IF iplCreateGL THEN DELETE ttGLTransaction.
    END. /* each ttGLTransaction */
    
    IF iplConsolidateOnly  THEN 
        opdAmountConsolidated = dAmountToPost.

END PROCEDURE.

PROCEDURE pPostAll PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes all ttInvoiceToPost for session.
        Transactions for updating the DB
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiCountPosted AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplExceptionsFound AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    
    FIND FIRST ttPostingMaster NO-ERROR.
    IF NOT AVAILABLE ttPostingMaster THEN 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Posting Master not available"
            .
        RETURN.
    END.
    
    RUN pPostGL(BUFFER ttPostingMaster, NO, OUTPUT oplError, OUTPUT opcMessage).
    
    IF oplError THEN RETURN.
    
    /*create ar-inv and ar-invl*/
    RUN pPostInvoices(OUTPUT opiCountPosted, OUTPUT oplError, OUTPUT opcMessage).
        
    /*Create GL Records*/
    RUN pPostGL(BUFFER ttPostingMaster, YES, OUTPUT oplError, OUTPUT opcMessage).       
    
    /*Update additional records*/
    RUN pUpdateOrders.
    RUN pUpdateBOLs.
    RUN pUpdateCustomers.
    RUN pUpdateEstPreps.
    RUN pUpdateFGItems.
    
    RUN pBuildExceptions(OUTPUT oplExceptionsFound).
    
/*REFACTOR - Add back AREXP and EDI processing*/
    
    
    
END PROCEDURE.

PROCEDURE pPostGLType PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Posts GLs for a specific type
     Notes:
     RUN pPostGLType(BUFFER ipbf-ttPostingMaster, iplCreateGL, ipcTransactionType, ipiRunID, 
        iplConsolidate, ipcConsolidateAccount, ipcConsolidateDesc, INPUT-OUTPUT iopdRunningBalance).
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttPostingMaster FOR ttPostingMaster.
    DEFINE INPUT PARAMETER iplCreateGL AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcTransactionType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiRunID AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplConsolidate AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcConsolidateAccount AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcConsolidateDesc AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdRunningBalance AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dAmount   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE riGLTrans AS ROWID   NO-UNDO.
    
    RUN pPostGLAccumulateAndAdd(BUFFER ipbf-ttPostingMaster, ipcTransactionType, ipiRunID, iplCreateGL, iplConsolidate, OUTPUT dAmount, INPUT-OUTPUT iopdRunningBalance).
    IF iplConsolidate AND dAmount NE 0 AND iplCreateGL THEN 
        RUN pCreateGLTrans(BUFFER ipbf-ttPostingMaster, dAmount, ipcConsolidateAccount, ipcConsolidateDesc, ipiRunID, 
            ipbf-ttPostingMaster.postDate, ipbf-ttPostingMaster.periodID, 
            ipbf-ttPostingMaster.currencyCode, ipbf-ttPostingMaster.currencyExRate, OUTPUT riGLTrans).

END PROCEDURE.

PROCEDURE pPostInvoices PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given the posting master, post all invoices,
     creating the ar-inv and ar-invl for valid invoices.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiCountPosted AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-inv-head       FOR inv-head.
    DEFINE BUFFER bf-inv-line       FOR inv-line.
    DEFINE BUFFER bf-inv-misc       FOR inv-misc.
    DEFINE BUFFER bf-ar-inv         FOR ar-inv.
    DEFINE BUFFER bf-ar-invl        FOR ar-invl.
    DEFINE BUFFER bf-child-inv-head FOR inv-head.
    
    DEFINE VARIABLE iLine    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iXno     AS INTEGER NO-UNDO.
    DEFINE VARIABLE riArInv  AS ROWID   NO-UNDO.
    DEFINE VARIABLE riArInvl AS ROWID   NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF bf-inv-head.
    DISABLE TRIGGERS FOR LOAD OF bf-inv-line.
    DISABLE TRIGGERS FOR LOAD OF bf-inv-misc.
    
    
    FOR EACH ttInvoiceToPost
        WHERE ttInvoiceToPost.isOKToPost,
        FIRST bf-inv-head EXCLUSIVE-LOCK 
        WHERE ROWID(bf-inv-head) EQ ttInvoiceToPost.riInvHead 
        :
        opiCountPosted = opiCountPosted + 1.
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
        FOR EACH ttInvoiceLineToPost
            WHERE ttInvoiceLineToPost.rNo EQ ttInvoiceToPost.rNo 
            AND ttInvoiceLineToPost.isOKToPost,
            FIRST bf-inv-line EXCLUSIVE-LOCK 
            WHERE ROWID(bf-inv-line) EQ ttInvoiceLineToPost.riInvLine:
                
            RUN pCreateARInvLIne(BUFFER bf-inv-head, BUFFER bf-inv-line, BUFFER ttInvoiceLineToPost, iXno, iLine, OUTPUT riArInvl).
            iLine = iLine + 1.
            
            DELETE bf-inv-line.
            DELETE ttInvoiceLineToPost.
            
        END. /*each invoice line*/
        FOR EACH ttInvoiceMiscToPost
            WHERE ttInvoiceMiscToPost.isOKToPost
            AND ttInvoiceMisctoPost.rNo EQ ttInvoicetoPost.rNo,
            FIRST bf-inv-misc EXCLUSIVE-LOCK
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
        
        //REFACTOR TEMPORARY REMOVAL - RUN pCreateEDI(BUFFER bf-inv-head).
        
        DELETE bf-inv-head.
        DELETE ttInvoiceToPost.
    END. /*each invoice to post*/

END PROCEDURE.

PROCEDURE pProcessInvoicesToPost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  For each InvoiceToPost, sum up totals for posting
     and create temp-tables for GL account creation
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiCountValid AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
     
    DEFINE BUFFER bf-inv-head FOR inv-head.
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER bf-inv-misc FOR inv-misc.
   
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
        opiCountValid = opiCountValid + 1. 
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
                ttInvoiceToPost.amountCost          = ttInvoiceToPost.amountCost + ttInvoiceLineToPost.costTotal
                ttInvoiceToPost.amountDiscount      = ttInvoiceToPost.amountDiscount + ttInvoiceLineToPost.amountDiscount
                ttInvoiceToPost.quantityTotalMSF    = ttInvoiceToPost.quantityTotalMSF + ttInvoiceLineToPost.quantityInvoicedMSF
                .

            IF FIRST(ttInvoiceLineToPost.orderID) THEN
                ASSIGN
                    ttInvoiceToPost.orderID   = bf-inv-line.ord-no
                    ttInvoiceToPost.orderDate = bf-inv-line.ord-date
                    .
            
            IF ttInvoiceLineToPost.amountBilledIncDiscount NE 0 THEN  
                RUN pAddGLTransaction(BUFFER ttPostingMaster, BUFFER ttInvoiceToPost, "LINE", ttInvoiceLineToPost.accountARSales, - ttInvoiceLineToPost.amountBilledIncDiscount, ttInvoiceLineToPost.itemID).               
            
            RUN pAddGLTransactionsForFG(BUFFER ttPostingMaster, BUFFER ttInvoiceLineToPost).
        
        END. /* each inv-line */

        FOR EACH ttInvoiceMiscToPost NO-LOCK
            WHERE ttInvoiceMiscToPost.rNo EQ ttInvoiceToPost.rNo 
            AND ttInvoiceMiscToPost.isOKToPost,
            FIRST bf-inv-misc
            WHERE ROWID(bf-inv-misc) EQ ttInvoiceMiscToPost.riInvMisc
            :
            
            IF ttInvoiceMiscToPost.isBillable AND ttInvoiceMiscToPost.amount NE 0 THEN 
                RUN pAddGLTransaction(BUFFER ttPostingMaster, BUFFER ttInvoiceToPost, "MISC", ttInvoiceMiscToPost.accountARSales, - ttInvoiceMiscToPost.amount, ttInvoiceMiscToPost.chargeID).
                
            ASSIGN 
                ttInvoiceToPost.amountCost       = ttInvoiceToPost.amountCost + ttInvoiceMiscToPost.costTotal
                ttInvoiceToPost.amountBilledMisc = ttInvoiceToPost.amountBilledMisc + ttInvoiceMiscToPost.amount
                .                    
           
        END. /* each inv-misc */

        
        IF ttInvoiceToPost.isInvoiceDateInCurrentPeriod THEN 
            ASSIGN 
                ttCustomerToUpdate.salesPTDExTax = ttCustomerToUpdate.salesPTDExTax + ttInvoiceToPost.amountBilledExTax
                ttCustomerToUpdate.salesPTD      = ttCustomerToUpdate.salesPTD + ttInvoiceToPost.amountBilledExTax
                ttCustomerToUpdate.costPTD       = ttCustomerToUpdate.costPTD + ttInvoiceToPost.amountCost
                ttCustomerToUpdate.commissionPTD = ttCustomerToUpdate.commissionPTD + ttInvoiceToPost.amountCommission
                ttCustomerToUpdate.msfPTD        = ttCustomerToUpdate.msfPTD + ttInvoiceToPost.quantityTotalMSF
                .
        ASSIGN 
            ttCustomerToUpdate.salesTotalExTax       = ttCustomerToUpdate.salesTotalExTax + ttInvoiceToPost.amountBilledExTax
            ttCustomerToUpdate.salesTotal            = ttCustomerToUpdate.salesTotal + ttInvoiceToPost.amountBilled
            ttCustomerToUpdate.costTotal             = ttCustomerToUpdate.costTotal + ttInvoiceToPost.amountCost 
            ttCustomerToUpdate.commissionTotal       = ttCustomerToUpdate.commissionTotal + ttInvoiceToPost.amountCommission
            ttCustomerToUpdate.orderBalanceReduction = ttCustomerToUpdate.orderBalanceReduction +  ttInvoiceToPost.amountBilled
            ttCustomerToUpdate.msfTotal              = ttCustomerToUpdate.msfTotal + ttInvoiceToPost.quantityTotalMSF
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
            RUN pAddGLTransaction(BUFFER ttPostingMaster, BUFFER ttInvoiceToPost, "DISC", ttInvoiceToPost.accountARDiscount, ttInvoiceToPost.amountDiscount, "").

        IF ttInvoiceToPost.amountBilledFreight NE 0 THEN 
            RUN pAddGLTransaction(BUFFER ttPostingMaster, BUFFER ttInvoiceToPost, "FREIGHT", ttInvoiceToPost.accountARFreight, - ttInvoiceToPost.amountBilledFreight, "").

        IF ttInvoiceToPost.amountBilledTax NE 0 THEN 
            RUN pAddGLTransaction(BUFFER ttPostingMaster, BUFFER ttInvoiceToPost, "TAX", ttInvoiceToPost.accountARSalesTax, - ttInvoiceToPost.amountBilledTax, "").
        
        IF ttInvoiceToPost.isCashTerms AND ttInvoiceToPost.amountBilled NE 0 THEN 
            RUN pAddGLTransaction(BUFFER ttPostingMaster, BUFFER ttInvoiceToPost, "CASH", ttInvoiceToPost.accountARCash, ttInvoiceToPost.amountBilled, "").
        ELSE 
            RUN pAddGLTransaction(BUFFER ttPostingMaster, BUFFER ttInvoiceToPost, "AR", ttInvoiceToPost.accountAR, ttInvoiceToPost.amountBilled, "").

        RUN pAddARLedgerTransaction (BUFFER ttInvoiceToPost).    
        
        
    END. /*Each ttInvoiceToPost*/    
    
/*REFACTOR - Need to process multi-currency - Manipulate GLTransactions and amounts and create CURR types for offsets*/

END PROCEDURE.

PROCEDURE pUpdateBOLs PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Updates db for BOLs in ttBOLLineToUpdate temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-oe-boll FOR oe-boll.
    DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
    
    DISABLE TRIGGERS FOR LOAD OF oe-boll.
    DISABLE TRIGGERS FOR LOAD OF oe-bolh.
    
    FOR EACH ttBOLlineToUpdate:
        FIND FIRST bf-oe-boll EXCLUSIVE-LOCK
            WHERE ROWID(bf-oe-boll) EQ ttBolLineToUpdate.riOeBoll
            NO-WAIT NO-ERROR.
        IF AVAILABLE bf-oe-boll THEN 
        DO:
            FIND FIRST bf-oe-bolh EXCLUSIVE-LOCK 
                WHERE ROWID(bf-oe-bolh) EQ ttBolLineToUpdate.riOeBolh
                NO-WAIT NO-ERROR.
            IF AVAILABLE bf-oe-bolh THEN 
            DO:
                ASSIGN 
                    bf-oe-boll.inv-no = ttBolLineToUpdate.invoiceID
                    bf-oe-bolh.inv-no = ttBolLineToUpdate.invoiceID
                    .
                /*Update loadtag.sts to "Completed" - May be unnecessary*/
    
                DELETE ttBOLlineToUpdate.
        
            END.  /*avail bf-oebolh to write*/
        
        END. /*avail bf-oe-boll to write*/
        
    END. /*each ttBOLLIneTOUpdatE*/

END PROCEDURE.

PROCEDURE pBuildExceptions PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Builds Exception List for invoices and peripheral tables
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplExceptionsExist AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cKey AS CHARACTER NO-UNDO.
    
    FOR EACH ttInvoiceToPost:
        ASSIGN 
            oplExceptionsExist = YES
            cKey               = "Inv: " + STRING(ttInvoiceToPost.invoiceID,">>>>>>>9").
        RUN pAddException(ttInvoiceToPost.riInvHead, ttInvoiceToPost.problemMessage,
            ttInvoiceToPost.company, "inv-head","Invoice Header", cKey,
            "", "", "").
    END.
    FOR EACH ttGLTransaction:
        ASSIGN 
            oplExceptionsExist = YES
            cKey               = "Acct: " + ttGLTransaction.account + " Trans: " + ttGLTransaction.transactionDesc.
        RUN pAddException(?, "Unable to create GL Transaction",
            ttGlTransaction.company, "gltrans","GL Transaction", cKey,
            "tr-amt", "Amount", STRING(ttGLTransaction.amount,"->>>>>>>>>9.99")).
    END.
    FOR EACH ttCustomerToUpdate:
        ASSIGN 
            oplExceptionsExist = YES
            cKey               = "Cust: " + ttCustomerToUpdate.customerID.
        RUN pAddException(ttCustomerToUpdate.riCust, "Unable to update - locked",
            ttCustomerToUpdate.company, "cust","Customer", cKey,
            "", "", "").
    END.
    FOR EACH ttFGItemToUpdate:
        ASSIGN 
            oplExceptionsExist = YES
            cKey               = "Item: " + ttFGItemToUpdate.itemID.
        RUN pAddException(ttFGItemToUpdate.riItemfg, "Unable to update - locked",
            ttFGItemToUpdate.company, "itemfg","FG Item", cKey,
            "", "", "").
    END.
    FOR EACH ttOrderLineToUpdate:
        ASSIGN 
            oplExceptionsExist = YES
            cKey               = "Order: " + STRING(ttOrderLineToUpdate.orderID) + " Line: " + STRING(ttOrderLineToUpdate.orderLine) + " Item: " + ttOrderLineToUpdate.itemID.
        RUN pAddException(ttOrderLineToUpdate.riOeOrdl, "Unable to update - locked",
            ttOrderLineToUpdate.company, "oe-ordl","Order Line", cKey,
            "", "", "").
    END.
    FOR EACH ttOrderMiscToUpdate:
        ASSIGN 
            oplExceptionsExist = YES
            cKey               = "Order: " + STRING(ttOrderMiscToUpdate.orderID) + " Line: " + STRING(ttOrderMiscToUpdate.orderLine) + " Charge: " + ttOrderMiscToUpdate.itemID.
        RUN pAddException(ttOrderMiscToUpdate.riOeOrdm, "Unable to update - locked",
            ttOrderMiscToUpdate.company, "oe-ordm","Order Misc", cKey,
            "", "", "").
    END.
    FOR EACH ttBOLLineToUpdate:
        ASSIGN 
            oplExceptionsExist = YES
            cKey               = "BOL: " + STRING(ttBOLLineToUpdate.bolID) + " Item: " + ttBOLLineToUpdate.itemID.
        RUN pAddException(ttBOLLineToUpdate.riOeBoll, "Unable to update - locked",
            ttBOLLineToUpdate.company, "oe-boll","BOL Line", cKey,
            "", "", "").
    END.

END PROCEDURE.

PROCEDURE pUpdateCustomers PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Updates db for customers in ttCustomerToUpdate temp-table
     Notes: 
     
     Replaces oe/invcust.p
    ------------------------------------------------------------------------------*/   
    DEFINE BUFFER bf-cust FOR cust.  
    
    DEFINE VARIABLE iPeriod AS INTEGER NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF cust.
    
    FOR EACH ttCustomerToUpdate:
        
        FIND FIRST bf-cust EXCLUSIVE-LOCK
            WHERE ROWID(bf-cust) EQ ttCustomerToUpdate.riCust
            NO-WAIT NO-ERROR.
        IF AVAILABLE bf-cust THEN 
        DO:
            ASSIGN
                iPeriod                  = ttCustomerToUpdate.periodID 
                bf-cust.sales[iPeriod]   = bf-cust.sales[iPeriod] + ttCustomerToUpdate.salesPTDExTax
                bf-cust.ptd-msf[iPeriod] = bf-cust.ptd-msf[iPeriod] + ttCustomerToUpdate.msfPTD   
                bf-cust.cost[1]          = bf-cust.cost[1] + ttCustomerToUpdate.costPTD       /* PTD */
                bf-cust.comm[1]          = bf-cust.comm[1] + ttCustomerToUpdate.commissionPTD /* PTD */
               
                bf-cust.ord-bal          = bf-cust.ord-bal - ttCustomerToUpdate.orderBalanceReduction
                bf-cust.ytd-sales        = bf-cust.ytd-sales + ttCustomerToUpdate.salesTotalExTax
                bf-cust.cost[5]          = bf-cust.cost[5] + ttCustomerToUpdate.costTotal
                bf-cust.comm[5]          = bf-cust.comm[5] + ttCustomerToUpdate.commissionTotal
                bf-cust.acc-bal          = bf-cust.acc-bal + ttCustomerToUpdate.accountBalanceIncrease
                bf-cust.ytd-msf          = bf-cust.ytd-msf + ttCustomerToUpdate.msfTotal
                .
        
            IF ttCustomerToUpdate.lastPayDate NE ? THEN 
                ASSIGN 
                    bf-cust.lpay      = ttCustomerToUpdate.lastPayAmount
                    bf-cust.lpay-date = ttCustomerToUpdate.lastPayDate
                    .

            IF bf-cust.acc-bal GE bf-cust.hibal THEN 
                ASSIGN 
                    bf-cust.hibal      = bf-cust.acc-bal
                    bf-cust.hibal-date = ttCustomerToUpdate.lastInvoiceDate
                    .
                
            DELETE ttCustomerToUpdate.
        
        END.  /*Avail bf-cust to write to*/
        
    END. /*EachttCustomerToUpdate*/
   
END PROCEDURE.

PROCEDURE pUpdateEstPreps PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-est-prep FOR est-prep.

    DISABLE TRIGGERS FOR LOAD OF est-prep.
    
    FOR EACH ttEstPrepToUpdate:
        
        FIND FIRST bf-est-prep EXCLUSIVE-LOCK 
            WHERE ROWID(bf-est-prep) EQ ttEstPrepToUpdate.riEstPrep
            NO-WAIT NO-ERROR.
        IF AVAILABLE bf-est-prep THEN 
        DO:    
            IF ttEstPrepToUpdate.deleteEstPrep THEN 
                DELETE bf-est-prep.
            ELSE 
                bf-est-prep.simon  = ttEstPrepToUpdate.newSimon.
            
            DELETE ttEstPrepToUpdate.
        
        END. /*avail bf-est-prep to write*/
    
    END. /*Each ttEstPrepToUpdate*/

END PROCEDURE.

PROCEDURE pUpdateFGItems PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Updates DB for items from ttFGItemsToUpdate temp-table.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-itemfg FOR itemfg.

    DEFINE VARIABLE iPeriod AS INTEGER NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF itemfg.


    FOR EACH ttFGItemToUpdate:
        
        FIND FIRST bf-itemfg EXCLUSIVE-LOCK
            WHERE ROWID(bf-itemfg) EQ ttFGItemToUpdate.riItemfg
            NO-WAIT NO-ERROR.
        IF AVAILABLE bf-itemfg THEN 
        DO:            
            ASSIGN
                iPeriod                    = ttFGItemToUpdate.periodID 
                bf-itemfg.ptd-msf[iPeriod] = bf-itemfg.ptd-msf[iPeriod] + ttFGItemToUpdate.quantityInvoicedMSFPTD
                bf-itemfg.q-inv-ptd        = bf-itemfg.q-inv-ptd + ttFGItemToUpdate.quantityInvoicedPTD
                bf-itemfg.q-ship-ptd       = bf-itemfg.q-ship-ptd + ttFGItemToUpdate.quantityShippedPTD
                bf-itemfg.q-alloc-ptd      = bf-itemfg.q-alloc-ptd + ttFGItemToUpdate.quantityShippedPTD
            
                bf-itemfg.ytd-msf          = bf-itemfg.ytd-msf + ttFGItemToUpdate.quantityInvoicedMSFTotal
                bf-itemfg.q-inv            = bf-itemfg.q-inv + ttFGItemToUpdate.quantityInvoicedTotal
                bf-itemfg.q-ship           = bf-itemfg.q-ship + ttFGItemToUpdate.quantityShippedTotal
                bf-itemfg.q-alloc          = bf-itemfg.q-alloc + ttFGItemToUpdate.quantityShippedTotal
                .
            
            DELETE ttFGItemToUpdate.
            
        END. /*avail bf-itemfg to write*/
        
    END. /*each ttFGItemToUpdate*/    

END PROCEDURE.

PROCEDURE pUpdateOrders PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Updates DB for order lines and orders given ttOrderLineToUpdate temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-oe-ordm FOR oe-ordm.
    DEFINE BUFFER bf-oe-ord  FOR oe-ord.
    
    DEFINE VARIABLE cStatus      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReason      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFullyClosed AS LOGICAL   NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF oe-ordl.
    DISABLE TRIGGERS FOR LOAD OF oe-ordm.
    
    FOR EACH ttOrderLineToUpdate:
        FIND FIRST bf-oe-ordl EXCLUSIVE-LOCK 
            WHERE ROWID(bf-oe-ordl) EQ ttOrderLineToUpdate.riOeOrdl
            NO-WAIT NO-ERROR.
        IF AVAILABLE bf-oe-ordl THEN 
        DO:
            ASSIGN 
                bf-oe-ordl.t-inv-qty  = bf-oe-ordl.t-inv-qty + ttOrderLineToUpdate.newQuantityInvoiced
                bf-oe-ordl.t-ship-qty = bf-oe-ordl.t-ship-qty + ttOrderLineToUpdate.newQuantityShipped
                . 
            
            DELETE ttOrderLineToUpdate.
            
        END. /*avail bf-oe-ordl to write*/
        
    END. /*Each ttOrderlineToUpdate*/
    
    FOR EACH ttOrderMiscToUpdate:
        FIND FIRST bf-oe-ordm EXCLUSIVE-LOCK 
            WHERE ROWID(bf-oe-ordm) EQ ttOrderMiscToUpdate.riOeOrdm
            NO-WAIT NO-ERROR.
        
        IF AVAILABLE bf-oe-ordm THEN 
        DO:
            IF bf-oe-ordm.bill EQ "P" THEN 
                bf-oe-ordm.bill = ttOrderMiscToUpdate.billStatus.
            
            DELETE ttOrderMiscToUpdate.
        
        END. /*avail bf-oe-ordm to write*/
        
    END. /*each ttOrderMiscToUpdate*/
    
    /*REFACTOR - This could be major source of slowness*/
    FOR EACH ttOrderToUpdate,
        FIRST bf-oe-ord NO-LOCK
        WHERE ROWID(bf-oe-ord) EQ ttOrderToUpdate.riOeOrd:
        lFullyClosed = YES.
        FOR EACH bf-oe-ordl EXCLUSIVE-LOCK 
            WHERE bf-oe-ordl.company EQ bf-oe-ord.company 
            AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no 
            AND bf-oe-ordl.stat    NE "C":
            /* No UI */
            RUN oe/CloseOrder.p(INPUT ROWID(bf-oe-ordl),
                INPUT NO,
                OUTPUT cStatus,
                OUTPUT cReason).
            /* No UI */
            IF cStatus EQ 'C' THEN
                RUN oe/closelin.p (INPUT ROWID(oe-ordl),YES).
            ELSE 
                lFullyClosed = NO.
        END.
        IF lFullyClosed THEN 
            RUN oe\close.p(RECID(bf-oe-ord), YES).
    END. /* Each w-ord */            
    
    
END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fGetFilePath RETURNS CHARACTER PRIVATE
    ( ipcFolder AS CHARACTER, ipcFileBase AS CHARACTER, ipcFileUnique AS CHARACTER, ipcFileExt AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  Given inputs, validate folder and build new file name.  Return complete path.
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cFullFilePath AS CHARACTER NO-UNDO.

    IF ipcFolder EQ "" THEN 
        RUN FileSys_GetTempDirectory (OUTPUT ipcFolder).
    
    cFullFilePath = ipcFolder + "\" + ipcFileBase + ipcFileUnique + "." + ipcFileExt.
    
    RETURN cFullFilePath.
    		
END FUNCTION.

FUNCTION fGetGLTransactionHandle RETURNS HANDLE 
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the handle to the ttInvoiceToPost
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN TEMP-TABLE ttGLTransaction:HANDLE.
		
END FUNCTION.

FUNCTION fGetInvoiceLineToPostHandle RETURNS HANDLE 
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the handle to the ttInvoiceLineToPost
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN TEMP-TABLE ttInvoiceLineToPost:HANDLE.
		
END FUNCTION.

FUNCTION fGetInvoiceToPostHandle RETURNS HANDLE 
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the handle to the ttInvoiceToPost
     Notes:
    ------------------------------------------------------------------------------*/	
    RETURN TEMP-TABLE ttInvoiceToPost:HANDLE.
		
END FUNCTION.

FUNCTION fGetNextRun RETURNS INTEGER PRIVATE
    ( ipcCompany AS CHARACTER, iplUpdateControl AS LOGICAL):
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
                    iRun = bf-gl-ctrl.trnum + 1.
                IF iplUpdateControl THEN  
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

