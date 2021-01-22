
/*------------------------------------------------------------------------
    File        : PostInvoice.i
    Purpose     : 

    Syntax      :

    Description : Include File for all temp-tables for PostInvoice.p		

    Author(s)   : 
BV
    Created     : Thu Nov 19 15:45:04 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttPostingMaster NO-UNDO 
    FIELD company               AS CHARACTER
    FIELD blockZeroCost         AS LOGICAL
    FIELD deleteEstPrep         AS LOGICAL
    FIELD invoiceStart          AS INTEGER 
    FIELD invoiceEnd            AS INTEGER 
    FIELD invoiceDateStart      AS DATE 
    FIELD invoiceDateEnd        AS DATE 
    FIELD customerIDStart       AS CHARACTER 
    FIELD customerIDEnd         AS CHARACTER
    FIELD postDate              AS DATE
    FIELD periodID              AS INTEGER 
    FIELD periodDateStart       AS DATE
    FIELD periodDateEnd         AS DATE 
    FIELD accountAR             AS CHARACTER
    FIELD accountARFreight      AS CHARACTER
    FIELD accountARSales        AS CHARACTER
    FIELD accountARSalesTax     AS CHARACTER
    FIELD accountARDiscount     AS CHARACTER
    FIELD accountARCash         AS CHARACTER
    FIELD accountARCurrency     AS CHARACTER
    FIELD accountCOGS           AS CHARACTER
    FIELD accountFG             AS CHARACTER 
    FIELD journalNote           AS CHARACTER
    FIELD consolidateOnAR       AS CHARACTER
    FIELD consolidateOnFG       AS CHARACTER
    FIELD consolidateOnCOGS     AS CHARACTER
    FIELD consolidateOnLine     AS CHARACTER
    FIELD consolidateOnMisc     AS CHARACTER
    FIELD consolidateOnTax      AS CHARACTER
    FIELD consolidateOnDisc     AS CHARACTER
    FIELD consolidateOnFreight  AS CHARACTER
    FIELD consolidateOnCash     AS CHARACTER
    FIELD consolidateOnCurrency AS CHARACTER 
    FIELD currencyCode          AS CHARACTER 
    FIELD currencyExRate        AS DECIMAL
    FIELD exportPath            AS CHARACTER
    FIELD runID                 AS INTEGER
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
    FIELD customerName                 AS CHARACTER 
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
    FIELD termsCode                    AS CHARACTER
    FIELD taxGroup                     AS CHARACTER
    FIELD runID                        AS INTEGER
    FIELD estID                        AS CHARACTER
    .
    
DEFINE TEMP-TABLE ttInvoiceLineToPost NO-UNDO 
    FIELD riInvLine               AS ROWID
    FIELD company                 AS CHARACTER LABEL "Company" FORMAT "x(3)"
    FIELD invoiceID               AS INTEGER   LABEL "Invoice #" FORMAT ">>>>>>9"
    FIELD invoiceLine             AS INTEGER   LABEL "Line" FORMAT ">>>9"
    FIELD invoiceDate             AS DATE      LABEL "Invoice Date" FORMAT 99/99/9999
    FIELD customerID              AS CHARACTER LABEL "Cust ID" FORMAT "x(10)"
    FIELD customerName            AS CHARACTER LABEL "Cust Name" FORMAT "x(30)"
    FIELD itemID                  AS CHARACTER LABEL "Item ID" FORMAT "x(15)"
    FIELD itemName                AS CHARACTER LABEL "Item Name" FORMAT "x(30)"
    FIELD quantityOrdered         AS DECIMAL   LABEL "Ordered Qty" FORMAT ">>,>>>,>>9"
    FIELD quantityShipped         AS DECIMAL   LABEL "Shipped Qty" FORMAT ">>>,>>>,>>9"
    FIELD quantityInvoiced        AS DECIMAL   LABEL "Invoiced Qty" FORMAT ">>>,>>>,>>9"
    FIELD orderID                 AS INTEGER   LABEL "Order ID" FORMAT ">>>>>>9"
    FIELD customerPO              AS CHARACTER LABEL "Customer PO" FORMAT "x(20)"
    FIELD customerLot             AS CHARACTER LABEL "Customer Lot" FORMAT "x(20)"
    FIELD salesGroup              AS CHARACTER LABEL "Sales Group" FORMAT "x(5)"
    FIELD salesGroupName          AS CHARACTER LABEL "Sales Group Name" FORMAT "x(30)"
    FIELD postDate                AS DATE      LABEL "Post Date" FORMAT 99/99/9999                                                                   
    FIELD runID                   AS INTEGER   LABEL "Run" FORMAT ">>>>>>>>9"
    FIELD isMisc                  AS LOGICAL   LABEL "Misc"
    FIELD rNo                     AS INTEGER 
    FIELD rNoOld                  AS INTEGER
    FIELD isOKToPost              AS LOGICAL   LABEL "OK To Post"
    FIELD problemMessage          AS CHARACTER LABEL "Problem Desc" FORMAT "x(40)"
    FIELD pricePerUOM             AS DECIMAL   LABEL "Price Per UOM" FORMAT ">>>,>>>9.99"
    FIELD priceUOM                AS CHARACTER LABEL "Price UOM" FORMAT "x(4)"
    FIELD costPerUOM              AS DECIMAL   LABEL "Cost Per UOM" FORMAT ">>,>>>,>>9.99"
    FIELD costUOM                 AS CHARACTER LABEL "Cost UOM" FORMAT "x(4)"
    FIELD costTotal               AS DECIMAL   LABEL "Cost Total" FORMAT ">>,>>>,>>9.99"
    FIELD costDirectLabor         AS DECIMAL   LABEL "Cost Direct Labor" FORMAT ">>,>>>,>>9.99"
    FIELD costFixedOverhead       AS DECIMAL   LABEL "Cost Fixed Overhead" FORMAT ">>,>>>,>>9.99"
    FIELD costVariableOverhead    AS DECIMAL   LABEL "Cost Variable Overhead" FORMAT ">>,>>>,>>9.99"
    FIELD costDirectMaterial      AS DECIMAL   LABEL "Cost Direct Material" FORMAT ">>,>>>,>>9.99"
    FIELD costSource              AS CHARACTER LABEL "Cost Source" FORMAT "x(30)"
    FIELD costStdFreight          AS DECIMAL   LABEL "Cost Std Freight" FORMAT ">>,>>>,>>9.99"
    FIELD costStdWarehouse        AS DECIMAL   LABEL "Cost Std Warehouse" FORMAT ">>,>>>,>>9.99"
    FIELD costStdDeviation        AS DECIMAL   LABEL "Cost Std Deviation" FORMAT ">>,>>>,>>9.99"
    FIELD costStdManufacture      AS DECIMAL   LABEL "Cost Std Manufacture" FORMAT ">>,>>>,>>9.99"
    FIELD costFull                AS DECIMAL   LABEL "Cost Full" FORMAT ">>,>>>,>>9.99"
    FIELD quantityInvoicedWeight  AS DECIMAL   LABEL "Invoiced Weight" FORMAT ">>,>>>,>>9.99"
    FIELD quantityInvoicedMSF     AS DECIMAL   LABEL "Invoiced MSF" FORMAT ">>,>>>,>>9.99"
    FIELD weightUOM               AS CHARACTER LABEL "Weight UOM" FORMAT "x(4)"
    FIELD accountAR               AS CHARACTER LABEL "AR Account" FORMAT "x(20)"
    FIELD accountARFreight        AS CHARACTER LABEL "Freight Account" FORMAT "x(20)"
    FIELD accountARSales          AS CHARACTER LABEL "Sales Account" FORMAT "x(20)"
    FIELD accountARSalesTax       AS CHARACTER LABEL "Sales Tax Account" FORMAT "x(20)"
    FIELD accountARDiscount       AS CHARACTER LABEL "Discount Account" FORMAT "x(20)"
    FIELD accountARCash           AS CHARACTER LABEL "Cash Account" FORMAT "x(20)"
    FIELD accountDLCogs           AS CHARACTER LABEL "COGS DL Account" FORMAT "x(20)"
    FIELD accountDLFG             AS CHARACTER LABEL "FG DL Account" FORMAT "x(20)"
    FIELD accountVOCogs           AS CHARACTER LABEL "COGS VO Account" FORMAT "x(20)"
    FIELD accountVOFG             AS CHARACTER LABEL "FG VO Account" FORMAT "x(20)"
    FIELD accountFOCogs           AS CHARACTER LABEL "COGS FO Account" FORMAT "x(20)"
    FIELD accountFOFG             AS CHARACTER LABEL "FG FOAR Account" FORMAT "x(20)"
    FIELD accountDMCogs           AS CHARACTER LABEL "COGS DM Account" FORMAT "x(20)"
    FIELD accountDMFG             AS CHARACTER LABEL "FG DM Account" FORMAT "x(20)"
    FIELD quantityPerSubUnit      AS DECIMAL   LABEL "Case Count" FORMAT ">>,>>9"
    FIELD amountDiscount          AS DECIMAL   LABEL "Discount" FORMAT ">>,>>>,>>9.99"
    FIELD amountBilled            AS DECIMAL   LABEL "Billed" FORMAT ">>,>>>,>>9.99"
    FIELD amountBilledIncDiscount AS DECIMAL   LABEL "Billed Inc. Discount" FORMAT ">>,>>>,>>9.99"
    FIELD amountCommission        AS DECIMAL   LABEL "Commission" FORMAT ">>,>>>,>>9.99"
    FIELD locationID              AS CHARACTER LABEL "Location" FORMAT ">>,>>>,>>9.99"
    FIELD bolID                   AS INTEGER   LABEL "BOL" FORMAT ">>,>>>,>>9.99"
    FIELD squareFeetPerEA         AS DECIMAL   LABEL "Sq Ft Per EA" FORMAT ">>,>>>,>>9.99"
    FIELD productCategory         AS CHARACTER LABEL "Product Category" FORMAT "x(8)"
    FIELD currencyCode            AS CHARACTER LABEL "Currency" FORMAT "x(8)"
    FIELD currencyExRate          AS DECIMAL   LABEL "Exchange Rate" FORMAT ">>>,>>>9.99" 
    FIELD periodID                AS INTEGER   LABEL "Period" FORMAT ">9"
    FIELD isTaxable               AS LOGICAL   LABEL "Taxable"
    FIELD shipID                  AS CHARACTER LABEL "Ship To" FORMAT "x(10)"
    FIELD termsCode               AS CHARACTER LABEL "Terms" FORMAT "x(5)"
    FIELD isFreightBillable       AS LOGICAL   LABEL "Bill Freight"
    .    
    
DEFINE TEMP-TABLE ttInvoiceMiscToPost NO-UNDO 
    LIKE ttInvoiceLineToPost
    FIELD riInvMisc  AS ROWID
    FIELD isBillable AS LOGICAL 
    FIELD chargeID   AS CHARACTER 
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
    FIELD riOeOrd  AS ROWID 
    FIELD company  AS CHARACTER 
    FIELD orderID  AS INTEGER
    FIELD isClosed AS LOGICAL 
    FIELD reOeOrd  AS RECID 
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

DEFINE TEMP-TABLE rpt NO-UNDO 
    LIKE ttInvoiceLineToPost
    .
    
DEFINE TEMP-TABLE ttInvoiceError NO-UNDO 
    FIELD riInvError     AS ROWID
    FIELD invoiceID      AS INTEGER 
    FIELD problemMessage AS CHARACTER
    FIELD isOKToPost     AS LOGICAL 
    .  

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
