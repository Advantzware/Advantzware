
/*------------------------------------------------------------------------
    File        : ImportAP1.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for AP Invoices 

    Author(s)   : Vishnu Vellanki
    Created     : Thursday Sep 26 16:18:38 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
DEFINE TEMP-TABLE ttImportAP1
    FIELD Company           AS CHARACTER FORMAT "x(3)"
    FIELD Location          AS CHARACTER 
    FIELD VendorID          AS CHARACTER FORMAT "x(10)"           COLUMN-LABEL "Vendor ID"          HELP "Required - Must be valid - Size:10"
    FIELD VendorName        AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "Vendor Name"        HELP "Optional - Character"
    FIELD LinePONumber1     AS INTEGER   FORMAT ">>>>>9"          COLUMN-LABEL "PO #1"              HELP "Optional - INTEGER"
    FIELD LinePONumber2     AS INTEGER   FORMAT ">>>>>9"          COLUMN-LABEL "PO #2"              HELP "Optional - INTEGER"
    FIELD LinePONumber3     AS INTEGER   FORMAT ">>>>>9"          COLUMN-LABEL "PO #3"              HELP "Optional - INTEGER"
    FIELD LinePONumber4     AS INTEGER   FORMAT ">>>>>9"          COLUMN-LABEL "PO #4"              HELP "Optional - INTEGER"
    FIELD LinePONumber5     AS INTEGER   FORMAT ">>>>>9"          COLUMN-LABEL "PO #5"              HELP "Optional - INTEGER"
    FIELD LinePONumber6     AS INTEGER   FORMAT ">>>>>9"          COLUMN-LABEL "PO #6"              HELP "Optional - INTEGER"
    FIELD LinePONumber7     AS INTEGER   FORMAT ">>>>>9"          COLUMN-LABEL "PO #7"              HELP "Optional - INTEGER"
    FIELD LinePONumber8     AS INTEGER   FORMAT ">>>>>9"          COLUMN-LABEL "PO #8"              HELP "Optional - INTEGER"
    FIELD LinePONumber9     AS INTEGER   FORMAT ">>>>>9"          COLUMN-LABEL "PO #9"              HELP "Optional - INTEGER"
    FIELD LinePONumber10    AS INTEGER   FORMAT ">>>>>9"          COLUMN-LABEL "PO #10"             HELP "Optional - INTEGER"
    FIELD LinePONumber11    AS INTEGER   FORMAT ">>>>>9"          COLUMN-LABEL "PO #11"             HELP "Optional - INTEGER"
    FIELD LinePONumber12    AS INTEGER   FORMAT ">>>>>9"          COLUMN-LABEL "PO #12"             HELP "Optional - INTEGER"
    FIELD LinePONumber13    AS INTEGER   FORMAT ">>>>>9"          COLUMN-LABEL "PO #13"             HELP "Optional - INTEGER"
    FIELD LinePONumber14    AS INTEGER   FORMAT ">>>>>9"          COLUMN-LABEL "PO #14"             HELP "Optional - INTEGER"
    FIELD LinePONumber15    AS INTEGER   FORMAT ">>>>>9"          COLUMN-LABEL "PO #15"             HELP "Optional - INTEGER"
    FIELD InvoiceNo         AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "Invoice #"          HELP "Required - Size:20"
    FIELD InvoiceDate       AS DATE      FORMAT "99/99/99"        COLUMN-LABEL "Inv Date"           HELP "Defaults to Today - Date"
    FIELD TotalAmount       AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "$ Total Amount"     HELP "Optional - Decimal"
    FIELD Tax               AS CHARACTER FORMAT "X(20)"           COLUMN-LABEL "Tax #"              HELP "Optional - Size:20"
    FIELD VendorTerms       AS CHARACTER FORMAT "x(8)"            COLUMN-LABEL "Vendor Terms"       HELP "Optional - Character"
    FIELD APClerkAssigned   AS CHARACTER FORMAT "x(8)"            COLUMN-LABEL "AP Clerk Assigned"  HELP "Optional - Character"
    FIELD LineAccount1      AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "GL Accnt #1"        HELP "Defaults to Vendor GL Account - Size:20"
    FIELD LineQuantity1     AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Quantity1"          HELP "Defaults to 1 - Decimal"
    FIELD LinePrice1        AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Price1"             HELP "Defaults to Line Amount - Decimal"
    FIELD POLine1           AS INTEGER   FORMAT ">9"              COLUMN-LABEL "PO Line 1"          HELP "Defaults to 1 - Integer"
    FIELD Linedescription1  AS CHARACTER FORMAT "x(35)"           COLUMN-LABEL "Notes1"             HELP "Optional - Character"
    FIELD LineAccount2      AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "GL Accnt #2"        HELP "Defaults to Vendor GL Account - Size:20"
    FIELD LineQuantity2     AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Quantity 2"         HELP "Defaults to 1 - Decimal"
    FIELD LinePrice2        AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Price 2"            HELP "Defaults to Line Amount - Decimal"
    FIELD POLine2           AS INTEGER   FORMAT ">9"              COLUMN-LABEL "PO Line 2"          HELP "Defaults to 1 - Integer"
    FIELD Linedescription2  AS CHARACTER FORMAT "x(35)"           COLUMN-LABEL "Notes2"             HELP "Optional - Character"
    FIELD LineAccount3      AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "GL Accnt #3"        HELP "Defaults to Vendor GL Account - Size:20"
    FIELD LineQuantity3     AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Quantity 3"         HELP "Defaults to 1 - Decimal"
    FIELD LinePrice3        AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Price 3"            HELP "Defaults to Line Amount - Decimal"
    FIELD POLine3           AS INTEGER   FORMAT ">9"              COLUMN-LABEL "PO Line 3"          HELP "Defaults to 1 - Integer"
    FIELD Linedescription3  AS CHARACTER FORMAT "x(35)"           COLUMN-LABEL "Notes3"             HELP "Optional - Character"
    FIELD LineAccount4      AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "GL Accnt #4"        HELP "Defaults to Vendor GL Account - Size:20"
    FIELD LineQuantity4     AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Quantity 4"         HELP "Defaults to 1 - Decimal"
    FIELD LinePrice4        AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Price 4"            HELP "Defaults to Line Amount - Decimal"
    FIELD POLine4           AS INTEGER   FORMAT ">9"              COLUMN-LABEL "PO Line 4"          HELP "Defaults to 1 - Integer"
    FIELD Linedescription4  AS CHARACTER FORMAT "x(35)"           COLUMN-LABEL "Notes4"             HELP "Optional - Character"
    FIELD LineAccount5      AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "GL Accnt #5"        HELP "Defaults to Vendor GL Account - Size:20"
    FIELD LineQuantity5     AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Quantity5"          HELP "Defaults to 1 - Decimal"
    FIELD LinePrice5        AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Price5"             HELP "Defaults to Line Amount - Decimal"
    FIELD POLine5           AS INTEGER   FORMAT ">9"              COLUMN-LABEL "PO Line 5"          HELP "Defaults to 1 - Integer"
    FIELD Linedescription5  AS CHARACTER FORMAT "x(35)"           COLUMN-LABEL "Notes5"             HELP "Optional - Character"
    FIELD LineAccount6      AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "GL Accnt #6"        HELP "Defaults to Vendor GL Account - Size:20"
    FIELD LineQuantity6     AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Quantity 6"         HELP "Defaults to 1 - Decimal"
    FIELD LinePrice6        AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Price 6"            HELP "Defaults to Line Amount - Decimal"
    FIELD POLine6           AS INTEGER   FORMAT ">9"              COLUMN-LABEL "PO Line 6"          HELP "Defaults to 1 - Integer"
    FIELD Linedescription6  AS CHARACTER FORMAT "x(35)"           COLUMN-LABEL "Notes6"             HELP "Optional - Character"
    FIELD LineAccount7      AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "GL Accnt #7"        HELP "Defaults to Vendor GL Account - Size:20"
    FIELD LineQuantity7     AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Quantity 7"         HELP "Defaults to 1 - Decimal"
    FIELD LinePrice7        AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Price 7"            HELP "Defaults to Line Amount - Decimal"
    FIELD POLine7           AS INTEGER   FORMAT ">9"              COLUMN-LABEL "PO Line 7"          HELP "Defaults to 1 - Integer"
    FIELD Linedescription7  AS CHARACTER FORMAT "x(35)"           COLUMN-LABEL "Notes7"             HELP "Optional - Character"
    FIELD LineAccount8      AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "GL Accnt #8"        HELP "Defaults to Vendor GL Account - Size:20"
    FIELD LineQuantity8     AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Quantity 8"         HELP "Defaults to 1 - Decimal"
    FIELD LinePrice8        AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Price 8"            HELP "Defaults to Line Amount - Decimal"
    FIELD POLine8           AS INTEGER   FORMAT ">9"              COLUMN-LABEL "PO Line 8"          HELP "Defaults to 1 - Integer"
    FIELD Linedescription8  AS CHARACTER FORMAT "x(35)"           COLUMN-LABEL "Notes8"             HELP "Optional - Character" 
    FIELD LineAccount9      AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "GL Accnt #9"        HELP "Defaults to Vendor GL Account - Size:20"
    FIELD LineQuantity9     AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Quantity9"          HELP "Defaults to 1 - Decimal"
    FIELD LinePrice9        AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Price9"             HELP "Defaults to Line Amount - Decimal"
    FIELD POLine9           AS INTEGER   FORMAT ">9"              COLUMN-LABEL "PO Line 9"          HELP "Defaults to 1 - Integer"
    FIELD Linedescription9  AS CHARACTER FORMAT "x(35)"           COLUMN-LABEL "Notes9"             HELP "Optional - Character"
    FIELD LineAccount10     AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "GL Accnt #10"       HELP "Defaults to Vendor GL Account - Size:20"
    FIELD LineQuantity10    AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Quantity 10"        HELP "Defaults to 1 - Decimal"
    FIELD LinePrice10       AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Price 10"           HELP "Defaults to Line Amount - Decimal"
    FIELD POLine10          AS INTEGER   FORMAT ">9"              COLUMN-LABEL "PO Line 10"         HELP "Defaults to 1 - Integer"
    FIELD Linedescription10 AS CHARACTER FORMAT "x(35)"           COLUMN-LABEL "Notes10"            HELP "Optional - Character"
    FIELD LineAccount11     AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "GL Accnt #11"       HELP "Defaults to Vendor GL Account - Size:20"
    FIELD LineQuantity11    AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Quantity 11"        HELP "Defaults to 1 - Decimal"
    FIELD LinePrice11       AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Price 11"           HELP "Defaults to Line Amount - Decimal"
    FIELD POLine11          AS INTEGER   FORMAT ">9"              COLUMN-LABEL "PO Line 11"         HELP "Defaults to 1 - Integer"
    FIELD Linedescription11 AS CHARACTER FORMAT "x(35)"           COLUMN-LABEL "Notes11"            HELP "Optional - Character"
    FIELD LineAccount12     AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "GL Accnt #12"       HELP "Defaults to Vendor GL Account - Size:20"
    FIELD LineQuantity12    AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Quantity 12"        HELP "Defaults to 1 - Decimal"
    FIELD LinePrice12       AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Price 12"           HELP "Defaults to Line Amount - Decimal"
    FIELD POLine12          AS INTEGER   FORMAT ">9"              COLUMN-LABEL "PO Line 12"         HELP "Defaults to 1 - Integer"
    FIELD Linedescription12 AS CHARACTER FORMAT "x(35)"           COLUMN-LABEL "Notes12"            HELP "Optional - Character"
    FIELD LineAccount13     AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "GL Accnt #13"       HELP "Defaults to Vendor GL Account - Size:20"
    FIELD LineQuantity13    AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Quantity13"         HELP "Defaults to 1 - Decimal"
    FIELD LinePrice13       AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Price13"            HELP "Defaults to Line Amount - Decimal"
    FIELD POLine13          AS INTEGER   FORMAT ">9"              COLUMN-LABEL "PO Line 13"         HELP "Defaults to 1 - Integer"
    FIELD Linedescription13 AS CHARACTER FORMAT "x(35)"           COLUMN-LABEL "Notes14"            HELP "Optional - Character"
    FIELD LineAccount14     AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "GL Accnt #14"       HELP "Defaults to Vendor GL Account - Size:20"
    FIELD LineQuantity14    AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Quantity 14"        HELP "Defaults to 1 - Decimal"
    FIELD LinePrice14       AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Price 14"           HELP "Defaults to Line Amount - Decimal"
    FIELD POLine14          AS INTEGER   FORMAT ">9"              COLUMN-LABEL "PO Line 14"         HELP "Defaults to 1 - Integer"
    FIELD Linedescription14 AS CHARACTER FORMAT "x(35)"           COLUMN-LABEL "Notes14"            HELP "Optional - Character"
    FIELD LineAccount15     AS CHARACTER FORMAT "x(20)"           COLUMN-LABEL "GL Accnt #15"       HELP "Defaults to Vendor GL Account - Size:20"
    FIELD LineQuantity15    AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Quantity 15"        HELP "Defaults to 1 - Decimal"
    FIELD LinePrice15       AS DECIMAL   FORMAT "->>>,>>>,>>>.99" COLUMN-LABEL "Price 15"           HELP "Defaults to Line Amount - Decimal"
    FIELD POLine15          AS INTEGER   FORMAT ">9"              COLUMN-LABEL "PO Line 15"         HELP "Defaults to 1 - Integer"
    FIELD Linedescription15 AS CHARACTER FORMAT "x(35)"           COLUMN-LABEL "Notes15"            HELP "Optional - Character"
    .
    

DEFINE VARIABLE giIndexOffset    AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/
DEFINE VARIABLE hdTagProcs       AS HANDLE  NO-UNDO.
DEFINE VARIABLE dTotalInvoiceAmt AS DECIMAL NO-UNDO.
DEFINE VARIABLE lInvalid         AS LOGICAL NO-UNDO.
DEFINE VARIABLE hdAPInvoiceProcs AS HANDLE  NO-UNDO.

RUN system/TagProcs.p PERSISTENT SET hdTagProcs.
RUN ap/APInvoiceProcs.p PERSISTENT SET hdAPInvoiceProcs.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
 /*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportAP1"}

PROCEDURE GetSummaryMessage:
/*------------------------------------------------------------------------------
 Purpose: To return the total invoice amount
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcSummaryMessage AS CHARACTER NO-UNDO.
    
    opcSummaryMessage = "Total Invoice Amount = " + STRING(dTotalInvoiceAmt).
          
END PROCEDURE.

/* FG Item Receipt quantity and price validation */

PROCEDURE pValidateAccountNum PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcActNum  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplInvalid AS LOGICAL   NO-UNDO.
    
    FIND FIRST account NO-LOCK 
         WHERE account.company EQ ipcCompany
           AND account.actnum  EQ ipcActNum  
         NO-ERROR.
    IF AVAILABLE account THEN 
        oplInvalid = account.inactive.
    ELSE 
        oplInvalid = YES.
END PROCEDURE.

PROCEDURE pValidateFGItemReceiptQtyPrice:
    DEFINE INPUT        PARAMETER ipriAPInvl   AS ROWID     NO-UNDO.
    DEFINE INPUT        PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiPoNo      AS INTEGER   NO-UNDO. 
    DEFINE INPUT        PARAMETER ipiPoLine    AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipdQuantity  AS INTEGER   NO-UNDO. 
    DEFINE INPUT        PARAMETER ipdPrice     AS INTEGER   NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER ioplHold     AS LOGICAL   NO-UNDO.    
    DEFINE INPUT-OUTPUT PARAMETER iopcHoldNote AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dQuantity         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lQtyMatch         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lPriceMatch       AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCostFromHistory  AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE lRecordsFound               AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lHasNegativeReceipts        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dQuantityAvailableToInvoice AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError                      AS LOGICAL   NO-UNDO.
    
    FIND FIRST po-ord NO-LOCK
         WHERE po-ord.company EQ ipcCompany
           AND po-ord.po-no   EQ ipiPoNo
         NO-ERROR.
    
    IF NOT AVAILABLE po-ord THEN
        RETURN.
    
    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company EQ ipcCompany
           AND po-ordl.po-no   EQ po-ord.po-no
           AND po-ordl.line    EQ ipiPoLine
         NO-ERROR.
    IF NOT AVAILABLE po-ordl THEN
        RETURN.
    
    FIND FIRST ap-invl NO-LOCK
         WHERE ROWID(ap-invl) EQ ipriAPInvl NO-ERROR.
    IF NOT AVAILABLE ap-invl THEN
        RETURN.
 
    RUN APInvoice_GetReceiptsQtyAvailable IN hdAPInvoiceProcs (
        INPUT  ap-invl.rec_key,
        INPUT  ap-invl.cons-uom,
        OUTPUT lRecordsFound,
        OUTPUT lHasNegativeReceipts,
        OUTPUT dQuantityAvailableToInvoice
        ).
                 
    FOR FIRST fg-rcpth NO-LOCK
       WHERE fg-rcpth.company   EQ ipcCompany
         AND fg-rcpth.i-no      EQ po-ordl.i-no
         AND fg-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
         AND fg-rcpth.rita-code EQ "R"
       USE-INDEX item-po,
       FIRST fg-rdtlh EXCLUSIVE-LOCK
       WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
         AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
         AND NOT CAN-FIND(FIRST POReceiptLink 
                          WHERE POReceiptLink.inventoryStockRecKey EQ fg-rdtlh.rec_key):
        dCostFromHistory = fg-rdtlh.cost.
    END.
    
    IF dQuantityAvailableToInvoice EQ ipdQuantity THEN DO:
        IF dCostFromHistory EQ ipdPrice THEN
            lPriceMatch = TRUE.
        ELSE
            lPriceMatch = FALSE.
            
        lQtyMatch = TRUE.
        
        RUN APInvoice_AutoSelectReceipts IN hdAPInvoiceProcs (
            INPUT  ap-invl.rec_key,
            INPUT  ap-invl.qty,
            INPUT  ap-invl.cons-uom,
            OUTPUT lError,
            OUTPUT cMessage
            ).
    END.
    IF NOT lQtyMatch THEN
        ASSIGN
            ioplHold    = YES
            cMessage    = "PO # " + STRING(ipiPoNo)  + 
                          "-" + STRING(po-ordl.line) + " Quantity (" + STRING(ipdQuantity) + 
                          ") does not match"
            iopcHoldNote = IF iopcHoldNote EQ '' THEN
                              cMessage
                          ELSE
                              iopcHoldNote + "|" + cMessage
            .
        
    IF NOT lPriceMatch THEN
        ASSIGN
            ioplHold     = YES
            cMessage     = "PO # " + STRING(ipiPoNo) + 
                           "-" + STRING(po-ordl.line) + " Price (" + STRING(ipdPrice)  + 
                           ") does not match"
            iopcHoldNote = IF iopcHoldNote EQ '' THEN
                               cMessage
                           ELSE
                               iopcHoldNote + "|" + cMessage
            .

    IF lHasNegativeReceipts THEN
        ASSIGN
            ioplHold     = YES
            cMessage     = "PO # " + STRING(ipiPoNo) + 
                           "-" + STRING(po-ordl.line) + " Has negative receipts"
            iopcHoldNote = IF iopcHoldNote EQ '' THEN
                               cMessage
                           ELSE
                               iopcHoldNote + "|" + cMessage
            . 
    
END PROCEDURE.

/* RM Item Receipt quantity and price validation */
PROCEDURE pValidateRMItemReceiptQtyPrice:
    DEFINE INPUT        PARAMETER ipriAPInvl   AS ROWID     NO-UNDO.
    DEFINE INPUT        PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiPoNo      AS INTEGER   NO-UNDO. 
    DEFINE INPUT        PARAMETER ipiPoLine    AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipdQuantity  AS DECIMAL   NO-UNDO. 
    DEFINE INPUT        PARAMETER ipdPrice     AS DECIMAL   NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER ioplHold     AS LOGICAL   NO-UNDO.    
    DEFINE INPUT-OUTPUT PARAMETER iopcHoldNote AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dQuantity   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lQtyMatch   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lPriceMatch AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lRecordsFound               AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lHasNegativeReceipts        AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dQuantityAvailableToInvoice AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lError                      AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dCostFromHistory            AS DECIMAL NO-UNDO.
    
    FIND FIRST po-ord NO-LOCK
         WHERE po-ord.company EQ ipcCompany
           AND po-ord.po-no   EQ ipiPoNo
         NO-ERROR.
    
    IF NOT AVAILABLE po-ord THEN
        RETURN.
    
    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company EQ ipcCompany
           AND po-ordl.po-no   EQ po-ord.po-no
           AND po-ordl.line    EQ ipiPoLine
         NO-ERROR.
    IF NOT AVAILABLE po-ordl THEN
        RETURN.
    
    FIND FIRST ap-invl NO-LOCK
         WHERE ROWID(ap-invl) EQ ipriAPInvl NO-ERROR.
    IF NOT AVAILABLE ap-invl THEN
        RETURN.
        
    RUN APInvoice_GetReceiptsQtyAvailable IN hdAPInvoiceProcs (
        INPUT  ap-invl.rec_key,
        INPUT  ap-invl.cons-uom,
        OUTPUT lRecordsFound,
        OUTPUT lHasNegativeReceipts,
        OUTPUT dQuantityAvailableToInvoice
        ).
                 
    FOR FIRST rm-rcpth NO-LOCK
       WHERE rm-rcpth.company   EQ ipcCompany
         AND rm-rcpth.i-no      EQ po-ordl.i-no
         AND rm-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
         AND rm-rcpth.rita-code EQ "R"
       USE-INDEX item-po,
       FIRST rm-rdtlh EXCLUSIVE-LOCK
       WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
         AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
         AND NOT CAN-FIND(FIRST POReceiptLink 
                          WHERE POReceiptLink.inventoryStockRecKey EQ rm-rdtlh.rec_key):
        dCostFromHistory = rm-rdtlh.cost.
    END.
    
    IF dQuantityAvailableToInvoice EQ ipdQuantity THEN DO:
        IF dCostFromHistory EQ ipdPrice THEN
            lPriceMatch = TRUE.
        ELSE
            lPriceMatch = FALSE.
            
        lQtyMatch = TRUE.
        
        RUN APInvoice_AutoSelectReceipts IN hdAPInvoiceProcs (
            INPUT  ap-invl.rec_key,
            INPUT  ap-invl.qty,
            INPUT  ap-invl.cons-uom,
            OUTPUT lError,
            OUTPUT cMessage
            ).
    END.
    IF NOT lQtyMatch THEN
        ASSIGN
            ioplHold    = YES
            cMessage    = "PO # " + STRING(ipiPoNo)  + 
                          "-" + STRING(po-ordl.line,"999") + ", Quantity " + STRING(ipdQuantity) + 
                          " does not match"
            iopcHoldNote = IF iopcHoldNote EQ '' THEN
                              cMessage
                          ELSE
                             iopcHoldNote + "|" + cMessage
            .
        
    IF NOT lPriceMatch THEN
        ASSIGN
            ioplHold     = YES
            cMessage     = "PO # " + STRING(ipiPoNo) + 
                           "-" + STRING(po-ordl.line,"999") + ", Price " + STRING(ipdPrice)  + 
                           " does not match"
            iopcHoldNote = IF iopcHoldNote EQ '' THEN
                              cMessage
                          ELSE
                             iopcHoldNote + "|" + cMessage
            .
            
    IF lHasNegativeReceipts THEN
        ASSIGN
            ioplHold     = YES
            cMessage     = "PO # " + STRING(ipiPoNo) + 
                           "-" + STRING(po-ordl.line) + " Has negative receipts"
            iopcHoldNote = IF iopcHoldNote EQ '' THEN
                               cMessage
                           ELSE
                               iopcHoldNote + "|" + cMessage
            .
    
END PROCEDURE.

/* Validates PO Number and Account Number */
PROCEDURE pValidatePoAccountNumber:
    DEFINE INPUT        PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiPoNo        AS INTEGER   NO-UNDO. 
    DEFINE INPUT        PARAMETER ipiPoLine      AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiLineAccount AS CHARACTER NO-UNDO. 
    DEFINE INPUT        PARAMETER ipdQuantity    AS DECIMAL   NO-UNDO. 
    DEFINE INPUT        PARAMETER ipdPrice       AS DECIMAL   NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER ioplHold       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplItemType    AS LOGICAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcHoldNote   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
        
    FIND FIRST po-ord NO-LOCK
         WHERE po-ord.company EQ ipcCompany
           AND po-ord.po-no   EQ ipiPONo
         NO-ERROR.
    IF NOT AVAILABLE po-ord THEN DO:
        ASSIGN
            ioplHold      = YES            
            cMessage     = "PO # (" + STRING(ipiPoNo) + 
                           ") is not available"
            iopcHoldNote  = IF iopcHoldNote EQ '' THEN
                               cMessage
                           ELSE
                               iopcHoldNote + "|" + cMessage
            .

        RETURN.            
    END.
                
    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company EQ po-ord.company
           AND po-ordl.po-no   EQ po-ord.po-no
           AND po-ordl.line    EQ ipiPoLine
         NO-ERROR.    
    IF AVAILABLE po-ordl THEN DO:       
        oplItemType = po-ordl.item-type.
        IF po-ordl.actnum NE ipiLineAccount THEN DO:
            ASSIGN
                ioplHold     = YES
                cMessage     = "PO # " + STRING(ipiPoNo) + 
                               ", Account Number " + ipiLineAccount  + 
                               " does not match"
                iopcHoldNote = IF iopcHoldNote EQ '' THEN
                                   cMessage
                               ELSE
                                   iopcHoldNote + "|" + cMessage
                .
        END.
    END.
    ELSE DO:
        ASSIGN
            ioplHold     = YES
            cMessage     = "PO # (" + STRING(ipiPoNo) + 
                           "), Line # (" + STRING(ipiPOLine) + 
                           ") does not match"
            iopcHoldNote = IF iopcHoldNote EQ '' THEN
                               cMessage
                           ELSE
                               iopcHoldNote + "|" + cMessage
            .
        RETURN.
    END.
END PROCEDURE.

/* Creates Invoicelines */
PROCEDURE pCreateInvoiceLine:
    DEFINE INPUT  PARAMETER ipriapinv          AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoNo            AS INTEGER   NO-UNDO. 
    DEFINE INPUT  PARAMETER ipiPoLine          AS INTEGER   NO-UNDO. 
    DEFINE INPUT  PARAMETER ipcLineAccount     AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER ipdQuantity        AS DECIMAL   NO-UNDO. 
    DEFINE INPUT  PARAMETER ipdPrice           AS DECIMAL   NO-UNDO. 
    DEFINE INPUT  PARAMETER ipcLineDescrption  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdTotalAmount     AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opriAPInvl         AS ROWID     NO-UNDO.
    
    DEFINE BUFFER bf-ap-invl FOR ap-invl.

    FIND FIRST bf-ap-invl EXCLUSIVE-LOCK 
         WHERE bf-ap-invl.company EQ ap-inv.company
           AND bf-ap-invl.i-no    EQ ap-inv.i-no
           AND bf-ap-invl.po-no   EQ ipiPONo
           AND bf-ap-invl.po-line EQ ipiPoLine
         NO-ERROR.

    IF NOT AVAILABLE bf-ap-invl THEN DO:
        RUN pCreateNewInvoiceLine (
            INPUT  ipriapinv, 
            INPUT  ipiPoNo,
            INPUT  ipiPoLine,
            OUTPUT opriAPInvl
            ).
            
        FIND bf-ap-invl EXCLUSIVE-LOCK 
            WHERE ROWID(bf-ap-invl) EQ opriAPInvl
            NO-ERROR.
    END.        
    IF NOT AVAILABLE bf-ap-invl THEN
        RETURN ERROR.
    ASSIGN
        bf-ap-invl.amt     = ipdTotalAmount 
        bf-ap-invl.qty     = ipdQuantity
        bf-ap-invl.unit-pr = ipdPrice
        bf-ap-invl.po-no   = ipiPoNo       
        bf-ap-invl.po-line = ipiPOLine
        bf-ap-invl.actnum  = ipcLineAccount
        bf-ap-invl.dscr    = ipcLineDescrption
        .

    RELEASE bf-ap-invl.    
END PROCEDURE.

/* Validates Key Fields */
PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportAP1 FOR ttImportAP1.
    
    DEFINE INPUT  PARAMETER iplUpdateDuplicates AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplFieldValidation  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid            AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote             AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cPOLineDetails  AS CHARACTER NO-UNDO EXTENT 15.
    DEFINE VARIABLE lInvLineAvail   AS LOGICAL   NO-UNDO EXTENT 15.      
    DEFINE VARIABLE iIndex          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lDuplicateLines AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-ttImportAP1 FOR ttImportAP1.
     
    
    oplValid = YES.
    
    RUN pFetchInvoiceDetails (
        INPUT TEMP-TABLE ttImportAP1:HANDLE,
        OUTPUT cPOLineDetails
        ).
  
    IF ipbf-ttImportAP1.Company EQ '' AND oplValid THEN DO:
        ASSIGN 
            oplValid = NO
            opcNote  = "Key Field Blank: Company"
            .        
    END.
 
    IF ipbf-ttImportAP1.VendorID EQ '' AND oplValid THEN DO:
        ASSIGN 
            oplValid = NO
            opcNote  = "Key Field Blank: VendorID"
            .        
    END.
    
    IF ipbf-ttImportAP1.InvoiceNo EQ '' AND oplValid THEN DO:
        ASSIGN 
            oplValid = NO
            opcNote  = "Key Field Blank: Invoice#"
            .       
    END.
    
    IF ipbf-ttImportAP1.InvoiceDate EQ ? AND oplValid THEN DO:
        ASSIGN 
            oplValid = NO
            opcNote  = "Key Field Blank: InvoiceDate#"
            .       
    END.
    
    FIND FIRST vend NO-LOCK 
         WHERE vend.company EQ ipbf-ttImportAP1.Company
           AND vend.vend-no EQ ipbf-ttImportAP1.VendorID
         NO-ERROR. 
    IF NOT AVAILABLE vend AND oplValid THEN DO:
        ASSIGN 
            oplValid = NO 
            opcNote  = "Key Field Invalid: VendorID"
            .       
    END.

    FIND FIRST bf-ttImportAP1 NO-LOCK 
        WHERE bf-ttImportAP1.Company        EQ ipbf-ttImportAP1.Company
          AND bf-ttImportAP1.VendorID       EQ ipbf-ttImportAP1.VendorID
          AND bf-ttImportAP1.InvoiceNo      EQ ipbf-ttImportAP1.InvoiceNo
          AND bf-ttImportAP1.LinePONumber1  EQ ipbf-ttImportAP1.LinePONumber1
          AND bf-ttImportAP1.LineAccount1   EQ ipbf-ttImportAP1.LineAccount1
          AND bf-ttImportAP1.LinePONumber2  EQ ipbf-ttImportAP1.LinePONumber2
          AND bf-ttImportAP1.LineAccount2   EQ ipbf-ttImportAP1.LineAccount2
          AND bf-ttImportAP1.LinePONumber3  EQ ipbf-ttImportAP1.LinePONumber3
          AND bf-ttImportAP1.LineAccount3   EQ ipbf-ttImportAP1.LineAccount3
          AND bf-ttImportAP1.LinePONumber4  EQ ipbf-ttImportAP1.LinePONumber4
          AND bf-ttImportAP1.LineAccount4   EQ ipbf-ttImportAP1.LineAccount4
          AND bf-ttImportAP1.LinePONumber5  EQ ipbf-ttImportAP1.LinePONumber5
          AND bf-ttImportAP1.LineAccount5   EQ ipbf-ttImportAP1.LineAccount5
          AND bf-ttImportAP1.LinePONumber6  EQ ipbf-ttImportAP1.LinePONumber6
          AND bf-ttImportAP1.LineAccount6   EQ ipbf-ttImportAP1.LineAccount6
          AND bf-ttImportAP1.LinePONumber7  EQ ipbf-ttImportAP1.LinePONumber7
          AND bf-ttImportAP1.LineAccount7   EQ ipbf-ttImportAP1.LineAccount7
          AND bf-ttImportAP1.LinePONumber8  EQ ipbf-ttImportAP1.LinePONumber8
          AND bf-ttImportAP1.LineAccount8   EQ ipbf-ttImportAP1.LineAccount8
          AND bf-ttImportAP1.LinePONumber9  EQ ipbf-ttImportAP1.LinePONumber9
          AND bf-ttImportAP1.LineAccount9   EQ ipbf-ttImportAP1.LineAccount9
          AND bf-ttImportAP1.LinePONumber10 EQ ipbf-ttImportAP1.LinePONumber10
          AND bf-ttImportAP1.LineAccount10  EQ ipbf-ttImportAP1.LineAccount10
          AND bf-ttImportAP1.LinePONumber11 EQ ipbf-ttImportAP1.LinePONumber11
          AND bf-ttImportAP1.LineAccount11  EQ ipbf-ttImportAP1.LineAccount11
          AND bf-ttImportAP1.LinePONumber12 EQ ipbf-ttImportAP1.LinePONumber12
          AND bf-ttImportAP1.LineAccount12  EQ ipbf-ttImportAP1.LineAccount12
          AND bf-ttImportAP1.LinePONumber13 EQ ipbf-ttImportAP1.LinePONumber13
          AND bf-ttImportAP1.LineAccount13  EQ ipbf-ttImportAP1.LineAccount13
          AND bf-ttImportAP1.LinePONumber14 EQ ipbf-ttImportAP1.LinePONumber14
          AND bf-ttImportAP1.LineAccount14  EQ ipbf-ttImportAP1.LineAccount14
          AND bf-ttImportAP1.LinePONumber15 EQ ipbf-ttImportAP1.LinePONumber15
          AND bf-ttImportAP1.LineAccount15  EQ ipbf-ttImportAP1.LineAccount15
          AND ROWID(bf-ttImportAP1)         NE ROWID(ipbf-ttImportAP1)
        NO-ERROR.        
    IF AVAILABLE bf-ttImportAP1 AND oplValid THEN DO:
        ASSIGN 
            oplValid = NO 
            opcNote  = "Duplicate Record in Import File"
            .       
    END.

    ipbf-ttImportAP1.InvoiceNo = TRIM(ipbf-ttImportAP1.InvoiceNo).

    FIND FIRST ap-inv NO-LOCK 
        WHERE ap-inv.company EQ ipbf-ttImportAP1.Company
          AND ap-inv.vend-no EQ ipbf-ttImportAP1.VendorID
          AND ap-inv.inv-no  EQ ipbf-ttImportAP1.InvoiceNo
        NO-ERROR.
    IF NOT AVAILABLE ap-inv AND oplValid THEN DO:   
        ASSIGN 
            opcNote = "Add record"
            .
    END.   
    /* Additional validation to check if PO lines are existing in invoice.
       If atleast one PO line is not existing in invoice then create invoice line */ 
    DO iIndex = 1 TO EXTENT(cPOLineDetails):
        IF cPOLineDetails[iIndex] NE "" AND oplValid THEN DO:
            RUN pValidateAccountNum(
                INPUT ipbf-ttImportAP1.Company,
                INPUT ENTRY(2,cPOLineDetails[iIndex]),
                OUTPUT lInvalid
                ).
            IF lInvalid THEN DO:
                ASSIGN 
                    opcNote = "Invalid GL account No. " + STRING(iIndex)
                    oplValid = FALSE
                    .                    
                LEAVE.            
            END.
          
            IF AVAILABLE ap-inv THEN DO:
                FIND FIRST ap-invl NO-LOCK 
                    WHERE ap-invl.company EQ ap-inv.company
                      AND ap-invl.i-no    EQ ap-inv.i-no
                      AND ap-invl.po-no   EQ INTEGER(ENTRY(1,cPOLineDetails[iIndex]))
                      AND ap-invl.po-line EQ iIndex
                    NO-ERROR.
                IF AVAILABLE ap-invl THEN 
                DO:
                    IF NOT iplUpdateDuplicates THEN 
                        ASSIGN
                            lDuplicateLines       = IF iIndex GT 1 THEN
                                                        lInvLineAvail[iIndex - 1] AND lDuplicateLines
                                                    ELSE
                                                        TRUE
                            lInvLineAvail[iIndex] = TRUE 
                            opcNote               = "Add record - Duplicate Line(s) Exists in Invoice"
                            .
                    ELSE
                        ASSIGN 
                            opcNote = "Update record - All fields to be overwritten"
                            .
                END.
                ELSE  
                    ASSIGN
                        lInvLineAvail[iIndex] = FALSE 
                        opcNote               = IF iIndex GT 1 AND lDuplicateLines THEN
                                                    "Add record - Duplicate Line(s) Exists in Invoice"
                                                ELSE
                                                    "Add record - New Line to Existing Invoice"
                        lDuplicateLines       = FALSE
                        .
            END.
        END.
    END.

    IF AVAILABLE ap-inv AND oplValid AND ap-inv.posted THEN    
        ASSIGN 
            opcNote = "Posted invoice - Will be skipped"
            oplValid = FALSE
             .
     
    IF lDuplicateLines AND oplValid THEN DO:
        ASSIGN
            oplValid = FALSE
            opcNote  = "Duplicate Lines - Will be skipped"
            .
       
    END.
    IF oplValid THEN DO:
        IF AVAILABLE ap-inv THEN DO: 
            IF iplUpdateDuplicates THEN 
                opcNote = "Update record - All fields to be overwritten".
            ELSE 
                ASSIGN 
                    opcNote = "Duplicate Lines - Will be Skipped" 
                    oplValid = FALSE
                    .   
        END.        
        ELSE  
            ASSIGN
                opcNote = "Add record"
                .         
    END.          
END PROCEDURE.

/* Creates New Invoice */
PROCEDURE pCreateNewInvoice:
    /*------------------------------------------------------------------------------
     Purpose: Creates a new AP invoice, setting defaults based on key values
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER.
    DEFINE INPUT  PARAMETER ipcVendor   AS CHARACTER.
    DEFINE INPUT  PARAMETER ipcInvoice  AS CHARACTER.
    DEFINE INPUT  PARAMETER ipdtInvDate AS DATE.
    DEFINE OUTPUT PARAMETER opriAPInv   AS ROWID.
    
    DEFINE BUFFER bf-ap-inv FOR ap-inv.
    
    FIND FIRST bf-ap-inv EXCLUSIVE-LOCK
         WHERE bf-ap-inv.company EQ ipcCompany
           AND bf-ap-inv.vend-no EQ ipcVendor
           AND bf-ap-inv.inv-no  EQ ipcInvoice
         NO-ERROR.
    IF NOT AVAILABLE bf-ap-inv THEN DO:    
        CREATE bf-ap-inv.
        ASSIGN
            bf-ap-inv.company  = ipcCompany
            bf-ap-inv.inv-no   = ipcInvoice
            bf-ap-inv.inv-date = TODAY
            bf-ap-inv.vend-no  = ipcVendor
            .
    END.        
    IF ipdtInvDate NE ? THEN 
        bf-ap-inv.inv-date = DATE(ipdtInvDate).  
                           
    FIND FIRST vend NO-LOCK 
         WHERE vend.company EQ ipcCompany
           AND vend.vend-no EQ ipcVendor
         NO-ERROR.
    IF AVAILABLE vend THEN 
    DO:
        ASSIGN
            bf-ap-inv.disc-%    = vend.disc-%
            bf-ap-inv.disc-days = vend.disc-days
            bf-ap-inv.tax-gr    = vend.tax-gr
            .  
        
        FIND FIRST currency NO-LOCK  
             WHERE currency.company EQ ipcCompany
               AND currency.c-code  EQ vend.curr-code
             NO-ERROR.
        FIND FIRST company NO-LOCK 
            WHERE company.company EQ ipcCompany
            NO-ERROR.
        IF NOT AVAILABLE currency AND AVAILABLE company THEN 
            FIND FIRST currency NO-LOCK 
                 WHERE currency.company EQ ipcCompany
                   AND currency.c-code  EQ company.curr-code
                 NO-ERROR.
        IF AVAILABLE currency THEN
            ASSIGN 
                bf-ap-inv.ex-rate      = currency.ex-rate
                bf-ap-inv.curr-code[1] = currency.c-code
                .
        ELSE 
            ASSIGN 
                bf-ap-inv.ex-rate      = 1
                bf-ap-inv.curr-code[1] = 'USD'
                .                        
        
        FIND FIRST terms WHERE terms.t-code EQ vend.terms NO-LOCK NO-ERROR.
        IF AVAILABLE terms THEN
            ASSIGN
                bf-ap-inv.disc-%    = terms.disc-rate
                bf-ap-inv.disc-days = terms.disc-days
                .
        bf-ap-inv.due-date = IF AVAILABLE terms THEN terms.net-day + bf-ap-inv.inv-date
        ELSE bf-ap-inv.inv-date.
                                                   
    END.
    opriAPInv = ROWID(bf-ap-inv).
    RELEASE bf-ap-inv.


END PROCEDURE.

/* Creates InvoiceLines */
PROCEDURE pCreateNewInvoiceLine:
    /*------------------------------------------------------------------------------
        Purpose: Creates a new AP invoice line, setting defaults based on key values
        Notes:
       ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriAPInv  AS ROWID.
    DEFINE INPUT  PARAMETER ipiPONo    AS INTEGER.
    DEFINE INPUT  PARAMETER ipiPOLine  AS INTEGER.
    DEFINE OUTPUT PARAMETER opriAPInvl AS ROWID.
    
    DEFINE VARIABLE cAccount AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ap-invl FOR ap-invl.
    
    FIND ap-inv NO-LOCK 
        WHERE ROWID(ap-inv) EQ ipriAPInv
        NO-ERROR.
 
    IF NOT AVAILABLE ap-inv THEN RETURN.
    
    FIND FIRST ap-ctrl NO-LOCK
         WHERE ap-ctrl.company EQ ap-inv.company
         NO-ERROR.
    
    FIND FIRST vend NO-LOCK 
        WHERE vend.company   EQ ap-inv.company
          AND vend.vend-no   EQ ap-inv.vend-no
        NO-ERROR.
    
    IF NOT AVAILABLE vend THEN RETURN.
    
    IF vend.actnum NE "" THEN
        cAccount = vend.actnum.
    ELSE IF AVAILABLE ap-ctrl THEN
        cAccount = ap-ctrl.purchases.     

    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company EQ ap-inv.company
           AND po-ordl.po-no   EQ ipiPONo
           AND po-ordl.line    EQ ipiPOLine
         NO-ERROR.
               
    CREATE ap-invl.
    ASSIGN
        ap-invl.i-no       = ap-inv.i-no
        ap-invl.company    = ap-inv.company
        ap-invl.vend-no    = ap-inv.vend-no
        ap-invl.line       = (ipiPONo * 1000) + ipiPOLine
        ap-invl.actnum     = cAccount
        ap-invl.loc        = ap-inv.loc
        ap-invl.period     = ap-inv.period
        ap-invl.posted     = ap-inv.posted
        ap-invl.cons-uom   = "EA"
        ap-invl.pr-qty-uom = "EA"
        ap-invl.tax        = ap-inv.tax-gr NE ""
        .

    IF AVAILABLE po-ordl THEN
        ap-invl.item-type = po-ordl.item-type.
        
    opriAPInvl = ROWID(ap-invl).
    RELEASE ap-invl.
    
END PROCEDURE.

/* Processes a loaded record */
PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportAP1 FOR ttImportAP1.
   
    DEFINE INPUT        PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded      AS INTEGER NO-UNDO.

    DEFINE VARIABLE riAPInv    AS ROWID     NO-UNDO. 
    DEFINE VARIABLE riAPInvl   AS ROWID     NO-UNDO. 
    DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHoldNote  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lHold      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iIndex     AS INTEGER   NO-UNDO.

    DEFINE VARIABLE cPOLineDetails   AS CHARACTER NO-UNDO EXTENT 15.
    DEFINE VARIABLE iLinePONumber    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLinePOLineNo    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cLineAccount     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dLineQuantity    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dLinePrice       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cLineDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lItemType        AS LOGICAL   NO-UNDO.
    
    FIND FIRST ap-inv EXCLUSIVE-LOCK
         WHERE ap-inv.company EQ ipbf-ttImportAP1.Company
           AND ap-inv.inv-no  EQ ipbf-ttImportAP1.InvoiceNo
           AND ap-inv.vend-no EQ ipbf-ttImportAP1.VendorID
         NO-ERROR.
         
    IF NOT AVAILABLE ap-inv THEN DO: /*create a new one*/
    
        iopiAdded = iopiAdded + 1.
        
        RUN pCreateNewInvoice (
            INPUT  ipbf-ttImportAP1.Company, 
            INPUT  ipbf-ttImportAP1.VendorID, 
            INPUT  ipbf-ttImportAP1.InvoiceNo, 
            INPUT  ipbf-ttImportAP1.InvoiceDate, 
            OUTPUT riApInv
            ).
            
        FIND ap-inv EXCLUSIVE-LOCK
            WHERE ROWID(ap-inv) EQ riAPInv
            NO-ERROR.   
    END.
    IF NOT AVAILABLE ap-inv THEN NEXT.      
            
    /*Override defaults with imported values for header*/
    IF ipbf-ttImportAP1.InvoiceDate NE ? THEN 
        ap-inv.inv-date =  ipbf-ttImportAP1.InvoiceDate.
        
    FIND FIRST terms NO-LOCK
         WHERE terms.company EQ ipbf-ttImportAP1.company
           AND terms.dscr    EQ ipbf-ttImportAP1.VendorTerms
         NO-ERROR.
    IF AVAILABLE terms THEN
        ap-inv.due-date = (terms.net-day + ipbf-ttImportAP1.InvoiceDate).
    ELSE DO:
        FIND FIRST vend NO-LOCK
             WHERE vend.company EQ ipbf-ttImportAP1.company
               AND vend.vend-no EQ ipbf-ttImportAP1.VendorID                   
             NO-ERROR.
        IF AVAILABLE vend THEN DO:
            FIND FIRST terms NO-LOCK
                 WHERE terms.company EQ vend.company
                   AND terms.t-code  EQ vend.terms
                 NO-ERROR.
            IF AVAILABLE terms THEN
        ap-inv.due-date = (terms.net-day + ipbf-ttImportAP1.InvoiceDate).
        END.    
    END.

    /* Set receiver-no eq 0 so VU# won't try to create a manual check - 54241 - MYT */
    ASSIGN 
        ap-inv.receiver-no = "0".
    
    RUN pFetchInvoiceDetails (
        INPUT  TEMP-TABLE ttImportAP1:HANDLE,
        OUTPUT cPOLineDetails
        ).
        
    DO iIndex = 1 TO EXTENT(cPOLineDetails):
        IF cPOLineDetails[iIndex] NE "" THEN DO:
            ASSIGN
                iLinePONumber    = IF NUM-ENTRIES(cPOLineDetails[iIndex]) GE 1 THEN
                                       INTEGER(ENTRY(1,cPOLineDetails[iIndex]))
                                   ELSE
                                       0
                cLineAccount     = IF NUM-ENTRIES(cPOLineDetails[iIndex]) GE 2 THEN
                                       ENTRY(2,cPOLineDetails[iIndex])
                                   ELSE
                                       ""
                dLineQuantity    = IF NUM-ENTRIES(cPOLineDetails[iIndex]) GE 3 THEN
                                       DECIMAL(ENTRY(3,cPOLineDetails[iIndex]))
                                   ELSE
                                       1
                dLinePrice       = IF NUM-ENTRIES(cPOLineDetails[iIndex]) GE 4 THEN
                                       DECIMAL(ENTRY(4,cPOLineDetails[iIndex]))
                                   ELSE
                                       0
                cLineDescription = IF NUM-ENTRIES(cPOLineDetails[iIndex]) GE 5 THEN
                                       ENTRY(5,cPOLineDetails[iIndex])
                                   ELSE 
                                       ""
                iLinePOLineNo    = IF NUM-ENTRIES(cPOLineDetails[iIndex]) GE 5 THEN
                                       INTEGER(ENTRY(6,cPOLineDetails[iIndex]))
                                   ELSE 
                                       1                                                         
                NO-ERROR.
            
            IF iLinePONumber NE 0 THEN    
                RUN pValidatePoAccountNumber (
                    INPUT  ipbf-ttImportAP1.Company,
                    INPUT  iLinePONumber,
                    INPUT  iLinePOLineNo,
                    INPUT  cLineAccount,
                    INPUT  dLineQuantity,
                    INPUT  dLinePrice,
                    INPUT-OUTPUT lHold,
                    OUTPUT lItemtype,
                    INPUT-OUTPUT cHoldNote
                    ).
            
            IF AVAILABLE ap-inv THEN DO:
                RUN pCreateInvoiceLine (
                    INPUT  ROWID(ap-inv),
                    INPUT  iLinePONumber,
                    INPUT  iLinePOLineNo,
                    INPUT  cLineAccount,
                    INPUT  dLineQuantity,
                    INPUT  dLinePrice,
                    INPUT  cLineDescription,
                    INPUT  dLineQuantity * dLinePrice,
                    OUTPUT riAPInvl
                    ) NO-ERROR.
                
                IF NOT ERROR-STATUS:ERROR THEN DO:
                    IF NOT lItemType THEN
                        RUN pValidateFGItemReceiptQtyPrice (
                            INPUT        riAPInvl,
                            INPUT        ipbf-ttImportAP1.Company,
                            INPUT        iLinePONumber,
                            INPUT        iLinePOLineNo,
                            INPUT        dLineQuantity,
                            INPUT        dLinePrice,
                            INPUT-OUTPUT lHold,
                            INPUT-OUTPUT cHoldNote
                            ).
                    ELSE
                        RUN pValidateRMItemReceiptQtyPrice (
                            INPUT        riAPInvl,
                            INPUT        ipbf-ttImportAP1.Company,
                            INPUT        iLinePONumber,
                            INPUT        iLinePOLineNo,
                            INPUT        dLineQuantity,
                            INPUT        dLinePrice,
                            INPUT-OUTPUT lHold,
                            INPUT-OUTPUT cHoldNote
                            ).                    
                END.         
            END.
        END.
    END.

    IF ipbf-ttImportAP1.TotalAmount EQ 0 THEN  
        ASSIGN
            lHold                        = YES
            cHoldNote                    = IF cHoldNote EQ '' THEN
                                               "Total Amount is Null"
                                           ELSE  
                                               cHoldNote + "|" + "Total Amount is Null"
            .

    IF lHold THEN DO:
        ap-inv.stat = "H".
        DO iIndex = 1 TO NUM-ENTRIES(cHoldNote,"|"):        
            IF VALID-HANDLE(hdTagProcs) AND ENTRY(iIndex, cHoldNote, "|") NE "" THEN
                RUN AddTagHold IN hdTagProcs (
                    INPUT ap-inv.REC_KEY,
                    INPUT "ap-inv",
                    INPUT ENTRY(iIndex, cHoldNote, "|"),
                    INPUT "",
                    INPUT ""
                    ).
        END.
    END.
                                                           
    RUN pRecalculateInvoiceHeader (
        INPUT ROWID(ap-inv), 
        INPUT NO
        ).   
    FIND CURRENT ap-inv NO-LOCK NO-ERROR .
    FIND CURRENT ap-invl NO-LOCK NO-ERROR .

END PROCEDURE.

PROCEDURE pFetchInvoiceDetails:
    DEFINE INPUT  PARAMETER iphdTempTable     AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPOLineDetails  AS CHARACTER NO-UNDO EXTENT 15.
    
    DEFINE VARIABLE cPOLineDetails AS CHARACTER NO-UNDO EXTENT 15.
    DEFINE VARIABLE hdTTBuffer     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iIndex         AS INTEGER   NO-UNDO.
        
    hdTTBuffer = iphdTempTable:DEFAULT-BUFFER-HANDLE.
    
    RUN pFetchInvoiceLineDetails (
        INPUT  hdTTBuffer,
        OUTPUT cPOLineDetails
        ).   
      
    DO iIndex = 1 TO EXTENT(cPOLineDetails):
        opcPOLineDetails[iIndex] = IF cPOLineDetails[iIndex] NE "" THEN 
                                       STRING(hdTTBuffer:BUFFER-FIELD("LinePONumber1"):BUFFER-VALUE) + "," + cPOLineDetails[iIndex]
                                   ELSE 
                                       "".        
    END.     
END PROCEDURE.

PROCEDURE pFetchInvoiceLineDetails:
    DEFINE INPUT  PARAMETER iphdTTBuffer     AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPOLineDetails AS CHARACTER NO-UNDO EXTENT 15.
    
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
      
    DO iIndex = 1 TO EXTENT(opcPOLineDetails):
        IF iphdTTBuffer:BUFFER-FIELD("LineAccount" + STRING(iIndex)):BUFFER-VALUE  NE "" OR
           iphdTTBuffer:BUFFER-FIELD("LineQuantity" + STRING(iIndex)):BUFFER-VALUE NE 0  OR 
           iphdTTBuffer:BUFFER-FIELD("LinePrice" + STRING(iIndex)):BUFFER-VALUE    NE 0  OR
           iphdTTBuffer:BUFFER-FIELD("POLine" + STRING(iIndex)):BUFFER-VALUE       NE 0 THEN
           opcPOLineDetails[iIndex] = iphdTTBuffer:BUFFER-FIELD("LineAccount" + STRING(iIndex)):BUFFER-VALUE + ","
                                 + (IF iphdTTBuffer:BUFFER-FIELD("LineQuantity" + STRING(iIndex)):BUFFER-VALUE EQ 0 THEN
                                        "1"
                                    ELSE 
                                        STRING(iphdTTBuffer:BUFFER-FIELD("LineQuantity" + STRING(iIndex)):BUFFER-VALUE)) 
                                 + "," 
                                 + STRING(iphdTTBuffer:BUFFER-FIELD("LinePrice" + STRING(iIndex)):BUFFER-VALUE)
                                 + ","
                                 + iphdTTBuffer:BUFFER-FIELD("LineDescription"+ STRING(iIndex)):BUFFER-VALUE   
                                 + ","
                                 + (IF iphdTTBuffer:BUFFER-FIELD("POLine" + STRING(iIndex)):BUFFER-VALUE EQ 0 THEN
                                        "1"
                                    ELSE 
                                        STRING(iphdTTBuffer:BUFFER-FIELD("POLine" + STRING(iIndex)):BUFFER-VALUE))  
                                 .
    END.    
END PROCEDURE.

PROCEDURE pRecalculateInvoiceHeader:
    /*------------------------------------------------------------------------------
       Purpose:
       Notes:
      ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriAPInv AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iplOverwriteTax AS LOGICAL NO-UNDO.

    DEFINE VARIABLE dFreight        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTaxAmount     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dFrtTaxAmount  AS DECIMAL NO-UNDO.

    DEFINE BUFFER bf-ap-inv  FOR ap-inv.
    DEFINE BUFFER bf-ap-invl FOR ap-invl.


    FIND bf-ap-inv EXCLUSIVE-LOCK  
        WHERE ROWID(bf-ap-inv) EQ ipriAPInv NO-ERROR.

    IF AVAILABLE bf-ap-inv THEN 
    DO:

        IF NOT iplOverwriteTax THEN
            bf-ap-inv.tax-amt = 0.

        ASSIGN
            bf-ap-inv.net     = 0
            bf-ap-inv.freight = 0.
        
        FOR EACH bf-ap-invl WHERE bf-ap-invl.i-no EQ bf-ap-inv.i-no NO-LOCK:
            bf-ap-inv.net = bf-ap-inv.net + bf-ap-invl.amt.

            IF bf-ap-invl.tax AND NOT iplOverwriteTax THEN
            DO:
                RUN Tax_Calculate(INPUT bf-ap-inv.company,
                           INPUT bf-ap-inv.tax-gr,
                           INPUT FALSE,
                           INPUT bf-ap-invl.amt,
                           INPUT "", 
                           OUTPUT dTaxAmount).
                bf-ap-inv.tax-amt = bf-ap-inv.tax-amt +
                    ROUND(dTaxAmount,2).
            END.
            
            IF bf-ap-invl.po-no NE 0 THEN 
            DO:
                FIND FIRST po-ordl NO-LOCK 
                    WHERE po-ordl.company EQ  bf-ap-invl.company
                    AND po-ordl.po-no   EQ (IF bf-ap-invl.po-no EQ 0 THEN bf-ap-inv.po-no
                    ELSE bf-ap-invl.po-no)
                    AND po-ordl.line    EQ (bf-ap-invl.line + (bf-ap-invl.po-no * 1000 * -1)) 
                    USE-INDEX po-no NO-ERROR.

                IF AVAILABLE po-ordl THEN 
                DO:
                    RUN po/getfrtcs.p (ROWID(po-ordl), bf-ap-invl.qty, OUTPUT dFreight).
                    bf-ap-inv.freight = bf-ap-inv.freight + dFreight.
                END.
            END.
        END.
        
        IF bf-ap-inv.tax-gr NE "" THEN
        RUN Tax_Calculate(INPUT bf-ap-inv.company,
                           INPUT bf-ap-inv.tax-gr,
                           INPUT TRUE,
                           INPUT bf-ap-inv.freight,
                           INPUT "", 
                           OUTPUT dFrtTaxAmount).         

        ASSIGN
            bf-ap-inv.tax-amt = bf-ap-inv.tax-amt +
                               ROUND(dFrtTaxAmount,2)
            bf-ap-inv.net     = bf-ap-inv.net + bf-ap-inv.tax-amt
            bf-ap-inv.due     = bf-ap-inv.net - bf-ap-inv.disc-taken -
                                bf-ap-inv.paid + bf-ap-inv.freight
            dTotalInvoiceAmt  = dTotalInvoiceAmt + bf-ap-inv.net
            .
    END.

    FIND CURRENT bf-ap-inv NO-LOCK.



END PROCEDURE.

PROCEDURE pUpdateInvoiceLineFromPO:
    /*------------------------------------------------------------------------------
        Purpose:
        Notes:
       ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriAPInvl AS ROWID NO-UNDO.
        
/*    DEFINE VARIABLE v-qty        AS DECIMAL   NO-UNDO.                                          */
/*    DEFINE VARIABLE cAccountVend AS CHARACTER NO-UNDO.                                          */
/*    DEFINE VARIABLE v-dscr       AS CHARACTER NO-UNDO.                                          */
/*    DEFINE VARIABLE v-tmp-qty    LIKE ap-invl.qty NO-UNDO.                                      */
/*                                                                                                */
/*    DEFINE VARIABLE v-ext-cost   AS DECIMAL   NO-UNDO.                                          */
/*    DEFINE VARIABLE v-qty-uom    AS CHARACTER NO-UNDO.                                          */
/*    DEFINE VARIABLE v-out-qty    AS DECIMAL   NO-UNDO.                                          */
/*    DEFINE VARIABLE v-out-cost   AS DECIMAL   NO-UNDO.                                          */
/*    DEFINE VARIABLE v-setup-per  AS DECIMAL   NO-UNDO.                                          */
/*                                                                                                */
/*    DEFINE VARIABLE lSetup       AS LOGICAL   NO-UNDO.                                          */
/*                                                                                                */
/*    DEFINE BUFFER bf-ap-inv  FOR ap-inv.                                                        */
/*    DEFINE BUFFER bf-ap-invl FOR ap-invl.                                                       */
/*                                                                                                */
/*    DEFINE VARIABLE v-corr AS LOG.                                                              */
/*                                                                                                */
/*                                                                                                */
/*    FIND bf-ap-invl EXCLUSIVE-LOCK                                                              */
/*        WHERE ROWID(bf-ap-invl) EQ ipriAPInvl NO-ERROR.                                         */
/*    FIND FIRST bf-ap-inv OF bf-ap-invl no-lock                                                  */
/*    FIND FIRST vend NO-LOCK                                                                     */
/*        WHERE vend.company EQ bf-ap-invl.company                                                */
/*        AND vend.vend-no EQ bf-ap-inv.vend-no                                                   */
/*        NO-ERROR.                                                                               */
/*                                                                                                */
/*    FIND FIRST sys-ctrl                                                                         */
/*        WHERE sys-ctrl.company EQ bf-ap-invl.company                                            */
/*        AND sys-ctrl.name    EQ "MSFCALC"                                                       */
/*        NO-LOCK NO-ERROR.                                                                       */
/*    ASSIGN                                                                                      */
/*        cAccountVend = IF AVAILABLE vend THEN vend.actnum ELSE ""                               */
/*        v-dscr       = "".                                                                      */
/*                                                                                                */
/*    IF cAccountVend EQ "" THEN                                                                  */
/*        cAccountVend = v-ap-pur.                                                                */
/*                                                                                                */
/*    FOR EACH tt-pol                                                                             */
/*        WHERE tt-pol.selekt                                                                     */
/*        AND tt-pol.qty-to-inv NE 0,                                                             */
/*        FIRST po-ordl WHERE RECID(po-ordl) EQ tt-pol.rec-id NO-LOCK                             */
/*        BREAK BY po-ordl.po-no:                                                                 */
/*                                                                                                */
/*        FIND FIRST po-ord NO-LOCK                                                               */
/*            WHERE po-ord.company EQ po-ordl.company                                             */
/*            AND po-ord.po-no   EQ po-ordl.po-no                                                 */
/*            NO-ERROR.                                                                           */
/*                                                                                                */
/*        CREATE tt-ap-invl.                                                                      */
/*        ASSIGN                                                                                  */
/*            tt-ap-invl.i-no    = ap-inv.i-no                                                    */
/*            tt-ap-invl.actnum  = cAccountVend                                                   */
/*            tt-ap-invl.company = ap-inv.company                                                 */
/*            tt-ap-invl.vend-no = ap-inv.vend-no                                                 */
/*            tt-ap-invl.dscr    = v-dscr                                                         */
/*            tt-ap-invl.loc     = ap-inv.loc                                                     */
/*            tt-ap-invl.period  = ap-inv.period                                                  */
/*            tt-ap-invl.posted  = ap-inv.posted                                                  */
/*            tt-ap-invl.tax     = ap-inv.tax-gr NE ""                                            */
/*            .                                                                                   */
/*        IF aptax-chr = "ITEM" THEN                                                              */
/*        DO:                                                                                     */
/*            FIND ITEM WHERE ITEM.company = g_company                                            */
/*                AND ITEM.i-no = po-ordl.i-no NO-LOCK NO-ERROR.                                  */
/*            IF AVAILABLE ITEM THEN tt-ap-invl.tax = ITEM.tax-rcpt .                             */
/*        END.                                                                                    */
/*                                                                                                */
/*        ASSIGN                                                                                  */
/*            tt-ap-invl.po-no      = (po-ord.po-no)                                              */
/*            tt-ap-invl.LINE       = (po-ordl.LINE + (po-ord.po-no * 1000) ) /* ap/invline.i 1 */*/
/*            tt-ap-invl.dscr       = po-ordl.i-name                                              */
/*            tt-ap-invl.unit-pr    = (po-ordl.cost)                                              */
/*            tt-ap-invl.pr-qty-uom = po-ordl.pr-uom                                              */
/*            tt-ap-invl.cons-uom   = po-ordl.pr-qty-uom                                          */
/*            v-wid                 = po-ordl.s-wid                                               */
/*            v-len                 = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len  */
/*            v-dep                 = 0                                                           */
/*            v-bwt                 = 0.                                                          */
/*                                                                                                */
/*        IF tt-ap-invl.cons-uom EQ "ROLL" THEN tt-ap-invl.cons-uom = "LF".                       */
/*                                                                                                */
/*        IF po-ordl.item-type AND appaper-chr NE "PO UOM" AND                                    */
/*            CAN-FIND(FIRST item                                                                 */
/*            WHERE item.company EQ cocode                                                        */
/*            AND item.i-no    EQ po-ordl.i-no                                                    */
/*            AND item.mat-type EQ "P")          THEN                                             */
/*            tt-ap-invl.cons-uom = appaper-chr.                                                  */
/*                                                                                                */
/*        RELEASE prod.                                                                           */
/*        RELEASE costtype.                                                                       */
/*                                                                                                */
/*        IF po-ordl.item-type EQ NO                          AND                                 */
/*            (fgpostgl EQ "AllItems" OR fgpostgl EQ "POOnly") THEN                               */
/*        DO:                                                                                     */
/*            FIND FIRST itemfg                                                                   */
/*                WHERE itemfg.company EQ po-ordl.company                                         */
/*                AND itemfg.i-no    EQ po-ordl.i-no                                              */
/*                NO-LOCK NO-ERROR.                                                               */
/*                                                                                                */
/*            IF AVAILABLE itemfg THEN                                                            */
/*                FIND FIRST prodl                                                                */
/*                    WHERE prodl.company EQ itemfg.company                                       */
/*                    AND prodl.procat  EQ itemfg.procat                                          */
/*                    AND CAN-FIND(FIRST prod                                                     */
/*                    WHERE prod.company EQ cocode                                                */
/*                    AND prod.prolin  EQ prodl.prolin)                                           */
/*                    NO-LOCK NO-ERROR.                                                           */
/*                                                                                                */
/*            IF AVAILABLE prodl THEN                                                             */
/*                FIND FIRST prod                                                                 */
/*                    WHERE prod.company EQ prodl.company                                         */
/*                    AND prod.prolin  EQ prodl.prolin                                            */
/*                    NO-LOCK NO-ERROR.                                                           */
/*        END.                                                                                    */
/*                                                                                                */
/*        ELSE                                                                                    */
/*            IF po-ordl.item-type AND v-rmpostgl-char EQ "ALLITEMS" THEN                         */
/*            DO:                                                                                 */
/*                FIND FIRST item                                                                 */
/*                    WHERE item.company EQ cocode                                                */
/*                    AND item.i-no    EQ po-ordl.i-no                                            */
/*                    NO-LOCK NO-ERROR.                                                           */
/*                                                                                                */
/*                IF AVAILABLE item AND item.stocked THEN                                         */
/*                    FIND FIRST costtype                                                         */
/*                        WHERE costtype.company   EQ cocode                                      */
/*                        AND costtype.cost-type EQ item.cost-type                                */
/*                        NO-LOCK NO-ERROR.                                                       */
/*            END.                                                                                */
/*                                                                                                */
/*        IF AVAILABLE prod AND prod.wip-mat NE "" THEN                                           */
/*            tt-ap-invl.actnum = prod.wip-mat.                                                   */
/*        ELSE                                                                                    */
/*            IF AVAILABLE costtype AND costtype.ap-accrued NE "" THEN                            */
/*                tt-ap-invl.actnum = costtype.ap-accrued.                                        */
/*            ELSE                                                                                */
/*                IF lv-po-glnum THEN tt-ap-invl.actnum = po-ordl.actnum.                         */
/*                ELSE                                                                            */
/*                DO:                                                                             */
/*                    IF v-vend-actnum EQ "" THEN                                                 */
/*                    DO:                                                                         */
/*                        FIND FIRST vend WHERE vend.company EQ cocode                            */
/*                            AND vend.vend-no EQ po-ord.vend-no                                  */
/*                            NO-LOCK NO-ERROR.                                                   */
/*                        IF AVAILABLE vend THEN v-vend-actnum = vend.actnum.                     */
/*                    END.                                                                        */
/*                    tt-ap-invl.actnum = v-vend-actnum.                                          */
/*                END.                                                                            */
/*                                                                                                */
/*        IF v-vend-actnum EQ "" THEN                                                             */
/*            v-vend-actnum = v-ap-pur.                                                           */
/*                                                                                                */
/*        IF tt-ap-invl.actnum EQ "" THEN tt-ap-invl.actnum = v-vend-actnum.                      */
/*                                                                                                */
/*        FIND FIRST ITEM WHERE item.company EQ cocode                                            */
/*            AND item.i-no    EQ po-ordl.i-no                                                    */
/*            AND po-ordl.item-type                                                               */
/*            NO-LOCK NO-ERROR.                                                                   */
/*        IF AVAILABLE item THEN                                                                  */
/*        DO:                                                                                     */
/*            v-dep = item.s-dep.                                                                 */
/*            {po/pol-dims.i}                                                                     */
/*        END.                                                                                    */
/*                                                                                                */
/*        IF NOT po-ordl.item-type AND tt-ap-invl.cons-uom NE "EA" THEN                           */
/*            RUN sys/ref/convquom.p ("EA", tt-ap-invl.cons-uom,                                  */
/*                v-bwt, v-len, v-wid, v-dep,                                                     */
/*                tt-pol.qty-to-inv, OUTPUT tt-pol.qty-to-inv).                                   */
/*                                                                                                */
/*        ASSIGN                                                                                  */
/*            tt-ap-invl.qty = tt-pol.qty-to-inv.                                                 */
/*                                                                                                */
/*        IF tt-pol.qty-to-inv-uom NE "" AND                                                      */
/*            tt-pol.qty-to-inv-uom NE po-ordl.pr-qty-uom THEN                                    */
/*            RUN sys/ref/convquom.p (tt-pol.qty-to-inv-uom, po-ordl.pr-qty-uom,                  */
/*                v-bwt, v-len, v-wid, v-dep,                                                     */
/*                tt-ap-invl.qty, OUTPUT v-tmp-qty).                                              */
/*        ELSE                                                                                    */
/*            v-tmp-qty = tt-ap-invl.qty.                                                         */
/*        IF LOOKUP(po-ordl.pr-uom,"L,LOT") GT 0 THEN                                             */
/*        DO:                                                                                     */
/*            ASSIGN                                                                              */
/*                tt-ap-invl.amt        = po-ordl.t-cost                                          */
/*                tt-ap-invl.unit-pr    = po-ordl.cost                                            */
/*                tt-ap-invl.pr-qty-uom = po-ordl.pr-uom.                                         */
/*        END.                                                                                    */
/*        ELSE                                                                                    */
/*        DO:                                                                                     */
/*            /*Calculate proportionate amt based on total cost, not including setup*/            */
/*            tt-ap-invl.amt = (po-ordl.t-cost - po-ordl.setup) * ( v-tmp-qty / po-ordl.ord-qty ).*/
/*                                                                                                */
/*            /*Add setup charges back only if no other ap-invl exist for this po line*/          */
/*            lSetup = NO.                                                                        */
/*            FIND FIRST bf-ap-invl                                                               */
/*                WHERE bf-ap-invl.company EQ po-ordl.company                                     */
/*                AND bf-ap-invl.po-no EQ po-ordl.po-no                                           */
/*                AND bf-ap-invl.po-line EQ po-ordl.LINE                                          */
/*                AND bf-ap-invl.item-no EQ po-ordl.i-no                                          */
/*                NO-LOCK NO-ERROR.                                                               */
/*            IF NOT AVAILABLE bf-ap-invl THEN lSetup = YES.                                      */
/*            IF lSetup THEN tt-ap-invl.amt = tt-ap-invl.amt + po-ordl.setup.                     */
/*                                                                                                */
/*            tt-ap-invl.unit-pr = tt-ap-invl.amt / tt-ap-invl.qty.                               */
/*                                                                                                */
/*            IF tt-ap-invl.cons-uom NE tt-ap-invl.pr-qty-uom THEN                                */
/*                RUN sys/ref/convcuom.p (tt-ap-invl.cons-uom, tt-ap-invl.pr-qty-uom,             */
/*                    v-bwt, v-len, v-wid, v-dep,                                                 */
/*                    tt-ap-invl.unit-pr, OUTPUT tt-ap-invl.unit-pr).                             */
/*            tt-ap-invl.unit-pr = ROUND(tt-ap-invl.unit-pr, 2).                                  */
/*        END.                                                                                    */
/*        IF v-len EQ 0 THEN v-len = 12.                                                          */
/*        IF v-wid EQ 0 THEN v-wid = 12.                                                          */
/*                                                                                                */
/*        tt-ap-invl.sf-sht = IF v-corr THEN (v-len * v-wid * .007)                               */
/*        ELSE (v-len * v-wid / 144).                                                             */
/*                                                                                                */
/*        IF NOT AVAILABLE item             AND                                                   */
/*            (v-len EQ 0 OR v-wid EQ 0) THEN                                                     */
/*        DO:                                                                                     */
/*            FIND FIRST itemfg                                                                   */
/*                WHERE itemfg.company EQ cocode                                                  */
/*                AND itemfg.i-no    EQ po-ordl.i-no                                              */
/*                AND NOT po-ordl.item-type                                                       */
/*                NO-LOCK NO-ERROR.                                                               */
/*            IF AVAILABLE itemfg THEN tt-ap-invl.sf-sht = (itemfg.t-sqft).                       */
/*        END.                                                                                    */
/*                                                                                                */
/*        IF tt-ap-invl.cons-uom EQ "EA" THEN v-qty = tt-ap-invl.qty.                             */
/*        ELSE                                                                                    */
/*            RUN sys/ref/convquom.p(tt-ap-invl.cons-uom, "EA",                                   */
/*                v-bwt, v-len, v-wid, v-dep,                                                     */
/*                tt-ap-invl.qty, OUTPUT v-qty).                                                  */
/*                                                                                                */
/*        tt-ap-invl.amt-msf = (tt-ap-invl.sf-sht * v-qty / 1000).                                */
/*                                                                                                */
/*    END.                                                                                        */

END PROCEDURE.

