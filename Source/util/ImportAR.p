
/*------------------------------------------------------------------------
    File        : ImportEstimate.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Estimates	

    Author(s)   : BV
    Created     : Sun Jan 21:18:38 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportAR
    FIELD Company                 AS CHARACTER FORMAT "x(3)"       HELP "Required - Size:8" 
    FIELD Location                AS CHARACTER                     HELP "Optional - Size:8" 
    FIELD CustNo                  AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Customer" HELP "Required - Size:8" 
    FIELD ShipTo                  AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Ship-To#" HELP "Required - Size:8" 
    FIELD InvNo                   AS INTEGER FORMAT ">>>>>9" COLUMN-LABEL "Invoice#" HELP "Required - Integer" 
    FIELD PONum                   AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Po#"     HELP "Optional - Size:15"
    FIELD InvDate                 AS DATE FORMAT "99/99/99" COLUMN-LABEL "Inv Date"  HELP "Optional - Date" 
    FIELD DueDate                 AS DATE FORMAT "99/99/99" COLUMN-LABEL "Due Date"  HELP "Optional - Date" 
    FIELD TaxCode                 AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Tax Group" HELP "Optional - Size:3"  
    FIELD TermsCode               AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Terms"       HELP "Required - Size:8" 
    FIELD Discount                AS DECIMAL FORMAT ">>9.99%" COLUMN-LABEL "Discount%"  HELP "Optional - Decimal" 
    FIELD DiscountDays            AS DECIMAL FORMAT "z9" COLUMN-LABEL "Disc Days"       HELP "Optional - Integer"
    FIELD Carrier                 AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Carrier"     HELP "Optional - Size:5" 
    FIELD Freight                 AS DECIMAL FORMAT "->>,>>9.99" COLUMN-LABEL "Freight" HELP "Optional - decimal"   
    FIELD LineNo                  AS INTEGER FORMAT "99" COLUMN-LABEL "Line"            HELP "Optional - integer" 
    FIELD LineAccount             AS CHARACTER FORMAT "x(25)" COLUMN-LABEL "Account Number" HELP "Optional - Size:25"   
    FIELD LineItemNo              AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Item#"  HELP "Optional - Size:15"  
    FIELD LineItemName            AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Item Name" HELP "Optional - Size:30"   
    FIELD LineItemDescription     AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Item Description" HELP "Optional - Size:30"   
    FIELD LineCustomerLotNo       AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Customer Lot#"  HELP "Optional - Size:15"  
    FIELD LineQuantity            AS DECIMAL FORMAT "->>,>>>,>>9" COLUMN-LABEL "Invoice Qty" HELP "Optional - Integer"   
    FIELD LineQuantityUom         AS CHARACTER FORMAT "x(4)" COLUMN-LABEL "Cons UOM" HELP "Optional - Size:4" 
    FIELD LinePrice               AS DECIMAL FORMAT ">>,>>>,>>9.99<<<<"COLUMN-LABEL "Price" HELP "Optional - Decimal" 
    FIELD LinePriceUom            AS CHARACTER FORMAT "x(4)" COLUMN-LABEL "UOM" HELP "Optional - Size:4" 
    FIELD LineAmount              AS DECIMAL FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Amount" HELP "Optional - Decimal"
    FIELD LineDiscount            AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Dsct%" HELP "Optional - Decimal"
    FIELD LineCost                AS DECIMAL FORMAT "->>>,>>>,>>9.99<<<<" COLUMN-LABEL "Cost" HELP "Optional - Decimal"
    FIELD LineCostUom             AS CHARACTER FORMAT "x(4)" COLUMN-LABEL "Cost UOM" HELP "Optional - Size:4"
    FIELD LineSalesman1           AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "SlsRep1" HELP "Optional - Size:3"
    FIELD LineSalesman1Percent    AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "% of Sale" HELP "Optional - Decimal"
    FIELD LineSalesman1Commission AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Comm%" HELP "Optional - Decimal"
    FIELD LineSalesman2           AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "SlsRep2" HELP "Optional - Size:3"    
    FIELD LineSalesman2Percent    AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "% of Sale" HELP "Optional - Decimal"
    FIELD LineSalesman2Commission AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Comm%"  HELP "Optional - Decimal"   
    FIELD LineSalesman3           AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "SlsRep3"   HELP "Optional - Size -3"  
    FIELD LineSalesman3Percent    AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "% of Sale" HELP "Optional - Decimal"
    FIELD LineSalesman3Commission AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Comm%"     HELP "Optional - Decimal" 
    FIELD LineTax                 AS CHARACTER FORMAT "yes/no" COLUMN-LABEL "Tax"  HELP "Optional - Yes or N0"
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportAR"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportAR FOR ttImportAR.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportAR FOR ttImportAR.
    
  
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportAR.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportAR.CustNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Customer#".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportAR.ShipTo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Ship-To#".
    END.
    IF oplValid THEN 
    DO: 
        IF ipbf-ttImportAR.InvNo EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Invoice#".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportAR.TermsCode EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Terms Code".
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportAR NO-LOCK 
            WHERE bf-ttImportAR.Company EQ ipbf-ttImportAR.Company
            AND bf-ttImportAR.CustNo EQ ipbf-ttImportAR.CustNo
            AND bf-ttImportAR.InvNo EQ ipbf-ttImportAR.InvNo
            AND bf-ttImportAR.LineItemNo EQ ipbf-ttImportAR.LineItemNo 
            AND bf-ttImportAR.LineNo EQ ipbf-ttImportAR.LineNo
            AND ROWID(bf-ttImportAR) NE ROWID(ipbf-ttImportAR)
            NO-ERROR.
        IF AVAILABLE bf-ttImportAR THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    IF oplValid THEN 
    DO: 
        FIND FIRST ar-inv NO-LOCK 
            WHERE ar-inv.company EQ ipbf-ttImportAR.Company
            AND ar-inv.cust-no EQ ipbf-ttImportAR.CustNo
            AND ar-inv.inv-no EQ ipbf-ttImportAR.InvNo
            NO-ERROR .
        IF AVAILABLE ar-inv THEN 
            FIND FIRST ar-invl NO-LOCK 
                WHERE ar-invl.company EQ ar-inv.company
                AND ar-invl.inv-no EQ ar-inv.inv-no
                AND ar-invl.i-no EQ ipbf-ttImportAR.LineItemNo
                AND ar-invl.LINE EQ ipbf-ttImportAR.LineNo
                NO-ERROR.
        IF AVAILABLE ar-inv AND AVAILABLE ar-invl THEN 
        DO:
            IF NOT iplUpdateDuplicates THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Duplicate Exists:  Will be skipped"
                    .
            ELSE
                ASSIGN 
                    opcNote = "Update record - All fields to be overwritten"
                    .        
        END.
        ELSE IF AVAILABLE ar-inv AND NOT AVAILABLE ar-invl THEN 
                ASSIGN 
                    opcNote = "Add record - New Line to Existing Invoice"
                    .
            ELSE 
                ASSIGN 
                    opcNote = "Add record"
                    .
        
    END.
    
    IF oplValid AND iplFieldValidation THEN 
    DO:

        IF oplValid AND ipbf-ttImportAR.CustNo NE "" THEN 
        DO:
            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ ipbf-ttImportAR.Company
                AND cust.cust-no EQ ipbf-ttImportAR.CustNo
                AND lookup(cust.active,"A,E") > 0  NO-ERROR.
            IF NOT AVAILABLE cust THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Customer"
                    .
        END.
        IF oplValid AND ipbf-ttImportAR.ShipTo NE "" THEN 
        DO:
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company = ipbf-ttImportAR.Company
                AND shipto.cust-no = ipbf-ttImportAR.CustNo
                AND shipto.ship-id = ipbf-ttImportAR.ShipTo NO-ERROR.
            IF NOT AVAILABLE cust THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Ship-To#"
                    .
        END.
        IF oplValid AND ipbf-ttImportAR.TaxCode NE "" THEN 
        DO:
            FIND FIRST stax NO-LOCK
                WHERE stax.company = ipbf-ttImportAR.Company
                AND stax.tax-group = ipbf-ttImportAR.TaxCode
                NO-ERROR.
            IF NOT AVAILABLE stax THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Tax Group"
                    .
        END.
        IF oplValid AND ipbf-ttImportAR.TermsCode NE "" THEN 
        DO:
            FIND FIRST terms NO-LOCK 
                WHERE terms.company = ipbf-ttImportAR.Company
                AND terms.t-code EQ ipbf-ttImportAR.TermsCode NO-ERROR.
            IF NOT AVAILABLE terms THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Terms"
                    .
        END.
        IF oplValid AND ipbf-ttImportAR.LineAccount NE "" THEN 
        DO:
            FIND FIRST account NO-LOCK 
                WHERE account.company = ipbf-ttImportAR.Company
                AND account.TYPE <> "T"
                AND account.actnum = ipbf-ttImportAR.LineAccount NO-ERROR.
            IF NOT AVAIL account THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Account".
        END.

        IF oplValid AND ipbf-ttImportAR.LinePriceUom NE "" THEN 
        DO:
            IF LOOKUP(ipbf-ttImportAR.LinePriceUom,"EA,MSF,M") LE 0 THEN
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid UOM".
        END.
        IF oplValid AND ipbf-ttImportAR.LineCostUom NE "" THEN 
        DO:
            IF LOOKUP(ipbf-ttImportAR.LineCostUom,"EA,M") LE 0 THEN
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Cost UOM".
        END. 

        IF oplValid AND ipbf-ttImportAR.LineSalesman1 NE "" THEN 
        DO:
            FIND FIRST sman NO-LOCK WHERE
                sman.company EQ ipbf-ttImportAR.Company 
                AND sman.sman EQ ipbf-ttImportAR.LineSalesman1 NO-ERROR.
            IF NOT AVAIL sman THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Sales Rep1".
        END.
        IF oplValid AND ipbf-ttImportAR.LineSalesman2 NE "" THEN 
        DO:
            FIND FIRST sman NO-LOCK WHERE
                sman.company EQ ipbf-ttImportAR.Company 
                AND sman.sman EQ ipbf-ttImportAR.LineSalesman2 NO-ERROR.
            
            IF NOT AVAIL sman THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Sales Rep2".
        END.
        IF oplValid AND ipbf-ttImportAR.LineSalesman3 NE "" THEN 
        DO:
            FIND FIRST sman NO-LOCK WHERE
                sman.company EQ ipbf-ttImportAR.Company 
                AND sman.sman EQ ipbf-ttImportAR.LineSalesman3 NO-ERROR.
            
            IF NOT AVAIL sman THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Sales Rep3".
        END.

    END.
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportAR FOR ttImportAR.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.

    DEFINE VARIABLE riARInv  AS ROWID .
    DEFINE VARIABLE riARInvl AS ROWID.
    DEFINE BUFFER bf-ar-inv FOR ar-inv .
    DEFINE BUFFER bf-ar-invl FOR ar-invl .
       
    IF ipbf-ttImportAR.CustNo EQ "" THEN NEXT.
    IF ipbf-ttImportAR.InvNo EQ 0 THEN NEXT.

    FIND FIRST bf-ar-inv NO-LOCK
        WHERE bf-ar-inv.company EQ ipbf-ttImportAR.Company
        AND bf-ar-inv.inv-no EQ ipbf-ttImportAR.InvNo
        NO-ERROR.
    IF NOT AVAILABLE bf-ar-inv THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        RUN pCreateNewInvoiceAR (ipbf-ttImportAR.Company,ipbf-ttImportAR.CustNo, ipbf-ttImportAR.InvDate, OUTPUT riARInv).
        FIND bf-ar-inv EXCLUSIVE-LOCK 
            WHERE ROWID(bf-ar-inv) EQ riARInv
            NO-ERROR.
        IF NOT AVAILABLE bf-ar-inv THEN NEXT.
        /*Override defaults with values from import*/
        IF ipbf-ttImportAR.InvNo NE 0 THEN 
            bf-ar-inv.inv-no = ipbf-ttImportAR.InvNo.
        IF ipbf-ttImportAR.InvDate NE ? THEN 
            bf-ar-inv.inv-date = ipbf-ttImportAR.InvDate.
        IF ipbf-ttImportAR.DueDate NE ? THEN 
            bf-ar-inv.due-date = ipbf-ttImportAR.DueDate.
        IF ipbf-ttImportAR.ShipTo NE "" THEN 
            bf-ar-inv.ship-id = ipbf-ttImportAR.ShipTo.
        IF ipbf-ttImportAR.PONum NE "" THEN 
            bf-ar-inv.po-no = ipbf-ttImportAR.PONum.
        IF ipbf-ttImportAR.TaxCode NE "" THEN
        DO:
            FIND FIRST stax NO-LOCK 
                WHERE stax.company EQ ipbf-ttImportAR.Company
                AND stax.tax-group EQ ipbf-ttImportAR.TaxCode
                NO-ERROR.
            IF AVAILABLE stax THEN 
                bf-ar-inv.tax-code = ipbf-ttImportAR.TaxCode.
        END.
        IF ipbf-ttImportAR.TermsCode NE "" THEN 
            bf-ar-inv.terms = ipbf-ttImportAR.TermsCode.
        IF ipbf-ttImportAR.Discount NE 0 THEN 
            bf-ar-inv.disc-% = ipbf-ttImportAR.Discount.               
        IF ipbf-ttImportAR.DiscountDays NE 0 THEN 
            bf-ar-inv.disc-days = ipbf-ttImportAR.DiscountDays.
        IF ipbf-ttImportAR.Carrier NE "" THEN 
            bf-ar-inv.carrier = ipbf-ttImportAR.Carrier.
        IF ipbf-ttImportAR.Freight NE 0 THEN 
            bf-ar-inv.freight = ipbf-ttImportAR.Freight.                                     
    END. /*Not avail bf-ar-inv*/
    RUN pCreateNewInvoiceLineAR (ROWID(bf-ar-inv),ipbf-ttImportAR.LineNo,ipbf-ttImportAR.LineItemNo, OUTPUT riARInvl).
    FIND bf-ar-invl EXCLUSIVE-LOCK 
        WHERE ROWID(bf-ar-invl) EQ riARInvl
        NO-ERROR.
    IF NOT AVAILABLE bf-ar-invl THEN NEXT.
    /*Override defaults with values from import*/
    ASSIGN 
        bf-ar-invl.tax = ipbf-ttImportAR.LineTax EQ 'Y'
        bf-ar-invl.amt = ipbf-ttImportAR.LineAmount
        .
    IF ipbf-ttImportAR.LineQuantity NE 0 THEN 
        bf-ar-invl.qty = ipbf-ttImportAR.LineQuantity.
    ELSE 
        bf-ar-invl.qty = 1.
    bf-ar-invl.inv-qty = bf-ar-invl.qty.
    IF ipbf-ttImportAR.LineQuantityUom NE "" THEN 
        bf-ar-invl.cons-uom = ipbf-ttImportAR.LineQuantityUOM.
    ELSE 
        bf-ar-invl.cons-uom = "EA".
    
    IF ipbf-ttImportAR.LinePrice NE 0 THEN 
        bf-ar-invl.unit-pr = ipbf-ttImportAR.LinePrice.
    ELSE 
        bf-ar-invl.unit-pr = bf-ar-invl.amt.
    IF ipbf-ttImportAR.LinePriceUom NE "" THEN 
        bf-ar-invl.pr-qty-uom = ipbf-ttImportAR.LinePriceUom.
    ELSE 
        bf-ar-invl.pr-qty-uom = "EA".
    IF ipbf-ttImportAR.LineAccount NE "" THEN 
        bf-ar-invl.actnum = ipbf-ttImportAR.LineAccount.
    IF ipbf-ttImportAR.LineNo NE 0 THEN 
        bf-ar-invl.line = ipbf-ttImportAR.LineNo.
    IF ipbf-ttImportAR.LineItemNo NE "" THEN 
        bf-ar-invl.i-no = ipbf-ttImportAR.LineItemNo.
    IF ipbf-ttImportAR.LineItemName NE "" THEN 
        bf-ar-invl.i-name = ipbf-ttImportAR.LineItemName.
    IF ipbf-ttImportAR.LineItemDescription NE "" THEN                                 
        bf-ar-invl.i-dscr = ipbf-ttImportAR.LineItemDescription.     
    IF ipbf-ttImportAR.LineCustomerLotNo NE "" THEN 
        bf-ar-invl.lot-no = ipbf-ttImportAR.LineCustomerLotNo.
    IF ipbf-ttImportAR.LineDiscount NE 0 THEN                            
        bf-ar-invl.disc = ipbf-ttImportAR.LineDiscount.            
    IF ipbf-ttImportAR.LineCost NE 0 THEN 
        bf-ar-invl.cost = ipbf-ttImportAR.LineCost.                
    IF ipbf-ttImportAR.LineCostUom NE "" THEN 
        bf-ar-invl.cons-uom = ipbf-ttImportAR.LineCostUom.             
    IF ipbf-ttImportAR.LineSalesman1 NE "" THEN 
        bf-ar-invl.sman[1] = ipbf-ttImportAR.LineSalesman1.
    IF ipbf-ttImportAR.LineSalesman1Percent NE 0 THEN 
        bf-ar-invl.s-pct[1] = ipbf-ttImportAR.LineSalesman1Percent.
    IF ipbf-ttImportAR.LineSalesman1Commission NE 0 THEN 
        bf-ar-invl.s-comm[1] = ipbf-ttImportAR.LineSalesman1Commission.
    IF ipbf-ttImportAR.LineSalesman2 NE "" THEN 
        bf-ar-invl.sman[2] = ipbf-ttImportAR.LineSalesman2.
    IF ipbf-ttImportAR.LineSalesman2Percent NE 0 THEN 
        bf-ar-invl.s-pct[2] = ipbf-ttImportAR.LineSalesman2Percent.
    IF ipbf-ttImportAR.LineSalesman2Commission NE 0 THEN 
        bf-ar-invl.s-comm[3] = ipbf-ttImportAR.LineSalesman3Commission.
    IF ipbf-ttImportAR.LineSalesman3 NE "" THEN 
        bf-ar-invl.sman[3] = ipbf-ttImportAR.LineSalesman3.
    IF ipbf-ttImportAR.LineSalesman3Percent NE 0 THEN 
        bf-ar-invl.s-pct[3] = ipbf-ttImportAR.LineSalesman3Percent.
    IF ipbf-ttImportAR.LineSalesman3Commission NE 0 THEN 
        bf-ar-invl.s-comm[3] = ipbf-ttImportAR.LineSalesman3Commission.
                                
    RUN pRecalculateARInvoiceHeader(ROWID(bf-ar-inv), bf-ar-invl.amt).

    RELEASE bf-ar-inv .
    RELEASE bf-ar-invl.

END PROCEDURE.

PROCEDURE pCreateNewInvoiceAR:
    /*------------------------------------------------------------------------------
         Purpose: Creates a new AR invoice, setting defaults based on key values
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER.
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER.
    DEFINE INPUT PARAMETER ipdtInvDate AS DATE.
    DEFINE OUTPUT PARAMETER opriARInv AS ROWID.
    
    DEFINE VARIABLE iNextInvoiceNumber     AS INTEGER NO-UNDO.
    
    ASSIGN
        iNextInvoiceNumber     = 0.
    FIND FIRST ar-ctrl NO-LOCK  
        WHERE ar-ctrl.company EQ ipcCompany
        NO-ERROR.
    iNextInvoiceNumber = IF AVAILABLE ar-ctrl THEN ar-ctrl.last-inv + 1 ELSE 1.
    CREATE ar-inv.
    ASSIGN
        ar-inv.company  = ipcCompany
        ar-inv.inv-no   = iNextInvoiceNumber 
        ar-inv.inv-date = TODAY 
        ar-inv.cust-no  = ipcCustomer
        .
    IF ipdtInvDate NE ? THEN ar-inv.inv-date = ipdtInvDate.
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ ipcCompany
        AND cust.cust-no EQ ar-inv.cust-no
        NO-ERROR.    
    IF AVAILABLE cust THEN 
    DO:
        ASSIGN 
            ar-inv.cust-name    = cust.NAME
            ar-inv.terms        = cust.terms
            ar-inv.carrier      = cust.carrier
            ar-inv.tax-code     = cust.tax-gr
            ar-inv.curr-code[1] = cust.curr-code
            ar-inv.addr[1]      = cust.addr[1]
            ar-inv.addr[2]      = cust.addr[2]
            ar-inv.city         = cust.city
            ar-inv.state        = cust.state
            ar-inv.zip          = cust.zip
            .
        IF cust.curr-code = "" THEN 
        DO:
            FIND company NO-LOCK 
                WHERE company.company EQ ipcCompany
                NO-ERROR.
            IF AVAILABLE company THEN 
                ar-inv.curr-code[1] = company.curr-code.
        END.
        FIND FIRST shipto NO-LOCK 
            WHERE shipto.company EQ ipcCompany 
            AND shipto.cust-no EQ cust.cust-no
            NO-ERROR.
        IF AVAILABLE shipto THEN 
            ASSIGN 
                ar-inv.ship-id = shipto.ship-id
                .                               .
        FIND currency NO-LOCK 
            WHERE currency.company EQ ipcCompany
            AND currency.c-code EQ cust.curr-code 
            NO-ERROR.
        IF AVAILABLE currency THEN 
            ar-inv.ex-rate = currency.ex-rate.
        FIND FIRST terms NO-LOCK 
            WHERE terms.company EQ ipcCompany 
              AND terms.t-code EQ cust.terms
            NO-ERROR.
        IF AVAILABLE terms THEN 
            ASSIGN 
                ar-inv.terms-d   = terms.dscr
                ar-inv.due-date  = ar-inv.inv-date + terms.net-days
                ar-inv.disc-%    = terms.disc-rate
                ar-inv.disc-days = terms.disc-days
                .
    END.
    
    opriARInv = ROWID(ar-inv).
    RELEASE ar-inv.
    
END PROCEDURE.

PROCEDURE pCreateNewInvoiceLineAR:
    /*------------------------------------------------------------------------------
     Purpose: Creates a new AP invoice line, setting defaults based on key values
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriARInv AS ROWID.
    DEFINE INPUT PARAMETER ipiLine AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLineItem AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opriARInvl AS ROWID.
    
    DEFINE VARIABLE iNextLineNumber AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-ar-invl FOR ar-invl.
    
    FIND ar-inv NO-LOCK 
        WHERE ROWID(ar-inv) EQ ipriARInv
        NO-ERROR.
    IF NOT AVAILABLE ar-inv THEN RETURN.    
    FIND LAST bf-ar-invl NO-LOCK  
        WHERE bf-ar-invl.x-no EQ ar-inv.x-no 
        USE-INDEX x-no 
        NO-ERROR.
    iNextLineNumber = IF AVAILABLE bf-ar-invl THEN bf-ar-invl.LINE + 1 ELSE 1.
    FIND FIRST ar-ctrl NO-LOCK 
        WHERE ar-ctrl.company EQ ar-inv.company 
        NO-ERROR.

    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ ar-inv.company 
        AND cust.cust-no EQ ar-inv.cust-no
        NO-ERROR.

    FIND FIRST ar-invl EXCLUSIVE-LOCK   
        WHERE ar-invl.x-no EQ ar-inv.x-no
        AND ar-invl.LINE EQ ipiLine
        AND ar-invl.i-no EQ ipcLineItem NO-ERROR.

    IF NOT AVAIL ar-invl THEN 
    DO:
        CREATE ar-invl.
        ar-invl.LINE       = iNextLineNumber.
    END.

    ASSIGN       
        ar-invl.x-no       = ar-inv.x-no
        ar-invl.company    = ar-inv.company
        ar-invl.cust-no    = ar-inv.cust-no
        ar-invl.inv-no     = ar-inv.inv-no
        ar-invl.po-no      = ar-inv.po-no
        ar-invl.pr-qty-uom = "EA"
        ar-invl.cons-uom   = "EA"
        ar-invl.dscr[1]    = "M"
        ar-invl.actnum     = IF AVAILABLE ar-ctrl THEN ar-ctrl.sales ELSE ""
        ar-invl.sman[1]    = IF AVAILABLE cust THEN cust.sman ELSE ""
        ar-invl.s-pct[1]   = IF ar-invl.sman[1] NE "" THEN 100 ELSE 0    
        ar-invl.inv-date   = ar-inv.inv-date         
        .
    opriARInvl = ROWID(ar-invl).
    RELEASE ar-invl.
    
END PROCEDURE.

PROCEDURE pRecalculateARInvoiceHeader:
    /*------------------------------------------------------------------------------
     Purpose: Recaculates the balances on the AR header
     Notes: from ar/ar-invk.i
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriARInv AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdLineAmount AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dSubTotal   AS DECIMAL NO-UNDO.   
    DEFINE VARIABLE dTax        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTaxFreight AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-ar-inv  FOR ar-inv.
    DEFINE BUFFER bf-ar-invl FOR ar-invl.


    FIND bf-ar-inv EXCLUSIVE-LOCK  
        WHERE ROWID(bf-ar-inv) EQ ipriARInv NO-ERROR.

    IF AVAILABLE bf-ar-inv THEN 
    DO:
        bf-ar-inv.net = bf-ar-inv.net + ipdLineAmount.
        FIND FIRST cust NO-LOCK  
            WHERE cust.company EQ bf-ar-inv.company
            AND cust.cust-no EQ bf-ar-inv.cust-no 
            NO-ERROR.
            
        ASSIGN
            dSubTotal         = bf-ar-inv.net + bf-ar-inv.freight
            bf-ar-inv.tax-amt = 0
            dTax              = 0
            dTaxFreight       = 0.
   
        IF bf-ar-inv.tax-code NE "" AND cust.sort EQ "Y" THEN 
        DO:
            
            bf-ar-inv.tax-amt = dTax + dTaxFreight.
        END.

        ASSIGN
            bf-ar-inv.gross = dSubTotal + bf-ar-inv.tax-amt
            bf-ar-inv.due   = bf-ar-inv.gross - bf-ar-inv.paid - bf-ar-inv.disc-taken.
        
    END.
    FIND CURRENT bf-ar-inv NO-LOCK.
    RELEASE bf-ar-inv.

END PROCEDURE.
