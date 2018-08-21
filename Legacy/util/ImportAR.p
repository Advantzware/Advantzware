
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
    FIELD Company                  AS CHARACTER FORMAT "x(3)"
    FIELD Location                 AS CHARACTER
    FIELD cCustNo                  AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Customer"
    FIELD cShipTo                  AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Ship-To#"
    FIELD cInvNo                   AS CHARACTER FORMAT ">>>>>9" COLUMN-LABEL "Invoice#"  
    FIELD cPONum                   AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Po#"  
    FIELD cInvDate                 AS CHARACTER FORMAT "99/99/99" COLUMN-LABEL "Inv Date"   
    FIELD cDueDate                 AS CHARACTER FORMAT "99/99/99" COLUMN-LABEL "Due Date"   
    FIELD cTaxCode                 AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Tax Code"   
    FIELD cTermsCode               AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Terms"   
    FIELD cDiscount                AS CHARACTER FORMAT ">>9.99%" COLUMN-LABEL "Discount%"   
    FIELD cDiscountDays            AS CHARACTER FORMAT "z9" COLUMN-LABEL "Disc Days"   
    FIELD cCarrier                 AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Carrier"   
    FIELD cFreight                 AS CHARACTER FORMAT "->>,>>9.99" COLUMN-LABEL "Freight"   
    FIELD cLine                    AS CHARACTER FORMAT "99" COLUMN-LABEL "Line" 
    FIELD cLineAccount             AS CHARACTER FORMAT "x(25)" COLUMN-LABEL "Account Number"   
    FIELD cLineItemNo              AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Item#"   
    FIELD cLineItemName            AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Item Name"   
    FIELD cLineItemDescription     AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Item Description"   
    FIELD cLineCustomerLotNo       AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Customer Lot#"   
    FIELD cLineQuantity            AS CHARACTER FORMAT "->>,>>>,>>9" COLUMN-LABEL "Invoice Qty"   
    FIELD cLineQuantityUom         AS CHARACTER FORMAT "x(4)" COLUMN-LABEL "Cons UOM" 
    FIELD cLinePrice               AS CHARACTER FORMAT ">>,>>>,>>9.99<<<<"COLUMN-LABEL "Price"
    FIELD cLinePriceUom            AS CHARACTER FORMAT "x(4)" COLUMN-LABEL "UOM" 
    FIELD cLineAmount              AS CHARACTER FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Amount" 
    FIELD cLineDiscount            AS CHARACTER FORMAT ">>9.99" COLUMN-LABEL "Dsct%"
    FIELD cLineCost                AS CHARACTER FORMAT "->>>,>>>,>>9.99<<<<" COLUMN-LABEL "Cost"
    FIELD cLineCostUom             AS CHARACTER FORMAT "x(4)" COLUMN-LABEL "Cost UOM" 
    FIELD cLineSalesman1           AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "SlsRep1" 
    FIELD cLineSalesman1Percent    AS CHARACTER FORMAT ">>9.99" COLUMN-LABEL "% of Sale"
    FIELD cLineSalesman1Commission AS CHARACTER FORMAT ">>9.99" COLUMN-LABEL "Comm%"
    FIELD cLineSalesman2           AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "SlsRep2"     
    FIELD cLineSalesman2Percent    AS CHARACTER FORMAT ">>9.99" COLUMN-LABEL "% of Sale" 
    FIELD cLineSalesman2Commission AS CHARACTER FORMAT ">>9.99" COLUMN-LABEL "Comm%"     
    FIELD cLineSalesman3           AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "SlsRep3"     
    FIELD cLineSalesman3Percent    AS CHARACTER FORMAT ">>9.99" COLUMN-LABEL "% of Sale" 
    FIELD cLineSalesman3Commission AS CHARACTER FORMAT ">>9.99" COLUMN-LABEL "Comm%"     
    FIELD cLineTax                 AS CHARACTER FORMAT "yes/no" COLUMN-LABEL "Tax"  
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

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportAR FOR ttImportAR.
    
    RUN util/Validate.p PERSISTENT SET hdValidator.
  
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
        IF ipbf-ttImportAR.cCustNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Customer#".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportAR.cShipTo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Ship-To#".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportAR.cInvNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Invoice#".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportAR.cTermsCode EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Terms Code".
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportAR NO-LOCK 
            WHERE bf-ttImportAR.Company EQ ipbf-ttImportAR.Company
            AND bf-ttImportAR.cCustNo EQ ipbf-ttImportAR.cCustNo
            AND bf-ttImportAR.cInvNo EQ ipbf-ttImportAR.cInvNo
            AND bf-ttImportAR.cLineItemNo EQ ipbf-ttImportAR.cLineItemNo 
            AND bf-ttImportAR.cLine EQ ipbf-ttImportAR.cLine
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
            AND ar-inv.cust-no EQ ipbf-ttImportAR.cCustNo
            AND ar-inv.inv-no EQ INTEGER(ipbf-ttImportAR.cInvNo)
            NO-ERROR .
        IF AVAILABLE ar-inv THEN 
            FIND FIRST ar-invl NO-LOCK 
                WHERE ar-invl.company EQ ar-inv.company
                AND ar-invl.inv-no EQ ar-inv.inv-no
                AND ar-invl.i-no EQ ipbf-ttImportAR.cLineItemNo
                AND trim(STRING(ar-invl.LINE)) EQ ipbf-ttImportAR.cLine
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

        IF oplValid AND ipbf-ttImportAR.cCustNo NE "" THEN 
        DO:
            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ ipbf-ttImportAR.Company
                AND cust.cust-no EQ ipbf-ttImportAR.cCustNo
                AND lookup(cust.active,"A,E") > 0  NO-ERROR.
            IF NOT AVAILABLE cust THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Customer"
                    .
        END.
        IF oplValid AND ipbf-ttImportAR.cShipTo NE "" THEN 
        DO:
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company = ipbf-ttImportAR.Company
                AND shipto.cust-no = ipbf-ttImportAR.cCustNo
                AND shipto.ship-id = ipbf-ttImportAR.cShipTo NO-ERROR.
            IF NOT AVAILABLE cust THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Ship-To#"
                    .
        END.
        IF oplValid AND ipbf-ttImportAR.cTaxCode NE "" THEN 
        DO:
            FIND FIRST stax NO-LOCK
                WHERE stax.company = ipbf-ttImportAR.Company
                AND stax.tax-group = ipbf-ttImportAR.cTaxCode
                NO-ERROR.
            IF NOT AVAILABLE stax THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Tax Group"
                    .
        END.
        IF oplValid AND ipbf-ttImportAR.cTermsCode NE "" THEN 
        DO:
            FIND FIRST terms NO-LOCK 
                WHERE terms.company = ipbf-ttImportAR.Company
                AND terms.t-code EQ ipbf-ttImportAR.cTermsCode NO-ERROR.
            IF NOT AVAILABLE terms THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Terms"
                    .
        END.
        IF oplValid AND ipbf-ttImportAR.cLineAccount NE "" THEN 
        DO:
            FIND FIRST account NO-LOCK 
                WHERE account.company = ipbf-ttImportAR.Company
                AND account.TYPE <> "T"
                AND account.actnum = ipbf-ttImportAR.cLineAccount NO-ERROR.
            IF NOT AVAIL account THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Account".
        END.

        IF oplValid AND ipbf-ttImportAR.cLinePriceUom NE "" THEN 
        DO:
            IF LOOKUP(ipbf-ttImportAR.cLinePriceUom,"EA,MSF,M") LE 0 THEN
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid UOM".
        END.
        IF oplValid AND ipbf-ttImportAR.cLineCostUom NE "" THEN 
        DO:
            IF LOOKUP(ipbf-ttImportAR.cLineCostUom,"EA,M") LE 0 THEN
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Cost UOM".
        END. 

        IF oplValid AND ipbf-ttImportAR.cLineSalesman1 NE "" THEN 
        DO:
            FIND FIRST sman NO-LOCK WHERE
                sman.company EQ ipbf-ttImportAR.Company 
                AND sman.sman EQ ipbf-ttImportAR.cLineSalesman1 NO-ERROR.
            IF NOT AVAIL sman THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Sales Rep1".
        END.
        IF oplValid AND ipbf-ttImportAR.cLineSalesman2 NE "" THEN 
        DO:
            FIND FIRST sman NO-LOCK WHERE
                sman.company EQ ipbf-ttImportAR.Company 
                AND sman.sman EQ ipbf-ttImportAR.cLineSalesman2 NO-ERROR.
            
            IF NOT AVAIL sman THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Sales Rep2".
        END.
        IF oplValid AND ipbf-ttImportAR.cLineSalesman3 NE "" THEN 
        DO:
            FIND FIRST sman NO-LOCK WHERE
                sman.company EQ ipbf-ttImportAR.Company 
                AND sman.sman EQ ipbf-ttImportAR.cLineSalesman3 NO-ERROR.
            
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
       
    IF ipbf-ttImportAR.cCustNo EQ "" THEN NEXT.
    IF ipbf-ttImportAR.cInvNo EQ "" THEN NEXT.

    FIND FIRST ar-inv NO-LOCK
        WHERE ar-inv.company EQ ipbf-ttImportAR.Company
        AND ar-inv.inv-no EQ INT(ipbf-ttImportAR.cInvNo)
        NO-ERROR.
    IF NOT AVAILABLE ar-inv THEN 
    DO:
        iopiAdded = iopiAdded + 1.
        RUN pCreateNewInvoiceAR (ipbf-ttImportAR.Company,ipbf-ttImportAR.cCustNo, ipbf-ttImportAR.cInvDate, OUTPUT riARInv).
        FIND ar-inv EXCLUSIVE-LOCK 
            WHERE ROWID(ar-inv) EQ riARInv
            NO-ERROR.
        IF NOT AVAILABLE ar-inv THEN NEXT.
        /*Override defaults with values from import*/
        IF ipbf-ttImportAR.cInvNo NE "" THEN 
            ar-inv.inv-no = INTEGER(ipbf-ttImportAR.cInvNo).
        IF ipbf-ttImportAR.cInvDate NE "" THEN 
            ar-inv.inv-date = DATE(ipbf-ttImportAR.cInvDate).
        IF ipbf-ttImportAR.cDueDate NE "" THEN 
            ar-inv.due-date = DATE(ipbf-ttImportAR.cDueDate).
        IF ipbf-ttImportAR.cShipto NE "" THEN 
            ar-inv.ship-id = ipbf-ttImportAR.cShipTo.
        IF ipbf-ttImportAR.cPONum NE "" THEN 
            ar-inv.po-no = ipbf-ttImportAR.cPONum.
        IF ipbf-ttImportAR.cTaxCode NE "" THEN
        DO:
            FIND FIRST stax NO-LOCK 
                WHERE stax.company EQ ipbf-ttImportAR.Company
                AND stax.tax-group EQ ipbf-ttImportAR.cTaxCode
                NO-ERROR.
            IF AVAILABLE stax THEN 
                ar-inv.tax-code = ipbf-ttImportAR.cTaxCode.
        END.
        IF ipbf-ttImportAR.cTermsCode NE "" THEN 
            ar-inv.terms = ipbf-ttImportAR.cTermsCode.
        IF ipbf-ttImportAR.cDiscount NE "" THEN 
            ar-inv.disc-% = DECIMAL(ipbf-ttImportAR.cDiscount).               
        IF ipbf-ttImportAR.cDiscountDays NE "" THEN 
            ar-inv.disc-days = DECIMAL(ipbf-ttImportAR.cDiscountDays).
        IF ipbf-ttImportAR.cCarrier NE "" THEN 
            ar-inv.carrier = ipbf-ttImportAR.cCarrier.
        IF ipbf-ttImportAR.cFreight NE "" THEN 
            ar-inv.freight = DECIMAL(ipbf-ttImportAR.cFreight).                                     
    END. /*Not avail ar-inv*/
    RUN pCreateNewInvoiceLineAR (ROWID(ar-inv),ipbf-ttImportAR.cLine,ipbf-ttImportAR.cLineItemNo, OUTPUT riARInvl).
    FIND ar-invl EXCLUSIVE-LOCK 
        WHERE ROWID(ar-invl) EQ riARInvl
        NO-ERROR.
    IF NOT AVAILABLE ar-invl THEN NEXT.
    /*Override defaults with values from import*/
    ASSIGN 
        ar-invl.tax = ipbf-ttImportAR.cLineTax EQ 'Y'
        ar-invl.amt = DECIMAL(ipbf-ttImportAR.cLineAmount)
        .
    IF TRIM(ipbf-ttImportAR.cLineQuantity) NE "" THEN 
        ar-invl.qty = DECIMAL(ipbf-ttImportAR.cLineQuantity).
    ELSE 
        ar-invl.qty = 1.
    ar-invl.inv-qty = ar-invl.qty.
    IF ipbf-ttImportAR.cLineQuantityUom NE "" THEN 
        ar-invl.cons-uom = ipbf-ttImportAR.cLineQuantityUOM.
    ELSE 
        ar-invl.cons-uom = "EA".
    IF ipbf-ttImportAR.cLinePrice NE "" THEN 
        ar-invl.unit-pr = DECIMAL(ipbf-ttImportAR.cLinePrice).
    ELSE 
        ar-invl.unit-pr = ar-invl.amt.
    IF ipbf-ttImportAR.cLinePriceUom NE "" THEN 
        ar-invl.pr-qty-uom = ipbf-ttImportAR.cLinePriceUom.
    ELSE 
        ar-invl.pr-qty-uom = "EA".
    IF ipbf-ttImportAR.cLineAccount NE "" THEN 
        ar-invl.actnum = ipbf-ttImportAR.cLineAccount.
    IF ipbf-ttImportAR.cLine NE "" THEN 
        ar-invl.line = INTEGER(ipbf-ttImportAR.cLine).
    IF ipbf-ttImportAR.cLineItemNo NE "" THEN 
        ar-invl.i-no = ipbf-ttImportAR.cLineItemNo.
    IF ipbf-ttImportAR.cLineItemName NE "" THEN 
        ar-invl.i-name = ipbf-ttImportAR.cLineItemName.
    IF ipbf-ttImportAR.cLineItemDescription NE "" THEN                                 
        ar-invl.i-dscr = ipbf-ttImportAR.cLineItemDescription.     
    IF ipbf-ttImportAR.cLineCustomerLotNo NE "" THEN 
        ar-invl.lot-no = ipbf-ttImportAR.cLineCustomerLotNo.
    IF ipbf-ttImportAR.cLineDiscount NE "" THEN                            
        ar-invl.disc = DECIMAL(ipbf-ttImportAR.cLineDiscount).            
    IF ipbf-ttImportAR.cLineCost NE "" THEN 
        ar-invl.cost = DECIMAL(ipbf-ttImportAR.cLineCost).                
    IF ipbf-ttImportAR.cLineCostUom NE "" THEN 
        ar-invl.cons-uom = ipbf-ttImportAR.cLineCostUom.             
    IF ipbf-ttImportAR.cLineSalesman1 NE "" THEN 
        ar-invl.sman[1] = ipbf-ttImportAR.cLineSalesman1.
    IF ipbf-ttImportAR.cLineSalesman1Percent NE "" THEN 
        ar-invl.s-pct[1] = DECIMAL(ipbf-ttImportAR.cLineSalesman1Percent).
    IF ipbf-ttImportAR.cLineSalesman1Commission NE "" THEN 
        ar-invl.s-comm[1] = DECIMAL(ipbf-ttImportAR.cLineSalesman1Commission).
    IF ipbf-ttImportAR.cLineSalesman2 NE "" THEN 
        ar-invl.sman[2] = ipbf-ttImportAR.cLineSalesman2.
    IF ipbf-ttImportAR.cLineSalesman2Percent NE "" THEN 
        ar-invl.s-pct[2] = DECIMAL(ipbf-ttImportAR.cLineSalesman2Percent).
    IF ipbf-ttImportAR.cLineSalesman2Commission NE "" THEN 
        ar-invl.s-comm[3] = DECIMAL(ipbf-ttImportAR.cLineSalesman3Commission).
    IF ipbf-ttImportAR.cLineSalesman3 NE "" THEN 
        ar-invl.sman[3] = ipbf-ttImportAR.cLineSalesman3.
    IF ipbf-ttImportAR.cLineSalesman3Percent NE "" THEN 
        ar-invl.s-pct[3] = DECIMAL(ipbf-ttImportAR.cLineSalesman3Percent).
    IF ipbf-ttImportAR.cLineSalesman3Commission NE "" THEN 
        ar-invl.s-comm[3] = DECIMAL(ipbf-ttImportAR.cLineSalesman3Commission).
                                
    RUN pRecalculateARInvoiceHeader(ROWID(ar-inv), ar-invl.amt).

END PROCEDURE.

PROCEDURE pCreateNewInvoiceAR:
    /*------------------------------------------------------------------------------
         Purpose: Creates a new AR invoice, setting defaults based on key values
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER.
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER.
    DEFINE INPUT PARAMETER ipcInvDate AS CHARACTER.
    DEFINE OUTPUT PARAMETER opriARInv AS ROWID.
    
    DEFINE VARIABLE iNextInvoiceNumber     AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNextInvoiceLinkNumber AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-ar-inv FOR ar-inv.
    
    ASSIGN
        iNextInvoiceNumber     = 0
        iNextInvoiceLinkNumber = 0.
    FIND LAST bf-ar-inv NO-LOCK 
        USE-INDEX x-no 
        NO-ERROR.
    iNextInvoiceLinkNumber = IF AVAILABLE bf-ar-inv THEN bf-ar-inv.x-no + 1 ELSE 1.
    FIND FIRST ar-ctrl NO-LOCK  
        WHERE ar-ctrl.company EQ ipcCompany
        NO-ERROR.
    iNextInvoiceNumber = IF AVAILABLE ar-ctrl THEN ar-ctrl.last-inv + 1 ELSE 1.
    DO WHILE TRUE:
        FIND FIRST bf-ar-inv NO-LOCK 
            WHERE bf-ar-inv.company EQ ipcCompany
            AND bf-ar-inv.inv-no  EQ iNextInvoiceLinkNumber
            NO-ERROR.
        FIND FIRST inv-head NO-LOCK 
            WHERE inv-head.company EQ ipcCompany
            AND inv-head.inv-no  EQ iNextInvoiceLinkNumber
            NO-ERROR.
        IF NOT AVAILABLE bf-ar-inv AND NOT AVAILABLE inv-head THEN LEAVE.
        iNextInvoiceLinkNumber = iNextInvoiceLinkNumber + 1.
    END. 
    CREATE ar-inv.
    ASSIGN
        ar-inv.company  = ipcCompany
        ar-inv.inv-no   = iNextInvoiceNumber 
        ar-inv.x-no     = iNextInvoiceLinkNumber 
        ar-inv.inv-date = TODAY 
        ar-inv.cust-no  = ipcCustomer
        .
    IF ipcInvDate NE "" THEN ar-inv.inv-date = DATE(ipcInvDate).
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
            ar-inv.curr-code[1] = cust.curr-code.
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
            WHERE terms.t-code EQ cust.terms
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
    DEFINE INPUT PARAMETER ipLine AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipLineItem AS CHARACTER NO-UNDO.
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
        AND trim(string(ar-invl.LINE)) EQ ipLine
        AND ar-invl.i-no EQ ipLineItem NO-ERROR.

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
