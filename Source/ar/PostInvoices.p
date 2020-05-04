
/*------------------------------------------------------------------------
    File        : PostInvoices.p
    Purpose     : 

    Syntax      :

    Description : Given Range of Invoice #s, Post Invoices from Order Processing to AR		

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

DEFINE TEMP-TABLE ttInvoiceToPost
    FIELD riInvHead           AS ROWID
    FIELD company             AS CHARACTER
    FIELD postDate            AS DATE  
    FIELD postDatePeriod      AS INTEGER 
    FIELD customerID          AS CHARACTER 
    FIELD invoiceID           AS INTEGER 
    FIELD isFactored          AS LOGICAL
    FIELD isOKToPost          AS LOGICAL
    FIELD problemMessage      AS CHARACTER 
    FIELD orderID             AS INTEGER
    FIELD orderDate           AS DATE
    FIELD quantityTotal       AS INTEGER
    FIELD quantityTotalWeight AS DECIMAL
    FIELD currencyCode        AS CHARACTER 
    FIELD currencyExRate      AS DECIMAL
    FIELD currencyARAccount   AS CHARACTER
    FIELD accountAR           AS CHARACTER
    FIELD accountARFreight    AS CHARACTER
    FIELD accountARSales      AS CHARACTER
    FIELD accountARSalesTax   AS CHARACTER
    FIELD accountARDiscount   AS CHARACTER
    FIELD amountTotal         AS DECIMAL 
    FIELD amountTotalDiscount AS DECIMAL
    .
    
DEFINE TEMP-TABLE ttInvoiceLineToPost
    FIELD riInvLine              AS ROWID
    FIELD company                AS CHARACTER 
    FIELD invoiceID              AS INTEGER
    FIELD isOKToPost             AS LOGICAL
    FIELD problemMessage         AS CHARACTER  
    FIELD orderID                AS INTEGER
    FIELD itemID                 AS CHARACTER                        
    FIELD itemName               AS CHARACTER                     
    FIELD quantityOrdered        AS DECIMAL
    FIELD quantityInvoiced       AS DECIMAL 
    FIELD quantityShipped        AS DECIMAL
    FIELD pricePerUOM            AS DECIMAL
    FIELD priceUOM               AS CHARACTER
    FIELD priceTotal             AS DECIMAL
    FIELD costPerUOM             AS DECIMAL 
    FIELD costUOM                AS CHARACTER
    FIELD costTotal              AS DECIMAL 
    FIELD costDirectLabor        AS DECIMAL 
    FIELD costFixedOverhead      AS DECIMAL 
    FIELD costVariableOverhead   AS DECIMAL 
    FIELD costDirectMaterial     AS DECIMAL 
    FIELD costSource             AS CHARACTER
    FIELD costStdFreight         AS DECIMAL  
    FIELD costStdWarehouse       AS DECIMAL  
    FIELD costStdDeviation       AS DECIMAL  
    FIELD costStdManufacture     AS DECIMAL 
    FIELD costFull               AS DECIMAL 
    FIELD quantityInvoicedWeight AS DECIMAL 
    FIELD weightUOM              AS CHARACTER 
    FIELD accountAR              AS CHARACTER
    FIELD accountARFreight       AS CHARACTER
    FIELD accountARSales         AS CHARACTER
    FIELD accountARSalesTax      AS CHARACTER
    FIELD accountARDiscount      AS CHARACTER
    FIELD quantityPerSubUnit     AS DECIMAL
    FIELD amountWithoutDiscount  AS DECIMAL 
    FIELD amountDiscount         AS DECIMAL
    FIELD amount                 AS DECIMAL 
    FIELD locationID             AS CHARACTER
    FIELD bolID                  AS INTEGER
    .    
DEFINE TEMP-TABLE ttInvoiceMiscToPost
    FIELD riInvMisc      AS ROWID
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
    
DEFINE TEMP-TABLE ttOrderToUpdate
    FIELD riOeOrd AS ROWID 
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
    FIELD orderID             AS INTEGER
    FIELD orderLine           AS INTEGER 
    FIELD itemID              AS CHARACTER
    FIELD newQuantityInvoiced AS DECIMAL 
    FIELD newQuantityShipped  AS DECIMAL
    .    
DEFINE TEMP-TABLE ttCustomerToUpdate 
    FIELD riCust             AS ROWID
    FIELD orderBalanceChange AS DECIMAL.
    

DEFINE TEMP-TABLE ttItemToUpdate
    FIELD riItemfg AS ROWID
    .
    
{oe/ttSaveLine.i}
/*Program-level Handles for persistent procs*/
DEFINE VARIABLE ghNotesProcs    AS HANDLE  NO-UNDO.

/*Program-level variables*/
DEFINE VARIABLE giRunID         AS INTEGER NO-UNDO.
DEFINE VARIABLE gdtPostDate     AS DATE    NO-UNDO.
DEFINE VARIABLE giPeriod        AS INTEGER NO-UNDO.

/*Settings*/
DEFINE VARIABLE glBlockZeroCost AS LOGICAL NO-UNDO.

    
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


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

RUN pGetSettings(ipcCompany).

/*Build the master list of invoices based on criteria*/
RUN pBuildInvoicesToPost(ipcCompany, 
    ipiInvNoStart, ipiInvNoEnd,
    ipdtInvDateStart, ipdtInvDateEnd,
    ipcCustomerIDStart, ipcCustomerIDEnd,
    OUTPUT oplError, OUTPUT opcMessage).

/*Process the master list of invoices for reporting and/or posting*/
RUN pProcessInvoicesToPost (ipcCompany, ipdtPostDate).

/*IF LOOKUP("ReportFirst",ipcOptions) GT 0 OR LOOKUP("ReportBoth",ipcOptions) GT 0 THEN   */
/*DO:                                                                                     */
/*    RUN pBuildInvoicesReport.                                                           */
/*    IF LOOKUP("Post",ipcOptions) GT 0 THEN                                              */
/*        RUN pPostInvoices.                                                              */
/*    IF LOOKUP("ReportLast",ipcOptions) GT 0 OR LOOKUP("ReportBoth",ipcOptions) GT 0 THEN*/
/*        RUN pShowInvoicesReport.                                                        */

DELETE OBJECT ghNotesProcs.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pAlignMultiInvoiceLinesWithMaster PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a multi-invoice, align the child invoice lines with the master invoice
     Notes:  includes the former "create-save-line" procedure that used to be a disaster 
     with reftable read/write
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-master-inv-head FOR inv-head.
    DEFINE OUTPUT PARAMETER oplInvalid AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-child-inv-head FOR inv-head.
    DEFINE BUFFER bf-child-inv-line FOR inv-line.
    DEFINE BUFFER bf-child-inv-misc FOR inv-misc.
    
    DEFINE VARIABLE lValid AS LOGICAL NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF inv-line.
    DISABLE TRIGGERS FOR LOAD OF inv-misc.
    
    FOR EACH bf-child-inv-head NO-LOCK
        WHERE bf-child-inv-head.company  EQ ipbf-master-inv-head.company
        AND bf-child-inv-head.cust-no       EQ ipbf-master-inv-head.cust-no
        AND bf-child-inv-head.inv-no        EQ ipbf-master-inv-head.inv-no
        AND bf-child-inv-head.multi-invoice EQ NO:
        lValid = YES.
        FOR EACH bf-child-inv-line EXCLUSIVE-LOCK 
            WHERE bf-child-inv-line.r-no EQ bf-child-inv-head.r-no:
            CREATE ttSaveLine.
            ASSIGN
                //ttSaveLine.sessionID   = "save-line" + STRING(v-trnum,"9999999999")
                ttSaveLine.invLineRNo  = bf-child-inv-line.r-no
                ttSaveLine.invHeadRNo  = ipbf-master-inv-head.r-no
                ttSaveLine.invRowiD    = ROWID(bf-child-inv-line)
                bf-child-inv-line.r-no = ipbf-master-inv-head.r-no
                .
        END.

        FOR EACH bf-child-inv-misc EXCLUSIVE-LOCK 
            WHERE bf-child-inv-misc.r-no EQ bf-child-inv-head.r-no:
            CREATE ttSaveLine.
            ASSIGN
                //ttSaveLine.sessionID  = "save-line" + STRING(v-trnum,"9999999999")
                ttSaveLine.invMiscRNo  = bf-child-inv-misc.r-no
                ttSaveLine.invHeadRNo  = ipbf-master-inv-head.r-no
                ttSaveLine.invRowID    = ROWID(bf-child-inv-misc)
                bf-child-inv-misc.r-no = ipbf-master-inv-head.r-no
                .
        END.
    END.
    oplInvalid = NOT lValid.
    
END PROCEDURE.

PROCEDURE pBuildInvoicesToPost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given Criteria Range, build invoices to post
     Notes:  Will process multi-invoices and "link-up" the inv-lines to one master
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiInvNoStart AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiInvNoEnd AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtInvDateStart AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipdtInvDateEnd AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerIDStart AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerIDEnd AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-inv-head                   FOR inv-head.
    DEFINE BUFFER bf-inv-line                   FOR inv-line.
    DEFINE BUFFER bf-inv-misc                   FOR inv-misc.
    DEFINE BUFFER bf-cust                       FOR cust.
    DEFINE BUFFER bf-MultiInvoiceChild-inv-head FOR inv-head.
    
    DEFINE VARIABLE lInvalidMultiInvoice AS LOGICAL NO-UNDO.

    RUN pInitialize.        
    FOR EACH bf-inv-head NO-LOCK
        WHERE bf-inv-head.company  EQ ipcCompany
        AND bf-inv-head.printed  EQ YES
        AND bf-inv-head.inv-no   GT 0
        AND bf-inv-head.inv-no   GE ipiInvNoStart
        AND bf-inv-head.inv-no   LE ipiInvNoEnd
        AND bf-inv-head.inv-date GE ipdtInvDateStart
        AND bf-inv-head.inv-date LE ipdtInvDateEnd
        AND bf-inv-head.cust-no  GE ipcCustomerIDStart
        AND bf-inv-head.cust-no  LE ipcCustomerIDEnd    
        AND (CAN-FIND(FIRST bf-inv-line WHERE bf-inv-line.r-no EQ bf-inv-head.r-no)
        OR CAN-FIND(FIRST bf-inv-misc WHERE bf-inv-misc.r-no = bf-inv-head.r-no )
        OR bf-inv-head.multi-invoice)
        AND bf-inv-head.stat     NE "H"
        USE-INDEX prnt,
        FIRST bf-cust NO-LOCK
        WHERE bf-cust.company EQ bf-inv-head.company
        AND bf-cust.cust-no EQ bf-inv-head.cust-no
        AND ((bf-cust.inv-meth EQ ? AND bf-inv-head.multi-invoice) OR (bf-cust.inv-meth NE ? AND NOT bf-inv-head.multi-invoice))  /*Filter multi-invoices correctly based on customer*/
//        TRANSACTION - REFACTOR - why transaction?
        :
        /*Add CustomerList Exclusions*/
        /*TBD*/
        
        IF fIsWritable(ROWID(bf-inv-head)) THEN 
        DO:
            CREATE ttInvoiceToPost.
            ASSIGN 
                ttInvoiceToPost.riInvHead  = ROWID(bf-inv-head)
                ttInvoiceToPost.isFactored = fIsFactored(bf-cust.factored, bf-inv-head.r-no)
                ttInvoiceToPost.invoiceID  = bf-inv-head.inv-no
                ttInvoiceToPost.company    = bf-inv-head.company
                ttInvoiceToPost.customerID = bf-inv-head.cust-no
                ttInvoiceToPost.isOKToPost = YES
                .
            
            /*Manage Multi Invoices*/
            IF bf-inv-head.multi-invoice THEN 
            DO:
                RUN pAlignMultiInvoiceLinesWithMaster(BUFFER bf-inv-head, OUTPUT lInvalidMultiInvoice).
                IF lInvalidMultiInvoice THEN 
                    DELETE ttInvoiceToPost.
            END. /*Multi-invoice header*/
        END. /*inv-head is writable*/
    END.  /*Each Inv-head that meets range criteria*/
    
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
    
    DEF BUFFER bf-child-inv-head FOR inv-head.
    
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
                   
    END.  /*valid ar-ctrl*/
    
END PROCEDURE.

PROCEDURE pGetCurrencyCodeAndRate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given customer , get applicable currency code and rate
     Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-inv-head FOR inv-head.
    DEFINE OUTPUT PARAMETER opcCurrencyCode AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCurrencyExchangeRate AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCurrencyARAccount AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-cust     FOR cust.
    DEFINE BUFFER bf-company  FOR company.
    DEFINE BUFFER bf-currency FOR currency.
    
    /*        IF bf-inv-head.terms NE "CASH"              AND             */
    /*            cCompCurr NE ""                    AND                  */
    /*            cCompCurr NE bf-inv-head.curr-code[1] THEN              */
    /*            FIND FIRST currency NO-LOCK                             */
    /*                WHERE currency.company     EQ bf-inv-head.company   */
    /*                AND currency.c-code      EQ bf-inv-head.curr-code[1]*/
    /*                AND currency.ar-ast-acct NE ""                      */
    /*                AND currency.ex-rate     GT 0                       */
    /*                NO-ERROR.                                           */

    opcCurrencyCode = ipbf-inv-head.curr-code[1].
    IF opcCurrencyCode EQ "" THEN 
        FIND FIRST bf-cust NO-LOCK 
            WHERE bf-cust.company EQ ipbf-inv-head.company
            AND bf-cust.cust-no EQ ipbf-inv-head.cust-no 
            NO-ERROR.
    IF AVAIL bf-cust THEN 
        ASSIGN 
            opcCurrencyCode = bf-cust.curr-code
            .
    IF opcCurrencyCode EQ "" THEN 
    DO:
        FIND FIRST bf-company NO-LOCK    
            WHERE bf-company.company EQ ipbf-inv-head.company 
            NO-ERROR.
        IF AVAIL company THEN 
            opcCurrencyCode = company.curr-code.
    END.            
    IF opcCurrencyCode NE "" THEN 
        FIND bf-currency NO-LOCK 
            WHERE bf-currency.company EQ ipbf-inv-head.company
            AND bf-currency.c-code EQ opcCurrencyCode
            AND bf-currency.ar-ast-acct NE ""
            AND bf-currency.ex-rate GT 0 
            NO-ERROR.
    IF AVAIL bf-currency THEN 
        ASSIGN 
            opdCurrencyExchangeRate = bf-currency.ex-rate 
            opcCurrencyARAccount    = bf-currency.ar-ast-acct
            .
   

END PROCEDURE.

PROCEDURE pGetSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Gets all required NK1 settings for posting run
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (ipcCompany, "INVPOST", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN glBlockZeroCost = cReturn EQ "YES".
    

END PROCEDURE.

PROCEDURE pInitialize PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Initializes the post
     Notes:
    ------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttInvoiceToPost.
    EMPTY TEMP-TABLE ttInvoiceLineToPost.
    EMPTY TEMP-TABLE ttSaveLine.

END PROCEDURE.

PROCEDURE pPostGL PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Posts all pending GL Transactions
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiRunID AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE riGLTrans    AS ROWID NO-UNDO.
    
    DEFINE VARIABLE cAccountDesc LIKE gltrans.tr-dscr NO-UNDO. 
   
    /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
    /*        FOR EACH tt-gl,                                        */
    /*            FIRST gltrans WHERE ROWID(gltrans) EQ tt-gl.row-id:*/
    /*            DELETE gltrans.                                    */
    /*        END.                                                   */
    /*        EMPTY TEMP-TABLE tt-gl.                                */
        
    FOR EACH ttGLTransaction
        WHERE ttGLTransaction.transactionType EQ 'work-line'
        BREAK BY ttGLTransaction.account
        BY ttGLTransaction.invoiceID:
        
        ACCUMULATE ttGlTransaction.amount (TOTAL BY ttGLTransaction.invoiceID).
        
        IF LAST-OF(ttGlTransaction.invoiceID) THEN 
            RUN pCreateGLTrans(BUFFER ttGLTransaction, 
                - (ACCUMULATE TOTAL BY ttGLTransaction.invoiceID ttGlTransaction.amount),
                ipiRunID,
                OUTPUT riGLTrans).
                           
    END. /* each work-line ttGLTransaction */
    
    FOR EACH ttGLTransaction
        WHERE ttGLTransaction.transactionType EQ 'work-misc'
        BREAK BY ttGLTransaction.account
        BY ttGLTransaction.invoiceID:
        
        ACCUMULATE ttGlTransaction.amount (TOTAL BY ttGLTransaction.invoiceID).
        
        IF LAST-OF(ttGlTransaction.invoiceID) THEN 
            RUN pCreateGLTrans(BUFFER ttGLTransaction, 
                - (ACCUMULATE TOTAL BY ttGLTransaction.invoiceID ttGlTransaction.amount),
                ipiRunID,
                OUTPUT riGLTrans).
                           
    END. /* each work-misc ttGLTransaction */
    
    FOR EACH ttGLTransaction
        WHERE ttGLTransaction.transactionType EQ 'work-tax'
        BREAK BY ttGLTransaction.account
        BY ttGLTransaction.invoiceID:
        
        ACCUMULATE ttGlTransaction.amount (TOTAL BY ttGLTransaction.invoiceID).
        
        IF LAST-OF(ttGlTransaction.invoiceID) THEN 
            RUN pCreateGLTrans(BUFFER ttGLTransaction, 
                - (ACCUMULATE TOTAL BY ttGLTransaction.invoiceID ttGlTransaction.amount),
                ipiRunID,
                OUTPUT riGLTrans).
                           
    END. /* each work-tax ttGLTransaction */
    
    /** POST SALES TAX TO G/L TRANS **/
    FOR EACH tt-report NO-LOCK
        WHERE tt-report.term-id EQ ""
        AND tt-report.key-01  EQ "work-tax"
            
        BREAK BY tt-report.key-02
        BY tt-report.key-03:

        ACCUMULATE dec(tt-report.key-05) (TOTAL BY tt-report.key-03).

        IF LAST-OF(tt-report.key-03) THEN 
        DO:
            RUN get-tr-dscr (INT(tt-report.key-03), OUTPUT cAccountDesc).

            CREATE tt-gl.
            CREATE gltrans.
            ASSIGN
                tt-gl.row-id    = ROWID(gltrans)
                gltrans.company = cocode
                gltrans.actnum  = tt-report.key-02
                gltrans.jrnl    = "OEINV"
                gltrans.tr-dscr = TRIM(cAccountDesc) + " TAX"
                gltrans.tr-date = dtPostDate
                gltrans.tr-amt  = - (ACCUMULATE TOTAL BY tt-report.key-03 dec(tt-report.key-05))
                gltrans.period  = tran-period
                gltrans.trnum   = v-trnum
                .
            RELEASE gltrans.
        END. /* last actnum */
    END. /* each work-tax */

    /** POST CURRENCY TO G/L TRANS **/
    FOR EACH tt-report NO-LOCK
        WHERE tt-report.term-id EQ ""
        AND tt-report.key-01  EQ "work-curr"
            
        BREAK BY tt-report.key-02:

        ACCUMULATE dec(tt-report.key-05) (TOTAL BY tt-report.key-02).

        IF LAST-OF(tt-report.key-02) THEN 
        DO:
            CREATE tt-gl.
            CREATE gltrans.
            ASSIGN
                tt-gl.row-id    = ROWID(gltrans)
                gltrans.company = cocode
                gltrans.actnum  = tt-report.key-02
                gltrans.jrnl    = "OEINV"
                gltrans.tr-dscr = "ORDER ENTRY INVOICE CURRENCY GAIN/LOSS"
                gltrans.tr-date = dtPostDate
                gltrans.tr-amt  = - (ACCUMULATE TOTAL BY tt-report.key-02 dec(tt-report.key-05))
                gltrans.period  = tran-period
                gltrans.trnum   = v-trnum
                .

            RELEASE gltrans.
        END. /* last actnum */
    END. /* each work-tax */

    FOR EACH tmp-work-job
        BREAK BY tmp-work-job.fg
        BY tmp-work-job.actnum
        BY tmp-work-job.inv-no:

        ACCUMULATE tmp-work-job.amt (TOTAL BY tmp-work-job.inv-no).

        IF LAST-OF(tmp-work-job.inv-no) THEN 
        DO:
            RUN get-tr-dscr (tmp-work-job.inv-no, OUTPUT cAccountDesc).

            CREATE tt-gl.
            CREATE gltrans.
            ASSIGN
                tt-gl.row-id    = ROWID(gltrans)
                gltrans.company = cocode
                gltrans.actnum  = tmp-work-job.actnum
                gltrans.jrnl    = "OEINV"
                gltrans.tr-date = dtPostDate
                gltrans.period  = tran-period
                gltrans.trnum   = v-trnum
                .

            IF tmp-work-job.fg THEN
                ASSIGN
                    gltrans.tr-amt  = - (ACCUMULATE TOTAL BY tmp-work-job.inv-no tmp-work-job.amt)
                    gltrans.tr-dscr = TRIM(cAccountDesc) + " FG".
            ELSE
                ASSIGN
                    gltrans.tr-amt  = (ACCUMULATE TOTAL BY tmp-work-job.inv-no tmp-work-job.amt)
                    gltrans.tr-dscr = TRIM(cAccountDesc) + " COGS".

            RELEASE gltrans.
        END.
    END. /* each work-job */

    /** POST FREIGHT TO G/L TRANS **/
    CREATE tt-gl.
    CREATE gltrans.
    ASSIGN
        tt-gl.row-id    = ROWID(gltrans)
        gltrans.company = cocode
        gltrans.actnum  = v-ar-freight
        gltrans.jrnl    = "OEINV"
        gltrans.tr-dscr = "ORDER ENTRY INVOICE FREIGHT"
        gltrans.tr-date = dtPostDate 
        gltrans.tr-amt  = v-post-freight
        gltrans.period  = tran-period
        gltrans.trnum   = v-trnum
        .
    RELEASE gltrans. 

    /** POST DISCOUNT TO G/L TRANS **/
    CREATE tt-gl.
    CREATE gltrans.
    ASSIGN
        tt-gl.row-id    = ROWID(gltrans) 
        gltrans.company = cocode
        gltrans.actnum  = v-ar-disc
        gltrans.jrnl    = "OEINV"
        gltrans.tr-dscr = "ORDER ENTRY INVOICE DISCOUNT"
        gltrans.tr-date = dtPostDate
        gltrans.tr-amt  = v-post-disc
        gltrans.period  = tran-period
        gltrans.trnum   = v-trnum
        .
    RELEASE gltrans.

    /** POST CASH TO G/L TRANS **/
    IF v-post-cash NE 0 THEN 
    DO:
        CREATE tt-gl.
        CREATE gltrans.
        ASSIGN
            tt-gl.row-id    = ROWID(gltrans)
            gltrans.company = cocode
            gltrans.actnum  = ar-ctrl.cash-act
            gltrans.jrnl    = "CASHR"
            gltrans.tr-dscr = "CASH RECEIPT - INVOICE"
            gltrans.tr-date = dtPostDate
            gltrans.tr-amt  = v-post-cash
            gltrans.period  = tran-period
            gltrans.trnum   = v-trnum
            v-post-cash     = - v-post-cash
            .
        RELEASE gltrans.
    END.

    /** OFFSET ENTRY TO G/L **/
    CREATE tt-gl.
    CREATE gltrans.
    ASSIGN
        tt-gl.row-id    = ROWID(gltrans)
        gltrans.company = cocode
        gltrans.actnum  = v-ar-acct
        gltrans.jrnl    = "OEINV"
        gltrans.tr-dscr = "ORDER ENTRY INVOICE"
        gltrans.tr-date = dtPostDate
        gltrans.tr-amt  = v-post-total
        gltrans.period  = tran-period
        gltrans.trnum   = v-trnum
        .
    RELEASE gltrans.
    

END PROCEDURE.

PROCEDURE pPostInvoices PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE BUFFER bf-inv-head FOR inv-head.
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER bf-inv-misc FOR inv-misc.
    DEFINE BUFFER bf-ar-inv   FOR ar-inv.
    DEFINE BUFFER bf-ar-invl  FOR ar-invl.
    DEFINE BUFFER bf-oe-ordl  FOR oe-ordl.
    DEFINE BUFFER bf-oe-boll  FOR oe-boll.
    DEFINE BUFFER bf-oe-bolh  FOR oe-bolh.
    
    DEFINE VARIABLE iLine    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iXno     AS INTEGER NO-UNDO.
    DEFINE VARIABLE riArInv  AS ROWID   NO-UNDO.
    DEFINE VARIABLE riArInvl AS ROWID   NO-UNDO.
          
    /*    DISABLE TRIGGERS FOR LOAD OF inv-head.*/
    /*    DISABLE TRIGGERS FOR LOAD OF inv-line.*/
    /*    DISABLE TRIGGERS FOR LOAD OF oe-ord.  */
    /*    DISABLE TRIGGERS FOR LOAD OF oe-ordl. */
    /*    DISABLE TRIGGERS FOR LOAD OF itemfg.  */
    /*    DISABLE TRIGGERS FOR LOAD OF oe-relh. */
    /*    DISABLE TRIGGERS FOR LOAD OF oe-rell. */
    
    FOR EACH ttInvoiceToPost NO-LOCK
        WHERE ttInvoiceToPost.isOKToPost,
        FIRST bf-inv-head NO-LOCK 
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
            WHERE ttInvoiceLineToPost.isOKToPost,
            FIRST bf-inv-line NO-LOCK
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
            
        END. /*each invoice line*/
        FOR EACH ttInvoiceMiscToPost NO-LOCK
            WHERE ttInvoiceMiscToPost.isOKToPost,
            FIRST bf-inv-misc NO-LOCK
            WHERE ROWID(bf-inv-misc) EQ ttInvoiceMiscToPost.riInvMisc:
         
            RUN pCreateARInvLIne(BUFFER bf-inv-head, BUFFER bf-inv-misc, BUFFER ttInvoiceMiscToPost, iXno, iLine, OUTPUT riArInvl).
            iLine = iLine + 1.
            
        END. /*each invoice misc*/
    END. /*each invoice to post*/
    
    FOR EACH ttOrderLineToUpdate NO-LOCK,
        FIRST bf-oe-ordl EXCLUSIVE-LOCK 
        WHERE ROWID(bf-oe-ordl) EQ ttOrderLineToUpdate.riOeOrdl:
        
        ASSIGN 
            bf-oe-ordl.t-inv-qty  = bf-oe-ordl.t-inv-qty + ttOrderLineToUpdate.newQuantityInvoiced
            bf-oe-ordl.t-ship-qty = bf-oe-ordl.t-ship-qty + ttOrderLineToUpdate.newQuantityShipped
            . 
    END.
    FOR EACH ttBOLlineToUpdate NO-LOCK,
        FIRST bf-oe-boll EXCLUSIVE-LOCK
        WHERE ROWID(bf-oe-boll) EQ ttBolLineToUpdate.riOeBoll,
        FIRST bf-oe-bolh EXCLUSIVE-LOCK 
        WHERE ROWID(bf-oe-bolh) EQ ttBolLineToUpdate.riOeBolh:
        ASSIGN 
            bf-oe-boll.inv-no = ttBolLineToUpdate.invoiceID
            bf-oe-bolh.inv-no = ttBolLineToUpdate.invoiceID
            .
    END.
END PROCEDURE.

PROCEDURE pBuildInvoicesReport PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Reports on all ttInvoicesToPost
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

PROCEDURE pProcessInvoicesToPost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  For each InvoiceToPost, create all required temp-tables for 
        processing of report and posting
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtPostDate AS DATE NO-UNDO.
    //DEFINE INPUT PARAMETER ip-list-post AS CHARACTER NO-UNDO.
    //DEFINE INPUT PARAMETER oeprep-log   AS LOGICAL   NO-UNDO.

    DEFINE BUFFER bf-inv-head FOR inv-head.
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER bf-inv-misc FOR inv-misc.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-oe-ordl  FOR oe-ordl.
    DEFINE BUFFER bf-oe-ord   FOR oe-ord.
    DEFINE BUFFER bf-fgcat    FOR fgcat.
    DEFINE BUFFER bf-period   FOR period.
    DEFINE BUFFER bf-oe-bolh  FOR oe-bolh.
    DEFINE BUFFER bf-oe-boll  FOR oe-boll.
    
    
    DEFINE VARIABLE iPeriod                   AS INTEGER.
    DEFINE VARIABLE cDefaultAccountAR         AS CHARACTER.
    DEFINE VARIABLE cDefaultAccountARFreight  AS CHARACTER.
    DEFINE VARIABLE cDefaultAccountARSales    AS CHARACTER.
    DEFINE VARIABLE cDefaultAccountARSalesTax AS CHARACTER.
    DEFINE VARIABLE cDefaultAccountARDiscount AS CHARACTER.
    DEFINE VARIABLE lAccountError             AS LOGICAL.
    DEFINE VARIABLE cAccountErrorMessage      AS CHARACTER.
    DEFINE VARIABLE dDiscountAmount           AS DECIMAL.
    DEFINE VARIABLE dInvoiceAmount            AS DECIMAL.

        
    /*    DEFINE VARIABLE ld-t            AS DECIMAL FORMAT "->>>>9.99" EXTENT 3 NO-UNDO.*/
    /*    DEFINE VARIABLE ld-pton         AS DECIMAL FORMAT "->>>9.999" NO-UNDO.         */
    /*    DEFINE VARIABLE v-close-line-ok AS LOGICAL INITIAL NO.                         */
    /*    DEFINE VARIABLE v-first         AS LOG     INIT YES.                           */
    /*    DEFINE VARIABLE v-tot-frt       AS DECIMAL NO-UNDO.                            */
  
  

     
    FIND FIRST bf-period NO-LOCK 
        WHERE bf-period.company EQ ipcCompany
        AND bf-period.pst LE ipdtPostDate
        AND bf-period.pend GE ipdtPostDate
        NO-ERROR.
    IF AVAILABLE bf-period THEN 
        iPeriod = bf-period.pnum.                    
    
    RUN pGetAccountDefaults(ipcCompany, 
        OUTPUT cDefaultAccountAR, OUTPUT cDefaultAccountARFreight, OUTPUT cDefaultAccountARSales, OUTPUT cDefaultAccountARSalesTax, OUTPUT cDefaultAccountARDiscount,
        OUTPUT lAccountError, OUTPUT cAccountErrorMessage).
 
    InvoiceBlock:
    FOR EACH ttInvoiceToPost,    
        FIRST bf-inv-head NO-LOCK 
        WHERE ROWID(bf-inv-head) EQ ttInvoiceToPost.riInvHead
        BY ttInvoiceToPost.invoiceID:
         
        IF iPeriod EQ 0 THEN 
        DO:
            ASSIGN 
                ttInvoiceToPost.isOKToPost     = NO
                ttInvoiceToPost.problemMessage = 'Period is not available for post date of ' + STRING(ipdtPostDate) 
                .
            NEXT InvoiceBlock.
        END.
        RUN pGetCurrencyCodeAndRate(BUFFER bf-inv-head, OUTPUT ttInvoiceToPost.currencyCode, OUTPUT ttInvoiceToPost.currencyExRate, OUTPUT ttInvoiceToPost.currencyARAccount).        
        
        ASSIGN 
            ttInvoiceToPost.accountAR         = cDefaultAccountAR
            ttInvoiceToPost.accountARFreight  = cDefaultAccountARFreight
            ttInvoiceToPost.accountARSales    = cDefaultAccountARSales
            ttInvoiceToPost.accountARSalesTax = cDefaultAccountARSalesTax
            ttInvoiceToPost.accountArDiscount = cDefaultAccountARDiscount
            .
        IF lAccountError THEN 
        DO:
            ASSIGN 
                ttInvoiceToPost.isOKToPost     = NO
                ttInvoiceToPost.problemMessage = cAccountErrorMessage.
            NEXT InvoiceBlock.
        END.
        
        LineBlock:
        FOR EACH bf-inv-line NO-LOCK
            WHERE bf-inv-line.r-no EQ bf-inv-head.r-no
            USE-INDEX r-no
            BREAK BY bf-inv-line.ord-no:
            
            CREATE ttInvoiceLineToPost.
            ASSIGN 
                ttInvoiceLineToPost.riInvLine         = ROWID(bf-inv-line)
                ttInvoiceLineToPost.company           = bf-inv-line.company
                ttInvoiceLineToPost.invoiceID         = ttInvoiceToPost.invoiceID
                ttInvoiceLineToPost.orderID           = bf-inv-line.ord-no
                ttInvoiceLineToPost.itemID            = bf-inv-line.i-no
                ttInvoiceLineToPost.itemName          = bf-inv-line.i-name
                ttInvoiceLineToPost.quantityOrdered   = bf-inv-line.qty
                ttInvoiceLineToPost.quantityInvoiced  = bf-inv-line.inv-qty
                ttInvoiceLineToPost.quantityShipped   = bf-inv-line.ship-qty
                ttInvoiceLineToPost.pricePerUOM       = bf-inv-line.price
                ttInvoiceLineToPost.priceUOM          = bf-inv-line.pr-uom
                ttInvoiceLineToPost.priceTotal        = bf-inv-line.t-price
                ttInvoiceLineToPost.costPerUOM        = bf-inv-line.cost
                ttInvoiceLineToPost.isOKToPost        = YES
                ttInvoiceToPost.quantityTotal         = ttInvoiceToPost.quantityTotal + bf-inv-line.inv-qty
                ttInvoiceLineToPost.accountAR         = ttInvoiceToPost.accountAR
                ttInvoiceLineToPost.accountARFreight  = ttInvoiceToPost.accountARFreight
                ttInvoiceLineToPost.accountARSales    = ttInvoiceToPost.accountARSales
                ttInvoiceLineToPost.accountARSalesTax = ttInvoiceToPost.accountARSalesTax
                ttInvoiceLineToPost.accountARDiscount = ttInvoiceToPost.accountArDiscount
                ttInvoiceLineToPost.bol               = bf-inv-head.bol-no
                .

            /*FG Dependent fields*/
            IF bf-inv-line.i-no NE "" THEN 
                FIND FIRST bf-itemfg NO-LOCK
                    WHERE bf-itemfg.company EQ bf-inv-line.company
                    AND bf-itemfg.i-no EQ bf-inv-line.i-no
                    NO-ERROR.
            IF NOT AVAILABLE bf-itemfg THEN 
            DO:
                ASSIGN 
                    ttInvoiceLineToPost.isOKToPost     = NO
                    ttInvoiceLineToPost.problemMessage = "Invalid FG Item " + bf-inv-line.i-no
                    .
                NEXT LineBlock.
            END.            
            ASSIGN 
                ttInvoiceLineToPost.quantityInvoicedWeight = bf-inv-line.qty * bf-itemfg.weight-100 / 100
                ttInvoiceLineToPost.weightUOM              = "LB"
                ttInvoiceToPost.quantityTotalWeight        = ttInvoiceToPost.quantityTotalWeight + ttInvoiceLineToPost.quantityInvoicedWeight
                ttInvoiceLineToPost.quantityPerSubUnit     = MAX(bf-itemfg.case-count, 1)
                ttInvoiceLineToPost.costFull               = bf-itemfg.spare-dec-1
                .
    
            /*BOL Dependent Fields*/
            FIND FIRST bf-oe-boll NO-LOCK 
                WHERE bf-oe-boll.company EQ bf-inv-line.company
                AND bf-oe-boll.b-no EQ bf-inv-line.b-no
                AND bf-oe-boll.i-no EQ bf-inv-line.i-no 
                NO-ERROR.     
            
            IF AVAILABLE bf-oe-boll THEN 
            DO:
                FIND FIRST bf-oe-bolh NO-LOCK  
                    WHERE bf-oe-bolh.b-no EQ bf-oe-bolh.b-no 
                    NO-ERROR.
                FIND FIRST ttBolLineToUpdate
                    WHERE ttBolLineToUpdate.riBOLLine EQ ROWID(bf-oe-boll)
                    NO-ERROR.
                IF NOT AVAILABLE ttBolLineToUpdate THEN 
                DO:
                    CREATE ttBolLineToUpdate.
                    ASSIGN 
                        ttBolLineToUpdate.riOeBoll  = ROWID(bf-oe-boll)
                        ttBollineToUpdate.invoiceID = ttInvoiceLineToPost.invoiceID
                        .
                END. /*create new bol line to update*/
                
                IF AVAILABLE bf-oe-bolh THEN 
                    ASSIGN 
                        ttBolLineToUpdate.riOeBolh = ROWID(bf-oe-bolh)
                        ttInvoiceLineToPost.bolID  = bf-oe-bolh.bol-no 
                        .
                ASSIGN 
                    ttInvoiceLineToPost.locationID = bf-oe-boll.loc
                    .
            END.
            
            /*Product Category Dependencies*/
            FIND FIRST bf-fgcat NO-LOCK
                WHERE bf-fgcat.company EQ bf-itemfg.company
                AND bf-fgcat.procat  EQ bf-itemfg.procat
                NO-ERROR.
            IF AVAILABLE bf-fgcat THEN 
            DO:
                /*Override default GL account for sales*/
                ASSIGN 
                    lAccountError        = NO
                    cAccountErrorMessage = ""
                    .
                RUN pCheckAccount(bf-fgcat.company, bf-fgcat.glacc, "FG Category of " + bf-fgcat.procat, "GL Account", OUTPUT lAccountError, OUTPUT cAccountErrorMessage). 
                IF NOT lAccountError THEN 
                    ttInvoiceLineToPost.accountARSales = bf-fgcat.glacc.
            END.
            
            /*Order Dependencies*/
            IF bf-inv-line.ord-no NE 0 THEN 
                FIND FIRST bf-oe-ordl NO-LOCK
                    WHERE bf-oe-ordl.company EQ bf-inv-line.company
                    AND bf-oe-ordl.ord-no  EQ bf-inv-line.ord-no
                    AND bf-oe-ordl.line    EQ bf-inv-line.line
                    AND bf-oe-ordl.i-no    EQ bf-inv-line.i-no
                    USE-INDEX ord-no 
                    NO-ERROR.
            IF bf-inv-line.ord-no NE 0 AND AVAILABLE bf-oe-ordl THEN 
            DO:
                FIND FIRST bf-oe-ord NO-LOCK
                    WHERE bf-oe-ord.company EQ bf-oe-ordl.company
                    AND bf-oe-ord.ord-no  EQ bf-oe-ordl.ord-no
                    NO-ERROR.
                IF bf-oe-ordl.cas-cnt NE 0 THEN 
                    ttInvoiceLineToPost.quantityPerSubUnit = bf-oe-ordl.cas-cnt. 
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
                    ttOrderLineToUpdate.newQuantityInvoiced = ttOrderLineToUpdate.newQuantityInvoiced + bf-inv-line.inv-qty
                    ttOrderLineToUpdate.newQuantityShipped  = ttOrderLineToUpdate.newQuantityShipped + bf-inv-line.ship-qty
                    .
            END.
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
            IF FIRST(bf-inv-line.ord-no) THEN
                ASSIGN
                    ttInvoiceToPost.orderID   = bf-inv-line.ord-no
                    ttInvoiceToPost.orderDate = bf-inv-line.ord-date
                    .

            RUN oe/GetCostInvl.p (ROWID(bf-inv-line),
                OUTPUT ttInvoiceLineToPost.costDirectLabor, OUTPUT ttInvoiceLineToPost.costFixedOverhead,
                OUTPUT ttInvoiceLineToPost.costVariableOverhead, OUTPUT ttInvoiceLineToPost.costDirectMaterial,
                OUTPUT ttInvoiceLineToPost.costPerUOM, OUTPUT ttInvoiceLineToPost.costUOM, 
                OUTPUT ttInvoiceLineToPost.costTotal, OUTPUT ttInvoiceLineToPost.costSource,
                OUTPUT ttInvoiceLineToPost.costStdFreight, 
                OUTPUT ttInvoiceLineToPost.costStdWarehouse, 
                OUTPUT ttInvoiceLineToPost.costStdDeviation, 
                OUTPUT ttInvoiceLineToPost.costStdManufacture).
                
            IF glBlockZeroCost AND bf-inv-line.inv-qty NE 0 AND ttInvoiceLineToPost.costTotal EQ 0 THEN 
            DO:
                ASSIGN 
                    ttInvoiceLineToPost.isOKToPost     = NO
                    ttInvoiceLineToPost.problemMessage = "Zero Cost"
                    .
            END.
            
            IF bf-inv-line.cas-cnt NE 0 THEN 
                ttInvoiceLineToPost.quantityPerSubUnit = bf-inv-line.cas-cnt.
            
            IF bf-inv-line.t-price NE 0 THEN 
            DO:
                ASSIGN 
                    ttInvoiceLineToPost.amountWithoutDiscount = bf-inv-line.t-price
                    ttInvoiceLineToPost.amount                = bf-inv-line.t-price
                    .
                IF bf-inv-line.disc NE 0 THEN 
                DO:
                    RUN Conv_CalcTotalPrice(bf-inv-line.company, 
                        bf-inv-line.i-no, 
                        bf-inv-line.inv-qty,
                        bf-inv-line.price,
                        bf-inv-line.pr-uom,
                        0,
                        ttInvoiceLineToPost.quantityPerSubUnit,
                        OUTPUT ttInvoiceLineToPost.amount).
                    
                    ASSIGN
                        ttInvoiceLineToPost.amountDiscount  = ttInvoiceLineToPost.amount - ttInvoiceLineToPost.amountWithoutDiscount
                        ttInvoiceToPost.amountTotalDiscount = ttInvoiceToPost.amountTotalDiscount + ttInvoiceLineToPost.amountDiscount
                        .
                END.
                
                CREATE ttGLTransaction.
                ASSIGN 
                    ttGLTransaction.transactionType   = "work-line"
                    ttGLTransaction.journalNote       = "OEINV"
                    ttGLTransaction.transactionDate   = ipdtPostDate
                    ttGLTransaction.transactionPeriod = iPeriod
                    ttGLTransaction.transactionDesc   = fGetTransactionDescription(ttInvoiceToPost.company, ttInvoiceToPost.customerID, ttInvoiceToPost.invoiceID) + " LINE"
                    ttGLTransaction.company           = ttInvoiceLineToPost.company
                    ttGLTransaction.account           = ttInvoiceLineToPost.accountARSales
                    ttGLTransaction.invoiceID         = ttInvoiceLineToPost.invoiceID
                    ttGLTransaction.amount            = ttInvoiceLineToPost.amount
                    ttGLTransaction.currencyCode      = ttInvoiceToPost.currencyCode
                    ttGLTransaction.currencyExRate    = ttInvoiceToPost.currencyExRate
                    ttGLTransaction.quantityWeight    = ttInvoiceLineToPost.quantityInvoicedWeight
                    ttGLTransaction.itemID            = ttInvoiceLineToPost.itemID
                    .
            END.
        /*TO DO*/
        /*            IF v-post THEN                                                                                        */
        /*            DO:                                                                                                   */
        /*                /*** Calculate the amount of dollars to take out of the                                           */
        /*                     customer's on order balance ***/                                                             */
        /*                FIND CURRENT oe-ordl NO-ERROR.                                                                    */
        /*                IF AVAILABLE oe-ordl THEN                                                                         */
        /*                DO:                                                                                               */
        /*                                                                                                                  */
        /*                    RUN ar/calctax2.p (oe-ord.tax-gr,                                                             */
        /*                        NO,                                                                                       */
        /*                        oe-ordl.t-price,                                                                          */
        /*                        oe-ordl.company,                                                                          */
        /*                        oe-ordl.i-no,                                                                             */
        /*                        OUTPUT dTax).                                                                             */
        /*                                                                                                                  */
        /*                    dUninvOrdlAmt = oe-ordl.t-price +                                                             */
        /*                        (IF oe-ordl.tax THEN dTax ELSE 0).                                                        */
        /*                                                                                                                  */
        /*                    FOR EACH ar-invl NO-LOCK                                                                      */
        /*                        WHERE ar-invl.company EQ cocode                                                           */
        /*                        AND ar-invl.posted  EQ YES                                                                */
        /*                        AND ar-invl.cust-no EQ bf-inv-head.cust-no                                                */
        /*                        AND ar-invl.ord-no  EQ inv-line.ord-no                                                    */
        /*                        AND ar-invl.line    EQ inv-line.line                                                      */
        /*                        AND ar-invl.i-no    EQ inv-line.i-no                                                      */
        /*                        USE-INDEX inv-status :                                                                    */
        /*                                                                                                                  */
        /*                        RUN ar/calctax2.p (ar-inv.tax-code,                                                       */
        /*                            NO,                                                                                   */
        /*                            ar-invl.amt,                                                                          */
        /*                            ar-invl.company,                                                                      */
        /*                            ar-invl.i-no,                                                                         */
        /*                            OUTPUT dTax).                                                                         */
        /*                                                                                                                  */
        /*                        dUninvOrdlAmt = dUninvOrdlAmt - ar-invl.amt -                                             */
        /*                            (IF ar-invl.tax THEN dTax ELSE 0).                                                    */
        /*                    END.                                                                                          */
        /*                END.                                                                                              */
        /*                                                                                                                  */
        /*                ELSE                                                                                              */
        /*                    dUninvOrdlAmt = 0.                                                                            */
        /*                                                                                                                  */
        /*                dTax = 0.                                                                                         */
        /*                IF inv-line.tax THEN                                                                              */
        /*                    RUN ar/calctax2.p (bf-inv-head.tax-gr,                                                        */
        /*                        NO,                                                                                       */
        /*                        inv-line.t-price,                                                                         */
        /*                        inv-line.company,                                                                         */
        /*                        inv-line.i-no,                                                                            */
        /*                        OUTPUT dTax).                                                                             */
        /*                                                                                                                  */
        /*                IF inv-line.t-price + dTax LT dUninvOrdlAmt THEN                                                  */
        /*                    v-reduce-ord-bal = v-reduce-ord-bal + inv-line.t-price + dTax.                                */
        /*                ELSE                                                                                              */
        /*                    v-reduce-ord-bal = v-reduce-ord-bal + dUninvOrdlAmt.                                          */
        /*                                                                                                                  */
        /*            END. /* v-post */                                                                                     */
        /*                                                                                                                  */
        /*            IF AVAILABLE tt-report THEN                                                                           */
        /*                ASSIGN                                                                                            */
        /*                    dLineTot   = dLineTot   + inv-line.t-price                                                    */
        /*                    dLineTot-w = dLineTot-w + tt-report.weight                                                    */
        /*                    .                                                                                             */
        /*                                                                                                                  */
        /*            IF v-post THEN                                                                                        */
        /*            DO:                                                                                                   */
        /*                IF inv-line.ord-no NE 0 THEN                                                                      */
        /*                DO:                                                                                               */
        /*                    /* Sum all release qty */                                                                     */
        /*                    dSumRelQty = 0.                                                                               */
        /*                                                                                                                  */
        /*                    FOR EACH oe-boll NO-LOCK                                                                      */
        /*                        WHERE oe-boll.company EQ inv-line.company                                                 */
        /*                        AND oe-boll.b-no    EQ inv-line.b-no                                                      */
        /*                        AND oe-boll.ord-no  EQ inv-line.ord-no                                                    */
        /*                        AND oe-boll.i-no    EQ inv-line.i-no                                                      */
        /*                        AND oe-boll.line    EQ inv-line.line                                                      */
        /*                        AND oe-boll.po-no   EQ inv-line.po-no                                                     */
        /*                        AND CAN-FIND(FIRST oe-bolh                                                                */
        /*                        WHERE oe-bolh.b-no   EQ oe-boll.b-no                                                      */
        /*                        AND oe-bolh.posted EQ YES)                                                                */
        /*                                                                                                                  */
        /*                        BREAK BY oe-boll.r-no                                                                     */
        /*                        BY oe-boll.rel-no                                                                         */
        /*                        BY oe-boll.b-ord-no:                                                                      */
        /*                                                                                                                  */
        /*                        IF FIRST-OF(oe-boll.b-ord-no) THEN                                                        */
        /*                            FOR EACH oe-rell NO-LOCK                                                              */
        /*                                WHERE oe-rell.company  EQ oe-boll.company                                         */
        /*                                AND oe-rell.ord-no   EQ oe-boll.ord-no                                            */
        /*                                AND oe-rell.line     EQ oe-boll.line                                              */
        /*                                AND oe-rell.i-no     EQ oe-boll.i-no                                              */
        /*                                AND oe-rell.r-no     EQ oe-boll.r-no                                              */
        /*                                AND oe-rell.rel-no   EQ oe-boll.rel-no                                            */
        /*                                AND oe-rell.b-ord-no EQ oe-boll.b-ord-no                                          */
        /*                                AND CAN-FIND(FIRST oe-relh                                                        */
        /*                                WHERE oe-relh.r-no   EQ oe-boll.r-no                                              */
        /*                                AND oe-relh.posted EQ YES)                                                        */
        /*                                USE-INDEX ord-no :                                                                */
        /*                                dSumRelQty = dSumRelQty + oe-rell.qty.                                            */
        /*                            END.                                                                                  */
        /*                    END.                                                                                          */
        /*                                                                                                                  */
        /*                    IF AVAILABLE oe-ordl AND  dSumRelQty GE oe-ordl.qty AND                                       */
        /*                        (CAN-FIND(oe-boll WHERE oe-boll.company EQ inv-line.company                               */
        /*                        AND oe-boll.b-no   EQ inv-line.b-no                                                       */
        /*                        AND oe-boll.ord-no EQ inv-line.ord-no                                                     */
        /*                        AND oe-boll.i-no   EQ inv-line.i-no                                                       */
        /*                        AND oe-boll.line   EQ inv-line.line                                                       */
        /*                        AND oe-boll.po-no  EQ inv-line.po-no                                                      */
        /*                        AND oe-boll.p-c    EQ TRUE) OR                                                            */
        /*                        inv-line.p-c EQ TRUE) THEN                                                                */
        /*                        FOR EACH oe-ordl WHERE oe-ordl.company EQ cocode                                          */
        /*                            AND oe-ordl.ord-no  EQ inv-line.ord-no                                                */
        /*                            AND oe-ordl.i-no    EQ inv-line.i-no:                                                 */
        /*                            /* previous runs may have overstated the shipped qty.  re-acquire the "truth" */      */
        /*                            RUN oe/ordlsqty.p (ROWID(oe-ordl), OUTPUT oe-ordl.inv-qty,                            */
        /*                                OUTPUT oe-ordl.ship-qty).                                                         */
        /*                            ASSIGN                                                                                */
        /*                                oe-ordl.t-ship-qty = oe-ordl.ship-qty.                                            */
        /*                            ASSIGN                                                                                */
        /*                                iCloseQty = oe-ordl.qty - oe-ordl.t-ship-qty.                                     */
        /*                            IF iCloseQty LT 0                                                                     */
        /*                                /*10021404 - also do not reduce allocated for Invoice Only*/                      */
        /*                                OR (CAN-FIND(oe-boll WHERE oe-boll.company EQ inv-line.company                    */
        /*                                AND oe-boll.b-no   EQ inv-line.b-no                                               */
        /*                                AND oe-boll.ord-no EQ inv-line.ord-no                                             */
        /*                                AND oe-boll.i-no   EQ inv-line.i-no                                               */
        /*                                AND oe-boll.line   EQ inv-line.line                                               */
        /*                                AND oe-boll.po-no  EQ inv-line.po-no                                              */
        /*                                AND oe-boll.s-code EQ "I"))                                                       */
        /*                                THEN iCloseQty = 0.                                                               */
        /*                                                                                                                  */
        /*                            FIND FIRST xoe-ord NO-LOCK WHERE xoe-ord.company EQ cocode                            */
        /*                                AND xoe-ord.ord-no  EQ oe-ordl.ord-no                                             */
        /*                                NO-ERROR.                                                                         */
        /*                                                                                                                  */
        /*                            IF AVAILABLE itemfg THEN                                                              */
        /*                            DO:                                                                                   */
        /*                                IF xoe-ord.type NE "T" THEN                                                       */
        /*                                    itemfg.q-alloc = itemfg.q-alloc - iCloseQty.                                  */
        /*                                IF itemfg.q-alloc LT 0 THEN itemfg.q-alloc = 0.                                   */
        /*                                                                                                                  */
        /*                                itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.                    */
        /*                                IF itemfg.q-avail LT 0 THEN itemfg.q-avail = 0.                                   */
        /*                                                                                                                  */
        /*                                RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT xoe-ord.loc).                         */
        /*                                                                                                                  */
        /*                                FIND FIRST itemfg-loc                                                             */
        /*                                    WHERE itemfg-loc.company EQ itemfg.company                                    */
        /*                                    AND itemfg-loc.i-no    EQ itemfg.i-no                                         */
        /*                                    AND itemfg-loc.loc     EQ xoe-ord.loc                                         */
        /*                                    EXCLUSIVE-LOCK NO-ERROR.                                                      */
        /*                                                                                                                  */
        /*                                ASSIGN                                                                            */
        /*                                    itemfg.q-ptd     = itemfg.q-ptd     - iCloseQty                               */
        /*                                    itemfg.q-ord-ytd = itemfg.q-ord-ytd - iCloseQty.                              */
        /*                                                                                                                  */
        /*                                IF AVAILABLE itemfg-loc THEN                                                      */
        /*                                    itemfg-loc.q-alloc = itemfg-loc.q-alloc - iCloseQty.                          */
        /*                                                                                                                  */
        /*                                IF AVAIL(itemfg-loc) THEN                                                         */
        /*                                DO:                                                                               */
        /*                                    IF itemfg-loc.q-alloc LT 0 THEN itemfg-loc.q-alloc = 0.                       */
        /*                                                                                                                  */
        /*                                    itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.*/
        /*                                    IF itemfg-loc.q-avail LT 0 THEN itemfg-loc.q-avail = 0.                       */
        /*                                                                                                                  */
        /*                                    ASSIGN                                                                        */
        /*                                        itemfg-loc.q-ptd     = itemfg-loc.q-ptd     - iCloseQty                   */
        /*                                        itemfg-loc.q-ord-ytd = itemfg-loc.q-ord-ytd - iCloseQty.                  */
        /*                                END.                                                                              */
        /*                                FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.                                         */
        /*                            END.                                                                                  */
        /*                                                                                                                  */
        /*                            IF iCloseQty GT 0 THEN                                                                */
        /*                            DO:                                                                                   */
        /*                                ASSIGN                                                                            */
        /*                                    iUomRate = IF oe-ordl.pr-uom EQ "M"  THEN 1000                                */
        /*                                ELSE                                                                              */
        /*                                IF oe-ordl.pr-uom EQ "C"  THEN 100                                                */
        /*                                ELSE                                                                              */
        /*                                IF AVAILABLE itemfg           AND                                                 */
        /*                                   oe-ordl.pr-uom  EQ "CS"                                                        */
        /*                                                          THEN itemfg.case-count                                  */
        /*                                ELSE 1                                                                            */
        /*                                                                                                                  */
        /*                                    dDcrVal  = (iCloseQty / iUomRate) * oe-ordl.price                             */
        /*                                    dDcrVal  = dDcrVal - (dDcrVal * oe-ordl.disc / 100).                          */
        /*                                                                                                                  */
        /*                                IF oe-ordl.tax THEN                                                               */
        /*                                DO:                                                                               */
        /*                                    RUN ar/calctax2.p (oe-ord.tax-gr,                                             */
        /*                                        NO,                                                                       */
        /*                                        dDcrVal,                                                                  */
        /*                                        oe-ordl.company,                                                          */
        /*                                        oe-ordl.i-no,                                                             */
        /*                                        OUTPUT dTax).                                                             */
        /*                                                                                                                  */
        /*                                    dDcrVal = dDcrVal + dTax.                                                     */
        /*                                END.                                                                              */
        /*                                                                                                                  */
        /*                                IF AVAILABLE cust THEN cust.ord-bal = cust.ord-bal - dDcrVal.                     */
        /*                            END.                                                                                  */
        /*                        END. /* for each oe-ordl */                                                               */
        /*                END.                                                                                              */
        /*            END.                                                                                                  */
        END. /* each inv-line */

        /******************* MISCELLANEOUS ITEMS ***********************************/
        /* Be aware that job nos are not stroed in ar-invl records for misc charges*/

        FOR EACH bf-inv-misc
            WHERE bf-inv-misc.r-no EQ bf-inv-head.r-no
            USE-INDEX r-no:
             
            CREATE ttInvoiceMiscToPost.
            ASSIGN 
                ttInvoiceMiscToPost.riInvMisc      = ROWID(bf-inv-misc)
                ttInvoiceMiscToPost.company        = bf-inv-misc.company
                ttInvoiceMiscToPost.isOKToPost     = YES
                ttInvoiceMiscToPost.invoiceID      = bf-inv-head.inv-no
                ttInvoiceMiscToPost.orderID        = bf-inv-misc.ord-no
                ttInvoiceMiscToPost.itemID         = bf-inv-misc.inv-i-no
                ttInvoiceMiscToPost.chargeID       = bf-inv-misc.charge
                ttInvoiceMiscToPost.isTaxable      = bf-inv-misc.tax
                ttInvoiceMiscToPost.isBillable     = bf-inv-misc.bill EQ "Y"
                ttInvoiceMiscToPost.accountARSales = ttInvoiceToPost.accountARSales
                ttInvoiceMiscToPost.amount         = bf-inv-misc.amt
                .
            IF bf-inv-misc.actnum NE "" THEN 
            DO:
                ASSIGN 
                    lAccountError        = NO
                    cAccountErrorMessage = ""
                    .
                RUN pCheckAccount(bf-inv-misc.company, bf-inv-misc.actnum, "Misc GL Account", OUTPUT lAccountError, OUTPUT cAccountErrorMessage). 
                IF NOT lAccountError THEN 
                    ttInvoiceMiscToPost.accountARSales =  bf-inv-misc.actnum.
            END.
            
            IF ttInvoiceMiscToPost.isBillable AND ttInvoiceMiscToPost.amount NE 0 THEN 
            DO:
                CREATE ttGLTransaction.
                ASSIGN 
                    ttGLTransaction.transactionType   = "work-misc"
                    ttGLTransaction.journalNote       = "OEINV"
                    ttGLTransaction.transactionDate   = ipdtPostDate
                    ttGLTransaction.transactionPeriod = iPeriod
                    ttGLTransaction.transactionDesc   = fGetTransactionDescription(ttInvoiceToPost.company, ttInvoiceToPost.customerID, ttInvoiceToPost.invoiceID) + " MISC"
                    ttGLTransaction.company           = ttInvoiceToPost.company
                    ttGLTransaction.account           = ttInvoiceMiscToPost.accountARSales
                    ttGLTransaction.invoiceID         = ttInvoiceToPost.invoiceID
                    ttGLTransaction.amount            = ttInvoiceMiscToPost.amount
                    ttGLTransaction.currencyCode      = ttInvoiceToPost.currencyCode
                    ttGLTransaction.currencyExRate    = ttInvoiceToPost.currencyExRate
                    ttGLTransaction.itemID            = ttInvoiceMiscToPost.chargeID
                    .
                
                ttInvoiceToPost.amountTotalMisc = ttInvoiceToPost.amountTotalMisc + bf-inv-misc.amt. 
                
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
                
            IF v-post THEN 
            DO:
                FIND FIRST oe-ordm
                    WHERE oe-ordm.company EQ inv-misc.company
                    AND oe-ordm.ord-no  EQ inv-misc.ord-no
                    AND oe-ordm.line    EQ inv-misc.line
                    AND oe-ordm.charge  EQ inv-misc.charge
                    NO-ERROR.
                IF AVAILABLE oe-ordm THEN 
                DO:
                    IF oe-ordm.bill EQ "P" THEN oe-ordm.bill = IF inv-misc.bill EQ "Y" THEN "I" ELSE "Y".

                    

                    IF oe-ordm.miscType EQ 1 THEN
                        FOR EACH est-prep
                            WHERE est-prep.company EQ oe-ordm.company
                            AND est-prep.est-no  EQ oe-ordm.est-no
                            AND est-prep.eqty    EQ oe-ordm.estPrepEqty
                            AND est-prep.line    EQ oe-ordm.estPrepLine
                            AND est-prep.code    EQ oe-ordm.charge
                            AND est-prep.simon   EQ "S"
                            AND est-prep.amtz    EQ 100:
                            IF oeprep-log THEN DELETE est-prep.
                            ELSE est-prep.simon = "N".
                        END.


                    
                END.

                IF inv-misc.bill EQ "Y" THEN 
                DO:
                    RUN ar/calctax2.p (bf-inv-head.tax-gr, 
                        NO,
                        inv-misc.amt,
                        inv-misc.company,
                        inv-misc.inv-i-no,
                        OUTPUT dTax).
                
                    v-reduce-ord-bal = v-reduce-ord-bal + inv-misc.amt +
                        (IF inv-misc.tax THEN dTax ELSE 0).
                END.
              
                FIND FIRST ar-invl NO-LOCK
                    WHERE ar-invl.x-no EQ v-xno
                    AND ar-invl.line EQ v-xline + 1
                    NO-ERROR.
                IF NOT AVAILABLE ar-invl THEN CREATE ar-invl.
                {oe/invmpost.i}.

                /* gdm - 09290908 */ RUN get-lot-no.

                IF inv-misc.bill NE "Y" THEN ar-invl.bill = NO.
                IF cExportNk1 EQ "Sonoco" THEN RUN oe/sonofile.p (3,RECID(ar-invl)).
            END. /* v-post */
                    
        END. /* each inv-misc */

        /******************* DISCOUNT ITEMS ****************************************/
        ASSIGN
            v-post-disc   = v-post-disc   + dInvDisc
            v-post-disc-w = v-post-disc-w + dInvDisc-w.
        
        IF ttInvoiceToPost.amountTotalDiscount NE 0 THEN 
        DO:
            CREATE ttGLTransaction.
            ASSIGN 
                ttGLTransaction.transactionType   = "work-disc"
                ttGLTransaction.journalNote       = ""
                ttGLTransaction.transactionDate   = ipdtPostDate
                ttGLTransaction.transactionPeriod = iPeriod
                ttGLTransaction.transactionDesc   = fGetTransactionDescription(ttInvoiceToPost.company, ttInvoiceToPost.customerID, ttInvoiceToPost.invoiceID) + " MISC"
                ttGLTransaction.company           = ttInvoiceToPost.company
                ttGLTransaction.account           = ttInvoiceMiscToPost.accountARSales
                ttGLTransaction.invoiceID         = ttInvoiceToPost.invoiceID
                ttGLTransaction.amount            = ttInvoiceMiscToPost.amount
                ttGLTransaction.currencyCode      = ttInvoiceToPost.currencyCode
                ttGLTransaction.currencyExRate    = ttInvoiceToPost.currencyExRate
                ttGLTransaction.itemID            = ttInvoiceMiscToPost.chargeID
                .
        END.
        IF dInvDisc NE 0 THEN 
        DO:
            CREATE tt-report.
            ASSIGN
                tt-report.term-id = ""
                tt-report.key-01  = "work-disc"
                tt-report.key-02  = STRING(bf-inv-head.inv-no,"9999999")
                tt-report.key-05  = STRING(dInvDisc)
                tt-report.weight  = dInvDisc-w
                .
        END.

        /******************* TAX ITEMS *********************************************/
        IF bf-inv-head.t-inv-Tax NE 0 THEN 
        DO:
            IF bf-inv-head.tax-gr NE "" THEN 
            DO:
                IF bf-inv-head.multi-invoice THEN 
                DO:
                    FOR EACH b-bf-inv-head
                        WHERE b-bf-inv-head.company       EQ bf-inv-head.company
                        AND b-bf-inv-head.cust-no       EQ bf-inv-head.cust-no
                        AND b-bf-inv-head.inv-no        EQ bf-inv-head.inv-no
                        AND b-bf-inv-head.multi-invoice EQ NO:
                        RUN calc-tax-gr (ROWID(b-bf-inv-head), bf-inv-head.inv-no).
                    END.

                END.
                ELSE 
                    RUN calc-tax-gr (ROWID(bf-inv-head), bf-inv-head.inv-no).

            END.

            ELSE 
            DO:
                FIND FIRST account NO-LOCK
                    WHERE account.company EQ cocode
                    AND account.actnum  EQ v-ar-stax
                    NO-ERROR.
                CREATE tt-report.
                ASSIGN
                    tt-report.term-id = ""
                    tt-report.key-01  = "work-tax"
                    tt-report.key-02  = account.actnum
                    tt-report.key-03  = STRING(bf-inv-head.inv-no,"9999999")
                    tt-report.key-05  = STRING(bf-inv-head.t-inv-tax *
                                        (IF AVAILABLE currency  THEN
                                           currency.ex-rate ELSE 1))
                    tt-report.weight  = dLineTot-w
                    .
            END.
        END.

        IF NOT v-post THEN 
        DO:
            fDebugMsg("List Post, not v-post, create ttInvoicePostUpdateGL Recs") .
            ASSIGN
                ld-t[2] = dLineTot-w / 2000
                ld-t[3] = ld-t[3] + dLineTot-w
                ld-pton = bf-inv-head.t-inv-rev / ld-t[2]
                .

            IF ld-pton EQ ? THEN ld-pton = 0.

            IF NOT v-detail THEN 
            DO:
         
                /* Create temp-table without detail info */
                FIND FIRST ttInvoicePostUpdateGL
                    WHERE ttInvoicePostUpdateGL.invNo             = bf-inv-head.inv-no
                    AND ttInvoicePostUpdateGL.invDate           = bf-inv-head.inv-date
                    AND ttInvoicePostUpdateGL.custNo            = bf-inv-head.cust-no
                    AND ttInvoicePostUpdateGL.custName          = bf-inv-head.cust-name
                    AND ttInvoicePostUpdateGL.orderNumber       = v-ord-no
                    AND  ttInvoicePostUpdateGL.invoiceQty        = iInvoiceQty
                    AND ttInvoicePostUpdateGL.totInvoicefreight = bf-inv-head.t-inv-freight
                    AND ttInvoicePostUpdateGL.totInvoiceTax     = bf-inv-head.t-inv-tax
                    AND ttInvoicePostUpdateGL.miscTot           = dMiscTotal
                    AND ttInvoicePostUpdateGL.lineTot           = dLineTot
                    AND ttInvoicePostUpdateGL.iInvRev           = bf-inv-head.t-inv-rev
                    NO-ERROR .
                IF NOT AVAILABLE ttInvoicePostUpdateGL THEN 
                DO:
                    CREATE ttInvoicePostUpdateGL.
                    ASSIGN
                        ttInvoicePostUpdateGL.invNo             = bf-inv-head.inv-no
                        ttInvoicePostUpdateGL.invDate           = bf-inv-head.inv-date
                        ttInvoicePostUpdateGL.custNo            = bf-inv-head.cust-no
                        ttInvoicePostUpdateGL.custName          = bf-inv-head.cust-name
                        ttInvoicePostUpdateGL.orderNumber       = v-ord-no
                        ttInvoicePostUpdateGL.invoiceQty        = iInvoiceQty
                        ttInvoicePostUpdateGL.totInvoicefreight = bf-inv-head.t-inv-freight
                        ttInvoicePostUpdateGL.totInvoiceTax     = bf-inv-head.t-inv-tax
                        ttInvoicePostUpdateGL.miscTot           = dMiscTotal
                        ttInvoicePostUpdateGL.lineTot           = dLineTot
                        ttInvoicePostUpdateGL.iInvRev           = bf-inv-head.t-inv-rev
                        .
                    IF lPrintTon THEN 
                        ASSIGN 
                            ttInvoicePostUpdateGL.weightPerTon = ld-t[2]
                            ttInvoicePostUpdateGL.pricePerTon  = ld-pton
                            .
                END.
            END.
          
            ELSE 
            DO:
                /* If v-detail */
                FOR EACH w-inv-line BREAK BY w-inv-line.ord-no:
                    IF lPrintTon THEN 
                    DO WITH FRAME invlt:
                        ASSIGN
                            ld-t[1] = w-inv-line.weight / 2000
                            ld-pton = w-inv-line.t-price / ld-t[1].

                        IF ld-pton EQ ? THEN ld-pton = 0.
                        dProfit = (w-inv-line.t-price - w-inv-line.t-cost) / w-inv-line.t-price * 100.
       
                    END.
                    ELSE
                    DO WITH FRAME invl:
                        dProfit = (w-inv-line.t-price - w-inv-line.t-cost) / w-inv-line.t-price * 100.
                
                    END.
                    dprofit = dProfit / 100.
                    
                    FIND FIRST ttInvoicePostUpdateGL
                        WHERE ttInvoicePostUpdateGL.invNo             = bf-inv-head.inv-no
                        AND ttInvoicePostUpdateGL.invDate           = bf-inv-head.inv-date
                        AND ttInvoicePostUpdateGL.custNo            = bf-inv-head.cust-no
                        AND ttInvoicePostUpdateGL.custName          = bf-inv-head.cust-name
                        AND ttInvoicePostUpdateGL.orderNumber       = v-ord-no
                        AND  ttInvoicePostUpdateGL.invoiceQty        = iInvoiceQty
                        AND ttInvoicePostUpdateGL.totInvoicefreight = bf-inv-head.t-inv-freight
                        AND ttInvoicePostUpdateGL.totInvoiceTax     = bf-inv-head.t-inv-tax
                        AND ttInvoicePostUpdateGL.miscTot           = dMiscTotal
                        AND ttInvoicePostUpdateGL.lineTot           = dLineTot
                        AND ttInvoicePostUpdateGL.iInvRev           = bf-inv-head.t-inv-rev
                        AND ttInvoicePostUpdateGL.iNo               = w-inv-line.i-no
                        AND ttInvoicePostUpdateGL.iName             = w-inv-line.i-name
                        AND ttInvoicePostUpdateGL.qty               = w-inv-line.qty
                        AND ttInvoicePostUpdateGL.invQty            = w-inv-line.inv-qty
                        AND ttInvoicePostUpdateGL.shipQty           = w-inv-line.ship-qty
                        AND ttInvoicePostUpdateGL.price             = w-inv-line.price
                        AND ttInvoicePostUpdateGL.uom               = w-inv-line.uom
                        AND ttInvoicePostUpdateGL.TotPrice          = w-inv-line.t-price
                        AND ttInvoicePostUpdateGL.profit            = dProfit                    
                        NO-ERROR .
                    IF NOT AVAILABLE ttInvoicePostUpdateGL THEN 
                    DO:                    
                        CREATE ttInvoicePostUpdateGL.
                        ASSIGN
                            ttInvoicePostUpdateGL.invNo             = bf-inv-head.inv-no
                            ttInvoicePostUpdateGL.invDate           = bf-inv-head.inv-date
                            ttInvoicePostUpdateGL.custNo            = bf-inv-head.cust-no
                            ttInvoicePostUpdateGL.custName          = bf-inv-head.cust-name
                            ttInvoicePostUpdateGL.orderNumber       = v-ord-no
                            ttInvoicePostUpdateGL.invoiceQty        = iInvoiceQty
                            ttInvoicePostUpdateGL.totInvoicefreight = bf-inv-head.t-inv-freight
                            ttInvoicePostUpdateGL.totInvoiceTax     = bf-inv-head.t-inv-tax
                            ttInvoicePostUpdateGL.miscTot           = dMiscTotal
                            ttInvoicePostUpdateGL.lineTot           = dLineTot
                            ttInvoicePostUpdateGL.iInvRev           = bf-inv-head.t-inv-rev
                            /*                ttInvoicePostUpdateGL.weightPerTon      = bf-inv-head.weightPerTon*/
                            /*                ttInvoicePostUpdateGL.pricePerTon       = bf-inv-head.pricePerTon */
                            ttInvoicePostUpdateGL.iNo               = w-inv-line.i-no
                            ttInvoicePostUpdateGL.iName             = w-inv-line.i-name
                            ttInvoicePostUpdateGL.qty               = w-inv-line.qty
                            ttInvoicePostUpdateGL.invQty            = w-inv-line.inv-qty
                            ttInvoicePostUpdateGL.shipQty           = w-inv-line.ship-qty
                            ttInvoicePostUpdateGL.price             = w-inv-line.price
                            ttInvoicePostUpdateGL.uom               = w-inv-line.uom
                            ttInvoicePostUpdateGL.TotPrice          = w-inv-line.t-price
                            ttInvoicePostUpdateGL.profit            = dProfit
                            .


                        IF lPrintTon THEN 
                            ASSIGN 
                                ttInvoicePostUpdateGL.weightPerTon = ld-t[1] 
                                ttInvoicePostUpdateGL.pricePerTon  = ld-pton
                                .
                    END. /* if createing temp table record */
                    DELETE w-inv-line.

                END.

                FOR EACH w-ord-misc BREAK BY w-ord-misc.ord-no WITH FRAME invm:
       
                    DELETE w-ord-misc.
                END. /* each w-inv-line */
            END.

        END. /* not v-post */

        ASSIGN
            v-post-total   = v-post-total   + bf-inv-head.t-inv-rev
            v-post-total-w = v-post-total-w + dLineTot-w.

        IF AVAILABLE currency THEN 
        DO:
            CREATE tt-report.
            ASSIGN
                tt-report.term-id = ""
                tt-report.key-01  = "work-curr"
                tt-report.key-02  = currency.ar-ast-acct
                tt-report.key-05  = STRING(((bf-inv-head.t-inv-rev * currency.ex-rate) -
                                       bf-inv-head.t-inv-rev) * -1).
        END.

        v-tot-frt = 0.
        IF bf-inv-head.multi-invoice THEN
            FOR EACH b-bf-inv-head
                WHERE b-bf-inv-head.company       EQ bf-inv-head.company
                AND b-bf-inv-head.cust-no       EQ bf-inv-head.cust-no
                AND b-bf-inv-head.inv-no        EQ bf-inv-head.inv-no
                AND b-bf-inv-head.multi-invoice EQ NO:
  
                IF b-bf-inv-head.f-bill AND b-bf-inv-head.t-inv-freight NE 0 THEN 
                    v-tot-frt = v-tot-frt + b-bf-inv-head.t-inv-freight *
                        (IF AVAILABLE currency THEN currency.ex-rate ELSE 1).
            END.
        ELSE
            IF bf-inv-head.f-bill THEN
                v-tot-frt = bf-inv-head.t-inv-freight *
                    (IF AVAILABLE currency THEN currency.ex-rate ELSE 1).
        /** if Freight Is Billable then Post to GL **/
        IF v-tot-frt NE 0 THEN 
        DO:
            dTempAmount = v-tot-frt.
            ASSIGN
                v-post-freight   = v-post-freight   - dTempAmount
                v-post-freight-w = v-post-freight-w - dLineTot-w
                v-reduce-ord-bal = v-reduce-ord-bal + v-tot-frt
                .

            CREATE tt-report.
            ASSIGN
                tt-report.term-id = ""
                tt-report.key-01  = "work-freight"
                tt-report.key-02  = STRING(bf-inv-head.inv-no,"9999999")
                tt-report.key-05  = STRING(- dTempAmount)
                tt-report.weight  = - dLineTot-w
                .
        END.

        IF bf-inv-head.terms EQ "CASH" AND bf-inv-head.t-inv-rev NE 0 THEN 
        DO:
            ASSIGN
                v-post-cash    = v-post-cash    + bf-inv-head.t-inv-rev
                v-post-total   = v-post-total   - bf-inv-head.t-inv-rev
                v-post-cash-w  = v-post-cash-w  + dLineTot-w
                v-post-total-w = v-post-total-w - dLineTot-w.

            CREATE tt-report.
            ASSIGN
                tt-report.term-id = ""
                tt-report.key-01  = "work-cash"
                tt-report.key-02  = STRING(bf-inv-head.inv-no,"9999999")
                tt-report.key-05  = STRING(bf-inv-head.t-inv-rev)
                tt-report.weight  = dLineTot-w.
        END.

        IF v-post THEN 
        DO:
            RUN oe/invcust.p (RECID(bf-inv-head), v-ord-no, dtPostDate, tran-period).

            IF AVAILABLE ar-inv THEN 
            DO:
                ASSIGN
                    ar-inv.ord-no   = v-ord-no
                    ar-inv.ord-date = v-ord-date.

                RUN oe/invcost.p (RECID(ar-inv)).

                RELEASE ar-inv.
            END.

            /* update loadtag status - Bill of lading task#: 10190414 */
            FOR EACH inv-line NO-LOCK OF bf-inv-head ,
                EACH oe-boll NO-LOCK
                WHERE oe-boll.company EQ inv-line.company
                AND oe-boll.b-no    EQ inv-line.b-no
                AND oe-boll.ord-no  EQ inv-line.ord-no
                AND oe-boll.i-no    EQ inv-line.i-no
                AND oe-boll.line    EQ inv-line.line
                AND oe-boll.po-no   EQ inv-line.po-no
                AND CAN-FIND(FIRST oe-bolh
                WHERE oe-bolh.b-no   EQ inv-line.b-no
                AND oe-bolh.posted EQ YES)
                :

                IF oe-boll.tag GT "" THEN
                    FIND FIRST loadtag EXCLUSIVE-LOCK
                        WHERE loadtag.company EQ bf-inv-head.company
                        AND loadtag.item-type EQ NO
                        AND loadtag.i-no    EQ inv-line.i-no
                        AND loadtag.job-no  EQ oe-boll.job-no
                        AND loadtag.job-no2 EQ oe-boll.job-no2
                        AND loadtag.tag-no  EQ oe-boll.tag
                        USE-INDEX tag NO-ERROR.
                ELSE IF oe-boll.job-no GT "" THEN
                        FIND FIRST loadtag EXCLUSIVE-LOCK
                            WHERE loadtag.company EQ bf-inv-head.company
                            AND loadtag.item-type EQ NO
                            AND loadtag.i-no    EQ inv-line.i-no
                            AND loadtag.job-no  EQ oe-boll.job-no
                            AND loadtag.job-no2 EQ oe-boll.job-no2
                            AND loadtag.tag-no  EQ oe-boll.tag
                            USE-INDEX job-no NO-ERROR.
                    ELSE 
                        FIND FIRST loadtag EXCLUSIVE-LOCK
                            WHERE loadtag.company EQ bf-inv-head.company
                            AND loadtag.item-type EQ NO
                            AND loadtag.i-no    EQ inv-line.i-no
                            AND loadtag.job-no  EQ oe-boll.job-no
                            AND loadtag.job-no2 EQ oe-boll.job-no2
                            AND loadtag.tag-no  EQ oe-boll.tag
                            USE-INDEX i-no NO-ERROR.

                IF AVAILABLE loadtag THEN
                DO:
                    loadtag.sts = "Completed".
                    FIND CURRENT loadtag NO-LOCK NO-ERROR.
                END.
            END.

            FOR EACH inv-line WHERE inv-line.r-no EQ bf-inv-head.r-no USE-INDEX r-no
                BREAK BY inv-line.b-no:

                IF LAST-OF(inv-line.b-no) THEN
                    FOR EACH oe-boll NO-LOCK
                        WHERE oe-boll.company EQ inv-line.company
                        AND oe-boll.b-no    EQ inv-line.b-no
                        AND CAN-FIND(FIRST oe-bolh
                        WHERE oe-bolh.b-no   EQ oe-boll.b-no
                        AND oe-bolh.posted EQ YES)
                
                        BREAK BY oe-boll.r-no:

                        IF LAST-OF(oe-boll.r-no) THEN
                            FOR EACH oe-rell
                                WHERE oe-rell.company EQ oe-boll.company
                                AND oe-rell.r-no    EQ oe-boll.r-no
                                AND (oe-rell.posted EQ NO OR
                                NOT CAN-FIND(FIRST tmp-oe-boll
                                WHERE tmp-oe-boll.company  EQ oe-rell.company
                                AND tmp-oe-boll.r-no     EQ oe-rell.r-no
                                AND tmp-oe-boll.ord-no   EQ oe-rell.ord-no
                                AND tmp-oe-boll.i-no     EQ oe-rell.i-no
                                AND tmp-oe-boll.line     EQ oe-rell.line
                                AND tmp-oe-boll.rel-no   EQ oe-rell.rel-no
                                AND tmp-oe-boll.b-ord-no EQ oe-rell.b-ord-no
                                AND tmp-oe-boll.po-no    EQ oe-rell.po-no
                                USE-INDEX ord-no))
                                USE-INDEX r-no,

                                FIRST oe-relh NO-LOCK
                                WHERE oe-relh.r-no   EQ oe-rell.r-no
                                AND oe-relh.posted EQ YES
                                USE-INDEX r-no :

                                FOR EACH oe-rel
                                    WHERE oe-rel.company  EQ oe-rell.company
                                    AND oe-rel.link-no  EQ oe-rell.r-no
                                    AND oe-rel.ord-no   EQ oe-rell.ord-no
                                    AND oe-rel.i-no     EQ oe-rell.i-no
                                    AND oe-rel.line     EQ oe-rell.line
                                    AND oe-rel.rel-no   EQ oe-rell.rel-no
                                    AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
                                    AND oe-rel.po-no    EQ oe-rell.po-no
                                    USE-INDEX link:
                                    ASSIGN
                                        oe-rel.rel-no   = 0
                                        oe-rel.b-ord-no = 0
                                        oe-rel.link-no  = 0.
                                END.

                                DELETE oe-rell.
                            END.
                    END.

                DELETE inv-line.
            END.

            FOR EACH inv-misc WHERE inv-misc.r-no EQ bf-inv-head.r-no USE-INDEX r-no:
                DELETE inv-misc.
            END.

            IF inv-head.multi-invoice THEN
                FOR EACH b-inv-head
                    WHERE b-inv-head.company       EQ inv-head.company
                    AND b-inv-head.cust-no       EQ inv-head.cust-no
                    AND b-inv-head.inv-no        EQ inv-head.inv-no
                    AND b-inv-head.multi-invoice EQ NO:

                    DELETE b-inv-head.
                END.

            ELSE DELETE inv-head.

            IF cPrintFormat NE "Fibre" THEN RUN post-gl.
        
        END.

        FIND CURRENT inv-head NO-LOCK NO-ERROR.
        FIND CURRENT inv-line NO-LOCK NO-ERROR.
        FIND CURRENT itemfg   NO-LOCK NO-ERROR.
        FIND CURRENT oe-ordl  NO-LOCK NO-ERROR.
        FIND CURRENT ar-invl  NO-LOCK NO-ERROR.
        FIND CURRENT oe-ordm  NO-LOCK NO-ERROR.
        FIND CURRENT cust     NO-LOCK NO-ERROR.

    END PROCEDURE.

/* ************************  Function Implementations ***************** */

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
    
    DEFINE iLastXNo AS INTEGER NO-UNDO.
    
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
