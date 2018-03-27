
/*------------------------------------------------------------------------
    File        : custom.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Nov 16 13:47:51 EST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE v-xno         AS INTEGER.
DEFINE VARIABLE v-xline       AS INTEGER.
DEFINE VARIABLE v-ord-no      AS INTEGER.
DEFINE VARIABLE v-ord-date    AS DATE.
DEFINE VARIABLE cocode        AS CHARACTER INIT '001'.
DEFINE VARIABLE v-post        AS LOG       INIT TRUE.
DEFINE VARIABLE v-post-disc   AS DECIMAL.
DEFINE VARIABLE v-post-disc-w AS DECIMAL. 
DEFINE VARIABLE dTaxRate      AS DECIMAL   EXTENT 10.
DEFINE VARIABLE dTotalTax     AS DECIMAL.
DEFINE VARIABLE dTotalRate    AS DECIMAL .    
    DEFINE VARIABLE iInvoiceQty AS INTEGER.
    DEFINE VARIABLE iCaseCount  AS INTEGER.
    
DEFINE NEW SHARED TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD weight AS DECIMAL.

 

DEFINE NEW SHARED TEMP-TABLE work-job NO-UNDO
    FIELD actnum LIKE account.actnum
    FIELD amt    LIKE inv-line.t-price
    FIELD weight AS DECIMAL
    FIELD fg     AS LOG.

DEFINE NEW SHARED TEMP-TABLE tmp-work-job NO-UNDO
    FIELD actnum LIKE account.actnum
    FIELD amt    LIKE inv-line.t-price
    FIELD inv-no LIKE inv-line.inv-no
    FIELD i-no   LIKE inv-line.i-no
    FIELD weight AS DECIMAL
    FIELD fg     AS LOG.

DEFINE NEW SHARED TEMP-TABLE w-inv-line NO-UNDO
    FIELD ord-no   LIKE inv-line.ord-no
    FIELD i-no     LIKE inv-line.i-no
    FIELD i-name   LIKE inv-line.i-name
    FIELD qty      LIKE inv-line.qty
    FIELD inv-qty  LIKE inv-line.inv-qty
    FIELD ship-qty LIKE inv-line.ship-qty
    FIELD price    LIKE inv-line.price
    FIELD uom      LIKE inv-line.pr-uom
    FIELD t-price  LIKE inv-line.t-price
    FIELD weight   AS DECIMAL
    FIELD cost     LIKE inv-line.cost
    FIELD t-cost   LIKE inv-line.t-cost.

DEFINE NEW SHARED TEMP-TABLE w-ord-misc NO-UNDO
    FIELD ord-no LIKE inv-misc.ord-no
    FIELD charge LIKE inv-misc.charge
    FIELD dscr   LIKE inv-misc.dscr
    FIELD amt    LIKE inv-misc.amt
    FIELD tax    LIKE inv-misc.tax
    FIELD bill   LIKE inv-misc.bill.
 

DEFINE NEW SHARED WORKFILE w-ord
    FIELD ord-no LIKE oe-ord.ord-no
    FIELD rec-id AS   RECID.
   
   
DEFINE new shared VARIABLE v-ar-acct    LIKE ar-ctrl.receivables.
DEFINE new shared VARIABLE v-ar-freight LIKE ar-ctrl.freight.
DEFINE new shared VARIABLE v-ar-stax    LIKE ar-ctrl.stax.
DEFINE new shared VARIABLE v-ar-sales   LIKE ar-ctrl.sales.
DEFINE new shared VARIABLE v-ar-disc    LIKE ar-ctrl.discount.
DEFINE new shared VARIABLE v-return     AS logical.


FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK NO-ERROR.

v-return = NOT AVAIL ar-ctrl.
    
IF v-return THEN 
DO:
    MESSAGE "A/R Control File does not exist for this company..."
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

ELSE 
DO:
    /** GET A/R RECEIVABLES ACCOUNT # **/
    FIND FIRST account
        WHERE account.company EQ cocode
        AND account.actnum  EQ ar-ctrl.receivables
        NO-LOCK NO-ERROR.
    v-return = NOT AVAIL account.
END.

IF v-return THEN 
do:
    MESSAGE "A/R Control File has a null or invalid Receivables Account..."
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

ELSE 
DO:
    /** GET A/R FREIGHT ACCOUNT # **/
    FIND FIRST account
        WHERE account.company EQ cocode
        AND account.actnum  EQ ar-ctrl.freight
        NO-LOCK NO-ERROR.
    v-return = NOT AVAIL account.
END.
 
IF v-return THEN 
DO:
    MESSAGE "A/R Control File has a null or invalid Freight Account..."
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

ELSE 
DO:
    /** GET A/R SALES TAX ACCOUNT # **/
    FIND FIRST account
        WHERE account.company EQ cocode
        AND account.actnum  EQ ar-ctrl.stax
        NO-LOCK NO-ERROR.
    v-return = NOT AVAIL account.
END.

IF v-return THEN 
DO:
    MESSAGE "A/R Control File has a null or invalid Sales Tax Account..."
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

ELSE 
DO:
    /** GET A/R SALES ACCOUNT # **/
    FIND FIRST account
        WHERE account.company EQ cocode
        AND account.actnum  EQ ar-ctrl.sales
        NO-LOCK NO-ERROR.
    v-return = NOT AVAIL account.
END.
  
IF v-return THEN 
DO:
    MESSAGE "A/R Control File has a null or invalid Sales Account..."
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

ELSE 
DO:
    /** GET A/R DISCOUNT ACCOUNT # **/
    FIND FIRST account
        WHERE account.company EQ cocode
        AND account.actnum  EQ ar-ctrl.discount
        NO-LOCK NO-ERROR.
    v-return = NOT AVAIL account.
END.

IF v-return THEN 
DO:
    MESSAGE "A/R Control File has a null or invalid Discount Account..."
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

ELSE
    assign
        v-ar-acct    = ar-ctrl.receivables
        v-ar-freight = ar-ctrl.freight
        v-ar-stax    = ar-ctrl.stax
        v-ar-sales   = ar-ctrl.sales
        v-ar-disc    = ar-ctrl.discount.   
   
DEFINE TEMP-TABLE inv-head LIKE inv-head.
DEFINE TEMP-TABLE inv-line LIKE inv-line.   
DEFINE TEMP-TABLE inv-misc LIKE inv-misc.
   def var v-rno as int.
FOR EACH ar-inv WHERE ar-inv.company EQ '001' AND ar-inv.inv-no ge  611022 
and inv-no le 611040:
    v-rno = next-value(inv_r_no_seq).
    CREATE inv-head. 
    ASSIGN            
        inv-head.r-no          = v-rno                                
        v-xno                  = ar-inv.x-no            
        inv-head.company       = ar-inv.company         
        v-ord-no               = ar-inv.ord-no          
        v-ord-date             = ar-inv.ord-date        
        inv-head.inv-no        = ar-inv.inv-no          
        inv-head.sold-name     = ar-inv.sold-name       
        inv-head.bill-to       = ar-inv.bill-to         
        inv-head.sold-city     = ar-inv.sold-city       
        inv-head.sold-zip      = ar-inv.sold-zip        
        inv-head.contact       = ar-inv.contact         
        inv-head.terms         = ar-inv.terms           
        inv-head.frt-pay       = ar-inv.frt-pay         
        inv-head.fob-code      = ar-inv.fob-code        
        inv-head.carrier       = ar-inv.carrier         
        inv-head.cust-no       = ar-inv.cust-no         
        inv-head.inv-date      = ar-inv.inv-date        
        inv-head.sold-no       = ar-inv.sold-id         
        inv-head.sold-no /* RLL */                          = ar-inv.ship-id         
        inv-head.addr[1]       = ar-inv.addr[1]         
        inv-head.addr[2]       = ar-inv.addr[2]         
        inv-head.state         = ar-inv.state           
        inv-head.zip           = ar-inv.zip             
        inv-head.city          = ar-inv.city            
        inv-head.sold-state    = ar-inv.sold-state      
        inv-head.cust-name     = ar-inv.cust-name       
        inv-head.terms-d       = ar-inv.terms-d         
        inv-head.sold-addr[1]  = ar-inv.sold-addr[1]    
        inv-head.sold-addr[2]  = ar-inv.sold-addr[2]    
        inv-head.bill-i[1]     = ar-inv.bill-i[1]       
        inv-head.bill-i[2]     = ar-inv.bill-i[2]       
        inv-head.bill-i[3]     = ar-inv.bill-i[3]       
        inv-head.bill-i[4]     = ar-inv.bill-i[4]       
        inv-head.f-bill        = ar-inv.f-bill          
        inv-head.ship-i[1]     = ar-inv.ship-i[1]       
        inv-head.ship-i[2]     = ar-inv.ship-i[2]       
        inv-head.ship-i[3]     = ar-inv.ship-i[3]       
        inv-head.ship-i[4]     = ar-inv.ship-i[4]       
        inv-head.STAT          = ar-inv.STAT            
        inv-head.TAX-GR        = ar-inv.TAX-code        
        /*ar-inv.t-comm + inv-head.t-comm                     =  ar-inv.t-comm           */
        inv-head.t-inv-weight   /* total weight shipped */  = ar-inv.t-weight        
        inv-head.t-inv-freight  /* total freight Invoiced */= ar-inv.freight         
        inv-head.t-inv-tax      /* total tax Invoiced */    = ar-inv.tax-amt         
        inv-head.t-inv-cost     /* total cost invoiced */   = ar-inv.t-cost          
        inv-head.t-inv-rev     = ar-inv.due            
        inv-head.t-inv-rev     = ar-inv.gross
        .

        
    FOR EACH ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no
        AND ar-invl.misc = false /* ar-invl.dscr[1] NE "M" */:
    
        /* -------------------------------------------------- oe/invlpost.i 7/93 rd  */
        /* o/e invoicing lines history assigment of fields                           */
        /* -------------------------------------------------------------------------- */

        FIND FIRST oe-bolh 
            WHERE oe-bolh.b-no EQ ar-invl.b-no 
            NO-LOCK NO-ERROR.
        FIND FIRST oe-boll 
            WHERE oe-boll.company EQ oe-bolh.company
            AND oe-boll.b-no EQ oe-bolh.b-no
            AND oe-boll.i-no EQ ar-invl.i-no 
            NO-LOCK NO-ERROR.
        DEFINE VARIABLE v-cost AS DECIMAL EXTENT 4.
    
        CREATE inv-line.
        ASSIGN
            inv-line.r-no = inv-head.r-no
            v-xno                   = ar-invl.x-no            
            inv-head.inv-no         = ar-invl.inv-no         
            inv-head.bol-no         = ar-invl.bol-no         
            inv-line.b-no           = ar-invl.b-no           
            inv-line.company        = ar-invl.company        
            inv-line.ord-no         = ar-invl.ord-no         
            inv-line.cust-no        = ar-invl.cust-no        
 
            v-xline                 = ar-invl.line           
            inv-line.est-no         = ar-invl.est-no         
            inv-line.est-type       = ar-invl.est-type       
            inv-line.form-no        = ar-invl.form-no        
            inv-line.blank-no       = ar-invl.blank-no       
            inv-line.job-no         = ar-invl.job-no         
            inv-line.job-no2        = ar-invl.job-no2        
            inv-line.part-no        = ar-invl.part-no        
            inv-line.i-no           = ar-invl.i-no           
            inv-line.i-name         = ar-invl.i-name         
            inv-line.i-dscr         = ar-invl.i-dscr         
            inv-line.po-no          = ar-invl.po-no          
            inv-line.req-code       = ar-invl.req-code       
            inv-line.req-date       = ar-invl.req-date       
            inv-line.prom-code      = ar-invl.prom-code      
            inv-line.prom-date      = ar-invl.prom-date      
            inv-line.part-dscr1     = ar-invl.part-dscr1     
            inv-line.part-dscr2     = ar-invl.part-dscr2     
            inv-line.po-no-po       = ar-invl.po-no-po       
            inv-line.cas-cnt        = ar-invl.cas-cnt        
            inv-line.pr-uom         = ar-invl.pr-uom         
            inv-line.price          = ar-invl.unit-pr        
            inv-line.tax            = ar-invl.tax            
            inv-line.disc           = ar-invl.disc           
            inv-line.t-price   /* total price of invoiced item */                = ar-invl.amt            
            inv-line.t-weight  /* total weight of invoiced item */               = ar-invl.t-weight       
            inv-line.t-freight /* total freight of invoiced item */              = ar-invl.t-freight      
            inv-line.ship-qty       = ar-invl.ship-qty       
            inv-line.inv-qty        = ar-invl.inv-qty        
            inv-line.qty            = ar-invl.qty            
            /* itemfg.spare-dec-1 /* Full Cost */                                   = ar-invl.spare-dec-1     */
            inv-line.sman[1]        = ar-invl.sman[1]        
            inv-line.sman[2]        = ar-invl.sman[2]        
            inv-line.sman[3]        = ar-invl.sman[3]        
            inv-line.s-pct[1]       = ar-invl.s-pct[1]       
            inv-line.s-pct[2]       = ar-invl.s-pct[2]       
            inv-line.s-pct[3]       = ar-invl.s-pct[3]       
            inv-line.s-comm[1]      = ar-invl.s-comm[1]      
            inv-line.s-comm[2]      = ar-invl.s-comm[2]      
            inv-line.s-comm[3]      = ar-invl.s-comm[3]      
            inv-line.sname[1]       = ar-invl.sname[1]       
            inv-line.sname[2]       = ar-invl.sname[2]       
            inv-line.sname[3]       = ar-invl.sname[3]       
            inv-line.s-commbasis[1] = ar-invl.s-commbasis[1] 
            inv-line.s-commbasis[2] = ar-invl.s-commbasis[2] 
            inv-line.s-commbasis[3] = ar-invl.s-commbasis[3] 
            /* no                                                                   = ar-invl.misc            */
            /* yes                                                                  = ar-invl.posted          */
            inv-line.pr-uom         = ar-invl.pr-qty-uom     
            inv-line.cost           = ar-invl.cost           
            /* ar-invl.cost * (ar-invl.inv-qty / 1000)                              = ar-invl.t-cost           */         
            /*  / "M"                                                                  = ar-invl.dscr[1]       */
            inv-line.cost           = ar-invl.std-tot-cost   
            v-cost[1]               = ar-invl.std-lab-cost   
            v-cost[2]               = ar-invl.std-fix-cost   
            v-cost[3]               = ar-invl.std-var-cost   
            v-cost[4]               = ar-invl.std-mat-cost   
            /* IF AVAIL oe-boll THEN oe-boll.loc ELSE ""                            = ar-invl.loc             */
            inv-line.lot-no         = ar-invl.lot-no         
            .  
/*   disp inv-line.r-no.  */
    END.  /* create inv-line */
    def var mcnt as int.
    for each ar-invm where ar-invm.x-no eq ar-inv.x-no no-lock.
    mcnt = mcnt + 1.
    end.
/*  message mcnt "mcnt" view-as alert-box. */
    FOR EACH ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no no-lock:
      mcnt = mcnt + 1.
    end.
/*  message mcnt "mcnt2" view-as alert-box. */

    FOR EACH ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no
        AND ar-invl.misc = true:
        CREATE inv-misc.
        ASSIGN
            inv-misc.r-no           = inv-head.r-no
            /* v-XNO                       =          ar-invl.x-no */          
            inv-misc.company        = ar-invl.company     
            /*inv-head.inv-no         = ar-invl.INV-NO */        
            inv-misc.ord-no         = ar-invl.ord-no        
            /* inv-head.cust-no        = ar-invl.cust-no */       
            /* v-XLINE + 1                 =          v-XLINE */               
            /* v-XLINE                     =          ar-invl.line */          
            inv-misc.est-no         = ar-invl.est-no        
            inv-misc.tax            = ar-invl.tax           
            inv-misc.actnum         = ar-invl.actnum        
            inv-misc.amt            = ar-invl.prep-amt      
            /* 1                           =          ar-invl.qty            */
            inv-misc.amt            = ar-invl.unit-pr       
            inv-misc.amt            = ar-invl.amt           
            inv-misc.cost           = ar-invl.t-cost        
            /*ar-invl.t-cost / 1000       =          ar-invl.cost */          
            /* "M"                         =          ar-invl.dscr[1]        */
            inv-misc.charge         = ar-invl.prep-charge   
            inv-misc.cost           = ar-invl.prep-cost     
            inv-misc.dscr           = ar-invl.prep-dscr     
            inv-misc.charge         = ar-invl.i-name        
            inv-misc.dscr           = ar-invl.i-dscr        
            inv-misc.po-no          = ar-invl.po-no         
            inv-misc.po-no-po       = ar-invl.po-no-po      
            inv-misc.s-man[1]       = ar-invl.sman[1]       
            inv-misc.s-man[2]       = ar-invl.sman[2]       
            inv-misc.s-man[3]       = ar-invl.sman[3]       
            inv-misc.s-pct[1]       = ar-invl.s-pct[1]      
            inv-misc.s-pct[2]       = ar-invl.s-pct[2]      
            inv-misc.s-pct[3]       = ar-invl.s-pct[3]      
            inv-misc.s-comm[1]      = ar-invl.s-comm[1]     
            inv-misc.s-comm[2]      = ar-invl.s-comm[2]     
            inv-misc.s-comm[3]      = ar-invl.s-comm[3]     
            inv-misc.s-commbasis[1] = ar-invl.s-commbasis[1]
            inv-misc.s-commbasis[2] = ar-invl.s-commbasis[2]
            inv-misc.s-commbasis[3] = ar-invl.s-commbasis[3]
            inv-misc.inv-i-no       = ar-invl.inv-i-no      
            inv-misc.inv-line       = ar-invl.inv-line      
            /* YES                         =          ar-invl.misc */          
            inv-misc.bill           = IF ar-invl.billable THEN "Y" ELSE "N"
            inv-misc.spare-char-1   = ar-invl.spare-char-1  
            /* YES.                        =          ar-invl.posted */
            .   
           disp inv-misc.r-no. 
    END. /* create inv-misc */ 
END. /* create inv-head */



FOR EACH inv-head, EACH inv-line
    WHERE inv-line.r-no EQ inv-head.r-no
    USE-INDEX r-no BREAK BY inv-line.ord-no:
     disp inv-line.i-no.
     down.
    FIND FIRST itemfg no-lock
        WHERE (itemfg.company  = cocode )
        AND itemfg.i-no EQ inv-line.i-no
        NO-ERROR.

    FIND FIRST uom NO-LOCK
        WHERE uom.uom  EQ inv-line.pr-uom
        AND uom.mult NE 0
        NO-ERROR.

    FIND FIRST oe-ordl
        WHERE oe-ordl.company EQ cocode
        AND oe-ordl.ord-no  EQ inv-line.ord-no
        AND oe-ordl.line    EQ inv-line.line
        AND oe-ordl.i-no    EQ inv-line.i-no
        USE-INDEX ord-no NO-ERROR.

    RELEASE oe-ord.
    IF inv-line.ord-no NE 0 AND AVAILABLE oe-ordl THEN
        FIND FIRST oe-ord NO-LOCK
            WHERE oe-ord.company EQ oe-ordl.company
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.

    IF AVAILABLE oe-ord AND v-post THEN 
    DO:
        
        CREATE w-ord.
        ASSIGN
            w-ord.ord-no = oe-ord.ord-no
            w-ord.rec-id = RECID(oe-ord)
            .
    END.

    ASSIGN
        iInvoiceQty = iInvoiceQty + inv-line.inv-qty
        iCaseCount  = IF inv-line.cas-cnt NE 0 THEN
                         inv-line.cas-cnt
                       ELSE
                       IF AVAILABLE oe-ordl AND oe-ordl.cas-cnt NE 0 THEN
                         oe-ordl.cas-cnt
                       ELSE
                       IF AVAILABLE itemfg AND itemfg.case-count NE 0 THEN
                         itemfg.case-count
                       ELSE 1.

    IF FIRST(inv-line.ord-no) THEN
        ASSIGN
            v-ord-no   = inv-line.ord-no
            v-ord-date = inv-line.ord-date.


    /*if v-detail and not v-post then do:*/
    CREATE w-inv-line.
    ASSIGN
        w-inv-line.ord-no   = inv-line.ord-no
        w-inv-line.i-no     = inv-line.i-no
        w-inv-line.i-name   = inv-line.i-name
        w-inv-line.qty      = inv-line.qty
        w-inv-line.inv-qty  = inv-line.inv-qty
        w-inv-line.ship-qty = inv-line.ship-qty
        w-inv-line.price    = inv-line.price
        w-inv-line.uom      = inv-line.pr-uom
        w-inv-line.t-price  = inv-line.t-price
        w-inv-line.cost     = inv-line.cost
        .
    /*end.*/

    IF AVAILABLE itemfg THEN
        FIND FIRST fgcat NO-LOCK 
            WHERE fgcat.company EQ cocode
            AND fgcat.procat  EQ itemfg.procat
            NO-ERROR.
    ELSE
        IF v-post THEN UNDO , NEXT .
    /* not needed since cost coming from ar-invl *
    RUN oe/invlcost.p (ROWID(inv-line),
        OUTPUT v-cost[1], OUTPUT v-cost[2],
        OUTPUT v-cost[3], OUTPUT v-cost[4],
        OUTPUT inv-line.cost, OUTPUT inv-line.t-cost).
    */
    
    w-inv-line.t-cost = inv-line.t-cost.
    
    /* don't know what this is 
    IF inv-line.inv-qty NE 0 AND
        inv-line.t-cost EQ 0  AND 
        NOT lPostZeroCGS   THEN UNDO ordblock, NEXT ordblock.
    */
            
    /*
    RUN oe/invposty.p (inv-head.inv-no, inv-line.i-no, inv-line.inv-qty,
        "M", v-cost[1], v-cost[2], v-cost[3], v-cost[4]).
    */
    DEFINE VARIABLE v-invline   AS RECID.
    DEFINE VARIABLE v-invhead   AS RECID.
    DEFINE VARIABLE dtPostDate  AS DATE.
    DEFINE VARIABLE tran-period AS INTEGER.
    DEFINE VARIABLE dTempAmount AS DECIMAL.
            
    IF AVAILABLE itemfg AND v-post THEN 
    DO:
        ASSIGN
            v-invline = RECID(inv-line)
            v-invhead = RECID(inv-head).
            
    /* inventory update */
    /* RUN oe/invpost3.p (dtPostDate, tran-period). */
    END. /* avail itemfg & v-post */

    RUN calc-tons (w-inv-line.i-no, w-inv-line.inv-qty, OUTPUT w-inv-line.weight).
/*   message "inv-line.t-price" inv-line.t-price "disc" inv-line.disc view-as alert-box. */
    IF inv-line.t-price NE 0 THEN 
    DO:
        dTempAmount = 0.
        IF inv-line.disc NE 0 THEN 
        DO:
            dTempAmount = ROUND((IF inv-line.pr-uom BEGINS "L" AND
                inv-line.pr-uom NE "LB"    THEN
                IF inv-line.inv-qty LT 0 THEN -1 ELSE 1
                ELSE
                IF inv-line.pr-uom EQ "CS" THEN
                inv-line.inv-qty / iCaseCount
                ELSE
                IF AVAILABLE uom THEN
                inv-line.inv-qty / uom.mult
                ELSE
                inv-line.inv-qty / 1000) *
                inv-line.price,2) -
                inv-line.t-price.

            IF AVAILABLE currency THEN
                dTempAmount = dTempAmount * currency.ex-rate.
                
            DEFINE VARIABLE dInvDisc   AS DECIMAL.
            DEFINE VARIABLE dInvDisc-w AS DECIMAL .
            ASSIGN
                dInvDisc   = dInvDisc + dTempAmount
                dInvDisc-w = dInvDisc-w + w-inv-line.weight.
        END.

        
        CREATE tt-report.
        ASSIGN
            tt-report.term-id = ""
            tt-report.key-01  = "work-line"
            tt-report.key-02  = IF AVAILABLE fgcat AND fgcat.glacc NE ""
                                 THEN fgcat.glacc ELSE v-ar-sales
            tt-report.key-03  = STRING(inv-head.inv-no,"999999")
            tt-report.key-04  = inv-line.i-no
            tt-report.weight  = w-inv-line.weight
            dTempAmount       = dTempAmount +
                                 (inv-line.t-price *
                                  (IF AVAILABLE currency THEN currency.ex-rate ELSE 1))
            tt-report.key-05  = STRING(dTempAmount).
                    
        FOR EACH inv-misc:
                     
            CREATE tt-report.
            ASSIGN
                tt-report.term-id = ""
                tt-report.key-01  = "work-misc"
                tt-report.key-02  = IF inv-misc.actnum NE ""
                                                     THEN inv-misc.actnum ELSE v-ar-sales
                tt-report.key-03  = STRING(inv-head.inv-no,"999999")
                tt-report.key-04  = inv-misc.charge
                tt-report.key-05  = STRING(inv-misc.amt *
                                                            (IF AVAILABLE currency  THEN
                                                               currency.ex-rate ELSE 1)).
        END.
                    
                    
                    
      
                   IF inv-line.t-price NE 0 THEN 
                    DO:
                        dTempAmount = 0.
                        IF inv-line.disc NE 0 THEN 
                        DO:
                            dTempAmount = ROUND((IF inv-line.pr-uom BEGINS "L" AND
                                inv-line.pr-uom NE "LB"    THEN
                                IF inv-line.inv-qty LT 0 THEN -1 ELSE 1
                                ELSE
                                IF inv-line.pr-uom EQ "CS" THEN
                                inv-line.inv-qty / iCaseCount
                                ELSE
                                IF AVAILABLE uom THEN
                                inv-line.inv-qty / uom.mult
                                ELSE
                                inv-line.inv-qty / 1000) *
                                inv-line.price,2) -
                                inv-line.t-price.
        
                            IF AVAILABLE currency THEN
                                dTempAmount = dTempAmount * currency.ex-rate.
        
                            ASSIGN
                                dInvDisc   = dInvDisc + dTempAmount
                                dInvDisc-w = dInvDisc-w + w-inv-line.weight.
                        END.
                  end.

        ASSIGN
            v-post-disc   = v-post-disc   + dInvDisc
            v-post-disc-w = v-post-disc-w + dInvDisc-w.
/*   message "dInvDisc" dInvDisc view-as alert-box. */
        IF dInvDisc NE 0 THEN 
        DO:
            CREATE tt-report.
            ASSIGN
                tt-report.term-id = ""
                tt-report.key-01  = "work-disc"
                tt-report.key-02  = STRING(inv-head.inv-no,"999999")
                tt-report.key-05  = STRING(dInvDisc)
                tt-report.weight  = dInvDisc-w
                .
        END.                    
        DEFINE BUFFER b-inv-head FOR inv-head.
/*           message "t-inv-tax" inv-head.t-inv-tax view-as alert-box. */
        IF inv-head.t-inv-Tax NE 0 THEN 
        DO:
            IF inv-head.tax-gr NE "" THEN 
            DO:
                IF inv-head.multi-invoice THEN 
                DO:
                    FOR EACH b-inv-head
                        WHERE b-inv-head.company       EQ inv-head.company
                        AND b-inv-head.cust-no       EQ inv-head.cust-no
                        AND b-inv-head.inv-no        EQ inv-head.inv-no
                        AND b-inv-head.multi-invoice EQ NO:
                        RUN calc-tax-gr (ROWID(b-inv-head), inv-head.inv-no).
                    END.

                END.
                ELSE 
                    RUN calc-tax-gr (ROWID(inv-head), inv-head.inv-no).

            END.

            ELSE 
            DO:

                DEFINE VARIABLE dLineTot-w AS DECIMAL.
                FIND FIRST account NO-LOCK
                    WHERE account.company EQ cocode
                    AND account.actnum  EQ v-ar-stax
                    NO-ERROR.
                CREATE tt-report.
                ASSIGN
                    tt-report.term-id = ""
                    tt-report.key-01  = "work-tax"
                    tt-report.key-02  = account.actnum
                    tt-report.key-03  = STRING(inv-head.inv-no,"999999")
                    tt-report.key-05  = STRING(inv-head.t-inv-tax *
                                        (IF AVAILABLE currency  THEN
                                           currency.ex-rate ELSE 1))
                    tt-report.weight  = dLineTot-w
                    .
            END.
        END. /* work tax */
        
        def var v-post-total as dec.
        def var v-post-total-w as dec.
        def var v-tot-frt as dec.
        
            ASSIGN
            v-post-total   = v-post-total   + inv-head.t-inv-rev
            v-post-total-w = v-post-total-w + dLineTot-w.

        IF AVAILABLE currency THEN 
        DO:
            CREATE tt-report.
            ASSIGN
                tt-report.term-id = ""
                tt-report.key-01  = "work-curr"
                tt-report.key-02  = currency.ar-ast-acct
                tt-report.key-05  = STRING(((inv-head.t-inv-rev * currency.ex-rate) -
                                       inv-head.t-inv-rev) * -1).
        END.

        v-tot-frt = 0.
        IF inv-head.multi-invoice THEN
            FOR EACH b-inv-head
                WHERE b-inv-head.company       EQ inv-head.company
                AND b-inv-head.cust-no       EQ inv-head.cust-no
                AND b-inv-head.inv-no        EQ inv-head.inv-no
                AND b-inv-head.multi-invoice EQ NO:
  
                IF b-inv-head.f-bill AND b-inv-head.t-inv-freight NE 0 THEN 
                    v-tot-frt = v-tot-frt + b-inv-head.t-inv-freight *
                        (IF AVAILABLE currency THEN currency.ex-rate ELSE 1).
            END.
        ELSE
            IF inv-head.f-bill THEN
                v-tot-frt = inv-head.t-inv-freight *
                    (IF AVAILABLE currency THEN currency.ex-rate ELSE 1).
        /** if Freight Is Billable then Post to GL **/
        IF v-tot-frt NE 0 THEN 
        DO:
        def var v-post-freight as dec.
        def var v-post-freight-w as dec.
        def var v-reduce-ord-bal as dec.
        
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
                tt-report.key-02  = STRING(inv-head.inv-no,"999999")
                tt-report.key-05  = STRING(- dTempAmount)
                tt-report.weight  = - dLineTot-w
                .
        END.

        IF inv-head.terms EQ "CASH" AND inv-head.t-inv-rev NE 0 THEN 
        DO:
        def var v-post-cash as dec.
        def var v-post-cash-w as dec.
            ASSIGN
                v-post-cash    = v-post-cash    + inv-head.t-inv-rev
                v-post-total   = v-post-total   - inv-head.t-inv-rev
                v-post-cash-w  = v-post-cash-w  + dLineTot-w
                v-post-total-w = v-post-total-w - dLineTot-w.

            CREATE tt-report.
            ASSIGN
                tt-report.term-id = ""
                tt-report.key-01  = "work-cash"
                tt-report.key-02  = STRING(inv-head.inv-no,"999999")
                tt-report.key-05  = STRING(inv-head.t-inv-rev)
                tt-report.weight  = dLineTot-w.
        END.
        
        
        
    END.
END.
        pause.
output to c:\temp\tt-report.d.
for each tt-report. 
  export tt-report.
end.
output close.        
os-command("notepad c:\temp\tt-report.d").        
PROCEDURE calc-tax-gr :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipr-head-row AS ROWID.
    DEFINE INPUT PARAMETER ipi-inv-no LIKE inv-head.inv-no NO-UNDO.
    DEFINE BUFFER bf-currency FOR currency.
    DEFINE BUFFER bf-inv-head FOR inv-head.

    DEFINE VARIABLE k      AS INTEGER NO-UNDO.
    DEFINE VARIABLE dAccum AS DECIMAL NO-UNDO.

    FIND bf-inv-head NO-LOCK WHERE ROWID(bf-inv-head) = ipr-head-row  NO-ERROR.

    IF NOT AVAILABLE bf-inv-head THEN
        RETURN.

    FIND FIRST bf-currency NO-LOCK
        WHERE bf-currency.company     EQ bf-inv-head.company
        AND bf-currency.c-code      EQ bf-inv-head.curr-code[1]
        AND bf-currency.ar-ast-acct NE ""
        AND bf-currency.ex-rate     GT 0
        NO-ERROR.

    ASSIGN 
        dTotalTax  = 0
        dTotalRate = 0.
    FIND FIRST stax NO-LOCK
    
     where (stax.tax-group              begins cocode
       and  substr(stax.tax-group,1,10) eq cocode)
         AND 
substr(stax.tax-group,11,length(trim(stax.tax-group)) - 10)
  EQ bf-inv-head.tax-gr
        NO-ERROR.
    IF NOT AVAILABLE stax THEN
        FIND FIRST stax NO-LOCK
            WHERE stax.company = bf-inv-head.company AND
            stax.tax-group EQ bf-inv-head.tax-gr
            NO-ERROR.
    DEFINE VARIABLE i AS INTEGER.

    dAccum = 1.
    IF AVAILABLE stax THEN 
    DO:
        DO i = 1 TO EXTENT(stax.tax-rate1):
            IF stax.tax-rate1[i] = 0 THEN NEXT.
            dTaxRate[i] = stax.tax-rate1[i].
            IF stax.accum-tax THEN 
            DO: 
                /*        ##PN - must find effective rate since this is accumulated*/
                dAccum = dAccum  * (1 + dTaxRate[i] / 100).
                dTaxRate[i] = 100 * (dAccum - (dTotalRate / 100) - 1).
            END.
            IF stax.company EQ "yes" AND i GT 1 THEN
            DO k = 1 TO i - 1:
                dTaxRate[i] = dTaxRate[i] +
                    (dTaxRate[i] * (stax.tax-rate1[k] / 100)).
            END.
            dTotalRate = dTotalRate + dTaxRate[i].
        END.
      
        DO i = 1 TO EXTENT(stax.tax-rate1):
            IF stax.tax-rate1[i] = 0 THEN NEXT.
            ASSIGN 
                dTaxRate[i] = ROUND(dTaxRate[i] / dTotalRate *
                                     bf-inv-head.t-inv-tax,2)
                dTotalTax   = dTotalTax + dTaxRate[i]
                .
        END.
      
        IF bf-inv-head.t-inv-tax NE dTotalTax THEN
            dTaxRate[1] = dTaxRate[1] +
                (bf-inv-head.t-inv-tax - dTotalTax).
      
        DO i = 1 TO EXTENT(stax.tax-rate1):
            IF stax.tax-rate1[i] = 0 THEN NEXT.
            FIND FIRST account NO-LOCK
                WHERE account.company EQ cocode
                AND account.actnum  EQ stax.tax-acc1[i]
                NO-ERROR.
            
            IF AVAILABLE account AND dTaxRate[i] NE 0 THEN 
            DO:
                CREATE tt-report.
                ASSIGN
                    tt-report.term-id = ""
                    tt-report.key-01  = "work-tax"
                    tt-report.key-02  = account.actnum
                    tt-report.key-03  = STRING(ipi-inv-no,"999999")
                    tt-report.key-04  = bf-inv-head.tax-gr
                    tt-report.key-05  = STRING(dTaxRate[i] *
                                      (IF AVAILABLE bf-currency  THEN
                                         bf-currency.ex-rate ELSE 1))
                    tt-report.weight  = dLineTot-w *
                               (dTaxRate[i] / bf-inv-head.t-inv-tax)
                    .
            END. /* avail account */

        END. /* 1 to 3 */

    END. /* avail stax */
END PROCEDURE.        

PROCEDURE calc-tons :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ip-i-no LIKE itemfg.i-no NO-UNDO.
    DEFINE INPUT  PARAMETER ip-qty AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER op-weight AS DECIMAL NO-UNDO.

    DEFINE BUFFER b-itemfg FOR itemfg.


    FIND FIRST b-itemfg NO-LOCK
        WHERE b-itemfg.company EQ cocode
        AND b-itemfg.i-no    EQ ip-i-no
        NO-ERROR.
    IF AVAILABLE b-itemfg AND b-itemfg.weight-100 NE 0 THEN
        op-weight = b-itemfg.weight-100 * ip-qty / 100.

END PROCEDURE.
