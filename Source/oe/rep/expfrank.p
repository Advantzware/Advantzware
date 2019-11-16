/* ---------------------------------------------- oe/rep/expfrank.p   */
/* Export INVOICE   for Frankston          */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
/*{oe/rep/invoice.i}*/
/*DEF SHARED VAR v-term-id AS cha NO-UNDO. changed to use tt-report oe/invwork2.i*/
{oe/invwork2.i} 

def var v-shipto-name as char format "x(30)" NO-UNDO.
def var v-shipto-addr as char format "x(30)" extent 2 NO-UNDO.
def var v-shipto-city as char format "x(15)" NO-UNDO.
def var v-shipto-state as char format "x(2)" NO-UNDO.
def var v-shipto-zip as char format "x(10)" NO-UNDO.
DEF VAR v-country AS cha NO-UNDO.

DEF VAR v-duns AS cha NO-UNDO.
DEF VAR v-email AS cha NO-UNDO.
DEF VAR v-shipto-country AS cha NO-UNDO.
DEF VAR v-client-num AS cha FORM "x(4)" INIT "1526"NO-UNDO.
DEF VAR v-line AS INT FORM ">>>>9" NO-UNDO.
DEF VAR v-price AS DEC NO-UNDO.
DEF VAR v-price-code AS cha FORM "x(2)" NO-UNDO.
DEF VAR v-size AS cha NO-UNDO.
DEF VAR v-color AS cha NO-UNDO.
DEF VAR v-upc AS cha NO-UNDO.
DEF VAR v-catalog AS cha NO-UNDO.
DEF VAR v-article AS cha NO-UNDO.
DEF VAR v-inv-as-of-date AS DATE NO-UNDO.
DEF VAR v-merch-amt AS DEC NO-UNDO.
DEF VAR v-date-ship AS DATE NO-UNDO.
DEF VAR v-store AS cha NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-shipvia AS cha NO-UNDO.
DEF VAR v-pay-code AS cha NO-UNDO.
DEF VAR v-po-date AS DATE NO-UNDO.
DEF VAR v-po-no LIKE inv-line.po-no NO-UNDO.
DEF VAR v-tax-amt AS DEC NO-UNDO.
DEF VAR v-misc-amt AS DEC NO-UNDO.
DEF VAR v-allowance-amt AS DEC NO-UNDO.
DEF VAR v-vend-id AS cha NO-UNDO.
DEF VAR v-num-cust AS INT NO-UNDO.
DEF VAR v-num-inv AS INT NO-UNDO.
DEF VAR v-num-cm AS INT NO-UNDO.
DEF VAR v-tot-inv AS DEC NO-UNDO.
DEF VAR v-tot-cm AS DEC NO-UNDO.
DEF VAR v-pr-uom AS cha NO-UNDO.
DEF VAR v-tmp-price AS DEC NO-UNDO.

{oe/citexprt.i}

for each tt-report NO-LOCK where tt-report.term-id eq "" /*v-term-id*/ 
                             AND tt-report.key-02 = "factored" ,
        first inv-head where recid(inv-head) eq tt-report.rec-id no-lock
        /*break by tt-report.key-01
              by tt-report.key-02*/:

      FIND FIRST cust WHERE cust.company = inv-head.company
                        AND cust.cust-no = inv-head.cust-no NO-LOCK NO-ERROR.

      assign  v-shipto-name = inv-head.sold-name
              v-shipto-addr[1] = inv-head.sold-addr[1]
              v-shipto-addr[2] = inv-head.sold-addr[2]
              v-shipto-city = inv-head.sold-city
              v-shipto-state = inv-head.sold-state
              v-shipto-zip = inv-head.sold-zip.

      
      find first oe-bolh where oe-bolh.company = inv-head.company and
          oe-bolh.bol-no = inv-head.bol-no use-index bol-no no-lock no-error.
      if avail oe-bolh then do:
     /*   find first oe-relh where oe-relh.company = oe-bolh.company and
                   oe-relh.r-no = oe-bolh.r-no no-lock no-error.
        if avail oe-relh then */
        find first shipto where shipto.company  = oe-bolh.company and
                   shipto.cust-no = oe-bolh.cust-no and
                   shipto.ship-id = oe-bolh.ship-id no-lock no-error.
        if avail shipto then
        assign  v-shipto-name = shipto.ship-name
                v-shipto-addr[1] = shipto.ship-addr[1]
                v-shipto-addr[2] = shipto.ship-addr[2]
                v-shipto-city = shipto.ship-city
                v-shipto-state = shipto.ship-state
                v-shipto-zip = shipto.ship-zip.

      end. /* avail oe-bolh */
      /*Customer Name & Address Record */
      PUT v-client-num  /* client number 1-4 */
          " " /* Trade Style 5 */
          "N" /* Record Type 6 */
          STRING(inv-head.cust-no) FORM "x(15)" /* Customer# 7 - 21 */
          string(inv-head.inv-no) FORM "x(8)" /* Invoice # 22 - 29 */
          FILL(" ",7) FORM "x(7)" /* filler  30-36 */
          inv-head.cust-name FORM "x(30)"  /* Bill To name  37-66 */
          inv-head.addr[1]   FORM "x(30)"  /* Bill to address1 67-96 */
          inv-head.addr[1]   FORM "x(30)"  /* Bill to address2 97-126 */
          inv-head.city FORM "x(17)"  /* bill to city 127 - 143 */
          inv-head.state FORM "x(2)"  /* bill to state 144-145 */
          inv-head.zip FORM "x(10)"   /* bill to zip 146-155*/
          v-country FORM "x(17)"     /* bill to country 156 - 172*/
          cust.area-code + cust.phone FORM "x(15)" /*Customer Phone# 173-187 */
          cust.fax FORM "x(15)" /* customer fax#  188-202*/
          v-duns FORM "x(9)"    /* DUNS# 203-211*/
          v-email FORM "x(40)"  /* Customer Email 212- 251*/
          v-shipto-name FORM "x(30)"  /*ship to name 252-281*/
          v-shipto-addr[1] FORM "x(30)" /*ship to address1 282-311*/
          v-shipto-addr[2] FORM "x(30)" /*ship to address2 312-341*/
          v-shipto-city FORM "x(17)"    /*shipto city 342-358*/
          v-shipto-state FORM "x(2)"    /*shipto state 359-360*/       
          v-shipto-zip  FORM "x(10)"    /*shipto zip 361-370*/
          v-shipto-country FORM "x(17)" /*shipto country 371-387 */
          SKIP
          .

         /* Invoice Line Item Records*/  
         v-line = 0. 
         for each inv-line no-lock where inv-line.r-no = inv-head.r-no:
             FIND FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no = inv-line.b-no NO-ERROR.
             assign v-date-ship = IF AVAIL oe-bolh THEN oe-bolh.bol-date ELSE TODAY.

             v-line = v-line + 1.
             v-price = inv-line.price * (1 - (inv-line.disc / 100)).
             v-price-code = "UM".  /* PE,PD,HP or UM*/
             v-po-no = inv-line.po-no.
             v-pr-uom = "EA".
             find first itemfg {sys/look/itemfgrlW.i}
                        and itemfg.i-no eq inv-line.i-no no-lock no-error.
             v-tmp-price = if inv-line.pr-uom begins "L" AND
                              inv-line.pr-uom NE "LB" 
                           THEN if inv-line.inv-qty lt 0 then -1 else 1
                                ELSE if inv-line.pr-uom eq "CS" then
                                     v-price / (if inv-line.cas-cnt ne 0 then inv-line.cas-cnt else
                                       if avail itemfg and itemfg.case-count ne 0
                                                   then itemfg.case-count else
                                                        1)
                           ELSE if inv-line.pr-uom eq "C" THEN v-price / 100
                           ELSE if inv-line.pr-uom eq "M" THEN v-price / 1000
                           ELSE v-price.


             PUT v-client-num
                 " "
                 "I"
                 STRING(inv-head.cust-no) FORM "x(15)" /* Customer# 7 - 21 */
                 string(inv-head.inv-no) FORM "x(8)" /* Invoice # 22 - 29 */
                 FILL(" ",7) FORM "x(7)" /* filler  30-36 */
                 v-line FORM ">>>>9" /* Sequential Line# 37-41 */
                 inv-line.inv-qty FORM "->>>>>>>>9"  /*Invoice Qty 42-51*/
                 v-pr-uom FORM "x(2)"   /*Unit or Basis for Measurement code 52-53
                                                 Valid Values- EA,DZ,PR or YD */
                 v-tmp-price  FORM "->>>>>>>>>>>>>9.9<<<<" /* unit price 54-70*/                 
                 v-price-code FORM "x(2)" /*Basis of Unit Price Code 71-72*/
                 v-upc     FORM "x(20)"   /* UPC Number 73-92 */
                 v-catalog FORM "x(20)"   /* Buyer's catalog number 93-112*/
                 inv-line.i-no FORM "x(20)" /* Vendor Style# 113-132*/
                 v-article FORM "x(20)"     /*Europen Article # 133-152*/
                 inv-line.i-name FORM "x(30)" /*Item Description Line1 153-182*/
                 inv-line.part-dscr1 FORM "x(30)" /*Item Description Line2 183-212*/
                 v-color FORM "x(20)"  /* Color Description 213-232*/
                 v-size FORM "x(20)"   /* Size 233-252*/
                 SKIP.
         END.
         v-misc-amt = 0.
         for each inv-misc no-lock where inv-misc.company = inv-head.company and
                                    inv-misc.r-no = inv-head.r-no and
                                    inv-misc.bill = "Y" :
             v-misc-amt = v-misc-amt + inv-misc.amt.
         END.
         
         /* Invoice Summary Record */
         v-inv-freight = if inv-head.f-bill THEN inv-head.t-inv-freight ELSE 0.
         v-tax-amt = IF inv-head.tax-gr <> "" THEN inv-head.t-inv-tax ELSE 0.
         find FIRST carrier where carrier.company = inv-head.company and
                             carrier.carrier = inv-head.carrier no-lock no-error.
         v-shipvia = if avail carrier THEN carrier.dscr ELSE "".
         ASSIGN v-merch-amt = v-merch-amt * 100
                v-allowance-amt = v-allowance-amt * 100
                v-misc-amt = v-misc-amt * 100
                v-inv-freight = v-inv-freight * 100
                v-tax-amt = v-tax-amt * 100
                .    
         PUT v-client-num
             " "
             "D"      /* D for Invoice, C for C/M*/
             STRING(inv-head.cust-no) FORM "x(15)" /* Customer# 7 - 21 */
             string(inv-head.inv-no) FORM "x(8)" /* Invoice # 22 - 29 */
             FILL(" ",7) FORM "x(7)" /* filler  30-36 */
             v-line FORM ">>>>9" /* invoice item count 37-41*/
             inv-head.t-inv-rev * 100 FORM "->>>>>>>>9"  /* Invoice,C/M amount 42-51*/
             inv-head.inv-date FORM "999999"   /* Invoice Date 52-57*/
             v-inv-as-of-date FORM "999999"    /* Invoice As of Date 58-63 */
             v-date-ship FORM "999999"  /* Ship Date 64-69*/
             FILL(" ",6)  FORM "x(6)"   /* Reserved 70-75*/
             inv-head.terms FORM "x(3)" /* Terms Code 76-78 */
             inv-head.terms-d FORM "x(30)" /* Terms description 79-108*/
             v-merch-amt FORM "->>>>>>>>9" /* Merchandise AMount 109-118 */
             v-store FORM "x(5)" /* Store # 119-123*/
             v-po-no FORM "x(22)"  /* PO# 146-151*/
             v-po-date FORM "999999"  /* PO Date 152-157*/
             fill(" ",33) FORM "x(33)" /* 6+1+1+10+8+7 dept,RiskCode Discount Type,discount amount*/
             v-inv-freight FORM "->>>>>>>>9"
             v-tax-amt FORM "->>>>>>>>9"
             v-misc-amt FORM "->>>>>>>>9"
             v-allowance-amt FORM "->>>>>>>>9"
             v-vend-id FORM "x(15)"
             v-shipvia FORM "x(30)"
             v-pay-code FORM "x(2)"
             SKIP.
         ASSIGN v-num-cust = v-num-cust + 1
                v-num-inv = v-num-inv + 1                
                v-tot-inv = v-tot-inv + inv-head.t-inv-rev
                .
end. /* each tt-report */

/* Assignment Total Record */
ASSIGN v-tot-inv = v-tot-inv * 100
       v-tot-cm = v-tot-cm * 100
       .
PUT v-client-num  /* 1-4*/
    " "
    "S" 
    v-num-cust FORM ">>>>>9" /*Number of Customer 7-12*/
    v-num-inv FORM ">>>>>9"  /*Number of invoices 13-18*/
    v-num-cm  FORM ">>>>>9"  /*Number of Credit Memos 19-24*/
    v-tot-inv FORM "->>>>>>>>>>9"  /* Total Invoice Amt 25-36*/
    v-tot-cm  FORM "->>>>>>>>>>9"  /* Total CM Amt  37-48*/
    v-assign-num FORM ">>>9"       /* Assignment Number 49-52*/
    v-assign-date FORM "999999"    /* Assignment Date 53-58*/
    "0"                            /* Factoring Fee Code */
    SKIP.
/*Transmission Total Record*/
PUT "9999"  /* 1-4*/
    " "
    "T" 
    v-num-cust FORM ">>>>>9" /*Number of Customer 7-12*/
    v-num-inv FORM ">>>>>9"  /*Number of invoices 13-18*/
    v-num-cm  FORM ">>>>>9"  /*Number of Credit Memos 19-24*/
    v-tot-inv FORM "->>>>>>>>>>9"  /* Total Invoice Amt 25-36*/
    v-tot-cm  FORM "->>>>>>>>>>9"  /* Total CM Amt  37-48*/
    v-assign-date FORM "999999"    /* Assignment Date 49-54*/
    SKIP.

DO TRANSACTION:
   FIND CURRENT reftable .
   reftable.val[1] = reftable.val[1] + 1.
END.


/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
