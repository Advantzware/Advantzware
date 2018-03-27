/* ---------------------------------------------- ar/rep/expfmemo.p   */
/* Export Credit Memo   for Frankston          */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
/*ar/rep/invoice.i}*/
DEF SHARED VAR v-term-id AS cha NO-UNDO.

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
DEF VAR v-inv-freight LIKE ar-inv.freight NO-UNDO.
DEF VAR v-shipvia AS cha NO-UNDO.
DEF VAR v-pay-code AS cha NO-UNDO.
DEF VAR v-po-date AS DATE NO-UNDO.
DEF VAR v-po-no LIKE ar-invl.po-no NO-UNDO.
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
DEF VAR v-inv-amt AS DEC NO-UNDO.
DEF VAR v-subtot-lines AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-tax-rate as dec format "->>>.99" NO-UNDO.
def var v-tax-code like stax.tax-code NO-UNDO.
def var v-tx-rate like stax.tax-rate NO-UNDO.
def workfile w-tax
    field w-dsc as   char
    field w-tax as   dec.
def var v-t-price as dec format ">>>>>>9.99" no-undo.
DEF VAR v-frt-tax AS DEC NO-UNDO.
DEF VAR v-inv-qty AS INT NO-UNDO.
DEF VAR v-i-no AS cha NO-UNDO.
DEF VAR v-i-name AS cha NO-UNDO.
DEF VAR v-part-dscr1 AS cha NO-UNDO.
DEF VAR v-dbcr AS cha NO-UNDO.
DEF VAR v-terms AS cha NO-UNDO.
DEF VAR v-terms-d AS cha NO-UNDO.
DEF VAR v-inv-num AS INT NO-UNDO.

{oe/citexprt.i}

for each report where report.term-id eq v-term-id no-lock,
    first ar-cash where recid(ar-cash) eq report.rec-id no-lock
        break by ar-cash.cust-no
            by ar-cash.check-no:
    v-inv-amt = 0.
      FIND FIRST cust WHERE cust.company = ar-cash.company
                        AND cust.cust-no = ar-cash.cust-no NO-LOCK NO-ERROR.
      /*
      find first shipto where shipto.company eq cocode
             and shipto.cust-no eq ar-inv.cust-no
             and shipto.ship-id eq ar-inv.ship-id no-lock no-error.
      IF AVAIL shipto THEN 
           assign  v-shipto-name = shipto.ship-name
                   v-shipto-addr[1] = shipto.ship-addr[1]
                   v-shipto-addr[2] = shipto.ship-addr[2]
                   v-shipto-city = shipto.ship-city
                   v-shipto-state = shipto.ship-state
                   v-shipto-zip = shipto.ship-zip      
                   .
     */ 
/*
      find first oe-bolh where oe-bolh.company = ar-inv.company and
          oe-bolh.bol-no = ar-invl.bol-no use-index bol-no no-lock no-error.
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
*/      
/*      find first stax
            {sys/ref/stax1W.i}
              and {sys/ref/taxgroup.i stax} eq ar-inv.tax-code
            no-lock no-error.
      if not avail stax then
        find first stax where stax.tax-group eq ar-inv.tax-code
            no-lock no-error.
   
      if avail stax then
          assign v-tax-rate = stax.tax-rate[1] +
                              stax.tax-rate[2] + stax.tax-rate[3]
                 v-tax-code[1] = stax.tax-code[1]
                 v-tax-code[2] = stax.tax-code[2]
                 v-tax-code[3] = stax.tax-code[3]
                 v-tx-rate[1]  = stax.tax-rate[1]
                 v-tx-rate[2]  = stax.tax-rate[2]
                 v-tx-rate[3]  = stax.tax-rate[3].
*/

      /*Customer Name & Address Record */
      PUT v-client-num  /* client number 1-4 */
          " " /* Trade Style 5 */
          "N" /* Record Type 6 */
          STRING(ar-cash.cust-no) FORM "x(15)" /* Customer# 7 - 21 */
          string(ar-cash.check-no) FORM "x(8)" /* Invoice # 22 - 29 */
          FILL(" ",7) FORM "x(7)" /* filler  30-36 */
          cust.name FORM "x(30)"  /* Bill To name  37-66 */
          cust.addr[1]   FORM "x(30)"  /* Bill to address1 67-96 */
          cust.addr[1]   FORM "x(30)"  /* Bill to address2 97-126 */
          cust.city FORM "x(17)"  /* bill to city 127 - 143 */
          cust.state FORM "x(2)"  /* bill to state 144-145 */
          cust.zip FORM "x(10)"   /* bill to zip 146-155*/
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
          v-subtot-lines = 0.
          v-inv-num = 0.
          for each ar-cashl NO-LOCK where ar-cashl.company eq ar-cash.company
                            and ar-cashl.c-no    eq ar-cash.c-no
                            AND (ar-cashl.amt-paid + ar-cashl.amt-disc < 0)
                            break by ar-cashl.line:

             v-line = v-line + 1.
             v-price = ar-cashl.amt-paid - ar-cashl.amt-disc.
             v-price-code = "UM".  /* PE,PD,HP or UM*/
             v-po-no = "".
             v-pr-uom = "EA".
             v-inv-qty = 1.
             v-i-no = "".
             v-i-name = "".
             v-part-dscr1 = "".         
             IF v-price < 0 THEN v-price = v-price * (-1).
             v-inv-num = IF v-inv-num = 0 THEN ar-cashl.inv-no ELSE v-inv-num.

      /*
             find first itemfg {sys/look/itemfgrlW.i}
                        and itemfg.i-no eq ar-invl.i-no no-lock no-error.
             v-tmp-price = if ar-invl.pr-uom begins "L" AND
                              ar-invl.pr-uom NE "LB" 
                           THEN if ar-invl.inv-qty lt 0 then -1 else 1
                                ELSE if ar-invl.pr-uom eq "CS" then
                                     v-price / (if ar-invl.cas-cnt ne 0 then ar-invl.cas-cnt else
                                       if avail itemfg and itemfg.case-count ne 0
                                                   then itemfg.case-count else
                                                        1)
                           ELSE if ar-invl.pr-uom eq "C" THEN v-price / 100
                           ELSE if ar-invl.pr-uom eq "M" THEN v-price / 1000
                           ELSE v-price. */
             v-subtot-lines = v-subtot-lines + v-price.
/*
             if ar-invl.tax and avail stax then
                do i = 1 to 3:
                  if stax.tax-code[i] ne "" then do:
                    create w-tax.
                    assign
                     w-dsc      = stax.tax-dscr[i]
                     w-tax      = round((if stax.accum-tax then v-t-price
                                                           else ar-invl.amt) *
                                        stax.tax-rate[i] / 100,2)
                     v-t-price  = v-t-price + w-tax
                     v-t-tax[i] = v-t-tax[i] + w-tax
                     .
                  end.
             end.
  */
             PUT v-client-num
                 " "
                 "I"
                 STRING(ar-cash.cust-no) FORM "x(15)" /* Customer# 7 - 21 */
                 string(ar-cash.check-no) FORM "x(8)" /* Invoice # 22 - 29 */
                 FILL(" ",7) FORM "x(7)" /* filler  30-36 */
                 v-line                 /* Sequential Line# 37-41 */
                 v-inv-qty FORM "->>>>>>>>9"  /*Invoice Qty 42-51*/
                 v-pr-uom FORM "x(2)"  /*Unit or Basis for Measurement code 52-53
                                                 Valid Values- EA,DZ,PR or YD */
                 v-price FORM "->>>>>>>>>>>>>9.9<<<<" /* unit price 54-70*/
                 v-price-code FORM "x(2)" /*Basis of Unit Price Code 71-72*/
                 v-upc     FORM "x(20)"   /* UPC Number 73-92 */
                 v-catalog FORM "x(20)"   /* Buyer's catalog number 93-112*/
                 v-i-no FORM "x(20)" /* Vendor Style# 113-132*/
                 v-article FORM "x(20)"     /*Europen Article # 133-152*/
                 v-i-name FORM "x(30)" /*Item Description Line1 153-182*/
                 v-part-dscr1 FORM "x(30)" /*Item Description Line2 183-212*/
                 v-color FORM "x(20)"  /* Color Description 213-232*/
                 v-size FORM "x(20)"   /* Size 233-252*/
                 SKIP.

         END.
         
         v-misc-amt = 0.
/*
         for each ar-invm no-lock where ar-invm.company = ar-inv.company and
                                    ar-invm.x-no = ar-inv.x-no :
             v-misc-amt = v-misc-amt + ar-invm.amt.
             if ar-invm.tax and avail stax then
             DO  i = 1 to 3:
              if stax.tax-code[i] ne "" then do:
                create w-tax.
                assign
                 w-dsc      = stax.tax-dscr[i]
                 w-tax      = if stax.accum-tax then v-t-price
                              else ar-invm.amt
                 w-tax      = round(w-tax * (1 + (stax.tax-rate[i] / 100)),2) - w-tax
                 v-t-price  = v-t-price + w-tax
                 v-t-tax[i] = v-t-tax[i] + w-tax
                 .
              end.
            end.
         END.
*/         
         v-misc-amt = v-misc-amt * 100.
         /* Invoice Summary Record */
  /*       v-inv-freight = if (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
                         THEN ar-inv.freight ELSE 0.    
         v-inv-freight = v-inv-freight * 100.
         v-frt-tax = ar-inv.freight.        
         IF ar-inv.tax-code <> "" and
           (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
           AND ar-inv.freight <> 0
           AND AVAIL stax THEN
         do i = 1 to 3:
           if stax.tax-code[i] ne "" AND stax.tax-frt[i] EQ YES then do:
                create w-tax.
                assign
                 w-dsc      = stax.tax-dscr[i]
                 w-tax      = round((if stax.accum-tax then v-frt-tax
                                                         ELSE ar-inv.freight) *
                                        stax.tax-rate[i] / 100,2)                 
                 v-frt-tax  = v-frt-tax + w-tax
                 v-t-tax[i] = v-t-tax[i] + w-tax
                 .
           END.
         end.      

         find FIRST carrier where carrier.company = ar-inv.company and
                             carrier.carrier = ar-inv.carrier no-lock no-error.
         v-shipvia = if avail carrier THEN carrier.dscr ELSE "".
*/         
         ASSIGN v-merch-amt = v-merch-amt * 100
                v-allowance-amt = v-allowance-amt * 100.
                v-inv-amt = v-subtot-lines + v-t-tax[1] + v-t-tax[2] + v-t-tax[3] + v-inv-freight.

         /* export memo summary */
         v-dbcr = "C".
         v-terms = "".
         v-terms-d = "".

         PUT v-client-num
             " "
             v-dbcr FORM "x"      /* D for Invoice, C for C/M*/
             STRING(ar-cash.cust-no) FORM "x(15)" /* Customer# 7 - 21 */
             string(ar-cash.check-no) FORM "x(8)" /* Invoice # 22 - 29 */
             FILL(" ",7) FORM "x(7)" /* filler  30-36 */
             v-line FORM ">>>>9" /* invoice item count 37-41*/
             v-inv-amt * 100 FORM "->>>>>>>>9"  /* Invoice,C/M amount 42-51*/
             ar-cash.check-date FORM "999999"   /* Invoice Date 52-57*/
             v-inv-as-of-date FORM "999999"    /* Invoice As of Date 58-63 */
             v-date-ship FORM "999999"  /* Ship Date 64-69*/
             FILL(" ",6) FORM "x(6)"    /* Reserved 70-75*/
             v-terms FORM "x(3)" /* Terms Code 76-78 */
             v-terms-d FORM "x(30)" /* Terms description 79-108*/
             v-merch-amt FORM "->>>>>>>>9" /* Merchandise AMount 109-118 */
             v-store FORM "x(5)" /* Store # 119-123*/
             v-po-no FORM "x(22)"  /* PO# 146-151*/
             v-po-date FORM "999999"  /* PO Date 152-157*/
             fill(" ",18) FORM "x(18)" /* 6+1+1+10 dept,RiskCode Discount Type,discount amount*/
             v-inv-num FORM ">>>>>>>9"
             fill(" ",7) FORM "x(7)"
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
                v-tot-inv = v-tot-inv + (v-inv-amt * 100)
                .
end. /* each report */

/* Assignment Total Record */
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
