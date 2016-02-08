/* ---------------------------------------------- ar/rep/expconts.p   */
/* Export AR INVOICE   for Container Services         */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
/*{ar/rep/invoice.i}*/
DEF VAR v-term-id AS cha NO-UNDO.

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
DEF VAR v-po-line AS INT NO-UNDO.
DEF VAR v-got-edi AS LOG NO-UNDO.
{oe/cnsexprt.i}

for each report NO-LOCK where report.term-id eq v-term-id 
                         /* AND report.key-02 = "Factored",*/,
        first ar-inv where recid(ar-inv) eq report.rec-id no-lock,
        FIRST cust WHERE cust.company = ar-inv.company
                     AND cust.cust-no = ar-inv.cust-no 
                     AND cust.an-edi-cust NO-LOCK 
        break by ar-inv.cust-no
              by ar-inv.inv-no:

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
      find first stax
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

      /*Customer Name & Address Record */
      PUT v-client-num  /* client number 1-4 */
          " " /* Trade Style 5 */
          "N" /* Record Type 6 */
          STRING(ar-inv.cust-no) FORM "x(15)" /* Customer# 7 - 21 */
          string(ar-inv.inv-no) FORM "x(8)" /* Invoice # 22 - 29 */
          FILL(" ",7) FORM "x(7)" /* filler  30-36 */
          ar-inv.cust-name FORM "x(30)"  /* Bill To name  37-66 */
          ar-inv.addr[1]   FORM "x(30)"  /* Bill to address1 67-96 */
          ar-inv.addr[1]   FORM "x(30)"  /* Bill to address2 97-126 */
          ar-inv.city FORM "x(17)"  /* bill to city 127 - 143 */
          ar-inv.state FORM "x(2)"  /* bill to state 144-145 */
          ar-inv.zip FORM "x(10)"   /* bill to zip 146-155*/
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
         for each ar-invl no-lock where ar-invl.x-no = ar-inv.x-no:
             FIND FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no = ar-invl.b-no NO-ERROR.
             assign v-date-ship = IF AVAIL oe-bolh THEN oe-bolh.bol-date ELSE TODAY.

             v-line = v-line + 1.
             v-price = ar-invl.unit-pr * (1 - (ar-invl.disc / 100)).
             v-price-code = "UM".  /* PE,PD,HP or UM*/
             v-po-no = ar-invl.po-no.              
             v-pr-uom = "EA".
             v-t-price = ar-invl.amt.
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
                           ELSE v-price.
             v-subtot-lines = v-subtot-lines + ar-invl.amt.
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
             FIND FIRST oe-ordl WHERE oe-ordl.company = ar-invl.company
                                  AND oe-ordl.ord-no = ar-invl.ord-no
                                  AND oe-ordl.i-no = ar-invl.i-no NO-LOCK NO-ERROR.
             v-po-line = IF AVAIL oe-ordl THEN oe-ordl.e-num ELSE 0.
             PUT v-client-num
                 " "
                 "I"
                 STRING(ar-inv.cust-no) FORM "x(15)" /* Customer# 7 - 21 */
                 string(ar-inv.inv-no) FORM "x(8)" /* Invoice # 22 - 29 */
                 FILL(" ",7) FORM "x(7)" /* filler  30-36 */
                 v-line                 /* Sequential Line# 37-41 */
                 ar-invl.qty FORM "->>>>>>>>9"  /*Invoice Qty 42-51*/
                 v-pr-uom FORM "x(2)"  /*Unit or Basis for Measurement code 52-53
                                                 Valid Values- EA,DZ,PR or YD */
                 v-tmp-price FORM "->>>>>>>>>>>>>9.9<<<<" /* unit price 54-70*/
                 v-price-code FORM "x(2)" /*Basis of Unit Price Code 71-72*/
                 v-upc     FORM "x(20)"   /* UPC Number 73-92 */
                 v-catalog FORM "x(20)"   /* Buyer's catalog number 93-112*/
                 ar-invl.i-no FORM "x(20)" /* Vendor Style# 113-132*/
                 v-article FORM "x(20)"     /*Europen Article # 133-152*/
                 ar-invl.i-name FORM "x(30)" /*Item Description Line1 153-182*/
                 ar-invl.part-dscr1 FORM "x(30)" /*Item Description Line2 183-212*/
                 v-color FORM "x(20)"  /* Color Description 213-232*/
                 v-size FORM "x(20)"   /* Size 233-252*/
                 ar-invl.bol-no FORM ">>>>>>>9"
                 ar-invl.amt FORM "->>>>>>>9.99"
                 ar-invl.po-no FORM "x(15)"
                 v-po-line FORM ">>9"
                 SKIP.
         END.
         
         v-misc-amt = 0.
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
         v-misc-amt = v-misc-amt * 100.
         /* Invoice Summary Record */
         v-inv-freight = if (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
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
         ASSIGN v-merch-amt = v-merch-amt * 100
                v-allowance-amt = v-allowance-amt * 100.
         v-inv-amt = v-subtot-lines + v-t-tax[1] + v-t-tax[2] + v-t-tax[3] + v-inv-freight.

         PUT v-client-num
             " "
             "D"      /* D for Invoice, C for C/M*/
             STRING(ar-inv.cust-no) FORM "x(15)" /* Customer# 7 - 21 */
             string(ar-inv.inv-no) FORM "x(8)" /* Invoice # 22 - 29 */
             FILL(" ",7) FORM "x(7)" /* filler  30-36 */
             v-line FORM ">>>>9" /* invoice item count 37-41*/
             v-inv-amt * 100 FORM "->>>>>>>>9"  /* Invoice,C/M amount 42-51*/
             ar-inv.inv-date FORM "999999"   /* Invoice Date 52-57*/
             v-inv-as-of-date FORM "999999"    /* Invoice As of Date 58-63 */
             v-date-ship FORM "999999"  /* Ship Date 64-69*/
             FILL(" ",6) FORM "x(6)"    /* Reserved 70-75*/
             ar-inv.terms FORM "x(3)" /* Terms Code 76-78 */
             ar-inv.terms-d FORM "x(30)" /* Terms description 79-108*/
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
             ar-inv.bill-i[1]
             ar-inv.bill-i[2]
             SKIP.
         ASSIGN v-num-cust = v-num-cust + 1
                v-num-inv = v-num-inv + 1                
                v-tot-inv = v-tot-inv + (v-inv-amt * 100)
                .
         v-got-edi = YES.
end. /* each report */
IF v-got-edi THEN DO:
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
END.

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
