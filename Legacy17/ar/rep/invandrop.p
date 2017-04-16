/* ---------------------------------------------- ar/rep/invandrop.p          */
/* PRINT INVOICE   Xprint form for Androp Packing                             */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{ar/rep/invoice.i}
{custom/notesdef.i}

DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.
def var v-salesman as char format "x(14)" NO-UNDO.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO.
def var v-addr3 as char format "x(30)" NO-UNDO.
def var v-sold-addr3 as char format "x(30)" NO-UNDO.
def var v-shipto-name as char format "x(30)" NO-UNDO.
def var v-shipto-addr as char format "x(30)" extent 2 NO-UNDO.
def var v-shipto-city as char format "x(15)" NO-UNDO.
def var v-shipto-state as char format "x(2)" NO-UNDO.
def var v-shipto-zip as char format "x(10)" NO-UNDO.
def var v-line as int NO-UNDO.
def var v-printline as INT NO-UNDO.
def var v-t-weight like ar-invl.t-weight NO-UNDO.
def var v-inv-no as int NO-UNDO.
def var v-tot-cas as dec format "->>>9.9999" NO-UNDO.
def var v-tot-pallets as int NO-UNDO.
def var v-tot-qty as INT NO-UNDO.
def var v-inv-date as date initial TODAY FORM "99/99/9999" NO-UNDO.
def shared var v-fr-tax as logical initial no NO-UNDO.
def var v-tax-rate as dec format "->>>.99" NO-UNDO.
def var v-tax-code like stax.tax-code NO-UNDO.
def var v-tx-rate like stax.tax-rate NO-UNDO.
def var v-ans as logical initial no NO-UNDO.
def var v-date-ship as date initial today NO-UNDO.
def var v-del-no as int format ">>>>>>" NO-UNDO.
def var v-bol-cases LIKE oe-boll.cases NO-UNDO.
def var v-set-qty AS INT NO-UNDO.
def var v-part-qty AS DEC FORMAT "999.9999" NO-UNDO.
def var v-net like inv-head.t-inv-rev NO-UNDO.
def var v-case-cnt as char format "x(80)" extent 5 NO-UNDO.
def var v-case-line as char NO-UNDO.
def var v-part-line as char NO-UNDO.
def var tmp1 as dec NO-UNDO.
def var tmp2 as date NO-UNDO.
def var net1 as dec NO-UNDO.
def var net2 as dec NO-UNDO.
def var net3 as dec NO-UNDO.
def var cnt as int NO-UNDO.
def var disp-frt as char init "Freight:" format "x(8)" NO-UNDO.
def var minus-ship as int NO-UNDO.
DEF VAR lv-bol-no LIKE oe-bolh.bol-no NO-UNDO.
DEF VAR v-due-date AS DATE FORM "99/99/9999" NO-UNDO.

DEF BUFFER xar-inv FOR ar-inv.
DEF BUFFER xar-invl FOR ar-invl.

def TEMP-TABLE w-sman
  field sman as char format "x(4)".

def var v-ord-del-hdr as char format "x(3)" init "Del" NO-UNDO.
def var v-beeler-lines as INT NO-UNDO.
def var v-part-info as char format "x(30)" NO-UNDO.
def var v as INT NO-UNDO.
def var v-bo-qty as int format "99999" no-undo.
def var v-inv-qty as dec no-undo.
def var v-ship-qty as int format "99999" no-undo.
def var v-i-no as char format "x(15)" no-undo.
def var v-i-dscr as char format "x(18)" no-undo.
def var v-price as dec format ">>>>9.9999" no-undo.
def var v-t-price as dec format ">>>>>>9.99" no-undo.
def var v-po-no like ar-invl.po-no no-undo.
def var v-bill-i as char format "x(25)" no-undo.
def var v-ord-no like oe-ord.ord-no no-undo.
def var v-ord-date like oe-ord.ord-date no-undo.
def var v-ship-i as char format "x(25)" no-undo.
def var v-rel-po-no like oe-rel.po-no no-undo.
def var v-price-head as char format "x(5)" no-undo.
DEF VAR v-subtot-lines AS DEC NO-UNDO.
def TEMP-TABLE w-tax
    field w-dsc as   char
    field w-tax as   dec.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-lines AS INT NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-frt-tax AS DEC NO-UNDO.
DEF VAR lv-inv-list AS CHAR NO-UNDO.
DEF VAR v-tot-tax AS DEC NO-UNDO.

FIND FIRST ar-inv NO-LOCK NO-ERROR.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(50)" NO-UNDO.

ASSIGN ls-image1 = "images\androp.jpg".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-email AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR v-page-num AS INT NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "INVPRINT" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".
   
    find first company where company.company = cocode no-lock no-error.

     ASSIGN v-comp-add1 = ""
        v-comp-add2 = "" 
        v-comp-add3 = ""
        v-comp-add4 = ""
        v-comp-add5 = "".

 IF lv-display-comp THEN DO:
    FIND FIRST cust WHERE cust.company = cocode AND
                       cust.active = "X" NO-LOCK NO-ERROR.
    IF AVAIL cust THEN
       ASSIGN v-comp-add1 = cust.addr[1]
           v-comp-add2 = cust.addr[2]
           v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
           v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
           v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
           lv-email    = "Email:  " + cust.email 
           lv-comp-name = cust.NAME   
           .
 END.
    

    find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

    for each report where report.term-id eq v-term-id no-lock,
        first ar-inv where recid(ar-inv) eq report.rec-id no-lock,
        FIRST cust WHERE cust.company = ar-inv.company
                     AND cust.cust-no = ar-inv.cust-no NO-LOCK 

        break by (IF v-print-fmt EQ "ASIXprnt" THEN "" ELSE ar-inv.cust-no)
              by ar-inv.inv-no:
     
        find first carrier where carrier.company eq cocode
             and carrier.carrier eq ar-inv.carrier no-lock no-error.
        if avail carrier THEN ASSIGN v-shipvia = carrier.dscr.
        else assign v-shipvia = "".

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
                   v-addr3 = ar-inv.city + ", " + ar-inv.state + "  " + ar-inv.zip
                   .
         v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip .


        if ar-inv.fob-code begins "ORIG" then
         assign v-fob = "Origin".
        else
         assign v-fob = "Destination".

        assign
          
          v-line = 1
          v-printline = 0.
    
        find first stax
            {sys/ref/stax1W.i}
              and {sys/ref/taxgroup.i stax} eq ar-inv.tax-code
            no-lock no-error.
        if not avail stax then
           find first stax where stax.company = ar-inv.company AND stax.tax-group eq ar-inv.tax-code
               no-lock no-error.
        if not avail stax then
           find first stax where stax.tax-group eq ar-inv.tax-code no-lock no-error.

        if avail stax then
          assign v-tax-rate = stax.tax-rate[1] +
                              stax.tax-rate[2] + stax.tax-rate[3]
                 v-tax-code[1] = stax.tax-code[1]
                 v-tax-code[2] = stax.tax-code[2]
                 v-tax-code[3] = stax.tax-code[3]
                 v-tx-rate[1]  = stax.tax-rate[1]
                 v-tx-rate[2]  = stax.tax-rate[2]
                 v-tx-rate[3]  = stax.tax-rate[3].

        assign v-tot-pallets = 0
               v-date-ship   = ar-inv.inv-date.
        
       for each ar-invl NO-LOCK
           where ar-invl.x-no  eq ar-inv.x-no  
             and (ar-invl.misc eq no or ar-invl.billable)
           break by ar-invl.i-no:
         do i = 1 to 3:
          if ar-invl.sman[i] ne "" then do:
            create w-sman.
            assign w-sman.sman = ar-invl.sman[i].
          end.
         end.
         assign v-tot-qty = v-tot-qty + ar-invl.ship-qty
                v-t-weight = v-t-weight + (round(ar-invl.t-weight /
                            ar-invl.qty, 2) * ar-invl.inv-qty).

         v-tot-pallets = 0.
        
         FIND FIRST oe-bolh WHERE oe-bolh.b-no = ar-invl.b-no NO-LOCK NO-ERROR.
         IF AVAIL oe-bolh THEN v-date-ship = oe-bolh.bol-date.

         if last-of(ar-invl.i-no) then do:
           if ar-invl.est-no ne "" then
           do:
             find first eb where eb.company = ar-invl.company and
               eb.est-no = ar-invl.est-no and
               eb.form-no = ar-invl.form-no and
               eb.blank-no = ar-invl.blank-no no-lock no-error.

             IF ar-invl.form-no = 0 AND ar-invl.est-type = 2 THEN
             DO:
               FOR EACH fg-set NO-LOCK WHERE fg-set.company = ar-invl.company
                                       AND fg-set.set-no = ar-invl.i-no:
                    ASSIGN v-set-qty = v-set-qty + fg-set.part-qty.
               END.
               IF v-set-qty = 0 THEN ASSIGN v-set-qty = 1.
               FOR EACH eb NO-LOCK WHERE eb.company = ar-invl.company AND
                              eb.est-no = ar-invl.est-no AND
                              eb.form-no NE 0:
                 FIND fg-set WHERE fg-set.company = ar-invl.company AND
                    fg-set.set-no = ar-invl.i-no  AND
                    fg-set.part-no = eb.stock-no NO-LOCK NO-ERROR.

                 IF AVAIL fg-set AND fg-set.part-qty NE 0 THEN
                   ASSIGN v-part-qty = fg-set.part-qty / v-set-qty.
                 ELSE
                   ASSIGN v-part-qty = 1 / v-set-qty.


                IF eb.cas-cnt = 0 THEN
                   ASSIGN v-tot-cas = ROUND((v-t-weight * v-part-qty) /
                                      eb.cas-wt, 2).
                 ELSE
                   ASSIGN v-tot-cas = ROUND((v-tot-qty * v-part-qty) /
                                      eb.cas-cnt, 2).
                 if v-bol-cases ne 0 then
                   assign v-tot-cas = v-bol-cases.
                 
               END. /* each eb */
             END. /* do */
             ELSE
             IF AVAIL eb THEN
             DO:
               IF eb.cas-cnt = 0 THEN
                 ASSIGN v-tot-cas = ROUND(v-t-weight / eb.cas-wt, 2).
               ELSE
                 ASSIGN v-tot-cas = ROUND(v-tot-qty / eb.cas-cnt, 2).
               if v-bol-cases ne 0 then
                 assign v-tot-cas = v-bol-cases.
               
             END. /* do */
           end. /* est-no ne "" */
          assign
             v-t-weight = 0
             v-tot-cas = 0
             v-tot-qty = 0.
         end. /* last-of i-no */
        end. /* each ar-invl */
    
                        /** Build Salesman Id String **/
        v-salesman = "".
        for each w-sman break by w-sman.sman:
          if first-of(w-sman.sman) then
            assign v-salesman = v-salesman + w-sman.sman.
          delete w-sman.
        end.

        assign v-po-no = ar-inv.po-no
               v-bill-i = ar-inv.bill-i[1]
               v-ord-no = ar-inv.ord-no
               v-ord-date = ar-inv.ord-date.

        find first ar-invl
             where ar-invl.x-no  eq ar-inv.x-no  
               and (ar-invl.misc eq no or ar-invl.billable)
            no-lock no-error.
        if avail ar-invl then
           assign v-price-head = ar-invl.pr-uom
                  v-po-no = IF ar-invl.po-no <> "" THEN ar-invl.po-no ELSE ar-inv.po-no
                  v-ord-no = ar-invl.ord-no
                  lv-bol-no = ar-invl.bol-no.

        /* display heder info 
         view frame invhead-comp.  /* Print headers */  */
        IF v-salesman = "" THEN v-salesman = cust.sman.

        ASSIGN
           v-inv-date = ar-inv.inv-date
           v-due-date = ar-inv.due-date
           v-tot-pallets = 0
           v-t-weight = 0.

        for each ar-invl where ar-invl.x-no  eq ar-inv.x-no  
               and (ar-invl.misc eq no) NO-LOCK:
            v-t-weight = v-t-weight + (round(ar-invl.t-weight /
                         ar-invl.qty, 2) * ar-invl.inv-qty).
            FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = ar-invl.b-no :
               FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = oe-bolh.company AND
                                       oe-boll.b-no = oe-bolh.b-no AND
                                       oe-boll.i-no = ar-invl.i-no AND
                                       oe-boll.ord-no = ar-invl.ord-no:
                                   /** Bill Of Lading TOTAL CASES **/
                  ASSIGN v-tot-pallets = v-tot-pallets + oe-boll.cases +
                                    (if oe-boll.partial gt 0 then 1 else 0).
               END. /* each oe-boll */
            END.
        END.

        {ar/rep/invandrop.i}

        v-subtot-lines = 0.
        v-t-tax = 0.
        for each ar-invl no-lock
             where ar-invl.x-no  eq ar-inv.x-no  
               and (ar-invl.misc eq no or ar-invl.billable) 
              BY ar-invl.misc  BY ar-invl.i-no:
          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = ""
                 v-line = v-line + 1
                 v-beeler-lines = 0
                 lv-inv-list = "".

            find first oe-ordl where oe-ordl.company = cocode and
                                     oe-ordl.ord-no = ar-invl.ord-no and
                                     oe-ordl.i-no = ar-invl.i-no
                                     no-lock no-error.
            if avail oe-ordl THEN DO:
              assign v-bo-qty = if (ar-invl.qty - ar-invl.ship-qty -
                                    oe-ordl.t-ship-qty) < 0 then 0 else
                                   (ar-invl.qty - ar-invl.ship-qty -
                                    oe-ordl.t-ship-qty).

              IF NOT CAN-FIND(FIRST oe-boll
                              WHERE oe-boll.company EQ ar-invl.company
                                AND oe-boll.b-no    EQ ar-invl.b-no
                                AND oe-boll.po-no   EQ ar-invl.po-no
                                AND oe-boll.ord-no  EQ oe-ordl.ord-no
                                AND oe-boll.i-no    EQ oe-ordl.i-no
                                AND oe-boll.line    EQ oe-ordl.line
                                AND oe-boll.s-code  EQ "I"
                              USE-INDEX b-no) THEN
              FOR EACH xar-invl
                  WHERE xar-invl.company EQ oe-ordl.company
                    AND xar-invl.ord-no  EQ oe-ordl.ord-no
                    AND xar-invl.i-no    EQ oe-ordl.i-no
                    AND ROWID(xar-invl)  NE ROWID(ar-invl)
                    AND CAN-FIND(FIRST oe-boll
                                 WHERE oe-boll.company EQ xar-invl.company
                                   AND oe-boll.b-no    EQ xar-invl.b-no
                                   AND oe-boll.po-no   EQ xar-invl.po-no
                                   AND oe-boll.ord-no  EQ oe-ordl.ord-no
                                   AND oe-boll.i-no    EQ oe-ordl.i-no
                                   AND oe-boll.line    EQ oe-ordl.line
                                   AND oe-boll.s-code  EQ "I"
                                 USE-INDEX b-no)
                    
                  NO-LOCK:
                lv-inv-list = lv-inv-list + TRIM(STRING(xar-invl.inv-no,">>>>>>>>>>")) + " ".
              END.
            END.

            else
              assign v-bo-qty = if ( ar-invl.qty - ar-invl.ship-qty ) < 0
                                  then 0 else ar-invl.qty - ar-invl.ship-qty.

            assign v-inv-qty = ar-invl.ship-qty /* ar-invl.qty */
                   v-ship-qty = ar-invl.ship-qty
                   v-i-no = ar-invl.i-no
                   v-i-dscr = ar-invl.i-name
                   v-price = ar-invl.unit-pr * (1 - (ar-invl.disc / 100))
                   v-t-price = ar-invl.amt
                   v-subtot-lines = v-subtot-lines + ar-invl.amt.

                if ar-invl.tax and avail stax then
                do i = 1 to 3:
                  if stax.tax-code[i] ne "" then do:
                    create w-tax.
                    assign
                     w-dsc      = stax.tax-dscr[i]
                     w-tax      = round((if stax.company eq "yes" then v-t-price
                                                                  else ar-invl.amt) *
                                        stax.tax-rate[i] / 100,2)
                     v-t-price  = v-t-price + w-tax
                     v-t-tax[i] = v-t-tax[i] + w-tax
                     v-lines    = v-lines + 1.
                  end.
                end.

                if v-t-price ne ar-invl.amt then do:
                  create w-tax.
                  assign
                   w-dsc     = "******ITEM TOTAL:"
                   w-tax     = v-t-price
                   v-lines   = v-lines + 1.
                end.

             PUT space(1) v-inv-qty format "->>>>>9" SPACE(2)
                v-i-no  format "x(15)" SPACE(3)
                v-i-dscr  format "x(30)" SPACE(7)
                v-price  format "->>>,>>9.99<<" SPACE(6)
                ar-invl.amt  format "->>>,>>9.99"                     
                SKIP.

             v-printline = v-printline + 1.
      
            do v = 1 to 2:
              v-part-info = if v eq 1 then (IF ar-invl.part-dscr1 <> "" THEN ar-invl.part-dscr1 ELSE ar-invl.i-dscr)
                            else
                            if v eq 2 then ar-invl.part-dscr2
                            else  "".

              if v-part-info ne "" OR (v = 1 AND ar-invl.part-no <> "") then do:
                 IF v = 1 THEN PUT SPACE(28) v-part-info SKIP.
                 ELSE IF v = 2 THEN PUT SPACE(28) v-part-info SKIP.
                 
                 v-printline = v-printline + 1.
              end.
            end.
            put skip(1).
            v-printline = v-printline + 1.

            IF v-printline > 45 THEN DO:
               PAGE.
               v-printline = 0.
               {ar/rep/invandrop.i}
            END.

        end. /* each ar-invl */

        lv-line-chars = 80.
        {custom/notesprtA.i ar-inv v-inst 4}
         DO i = 1 TO 4:
            IF v-inst[i] <> "" THEN DO:                
               IF v-printline > 45 THEN DO:
                  PAGE.
                  v-printline = 0.
                  {ar/rep/invandrop.i}
               END.
               PUT v-inst[i] SKIP.
               v-printline = v-printline + 1.
            END.            
        end.

        IF v-printline > 45 THEN DO:
           PAGE.
           v-printline = 0.
           {ar/rep/invandrop.i}
        END.

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
                 w-tax      = round((if stax.company eq "yes" then v-frt-tax
                                                         ELSE ar-inv.freight) *
                                        stax.tax-rate[i] / 100,2)                 
                 v-frt-tax  = v-frt-tax + w-tax
                 v-t-tax[i] = v-t-tax[i] + w-tax
                 v-lines    = v-lines + 1.
           END.
        end.      

    v-tot-tax = 0.
    do i = 1 to 3:
       ASSIGN
          v-bot-lab[i] = if v-t-tax[i] ne 0 then
                        ((IF AVAIL stax THEN string(CAPS(stax.tax-code[i]),"x(5)") 
                           ELSE FILL(" ",5) ) +
                       fill(" ",6) + ":" +
                       string(v-t-tax[i],"->>>>>9.99")) else ""
          v-tot-tax = v-tot-tax + v-t-tax[i].
    end.
    v-inv-freight = if (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
                    THEN ar-inv.freight ELSE 0.

    PUT "<R49><C53><#8>"
             "<=8>           Sub Total  " v-subtot-lines FORM "->>,>>9.99"
        "<=8><R+1>           Sales Tax  " v-tot-tax FORM "->>,>>9.99"
        "<=8><R+2>Total Invoice Amount  " v-subtot-lines + v-tot-tax + v-inv-freight FORM "->>,>>9.99"
        "<=8><R+3>    Payment Received  "
        "<=8><R+4>          Grand Total " v-subtot-lines + v-tot-tax + v-inv-freight FORM "->>,>>9.99" 
        "<=8><C-43><FGCOLOR=RED>Please Remit To" 
        "<=8><R+1><C-43>Androp Packaging / Ontario Foam" 
        "<=8><R+2><C-43>4400 E. Francis Street"
        "<=8><R+3><C-43>Ontario, CA 91761<FGCOLOR=BLACK>"
        "<=8><R+4><C-43>Thank You" 
        "<=8><C-52><R+5><FROM><C+79><LINE><||3>"
        "<LPI=8><R72.5><C1><P7><FArial>Buyer agrees with seller,  Androp Packaging / Ontario Foam, 4400 E. Francis St. Ontario, CA 91761  to pay for"
        "<R73.5><C1>all purchase within 30 days. All charges billed but not paid at the end of 30 days will be considered delinquent and subject to finance charges. Finance charges are computed by a"
        "<R74.5><C1>periodic rate of 1.5% per month, witch is annual percentage rate of 18% applied to the “previous” unpaid balance less current credits. The minimum payment due will be payment"
        "<R75.5><C1>to buyer’s Indebtedness over 30 days. If not paid, Androp Packaging / Ontario Foam may declare the unpaid balance due and payable immediately. Buyer agrees to pay all collection expenses"
        "<R76.5><C1>including reasonable Attorneys fees and court cost, if it is necessary to collect through action."
        "<R77.5><C1>Buyer assumes full responsibility for all material purchased from  Androp Packaging / Ontario Foam, and agrees to be personally liable of all charges promptly.  Buyer agrees to notify"
        "<R78.5><C1>Androp Packaging / Ontario Foam in writing within 10 days of receipt of invoice of any discrepancy in billing. Failure to notify Androp Packaging / Ontario Foam signifies total acceptance and"
        "<R79.5><C1>responsibility for payments according to terms. Androp Packaging / Ontario Foam reserves the right to send out “Notice to Owners” and files liens on past due accounts, and use any and all"
        "<R80.5><C1>legal means available to force collection, if such action becomes necessary."
        "<R82><C1>All damaged and/or defective items must be reported by Buyer to Androp Packaging / Ontario Foam in writing within (3) days of delivery of the items to Buyer. Buyer’s failure to"
        "<R83><C1>do so shall constitute Buyer’s absolute waiver of any claims for damaged or defective items included in such delivery." 
        "<R84.5><C7>Androp Packaging / Ontario Foam may cancel courtesy agreement at any time it deems it necessary.<P10>"
        "<LPI=6>".

    ASSIGN
       v-printline = v-printline + 6
       v-page-num = PAGE-NUM.
    
    PAGE. 

    DO TRANSACTION:
       FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
       ASSIGN xar-inv.printed = yes.

    END. /* DO TRANSACTION avail ar-inv */ 
 
end. /* each report, ar-inv */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
