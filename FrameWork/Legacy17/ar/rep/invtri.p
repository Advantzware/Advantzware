/* ---------------------------------------------- ar/rep/invoracl.p  */
/* PRINT INVOICE   Xprint Standard Form  for Oracle           */
/* -------------------------------------------------------------------------- */


{sys/inc/var.i shared}

{ar/rep/invoice.i}
{custom/notesdef.i}
DEF VAR v-inst AS cha FORM "x(60)" EXTENT 4 NO-UNDO.

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
def var v-pitch like asi.printer.pitch NO-UNDO.
def var v-len as int NO-UNDO.
def var v-hldpitch like asi.printer.pitch NO-UNDO.
def var v-t-weight like ar-invl.t-weight NO-UNDO.
def var v-inv-no as int NO-UNDO.
def var v-tot-cas as dec format "->>>9.9999" NO-UNDO.
def var v-tot-pallets as int NO-UNDO.
def var v-tot-qty as INT NO-UNDO.
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
DEF VAR v-slsname AS cha FORM "x(20)" NO-UNDO.
def buffer xar-inv for ar-inv .
DEF VAR lv-got-misc AS LOG NO-UNDO.

def workfile w-sman
  field sman as char format "x(4)"
  FIELD sname AS cha FORM "x(20)"  .

def var v-ord-del-hdr as char format "x(3)" init "Del".
def var v-beeler-lines as int.
def var v-part-info as char format "x(30)".
def var v as int.
def var v-bo-qty as int format "99999" no-undo.
def var v-inv-qty as int format "99999" no-undo.
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
def workfile w-tax
    field w-dsc as   char
    field w-tax as   dec.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-lines AS INT NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-frt-tax AS DEC NO-UNDO.

FIND FIRST ar-inv NO-LOCK NO-ERROR.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(50)" NO-UNDO.
DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR lv-tot-pg AS INT NO-UNDO.
DEF VAR ln-cnt AS INT NO-UNDO.

ASSIGN ls-image1 = "images\trilake.jpg"
       FILE-INFO:FILE-NAME = ls-image1
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
DEF VAR lv-bol-no LIKE oe-bolh.bol-no NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "INVPRINT" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company eq cocode NO-LOCK. 

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
           lv-comp-name = cust.NAME.
 END.

  find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

  for each report where report.term-id eq v-term-id no-lock,
      first ar-inv where recid(ar-inv) eq report.rec-id no-lock
      break by ar-inv.cust-no
            by ar-inv.inv-no:
   
      FIND FIRST cust WHERE cust.company = ar-inv.company
                        AND cust.cust-no = ar-inv.cust-no NO-LOCK NO-ERROR.

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
                 v-addr3 = ar-inv.city + ", " + ar-inv.state + "  " + ar-inv.zip.

      v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip .

      if ar-inv.fob-code begins "ORIG" THEN assign v-fob = "Origin".
      ELSE assign v-fob = "Destination".

      ASSIGN v-line = 1
             v-printline = 0.

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

        assign v-tot-pallets = 0
               ln-cnt = 0
               lv-tot-pg = 1.

        for each ar-invl no-lock where ar-invl.x-no = ar-inv.x-no break by ar-invl.i-no:
           do i = 1 to 3:
             if ar-invl.sman[i] ne "" then do:
               create w-sman.
               assign w-sman.sman = ar-invl.sman[i].
               FIND FIRST sman WHERE sman.company = ar-invl.company
                              AND sman.sman = ar-invl.sman[i] NO-LOCK NO-ERROR.
               w-sman.sname = IF AVAIL sman THEN sman.sname ELSE "".
             end.
           end.
           assign v-tot-qty = v-tot-qty + ar-invl.ship-qty
                  v-t-weight = v-t-weight + (round(ar-invl.t-weight /
                               ar-invl.qty, 2) * ar-invl.inv-qty)
                  v-tot-pallets = 0.
           
           if last-of(ar-invl.i-no) then do:
              if ar-invl.est-no ne "" THEN do:
                 find first eb where eb.company = ar-invl.company and
                      eb.est-no = ar-invl.est-no and
                      eb.e-num = ar-invl.e-num and
                      eb.form-no = ar-invl.form-no and
                      eb.blank-no = ar-invl.blank-no no-lock no-error.

                 IF ar-invl.form-no = 0 AND ar-invl.est-type = 2 THEN DO:
                    FOR EACH fg-set NO-LOCK WHERE fg-set.company = ar-invl.company
                             AND fg-set.set-no = ar-invl.i-no:
                        ASSIGN v-set-qty = v-set-qty + fg-set.part-qty.
                    END.
                    IF v-set-qty = 0 THEN ASSIGN v-set-qty = 1.
                    FOR EACH eb NO-LOCK WHERE eb.company = ar-invl.company AND
                        eb.est-no = ar-invl.est-no AND
                        eb.e-num = ar-invl.e-num AND
                        eb.form-no NE 0:
                        FIND fg-set WHERE fg-set.company = ar-invl.company AND
                             fg-set.set-no = ar-invl.i-no  AND
                             fg-set.part-no = eb.stock-no NO-LOCK NO-ERROR.

                        IF AVAIL fg-set AND fg-set.part-qty NE 0 THEN
                           ASSIGN v-part-qty = fg-set.part-qty / v-set-qty.
                        ELSE ASSIGN v-part-qty = 1 / v-set-qty.
             
                        IF eb.cas-cnt = 0 THEN
                           ASSIGN v-tot-cas = ROUND((v-t-weight * v-part-qty) / eb.cas-wt, 2).
                        ELSE
                           ASSIGN v-tot-cas = ROUND((v-tot-qty * v-part-qty) / eb.cas-cnt, 2).
                        if v-bol-cases ne 0 THEN assign v-tot-cas = v-bol-cases.
                        
                    END. /* each eb */
                 END. /* ar-invl.form-no = 0 AND ar-invl.est-type = 2 do */
                 ELSE IF AVAIL eb THEN
                 DO:
                   IF eb.cas-cnt = 0 THEN
                      ASSIGN v-tot-cas = ROUND(v-t-weight / eb.cas-wt, 2).
                   ELSE
                      ASSIGN v-tot-cas = ROUND(v-tot-qty / eb.cas-cnt, 2).
                   if v-bol-cases ne 0 then
                      assign v-tot-cas = v-bol-cases.
                 END. /* do */

              end. /* est-no ne "" */

              /* gdm - 11180802 */
              FIND FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no = ar-invl.b-no NO-ERROR.
              ASSIGN 
                  v-date-ship = IF AVAIL oe-bolh THEN oe-bolh.bol-date ELSE TODAY
                  lv-bol-no   = ar-invl.bol-no
                  v-t-weight = 0
                  v-tot-cas = 0
                  v-tot-qty = 0.
           end. /* last-of i-no */
           ln-cnt = ln-cnt + 3.
           IF ar-invl.part-no <> "" OR ar-invl.part-dscr1 <> "" THEN ln-cnt = ln-cnt + 1.
           IF ar-invl.part-dscr2 <> "" THEN ln-cnt = ln-cnt + 1.

        end. /* each ar-invl */
        lv-got-misc = NO.
        for each ar-invm no-lock where ar-invm.company = ar-inv.company and
                 ar-invm.x-no = ar-inv.x-no :
            ln-cnt = ln-cnt + 2.
            IF NOT lv-got-misc THEN ln-cnt = ln-cnt + 2. /* lines for misc labels */
            lv-got-misc = YES.
        END.

        ASSIGN
           /*lv-tot-pg = lv-tot-pg + TRUNC( ln-cnt / 22,0)*/

           lv-tot-pg = IF ln-cnt MOD 23 = 0 THEN TRUNC( ln-cnt / 23,0)
                       ELSE lv-tot-pg + TRUNC( ln-cnt / 23,0)

        /** Build Salesman Id String **/
           v-salesman = ""
           v-slsname = "".

        for each w-sman break by w-sman.sman:
          if first-of(w-sman.sman) THEN DO:
             assign v-salesman = v-salesman + w-sman.sman
                    v-slsname = v-slsname + w-sman.sNAME.
          END.
          delete w-sman.
        end.

        {ar/rep/invtri.i}
        ASSIGN
           v-subtot-lines = 0
           v-t-tax = 0.

        for each ar-invl no-lock where ar-invl.x-no = ar-inv.x-no:
          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = ""
                 v-line = v-line + 1
                 v-beeler-lines = 0.

          do v = 1 to 2:
            v-part-info = if v eq 1 then ar-invl.part-dscr1
                          else           ar-invl.part-dscr2.

            if v-part-info ne "" then v-beeler-lines = v-beeler-lines + 1.
          end.

          v-printline = v-printline + v-beeler-lines.

          find first oe-ordl where oe-ordl.company = cocode and
                                   oe-ordl.ord-no = ar-invl.ord-no and
                                   oe-ordl.i-no = ar-invl.i-no
                                   no-lock no-error.
          if avail oe-ordl then
            assign v-bo-qty = if (ar-invl.qty - ar-invl.ship-qty -
                                  oe-ordl.t-ship-qty) < 0 then 0 else
                                 (ar-invl.qty - ar-invl.ship-qty -
                                  oe-ordl.t-ship-qty).
          else
            assign v-bo-qty = if ( ar-invl.qty - ar-invl.ship-qty ) < 0
                                then 0 else ar-invl.qty - ar-invl.ship-qty.

          assign v-inv-qty = ar-invl.qty
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

          IF v-ship-qty EQ 0 THEN v-ship-qty = ar-invl.inv-qty.
          
          ASSIGN
             v-price-head = ar-invl.pr-uom
             v-po-no = ar-invl.po-no.

          PUT v-po-no
              v-ship-qty FORMAT ">>>>>9" SPACE
              ar-invl.ord-no FORMAT ">>>>>9" SPACE
              v-i-no  format "x(15)" SPACE
              v-i-dscr  format "x(24)" .

          IF v-price GE 0 THEN
             PUT v-price format "$>>,>>9.99" SPACE(1).
          ELSE
             IF v-price GT -9999 THEN
                PUT v-price format "$->,>>9.99" SPACE(1).
          ELSE
             PUT v-price format "$->>,>>9.99".    

          PUT v-price-head FORM "x(4)"
              ar-invl.amt  format "$->>>,>>9.99"                     
              SKIP.
          v-printline = v-printline + 1.
          
           do v = 1 to 2:
             v-part-info = if v eq 1 then ar-invl.part-dscr1
                           else           ar-invl.part-dscr2.
             IF v EQ 1 AND ar-invl.part-dscr1 EQ "" AND
                           ar-invl.part-dscr2 EQ "" THEN
               v-part-info = ar-invl.i-dscr.

             
             if v-part-info ne "" OR ar-invl.part-no <> "" then do:
                IF v = 1 THEN
                DO:
                   PUT SPACE(29) ar-invl.part-no SPACE(1) v-part-info SKIP.
                   v-printline = v-printline + 1.
                END.
                ELSE IF v-part-info NE "" THEN
                DO:
                   PUT SPACE(45) v-part-info SKIP.
                   v-printline = v-printline + 1.
                END.
             end.
           end.
           put skip(1).
           v-printline = v-printline + 1.

           IF v-printline > 50 THEN DO:
              PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
              PAGE.
              v-printline = 0.
              {ar/rep/invtri.i}
           END.
        end. /* each ar-invl */

        for each ar-invm no-lock where ar-invm.company = ar-inv.company and
          ar-invm.x-no = ar-inv.x-no 
           break by ord-no with frame detailm:
          if first(ar-invm.ord-no) then
          do:
            IF v-printline > 50 THEN DO:
               PAGE.
               v-printline = 0.
               {ar/rep/invtri.i}
            END.
            put "** Miscellaneous Items **" at 23 skip(1).
            assign v-printline = v-printline + 2.
          end.
            
          put ar-invm.charge AT 10 ar-invm.dscr ar-invm.amt  SKIP.
            v-subtot-lines = v-subtot-lines + ar-invm.amt.
            v-printline = v-printline + 1.
            if ar-invm.tax and avail stax then
            do i = 1 to 3:
              if stax.tax-code[i] ne "" then do:
                create w-tax.
                assign
                 w-dsc      = stax.tax-dscr[i]
                 w-tax      = if stax.company eq "yes" then v-t-price
                              else ar-invm.amt
                 w-tax      = round(w-tax * (1 + (stax.tax-rate[i] / 100)),2) - w-tax
                 v-t-price  = v-t-price + w-tax
                 v-t-tax[i] = v-t-tax[i] + w-tax
                 v-lines    = v-lines + 1.
              end.
            end.

            if v-t-price ne ar-invm.amt then do:
              create w-tax.
              assign
               w-dsc     = "******ITEM TOTAL:"
               w-tax     = v-t-price
               v-lines   = v-lines + 1.
            end.

         IF v-printline > 53 THEN DO:
            PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
            PAGE.
            v-printline = 0.
            {ar/rep/invtri.i}
         END.

        end. /* each ar-invm */

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

       
      do i = 1 to 3:
       v-bot-lab[i] = if v-t-tax[i] ne 0 then
                        ((IF AVAIL stax THEN string(CAPS(stax.tax-code[i]),"x(5)") 
                           ELSE FILL(" ",5) ) +
                       fill(" ",6) + ":" +
                       string(v-t-tax[i],"$->>>>9.99")) else "".
      end.
      v-inv-freight = if (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
                    THEN ar-inv.freight ELSE 0.

    PUT "<R54><C60><#8><FROM><R+5><C+20><RECT> " 
        "<=8> Sub Total  :" v-subtot-lines FORM "$->>,>>9.99"
        "<=8><R+1> Freight    :" v-inv-freight FORM "$->>,>>9.99"
        "<=8><R+2> " v-bot-lab[1] 
        "<=8><R+3> " v-bot-lab[2]
        "<=8><R+4> Grand Total:" v-subtot-lines + v-t-tax[1] + v-t-tax[2] + v-t-tax[3] + v-inv-freight FORM "->>,>>9.99" 
        "<=8><R+6><C40><P14><B> Thank  You!</B><P10>"
        "<=8><R+5><C65>Page "  string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)"
        "<R56><C1><FROM><R+5><C+58><RECT>"
        "<R56><C3><B>COMMENTS:</B>"
        "<R57><C3>" ar-inv.bill-i[1] FORMAT "X(70)"
        "<R58><C3>" ar-inv.bill-i[2] FORMAT "X(70)"
        "<R59><C3>" ar-inv.bill-i[3] FORMAT "X(70)"
        "<R60><C3>" ar-inv.bill-i[4] FORMAT "X(70)".

    ASSIGN
       lv-pg-num = PAGE-NUM
       v-printline = v-printline + 6.

    page.

    DO TRANSACTION:
       FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
       ASSIGN xar-inv.printed = yes.
    END. 
 
  end. /* each ar-inv */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
