/* ----------------------------------------------- oe/rep/invrudd.p 02/98 FWK */
/*                                                                            */
/* PRINT INVOICE when sys-ctrl.char-fld eq "Rudd" - O/E Module                */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/invoice.i}

def var v-salesman as char format "x(3)".
def var v-fob as char format "x(11)".
def var v-shipvia like carrier.dscr.
def var v-addr3 as char format "x(30)".
def var v-sold-addr3 as char format "x(30)".
def var v-shipto-name as char format "x(30)".
def var v-shipto-addr as char format "x(30)" extent 2.
def var v-shipto-city as char format "x(15)".
def var v-shipto-state as char format "x(2)".
def var v-shipto-zip as char format "x(10)".
def var v-line as int.
def var v-printline as int.
def var v-invhead as char format "x(13)" initial
  "I N V O I C E".
def var v-pitch like printer.pitch.
def var v-len as int.
def var v-hldpitch like printer.pitch.
def var v-t-weight like inv-line.t-weight.
def var v-tot-cas as dec format "->>>9.9999".
def var v-tot-pallets as int.
def var v-tot-qty as int.
def var v-inv-date as date initial TODAY.
def shared var v-fr-tax as logical initial no.
def var v-tax-rate as dec format "->>>.99".
def var v-tax-code like stax.tax-code.
def var v-tx-rate like stax.tax-rate.
def var v-ans as logical initial no.
def var v-date-ship as date initial today.
def var v-del-no as int format ">>>>>>".
def var v-bol-cases LIKE oe-boll.cases.
def var v-set-qty AS INT.
def var v-part-qty AS DEC FORMAT "999.9999".
def var old-frt like inv-head.t-inv-freight no-undo.
def var v-net like inv-head.t-inv-rev.
def var v-case-cnt as char format "x(80)" extent 5.
def var v-case-line as char.
def var v-part-line as char.
def var tmp1 as dec.
def var tmp2 as date.
def var net1 as dec.
def var net2 as dec.
def var net3 as dec.
def var cnt as int.
def var disp-frt as char init "Freight:" format "x(8)".
def var v-max-lines as int.
def var v-ord-no like oe-ord.ord-no no-undo.
def var v-cust-part like inv-line.part-no no-undo.
def var v-i-name like inv-line.i-name no-undo.

def buffer xinv-head for inv-head.
def buffer xinv-line for inv-line.

def var v-ord-del-hdr as char format "x(3)" init "Del".
def var v-beeler-lines as int.
def var v-part-info as char format "x(30)".
def var v as int.
def var v-inv-qty as int format "99999" no-undo.
def var v-i-no as char format "x(15)" no-undo.
def var v-price as dec format ">>>9.9999" no-undo.
def var v-t-price as dec format ">>>>9.99" no-undo.
def var v-ship-i as char format "x(25)" no-undo.
def var v-rel-po-no like oe-rel.po-no no-undo.
def var v-hdr as char format "x(80)" extent 4.
def var v-job as char format "x(9)" no-undo.
def var v-bol-no like inv-head.bol-no no-undo.

assign
 substr(v-hdr[1],60,20) = "INVOICE#  INV DATE"

 substr(v-hdr[2],01,07) = "SOLD TO"
 substr(v-hdr[2],46,07) = "SHIP TO"

 v-hdr[3] = "YOUR ORDER NO  DELIV DATE SHIP VIA             F.O.B.   " +
            "   TERMS            SLMN"

 v-hdr[4] = "    QTY  P  C JOB NUMBER    DESCRIPTION                 " +
            "      PRICE       TOTAL ".

FIND FIRST inv-head NO-LOCK NO-ERROR.

form header
     "INVOICE" at 40
     v-inv-date            to 66
     inv-head.inv-no       to 77
     skip(7)
     "SOLD TO" at 2 "SHIP TO" at 47 skip(1)
     inv-head.cust-name    at 3
     v-shipto-name         at 48
     inv-head.addr[1]      at 3
     v-shipto-addr[1]      at 48
     inv-head.addr[2]      at 3
     v-shipto-addr[2]      at 48
     v-addr3               at 3
     v-sold-addr3          at 48
     skip(2)
     "CUST ORDER#" at 1 v-rel-po-no
     "RUDD ORDER#" v-ord-no
     "TERM:" inv-head.terms-d
     skip(1)
     "QUANTITY" at 1 "DESCRIPTION" at 15 "PRICE" at 51 "TOTAL " to 79
     "--------" at 1 "-----------" at 15 "-----" at 51 "----- " to 79
     skip(1)
  with frame invhead page-top no-labels no-box no-underline stream-io width 80.

form v-inv-qty                 format ">>>>>>9-"
     v-i-no            at 15   format "x(13)"
     v-price           at 51   format ">>>>9.99<<"
     v-t-price         to 80   format ">>>>>9.99-" skip
     v-cust-part       at 15 skip
     v-i-name          at 15 skip
  with frame rudd-detail no-attr-space no-labels
        no-box no-underline down stream-io width 80.

form v-part-info       at 15
  with frame beeler no-labels no-box no-underline down stream-io width 80.

form inv-misc.charge
     inv-misc.dscr     at 29
     inv-misc.amt      to 80   format ">>>>>9.99-"
     skip(1)
  with frame detailm no-labels no-box no-underline down stream-io width 80.

form "Delivery Charge" to 65 inv-head.t-inv-freight format "->,>>9.99" to 79
     "-------------" to 79
     "Net Amount" TO 65 inv-head.t-inv-rev format "->,>>>,>>9.99" to 79
     "=============" to 79
     "Salesman:" at 1 v-salesman skip
     "Page #"
  with frame totals-comp no-labels no-box no-underline stream-io width 80.

form "Taxes- " at 1
     v-tax-code[1] at 9 format "x(4)"
     space(0) ":"
     net1 at 15 format "->>,>>9.99"
     v-tax-code[2] at 28 format "x(4)"
     space(0) ":"
     net2 at 34 format "->>,>>9.99"
     v-tax-code[3] at 46 format "x(4)"
     space(0) ":"
     net3 at 52 format "->>,>>9.99"
     "Tot Tax:" to 70
     space(0)
     inv-head.t-inv-tax  to 80 format "->>,>>9.99"
     skip(1)
  with frame tax no-labels no-box no-underline stream-io width 80.

form " " to 80
  with frame blankl no-labels no-box no-underline stream-io width 80.


    find first company where company.company = cocode no-lock no-error.
    find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

    if not v-print-head then v-hdr = "".

    v-max-lines = 28. 
        
    for each report where report.term-id eq v-term-id no-lock,
        first xinv-head where recid(xinv-head) eq report.rec-id no-lock
        break by report.key-01
              by report.key-02:

      assign  v-shipto-name = xinv-head.sold-name
              v-shipto-addr[1] = xinv-head.sold-addr[1]
              v-shipto-addr[2] = xinv-head.sold-addr[2]
              v-shipto-city = xinv-head.sold-city
              v-shipto-state = xinv-head.sold-state
              v-shipto-zip = xinv-head.sold-zip.

      v-del-no = 0.

      find first oe-bolh where oe-bolh.company = xinv-head.company and
          oe-bolh.bol-no = xinv-head.bol-no use-index bol-no no-lock no-error.
      if avail oe-bolh then do:
        find first oe-relh where oe-relh.company = oe-bolh.company and
                   oe-relh.r-no = oe-bolh.r-no no-lock no-error.
        if avail oe-relh then
        find first shipto where shipto.company  = oe-bolh.company and
                   shipto.cust-no = oe-relh.cust-no and
                   shipto.ship-id = oe-bolh.ship-id no-lock no-error.
        if avail shipto then
        assign  v-shipto-name = shipto.ship-name
                v-shipto-addr[1] = shipto.ship-addr[1]
                v-shipto-addr[2] = shipto.ship-addr[2]
                v-shipto-city = shipto.ship-city
                v-shipto-state = shipto.ship-state
                v-shipto-zip = shipto.ship-zip.

      end. /* avail oe-bolh */

      IF NOT v-reprint OR xinv-head.inv-no EQ 0 THEN
        RUN oe/get-inv#.p (ROWID(xinv-head)).

      DO TRANSACTION:
        FIND inv-head WHERE ROWID(inv-head) EQ ROWID(xinv-head).

        if inv-head.inv-date ne ? then v-inv-date = inv-head.inv-date.

        if inv-head.fob-code begins "ORIG" then
         assign v-fob = "Origin".
        else
         assign v-fob = "Destination".

        find first cust
            where cust.company eq cocode
              and cust.cust-no eq inv-head.cust-no
            no-lock no-error.

        v-salesman = if avail cust then cust.sman else "".

        find carrier where carrier.company = inv-head.company and
          carrier.carrier = inv-head.carrier no-lock no-error.
         if avail carrier then
           assign v-shipvia = carrier.dscr.
         else
           assign v-shipvia = "".
        assign
          v-addr3 = inv-head.city + ", " + inv-head.state + "  " + inv-head.zip
          v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip
          v-line = 1
          v-printline = 0.
          
        find first stax
            {sys/ref/stax1W.i}
              and {sys/ref/taxgroup.i stax} eq inv-head.tax-gr
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

        assign v-tot-pallets = 0.
        for each xinv-line no-lock where xinv-line.r-no = inv-head.r-no
          break by xinv-line.i-no:

         assign v-tot-qty = v-tot-qty + xinv-line.ship-qty
                v-t-weight = v-t-weight + (round(xinv-line.t-weight /
                            xinv-line.qty, 2) * xinv-line.inv-qty).

         FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = xinv-line.b-no AND
             oe-bolh.ord-no = xinv-line.ord-no:
           FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = oe-bolh.company AND
              oe-boll.b-no = oe-bolh.b-no AND
              oe-boll.i-no = xinv-line.i-no:

                                      /** Bill Of Lading TOTAL CASES **/
              ASSIGN v-bol-cases = v-bol-cases + oe-boll.cases.
           END. /* each oe-boll */
           assign v-date-ship = oe-bolh.bol-date
                  v-tot-pallets = v-tot-pallets + oe-bolh.tot-pallets.
         END. /* each oe-bolh */
         if last-of(xinv-line.i-no) then do:
           if xinv-line.est-no ne "" then
           do:
             find first eb where eb.company = xinv-line.company and
               eb.est-no = xinv-line.est-no and
               eb.e-num = xinv-line.e-num and
               eb.form-no = xinv-line.form-no and
               eb.blank-no = xinv-line.blank-no no-lock no-error.

             IF xinv-line.form-no = 0 AND xinv-line.est-type = 2 THEN
             DO:
               FOR EACH fg-set NO-LOCK WHERE fg-set.company = xinv-line.company
                  AND fg-set.set-no = xinv-line.i-no:
                 ASSIGN v-set-qty = v-set-qty + fg-set.part-qty.
               END.
               IF v-set-qty = 0 THEN
                  ASSIGN v-set-qty = 1.
               FOR EACH eb NO-LOCK WHERE eb.company = xinv-line.company AND
                  eb.est-no = xinv-line.est-no AND
                  eb.e-num = xinv-line.e-num AND
                  eb.form-no NE 0:
                 FIND fg-set WHERE fg-set.company = xinv-line.company AND
                    fg-set.set-no = xinv-line.i-no  AND
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
                 /***
                 ASSIGN v-tot-pallets = v-tot-pallets +
                      ROUND((v-tot-cas  / eb.cas-pal) + .49, 0).
                 ***/
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
               /***
               ASSIGN v-tot-pallets = v-tot-pallets +
                   ROUND((v-tot-cas  / eb.cas-pal) + .49, 0).
               ***/
             END. /* do */
           end. /* est-no ne "" */
          assign
             v-t-weight = 0
             v-tot-cas = 0
             v-tot-qty = 0.
         end. /* last-of i-no */
        end. /* each xinv-line */
                                         /** Build Salesman Id String **/
        /* Added code for if no BOL(i.e. bol-no = 0) use PO from
           1st line item for invoice */
        find first oe-bolh where oe-bolh.company = inv-head.company and
                                 oe-bolh.bol-no = inv-head.bol-no
                                 USE-INDEX bol-no no-lock no-error.
        if avail oe-bolh and inv-head.bol-no <> 0 then
          assign v-rel-po-no = oe-bolh.po-no.
        else
        do:
          find first inv-line where inv-line.r-no = inv-head.r-no no-lock no-error.
          if avail inv-line then
            assign v-rel-po-no = inv-line.po-no.
        end.

        find first inv-line where inv-line.r-no = inv-head.r-no no-lock no-error.
        if avail inv-line then
        do:
          find first oe-ord where oe-ord.company eq cocode
                              and oe-ord.ord-no eq inv-line.ord-no
                                no-lock no-error.
          if avail oe-ord then
            assign v-ord-no = oe-ord.ord-no.
        end.

        view frame invhead.  /* Print headers */

        page.

        assign v-printline = 0.

        for each inv-line no-lock where inv-line.r-no = inv-head.r-n:
          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = "".

          for each oe-boll no-lock where oe-boll.company = inv-line.company
                        and oe-boll.bol-no = inv-head.bol-no
                        /*and oe-boll.b-no = inv-line.b-no*/
                        and oe-boll.i-no = inv-line.i-no use-index bol-no:

                                       /** Build Case Count Display Lines **/
            if oe-boll.cases ne 0 and oe-boll.qty-case ne 0 then
            assign v-case-line = string(oe-boll.cases) + " @ " +
                                     string(oe-boll.qty-case).
            else assign v-case-line = "".
            if oe-boll.partial ne 0 then
            assign v-part-line = "1" + " @ " + string(oe-boll.partial).
            else assign v-part-line = "".

            do i = 1 to 5:
              if (80 - length(v-case-cnt[i])) > length(v-case-line) and
                v-case-line ne "" then
              assign v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                     v-case-line = "".
              if (80 - length(v-case-cnt[i])) > length(v-part-line) and
                v-part-line ne "" then
              assign v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
                     v-part-line = "".
            end. /* 1 to 5 */
          end. /* each oe-boll */

            assign v-line = v-line + 1.

            assign v-printline = v-printline + 3.

            v-beeler-lines = 0.
            do v = 1 to 2:
              v-part-info = if v eq 1 then inv-line.part-dscr1
                            else           inv-line.part-dscr2.

              if v-part-info ne "" then v-beeler-lines = v-beeler-lines + 1.
            end.
            v-printline = v-printline + v-beeler-lines.

            if v-printline gt v-max-lines + 1 then
            do:
              v-printline = v-printline - (2 + v-beeler-lines).
              put skip(v-max-lines + 3 - v-printline)
                  "Page #" page-number "* CONTINUED *" to 80.
              v-printline = 2 + v-beeler-lines.
              page.
            end.

            assign v-inv-qty = inv-line.inv-qty
                   v-i-no = inv-line.i-no
                   v-i-name = inv-line.i-name
                   v-cust-part = inv-line.part-no
                   v-price = inv-line.price
                   v-t-price = inv-line.t-price
                   v-job = fill(" ",6 - length(trim(inv-line.job-no))) +
                           trim(inv-line.job-no) + "-" +
                           trim(string(inv-line.job-no2,"99"))
                   v-bol-no = inv-head.bol-no.

            if trim(v-job) begins "-" then assign v-job = "".

            display v-inv-qty
                    v-i-no
                    v-price
                    v-t-price
                    v-cust-part
                    v-i-name

                with frame rudd-detail.

            do v = 1 to 2:
              v-part-info = if v eq 1 then inv-line.part-dscr1
                            else           inv-line.part-dscr2.

              if v-part-info ne "" then do:
                display v-part-info skip with frame beeler.
                down with frame beeler.
              end.
            end.

            put skip(1).
        end. /* each inv-line */

        for each inv-misc no-lock where inv-misc.company = inv-head.company and
          inv-misc.r-no = inv-head.r-no and
          inv-misc.bill = "Y" break by ord-no with frame detailm:

          assign v-line      = v-line + 1
                 v-printline = v-printline + 2.

          if first(inv-misc.ord-no) then v-printline = v-printline + 2.

          if v-printline gt v-max-lines + 1 then do:
            v-printline = v-printline - if first(inv-misc.ord-no) then 4 else 2.
            put skip(v-max-lines + 3 - v-printline)
                  "Page #" page-number "* CONTINUED *" to 80.
            v-printline = if first(inv-misc.ord-no) then 4 else 2.
            page.
          end.

          if first(inv-misc.ord-no) then
            put "** Miscellaneous Items **" at 23 skip(1).

          display inv-misc.charge inv-misc.dscr inv-misc.amt.
          down.
        end. /* each inv-misc */

        if v-prntinst then do:
          do i = 1 to 4:
            if inv-head.bill-i[i] ne "" then do:
              v-printline = v-printline + 1.
              put skip(1).
              leave.
            end.
          end. /* 1 to 4 */

          do i = 1 to 4:
            if inv-head.bill-i[i] ne "" then do:
              v-printline = v-printline + 1.
              if v-printline gt v-max-lines + 1 then do:
                v-printline = v-printline - 1.
                put skip(v-max-lines + 3 - v-printline)
                    "Page #" page-number "* CONTINUED *" to 80.
                v-printline = 1.
                page.
              end.

              put inv-head.bill-i[i] at 5 skip.
            end.
          end. /* 1 to 4 */
        end.

        if v-printline gt v-max-lines then v-printline = v-max-lines.

        put skip(v-max-lines - v-printline).

        old-frt = inv-head.t-inv-freight.

        if not inv-head.f-bill then inv-head.t-inv-freight = 0.

        v-net = inv-head.t-inv-rev -
                inv-head.t-inv-tax -
                inv-head.t-inv-freight.

        find first terms where terms.company = inv-head.company and
          terms.t-code = inv-head.terms no-lock no-error.

        if avail terms then
          assign
           tmp1 = v-net * (round(terms.disc-rate / 100, 2))
           tmp2 = today + terms.disc-days.

        find first cust where cust.company = cocode and
                   cust.cust-no = inv-head.cust-no no-lock no-error.
        if avail cust and cust.sort = "Y" then do:

          assign
           net1 = v-net * (v-tx-rate[1] / 100)
           net2 = v-net * (v-tx-rate[2] / 100)
           net3 = v-net * (v-tx-rate[3] / 100)

           net1 = net1 + (inv-head.t-inv-freight * (v-tx-rate[1] / 100))
           net2 = net2 + (inv-head.t-inv-freight * (v-tx-rate[2] / 100))
           net3 = net3 + (inv-head.t-inv-freight * (v-tx-rate[3] / 100))

           net1 = round(net1,2)
           net2 = round(net2,2)
           net3 = round(net3,2).

          if inv-head.t-inv-tax ne (net1 + net2 + net3) then
            if net3 gt 0 then
              net3 = net3 + (inv-head.t-inv-tax - (net1 + net2 + net3)).
            else
            if net2 gt 0 then
              net2 = net2 + (inv-head.t-inv-tax - (net1 + net2 + net3)).
            else
              net1 = net1 + (inv-head.t-inv-tax - (net1 + net2 + net3)).
        end.

        else display " " skip(1) with frame blankl.

        clear frame totals-comp  no-pause.

        display inv-head.t-inv-freight
                inv-head.t-inv-rev
                v-salesman
                page-number with frame totals-comp.
      end. /* DO TRANSACTION */
    end. /* each xinv-head */

/* END ---------------------------------- copr. 1997 Advanced Software, Inc. */

