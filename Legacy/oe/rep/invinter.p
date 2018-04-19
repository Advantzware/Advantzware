/* ------------------------------------------------ oe/rep/color.p  05/97 FWK */
/* COLOR CARTON PRINT INVOICE - O/E MODULE                                    */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var save_id as recid.

def var time_stamp as ch.
time_stamp = string(time,"hh:mmam").

{oe/rep/invoice.i}

def var v-salesman as char format "x(9)".
def var v-fob as char format "x(27)".
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
def var v-pitch like asi.printer.pitch.
def var v-len as int.
def var v-hldpitch like asi.printer.pitch.
def var v-t-weight like inv-line.t-weight.
def var v-tot-cas as dec format "->>>9.9999".
def var v-tot-qty as int.
def var v-inv-date as date initial TODAY.
def new shared var v-fr-tax as logical initial no.
def var v-tax-rate as dec format "->>>.99".
def var v-tax-code like stax.tax-code.
def var v-tx-rate like stax.tax-rate.
def var v-ans as logical initial no.
def var v-date-ship as date initial today.
def var v-bol-cases LIKE oe-boll.cases.
def var v-set-qty AS DECIMAL.
def var v-part-qty AS DEC FORMAT "999.9999".
def var v-net like inv-head.t-inv-rev.
def var v-case-cnt as char format "x(70)" extent 5.
def var v-case-line as char.
def var v-part-line as char.
def var tmp1 as dec.
def var tmp2 as date.
def var net1 as dec.
def var net2 as dec.
def var net3 as dec.
def var cnt as int.  /* DAR */
def var disp-frt as char init "Freight:". /* DAR */
def var old-frt like inv-head.t-inv-freight no-undo.
def var v-ord-del-hdr  as char format "x(3)" init "Del".
def var v-part-info    as char format "x(30)".
def var v-beeler-lines as int.
def var v              as int.

def var v-c-name         like company.name.
def var v-c-addr         like company.addr.
def var v-c-city         like company.city.
def var v-c-state        like company.state.
def var v-c-zip          like company.zip.

def buffer xinv-head for inv-head.
def buffer xinv-line for inv-line.

def workfile w-sman
  field sman as char format "x(4)".

FIND FIRST inv-head NO-LOCK NO-ERROR.

form header
    SKIP(2)
  v-invhead at 24
  v-inv-date at 70 skip
  v-c-name at 11 skip
  v-c-addr[1] at 11 skip
  v-c-addr[2] at 11
  inv-head.inv-no at 70 skip
  v-c-city at 11
  v-c-state v-c-zip skip
  v-salesman at 70 skip(3)
  v-fob at 4
  inv-head.terms-d at 32
  skip(2)
  inv-head.cust-no   at 11 skip
  inv-head.cust-name at 11 v-shipto-name    at 55 format "x(25)" skip
  inv-head.addr[1]   at 11 v-shipto-addr[1] at 55 format "x(25)" skip
  inv-head.addr[2]   at 11 v-shipto-addr[2] at 55 format "x(25)" skip
  v-addr3            at 11 v-sold-addr3     at 55 format "x(25)"
  skip(4)
  v-date-ship to 8
  v-shipvia at 11 format "x(25)"
  inv-head.t-inv-weight to 44 format ">>>>9.99"
  inv-head.bol-no to 65 format ">>>>>9"
  skip(2)
with frame invhead page-top no-labels no-box no-underline stream-io width 80.

form header
  v-invhead at 24
  "Date:" TO 56 v-inv-date skip
  v-c-name at 11 skip
  v-c-addr[1] at 11 skip
  v-c-addr[2] at 11
  "Inv #:" TO 56 inv-head.inv-no skip
  v-c-city at 11
  v-c-state v-c-zip skip
  "Salesman:" TO 56 v-salesman  skip(3)
  "FOB:" v-fob
  "TERMS:" inv-head.terms-d
  skip(2)
  "SOLD TO:" AT 11        "SHIP TO:" AT 55 SKIP
  inv-head.cust-no   at 11
  inv-head.cust-name at 11 v-shipto-name    at 55 format "x(25)" skip
  inv-head.addr[1]   at 11 v-shipto-addr[1] at 55 format "x(25)" skip
  inv-head.addr[2]   at 11 v-shipto-addr[2] at 55 format "x(25)" skip
  v-addr3            at 11 v-sold-addr3     at 55 format "x(25)"
  skip(2)
  "--------------------------------------------------------------------------------" SKIP
  "Ship Date" "Shipped Via" AT 11 "Weight" TO 44 v-ord-del-hdr to 51 "#" TO 53
  "BOL #" TO 65
  v-date-ship to 8
  v-shipvia at 11 format "x(25)"
  inv-head.t-inv-weight to 44 format ">>>>9.99"
  inv-head.bol-no to 65 format ">>>>>9"
  "--------------------------------------------------------------------------------" SKIP(1)
  "PO #" "Item/Description" AT 19 "QInv/QShip" TO 52 "Price" TO 64
  "Amount" TO 80 SKIP
  "----" "----------------" AT 19 "----------" TO 52 "-----" TO 64
  "------" TO 80 SKIP
with frame invhead-comp page-top no-labels no-box no-underline stream-io width 80.

form
  oe-ordl.po-no
  inv-line.i-no at 19
  inv-line.inv-qty to 52 format "->,>>>,>>9"
  inv-line.price to 64 format "->>,>>9.99" space(0)
  inv-line.pr-uom to 68
  inv-line.t-price to 80 format "->>>,>>9.99" skip
  inv-line.po-no
  inv-line.i-name at 19 format "x(23)"
  inv-line.ship-qty to 52 format "->,>>>,>>9" skip
with frame ilw-detail no-labels no-box no-underline down stream-io width 80.

form
  inv-line.po-no
  inv-line.i-no at 19
  inv-line.inv-qty to 52 format "->,>>>,>>9"
  inv-line.price to 64 format "->>,>>9.99" space(0)
  inv-line.pr-uom to 68
  inv-line.t-price to 80 format "->>>,>>9.99" skip
  inv-line.i-name at 19 format "x(23)"
  inv-line.ship-qty to 52 format "->,>>>,>>9" skip
with frame detail no-labels no-box no-underline down stream-io width 80.

form v-part-info at 19

    with frame beeler no-labels no-box no-underline down stream-io width 80.

form
   inv-misc.charge at  5 space(2) inv-misc.dscr format "x(30)"
    inv-misc.amt to 80 format "->>,>>9.99" skip(1)
with frame detailm no-labels no-box no-underline down stream-io width 80.

form
    disp-frt at 1
    inv-head.t-inv-freight format "->,>>9.99" at 11
    inv-head.t-inv-tax format "->,>>9.99" at 21
    v-net to 43 format "->,>>>,>>9.99"
    tmp1 to 56 format "->,>>9.99"
    tmp2 at 60
    space(0)
    inv-head.t-inv-rev to 80 format "->,>>>,>>9.99"
with frame totals no-labels no-box no-underline stream-io width 80.

form
    "Freight" to 20 "Tax" to 27
    "Net Amount" TO 43 "Cash Disc" TO 56 "If Paid By" AT 59
    "Invoice Amt" TO 80
    inv-head.t-inv-freight format "->,>>9.99" at 11
    inv-head.t-inv-tax format "->,>>9.99" at 21
    v-net to 43 format "->,>>>,>>9.99"
    tmp1 to 56 format "->,>>9.99"
    tmp2 at 60
    space(0)
    inv-head.t-inv-rev to 80 format "->,>>>,>>9.99"
with frame totals-comp no-labels no-box no-underline stream-io width 80.

form
    "Freight" to 20
    "Net Amount" TO 43 "Cash Disc" TO 56 "If Paid By" AT 59
    "Invoice Amt" TO 80
    inv-head.t-inv-freight format "->,>>9.99" at 11
    inv-head.t-inv-tax format "->,>>9.99" at 21
    v-net to 43 format "->,>>>,>>9.99"
    tmp1 to 56 format "->,>>9.99"
    tmp2 at 60
    space(0)
    inv-head.t-inv-rev to 80 format "->,>>>,>>9.99"
with frame totals-comp2 no-labels no-box no-underline stream-io width 80.

form
    "Taxes- " at 1
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
with frame tax no-labels no-box no-underline stream-io width 80.

form
    " " to 79
with frame blankl no-labels no-box no-underline stream-io width 80.

  
    find first company where company.company = cocode no-lock no-error.

    find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.
    v-fr-tax = oe-ctrl.f-tax.

    if oe-ctrl.prcom then
      assign
       v-c-name    = company.name
       v-c-addr[1] = company.addr[1]
       v-c-addr[2] = company.addr[2]
       v-c-city    = company.city
       v-c-state   = company.state
       v-c-zip     = company.zip.
    else
      assign
       v-invhead   = ""
       v-c-name    = ""
       v-c-addr[1] = ""
       v-c-addr[2] = ""
       v-c-city    = ""
       v-c-state   = ""
       v-c-zip     = "".

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
         assign v-fob = "Origin".   /* DAR */
        else
         assign v-fob = "Destination".

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

        for each xinv-line no-lock where xinv-line.r-no = inv-head.r-no
          break by xinv-line.i-no:
         do i = 1 to 3:
          if xinv-line.sman[i] ne "" then do:
            create w-sman.
            assign w-sman.sman = xinv-line.sman[i].
          end.
         end.
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
           v-date-ship = oe-bolh.bol-date.
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
                 ASSIGN v-set-qty = v-set-qty + fg-set.QtyPerSet.
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

                 IF AVAIL fg-set AND fg-set.QtyPerSet NE 0 THEN
                   ASSIGN v-part-qty = fg-set.QtyPerSet / v-set-qty.
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
        v-salesman = "".
        for each w-sman break by w-sman.sman:
          if first-of(w-sman.sman) then
            assign v-salesman = v-salesman + w-sman.sman.
          delete w-sman.
        end.

        if first(report.key-01) then
          if v-print-head then
            view frame invhead-comp.  /* Print headers */
          else
            view frame invhead.

        page.

        for each inv-line no-lock where inv-line.r-no = inv-head.r-no:
          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = "".

          for each oe-boll no-lock where oe-boll.company = inv-line.company
                        and oe-boll.bol-no = inv-head.bol-no
                        and oe-boll.b-no = inv-line.b-no
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

            assign v-line = v-line + 1
                   v-printline = v-printline + 2.

            v-beeler-lines = 0.
            if sys-ctrl.char-fld eq "Beeler" or
               sys-ctrl.char-fld eq "ILWalker" then do v = 1 to 3:
              v-part-info = if      v eq 1 then inv-line.part-no
                            else if v eq 2 then inv-line.part-dscr1
                            else                inv-line.part-dscr2.

              if v-part-info ne "" then v-beeler-lines = v-beeler-lines + 1.
            end.

            do i = 1 to 5:
              if v-case-cnt[i] ne "" then
                assign v-line = v-line + 1
                       v-beeler-lines = v-beeler-lines + 1.
            end.
/*
            v-printline = v-printline + v-beeler-lines.
*/

            if v-printline ge 25 then
            do:
              put skip(30 - v-printline) "* CONTINUED *" at 68.
              v-printline = 2 + v-beeler-lines.
              page.
            end.

            if sys-ctrl.char-fld eq "ILWalker" then
            do:
              find first oe-ordl where oe-ordl.company = inv-line.company and
                                       oe-ordl.ord-no = inv-line.ord-no and
                                       oe-ordl.line = inv-line.line and
                                       oe-ordl.i-no = inv-line.i-no
                                       no-lock no-error.

              display oe-ordl.po-no when avail oe-ordl
                      inv-line.i-no inv-line.inv-qty
                      inv-line.price inv-line.pr-uom inv-line.t-price
                      inv-line.po-no inv-line.i-name inv-line.ship-qty
                      with frame ilw-detail.

              down with frame ilw-detail.
            end.
            else
            do:
              display inv-line.po-no inv-line.i-no inv-line.inv-qty
                      inv-line.price inv-line.pr-uom inv-line.t-price
                      inv-line.i-name inv-line.ship-qty
                      with frame detail.

              down with frame detail.
            end.

            if sys-ctrl.char-fld eq "Beeler" or
              sys-ctrl.char-fld eq "ILWalker" then do v = 1 to 3:
              v-part-info = if      v eq 1 then inv-line.part-no
                            else if v eq 2 then inv-line.part-dscr1
                            else                inv-line.part-dscr2.

              if v-part-info ne "" then do:
                display v-part-info skip with frame beeler.
                down with frame beeler.
              end.
            end.

                                          /** Display Case Count Lines **/
          /* task 05150606
           do i = 1 to 5:
              if v-case-cnt[i] ne "" then
              do:
                put v-case-cnt[i] at 5.
                assign v-printline = v-printline + 1.
              end.
            end. /* 1 to 5 */
           */
            put skip(1).
            assign v-printline = v-printline + 1.
        end. /* each inv-line */

        for each inv-misc of inv-head no-lock where
          inv-misc.bill = "Y" break by ord-no:
          if first(inv-misc.ord-no) then
          do:
            put "** Miscellaneous Items **" at 23 skip(1).
            assign v-printline = v-printline + 2.
          end.
            if v-printline ge 25 then
            do:
              put skip(30 - v-printline) "* CONTINUED *" at 68.
              assign v-printline = 0.
              page.
            end.
            else
             down with frame detailm.
            display
                 inv-misc.charge inv-misc.dscr inv-misc.amt
            with frame detailm.

            assign v-line = v-line + 1
               v-printline = v-printline + 2.
        end. /* each inv-misc */

        if v-prntinst then do:
         do i = 1 to 4:
          if inv-head.bill-i[i] ne "" then do:
            if v-printline ge 25 then
            do:
              put skip(30 - v-printline) "* CONTINUED *" at 68.
              assign v-printline = 0.
              page.
            end.

            if i eq 1 then
              put "RECEIPT NO. " at 5 inv-head.bill-i[i] skip.
            else
              put inv-head.bill-i[i] at 5 skip.
            assign v-printline = v-printline + 1.
          end.
         end. /* 1 to 4 */
        end.
        do:  /* T O T A L S */

          put skip(28 - v-printline).

          old-frt = inv-head.t-inv-freight.

          if not inv-head.f-bill then inv-head.t-inv-freight = 0.

          v-net = inv-head.t-inv-rev -
                  inv-head.t-inv-tax -
                  inv-head.t-inv-freight.

          find first terms where terms.company = inv-head.company and
            terms.t-code = inv-head.terms no-lock no-error.

          tmp1 = 0.
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

            display
                v-tax-code[1]
                net1
                v-tax-code[2]
                net2
                v-tax-code[3]
                net3
                inv-head.t-inv-tax skip(1)
                with frame tax.
          end.
          else
            put skip(2).

          clear frame totals-comp  no-pause.
          clear frame totals-comp2 no-pause.
          clear frame totals       no-pause.

          if v-print-head /* Print invoice headers */ then
            if cust.sort = "N" then do:
               display inv-head.t-inv-freight
                       inv-head.t-inv-tax
                       v-net
                       tmp1 when avail terms and tmp1 gt 0
                       tmp2 when avail terms and tmp1 gt 0
                       v-net @ inv-head.t-inv-rev
                       inv-head.t-inv-rev when avail terms or
                                               inv-head.terms = "CASH"
                         with frame totals-comp.
            end.
            else do:
               display inv-head.t-inv-freight
                    /*   inv-head.t-inv-tax */
                       v-net
                       tmp1 when avail terms and tmp1 gt 0
                       tmp2 when avail terms and tmp1 gt 0
                       v-net @ inv-head.t-inv-rev
                       inv-head.t-inv-rev when avail terms or
                                               inv-head.terms = "CASH"
                         with frame totals-comp2.
            end.

          else
            if cust.sort = "N" then do:
               display disp-frt  /*  DAR */
                       inv-head.t-inv-freight
                       inv-head.t-inv-tax
                       v-net
                       tmp1 when avail terms and tmp1 gt 0
                       tmp2 when avail terms and tmp1 gt 0
                       v-net @ inv-head.t-inv-rev
                       inv-head.t-inv-rev when avail terms or
                                               inv-head.terms = "CASH"
                          with frame totals.
            end.
            else do:
               display disp-frt  /*  DAR */
                       inv-head.t-inv-freight
                       /*inv-head.t-inv-tax*/
                       v-net
                       tmp1 when avail terms and tmp1 gt 0
                       tmp2 when avail terms and tmp1 gt 0
                       v-net @ inv-head.t-inv-rev
                       inv-head.t-inv-rev when avail terms or
                                               inv-head.terms = "CASH"
                          with frame totals.
            end.

        end.
        inv-head.t-inv-freight = old-frt.
      end. /* DO TRANSACTION  */
    end. /* each xinv-head */

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */
