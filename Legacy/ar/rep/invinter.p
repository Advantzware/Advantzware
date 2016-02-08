/* ------------------------------------------------ oe/rep/color.p  05/97 FWK */
/* COLOR CARTON PRINT INVOICE - O/E MODULE                                    */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var save_id as recid.

def var time_stamp as ch.
time_stamp = string(time,"hh:mmam").

{ar/rep/invoice.i}

def var v-salesman as char format "x(9)" NO-UNDO.
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
def var v-printline as int NO-UNDO.
def var v-invhead as char format "x(13)" initial
  "I N V O I C E".
def var v-pitch like asi.printer.pitch.
def var v-len as int.
def var v-hldpitch like asi.printer.pitch.
def var v-t-weight like ar-invl.t-weight.
def var v-inv-no as int.
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
def var v-set-qty AS INT.
def var v-part-qty AS DEC FORMAT "999.9999".
def var v-net like ar-inv.net.
def var v-gross like ar-inv.gross.
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
def var old-frt like ar-inv.freight no-undo.
def var v-ord-del-hdr  as char format "x(3)" init "Del".
def var v-part-info    as char format "x(30)".
def var v-beeler-lines as int.
def var v              as int.

def var v-c-name         like company.name.
def var v-c-addr         like company.addr.
def var v-c-city         like company.city.
def var v-c-state        like company.state.
def var v-c-zip          like company.zip.
DEF VAR v-bol-no LIKE ar-invl.bol-no NO-UNDO.

def buffer xar-inv for ar-inv.
def buffer xar-invl for ar-invl.

def workfile w-sman
  field sman as char format "x(4)".

FIND FIRST xar-inv NO-LOCK NO-ERROR.

form header
    SKIP(2)
  v-invhead at 24
  v-inv-date at 70 skip
  v-c-name at 11 skip
  v-c-addr[1] at 11 skip
  v-c-addr[2] at 11
  xar-inv.inv-no at 70 skip
  v-c-city at 11
  v-c-state v-c-zip skip
  v-salesman at 70 skip(3)
  v-fob at 4
  xar-inv.terms-d at 32
  skip(2)
  xar-inv.cust-no   at 11 skip
  xar-inv.cust-name at 11 v-shipto-name    at 55 format "x(25)" skip
  xar-inv.addr[1]   at 11 v-shipto-addr[1] at 55 format "x(25)" skip
  xar-inv.addr[2]   at 11 v-shipto-addr[2] at 55 format "x(25)" skip
  v-addr3            at 11 v-sold-addr3     at 55 format "x(25)"
  skip(4)
  v-date-ship to 8
  v-shipvia at 11 format "x(25)"
  xar-inv.t-weight to 44 format ">>>>9.99"
  v-bol-no to 65 format ">>>>>9"
  skip(2)
with frame invhead page-top no-labels no-box no-underline stream-io width 80.

form header
  v-invhead at 24
  "Date:" TO 56 v-inv-date skip
  v-c-name at 11 skip
  v-c-addr[1] at 11 skip
  v-c-addr[2] at 11
  "Inv #:" TO 56 xar-inv.inv-no skip
  v-c-city at 11
  v-c-state v-c-zip skip
  "Salesman:" TO 56 v-salesman  skip(3)
  "FOB:" v-fob
  "TERMS:" xar-inv.terms-d
  skip(2)
  "SOLD TO:" AT 11        "SHIP TO:" AT 55 SKIP
  xar-inv.cust-no   at 11
  xar-inv.cust-name at 11 v-shipto-name    at 55 format "x(25)" skip
  xar-inv.addr[1]   at 11 v-shipto-addr[1] at 55 format "x(25)" skip
  xar-inv.addr[2]   at 11 v-shipto-addr[2] at 55 format "x(25)" skip
  v-addr3            at 11 v-sold-addr3     at 55 format "x(25)"
  skip(2)
  "--------------------------------------------------------------------------------" SKIP
  "Ship Date" "Shipped Via" AT 11 "Weight" TO 44 v-ord-del-hdr to 51 "#" TO 53
  "BOL #" TO 65
  v-date-ship to 8
  v-shipvia at 11 format "x(25)"
  xar-inv.t-weight to 44 format ">>>>9.99"
  v-bol-no to 65 format ">>>>>9"
  "--------------------------------------------------------------------------------" SKIP(1)
  "PO #" "Item/Description" AT 19 "QInv/QShip" TO 52 "Price" TO 64
  "Amount" TO 80 SKIP
  "----" "----------------" AT 19 "----------" TO 52 "-----" TO 64
  "------" TO 80 SKIP
with frame invhead-comp page-top no-labels no-box no-underline stream-io width 80.

form
  oe-ordl.po-no
  ar-invl.i-no at 19
  ar-invl.inv-qty to 52 format "->,>>>,>>9"
  ar-invl.unit-pr to 64 format "->>,>>9.99" space(0)
  ar-invl.pr-uom to 68
  ar-invl.amt to 80 format "->>>,>>9.99" skip
  ar-invl.po-no
  ar-invl.i-name at 19 format "x(23)"
  ar-invl.ship-qty to 52 format "->,>>>,>>9" skip
with frame ilw-detail no-labels no-box no-underline down stream-io width 80.

form
  ar-invl.po-no
  ar-invl.i-no at 19
  ar-invl.inv-qty to 52 format "->,>>>,>>9"
  ar-invl.unit-pr to 64 format "->>,>>9.99" space(0)
  ar-invl.pr-uom to 68
  ar-invl.amt to 80 format "->>>,>>9.99" skip
  ar-invl.i-name at 19 format "x(23)"
  ar-invl.ship-qty to 52 format "->,>>>,>>9" skip
with frame detail no-labels no-box no-underline down stream-io width 80.

form v-part-info at 19

    with frame beeler no-labels no-box no-underline down stream-io width 80.

form
   inv-misc.charge at  5 space(2) inv-misc.dscr format "x(30)"
    inv-misc.amt to 80 format "->>,>>9.99" skip(1)
with frame detailm no-labels no-box no-underline down stream-io width 80.

form
    disp-frt at 1
    xar-inv.freight format "->,>>9.99" at 11
    xar-inv.tax-amt format "->,>>9.99" at 21
    v-net to 43 format "->,>>>,>>9.99"
    tmp1 to 56 format "->,>>9.99"
    tmp2 at 60
    space(0)
    v-gross to 80 format "->,>>>,>>9.99"
with frame totals no-labels no-box no-underline stream-io width 80.

form
    "Freight" to 20 "Tax" to 27
    "Net Amount" TO 43 "Cash Disc" TO 56 "If Paid By" AT 59
    "Invoice Amt" TO 80
    xar-inv.freight format "->,>>9.99" at 11
    xar-inv.tax-amt format "->,>>9.99" at 21
    v-net to 43 format "->,>>>,>>9.99"
    tmp1 to 56 format "->,>>9.99"
    tmp2 at 60
    space(0)
    v-gross to 80 format "->,>>>,>>9.99"
with frame totals-comp no-labels no-box no-underline stream-io width 80.

form
    "Freight" to 20
    "Net Amount" TO 43 "Cash Disc" TO 56 "If Paid By" AT 59
    "Invoice Amt" TO 80
    xar-inv.freight format "->,>>9.99" at 11
    xar-inv.tax-amt format "->,>>9.99" at 21
    v-net to 43 format "->,>>>,>>9.99"
    tmp1 to 56 format "->,>>9.99"
    tmp2 at 60
    space(0)
    v-gross to 80 format "->,>>>,>>9.99"
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
    xar-inv.tax-amt  to 80 format "->>,>>9.99"
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
        first xar-inv where recid(xar-inv) eq report.rec-id NO-LOCK
        break by xar-inv.inv-no:

      
      assign  v-shipto-name = xar-inv.sold-name
              v-shipto-addr[1] = xar-inv.sold-addr[1]
              v-shipto-addr[2] = xar-inv.sold-addr[2]
              v-shipto-city = xar-inv.sold-city
              v-shipto-state = xar-inv.sold-state
              v-shipto-zip = xar-inv.sold-zip.
/*
      find first oe-bolh where oe-bolh.company = xar-inv.company and
          oe-bolh.bol-no = xar-inv.bol-no use-index bol-no no-lock no-error.
      if avail oe-bolh then do:
        find first oe-relh where oe-relh.company = oe-bolh.company and
                   oe-relh.r-no = oe-bolh.r-no no-lock no-error.
        if avail oe-relh then
        find first shipto where shipto.company  = oe-bolh.company and
                   shipto.cust-no = oe-relh.cust-no and
                   shipto.ship-no = oe-bolh.ship-no no-lock no-error.
        if avail shipto then
        assign  v-shipto-name = shipto.ship-name
                v-shipto-addr[1] = shipto.ship-addr[1]
                v-shipto-addr[2] = shipto.ship-addr[2]
                v-shipto-city = shipto.ship-city
                v-shipto-state = shipto.ship-state
                v-shipto-zip = shipto.ship-zip.
      end. /* avail oe-bolh */
*/
            /**
        if v-reprint and ar-inv.inv-no = 0 then next.
        else if not v-reprint and ar-inv.inv-no ne 0 then next.
      **/
        
        if xar-inv.inv-date ne ? then
         assign v-inv-date = xar-inv.inv-date.

        if xar-inv.fob-code begins "ORIG" then
         assign v-fob = "Origin".   /* DAR */
        else
         assign v-fob = "Destination".

        find carrier where carrier.company = xar-inv.company and
          carrier.carrier = xar-inv.carrier no-lock no-error.
         if avail carrier then
           assign v-shipvia = carrier.dscr.
         else
           assign v-shipvia = "".
        assign
          v-addr3 = xar-inv.city + ", " + xar-inv.state + "  " + xar-inv.zip
          v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip
          v-line = 1
          v-printline = 0.
          
        find first stax
            {sys/ref/stax1W.i}
              and {sys/ref/taxgroup.i stax} eq xar-inv.tax-code
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

        for each xar-invl no-lock where xar-invl.x-no = xar-inv.x-no
          break by xar-invl.i-no:
         do i = 1 to 3:
          if xar-invl.sman[i] ne "" then do:
            create w-sman.
            assign w-sman.sman = xar-invl.sman[i].
          end.
         end.
         assign v-tot-qty = v-tot-qty + xar-invl.ship-qty
                v-t-weight = v-t-weight + (round(xar-invl.t-weight /
                            xar-invl.qty, 2) * xar-invl.inv-qty).

         FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = xar-invl.b-no AND
             oe-bolh.ord-no = xar-invl.ord-no:
           FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = oe-bolh.company AND
              oe-boll.b-no = oe-bolh.b-no AND
              oe-boll.i-no = xar-invl.i-no:

                                      /** Bill Of Lading TOTAL CASES **/
              ASSIGN v-bol-cases = v-bol-cases + oe-boll.cases.
           END. /* each oe-boll */
           v-date-ship = oe-bolh.bol-date.
           v-bol-no = xar-invl.bol-no.
         END. /* each oe-bolh */
         if last-of(xar-invl.i-no) then do:
           if xar-invl.est-no ne "" then
           do:
             find first eb where eb.company = xar-invl.company and
               eb.est-no = xar-invl.est-no and
               /*eb.e-num = xar-invl.e-num and*/
               eb.form-no = xar-invl.form-no and
               eb.blank-no = xar-invl.blank-no no-lock no-error.

             IF xar-invl.form-no = 0 AND xar-invl.est-type = 2 THEN
             DO:
               FOR EACH fg-set NO-LOCK WHERE fg-set.company = xar-invl.company
                  AND fg-set.set-no = xar-invl.i-no:
                 ASSIGN v-set-qty = v-set-qty + fg-set.part-qty.
               END.
               IF v-set-qty = 0 THEN
                  ASSIGN v-set-qty = 1.
               FOR EACH eb NO-LOCK WHERE eb.company = xar-invl.company AND
                  eb.est-no = xar-invl.est-no AND
                  eb.e-num = xar-invl.e-num AND
                  eb.form-no NE 0:
                 FIND fg-set WHERE fg-set.company = xar-invl.company AND
                    fg-set.set-no = xar-invl.i-no  AND
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
        end. /* each xar-invl */
                                         /** Build Salesman Id String **/
        v-salesman = "".
        for each w-sman break by w-sman.sman:
          if first-of(w-sman.sman) then
            assign v-salesman = v-salesman + w-sman.sman.
          delete w-sman.
        end.

        if first(xar-inv.inv-no) then
          if v-print-head then
            view frame invhead-comp.  /* Print headers */
          else
            view frame invhead.

        page.

        for each ar-invl no-lock where ar-invl.x-no = xar-inv.x-no:
          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = "".

          for each oe-boll no-lock where oe-boll.company = ar-invl.company
                        and oe-boll.bol-no = ar-invl.bol-no
                        and oe-boll.b-no = ar-invl.b-no
                        and oe-boll.i-no = ar-invl.i-no use-index bol-no:

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

            
            do:
              display ar-invl.po-no ar-invl.i-no ar-invl.inv-qty
                      ar-invl.unit-pr ar-invl.pr-uom ar-invl.amt
                      ar-invl.i-name ar-invl.ship-qty
                      with frame detail.

              down with frame detail.
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
        end. /* each ar-invl */
        /*
        for each inv-misc of ar-inv no-lock where
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
       */

        if v-prntinst then do:
         do i = 1 to 4:
          if xar-inv.bill-i[i] ne "" then do:
            if v-printline ge 25 then
            do:
              put skip(30 - v-printline) "* CONTINUED *" at 68.
              assign v-printline = 0.
              page.
            end.

            if i eq 1 then
              put "RECEIPT NO. " at 5 xar-inv.bill-i[i] skip.
            else
              put xar-inv.bill-i[i] at 5 skip.
            assign v-printline = v-printline + 1.
          end.
         end. /* 1 to 4 */
        end.
        do:  /* T O T A L S */

          put skip(28 - v-printline).

          old-frt = xar-inv.freight.

          /*if not ar-inv.f-bill then ar-inv.freight = 0.*/

          ASSIGN
           v-gross = xar-inv.gross +
                     (IF xar-inv.ord-no EQ 0 THEN xar-inv.tax-amt ELSE 0)
           v-net   = v-gross - xar-inv.tax-amt - xar-inv.freight.

          find first terms where terms.company = xar-inv.company and
            terms.t-code = xar-inv.terms no-lock no-error.

          tmp1 = 0.
          if avail terms then
            assign
            tmp1 = v-net * (round(terms.disc-rate / 100, 2))
            tmp2 = today + terms.disc-days.

          find first cust where cust.company = cocode and
                     cust.cust-no = xar-inv.cust-no no-lock no-error.
          if avail cust and cust.sort = "Y" then do:

            assign
             net1 = v-net * (v-tx-rate[1] / 100)
             net2 = v-net * (v-tx-rate[2] / 100)
             net3 = v-net * (v-tx-rate[3] / 100)

             net1 = net1 + (xar-inv.freight * (v-tx-rate[1] / 100))
             net2 = net2 + (xar-inv.freight * (v-tx-rate[2] / 100))
             net3 = net3 + (xar-inv.freight * (v-tx-rate[3] / 100))

             net1 = round(net1,2)
             net2 = round(net2,2)
             net3 = round(net3,2).

            if xar-inv.tax-amt ne (net1 + net2 + net3) then
              if net3 gt 0 then
                net3 = net3 + (xar-inv.tax-amt - (net1 + net2 + net3)).
              else
              if net2 gt 0 then
                net2 = net2 + (xar-inv.tax-amt - (net1 + net2 + net3)).
              else
                net1 = net1 + (xar-inv.tax-amt - (net1 + net2 + net3)).

            display
                v-tax-code[1]
                net1
                v-tax-code[2]
                net2
                v-tax-code[3]
                net3
                xar-inv.tax-amt skip(1)
                with frame tax.
          end.
          else
            put skip(2).

          clear frame totals-comp  no-pause.
          clear frame totals-comp2 no-pause.
          clear frame totals       no-pause.

          if v-print-head /* Print invoice headers */ then
            if cust.sort = "N" then do:
               display xar-inv.freight
                       xar-inv.tax-amt
                       v-net
                       tmp1 when avail terms and tmp1 gt 0
                       tmp2 when avail terms and tmp1 gt 0
                       v-net @ v-gross
                       v-gross when avail terms or xar-inv.terms eq "CASH"
                         with frame totals-comp.
            end.
            else do:
               display xar-inv.freight
                    /*   ar-inv.t-inv-tax */
                       v-net
                       tmp1 when avail terms and tmp1 gt 0
                       tmp2 when avail terms and tmp1 gt 0
                       v-net @ v-gross
                       v-gross when avail terms or xar-inv.terms eq "CASH"
                         with frame totals-comp2.
            end.

          else
            if cust.sort = "N" then do:
               display disp-frt  /*  DAR */
                       xar-inv.freight
                       xar-inv.tax-amt
                       v-net
                       tmp1 when avail terms and tmp1 gt 0
                       tmp2 when avail terms and tmp1 gt 0
                       v-net @ v-gross
                       v-gross when avail terms or xar-inv.terms eq "CASH"
                          with frame totals.
            end.
            else do:
               display disp-frt  /*  DAR */
                       xar-inv.freight
                       /*ar-inv.t-inv-tax*/
                       v-net
                       tmp1 when avail terms and tmp1 gt 0
                       tmp2 when avail terms and tmp1 gt 0
                       v-net @ v-gross
                       v-gross when avail terms or xar-inv.terms eq "CASH"
                          with frame totals.
            end.

        end.
      find ar-inv where recid(ar-inv) = recid(xar-inv) no-error.
      if avail ar-inv then DO TRANSACTION:

        assign /* ar-inv.inv-date = today */
               ar-inv.printed = yes.
               /*ar-inv.stat = "X"
               ar-inv.freight = old-frt.*/
      end. /* DO TRANSACTION avail ar-inv */
    end. /* each xar-inv */

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */
