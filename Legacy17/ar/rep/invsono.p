/* ---------------------------------------------- oe/rep/invsono.p  04/99 FWK */
/* PRINT INVOICE - SONOCO FORMAT                                              */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def stream mess.

def var save_id as recid.

def var time_stamp as ch.
time_stamp = string(time, "hh:mmam").

{ar/rep/invoice.i}

def var v-salesman as char format "x(14)".
def var v-fob as char format "x(27)".
def var v-shipvia like carrier.dscr.
def var v-addr3 as char format "x(30)".
def var v-addr  as char format "x(30)" extent 2 no-undo.
def var v-sold-addr3 as char format "x(30)".
def var v-shipto-no like ar-inv.sold-id.
def var v-shipto-name as char format "x(30)".
def var v-shipto-addr as char format "x(30)" extent 2.
def var v-shipto-city as char format "x(15)".
def var v-shipto-state as char format "x(2)".
def var v-shipto-zip as char format "x(10)".
def var v-line as int.
def var v-printline as int.
def var v-invhead as char format "x(13)" init
  "I N V O I C E".
def var v-pitch like asi.printer.pitch.
def var v-len as int.
def var v-hldpitch like asi.printer.pitch.
def var v-t-weight like ar-invl.t-weight.
def var v-inv-no as int.
def var v-tot-cas as dec format "->>>9.9999".
def var v-tot-pallets as int.
def var v-tot-qty as int.
def var v-inv-date as date init today.
def new shared var v-fr-tax as log init no.
def var v-tax-rate as dec format "->>>.99".
def var v-tax-code like stax.tax-code.
def var v-tx-rate like stax.tax-rate.
def var v-ans as log init no.
def var v-date-ship as date init today.
def var v-del-no as int format ">>>>>>".
def var v-bol-cases like oe-boll.cases.
def var v-set-qty as int.
def var v-part-qty as dec format "999.9999".
def var v-net like ar-inv.due.
def var v-case-cnt as char format "x(80)" extent 5.
def var v-case-line as char.
def var v-part-line as char.
def var tmp1 as dec.
def var tmp2 as date.
def var net1 as dec.
def var net2 as dec.
def var net3 as dec.
def new shared var v-num-copies as int init 1 format ">9".
def var cnt as int.
def var disp-frt as char init "Freight:".
def var old-frt like ar-inv.freight no-undo.
def var v-ord-del-hdr  as char format "x(4)" init "Del#".
def var v-part-info    as char format "x(30)".
def var v-beeler-lines as int.
def var v              as int.
def var v-ord#         as int format ">>>>>>" no-undo.
def var v-qty          like ar-invl.qty no-undo.

def var v-cust-name      like ar-inv.cust-name.
def var v-c-name         like company.name.
def var v-c-addr         like company.addr.
def var v-c-city         like company.city.
def var v-c-state        like company.state.
def var v-c-zip          like company.zip.
def var v-bol-no like oe-bolh.bol-no no-undo.

def buffer xar-inv for ar-inv.
def buffer xar-invl for ar-invl.

DEF TEMP-TABLE tt-report LIKE report.

def workfile w-sman field sman as char format "x(4)".

FIND FIRST ar-inv NO-LOCK NO-ERROR.

form header
  v-invhead at 24
  v-inv-date at 72 FORMAT "99/99/99" skip
  v-c-name at 11 skip
  v-c-addr[1] at 11 skip
  v-c-addr[2] at 11
  "A720" + trim(string(ar-inv.inv-no,">>>>>>")) at 69 format "x(10)" skip
  v-c-city at 11
  v-c-state v-c-zip skip
  v-salesman at 69 skip(3)
  v-fob at 4
  ar-inv.terms-d at 32
  skip(3)
  ar-inv.cust-no   at 11 v-shipto-no      at 51 skip
  v-cust-name      at 11 v-shipto-name    at 51 SKIP
  v-addr[1]        at 11 v-shipto-addr[1] at 51 skip
  v-addr[2]        at 11 v-shipto-addr[2] at 51 skip
  v-addr3          at 11 v-sold-addr3     at 51
  skip(4)
  v-date-ship FORMAT "99/99/99" 
  v-shipvia at 11 format "x(25)"
  ar-inv.t-weight to 44 format ">>>>9.99"
  v-del-no to 53
  v-bol-no to 65 format ">>>>>9"
  v-tot-pallets at 69 skip(2)
with frame invhead page-top no-labels no-box no-underline stream-io width 90.

form header
  skip(2)
  "A720" + trim(string(ar-inv.inv-no,">>>>>>")) to 80 format "x(10)"
  v-inv-date to 80 FORMAT "99/99/99" skip
  "Salesman:" TO 63 v-salesman at 65 skip(3)
  "FOB:" v-fob
  "TERMS:" ar-inv.terms-d
  skip(2)
  "SOLD TO:" AT 11        "SHIP TO:" AT 51 skip
  ar-inv.cust-no   at 11
  v-cust-name      at 11 v-shipto-name    at 51 SKIP
  v-addr[1]        at 11 v-shipto-addr[1] at 51 skip
  v-addr[2]        at 11 v-shipto-addr[2] at 51 skip
  v-addr3          at 11 v-sold-addr3     at 51
  skip(2)
  "--------------------------------------------------------------------------------" skip
  "Ship Date" "Shipped Via" AT 11 "Weight" TO 44
  "BOL #" TO 65 "Pallets" AT 72 skip v-date-ship FORMAT "99/99/99" 
  v-shipvia at 11 format "x(25)"
  ar-inv.t-weight to 44 format ">>>>9.99"
  v-bol-no to 65 format ">>>>>9"
  v-tot-pallets at 69 skip
  "--------------------------------------------------------------------------------" skip(1)
  "PO #/Order #" "Item/Description" AT 19 "QInv/QShip" TO 52 "Price" TO 64
  "Amount" TO 80 skip
  "------------" "----------------" AT 19 "----------" TO 52 "-----" TO 64
  "------" TO 80 skip
with frame invhead-comp page-top no-labels no-box no-underline stream-io width 90.

form header
  v-invhead at 24
  "Date:" TO 56 v-inv-date FORMAT "99/99/99" skip
  v-c-name at 11 skip
  v-c-addr[1] at 11 skip
  v-c-addr[2] at 11
  "Inv #:" TO 56
  "A720" + trim(string(ar-inv.inv-no,">>>>>>")) format "x(10)"
  v-c-city at 11
  v-c-state v-c-zip skip
  "Salesman:" TO 56 v-salesman  skip(3)
  "FOB:" v-fob
  "TERMS:" ar-inv.terms-d
  skip(2)
  "SOLD TO:" AT 11        "SHIP TO:" AT 51 skip
  ar-inv.cust-no   at 11
  ar-inv.cust-name at 11 v-shipto-name    at 51 skip
  ar-inv.addr[1]   at 11 v-shipto-addr[1] at 51 skip
  ar-inv.addr[2]   at 11 v-shipto-addr[2] at 51 skip
  v-addr3          at 11 v-sold-addr3     at 51
  skip(2)
  "--------------------------------------------------------------------------------" skip
  "Ship Date" "Shipped Via" AT 11 "Weight" TO 44 v-ord-del-hdr to 51
  "BOL #" TO 65 "Pallets" AT 72 skip
  v-date-ship
  v-shipvia at 11 format "x(25)"
  ar-inv.t-weight to 44 format ">>>>9.99"
  v-del-no to 53
  v-bol-no to 65 format ">>>>>9"
  v-tot-pallets at 69 skip
  "--------------------------------------------------------------------------------" skip(1)
  "Customer PO#" "Item" AT 24 "Qty Invoiced" TO 53 skip
  "  Buyer PO #" "Description" AT 21 "Qty Shipped" TO 53 "Price" TO 64
  "Amount" TO 80 skip
  "------------" "----------------" AT 19 "------------" TO 53 "-----" TO 64
  "------" TO 80 skip
with frame ilwalker-comp page-top no-labels no-box no-underline stream-io width 90.

form
  oe-ordl.po-no
  ar-invl.i-no at 19
/*
  ar-invl.inv-qty to 52 format "->,>>>,>>9"
*/
  v-qty to 52 format "->,>>>,>>9"
  ar-invl.unit-pr to 64 format "->>,>>9.99" space(0)
  ar-invl.pr-uom to 68
  ar-invl.amt to 80 format "->>>,>>9.99" skip
  ar-invl.po-no
  ar-invl.i-name at 19 format "x(23)"
  ar-invl.ship-qty to 52 format "->,>>>,>>9" skip
with frame ilw-detail no-labels no-box no-underline down stream-io width 90.

form
  ar-invl.po-no
  ar-invl.i-no at 19
  v-qty to 52 format "->,>>>,>>9"
/*
  ar-invl.inv-qty to 52 format "->,>>>,>>9"
*/
  ar-invl.unit-pr to 64 format "->>,>>9.99" space(0)
  ar-invl.pr-uom to 68
  ar-invl.amt to 80 format "->>>,>>9.99" skip
  ar-invl.ord-no at 1
  ar-invl.i-name at 19 format "x(23)"
  ar-invl.ship-qty to 52 format "->,>>>,>>9" skip
with frame detail no-labels no-box no-underline down stream-io width 90.

form v-ord# at 1 v-part-info at 19
    with frame beeler no-labels no-box no-underline down stream-io width 90.

form inv-misc.charge at  5 space(2) inv-misc.dscr format "x(30)"
     inv-misc.amt to 80 format "->>,>>9.99" skip(1)
    with frame detailm no-labels no-box no-underline down stream-io width 90.

form disp-frt at 1
    ar-inv.freight format "->,>>9.99" at 11
    ar-inv.tax-amt format "->,>>9.99" at 21
    v-net to 43 format "->,>>>,>>9.99"
    tmp1 to 56 format "->,>>9.99"
    tmp2 at 60
    space(0)
    ar-inv.due to 80 format "->,>>>,>>9.99"
    with frame totals no-labels no-box no-underline stream-io width 90.

form "Freight" to 20 "Tax" to 27
    "Net Amount" TO 43 "Cash Disc" TO 56 "If Paid By" AT 59
    "Invoice Amt" TO 80
    ar-inv.freight format "->,>>9.99" at 11
    ar-inv.tax-amt format "->,>>9.99" at 21
    v-net to 43 format "->,>>>,>>9.99"
    tmp1 to 56 format "->,>>9.99"
    tmp2 at 60
    space(0)
    ar-inv.due to 80 format "->,>>>,>>9.99"
    with frame totals-comp no-labels no-box no-underline stream-io width 90.

form "Freight" to 20
    "Net Amount" TO 43 "Cash Disc" TO 56 "If Paid By" AT 59
    "Invoice Amt" TO 80
    ar-inv.freight format "->,>>9.99" at 11
    ar-inv.tax-amt format "->,>>9.99" at 21
    v-net to 43 format "->,>>>,>>9.99"
    tmp1 to 56 format "->,>>9.99"
    tmp2 at 60
    space(0)
    ar-inv.due to 80 format "->,>>>,>>9.99"
    with frame totals-comp2 no-labels no-box no-underline stream-io width 90.

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
   ar-inv.tax-amt  to 80 format "->>,>>9.99"
   with frame tax no-labels no-box no-underline stream-io width 90.

form " " to 80
     with frame blankl no-labels no-box no-underline stream-io width 90.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
v-fr-tax = oe-ctrl.f-tax.

  assign
   v-invhead   = ""
   v-c-name    = ""
   v-c-addr[1] = ""
   v-c-addr[2] = ""
   v-c-city    = ""
   v-c-state   = ""
   v-c-zip     = "".

find first sys-ctrl where
  sys-ctrl.company eq cocode and
  sys-ctrl.name    eq "INVCOPYS"
  no-lock no-error.
if not avail sys-ctrl then
do transaction:
  create sys-ctrl.
  assign
    sys-ctrl.company = cocode
    sys-ctrl.name    = "INVCOPYS"
    sys-ctrl.descrip = "The number of invoice copies to be printed".
  message "System control record NOT found.  Please enter the number of copies"
  update sys-ctrl.int-fld.
end.
v-num-copies = sys-ctrl.int-fld.

    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

        FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id NO-LOCK:

      find first ar-invl where ar-invl.company = ar-inv.company
        and ar-invl.inv-no = ar-inv.inv-no no-lock no-error.
      v-bol-no = if available ar-invl then ar-invl.bol-no else 0.

      create tt-report.
      assign
       tt-report.term-id = v-term-id
       tt-report.key-01  = ar-inv.cust-no
       tt-report.key-02  = string(v-bol-no,"9999999999")
       tt-report.rec-id  = recid(ar-inv).
    end.

    for each tt-report where tt-report.term-id eq v-term-id no-lock,
        first xar-inv where recid(xar-inv) eq tt-report.rec-id no-lock
        break by tt-report.key-01
              by tt-report.key-02:

      assign
        v-shipto-no      = xar-inv.sold-id
        v-shipto-name    = xar-inv.sold-name
        v-shipto-addr[1] = xar-inv.sold-addr[1]
        v-shipto-addr[2] = xar-inv.sold-addr[2]
        v-shipto-city    = xar-inv.sold-city
        v-shipto-state   = xar-inv.sold-state
        v-shipto-zip     = xar-inv.sold-zip
        v-cust-name      = xar-inv.cust-name.

      if v-shipto-no eq "" then
      do:
        find first shipto where shipto.company eq cocode
                          and shipto.ship-id eq xar-inv.ship-id
                        no-lock no-error.
        if avail shipto then
          assign
            v-shipto-no      = xar-inv.ship-id
            v-shipto-name    = shipto.ship-name
            v-shipto-addr[1] = shipto.ship-addr[1]
            v-shipto-addr[2] = shipto.ship-addr[2]
            v-shipto-city    = shipto.ship-city
            v-shipto-state   = shipto.ship-state
            v-shipto-zip     = shipto.ship-zip.
      end.

      do cnt = 1 to v-num-copies:

      v-del-no = 0.

      find first oe-bolh where oe-bolh.company = xar-inv.company and
          oe-bolh.bol-no = v-bol-no use-index bol-no no-lock no-error.
      if avail oe-bolh then do:
        find first oe-relh where oe-relh.company eq oe-bolh.company and
                   oe-relh.r-no eq oe-bolh.r-no no-lock no-error.
        if avail oe-relh then
        find first shipto where shipto.company  eq oe-bolh.company and
                   shipto.cust-no eq oe-bolh.cust-no and
                   shipto.ship-id eq oe-bolh.ship-id no-lock no-error.

        if available shipto then
        assign
          v-shipto-no      = shipto.ship-id
          v-shipto-name    = shipto.ship-name
          v-shipto-addr[1] = shipto.ship-addr[1]
          v-shipto-addr[2] = shipto.ship-addr[2]
          v-shipto-city    = shipto.ship-city
          v-shipto-state   = shipto.ship-state
          v-shipto-zip     = shipto.ship-zip.

      end. /* avail oe-bolh */

      find ar-inv where recid(ar-inv) eq recid(xar-inv) no-error.
      if avail ar-inv then
      do transaction:
        /* if not v-reprint then ar-inv.inv-no = v-inv-no. */
        if ar-inv.inv-date <> ? then v-inv-date = ar-inv.inv-date.

        v-fob = if ar-inv.fob-code begins "ORIG" then "Origin"
          else "Destination".

        find carrier where carrier.company = ar-inv.company and
          carrier.carrier = ar-inv.carrier no-lock no-error.

        v-shipvia = if avail carrier then carrier.dscr else "".

        if v-cust-name eq "" then
        do:
          find first cust where cust.company eq cocode
                            and cust.cust-no eq ar-inv.cust-no
                          no-lock no-error.
          if avail cust then
            v-cust-name = cust.name.
        end.

        if ar-inv.addr[1] eq "" then
        do:
          find first cust where cust.company eq cocode
                            and cust.cust-no eq ar-inv.cust-no
                          no-lock no-error.
          if avail cust then
            assign v-addr[1] = cust.addr[1]
                   v-addr[2] = cust.addr[2].
        end.
        else
          assign v-addr[1] = ar-inv.addr[1]
                 v-addr[2] = ar-inv.addr[2].


        if ar-inv.city eq "" and ar-inv.state eq "" and
           ar-inv.zip eq "" then
        do:
          find first cust where cust.company eq cocode
                            and cust.cust-no eq ar-inv.cust-no
                          no-lock no-error.
          if avail cust then
            v-addr3 = cust.city + ", " + cust.state + "  " + cust.zip.
        end.
        else
          v-addr3 = ar-inv.city + ", " + ar-inv.state + "  " + ar-inv.zip.

        ASSIGN
          v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip
          v-line = 1
          v-printline = 0.
          
        find first stax
            WHERE stax.company   EQ cocode
              and stax.tax-group eq ar-inv.tax-code
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

        v-tot-pallets = 0.
        for each xar-invl no-lock where xar-invl.x-no = ar-inv.x-no
           break by xar-invl.i-no:
           do i = 1 to 3:
             if xar-invl.sman[i] ne "" then
             do:
               create w-sman.
               w-sman.sman = xar-invl.sman[i].
             end.
           end.
           assign
             v-tot-qty = v-tot-qty + xar-invl.ship-qty
             v-t-weight = v-t-weight + (round(xar-invl.t-weight /
               xar-invl.qty, 2) * xar-invl.inv-qty).

           for each oe-bolh no-lock where oe-bolh.b-no eq xar-invl.b-no and
             oe-bolh.ord-no eq xar-invl.ord-no:
           for each oe-boll no-lock where oe-boll.company eq oe-bolh.company
               and oe-boll.b-no eq oe-bolh.b-no
               and oe-boll.i-no eq xar-invl.i-no:

                                      /** Bill Of Lading TOTAL CASES **/
              assign v-bol-cases = v-bol-cases + oe-boll.cases.
            end. /* each oe-boll */
            assign
              v-date-ship = oe-bolh.bol-date
              v-tot-pallets = v-tot-pallets + oe-bolh.tot-pallets.
         end. /* each oe-bolh */
         if last-of(xar-invl.i-no) then do:
           if xar-invl.est-no ne "" then do:
             find first eb where eb.company eq xar-invl.company and
               eb.est-no eq xar-invl.est-no and
               eb.e-num eq xar-invl.e-num and
               eb.form-no eq xar-invl.form-no and
               eb.blank-no eq xar-invl.blank-no no-lock no-error.

             if xar-invl.form-no eq 0                              and
                (xar-invl.est-type eq 2 or xar-invl.est-type eq 6) then do:
               for each fg-set no-lock where fg-set.company eq xar-invl.company
                  and fg-set.set-no eq xar-invl.i-no:
                 assign v-set-qty = v-set-qty + fg-set.part-qty.
               end.
               if v-set-qty = 0 then
                  assign v-set-qty = 1.
               for each eb no-lock where eb.company eq xar-invl.company and
                  eb.est-no eq xar-invl.est-no and
                  eb.e-num eq xar-invl.e-num and
                  eb.form-no ne 0:
                 find fg-set where fg-set.company eq xar-invl.company and
                    fg-set.set-no eq xar-invl.i-no  and
                    fg-set.part-no eq eb.stock-no no-lock no-error.

                 if avail fg-set and fg-set.part-qty ne 0 then
                   assign v-part-qty = fg-set.part-qty / v-set-qty.
                 else
                   assign v-part-qty = 1 / v-set-qty.


                if eb.cas-cnt eq 0 then
                   assign v-tot-cas = round((v-t-weight * v-part-qty) /
                                      eb.cas-wt, 2).
                 else
                   assign v-tot-cas = round((v-tot-qty * v-part-qty) /
                                      eb.cas-cnt, 2).
                 if v-bol-cases ne 0 then v-tot-cas = v-bol-cases.
               end. /* each eb */
             end. /* do */
             else
             if avail eb then
             do:
               if eb.cas-cnt eq 0 then
                 assign v-tot-cas = round(v-t-weight / eb.cas-wt, 2).
               else
                 assign v-tot-cas = round(v-tot-qty / eb.cas-cnt, 2).
               if v-bol-cases ne 0 then v-tot-cas = v-bol-cases.
             end. /* do */
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

        if first(tt-report.key-01) then
          if v-print-head then
            view frame invhead-comp.  /* Print headers */
          else
            view frame invhead.

        page.

        for each ar-invl no-lock where ar-invl.x-no eq ar-inv.x-no:
          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = "".

          for each oe-boll no-lock where oe-boll.company eq ar-invl.company
                        and oe-boll.bol-no eq ar-invl.bol-no
                        and oe-boll.i-no eq ar-invl.i-no
                        and oe-boll.ord-no eq ar-invl.ord-no use-index bol-no:


                                       /** Build Case Count Display Lines **/
            if oe-boll.cases ne 0 and oe-boll.qty-case ne 0 then
              v-case-line = string(oe-boll.cases) + " @ " +
                                     string(oe-boll.qty-case).
            else
              v-case-line = "".
            if oe-boll.partial ne 0 then
            assign v-part-line = "1" + " @ " + string(oe-boll.partial).
            else assign v-part-line = "".

            do i = 1 to 5:
              if (80 - length(v-case-cnt[i])) gt length(v-case-line) and
                v-case-line ne "" then
              assign v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                     v-case-line = "".
              if (80 - length(v-case-cnt[i])) gt length(v-part-line) and
                v-part-line ne "" then
              assign v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
                     v-part-line = "".
            end. /* 1 to 5 */
          end. /* each oe-boll */

            assign v-line = v-line + 1
                   v-printline = v-printline + 2.

            v-beeler-lines = 0.
            do v = 1 to 3:

              if v eq 1 then next.
              v-ord# = if v = 1 and v-print-fmt = "Beeler" then
                ar-invl.ord-no else 0.
              v-part-info = if      v eq 1 then ar-invl.part-no
                            else if v eq 2 then ar-invl.part-dscr1
                            else                ar-invl.part-dscr2.

              if v-part-info ne "" or v-ord# > 0 then
                v-beeler-lines = v-beeler-lines + 1.
            end.

            do i = 1 to 5:
              if v-case-cnt[i] ne "" then
                assign v-line = v-line + 1
                       v-beeler-lines = v-beeler-lines + 1.
            end.
            v-printline = v-printline + v-beeler-lines.

            if v-printline ge 29 then
            do:
              put skip(31 - v-printline) "* CONTINUED *" at 68.
              v-printline = 2 + v-beeler-lines.
              page.
            end.

            if ar-invl.inv-qty eq 0 then
              assign v-qty = ar-invl.qty.
            else
              assign v-qty = ar-invl.inv-qty.

            display ar-invl.po-no ar-invl.i-no v-qty
                      ar-invl.unit-pr ar-invl.pr-uom ar-invl.amt
                      ar-invl.ord-no ar-invl.i-name
                      ar-invl.ship-qty when v-print-fmt ne "Imperial"
                      with frame detail.

            down with frame detail.

            do v = 1 to 3:

              if v eq 1 then next.
              v-ord# = if v = 1 and v-print-fmt = "Beeler" then
                ar-invl.ord-no else 0.
              v-part-info = if      v eq 1 then ar-invl.part-no
                            else if v eq 2 then ar-invl.part-dscr1
                            else                ar-invl.part-dscr2.

              if v-part-info ne "" or v-ord# > 0 then do:
                display v-ord# v-part-info /* skip */ with frame beeler.
                down with frame beeler.
              end.
            end.

            v-part-info = ar-invl.i-dscr.
            if v-part-info ne "" then do:
              display v-part-info with frame beeler.
              down with frame beeler.
            end.

            put skip(1).
            assign v-printline = v-printline + 1.
        end. /* each ar-invl */

        do:  /* T O T A L S */
          put skip(29 - v-printline).

          old-frt = ar-inv.freight.

          if not ar-inv.f-bill then ar-inv.freight = 0.

          assign
            tmp1  = 0
            tmp2  = ?
            v-net = ar-inv.due - ar-inv.tax-amt - ar-inv.freight.

          release terms.
          find first terms where terms.company = ar-inv.company and
            terms.t-code = ar-inv.terms no-lock no-error.

          if avail terms then
            assign
             tmp1 = v-net * round(terms.disc-rate / 100, 2)
             tmp2 = today + terms.disc-days.
          
          if v-posted then tmp2 = ?.

          find cust where cust.company eq cocode and
                     cust.cust-no eq ar-inv.cust-no no-lock no-error.
          if available cust and cust.sort eq "Y" then
          do:
            assign
              net1 = v-net * v-tx-rate[1] / 100
              net2 = v-net * v-tx-rate[2] / 100
              net3 = v-net * v-tx-rate[3] / 100

              net1 = net1 + ar-inv.freight * v-tx-rate[1] / 100
              net2 = net2 + ar-inv.freight * v-tx-rate[2] / 100
              net3 = net3 + ar-inv.freight * v-tx-rate[3] / 100

              net1 = round(net1,2)
              net2 = round(net2,2)
              net3 = round(net3,2).

            if ar-inv.tax-amt ne (net1 + net2 + net3) then
              if net3 > 0 then
                net3 = net3 + (ar-inv.tax-amt - (net1 + net2 + net3)).
              else
              if net2 > 0 then
                net2 = net2 + (ar-inv.tax-amt - (net1 + net2 + net3)).
              else
                net1 = net1 + (ar-inv.tax-amt - (net1 + net2 + net3)).

            display
              v-tax-code[1]
              net1
              v-tax-code[2]
              net2
              v-tax-code[3]
              net3
              ar-inv.tax-amt skip(1)
            with frame tax.
          end.

          else display " " skip(1) with frame blankl.

          clear frame totals-comp  no-pause.
          clear frame totals-comp2 no-pause.
          clear frame totals      no-pause.

          if v-print-head /* Print invoice headers */ then
            if cust.sort = "N" then
              display
                ar-inv.freight
                ar-inv.tax-amt
                v-net
                tmp1 when avail terms
                tmp2 when avail terms
                v-net @ ar-inv.due
                ar-inv.due when avail terms or ar-inv.terms eq "CASH"
              with frame totals-comp.
            else
              display
                ar-inv.freight
                v-net
                tmp1 when avail terms
                tmp2 when avail terms
                v-net @ ar-inv.due
                ar-inv.due when avail terms or ar-inv.terms = "CASH"
              with frame totals-comp2.

          else
            if cust.sort eq "N" then
              display
                disp-frt
                ar-inv.freight
                ar-inv.tax-amt
                v-net
                tmp1 when avail terms
                tmp2 when avail terms
                v-net @ ar-inv.due
                ar-inv.due when avail terms or ar-inv.terms eq "CASH"
              with frame totals.
            else
              display
                disp-frt
                ar-inv.freight
                v-net
                tmp1 when avail terms
                tmp2 when avail terms
                v-net @ ar-inv.due
                ar-inv.due when avail terms or ar-inv.terms eq "CASH"
              with frame totals.
        end.
        /* WFK - Feb 6, 2016 
           Remove reference to edi as it is not implemented  */  

       /* {sys/inc/o810hookPL.i &reprint=ar-inv.printed &force_asn=false} */

        assign /* ar-inv.inv-date = today */
               ar-inv.printed = yes
               ar-inv.stat = "X"
               ar-inv.freight = old-frt.
      end. /* do transaction avail ar-inv */
     end.   /* do cnt = ... */
    end. /* each tt-report, first xar-inv */

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */

