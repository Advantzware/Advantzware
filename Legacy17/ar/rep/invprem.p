/* ----------------------------------------------- ar/rep/invprem.p 02/98 FWK */
/*                                                                            */
/* A/R Invoice Premier Print Program - A/R Module                             */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

{ar/rep/invoice.i}

def var save_id            as   recid no-undo.
def var v-dscr-lines       as   int.
def var v-printline        as   int.
def var v-part-info        like ar-invl.part-dscr1.
def var v-addr3            as   char format "x(30)".
def var v-sold-addr3       as   char format "x(30)".
def var v-fob              as   char format "x(11)".
def var v-net              like ar-inv.due.
def var tmp1               as   dec.
def var tmp2               as   date.
def var net1               as   dec.
def var net2               as   dec.
def var net3               as   dec.
def var v-hdr              as   char format "x(80)" extent 4.
def var v-max-lines        as   int.
def var v-tot-lines        like ar-invl.amt no-undo.
def var v-ship-date           like ar-inv.inv-date no-undo.

assign
 substr(v-hdr[1],60,20) = "INVOICE#  INV DATE"

 substr(v-hdr[2],01,07) = "SOLD TO"
 substr(v-hdr[2],46,07) = "SHIP TO"

 v-hdr[3] = "YOUR ORDER NO  DELIV DATE SHIP VIA             F.O.B.   " +
            "   TERMS            SLMN"

 v-hdr[4] = "    QTY  P  C JOB NUMBER    DESCRIPTION                 " +
            "      PRICE       TOTAL ".

form ar-invl.qty               format ">>>>>>9-"
     ar-invl.i-name    at 15   format "x(30)"
     ar-invl.unit-pr   to 64   format "->>>9.99"       /* ekw02240001 */     
     ar-invl.amt       to 79   format "->>>>>9.99"     /* ekw02240001 */
     ar-invl.i-dscr    at 15   format "x(30)"
  with frame detail no-attr-space no-labels no-box no-underline down stream-io width 80.

form ar-invl.ship-qty AT 1 format "->>>>9"
     ar-invl.po-no AT 9 format "x(15)"
     ar-invl.i-name    at 26   format "x(30)"
     ar-invl.unit-pr   at 61   format "->>>9.9999"     /* ekw02240001 */
     ar-invl.amt       at 73   format "->>>9.99"
with frame detailoe no-attr-space no-labels no-box
     no-underline down stream-io width 90.

form
  ar-invl.prep-charge at 5 space(2) ar-invl.prep-dscr
  ar-invl.amt to 80 format "->>,>>9.99"
  skip(1)
with frame detailm no-labels no-box no-underline down stream-io width 90.

form v-part-info       at 15
  with frame dscr no-labels no-box no-underline down stream-io width 80.

form
     "Sub-Total:" to 65 v-tot-lines to 79
     "Sales Tax:" to 65 ar-inv.tax-amt to 79
     "Freight:" to 65 ar-inv.freight format "->,>>9.99" to 79
     "-------------" to 79
     "Total Invoice:" TO 65 ar-inv.due format "->,>>>,>>9.99" to 79
     "=============" to 79
  with frame totals-comp no-labels no-box no-underline stream-io width 80.

form skip(1)
  with frame blankl no-labels no-box no-underline stream-io width 80.


if (not v-print-head) then v-hdr = "".

find first company where company.company = cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

v-max-lines = 26.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id

    break by ar-inv.inv-no:

  find first cust
      where cust.company eq cocode
        and cust.cust-no eq ar-inv.cust-no
      no-lock.

  find first shipto
      where shipto.company eq cocode
        and shipto.cust-no eq ar-inv.cust-no
        and shipto.ship-id eq ar-inv.ship-id
      no-lock no-error.

  if not avail shipto then
  find first shipto
      where shipto.company eq cocode
        and shipto.cust-no eq ar-inv.cust-no
        and shipto.ship-id eq ar-inv.cust-no
      no-lock no-error.

  if not avail shipto then
  find first shipto
      where shipto.company eq cocode
        and shipto.cust-no eq ar-inv.cust-no
      no-lock.

  find first carrier
      where carrier.company eq cocode
        and carrier.carrier eq ar-inv.carrier
      no-lock no-error.

  assign
   v-addr3      = cust.city + ", " +
                  cust.state + "  " +
                  cust.zip
   v-sold-addr3 = shipto.ship-city + ", " +
                  shipto.ship-state + "  " +
                  shipto.ship-zip
   v-fob        = if ar-inv.fob-code begins "ORIG" then "Origin"
                                                   else "Destination"
   v-printline  = 0
   v-ship-date  = ar-inv.inv-date.

  find first ar-invl where ar-invl.company eq ar-inv.company
                       and ar-invl.inv-no eq ar-inv.inv-no no-lock no-error.
  find first oe-bolh where oe-bolh.b-no eq ar-invl.b-no no-lock no-error.
  if avail oe-bolh then
  do:
    find first oe-ord where oe-ord.ord-no eq oe-bolh.ord-no and
                            oe-ord.company eq cocode no-lock no-error.
    v-ship-date = oe-bolh.bol-date.
  end.

  if first-of(ar-inv.inv-no) then
  do:
    if ar-inv.ord-no eq 0 then
    do:
      form header
         skip(2)
       ar-inv.inv-date       at 62 FORMAT "99/99/99" 
       ar-inv.inv-no         at 72
       skip(4)
       cust.name             at 8 shipto.ship-name      at 50
       cust.addr[1]          at 8 shipto.ship-addr[1]   at 50
       cust.addr[2]          at 8 shipto.ship-addr[2]   at 50
       v-addr3               at 8 v-sold-addr3          at 50
       skip(3)
       "CUST ORDER#" at 2 ar-inv.po-no
       "TERM:" to 48 ar-inv.terms-d to 79
       "SALESMAN:" at 44 ar-inv.sman-no
       skip(6)
       "QUANTITY" at 3 "DESCRIPTION" at 15 "PRICE" to 64 "TOTAL " to 79
/*
     "--------" at 1 "-----------" at 15 "-----" to 64 "----- " to 79
*/
       skip(1)
        with frame invhead page-top no-labels no-box no-underline stream-io width 80.
      view frame invhead.
    end.
    else
    do:
      form header
        skip(2)
        ar-inv.inv-date       at 62 FORMAT "99/99/99" 
        ar-inv.inv-no         at 72 skip(4)
        cust.name             at 8 shipto.ship-name      at 50
        cust.addr[1]          at 8 shipto.ship-addr[1]   at 50
        cust.addr[2]          at 8 shipto.ship-addr[2]   at 50
        v-addr3               at 8 v-sold-addr3          at 50
        skip(3)
        "B.O.L. #:" AT 50 oe-bolh.bol-no skip
        "ORDER NUMBER & DATE:" TO 22 ar-inv.ord-no ar-inv.ord-date FORMAT "99/99/99" 
        "SALESMAN:" AT 50 ar-invl.sman[1] skip
        "INVOICE NOTE:" TO 15 oe-ord.bill-i[1] format "x(25)"
        "ORIGINAL ORDER:" AT 50 skip
        "DATE SHIPPED:" TO 15 v-ship-date FORMAT "99/99/99" 
        "TERMS:" AT 50 ar-inv.terms-d format "x(15)" skip
        "SHIPPING INSTRUCTIONS:" TO 24 /* v-ship-i format "x(25)" */skip(2)
        "QTY" AT 3 "PRICE" AT 63 skip
        "SHIP" AT 3 "P.O.#" AT 9 "DESCRIPTION" AT 26
        ar-invl.pr-uom at 63 format "x(4)" "EXTENSION" AT 72 skip(1)
         with frame invhead-comp page-top no-labels no-box no-underline stream-io width 90.
        view frame invhead-comp.
    end.
  end.

  page.
  assign v-tot-lines = 0.

  for each ar-invl
      where ar-invl.x-no eq ar-inv.x-no
        and ar-invl.misc eq no
      break by ar-invl.line:

    v-printline = v-printline + 2.

    if v-printline gt 28 then do:
      v-printline = v-printline - (2 + v-dscr-lines).
      put skip(30 - v-printline) "* CONTINUED *" to 80.
      v-printline = 2 + v-dscr-lines.
      page.
    end.

    assign v-tot-lines = v-tot-lines + ar-invl.amt.

    if ar-inv.ord-no eq 0 then
    do:
      display ar-invl.qty
              ar-invl.i-name
              ar-invl.unit-pr
              ar-invl.amt
              ar-invl.i-dscr
        with frame detail.
      down with frame detail.
      if ar-invl.i-dscr ne "" then
      do:
        assign v-printline = v-printline + 1.
        put skip(1).
      end.
    end.
    else
    do:
      display ar-invl.ship-qty
              ar-invl.po-no
              ar-invl.i-name
              ar-invl.unit-pr
              ar-invl.amt
        with frame detailoe.
      down with frame detailoe.
      if ar-invl.part-dscr1 ne "" then
      do:
        put ar-invl.part-dscr1 at 26 skip.
            v-printline = v-printline + 1.
      end.
      if ar-invl.part-dscr2 ne "" then
      do:
        put ar-invl.part-dscr2 at 26 skip.
            v-printline = v-printline + 1.
      end.
      put skip(1).
    end.

  end. /* for each ar-invl record */

  for each ar-invl
      where ar-invl.x-no eq ar-inv.x-no
        and ar-invl.misc eq yes
      break by ar-invl.misc:

    v-printline = v-printline + 2.

    if v-printline gt 28 then do:
      v-printline = v-printline - (2 + v-dscr-lines).
      put skip(30 - v-printline) "* CONTINUED *" to 80.
      v-printline = 2 + v-dscr-lines.
      page.
    end.

    assign v-tot-lines = v-tot-lines + ar-invl.amt.

    if ar-inv.ord-no eq 0 then
    do:
      display ar-invl.qty
              ar-invl.i-name
              ar-invl.unit-pr
              ar-invl.amt
              ar-invl.i-dscr
        with frame detail.
      down with frame detail.
      if ar-invl.i-dscr ne "" then
      do:
        assign v-printline = v-printline + 1.
        put skip(1).
      end.
    end.
    else
    do:
      if first-of(ar-invl.misc) then
        put skip(1) "** Miscellaneous Items **" at 23 skip(1).

      display ar-invl.prep-charge
                   ar-invl.prep-dscr
              ar-invl.amt with frame detailm.
      down with frame detailm.

    end.

  end. /* for each ar-invl record */

  do i = 1 to 4:
    if ar-inv.bill-i[i] ne "" then do:
      if v-printline gt 28 then do:
        v-printline = v-printline - (2 + v-dscr-lines).
        put skip(30 - v-printline) "* CONTINUED *" to 80.
        v-printline = 2 + v-dscr-lines.
        page.
      end.

      put ar-inv.bill-i[i] at 5 skip.
      assign v-printline = v-printline + 1.
    end.
  end. /* 1 to 4 */

  if v-printline gt v-max-lines then v-printline = v-max-lines.

  put skip(v-max-lines - v-printline).

  find first terms
      where terms.company eq ar-inv.company
        and terms.t-code  eq ar-inv.terms
      no-lock no-error.

  if avail terms then
    assign
     tmp1 = ar-inv.net * (round(terms.disc-rate / 100, 2))
     tmp2 = today + terms.disc-days.

  if cust.sort eq "Y" then do:
    find first stax
        WHERE stax.company   EQ cocode
          and stax.tax-group eq ar-inv.tax-code
        no-lock no-error.
                      
    if avail stax then do:
      assign
       net1 = ar-inv.net * (stax.tax-rate[1] / 100)
       net2 = ar-inv.net * (stax.tax-rate[2] / 100)
       net3 = ar-inv.net * (stax.tax-rate[3] / 100)

       net1 = net1 + if stax.tax-frt[1] then
                       (ar-inv.freight * (stax.tax-rate[1] / 100)) else 0
       net2 = net2 + if stax.tax-frt[2] then
                       (ar-inv.freight * (stax.tax-rate[2] / 100)) else 0
       net3 = net3 + if stax.tax-frt[3] then
                       (ar-inv.freight * (stax.tax-rate[3] / 100)) else 0

       net1 = round(net1,2)
       net2 = round(net2,2)
       net3 = round(net3,2).

      if ar-inv.tax-amt ne (net1 + net2 + net3) then
        net1 = net1 + (ar-inv.tax-amt - (net1 + net2 + net3)).
    end.
  end.

  else display " " skip with frame blankl.

  display v-tot-lines
          ar-inv.tax-amt
          ar-inv.freight when ar-inv.f-bill
          ar-inv.due
        with frame totals-comp.

  ar-inv.printed = yes.
end. /* for each ar-inv record */

/* End ----------------------------------- Copr. 1997  Advanced Software Inc. */

