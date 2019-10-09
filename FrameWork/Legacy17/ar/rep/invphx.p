/* ------------------------------------------------ ar/rep/invphx.p 02/97 JLF */
/*                                                                            */
/* A/R Invoice for Phoenix - A/R Module                                       */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

{ar/rep/invoice.i}

def var save_id            as   recid no-undo.
def var v-dscr-lines       as   int.
def var v-printline        as   int.
def var v-part-info        like ar-invl.part-dscr1.
def var v-fob              as   char format "x(11)".
def var v-addr3            as   char format "x(30)".
def var v-sold-addr3       as   char format "x(30)".
def var v-shipto-name      as   char format "x(30)".
def var v-shipto-addr      as   char format "x(30)" extent 2.
def var v-shipto-city      as   char format "x(15)".
def var v-shipto-state     as   char format "x(2)".
def var v-shipto-zip       as   char format "x(10)".
def var v-net              like ar-inv.due.
def var tmp1               as   dec.
def var tmp2               as   date.
def var net1               as   dec.
def var net2               as   dec.
def var net3               as   dec.
def var v-invhead          as   char format "x(13)" initial "I N V O I C E".
def var v-inv-date         as   date initial TODAY.
def var v-c-name           like company.name.
def var v-c-addr           like company.addr.
def var v-c-city           like company.city.
def var v-c-state          like company.state.
def var v-c-zip            like company.zip.
DEF VAR v-index AS INT NO-UNDO.


form ar-invl.po-no
     ar-invl.i-no          at 19
     ar-invl.qty           to 52 format "->,>>>,>>9"
     ar-invl.unit-pr       to 64 format "->>,>>9.99" space(0)
     ar-invl.pr-uom        to 68
     ar-invl.amt           to 80 format "->>>,>>9.99" skip
     ar-invl.i-name        at 19 format "x(23)"
     ar-invl.ship-qty      to 52 format "->,>>>,>>9" skip
  with frame detail no-attr-space no-labels no-box no-underline down stream-io width 80.

form v-part-info       at 29
  with frame dscr no-labels no-box no-underline down stream-io width 80.

form skip(2)
     ar-inv.freight format "->,>>9.99" at 11
     ar-inv.tax-amt format "->,>>9.99" at 21
     ar-inv.net to 43 format "->,>>>,>>9.99"
     tmp1 to 56 format "->,>>9.99"
     tmp2 at 60
     space(0)
     ar-inv.due to 80 format "->,>>>,>>9.99"
  with frame totals no-labels no-box no-underline stream-io width 80.

form "Taxes- " at 1
     stax.tax-code[1] at 9 format "x(4)"
     space(0) ":"
     net1 at 15 format "->>,>>9.99"
     stax.tax-code[2] at 28 format "x(4)"
     space(0) ":"
     net2 at 34 format "->>,>>9.99"
     stax.tax-code[3] at 46 format "x(4)"
     space(0) ":"
     net3 at 52 format "->>,>>9.99"
     "Tot Tax:" to 70
     space(0)
     ar-inv.tax-amt to 80 format "->>,>>9.99"
  with frame tax no-labels no-box no-underline stream-io width 80.

form skip(1)
  with frame blankl no-labels no-box no-underline stream-io width 80.

find first company where company.company = cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

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
   v-printline  = 0.

  if first-of(ar-inv.inv-no) then do:
    form header
         v-invhead at 24
         ar-inv.inv-date at 58 FORMAT "99/99/99" skip
         v-c-name at 11 skip
         v-c-addr[1] at 11 skip
         v-c-addr[2] at 11
         ar-inv.inv-no at 58 skip
         v-c-city at 11
         v-c-state v-c-zip skip
         cust.sman at 58 when avail cust skip(3)
         v-fob at 6
         ar-inv.terms-d at 41
         skip(3)
         ar-inv.cust-no   at 11 skip
/*
         ar-inv.cust-name at 11 shipto.ship-name    at 50 skip
         ar-inv.addr[1]   at 11 shipto.ship-addr[1] at 50 skip
         ar-inv.addr[2]   at 11 shipto.ship-addr[2] at 50 skip
*/
         cust.name        at 11 shipto.ship-name    at 50 skip
         cust.addr[1]     at 11 shipto.ship-addr[1] at 50 skip
         cust.addr[2]     at 11 shipto.ship-addr[2] at 50 skip
         v-addr3          at 11 v-sold-addr3        at 50
         skip(4)
         carrier.dscr at 11 format "x(25)" when avail carrier
         ar-inv.t-weight to 44 format ">>>>9.99"
         skip(4)
        with frame invhead page-top no-labels no-box no-underline stream-io width 90.

    view frame invhead.
    page.
    v-printline = 0.
  end.

  for each ar-invl
      where ar-invl.x-no eq ar-inv.x-no
      break by ar-invl.line:

    v-printline = v-printline + 3.

    v-dscr-lines = 0.
    do v-index = 1 to 3:
      v-part-info = if v-index eq 1 then ar-invl.i-dscr
                    else
                    if v-index eq 2 then ar-invl.part-dscr1
                    else           ar-invl.part-dscr2.

      if v-part-info ne "" then v-dscr-lines = v-dscr-lines + 1.
    end.
    v-printline = v-printline + v-dscr-lines.

    if v-printline gt 27 then do:
      v-printline = v-printline - (3 + v-dscr-lines).
      put skip(27 - v-printline) "* CONTINUED *" to 80.
      v-printline = 3 + v-dscr-lines.
      page.
    end.

    display ar-invl.po-no
            ar-invl.i-no
            ar-invl.qty
            ar-invl.unit-pr
            ar-invl.pr-uom
            ar-invl.amt
            ar-invl.i-name
            ar-invl.ship-qty

        with frame detail.

    do v-index = 1 to 3:
      v-part-info = if v-index eq 1 then ar-invl.i-dscr
                    else
                    if v-index eq 2 then ar-invl.part-dscr1
                    else           ar-invl.part-dscr2.

      if v-part-info ne "" then do:
        display v-part-info skip with frame dscr.
        down with frame dscr.
      end.
    end.

    put skip(1).
  end. /* for each ar-invl record */

  if v-printline gt 27 then v-printline = 27.

  put skip(27 - v-printline).

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

      display stax.tax-code[1]
              net1
              stax.tax-code[2]
              net2
              stax.tax-code[3]
              net3
              ar-inv.tax-amt
          with frame tax.
    end.
      display " " skip(1) with frame blankl1.
  end.

  else display " " skip(1) with frame blankl.

  if cust.sort eq "N" then
    display ar-inv.freight
            ar-inv.tax-amt
            ar-inv.due - ar-inv.freight @ ar-inv.net
            tmp1 when avail terms
            tmp2 when avail terms
            ar-inv.due
        with frame totals.
  else
    display ar-inv.freight
            ar-inv.due - ar-inv.freight @ ar-inv.net
            tmp1 when avail terms
            tmp2 when avail terms
            ar-inv.due
        with frame totals.

  ar-inv.printed = yes.

end. /* for each ar-inv record */

/* End ----------------------------------- Copr. 1997  Advanced Software Inc. */
