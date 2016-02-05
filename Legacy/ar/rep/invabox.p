/* ----------------------------------------------- ar/rep/invabox.p 11/00 FWK */
/*                                                                            */
/* A/R Invoice Abox Print Program - A/R Module                                */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

{ar/rep/invoice.i}

def var save_id            as   recid no-undo.
def var v-dscr-lines       as   int NO-UNDO.
def var v-printline        as   int NO-UNDO.
def var v-part-info        like ar-invl.part-dscr1 NO-UNDO.
def var v-addr3            as   char format "x(30)" NO-UNDO.
def var v-sold-addr3       as   char format "x(30)" NO-UNDO.
def var v-fob              as   char format "x(11)" NO-UNDO.
def var v-net              like ar-inv.due NO-UNDO.
def var tmp1               as   dec NO-UNDO.
def var tmp2               as   date NO-UNDO.
def var net1               as   dec NO-UNDO.
def var net2               as   dec NO-UNDO.
def var net3               as   dec NO-UNDO.
def var v-hdr              as   char format "x(80)" extent 4 NO-UNDO.
def var v-max-lines        as   int NO-UNDO.
def var v-due-date         like ar-inv.due-date no-undo.
def var v-po-no            like ar-invl.po-no no-undo.
def workfile w-tax
    field w-dsc as   char
    field w-tax as   dec.
def var v-t-tax      as   dec extent 3 NO-UNDO.
DEF VAR v-frt-tax AS DEC NO-UNDO.
def var v-lines as INT NO-UNDO.
DEF VAR v-t-price AS DEC NO-UNDO.
DEF VAR v-index AS INT NO-UNDO.

assign
 substr(v-hdr[1],60,20) = "INVOICE#  INV DATE"

 substr(v-hdr[2],01,07) = "SOLD TO"
 substr(v-hdr[2],46,07) = "SHIP TO"

 v-hdr[3] = "YOUR ORDER NO  DELIV DATE SHIP VIA             F.O.B.   " +
            "   TERMS            SLMN"

 v-hdr[4] = "    QTY  P  C JOB NUMBER    DESCRIPTION                 " +
            "      PRICE       TOTAL ".

form ar-invl.qty               format ">>>>>>9-"
     ar-invl.i-no      at 15   format "x(13)"
     ar-invl.i-name    at 29   format "x(30)"
     ar-invl.unit-pr   at 61   format ">>>>9.99-"
     ar-invl.amt       to 80   format ">>>>>9.99-"
  with frame detail no-attr-space no-labels no-box no-underline down stream-io width 80.

form v-part-info       at 29
  with frame dscr no-labels no-box no-underline down stream-io width 80.

form "Freight" to 20 "Tax" to 27
     "Net Amount" TO 43 "Cash Disc" TO 56 "If Paid By" AT 59
     "Invoice Amt" TO 80
     ar-inv.freight format "->,>>9.99" at 11
     ar-inv.tax-amt format "->,>>9.99" at 21
     ar-inv.gross to 43 format "->,>>>,>>9.99"
     tmp1 to 56 format "->,>>9.99"
     tmp2 at 60
     space(0)
     ar-inv.net to 80 format "->,>>>,>>9.99"
  with frame totals-comp no-labels no-box no-underline stream-io width 80.

form "Freight" to 20
     "Net Amount" TO 43 "Cash Disc" TO 56 "If Paid By" AT 59
     "Invoice Amt" TO 80
     ar-inv.freight format "->,>>9.99" at 11
     ar-inv.tax-amt format "->,>>9.99" at 21
     ar-inv.gross to 43 format "->,>>>,>>9.99"
     tmp1 to 56 format "->,>>9.99"
     tmp2 at 60
     space(0)
     ar-inv.net to 80 format "->,>>>,>>9.99"
  with frame totals-comp2 no-labels no-box no-underline stream-io width 80.

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
     skip(1)
  with frame tax no-labels no-box no-underline stream-io width 80.

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
      no-lock.

  assign
   v-po-no    = ""
   v-due-date = ?
   v-t-tax = 0   .

  find first carrier
      where carrier.company eq cocode
        and carrier.carrier eq ar-inv.carrier
      no-lock no-error.

  for each ar-invl where ar-invl.x-no eq ar-inv.x-no no-lock:
    if v-po-no eq "" then v-po-no = ar-invl.po-no.
    find first oe-bolh
        where oe-bolh.company eq ar-inv.company
          and oe-bolh.bol-no  eq ar-invl.bol-no
        no-lock no-error.
    if avail oe-bolh and v-posted and v-due-date eq ? then
      v-due-date = oe-bolh.bol-date.
  end.
  
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

  if first(ar-inv.inv-no) then
    form header
         skip(11)
         v-hdr[1]
         ar-inv.inv-no         to 66
         ar-inv.inv-date       to 77 FORMAT "99/99/99" 
         skip(3)
         v-hdr[2]
         cust.name             at 2
         shipto.ship-name      at 47
         cust.addr[1]          at 2
         shipto.ship-addr[1]   at 47
         cust.addr[2]          at 2
         shipto.ship-addr[2]   at 47
         v-addr3               at 2
         v-sold-addr3          at 47
         skip(2)
         v-hdr[3]
         v-po-no               
         v-due-date            format "99/99/99" when v-due-date ne ?
         /* ar-inv.due-date */
         space(2)
         carrier.dscr          format "x(20)" when avail carrier
         v-fob
         ar-inv.terms-d        format "x(15)"
         space(2)
         cust.sman             when avail cust
         skip(1)
         v-hdr[4]
         skip(1)
      with frame invhead page-top no-labels no-box no-underline stream-io width 80.

  view frame invhead.
  page.

  for each ar-invl
      where ar-invl.x-no eq ar-inv.x-no
      break by ar-invl.line:

    v-printline = v-printline + 2.

    v-dscr-lines = 0.
    do v-index = 1 to 3:
      v-part-info = if v-index eq 1 then ar-invl.i-dscr
                    else
                    if v-index eq 2 then ar-invl.part-dscr1
                    else           ar-invl.part-dscr2.

      if v-part-info ne "" then v-dscr-lines = v-dscr-lines + 1.
    end.
    v-printline = v-printline + v-dscr-lines.

    if v-printline gt 15 then do:
      v-printline = v-printline - (2 + v-dscr-lines).
      put skip(17 - v-printline) "* CONTINUED *" to 80.
      v-printline = 2 + v-dscr-lines.
      page.
    end.

    display ar-invl.inv-qty @ ar-invl.qty
            ar-invl.i-no
            ar-invl.i-name
            ar-invl.unit-pr
            ar-invl.amt

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
/*      assign
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
 */
      v-frt-tax = ar-inv.freight.        
      v-t-price = ar-inv.gross.
      IF ar-inv.tax-code <> "" AND AVAIL stax THEN
      DO i = 1 TO 3:
         if stax.tax-code[i] ne ""  then do:
              create w-tax.
              assign
               w-dsc      = stax.tax-dscr[i]
               w-tax      = round((if stax.accum-tax  then v-t-price
                                                       ELSE ar-inv.gross) *
                                      stax.tax-rate[i] / 100,2)                 
               v-t-price  = v-t-price + w-tax
               v-t-tax[i] = v-t-tax[i] + w-tax
               v-lines    = v-lines + 1.
         END.
      END.
      IF ar-inv.tax-code <> "" and
         (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
         AND ar-inv.freight <> 0
         AND AVAIL stax THEN
      do i = 1 to 3:
        
         if stax.tax-frt[i] AND stax.tax-code[i] ne "" THEN DO:
            create w-tax.
              assign
               w-dsc      = stax.tax-dscr[i]
               w-tax      = round((if stax.accum-tax  then v-frt-tax
                                                       ELSE ar-inv.freight) *
                                      stax.tax-rate[i] / 100,2)                 
               v-frt-tax  = v-frt-tax + w-tax
               v-t-tax[i] = v-t-tax[i] + w-tax
               v-lines    = v-lines + 1.
         END.
      end.      
      ASSIGN net1 = v-t-tax[1]
             net2 = v-t-tax[2]
             net3 = v-t-tax[3].
      display stax.tax-code[1]
              net1
              stax.tax-code[2]
              net2
              stax.tax-code[3]
              net3
              ar-inv.tax-amt
          with frame tax.
    end.
  end.

  else display " " skip with frame blankl.

  if cust.sort eq "N" then
    display ar-inv.freight
            ar-inv.tax-amt
            /*ar-inv.net - ar-inv.freight @ ar-inv.due*/
            ar-inv.gross
            tmp1 when avail terms
            tmp2 when avail terms
            /*ar-inv.net + ar-inv.tax-amt @ ar-inv.net*/
            ar-inv.net
        with frame totals-comp.
    else
    display ar-inv.freight
            /*ar-inv.net - ar-inv.freight @ ar-inv.due */
            ar-inv.gross
            tmp1 when avail terms
            tmp2 when avail terms
            ar-inv.net /*+ ar-inv.tax-amt @ ar-inv.net*/
        with frame totals-comp2.

  ar-inv.printed = yes.

end. /* for each ar-inv record */

/* End ----------------------------------- Copr. 1997  Advanced Software Inc. */

