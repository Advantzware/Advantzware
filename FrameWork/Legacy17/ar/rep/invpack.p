/* ----------------------------------------------- ar/rep/invpack.p 04/98 FWK */
/*                                                                            */
/* A/R Invoice PAC 1/2 Print Program - A/R Module                             */
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
def var v-sold-name        as   char format "x(30)" no-undo.
def var v-sold-addr1       as   char format "x(30)" no-undo.
def var v-sold-addr2       as   char format "x(30)" no-undo.
def var v-carrier          as   char format "x(20)" no-undo.
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

form skip(1)
     ar-inv.freight format "->,>>9.99" to 22
     ar-inv.tax-amt format "->,>>9.99" to 32
     ar-inv.net format "->,>>>,>>9.99" to 46
     tmp1 format "->,>>9.99" to 56
     tmp2 to 67
     ar-inv.due format "->,>>>,>>9.99" to 80
  with frame totals-comp no-labels no-box no-underline stream-io width 80.

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

v-max-lines = 13.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id

    break by ar-inv.inv-no:

  find cust
      where cust.company eq cocode
        and cust.cust-no eq ar-inv.cust-no
      no-lock no-error.

  find shipto
      where shipto.company eq cocode
        and shipto.cust-no eq ar-inv.cust-no
        and shipto.ship-id eq ar-inv.ship-id
      no-lock no-error.

  find first carrier
      where carrier.company eq cocode
        and carrier.carrier eq ar-inv.carrier
      no-lock no-error.
 v-carrier = if available carrier then carrier.dscr else "".

 if available cust then
   v-addr3      = cust.city + ", " +
                  cust.state + "  " +
                  cust.zip.

 if available shipto then
 assign
   v-sold-name  = shipto.ship-name
   v-sold-addr1 = shipto.ship-addr[1]
   v-sold-addr2 = shipto.ship-addr[2]
   v-sold-addr3 = shipto.ship-city + ", " +
                  shipto.ship-state + "  " +
                  shipto.ship-zip.
 else
 assign
   v-sold-name = ""
   v-sold-addr1 = ""
   v-sold-addr2 = ""
   v-sold-addr3 = "".

 assign
   v-fob        = if ar-inv.fob-code begins "ORIG" then "Origin"
                                                   else "Destination"
   v-printline  = 0.

  if first(ar-inv.inv-no) then
    form header
         skip(6)
         v-hdr[1]
         ar-inv.inv-no         to 66
         ar-inv.inv-date       to 77 FORMAT "99/99/99" 
         skip(3)
         v-hdr[2]
         cust.name             at 2
         v-sold-name           at 47
         cust.addr[1]          at 2
         v-sold-addr1          at 47
         cust.addr[2]          at 2
         v-sold-addr2          at 47
         v-addr3               at 2
         v-sold-addr3          at 47
         skip(1)
         v-hdr[3]
         ar-inv.po-no
         ar-inv.due-date       FORMAT "99/99/99" 
         space(2)
         v-carrier          format "x(20)"
         v-fob
         ar-inv.terms-d        format "x(15)"
         space(2)
         cust.sman             when avail cust
         skip(1)
         v-hdr[4]
         skip(1)
      with frame invheadh page-top no-labels no-box no-underline stream-io width 80.

    view frame invheadh.
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

    display ar-invl.qty
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
     tmp2 = ar-inv.inv-date + terms.disc-days.

  display " " skip with frame blankl.

  display ar-inv.freight
          ar-inv.tax-amt
          ar-inv.due - ar-inv.freight @ ar-inv.net
          tmp1 when avail terms
          tmp2 when avail terms
          ar-inv.due
      with frame totals-comp.

  ar-inv.printed = yes.
end. /* for each ar-inv record */

/* End ----------------------------------- Copr. 1997  Advanced Software Inc. */

