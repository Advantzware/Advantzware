/* ---------------------------------------------- ar/rep/invhalfp.p 01/97 JLF */
/*                                                                            */
/* A/R Invoice 1/2 Page Print Program - A/R Module                            */
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
def var v-due-date         like ar-inv.due-date no-undo.
def VAR v-halfp            as log.
def var v-x as char format "x(4)" initial "" no-undo.
def var v-uom like inv-line.pr-uom no-undo.
def var v-job as char format "x(9)" no-undo.
def var v-bol-no like inv-head.bol-no no-undo.
def var v as INT NO-UNDO.
def var v-tot-sqft as dec format ">,>>>,>>9.99" no-undo.

DEF VAR v-paid-by AS cha FORM "x(14)" NO-UNDO.
DEF VAR v-po-no AS CHAR NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "INVPRINT"
                      no-lock no-error.
v-paid-by = IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "Livngstn" THEN "If Received By"
            ELSE "If Paid By".

assign
 substr(v-hdr[1],60,20) = "INVOICE#  INV DATE"

 substr(v-hdr[2],01,07) = "SOLD TO"
 substr(v-hdr[2],46,07) = "SHIP TO"

 v-hdr[3] = "YOUR ORDER NO  DELIV DATE SHIP VIA             F.O.B.   " +
            "   TERMS            SLMN"

 v-hdr[4] = "    QTY  P  C JOB NUMBER    DESCRIPTION                 " +
            "      PRICE       TOTAL ".

form ar-invl.qty           format ">>>>>>9-"
     ar-invl.i-no      at 15   format "x(13)"
     ar-invl.i-name    at 29   format "x(30)"
     ar-invl.unit-pr   at 61   format ">>>>9.99-"
     ar-invl.amt       to 80   format ">>>>>9.99-"
  with frame detail no-attr-space no-labels no-box no-underline down stream-io width 80.

form ar-invl.inv-qty           format ">>>>>>9-"
     v-x               at 10   format "x(4)"
     v-job             at 15   format "x(9)"
     ar-invl.i-no      at 29   format "x(15)"
     ar-invl.unit-pr   to 67   format ">>>>9.99"
     v-uom             AT 68   FORMAT "x(2)"
     ar-invl.amt       to 80   format ">>>>>9.99-" SKIP
     "BOL#"            AT 10
     v-bol-no          at 15
     ar-invl.i-name    at 29   format "x(30)" SKIP
  with frame tri-detail no-attr-space no-labels no-box no-underline down stream-io width 80.

form v-part-info       at 29
  with frame dscr no-labels no-box no-underline down stream-io width 80.

form "Freight" to 14 "Tax" to 22
     "Net Amount" TO 38 "Cash Disc" TO 51 v-paid-by AT 54
     "Invoice Amt" TO 80
     ar-inv.freight format "->,>>9.99" at 6
     ar-inv.tax-amt format "->,>>9.99" at 16
     ar-inv.net to 38 format "->,>>>,>>9.99"
     tmp1 to 51 format "->,>>9.99"
     tmp2 at 55
     space(0)
     ar-inv.due to 80 format "->,>>>,>>9.99"
  with frame totals-comp no-labels no-box no-underline stream-io width 80.

form "Freight" to 14
     "Net Amount" TO 38 "Cash Disc" TO 51 v-paid-by AT 54
     "Invoice Amt" TO 80
     ar-inv.freight format "->,>>9.99" at 6
     ar-inv.tax-amt format "->,>>9.99" at 16
     ar-inv.net to 38 format "->,>>>,>>9.99"
     tmp1 to 51 format "->,>>9.99"
     tmp2 at 55
     space(0)
     ar-inv.due to 80 format "->,>>>,>>9.99"
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

v-halfp = v-print-fmt ne "Livngstn".

find first company where company.company = cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

if v-halfp then
  if v-print-fmt eq "Clev 1/2" THEN v-max-lines = 11.

  else
  if v-print-fmt eq "TriState" then v-max-lines = 13.

  ELSE v-max-lines = 13.

ELSE v-max-lines = 29.

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

  find first ar-invl where ar-invl.company eq ar-inv.company
                       and ar-invl.inv-no  eq ar-inv.inv-no 
                     no-lock no-error.
  if available ar-invl then 
    find first oe-bolh where oe-bolh.company eq ar-inv.company
                         and oe-bolh.bol-no eq ar-invl.bol-no
                       no-lock no-error.

  if avail oe-bolh and v-posted then v-due-date = oe-bolh.bol-date.
  
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

  FIND FIRST ar-invl USE-INDEX x-no WHERE ar-invl.x-no = ar-inv.x-no NO-LOCK NO-ERROR.

  if first(ar-inv.inv-no) then
   if v-print-fmt eq "TriState" then
    form header
         skip(7)
         v-hdr[1]
         ar-inv.inv-no         to 66
         ar-inv.inv-date       to 77  FORMAT "99/99/99" 
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
         skip(1)
         v-hdr[3]
         ar-invl.po-no
         v-due-date            FORMAT "99/99/99" 
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
      with frame invhead-tri page-top no-labels no-box no-underline stream-io width 80.

   else

   if v-halfp then
    form header
         skip(10)
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
         skip(1)
         v-hdr[3]
         ar-inv.po-no
         v-due-date            FORMAT "99/99/99" 
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
      with frame invheadh page-top no-labels no-box no-underline stream-io width 80.

   else
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
         v-po-no FORMAT "X(15)"
         v-due-date            FORMAT "99/99/99" 
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

  if v-print-fmt eq "TriState" then
    view frame invhead-tri.
  else if v-halfp then
    view frame invheadh.
  else
  DO:
    v-po-no = IF ar-inv.po-no NE "" THEN ar-inv.po-no
              ELSE ar-invl.po-no.
    view frame invhead.
  END.
  page.

  for each ar-invl
      where ar-invl.x-no eq ar-inv.x-no
      break by ar-invl.line:

    v-printline = v-printline + 2.

    v-dscr-lines = 0.
    do v = 1 to 3:
      v-part-info = if v eq 1 then ar-invl.i-dscr
                    else
                    if v eq 2 then ar-invl.part-dscr1
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
    v-x = "".
    find first oe-ordl
              where oe-ordl.company eq cocode
                and oe-ordl.ord-no  eq ar-invl.ord-no
                and oe-ordl.i-no    eq ar-invl.i-no
              no-lock no-error.
    if avail oe-ordl then do:
            find first oe-ord
                where oe-ord.company eq cocode
                  and oe-ord.ord-no  eq oe-ordl.ord-no
                no-lock no-error.
            v-x = if avail oe-ord and
                     oe-ordl.ship-qty ge
                  (oe-ordl.qty - (oe-ordl.qty * oe-ordl.under-pct / 100)) then
                    "   X" else "X   ".
    end.
    ASSIGN v-uom     = ar-invl.pr-uom
           v-job     = fill(" ",6 - length(trim(ar-invl.job-no))) +
                       trim(ar-invl.job-no) + "-" +
                       trim(string(ar-invl.job-no2,"99"))
           v-bol-no  = ar-invl.bol-no.

    if v-print-fmt eq "TriState" then
    do:
          assign v-tot-sqft = 0.
          find first itemfg where itemfg.company eq cocode
                              and itemfg.i-no eq ar-invl.i-no
                              no-lock no-error.
          if avail itemfg then
            assign v-tot-sqft = ar-invl.inv-qty * itemfg.t-sqft.
    end.
    if trim(v-job) begins "-" then assign v-job = "".

    if v-print-fmt eq "TriState" then
        display ar-invl.inv-qty
                v-x
                v-job
                ar-invl.i-no
                v-uom
                ar-invl.unit-pr
                ar-invl.amt
                v-bol-no ar-invl.i-name
                with frame tri-detail.

    ELSE display ar-invl.qty
                     ar-invl.inv-qty WHEN ar-invl.inv-qty NE 0 @ ar-invl.qty
            ar-invl.i-no
            ar-invl.i-name
            ar-invl.unit-pr
            ar-invl.amt
        with frame detail.

    do v = 1 to 3:
      v-part-info = if v eq 1 then ar-invl.i-dscr
                    else
                    if v eq 2 then ar-invl.part-dscr1
                    else           ar-invl.part-dscr2.

      if v-part-info ne "" then do:
        display v-part-info skip with frame dscr.
        down with frame dscr.
      end.
    end.
    if v-print-fmt eq "TriState" then
    do:
        put "SQ FT." at 29 v-tot-sqft at 36 skip.
        v-printline = v-printline + 1.
    end.
    put skip(1).
  end. /* for each ar-invl record */

  if v-printline gt v-max-lines then v-printline = v-max-lines.

  put skip(v-max-lines - v-printline).

  find first terms
      where terms.company eq ar-inv.company
        and terms.t-code  eq ar-inv.terms
      no-lock no-error.

  RUN custom/inv-dnet.p (ROWID(ar-inv), OUTPUT v-net).

  if avail terms then
    assign
     tmp1 = v-net * (round(terms.disc-rate / 100, 2))
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
  end.

  else display " " skip with frame blankl.

  if cust.sort eq "N" then
    display ar-inv.freight
            ar-inv.tax-amt
            v-net @ ar-inv.net
            tmp1 when avail terms
            tmp2 when avail terms
            ar-inv.due
            v-paid-by
        with frame totals-comp.
    else
    display ar-inv.freight
            v-net @ ar-inv.net
            tmp1 when avail terms
            tmp2 when avail terms
            ar-inv.due
            v-paid-by
        with frame totals-comp2.

  ar-inv.printed = yes.
end. /* for each ar-inv record */

/* End ----------------------------------- Copr. 1997  Advanced Software Inc. */

