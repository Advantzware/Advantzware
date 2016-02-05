/* ---------------------------------------------- cec/quote/quohop.p 03/99 JLF */
/* print quotes in HOP format                                                 */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def shared buffer xquo for quotehd.
def buffer xqitm for quoteitm.
def buffer xqqty for quoteqty.
def buffer xqchg for quotechg.
def buffer b-qi for quoteitm.
def buffer x-qi for quoteitm.

{est/printquo.i}

def var numfit as int no-undo.
def var v-sold as ch extent 5 no-undo.
def var v-ship as ch extent 5 no-undo.
def var v-fob as char format "x(14)".
def var v-frt-pay as char format "x(14)".
def var tot as de no-undo.
def var v-over-under as char no-undo.
def var v-str as char extent 3.

def var v-part like quoteit.part-no no-undo.
def var v-line like quoteit.line no-undo.
def var trim-size like quoteit.size no-undo.
def var cc as int no-undo.
def var v-last-page as int.
def var v-page-tot as int.
def var v-cust like quote.cust-no.
def var v-blank as char format "x(5)".
DEF VAR style-name LIKE style.dscr NO-UNDO.


def stream last-page.

tmpstore = fill("-",130).

FIND first report where report.term-id eq v-term-id NO-LOCK NO-ERROR.

FIND first xquo  where recid(xquo) eq report.rec-id NO-LOCK NO-ERROR.
IF NOT AVAIL xquo THEN RETURN.

format xqitm.part-no    to 30   format "x(30)"
       xqitm.part-dscr1 to 61
       xqitm.i-coldscr  to 89   format "x(27)"
       xqqty.qty        to 100  format ">>,>>>,>>9"
       xqqty.price      to 110  format "$>>,>>9.99"
       xqqty.uom        to 114
       xxx              to 126  format "$->>>,>>9.99"
    with frame item-hop no-box no-labels down width 132 STREAM-IO.


find first sman
    where sman.company eq cocode
      and sman.sman    eq xquo.sman
    no-lock no-error.
find first carrier
    where carrier.company eq cocode
      and carrier.carrier eq xquo.carrier
    no-lock no-error.
find first terms
    where terms.company eq cocode
      and terms.t-code  eq xquo.terms
    no-lock no-error.
find first cust
    where cust.company eq xquo.company
      and cust.cust-no eq xquo.cust-no
    no-lock no-error.
find first est
    where est.company eq xquo.company
      and est.est-no  eq xquo.est-no
    no-lock no-error.
find first eb
    where eb.company eq xquo.company
      and eb.est-no  eq xquo.est-no
    no-lock no-error.

do i = 1 to 4:
  assign
   v-sold[i] = xquo.soldto[i]
   v-ship[i] = xquo.shipto[i].
end.

if xquo.shipto[1] eq xquo.soldto[1] and
   xquo.shipto[2] eq xquo.soldto[2] and
   xquo.shipto[3] eq xquo.soldto[3] and
   xquo.shipto[4] eq xquo.soldto[4] then
  assign
   v-ship[1] = "SAME"
   v-ship[2] = ""
   v-ship[3] = ""
   v-ship[4] = "".

assign
 v-fob     = if cust.fob-code eq "ORIG" then "Origin" else "Destination"
 v-frt-pay = if avail eb then
	       if eb.chg-method eq "P" then "Prepaid" else
	       if eb.chg-method eq "C" then "Collect" else "Prepay & Add"
	     else "Bill".

format header
       skip(1)
       v-sold[1]        at 4    format "x(30)"
       v-ship[1]        at 37   format "x(30)"
       v-fob            at 69
       v-frt-pay        at 84
       xquo.q-no        at 101  format ">>>>9"
       xquo.quo-date    at 117
       v-sold[2]        at 4    format "x(30)"
       v-ship[2]        at 37   format "x(30)"
       v-sold[3]        at 4    format "x(30)"
       v-ship[3]        at 37   format "x(30)"
       terms.dscr       at 69   format "x(24)"
       trim(string(cust.over-pct,">9.9<")) + "-" +
       trim(string(cust.under-pct,">9.9<"))
			at 117  format "x(11)"
       v-sold[4]        at 4    format "x(30)"
       v-ship[4]        at 37   format "x(30)"
       skip
       carrier.dscr     at 69   format "x(20)"
       sman.sname       at 101  format "x(14)"
       string(trim(string(page-number - v-last-page,">9")) + " OF " +
	      trim(string(v-page-tot,">9")))
			at 117 format "x(8)"
       skip(3)

      with frame quote width 132 no-labels no-box page-top STREAM-IO.


output stream last-page to value(tmp-dir + "quohop.txt") page-size 37.

view frame quote.
view stream last-page frame quote.

if not ch-multi then do:
  {cec/quote/quohop.i "stream last-page"}

  v-page-tot = page-number (last-page).

  {cec/quote/quohop.i}
end.

else
do while true:
  find first report
      where report.term-id eq v-term-id
	    and report.key-01  gt v-cust
      no-lock no-error.

  if avail report then do:
    v-cust = report.key-01.

    for each report
        where report.term-id eq v-term-id
          and report.key-01  eq v-cust
        no-lock,
        first xquo where recid(xquo) eq report.rec-id no-lock
        break by report.key-01
              by report.key-02
              by report.key-03:

      find first sman
	      where sman.company eq cocode
	        and sman.sman    eq xquo.sman
	      no-lock no-error.
      find first carrier
	      where carrier.company eq cocode
	        and carrier.carrier eq xquo.carrier
	      no-lock no-error.
      find first terms
	      where terms.company eq cocode
	        and terms.t-code  eq xquo.terms
	      no-lock no-error.
      find first cust
	      where cust.company eq xquo.company
	        and cust.cust-no eq xquo.cust-no
	      no-lock no-error.
      find first est
          where est.company eq xquo.company
            and est.est-no  eq xquo.est-no
          no-lock no-error.
      find first eb
          where eb.company eq xquo.company
            and eb.est-no  eq xquo.est-no
          no-lock no-error.

      do i = 1 to 4:
	    assign
	     v-sold[i] = xquo.soldto[i]
	     v-ship[i] = xquo.shipto[i].
      end.

      if xquo.shipto[1] eq xquo.soldto[1] and
	     xquo.shipto[2] eq xquo.soldto[2] and
	     xquo.shipto[3] eq xquo.soldto[3] and
	     xquo.shipto[4] eq xquo.soldto[4] then
	    assign
	     v-ship[1] = "SAME"
	     v-ship[2] = ""
	     v-ship[3] = ""
	     v-ship[4] = "".

      v-fob = if avail eb then
		if eb.chg-method eq "P" then "Prepaid" else "Collect"
	      else "Bill".

      if first(report.key-01) OR s-sep-page then page stream last-page.

      {cec/quote/quohop.i "stream last-page"}
    end.

    v-page-tot = page-number (last-page) - v-last-page.

    for each report
        where report.term-id eq v-term-id
          and report.key-01  eq v-cust
        no-lock,
        first xquo where recid(xquo) eq report.rec-id no-lock
        break by report.key-01
              by report.key-02
              by report.key-03:

      find first sman
	      where sman.company eq cocode
	        and sman.sman    eq xquo.sman
	      no-lock no-error.
      find first carrier
	      where carrier.company eq cocode
	        and carrier.carrier eq xquo.carrier
	      no-lock no-error.
      find first terms
	      where terms.company eq cocode
	        and terms.t-code  eq xquo.terms
	      no-lock no-error.
      find first cust
	      where cust.company eq xquo.company
	        and cust.cust-no eq xquo.cust-no
	      no-lock no-error.
      find first est
          where est.company eq xquo.company
            and est.est-no  eq xquo.est-no
          no-lock no-error.
      find first eb
          where eb.company eq xquo.company
            and eb.est-no  eq xquo.est-no
          no-lock no-error.

      do i = 1 to 4:
	    assign
	     v-sold[i] = xquo.soldto[i]
	     v-ship[i] = xquo.shipto[i].
      end.

      if xquo.shipto[1] eq xquo.soldto[1] and
	     xquo.shipto[2] eq xquo.soldto[2] and
	     xquo.shipto[3] eq xquo.soldto[3] and
	     xquo.shipto[4] eq xquo.soldto[4] then
	    assign
	     v-ship[1] = "SAME"
	     v-ship[2] = ""
	     v-ship[3] = ""
	     v-ship[4] = "".

      v-fob = if avail eb then
		if eb.chg-method eq "P" then "Prepaid" else "Collect"
	      else "Bill".

      if first(report.key-01) OR s-sep-page then page.

      {cec/quote/quohop.i}
    end.
  end.

  else leave.
end.

output stream last-page close.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
