/* ---------------------------------------------- oe/rep/rel-more.i 09/98 JLF */
/* Print Multiple Release/Picking tickets per header                          */
/* -------------------------------------------------------------------------- */

for each report
    where report.term-id eq v-term
      and report.key-01  eq "2"
      and report.key-02  eq b-report.key-02
      and report.key-03  eq b-report.key-03
      and report.key-04  eq b-report.key-04
      and report.key-05  eq b-report.key-05,

    first oe-rell where recid(oe-rell) eq report.rec-id,

    first oe-relh where oe-relh.r-no eq oe-rell.r-no,

    first oe-ordl
    where oe-ordl.company eq cocode
      and oe-ordl.ord-no  eq oe-rell.ord-no
      and oe-ordl.i-no    eq oe-rell.i-no
      and oe-ordl.line    eq oe-rell.line
    no-lock,

    first oe-ord
    where oe-ord.company eq cocode
      and oe-ord.ord-no  eq oe-rell.ord-no
    no-lock:

  find first eb
      where eb.company  eq oe-ordl.company
	    and eb.est-no   eq oe-ordl.est-no
	    and eb.form-no  eq oe-ordl.form-no
	    and eb.blank-no eq oe-ordl.blank-no
      no-lock no-error.

  assign
   v-partial  = if oe-rell.partial eq 0 then
		  oe-rell.qty - (oe-rell.cases * oe-rell.qty-case)
		else oe-rell.partial
   v-cases    = oe-rell.cases + (if v-partial gt 0 then 1 else 0)
   v-case     = (if oe-rell.cases eq 0 then "" else
		   trim(string(oe-rell.cases,"->>>>>9")) + " @ " +
		   trim(string(oe-rell.qty-case,"->>>>>9"))) +
		(if v-partial eq 0 then "" else
		  ("  1 @ " + trim(string(v-partial,"->>>>>9")))).

  if "{1}" eq "stream last-page" then
    assign
     v-salesman = oe-ord.sman[1] +
		  (if oe-ord.sman[2] eq "" then "" else ", " + oe-ord.sman[2]) +
		  (if oe-ord.sman[3] eq "" then "" else ", " + oe-ord.sman[1])
     v-weight   = v-weight +
		  (if oe-ordl.t-weight ne ? then
		     oe-ordl.t-weight / oe-ordl.qty * oe-rell.qty else 0)
     v-pallets  = v-pallets +
		  if avail eb then round((v-cases / eb.cas-pal) + .49,0) else 1.

  display {1}
	  oe-rell.qty                       to 9    format ">,>>>,>>9"
	  oe-rell.i-no                      at 13
	  v-case                            at 44
	  report.key-06                     at 65   format "x(15)"
	  oe-rell.qty + v-partial ge int(oe-ordl.qty)
					                    at 80   format "C/P"
	  oe-ordl.i-name                    at 13
	  skip(1)

      with down no-box no-labels STREAM-IO width 85.

  if "{1}" eq "" then do:
    assign
     oe-rell.printed = yes.
  RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).

    delete report.
  end.
end. /* for each report */

