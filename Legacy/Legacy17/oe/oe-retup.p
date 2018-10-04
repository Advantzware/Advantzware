/* -------------------------------------------------- oe/oe-retup.p 11/97 JLF */
/* update return from current return lines                                    */
/* -------------------------------------------------------------------------- */

def input parameter rec-id as recid.

{sys/inc/var.i shared}

def var v-tax-rate           as   dec format ">,>>9.99<<<".
def var v-frt-tax-rate       like v-tax-rate.
def var v-line               like oe-retl.line.
def var v-fac                as   int.

{sys/form/s-top.f}


find oe-reth where recid(oe-reth) eq rec-id.

find first ar-inv where ar-inv.company eq oe-reth.company
           and ar-inv.cust-no eq oe-reth.cust-no
           and ar-inv.inv-no  eq oe-reth.inv-no
           use-index ar-inv NO-LOCK NO-ERROR.

IF NOT AVAIL ar-inv THEN RETURN.

run ar/cctaxrt.p (input cocode, ar-inv.tax-code,
		          output v-tax-rate, output v-frt-tax-rate).

assign
 oe-reth.tot-qty-return = 0
 oe-reth.qty-return-inv = 0
 oe-reth.tot-cost       = 0
 oe-reth.tot-return-amt = 0
 oe-reth.tot-tax        = 0
 v-line                 = 0.

for each oe-retl
    where oe-retl.company eq oe-reth.company
      and oe-retl.r-no    eq oe-reth.r-no
    use-index r-no:

  find first ar-invl
      where ar-invl.company eq oe-retl.company
	and ar-invl.i-no    eq oe-retl.i-no
	and ar-invl.x-no    eq ar-inv.x-no
      use-index i-no no-lock no-error.

  find first itemfg
      {sys/look/itemfgrlW.i}
	and itemfg.i-no eq oe-retl.i-no
      no-lock no-error.

  v-fac = if oe-retl.uom       eq "CS" and
	     avail itemfg              and
	     itemfg.case-count ne 0    then itemfg.case-count
	  else if oe-retl.uom eq "C"   then 100
	  else if oe-retl.uom eq "M"   then 1000 else 1.

  if avail ar-invl and ar-invl.tax and v-tax-rate gt 0 then
    oe-reth.tot-tax = oe-reth.tot-tax + (v-tax-rate / 100 *
		 round((ar-invl.unit-pr * (oe-retl.tot-qty-return / v-fac)
		  * if available ar-invl
		  then ((100 - ar-invl.disc) / 100) else 1),2)).

  assign
   oe-reth.tot-qty-return = oe-reth.tot-qty-return + oe-retl.tot-qty-return
   oe-reth.qty-return-inv = oe-reth.qty-return-inv + oe-retl.qty-return-inv
   oe-reth.tot-cost       = oe-reth.tot-cost +
		  round((oe-retl.cost * (oe-retl.tot-qty-return / 1000)),2)
   oe-reth.tot-return-amt = oe-reth.tot-return-amt +
		  round((ar-invl.unit-pr * (oe-retl.tot-qty-return / v-fac)
		  * if available ar-invl
		  then ((100 - ar-invl.disc) / 100) else 1),2)
   v-line                 = v-line + 1
   oe-retl.line           = v-line.
end.

assign
 oe-reth.tot-tax        = oe-reth.tot-tax +
			  round((oe-reth.tot-freight * v-frt-tax-rate) / 100,2)

 oe-reth.tot-return-amt = oe-reth.tot-return-amt +
			  oe-reth.tot-tax + oe-reth.tot-freight.

/* end ---------------------------------- copr. 1996  advanced software, inc. */

