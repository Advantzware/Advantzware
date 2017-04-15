/* ------------------------------------------------ cec/pr4-spe.p 8/92 cd */

def input parameter v-vend-no like e-item-vend.vend-no.

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def var i as int no-undo.
def var j as int no-undo.
def shared var qty as INT NO-UNDO .

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-setup FOR reftable.

{cec/print4.i shared shared}

{cec/msfcalc.i}

{cec/rollfac.i}

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

   {cec/pr4-spe.i qty}

	 dm-tot[4] = dm-tot[4] + (s-cost[i] / (qty / 1000)).
	 brd.cost-m = brd.cost-m / v-sqft-fac.

	 display xef.spec-dscr[i]
		 s-qty[i]  to 48 space(1)
		 e-item.std-uom when available e-item
		 item.cons-uom when not available e-item @ e-item.std-uom
		 lv-setup-spe when lv-setup-spe ne 0 format ">>>9.99" to 59
         s-cost[i] / (qty / 1000) / v-sqft-fac format ">>>>9.99" to 68
		 s-cost[i] format ">>>>,>>9.99" to 80 skip
         with stream-io.
      end.
   end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

