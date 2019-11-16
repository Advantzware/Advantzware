/* ---------------------------------------------- ce/tan/pr4-spe.p 4/93 cd */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
DEF SHARED VAR qty AS INT NO-UNDO.
{ce/print4.i shared shared}

DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-setup FOR reftable.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

{ce/pr4-spe.i qty}

    display xef.spec-dscr[i] s-qty[i]  to 46 space(1)
	    e-item.std-uom when available e-item
	    item.cons-uom when not available e-item @ e-item.std-uom
	    s-cost[i] / (qty / 1000) to 69
        s-cost[i] format ">>>>,>>9.99" to 80 SKIP  WITH STREAM-IO.
  end.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
