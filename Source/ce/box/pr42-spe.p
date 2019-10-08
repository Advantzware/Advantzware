/* ------------------------------------------------ ce/box/pr42-spe.p 2/93 cd */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{ce/print4.i shared shared}

DEF SHARED VAR qty AS INT NO-UNDO.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-setup FOR reftable.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

for each xef where xef.company = xest.company
               AND xef.est-no eq xest.est-no:

  {ce/pr4-spe.i qty}

	 for each blk where blk.snum = xef.form-no:
        READKEY PAUSE 0. 
	    blk.cost = blk.cost + (s-cost[i] * blk.pct).
	 end.

	 display string(xef.form-no,"9") + "-00" format "x(4)"
		 xef.spec-dscr[i] s-qty[i]  to 50 space(1)
		 e-item.std-uom  when avail item
             item.cons-uom when not avail e-item @ e-item.std-uom
		 s-cost[i] / (t-blkqty[xef.form-no] / 1000) to 69
		 s-cost[i] format ">>>>,>>9.99" to 80 SKIP
         WITH STREAM-IO.
	 lin-count = lin-count + 1.
      end.
   end.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
