/* ------------------------------------------------- ce/com/pr4-spe.p 7/92 cd */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{cec/print4.i shared shared}

def var qm as DEC NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-setup FOR reftable.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

for each xef where xef.company = xest.company
               AND xef.est-no eq xest.est-no:
  qm = 0.
  for each bf-eb FIELDS(yrprice yld-qty bl-qty)
      where bf-eb.company = xef.company
        AND bf-eb.est-no  eq xef.est-no
        and bf-eb.form-no eq xef.form-no
      no-lock:
    qm = qm + (if bf-eb.yrprice then bf-eb.yld-qty else bf-eb.bl-qty).  
  end.
  qm = qm / 1000.
  
  {cec/pr4-spe.i t-blkqty[xef.form-no]}

      for each blk where blk.snum eq xef.form-no:
        blk.cost = blk.cost + (s-cost[i] * blk.pct).
      end.

      display string(xef.form-no,"9") + "-00" format "x(4)"
              xef.spec-dscr[i] s-qty[i]  to 50 space(1)
              e-item.std-uom  when avail item
              item.cons-uom when not avail e-item @ e-item.std-uom
              s-cost[i] / qm format ">>>>9.99" to 69
              s-cost[i] format ">>>>,>>9.99" to 80 SKIP WITH STREAM-IO.
      lin-count = lin-count + 1.
    end.
  end.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
