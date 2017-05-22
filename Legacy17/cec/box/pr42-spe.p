/* ----------------------------------------------- cec/box/pr42-spe.p 2/93 cd */

def input parameter v-vend-no like e-item-vend.vend-no.

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def var i as int no-undo.
def var j as int no-undo.
def shared var qty as int NO-UNDO.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-setup FOR reftable.

{cec/print4.i shared shared}
{cec/print42.i shared}

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

for each xef where xef.company = xest.company
               and xef.est-no    eq xest.est-no
               and (xef.form-no eq v-form-no or (not vmclean2)):

   {cec/pr4-spe.i tt-blk}

         for each blk where blk.snum = xef.form-no:
            blk.cost = blk.cost + (s-cost[i] * blk.pct).
         end.

         display string(xef.form-no,"99") + "-0" format "x(4)"
                 xef.spec-dscr[i] s-qty[i] to 50 space(1)
                 e-item.std-uom when available e-item
                 item.cons-uom when not available e-item @ e-item.std-uom
                 lv-setup-spe when lv-setup-spe ne 0 format ">>>9.99" to 63
                 s-cost[i] / (t-blkqty[xef.form-no] / 1000) format ">>>>9.99" to 71
                 s-cost[i] to 80 format ">>>>>9.99" skip with stream-io.
         lin-count = lin-count + 1.
      end.
   end.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
