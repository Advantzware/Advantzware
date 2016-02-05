/* -------------------------------------------------- oe/invlpost.i 7/93 rd  */
/* o/e invoicing lines history assigment of fields                           */
/* -------------------------------------------------------------------------- */

FIND FIRST oe-bolh 
    WHERE oe-bolh.b-no EQ inv-line.b-no 
    NO-LOCK NO-ERROR.
FIND FIRST oe-boll 
    WHERE oe-boll.company EQ oe-bolh.company
      AND oe-boll.b-no EQ oe-bolh.b-no
      AND oe-boll.i-no EQ inv-line.i-no 
    NO-LOCK NO-ERROR.

assign
 ar-invl.x-no       = v-xno
 ar-invl.actnum     = if avail fgcat and fgcat.glacc ne ""
                      then fgcat.glacc else v-ar-sales
 ar-invl.inv-no     = inv-head.inv-no
 ar-invl.bol-no     = IF AVAIL oe-bolh THEN oe-bolh.bol-no ELSE inv-head.bol-no
 ar-invl.b-no       = inv-line.b-no
 ar-invl.company    = inv-line.company
 ar-invl.ord-no     = inv-line.ord-no
 ar-invl.cust-no    = inv-line.cust-no
 v-xline            = v-xline + 1
 ar-invl.line       = v-xline
 ar-invl.est-no     = inv-line.est-no
 ar-invl.est-type   = inv-line.est-type
 ar-invl.form-no    = inv-line.form-no
 ar-invl.blank-no   = inv-line.blank-no
 ar-invl.job-no     = inv-line.job-no
 ar-invl.job-no2    = inv-line.job-no2
 ar-invl.part-no    = inv-line.part-no
 ar-invl.i-no       = inv-line.i-no
 ar-invl.i-name     = inv-line.i-name
 ar-invl.i-dscr     = inv-line.i-dscr
 ar-invl.po-no      = inv-line.po-no
 ar-invl.req-code   = inv-line.req-code
 ar-invl.req-date   = inv-line.req-date
 ar-invl.prom-code  = inv-line.prom-code
 ar-invl.prom-date  = inv-line.prom-date
 ar-invl.part-dscr1 = inv-line.part-dscr1
 ar-invl.part-dscr2 = inv-line.part-dscr2
 ar-invl.po-no-po   = inv-line.po-no-po
 ar-invl.cas-cnt    = inv-line.cas-cnt
 ar-invl.pr-uom     = inv-line.pr-uom
 ar-invl.unit-pr    = inv-line.price
 ar-invl.tax        = inv-line.tax
 ar-invl.disc       = inv-line.disc
 ar-invl.amt        = inv-line.t-price   /* total price of invoiced item */
 ar-invl.t-weight   = inv-line.t-weight  /* total weight of invoiced item */
 ar-invl.t-freight  = inv-line.t-freight /* total freight of invoiced item */
 ar-invl.ship-qty   = inv-line.ship-qty
 ar-invl.inv-qty    = inv-line.inv-qty
 ar-invl.qty        = inv-line.qty
 ar-invl.spare-dec-1 = itemfg.spare-dec-1 /* Full Cost */
 ar-invl.sman[1]    = inv-line.sman[1]
 ar-invl.sman[2]    = inv-line.sman[2]
 ar-invl.sman[3]    = inv-line.sman[3]
 ar-invl.s-pct[1]   = inv-line.s-pct[1]
 ar-invl.s-pct[2]   = inv-line.s-pct[2]
 ar-invl.s-pct[3]   = inv-line.s-pct[3]
 ar-invl.s-comm[1]  = inv-line.s-comm[1]
 ar-invl.s-comm[2]  = inv-line.s-comm[2]
 ar-invl.s-comm[3]  = inv-line.s-comm[3]
 ar-invl.sname[1]   = inv-line.sname[1]
 ar-invl.sname[2]   = inv-line.sname[2]
 ar-invl.sname[3]   = inv-line.sname[3]
 ar-invl.s-commbasis[1] = inv-line.s-commbasis[1]
 ar-invl.s-commbasis[2] = inv-line.s-commbasis[2]
 ar-invl.s-commbasis[3] = inv-line.s-commbasis[3]
 ar-invl.misc       = no
 ar-invl.posted     = yes
 ar-invl.pr-qty-uom = inv-line.pr-uom
 ar-invl.cost       = inv-line.cost
 ar-invl.t-cost     = ar-invl.cost * (ar-invl.inv-qty / 1000)
 ar-invl.dscr[1]    = "M"
 ar-invl.std-tot-cost = inv-line.cost
 ar-invl.std-lab-cost = v-cost[1]
 ar-invl.std-fix-cost = v-cost[2]
 ar-invl.std-var-cost = v-cost[3]
 ar-invl.std-mat-cost = v-cost[4]
 ar-invl.loc          = IF AVAIL oe-boll THEN oe-boll.loc ELSE ""
 ar-invl.lot-no       = inv-line.lot-no.

 
if ar-invl.ord-no eq 0 then ar-invl.s-pct[1] = 100. 
 
if avail oe-ordl then
  assign
   oe-ordl.t-ship-qty = oe-ordl.t-ship-qty + inv-line.ship-qty
   oe-ordl.t-inv-qty  = oe-ordl.t-inv-qty + inv-line.inv-qty.
   
