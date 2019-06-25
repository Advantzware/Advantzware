/* -------------------------------------------------- oe/invlpost.p 08/00 JLF */
/* set o/e invoice line cost                                                  */
/* -------------------------------------------------------------------------- */

def input  parameter v-rowid        as   ROWID.
def output parameter v-cost1        as   dec.
def output parameter v-cost2        as   dec.
def output parameter v-cost3        as   dec.
def output parameter v-cost4        as   dec.
def output parameter v-u-cost       like inv-line.cost.
def output parameter v-t-cost       like inv-line.t-cost.

{sys/inc/var.i shared}
{sys/form/s-top.f}

DEF VAR lv-b-no    LIKE inv-line.b-no NO-UNDO.
DEF VAR lv-ord-no  LIKE inv-line.ord-no NO-UNDO.
DEF VAR lv-i-no    LIKE inv-line.i-no NO-UNDO.
DEF VAR lv-po-no   LIKE inv-line.po-no NO-UNDO.
DEF VAR lv-job-no  LIKE inv-line.job-no NO-UNDO.
DEF VAR lv-job-no2 LIKE inv-line.job-no2 NO-UNDO.
DEF VAR lv-inv-qty LIKE inv-line.inv-qty NO-UNDO.

def var v-uom like itemfg.prod-uom.
def var v-qty like oe-boll.qty.
def var v-cost like inv-line.cost extent 4.
def var v-cost-m like inv-line.cost extent 4.


find inv-line where ROWID(inv-line) eq v-rowid no-lock no-error.

IF NOT AVAIL inv-line THEN
find ar-invl where ROWID(ar-invl) eq v-rowid no-lock no-error.

assign
 v-cost = 0
 v-qty  = 0.

IF AVAIL inv-line OR AVAIL ar-invl THEN DO:
  IF AVAIL inv-line THEN
    ASSIGN
     lv-b-no    = inv-line.b-no
     lv-ord-no  = inv-line.ord-no
     lv-i-no    = inv-line.i-no
     lv-po-no   = inv-line.po-no
     lv-job-no  = inv-line.job-no
     lv-job-no2 = inv-line.job-no2
     lv-inv-qty = inv-line.inv-qty.
  ELSE
    ASSIGN
     lv-b-no    = ar-invl.b-no
     lv-ord-no  = ar-invl.ord-no
     lv-i-no    = ar-invl.i-no
     lv-po-no   = ar-invl.po-no
     lv-job-no  = ar-invl.job-no
     lv-job-no2 = ar-invl.job-no2
     lv-inv-qty = ar-invl.inv-qty.

  RELEASE inv-line.
  RELEASE ar-invl.

  find first job-hdr
      where job-hdr.company eq cocode
        and job-hdr.job-no  eq lv-job-no
        and job-hdr.job-no2 eq lv-job-no2
        and job-hdr.i-no    eq lv-i-no
      use-index job-no no-lock no-error.

  find first itemfg
      where (itemfg.company  = cocode )
        and itemfg.i-no eq lv-i-no
      no-error.
      
  FOR EACH oe-boll
      WHERE oe-boll.company eq cocode
        AND oe-boll.b-no    eq lv-b-no
        AND oe-boll.ord-no  eq lv-ord-no
        AND oe-boll.i-no    eq lv-i-no
        AND oe-boll.po-no   eq lv-po-no
        AND oe-boll.qty     NE 0
      USE-INDEX b-no NO-LOCK:

    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq oe-boll.i-no
          and fg-bin.tag     eq oe-boll.tag
          and fg-bin.loc     eq oe-boll.loc
          and fg-bin.loc-bin eq oe-boll.loc-bin
          and fg-bin.job-no  eq oe-boll.job-no
          and fg-bin.job-no2 eq oe-boll.job-no2
        no-lock no-error.
                
    assign
     v-qty = v-qty + oe-boll.qty
     v-uom = "".
  
    if avail fg-bin and fg-bin.std-tot-cost ne 0 then
      assign
       v-cost-m[1] = fg-bin.std-lab-cost
       v-cost-m[2] = fg-bin.std-fix-cost
       v-cost-m[3] = fg-bin.std-var-cost
       v-cost-m[4] = fg-bin.std-mat-cost
       v-uom       = fg-bin.pur-uom.
       
    else
    if avail job-hdr and job-hdr.std-tot-cost ne 0 then
      assign
       v-cost-m[1] = job-hdr.std-lab-cost
       v-cost-m[2] = job-hdr.std-fix-cost
       v-cost-m[3] = job-hdr.std-var-cost
       v-cost-m[4] = job-hdr.std-mat-cost
       v-uom       = "M".
       
    else   
      assign
       v-cost-m[1] = itemfg.std-lab-cost
       v-cost-m[2] = itemfg.std-fix-cost
       v-cost-m[3] = itemfg.std-var-cost
       v-cost-m[4] = itemfg.std-mat-cost.

    if v-uom eq "" then v-uom = itemfg.prod-uom.

   do i = 1 to 4:
      if v-uom ne "M" then
        run sys/ref/convcuom.p(v-uom, "M", 0, 0, 0, 0,
                               v-cost-m[i], output v-cost-m[i]).
                                       
      v-cost[i] = v-cost[i] + (v-cost-m[i] * oe-boll.qty / 1000).
    end.                           
  end.
  
  do i = 1 to 4:
    v-cost[i] = v-cost[i] / (v-qty / 1000).
    
    if v-cost[i] eq ? then v-cost[i] = 0.
  end.
      
  assign
   v-cost1 = v-cost[1]
   v-cost2 = v-cost[2]
   v-cost3 = v-cost[3]
   v-cost4 = v-cost[4].
   
  v-u-cost = v-cost[1] + v-cost[2] + v-cost[3] + v-cost[4].
          
  if v-u-cost eq ? then v-u-cost = 0.
          
  v-t-cost = v-u-cost * lv-inv-qty / 1000.

  if v-t-cost eq ? then v-t-cost = 0.
end.  
          
 
