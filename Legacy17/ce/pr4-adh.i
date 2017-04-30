/* --------------------------------------------------- ce/pr4-adh.i 08/96 JLF */

find first item
    where item.company eq cocode
      and item.loc     eq locode
      and item.i-no    eq xeb.adhesive
    no-lock no-error.

if xeb.adhesive ne ""                     and
   avail item                             and
   (xeb.lin-in ne 0 or item.sqin-lb ne 0) then do:

   find first est-op
       where est-op.company = xest.company 
         AND est-op.est-no eq xest.est-no
         and (est-op.qty  eq v-op-qty OR xest.est-type NE 1)
         and est-op.line  gt 500
         and est-op.s-num eq xef.form-no
         and est-op.b-num eq xeb.blank-no
         and est-op.dept  eq (if item.mat-type eq "G" then "GL" else "QS")
     no-lock no-error.

   if avail est-op then do:
      g-qty = 0.

      if item.linin-lb ne 0 and xeb.lin-in ne 0 then
        g-qty = xeb.lin-in * (IF item.mat-type EQ "T" THEN 4 ELSE 1) / item.linin-lb.
      else
      if item.sqin-lb ne 0 then
        g-qty = (xeb.t-len * xeb.t-wid) / item.sqin-lb.

      g-qty = g-qty * est-op.num-sh *
                      v-num-up * (if xef.n-out   eq 0 then 1 else xef.n-out) *
                                 (if xef.n-out-l eq 0 then 1 else xef.n-out-l).

      find first glu where glu.i-code = item.i-no    and
                           glu.snum   = xeb.form-no  and
                           glu.bnum   = xeb.blank-no no-error.
      if not available glu then do:
         create glu.
         assign
            glu.id     = xeb.part-no
            glu.snum   = xeb.form-no
            glu.bnum   = xeb.blank-no
            glu.i-code = xeb.adhesive
            glu.i-dscr = item.est-dscr.
      end.
      glu.i-qty = glu.i-qty + g-qty.
   end.
end.

if xef.adh-code ne "" and v-1st-frm then do:
   find first item where item.company = cocode and item.loc = locode and
                         item.i-no    = xef.adh-code no-lock no-error.
   if available item then find first e-item of item no-lock no-error.
   find first est-op
       where est-op.company = xest.company
         AND est-op.est-no eq xest.est-no
         and (est-op.qty  eq v-op-qty OR xest.est-type NE 1)
         and est-op.line  gt 500
         and est-op.s-num eq xef.form-no
         and est-op.dept  eq "LM"
       no-lock no-error.
   if not available item or not available est-op or
   ((adh-qty[1] + adh-qty[2] + adh-qty[3]) = 0)
   then leave.
   
/*    g-qty = ((adh-qty[1] + adh-qty[2] + adh-qty[3]) * xef.adh-sqin * */
/*              est-op.num-sh) .                                       */
/*                                                                     */
   /*Adhesive only applied to one side*/
   g-qty = (xef.adh-sqin * est-op.num-sh).

   /*adjust material coverage rate based on flute - evened out per PackRite 
     (code left in in the event that changes) */
   IF xef.trim-pen = 125 THEN
         g-qty = g-qty / (item.sqin-lb * 1).
   ELSE IF xef.trim-pen = 78 THEN
         g-qty = g-qty / (item.sqin-lb * 1).
   ELSE IF xef.trim-pen = 52 THEN
         g-qty = g-qty / (item.sqin-lb * 1).
   ELSE
         g-qty = g-qty / item.sqin-lb.
   
   find first glu where glu.i-code = item.i-no    and
                        glu.snum   = xeb.form-no  and
                        glu.bnum   = xeb.blank-no no-error.
   if not available glu then do:
      create glu.
      assign
         glu.id     = xeb.part-no
         glu.snum   = xeb.form-no
         glu.bnum   = xeb.blank-no
         glu.i-code = xef.adh-code
         glu.i-dscr = item.est-dscr.
   end.
   glu.i-qty = glu.i-qty + g-qty.
end.


if xef.lam-code ne "" and v-1st-frm then do:
   find first item where item.company = cocode and item.loc = locode and
                         item.i-no    = xef.lam-code no-lock no-error.
   if available item then find first e-item of item no-lock no-error.
   find first est-op
       where est-op.company = xest.company
         AND est-op.est-no eq xest.est-no
         and (est-op.qty  eq v-op-qty OR xest.est-type NE 1)
         and est-op.line  gt 500
         and est-op.s-num eq xef.form-no
         and est-op.dept  eq "LM"
       no-lock no-error.
   if not available item or not available est-op or
   ((adh-qty[1] + adh-qty[2] + adh-qty[3]) = 0)
   then leave.  
/*    g-qty = ((adh-qty[1] + adh-qty[2] + adh-qty[3]) * xef.adh-sqin * */
/*              est-op.num-sh) .                                       */
/*                                                                     */
   /*Laminate only applied to one side*/
   g-qty = (xef.adh-sqin * est-op.num-sh).

   /*adjust material coverage rate based on flute*/
   IF xef.trim-pen = 125 THEN
         g-qty = g-qty / (item.sqin-lb * 1.3636).
   ELSE IF xef.trim-pen = 78 THEN
         g-qty = g-qty / (item.sqin-lb * 1.2).
   ELSE IF xef.trim-pen = 52 THEN
         g-qty = g-qty / (item.sqin-lb * 1.1111).
   ELSE
         g-qty = g-qty / item.sqin-lb.
   
   find first glu where glu.i-code = item.i-no    and
                        glu.snum   = xeb.form-no  and
                        glu.bnum   = xeb.blank-no no-error.
   if not available glu then do:
      create glu.
      assign
         glu.id     = xeb.part-no
         glu.snum   = xeb.form-no
         glu.bnum   = xeb.blank-no
         glu.i-code = xef.lam-code
         glu.i-dscr = item.est-dscr.
   end.
   glu.i-qty = glu.i-qty + g-qty.
end.

v-1st-frm = no.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
