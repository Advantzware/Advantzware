/* -------------------------------------------------- ce/pr4-adh2.i 08/96 JLF */

for each glu break by glu.i-code with frame abc down no-labels no-box:

   if not first-of(glu.i-code) then next.
   find first item where item.company = cocode and
			 item.i-no    = glu.i-code no-lock no-error.
   if available item then find first e-item of item no-lock no-error.
   else next.
   ASSIGN
   gqty  = 0
   gcost = 0.
   for each b-glu where b-glu.i-code = glu.i-code:
      gqty = gqty + b-glu.i-qty.   /* total pounds */
   end.

   if item.mat-type eq "T" then do: {sys/inc/roundup.i gqty} end.

   vuom = item.cons-uom.

   IF AVAIL e-item THEN
   DO:
      EMPTY TEMP-TABLE tt-ei.
      CREATE tt-ei.
      DO j = 1 TO 10:
         ASSIGN
            tt-ei.run-qty[j] = e-item.run-qty[j]
            tt-ei.run-cost[j] = e-item.run-cost[j].
      END.
      
            
         DO j = 1 TO 10:
            ASSIGN
               tt-ei.run-qty[j + 10] = e-item.runQty[j]
               tt-ei.run-cost[j + 10] = e-item.runCost[j].
         END.
      
      DO j = 1 TO 20:
         IF tt-ei.run-qty[j] LT gqty THEN NEXT.
            gcost = gqty * tt-ei.run-cost[j].
            IF e-item.std-uom NE "" THEN vuom = e-item.std-uom.
         LEAVE.
      END.
   END.

   ELSE gcost = gqty * IF ce-ctrl.r-cost THEN item.avg-cost
                                         ELSE item.last-cost.

   ASSIGN
   dm-tot[4] = dm-tot[4] + (gcost / (qty / 1000))
   dm-tot[5] = dm-tot[5] + gcost.

   /* rm handling chg per cwt */
   IF xeb.pur-man THEN
     IF rm-rate-f NE 0 THEN ctrl2[3] = ctrl2[3] + ((gqty / 100) * rm-rate-f).
     ELSE.
   ELSE
     IF ctrl[3] NE 0 THEN ctrl2[3] = ctrl2[3] + ((gqty / 100) * ctrl[3]).

   /* rm handling pct. */
   IF xeb.pur-man THEN
     IF hand-pct-f NE 0 THEN ctrl2[2] = ctrl2[2] + (gcost * hand-pct-f).
     ELSE.
   ELSE
     IF ctrl[2] NE 0 THEN ctrl2[2] = ctrl2[2] + (gcost * ctrl[2]).

   vqty = string(gqty,">>>>>9.99").

   if item.mat-type eq "T"                     and
      substr(vqty,length(vqty) - 2,3) eq ".00" then
     substr(vqty,length(vqty) - 2,3) = "".

   vqty = fill(" ",9 - length(trim(vqty))) + trim(vqty).

/* end ---------------------------------- copr. 1996  advanced software, inc. */
