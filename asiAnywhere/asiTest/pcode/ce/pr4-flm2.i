/* -------------------------------------------------- ce/pr4-flm2.i 08/96 JLF */

for each flm by flm.snum by flm.bnum with frame ab down no-labels no-box:

   find first est-flm
       where est-flm.company = xest.company AND est-flm.est-no eq xest.est-no
	   and est-flm.snum  eq flm.snum
	   and est-flm.bnum  eq flm.bnum
       no-lock no-error.
   find first item
       where item.company eq cocode
	 and item.i-no    eq flm.i-no
       no-lock no-error.
   if available item then find first e-item of item no-lock no-error.
   else next.
   fqty  = 0.
   fcost = 0.
   for each b-flm where b-flm.i-no = flm.i-no:
      fqty = fqty + b-flm.qty.
   end.

   {est/matcost.i fqty fcost film}

   assign
    fcost     = (fcost * flm.qty) + lv-setup-film
    flm.cost  = fcost
    flm.cosm  = flm.cost / (qty / 1000)
    dm-tot[4] = dm-tot[4] + flm.cosm
    dm-tot[5] = dm-tot[5] + flm.cost.

   /* rm handling chg per cwt */
   IF xeb.pur-man THEN
     IF rm-rate-f NE 0 THEN ctrl2[3] = ctrl2[3] + ((flm.qty / 100) * rm-rate-f).
     ELSE.
   ELSE
     IF ctrl[3] NE 0 THEN ctrl2[3] = ctrl2[3] + ((flm.qty / 100) * ctrl[3]).

   /* rm handling pct. */
   IF xeb.pur-man THEN
     IF hand-pct-f NE 0 THEN ctrl2[2] = ctrl2[2] + (fcost * hand-pct-f).
     ELSE.
   ELSE
     IF ctrl[2] NE 0 THEN ctrl2[2] = ctrl2[2] + (fcost * ctrl[2]).

/* end ---------------------------------- copr. 1996  advanced software, inc. */
