/* -------------------------------------------------- ce/pr4-ink2.i 08/96 JLF */

for each ink break by ink.i-dscr by ink.i-code
    with frame ab down no-labels no-box:
   if not first-of(ink.i-code) then next.
   find first item where item.company = cocode and
			 item.i-no    = ink.i-code no-lock no-error.
   if available item then find first e-item of item no-lock no-error.
   else next.
   iqty  = 0.
   icost = 0.
   for each b-ink
       where b-ink.i-code eq ink.i-code
         and b-ink.i-dscr eq ink.i-dscr:
      iqty = iqty + b-ink.i-qty.
   end.
   if iqty < item.min-lbs then iqty = item.min-lbs.

   {est/matcost.i iqty icost ink}

   ASSIGN
    icost     = (icost * iqty) + lv-setup-ink
    dm-tot[4] = dm-tot[4] + (icost / (qty / 1000)).
    dm-tot[5] = dm-tot[5] + icost.

   /* rm handling chg per cwt */
   IF xeb.pur-man THEN
     IF rm-rate-f NE 0 THEN ctrl2[3] = ctrl2[3] + ((iqty / 100) * rm-rate-f).
     ELSE.
   ELSE
     IF ctrl[3] NE 0 THEN ctrl2[3] = ctrl2[3] + ((iqty / 100) * ctrl[3]).

   /* rm handling pct. */
   IF xeb.pur-man THEN
     IF hand-pct-f NE 0 THEN ctrl2[2] = ctrl2[2] + (icost * hand-pct-f).
     ELSE.
   ELSE
     IF ctrl[2] NE 0 THEN ctrl2[2] = ctrl2[2] + (icost * ctrl[2]).

/* end ---------------------------------- copr. 1996  advanced software, inc. */
