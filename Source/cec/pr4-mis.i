/* -------------------------------------------------- cec/pr4-mis.i 07/96 JLF */

   do i = 1 to 6 with frame ad2 down no-labels no-box:
    /* only (i)ntegrate and (m)aintenance lines are done here */
    if index("SON",xef.mis-simon[i]) > 0 then next.
    if mis-cost[i] ne "" then do:
     qty = 0.
     FOR EACH xeb FIELDS(yld-qty)
         WHERE xeb.company      EQ xef.company
           AND xeb.est-no       EQ xef.est-no
           AND xeb.form-no      EQ xef.form-no
           AND (xeb.blank-no    EQ xef.mis-bnum[i] OR
                xef.mis-bnum[i] EQ 0)
         NO-LOCK:
       ASSIGN
        v-yld = IF xeb.yld-qty LT 0 THEN -1 / xeb.yld-qty ELSE xeb.yld-qty
        qty   = qty + (v-qty * v-yld).
     END.

	 {cec/refest5a.i MAT-QTY i "no-lock no-error"}

	 if avail reftable then do v = 1 to EXTENT(reftable.val):
	   if qty le reftable.val[v] then leave.
	   if v = EXTENT(reftable.val) then do:
	     v = 0.
	     release reftable.
	     leave.
	   end.
	 end.

	 if avail reftable then
	   {cec/refest5a.i MAT-CST i "no-lock no-error"}

     v-mat-cost = if avail reftable then reftable.val[v] else 0.

     IF ceprepprice-chr EQ "Profit" THEN
        mis-tot[5] = (xef.mis-matf[i] + (v-mat-cost * qty / 1000)) /
   	        		 (1 - (xef.mis-mkup[i] / 100)).
     ELSE
        mis-tot[5] = (xef.mis-matf[i] + (v-mat-cost * qty / 1000)) *
   	     		     (1 + (xef.mis-mkup[i] / 100)).

	 {cec/refest5a.i LAB-QTY i "no-lock no-error"}

	 if avail reftable then do v = 1 to EXTENT(reftable.val):
	   if qty le reftable.val[v] then leave.
	   if v = EXTENT(reftable.val) then do:
	     v = 0.
	     release reftable.
	     leave.
	   end.
	 end.

	 if avail reftable then
	   {cec/refest5a.i LAB-CST i "no-lock no-error"}

     v-lab-cost = if avail reftable then reftable.val[v] else 0.

     IF ceprepprice-chr EQ "Profit" THEN
	    mis-tot[6] = (xef.mis-labf[i] + (v-lab-cost * qty / 1000)) /
	        		 (1 - (xef.mis-mkup[i] / 100)).
     ELSE
        mis-tot[6] = (xef.mis-labf[i] + (v-lab-cost * qty / 1000)) *
	       			 (1 + (xef.mis-mkup[i] / 100)).

     prep-tot = mis-tot[5] + mis-tot[6].

     IF ceprep-cha EQ "FiveDollar" AND
        prep-tot ne 0 THEN DO:
        ld-fac = prep-tot.
        {sys/inc/roundupfive.i prep-tot}
        ASSIGN
           ld-fac = prep-tot / ld-fac
           mis-tot[5] = mis-tot[5] * ld-fac
           mis-tot[6] = mis-tot[6] * ld-fac.
     END.

	 if mis-tot[5] ne 0 then do:
	   create xprep.
	   assign xprep.frm      = xef.mis-snum[i]
		  xprep.blank-no = xef.mis-bnum[i]
		  xprep.qty      = 1
		  xprep.std-cost = mis-tot[5]
		  xprep.ml       = yes
		  xprep.cost-m   = mis-tot[5] / (qty / 1000)
		  xprep.simon    = xef.mis-simon[i]
		  xprep.code     = "MISM" + string(i,"9").
	 end.

	 if mis-tot[6] ne 0 then do:
	   create xprep.
	   assign xprep.frm      = xef.mis-snum[i]
		  xprep.blank-no = xef.mis-bnum[i]
		  xprep.qty      = 1
		  xprep.std-cost = mis-tot[6]
		  xprep.ml       = no
		  xprep.cost-m   = mis-tot[6] / (qty / 1000)
		  xprep.simon    = xef.mis-simon[i]
		  xprep.code     = "MISL" + string(i,"9").
	 end.

     ASSIGN
	    mis-tot[1] = mis-tot[1] + mis-tot[5]
	    mis-tot[2] = mis-tot[2] + (mis-tot[5] / (qty / 1000))
	    mis-tot[3] = mis-tot[3] + mis-tot[6]
	    mis-tot[4] = mis-tot[4] + (mis-tot[6] / (qty / 1000)).

/* end ---------------------------------- copr. 1996  advanced software, inc. */
