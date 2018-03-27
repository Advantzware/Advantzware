/* -------------------------------------------------- cec/pr4-mis.i 07/96 JLF */


   DO i = 1 TO 6 WITH FRAME ad2 DOWN NO-LABELS NO-BOX:
    /* only (i)ntegrate and (m)aintenance lines are done here */
    IF INDEX("SON",xef.mis-simon[i]) > 0 THEN NEXT.
    IF mis-cost[i] NE "" THEN DO:
     qty = 0.
     FOR EACH xeb FIELDS(quantityPerSet)
         WHERE xeb.company      EQ xef.company
           AND xeb.est-no       EQ xef.est-no
           AND xeb.form-no      EQ xef.form-no
           AND (xeb.blank-no    EQ xef.mis-bnum[i] OR
                xef.mis-bnum[i] EQ 0)
         NO-LOCK:
       ASSIGN
        v-yld = IF xeb.quantityPerSet LT 0 THEN -1 / xeb.quantityPerSet ELSE xeb.quantityPerSet
        qty   = qty + (v-qty * v-yld).
     END.

	 {cec/refest5a.i MAT-QTY i "no-lock no-error"}

	 IF AVAIL reftable THEN DO v = 1 TO EXTENT(reftable.val):
	   IF qty LE reftable.val[v] THEN LEAVE.
	   IF v = EXTENT(reftable.val) THEN DO:
	     v = 0.
	     RELEASE reftable.
	     LEAVE.
	   END.
	 END.

	 IF AVAIL reftable THEN
	   {cec/refest5a.i MAT-CST i "no-lock no-error"}

     v-mat-cost = IF AVAIL reftable THEN reftable.val[v] ELSE 0.
     IF xef.mis-simon[i] = 'M' THEN DO:
        mis-tot[5] = xef.mis-matf[i] + (v-mat-cost * qty / 1000).
        CREATE ttPrepMiscM.
        ASSIGN 
          ttPrepMiscM.iForm = xef.mis-snum[i]
          ttPrepMiscM.iBlank = xef.mis-bnum[i]
          ttPrepMiscM.dCostTotal = mis-tot[5]
          ttPrepMiscM.dCostM = mis-tot[5] / (qty / 1000 )
          ttPrepMiscM.lMatLab  = YES
          ttPrepMiscM.cSIMON = 'M'
          ttPrepMiscM.cCode     = "MISM" + string(i,"9")
          . 
        dMCostToExcludeMisc = dMCostToExcludeMisc + mis-tot[5].
        IF ceprepprice-chr EQ 'Profit' THEN
            ASSIGN 
                ttPrepMiscM.dPriceTotal =  mis-tot[5] / (1 - (xef.mis-mkup[i] / 100))
                dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[5] / (1 - (xef.mis-mkup[i] / 100)).
        ELSE 
            ASSIGN
                ttPrepMiscM.dPriceTotal =  mis-tot[5] * (1 + (xef.mis-mkup[i] / 100))
                dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[5] * (1 + (xef.mis-mkup[i] / 100)).
        ttPrepMiscM.dPriceM = ttPrepMiscM.dPriceTotal / (qty / 1000).
     END.
     ELSE IF ceprepprice-chr EQ "Profit" THEN
        mis-tot[5] = (xef.mis-matf[i] + (v-mat-cost * qty / 1000)) /
   	        		 (1 - (xef.mis-mkup[i] / 100)).
     ELSE
        mis-tot[5] = (xef.mis-matf[i] + (v-mat-cost * qty / 1000)) *
   	     		     (1 + (xef.mis-mkup[i] / 100)).

	 {cec/refest5a.i LAB-QTY i "no-lock no-error"}

	 IF AVAIL reftable THEN DO v = 1 TO EXTENT(reftable.val):
	   IF qty LE reftable.val[v] THEN LEAVE.
	   IF v = EXTENT(reftable.val) THEN DO:
	     v = 0.
	     RELEASE reftable.
	     LEAVE.
	   END.
	 END.

	 IF AVAIL reftable THEN
	   {cec/refest5a.i LAB-CST i "no-lock no-error"}

     v-lab-cost = IF AVAIL reftable THEN reftable.val[v] ELSE 0.
    
     IF xef.mis-simon[i] = 'M' THEN DO:
        mis-tot[6] = xef.mis-labf[i] + (v-lab-cost * qty / 1000).
        CREATE ttPrepMiscM.
        ASSIGN 
          ttPrepMiscM.iForm = xef.mis-snum[i]
          ttPrepMiscM.iBlank = xef.mis-bnum[i]
          ttPrepMiscM.dCostTotal = mis-tot[6]
          ttPrepMiscM.dCostM = mis-tot[6] / (qty / 1000 )
          ttPrepMiscM.lMatLab  = NO
          ttPrepMiscM.cSIMON = 'M'
          ttPrepMiscM.cCode     = "MISL" + string(i,"9")
          . 
        dMCostToExcludeMisc = dMCostToExcludeMisc + mis-tot[6].
        IF ceprepprice-chr EQ 'Profit' THEN 
            ASSIGN 
                ttPrepMiscM.dPriceTotal =  mis-tot[6] / (1 - (xef.mis-mkup[i] / 100))
                dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[6] / (1 - (xef.mis-mkup[i] / 100)).
        ELSE 
            ASSIGN 
                ttPrepMiscM.dPriceTotal =  mis-tot[6] * (1 + (xef.mis-mkup[i] / 100))
                dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[6] * (1 + (xef.mis-mkup[i] / 100)).
        ttPrepMiscM.dPriceM = ttPrepMiscM.dPriceTotal / (qty / 1000).
     END.
     ELSE IF ceprepprice-chr EQ "Profit" THEN
	    mis-tot[6] = (xef.mis-labf[i] + (v-lab-cost * qty / 1000)) /
	        		 (1 - (xef.mis-mkup[i] / 100)).
     ELSE
        mis-tot[6] = (xef.mis-labf[i] + (v-lab-cost * qty / 1000)) *
	       			 (1 + (xef.mis-mkup[i] / 100)).

     prep-tot = mis-tot[5] + mis-tot[6].

     IF ceprep-cha EQ "FiveDollar" AND
        prep-tot NE 0 THEN DO:
        ld-fac = prep-tot.
        {sys/inc/roundupfive.i prep-tot}
        ASSIGN
           ld-fac = prep-tot / ld-fac
           mis-tot[5] = mis-tot[5] * ld-fac
           mis-tot[6] = mis-tot[6] * ld-fac.
     END.

	 IF mis-tot[5] NE 0 THEN DO:
	   CREATE xprep.
	   ASSIGN xprep.frm      = xef.mis-snum[i]
		  xprep.blank-no = xef.mis-bnum[i]
		  xprep.qty      = 1
		  xprep.std-cost = mis-tot[5]
		  xprep.ml       = YES
		  xprep.cost-m   = mis-tot[5] / (qty / 1000)
		  xprep.simon    = xef.mis-simon[i]
		  xprep.code     = "MISM" + string(i,"9").
	 END.

	 IF mis-tot[6] NE 0 THEN DO:
	   CREATE xprep.
	   ASSIGN xprep.frm      = xef.mis-snum[i]
		  xprep.blank-no = xef.mis-bnum[i]
		  xprep.qty      = 1
		  xprep.std-cost = mis-tot[6]
		  xprep.ml       = NO
		  xprep.cost-m   = mis-tot[6] / (qty / 1000)
		  xprep.simon    = xef.mis-simon[i]
		  xprep.code     = "MISL" + string(i,"9").
	 END.

     ASSIGN
	    mis-tot[1] = mis-tot[1] + mis-tot[5]
	    mis-tot[2] = mis-tot[2] + (mis-tot[5] / (qty / 1000))
	    mis-tot[3] = mis-tot[3] + mis-tot[6]
	    mis-tot[4] = mis-tot[4] + (mis-tot[6] / (qty / 1000)).

/* end ---------------------------------- copr. 1996  advanced software, inc. */
