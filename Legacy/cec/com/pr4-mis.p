/* ------------------------------------------------- ce/com/pr4-mis.p 7/92 cd */

{sys/inc/var.i shared}

DEF SHARED BUFFER xest FOR est.
DEF SHARED BUFFER xef  FOR ef.
DEF SHARED BUFFER xeb  FOR eb.

DEF VAR qm AS DEC NO-UNDO.
DEF VAR qm1 AS DEC NO-UNDO.
DEF VAR v AS INT NO-UNDO.
DEF VAR v-mat-cost AS DEC NO-UNDO.
DEF VAR v-lab-cost AS DEC NO-UNDO.
DEF VAR ld-fac AS DEC NO-UNDO.
DEF VAR v-tmp-int AS INT NO-UNDO.

DEF BUFFER bf-eb FOR eb.

{cec/print4.i shared shared}

{sys/inc/ceprep.i}
{sys/inc/ceprepprice.i}
{sys/inc/cerun.i C}

FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.

mis-tot = 0.

head-blok:
FOR EACH xef WHERE xef.company = xest.company AND xef.est-no EQ xest.est-no NO-LOCK:
  DO i = 1 TO 6:
    IF INDEX("IM",xef.mis-simon[i]) EQ 0 OR xef.mis-cost[i] EQ "" THEN NEXT.
    
    PUT SKIP(1)
        "Miscellaneous Cost"
        "Mat/F"             TO 30
        "Lab/F"             TO 40
        "Mat/M"             TO 50
        "Lab/M"             TO 60.
    IF cerunc = "Protagon" THEN
        PUT "   OH%"            TO 69.
    ELSE
        PUT "Mrkup%"            TO 69.
    PUT
        "Total Cost"        TO 80
        SKIP.
        
    LEAVE head-blok.
  END.  
END.

FOR EACH xef WHERE xef.company = xest.company AND xef.est-no EQ xest.est-no:
  qm1 = 0.
  FOR EACH bf-eb FIELDS(yrprice yld-qty bl-qty)
      WHERE bf-eb.company = xef.company
        AND bf-eb.est-no   EQ xef.est-no
        AND bf-eb.form-no EQ xef.form-no
      NO-LOCK:
    qm1 = qm1 + (IF bf-eb.yrprice THEN bf-eb.yld-qty ELSE bf-eb.bl-qty).  
  END.
  qm1 = qm1 / 1000.

  /*
  do i = 1 to 6 with frame ad2 down no-labels no-box:
    /* only (i)ntegrate and (m)aintenance lines are done here */
    if index("IM",xef.mis-simon[i]) eq 0 then next.
    
    if xef.mis-bnum[i] eq 0 then do:
      mis-tot[5] = (xef.mis-matf[i] +
                    (xef.mis-matm[i] * (t-blkqty[xef.mis-snum[i]] / 1000))) /
                     (1 - (xef.mis-mkup[i] / 100)).
      mis-tot[6] = (xef.mis-labf[i] +
                    (xef.mis-labm[i] * (t-blkqty[xef.mis-snum[i]] / 1000))) /
                     (1 - (xef.mis-mkup[i] / 100)).

      if mis-tot[5] ne 0 then do:
        create xprep.
        assign
         xprep.frm      = xef.mis-snum[i]
         xprep.blank-no = xef.mis-bnum[i]
         xprep.qty      = 1
         xprep.std-cost = mis-tot[5]
         xprep.ml       = yes
         xprep.cost-m   = mis-tot[5] / qm
         xprep.simon    = xef.mis-simon[i]
         xprep.code     = "MISM" + string(i,"9").
      end.

      if mis-tot[6] ne 0 then do:
        create xprep.
        assign
         xprep.frm      = xef.mis-snum[i]
         xprep.blank-no = xef.mis-bnum[i]
         xprep.qty      = 1
         xprep.std-cost = mis-tot[6]
         xprep.ml       = no
         xprep.cost-m   = mis-tot[6] / qm
         xprep.simon    = xef.mis-simon[i]
         xprep.code     = "MISL" + string(i,"9").
      end.

      for each blk where blk.snum eq xef.mis-snum[i]:
        find first xjob
            where xjob.i-no     eq blk.id
              and xjob.form-no  eq blk.snum
              and xjob.blank-no eq blk.bnum
           no-error.
              
        assign
         xjob.lab = xjob.lab + (mis-tot[6] * blk.pct)
         xjob.mat = xjob.mat + (mis-tot[5] * blk.pct)
         blk.lab  = blk.lab  +  (mis-tot[6] * blk.pct)
         blk.cost = blk.cost + ((mis-tot[5] + mis-tot[6]) * blk.pct).
      end.

      /* material per 1000 blank */
      mis-tot[2] = mis-tot[2] + (mis-tot[5] / qm).

      /* labor per 1000 blank */
      mis-tot[4] = mis-tot[4] + (mis-tot[6] / qm).
    end.

    else do:
      find first blk
          where blk.snum eq xef.mis-snum[i]
            and blk.bnum eq xef.mis-bnum[i]
          no-error.
            
      find first xjob
          where xjob.i-no     eq blk.id
            and xjob.form-no  eq blk.snum
            and xjob.blank-no eq blk.bnum
          no-error.
            
      if avail blk then do:
        mis-tot[5] = (xef.mis-matf[i] +
                      (xef.mis-matm[i] * (blk.qyld / 1000))) /
                       (1 - (xef.mis-mkup[i] / 100)).
        mis-tot[6] = (xef.mis-labf[i] +
                      (xef.mis-labm[i] * (blk.qyld / 1000))) /
                       (1 - (xef.mis-mkup[i] / 100)).
                        
        assign
         xjob.lab = xjob.lab + mis-tot[6]
         xjob.mat = xjob.mat + mis-tot[5]
         blk.lab  = blk.lab  + mis-tot[6]
         blk.cost = blk.cost + (mis-tot[5] + mis-tot[6]).
          
        /* material per 1000 blank */
        mis-tot[2] = mis-tot[2] +
              (mis-tot[5] / ((if blk.yr$ then blk.qyld else blk.qreq) / 1000)).
        
        /* labor per 1000 blank */
        mis-tot[4] = mis-tot[4] +
              (mis-tot[6] / ((if blk.yr$ then blk.qyld else blk.qreq) / 1000)).
      end.
    end.

    /* total material */
    mis-tot[1] = mis-tot[1] + mis-tot[5].
    
    /* total labor */
    mis-tot[3] = mis-tot[3] + mis-tot[6].*/

       
  DO i = 1 TO 6 WITH FRAME ad2 DOWN NO-LABELS NO-BOX:
    /* only (i)ntegrate and (m)aintenance lines are done here */
    IF INDEX("SON",xef.mis-simon[i]) > 0 THEN NEXT.
    IF mis-cost[i] NE "" THEN DO:
     qm = 0.
     FOR EACH bf-eb FIELDS(yrprice yld-qty bl-qty)
         WHERE bf-eb.company      EQ xef.company
           AND bf-eb.est-no       EQ xef.est-no
           AND bf-eb.form-no      EQ xef.form-no
           AND (bf-eb.blank-no    EQ xef.mis-bnum[i] OR
                xef.mis-bnum[i] EQ 0)
         NO-LOCK:
       qm = qm + (IF bf-eb.yrprice THEN bf-eb.yld-qty ELSE bf-eb.bl-qty).
     END.

	 {cec/refest5a.i MAT-QTY i "no-lock no-error"}

	 IF AVAIL reftable THEN DO v = 1 TO EXTENT(reftable.val):
	   IF qm LE reftable.val[v] THEN LEAVE.
	   IF v = EXTENT(reftable.val) THEN DO:
	     v = 0.
	     RELEASE reftable.
	     LEAVE.
	   END.
	 END.

	 IF AVAIL reftable THEN
	   {cec/refest5a.i MAT-CST i "no-lock no-error"}

     ASSIGN
	 v-mat-cost = IF AVAIL reftable THEN reftable.val[v] ELSE 0.

     IF xef.mis-simon[i] = 'M' THEN DO:
        mis-tot[5] = xef.mis-matf[i] + (v-mat-cost * qm / 1000).
        dMCostToExcludeMisc = dMCostToExcludeMisc + mis-tot[5].
        IF ceprepprice-chr EQ 'Profit' THEN 
            dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[5] / (1 - (xef.mis-mkup[i] / 100)).
        ELSE 
            dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[5] * (1 + (xef.mis-mkup[i] / 100)).
     END.
     ELSE IF ceprepprice-chr EQ "Profit" THEN
        mis-tot[5] = (xef.mis-matf[i] + (v-mat-cost * qm / 1000)) /
		    		 (1 - (xef.mis-mkup[i] / 100)).
     ELSE
        mis-tot[5] = (xef.mis-matf[i] + (v-mat-cost * qm / 1000)) *
		    		 (1 + (xef.mis-mkup[i] / 100)).

	 {cec/refest5a.i LAB-QTY i "no-lock no-error"}

	 IF AVAIL reftable THEN DO v = 1 TO EXTENT(reftable.val):
	   IF qm LE reftable.val[v] THEN LEAVE.
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
        mis-tot[6] = xef.mis-labf[i] + (v-lab-cost * qm / 1000).
        dMCostToExcludeMisc = dMCostToExcludeMisc + mis-tot[6].
        IF ceprepprice-chr EQ 'Profit' THEN 
            dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[6] / (1 - (xef.mis-mkup[i] / 100)).
        ELSE 
            dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[6] * (1 + (xef.mis-mkup[i] / 100)).
     END.
     ELSE IF ceprepprice-chr EQ "Profit" THEN
        mis-tot[6] = (xef.mis-labf[i] + (v-lab-cost * qm / 1000)) /
		   		     (1 - (xef.mis-mkup[i] / 100)).
     ELSE
        mis-tot[6] = (xef.mis-labf[i] + (v-lab-cost * qm / 1000)) *
		   		     (1 + (xef.mis-mkup[i] / 100)).

     ASSIGN
        qm = qm / 1000
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
	   ASSIGN xprep.frm  = xef.mis-snum[i]
		  xprep.blank-no = xef.mis-bnum[i]
		  xprep.qty      = 1
		  xprep.std-cost = mis-tot[5]
		  xprep.ml       = YES
		  xprep.cost-m   = mis-tot[5] / qm
		  xprep.simon    = xef.mis-simon[i]
		  xprep.code     = "MISM" + string(i,"9").
	 END.

	 IF mis-tot[6] NE 0 THEN DO:
	   CREATE xprep.
	   ASSIGN xprep.frm  = xef.mis-snum[i]
		  xprep.blank-no = xef.mis-bnum[i]
		  xprep.qty      = 1
		  xprep.std-cost = mis-tot[6]
		  xprep.ml       = NO
		  xprep.cost-m   = mis-tot[6] / qm
		  xprep.simon    = xef.mis-simon[i]
		  xprep.code     = "MISL" + string(i,"9").
	 END.

     ASSIGN
	    mis-tot[1] = mis-tot[1] + mis-tot[5]
	    mis-tot[2] = mis-tot[2] + (mis-tot[5] / qm1)
	    mis-tot[3] = mis-tot[3] + mis-tot[6]
	    mis-tot[4] = mis-tot[4] + (mis-tot[6] / qm1).
         
         IF xef.mis-bnum[i] = 0 THEN
         FOR EACH blk WHERE blk.snum = xef.mis-snum[i]:
            FIND FIRST xjob
                WHERE xjob.i-no     EQ blk.id
                  AND xjob.form-no  EQ blk.snum
                  AND xjob.blank-no EQ blk.bnum
                NO-ERROR.
            ASSIGN
             xjob.lab = xjob.lab + (mis-tot[6] * blk.pct)
             xjob.mat = xjob.mat + (mis-tot[5] * blk.pct)
             blk.lab  = blk.lab  + (mis-tot[6] * blk.pct)
             blk.cost = blk.cost + ((mis-tot[5] + mis-tot[6]) * blk.pct).
         END.

         ELSE DO:
            FIND FIRST blk WHERE blk.snum = xef.mis-snum[i] AND
                                 blk.bnum = xef.mis-bnum[i] NO-ERROR.
            FIND FIRST xjob
                WHERE xjob.i-no     EQ blk.id
                  AND xjob.form-no  EQ blk.snum
                  AND xjob.blank-no EQ blk.bnum
                NO-ERROR.
            IF AVAILABLE blk THEN DO:
               ASSIGN xjob.lab = xjob.lab + mis-tot[6]
                      xjob.mat = xjob.mat + mis-tot[5]
                      blk.lab  = blk.lab  + mis-tot[6]
                      blk.cost = blk.cost + (mis-tot[5] + mis-tot[6]).
            END.
         END.
     
    DISPLAY xef.mis-cost[i]         FORMAT "x(20)"
            xef.mis-matf[i]         FORMAT ">>,>>9.99"   TO 30
            xef.mis-labf[i]         FORMAT ">>,>>9.99"   TO 40
            v-mat-cost              FORMAT ">>,>>9.99"   TO 50
            v-lab-cost              FORMAT ">>,>>9.99"   TO 60
            xef.mis-mkup[i]         FORMAT " >>9.99%"    TO 69
            mis-tot[5] + mis-tot[6] FORMAT ">>>>,>>9.99" TO 80
            SKIP WITH STREAM-IO.
   END.
  END.
END.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
