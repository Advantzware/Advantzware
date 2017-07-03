/* ------------------------------------------------- ce/com/pr4-mis.p 7/92 cd */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def var qm as DEC NO-UNDO.
def var qm1 as DEC NO-UNDO.
def var v as INT NO-UNDO.
def var v-mat-cost as dec NO-UNDO.
def var v-lab-cost as dec NO-UNDO.
DEF VAR ld-fac AS DEC NO-UNDO.
DEF VAR v-tmp-int AS INT NO-UNDO.

DEF BUFFER bf-eb FOR eb.

{ce/print4.i shared shared}

{sys/inc/ceprep.i}
{sys/inc/ceprepprice.i}

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

mis-tot = 0.

head-blok:
for each xef where xef.company = xest.company AND xef.est-no eq xest.est-no NO-LOCK:
  do i = 1 to 6:
    if index("IM",xef.mis-simon[i]) eq 0 or xef.mis-cost[i] eq "" then next.
    
    put skip(1)
        "Miscellaneous Cost"
        "Mat/F"             to 30
        "Lab/F"             to 40
        "Mat/M"             to 50
        "Lab/M"             to 60
        "Mrkup%"            to 69
        "Total Cost"        to 80
        skip.
        
    leave head-blok.
  end.  
end.

for each xef where xef.company = xest.company AND xef.est-no eq xest.est-no:
  qm1 = 0.
  for each bf-eb FIELDS(yrprice yld-qty bl-qty)
      where bf-eb.company = xef.company
        AND bf-eb.est-no   eq xef.est-no
        and bf-eb.form-no eq xef.form-no
      no-lock:
    qm1 = qm1 + (if bf-eb.yrprice then bf-eb.yld-qty else bf-eb.bl-qty).  
  end.
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

  do i = 1 to 6 with frame ad2 down no-labels no-box:
    /* only (i)ntegrate and (m)aintenance lines are done here */
    if index("SON",xef.mis-simon[i]) > 0 then next.
    if mis-cost[i] ne "" then do:
     qm = 0.
     FOR EACH bf-eb FIELDS(yrprice yld-qty bl-qty)
         WHERE bf-eb.company      EQ xef.company
           AND bf-eb.est-no       EQ xef.est-no
           AND bf-eb.form-no      EQ xef.form-no
           AND (bf-eb.blank-no    EQ xef.mis-bnum[i] OR
                xef.mis-bnum[i] EQ 0)
         NO-LOCK:
       qm = qm + (if bf-eb.yrprice then bf-eb.yld-qty else bf-eb.bl-qty).
     END.

	 {ce/refest5a.i MAT-QTY i "no-lock no-error"}

	 if avail reftable then do v = 1 to EXTENT(reftable.val):
	   if qm le reftable.val[v] then leave.
	   if v = EXTENT(reftable.val) then do:
	     v = 0.
	     release reftable.
	     leave.
	   end.
	 end.

	 if avail reftable then
	   {ce/refest5a.i MAT-CST i "no-lock no-error"}

     v-mat-cost = if avail reftable then reftable.val[v] else 0.

    IF xef.mis-simon[i] = 'M' THEN DO:
        mis-tot[5] = xef.mis-matf[i] + (v-mat-cost * qm / 1000).
        dMCostToExcludeMisc = dMCostToExcludeMisc + mis-tot[5].
        IF ceprepprice-chr EQ 'Profit' THEN 
            dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[5] / (1 - (xef.mis-mkup[i] / 100)).
        ELSE 
            dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[5] * (1 + (xef.mis-mkup[i] / 100)).
     END.
     ELSE IF ceprepprice-chr EQ "Profit" THEN
        mis-tot[5] = (xef.mis-matf[i] + (v-mat-cost * (qm / 1000))) /
                                        (1 - (xef.mis-mkup[i] / 100)).
     ELSE
        mis-tot[5] = (xef.mis-matf[i] + (v-mat-cost * (qm / 1000))) *
                                        (1 + (xef.mis-mkup[i] / 100)).

	 {ce/refest5a.i LAB-QTY i "no-lock no-error"}

	 if avail reftable then do v = 1 to EXTENT(reftable.val):
	   if qm le reftable.val[v] then leave.
	   if v = EXTENT(reftable.val) then do:
	     v = 0.
	     release reftable.
	     leave.
	   end.
	 end.

	 if avail reftable then
	   {ce/refest5a.i LAB-CST i "no-lock no-error"}

     v-lab-cost = if avail reftable then reftable.val[v] else 0.

      IF xef.mis-simon[i] = 'M' THEN DO:
        mis-tot[6] = xef.mis-labf[i] + (v-lab-cost * qty / 1000).
        dMCostToExcludeMisc = dMCostToExcludeMisc + mis-tot[6].
        IF ceprepprice-chr EQ 'Profit' THEN 
            dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[6] / (1 - (xef.mis-mkup[i] / 100)).
        ELSE 
            dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[6] * (1 + (xef.mis-mkup[i] / 100)).
     END.
     ELSE IF ceprepprice-chr EQ "Profit" THEN
        mis-tot[6] = (xef.mis-labf[i] + (v-lab-cost * (qm / 1000))) /
                                        (1 - (xef.mis-mkup[i] / 100)).
     ELSE
        mis-tot[6] = (xef.mis-labf[i] + (v-lab-cost * (qm / 1000))) *
                                        (1 + (xef.mis-mkup[i] / 100)).

     ASSIGN
        qm = qm / 1000
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
		  xprep.cost-m   = mis-tot[5] / qm
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
		  xprep.cost-m   = mis-tot[6] / qm
		  xprep.simon    = xef.mis-simon[i]
		  xprep.code     = "MISL" + string(i,"9").
	 end.

     ASSIGN
	    mis-tot[1] = mis-tot[1] + mis-tot[5]
	    mis-tot[2] = mis-tot[2] + (mis-tot[5] / qm1)
	    mis-tot[3] = mis-tot[3] + mis-tot[6]
	    mis-tot[4] = mis-tot[4] + (mis-tot[6] / qm1).
         
         if xef.mis-bnum[i] = 0 then
         for each blk where blk.snum = xef.mis-snum[i]:
            find first xjob
                where xjob.i-no     eq blk.id
                  and xjob.form-no  eq blk.snum
                  and xjob.blank-no eq blk.bnum
                no-error.
            assign
             xjob.lab = xjob.lab + (mis-tot[6] * blk.pct)
             xjob.mat = xjob.mat + (mis-tot[5] * blk.pct)
             blk.lab  = blk.lab  + (mis-tot[6] * blk.pct)
             blk.cost = blk.cost + ((mis-tot[5] + mis-tot[6]) * blk.pct).
         end.

         else do:
            find first blk where blk.snum = xef.mis-snum[i] and
                                 blk.bnum = xef.mis-bnum[i] no-error.
            find first xjob
                where xjob.i-no     eq blk.id
                  and xjob.form-no  eq blk.snum
                  and xjob.blank-no eq blk.bnum
                no-error.
            if available blk then do:
               assign xjob.lab = xjob.lab + mis-tot[6]
                      xjob.mat = xjob.mat + mis-tot[5]
                      blk.lab  = blk.lab  + mis-tot[6]
                      blk.cost = blk.cost + (mis-tot[5] + mis-tot[6]).
            end.
         end.
     
    display xef.mis-cost[i]         format "x(20)"
            xef.mis-matf[i]         format ">>,>>9.99"   to 30
            xef.mis-labf[i]         format ">>,>>9.99"   to 40
            v-mat-cost              format ">>,>>9.99"   to 50
            v-lab-cost              format ">>,>>9.99"   to 60
            xef.mis-mkup[i]         format " >>9.99%"    to 69
            mis-tot[5] + mis-tot[6] format ">>>>,>>9.99" to 80
            SKIP WITH STREAM-IO.
   end.
  end.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
