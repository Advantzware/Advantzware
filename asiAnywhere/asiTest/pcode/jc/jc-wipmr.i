rate = 1.
mr-rate = 1.
if ct = 1 THEN DO:
   IF v-tspost-val = "Actual" AND "{3}" = "mch-act" THEN do:
      rate = 0.
      mr-rate = 0.
     /* DO i = 1 TO ct :
        assign rate    = rate + mch-act.rate[i]
	           mr-rate = mr-rate + mch-act.rate[i].*/
      END.    
  /* END.*/
   ELSE assign rate    = (mach.run-rate / mach.run-crusiz) * {1}
               mr-rate = (mach.mr-rate / mach.mr-crusiz) * {2}.
END.
else if ct = 2 then
   assign rate    = mach.run-fixoh
	  mr-rate = mach.mr-fixoh.
else if ct = 3 then
   assign rate    = mach.run-varoh
	  mr-rate = mach.mr-varoh.
else if ct = 4 THEN DO:
   IF v-tspost-val = "Actual" AND "{3}" = "mch-act" THEN do:
      rate = 0.
      mr-rate = 0.
     /* DO i = 1 TO ct :
        assign rate    = rate + mch-act.rate[i]
	           mr-rate = mr-rate + mch-act.rate[i].*/
      /*END.*/    
      assign rate    = mach.run-varoh + mach.run-fixoh + rate
	         mr-rate = mach.mr-varoh  + mach.mr-fixoh  + rate.
   END.
   ELSE assign rate    = mach.run-varoh + mach.run-fixoh +
		                 ((mach.run-rate / mach.run-crusiz) * {1})
	           mr-rate = mach.mr-varoh  + mach.mr-fixoh  +
		                 ((mach.mr-rate / mach.mr-crusiz) * {2}).
END.  
if rate    = ? then rate    = 0.
if mr-rate = ? then mr-rate = 0.

