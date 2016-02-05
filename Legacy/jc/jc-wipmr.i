ASSIGN
   rate = 1
   mr-rate = 1.

if ct = 1 THEN DO:
   IF "{3}" EQ "mch-act" THEN DO:
     ASSIGN
      rate    = 0
      mr-rate = 0.

     IF v-tspost-val EQ "Actual" THEN
     DO i = 1 TO {1}:
       ASSIGN
        rate    = rate + mch-act.rate[i]
	    mr-rate = mr-rate + mch-act.rate[i].
     END.    

     ELSE
       ASSIGN
        rate    = mch-act.rate[INT(mch-act.crew)] + rate
	    mr-rate = mch-act.rate[INT(mch-act.crew)] + mr-rate.
   END.

   ELSE
     ASSIGN
      rate    = job-mch.run-rate
	  mr-rate = job-mch.mr-rate.
END.
else if ct = 2 then
   assign rate    = mach.run-fixoh
	  mr-rate = mach.mr-fixoh.
else if ct = 3 then
   assign rate    = mach.run-varoh
	  mr-rate = mach.mr-varoh.
else if ct = 4 THEN DO:
   IF "{3}" EQ "mch-act" THEN DO:
     ASSIGN
      rate    = mch-act.fixoh + mch-act.varoh
      mr-rate = mch-act.fixoh + mch-act.varoh.

     IF v-tspost-val EQ "Actual" THEN
     DO i = 1 TO {1}:
       ASSIGN
        rate    = rate + mch-act.rate[i]
	    mr-rate = mr-rate + mch-act.rate[i].
     END.    

     ELSE
       ASSIGN
        rate    = mch-act.rate[INT(mch-act.crew)] + rate
	    mr-rate = mch-act.rate[INT(mch-act.crew)] + mr-rate.
   END.

   ELSE
     ASSIGN
      rate    = job-mch.run-rate + job-mch.run-varoh + job-mch.run-fixoh
	  mr-rate = job-mch.mr-rate  + job-mch.mr-varoh  + job-mch.mr-fixoh.
END.  
ELSE IF ct = 7 THEN
DO:
   /*jc/jcwip-sc.i separates mch-act by
    mr and run by the mch-act.code*/
   IF "{3}" EQ "mch-act" THEN
      assign rate    = mch-act.varoh
	         mr-rate = mch-act.varoh.
   ELSE
      ASSIGN
         rate    = job-mch.run-varoh
	     mr-rate = job-mch.mr-varoh.
END.

ELSE IF ct = 8 THEN
DO:
   IF "{3}" EQ "mch-act" THEN
      assign rate    = mch-act.fixoh
             mr-rate = mch-act.fixoh.
   ELSE
      ASSIGN
         rate    = job-mch.run-fixoh
	     mr-rate = job-mch.mr-fixoh.
END.

if rate    = ? then rate    = 0.
if mr-rate = ? then mr-rate = 0.

