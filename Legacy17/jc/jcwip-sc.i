x = 99.

for each job-mch where job-mch.company = cocode and
		       job-mch.job = job.job
		       no-lock:
   find mach where mach.company = cocode and
		   mach.loc     = locode and
		   mach.m-code  = job-mch.m-code
		   no-lock no-error.
   if not available mach then next.

   {jc/jc-wipmr.i mach.run-crusiz mach.mr-crusiz "job-mch"}

   if hdr-id = ? or (job-mch.frm = job-hdr.frm and
     (job-mch.blank-no = job-hdr.blank-no or job-mch.blank-no = 0)) then
   do:
      v-pct = 1.
      if hdr-id <> ? and job-mch.blank-no = 0 then
	     v-pct = job-hdr.sq-in * .01.

      create x-mch.
      assign x-mch.form-no  = job-mch.frm
	     x-mch.line     = job-mch.line
	     x-mch.blank-no = job-mch.blank-no
         x-mch.pass     = job-mch.pass
	     x-mch.m-code   = job-mch.m-code
	     x-mch.i-no     = job-mch.i-no
	     x-mch.dept     = job-mch.dept
	     x-mch.wst-prct = job-mch.wst-prct
	     x-mch.est-speed = job-mch.speed
         x-mch.std-hrs   = job-mch.run-hr
         x-mch.std-mr-hrs = job-mch.mr-hr.

      IF job-mch.j-no EQ 0 THEN
        IF ct LT 5 OR ct EQ 7 OR ct EQ 8 THEN
          ASSIGN
	       x-mch.run-std = job-mch.run-hr * rate * v-pct
	       x-mch.mr-std  = job-mch.mr-hr * mr-rate * v-pct.
        ELSE
	    IF ct EQ 5 THEN
          ASSIGN
	       x-mch.run-std = job-mch.run-qty * v-pct
	       x-mch.mr-std  = job-mch.mr-waste * v-pct.
	    ELSE
	      ASSIGN
           x-mch.run-std = job-mch.run-qty * (job-mch.wst-prct * .01) * v-pct
	       x-mch.mr-std  = job-mch.mr-waste * v-pct.
   end.
   else
      next.
end.

for each mch-act where mch-act.company = cocode and
		       mch-act.job = job.job
		       no-lock:
   find mach where mach.company = cocode and
		   mach.loc     = locode and
		   mach.m-code  = mch-act.m-code
		   no-lock no-error.
   if not available mach then next.

   find first x-mch where x-mch.form-no  = mch-act.frm and
			  x-mch.blank-no = mch-act.blank-no and
			  x-mch.m-code   = mch-act.m-code AND
              x-mch.pass     = mch-act.pass
			  no-error.
   if not available x-mch then
   do:
      if hdr-id = ? or (mch-act.frm = job-hdr.frm and
	 (mch-act.blank-no = job-hdr.blank-no or mch-act.blank-no = 0)) then
      do:
	     create x-mch.
	     assign x-mch.form-no  = mch-act.frm
	            x-mch.line     = x
	            x-mch.blank-no = mch-act.blank-no
                x-mch.pass     = mch-act.pass
	     	    x-mch.m-code   = mch-act.m-code
	     	    x-mch.i-no     = mch-act.i-no
	     	    x-mch.run-std  = 0
	     	    x-mch.mr-std   = 0
	     	    x-mch.dept = mach.dept[1]
	            x = x - 1.
      end.
      else
	     next.
   end.

   {jc/jc-wipmr.i mch-act.crew mch-act.crew "mch-act"}
   
   v-pct = 1.
   if hdr-id <> ? and mch-act.blank-no = 0 then
      v-pct = job-hdr.sq-in * .01.

   find job-code where job-code.code = mch-act.code
		       no-lock no-error.
   if not available job-code then next.

   if job-code.cat = "MR" then
      if ct < 5 OR ct EQ 7 OR ct EQ 8 THEN
            x-mch.mr-act = x-mch.mr-act + ((mch-act.hours * mr-rate) * v-pct).
      else
	     x-mch.mr-act = x-mch.mr-act + ((mch-act.qty + mch-act.waste) * v-pct).
   else if job-code.cat = "RUN" or job-code.cat = "DT" then
   do:
      if ct < 5 OR ct EQ 7 OR ct EQ 8 THEN
            x-mch.run-act = x-mch.run-act + ((mch-act.hours * rate) * v-pct).
      else
      do:
	     if ct = 5 then
            x-mch.run-act = x-mch.run-act + (mch-act.qty *
			         v-pct).        
	     else
	        x-mch.run-act = x-mch.run-act + (mch-act.waste * v-pct).
      end.

      ASSIGN
         x-mch.run-hrs = x-mch.run-hrs + (mch-act.hours * v-pct)
         x-mch.act-qty = x-mch.act-qty + (mch-act.qty * v-pct)
         x-mch.wst-qty = x-mch.wst-qty + (mch-act.waste * v-pct).
   end.

   IF ct LT 5 OR ct EQ 7 OR ct EQ 8 THEN
      ASSIGN
         x-mch.run-std = x-mch.std-hrs * rate * v-pct
         x-mch.mr-std  = x-mch.std-mr-hrs * mr-rate * v-pct.
end.

ASSIGN
 v-std-tot = 0
 v-act-tot = 0
 v-var-tot = 0.

FOR each x-mch break by x-mch.line:
  create mch.
  BUFFER-COPY x-mch TO mch.
          
  if mch.run-act > 0 and x-mch.est-speed <> 0 THEN do:
    if ct = 6 THEN mch.run-std = x-mch.act-qty * (x-mch.wst-prct * .01).
    ELSE if ct = 5 THEN mch.run-std = x-mch.run-hrs * x-mch.est-speed.
    ELSE if ct < 5 OR ct = 7 OR ct = 8 THEN mch.run-std = (x-mch.run-std / x-mch.std-hrs) *
          	                     ROUND(((x-mch.act-qty) / x-mch.est-speed),2).
  end.

  assign
   mch.run-var  = mch.run-act - mch.run-std
   mch.mr-var   = mch.mr-act  - mch.mr-std
   v-std-tot[1] = v-std-tot[1] + mch.run-std
   v-act-tot[1] = v-act-tot[1] + mch.run-act
   v-var-tot[1] = v-var-tot[1] + mch.run-var
   v-std-tot[2] = v-std-tot[2] + mch.mr-std
   v-act-tot[2] = v-act-tot[2] + mch.mr-act
   v-var-tot[2] = v-var-tot[2] + mch.mr-var.  
end.

FIND FIRST mch
    WHERE mch.form-no  EQ 0
      AND mch.blank-no EQ 0
      AND mch.m-code   EQ "ALL"
    NO-ERROR.
IF AVAIL mch THEN DELETE mch.

CREATE mch.
ASSIGN
 mch.form-no  = 0
 mch.blank-no = 0
 mch.pass     = 0
 mch.m-code   = "ALL"
 mch.run-std  = v-std-tot[1]
 mch.run-act  = v-act-tot[1]
 mch.run-var  = v-var-tot[1]
 mch.mr-std   = v-std-tot[2]
 mch.mr-act   = v-act-tot[2]
 mch.mr-var   = v-var-tot[2].
