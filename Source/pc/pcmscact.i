find FIRST job where job.company = cocode and
	       job.job-no = pc-misc.job-no and
	       job.job-no2 = pc-misc.job-no2
	       exclusive-lock no-error.

if available job then
do:
   assign job.stat = "W".

   create misc-act.
   assign 
      misc-act.company   = cocode
	   misc-act.misc-date = pc-misc.misc-date
	   misc-act.misc-time = pc-misc.misc-time 
	   misc-act.job       = job.job
	   misc-act.job-no    = pc-misc.job-no
	   misc-act.job-no2   = pc-misc.job-no2
	   misc-act.frm       = pc-misc.frm
	   misc-act.blank-no  = pc-misc.blank-no
	   misc-act.m-code    = pc-misc.m-code
	   misc-act.ml        = pc-misc.ml
	   misc-act.i-no      = pc-misc.i-no
	   misc-act.dscr      = pc-misc.dscr
	   misc-act.opn       = yes
	   misc-act.cost-type = pc-misc.cost-type
	   misc-act.cost      = pc-misc.cost
	   misc-act.dept      = pc-misc.dept
	   misc-act.cost-m    = pc-misc.cost-m.
end.
