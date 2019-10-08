for each work-prep:
  delete work-prep.
end.

for each job-prep
    where job-prep.company eq cocode
      and job-prep.job     eq job.job
    no-lock:

  find first prep
      where prep.company eq cocode
	    and prep.code    eq job-prep.code
      no-lock no-error.

  if avail prep then do:
    find first work-prep
	where work-prep.code EQ job-prep.code
	no-error.

    if not avail work-prep then do:
      create work-prep.
      assign
       work-prep.code = prep.code
       work-prep.dscr = prep.dscr
       work-prep.work-ml = string(job-prep.ml)  .
    end.
  end.

  else do:
    find first ef
	where ef.company   EQ job.company
      AND ef.est-no    EQ job.est-no
	  and ef.form-no eq job-prep.frm
	no-lock.

    find first work-prep
	where work-prep.dscr EQ ef.mis-cost[int(substr(job-prep.code,5,1))]
	no-error.

    if not avail work-prep then do:
      create work-prep.
      assign
       work-prep.dscr = ef.mis-cost[int(substr(job-prep.code,5,1))].
       work-prep.work-ml = string(job-prep.ml) .
    end.
  end.

  assign
   work-prep.est-cost = work-prep.est-cost + (job-prep.std-cost * job-prep.qty).
end.

for each misc-act
    where misc-act.company eq cocode
      and misc-act.job     eq job.job
    no-lock:

  find first prep
      where prep.company eq cocode
	    and prep.code    eq misc-act.m-code
      no-lock no-error.

  if avail prep then do:
    find first work-prep
	where work-prep.code eq misc-act.m-code
	no-error.

    if not avail work-prep then do:
      create work-prep.
      assign
       work-prep.code = prep.code
       work-prep.dscr = prep.dscr
       work-prep.work-ml = string(misc-act.ml) .
    end.
  end.

  else do:
    find first ef
	where ef.company   EQ job.company
      AND ef.est-no    EQ job.est-no
	  and ef.form-no eq misc-act.frm
	no-lock.

    find first work-prep
	where work-prep.dscr eq ef.mis-cost[int(substr(misc-act.m-code,5,1))]
	no-error.

    if not avail work-prep then do:
      create work-prep.
      assign
       work-prep.dscr = ef.mis-cost[int(substr(misc-act.m-code,5,1))].
       work-prep.work-ml = string(misc-act.ml) .
    end.
  end.

  assign
   work-prep.act-cost = work-prep.act-cost + misc-act.cost.
end.
