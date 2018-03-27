
def input parameter v-recid  as recid.
def input parameter v-factor as int.

{sys/inc/var.i shared}

def var v-hrs   like job-mch.run-hr.
def var v-qty   like job-mch.run-qty.
def var v-time  as   int.
def var v-up-hs as   int.


DO TRANSACTION:
  {sys/inc/autopdc.i}
END.

find job where recid(job) eq v-recid no-lock no-error.

if not avail job then leave.

find first est
    where est.company EQ job.company
      AND est.est-no  eq job.est-no
    no-lock no-error.

if v-factor eq -1 then v-time = (24 * 3600) - 1.

for each shift where shift.company eq cocode no-lock
    
    by shift.start-time
    by shift.end-time:
    
  v-time = if v-factor eq 1 then shift.start-time else shift.end-time.
  leave.
end.

for each job-mch
    where job-mch.company                 eq cocode
      and job-mch.job                     eq job.job
      and job-mch.job-no                  eq job.job-no
      and job-mch.job-no2                 eq job.job-no2
      and (lookup(job-mch.m-code,autopdc) gt 0 or
           autopdc                        eq "*")  
    no-lock,

    first mach
    where mach.company eq cocode
      and mach.loc     eq locode
      and mach.m-code  eq job-mch.m-code
    no-lock:
    
  v-up-hs = 1.

  if job-mch.dept eq "HS" and
     avail est            and
     mach.therm           and
     mach.p-type eq "S"   then
    run sys/inc/numup.p (est.company, est.est-no, job-mch.frm, output v-up-hs).

  RELEASE eb.
  IF TRIM(job.est-no) NE "" THEN
  FIND FIRST eb
      WHERE eb.company   EQ job.company
        AND eb.est-no    EQ job.est-no
        AND eb.form-no   EQ job-mch.frm
        AND (eb.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0)
      NO-LOCK NO-ERROR.
    
  do i = 1 to 2:
    v-hrs = if i eq 1 then job-mch.mr-hr else job-mch.run-hr.
    
    if v-hrs ne 0 then do:
      create mch-act.
      assign
       mch-act.company  = cocode
       mch-act.op-date  = today
       mch-act.op-time  = time
       mch-act.job      = job-mch.job
       mch-act.j-no     = job-mch.j-no
       mch-act.job-no   = job-mch.job-no
       mch-act.job-no2  = job-mch.job-no2
       mch-act.frm      = job-mch.frm
       mch-act.blank-no = job-mch.blank-no
       mch-act.m-code   = job-mch.m-code
       mch-act.dept     = job-mch.dept
       mch-act.pass     = job-mch.pass
       mch-act.speed    = job-mch.speed
       mch-act.opn      = yes
       mch-act.complete = yes
       mch-act.hours    = v-hrs
       mch-act.shift    = if avail shift then shift.shift else 1
       mch-act.user-id  = USERID(LDBNAME(1)) .
             
      if i eq 1 then do:
        assign
         mch-act.crew  = mach.mr-crusiz
         mch-act.waste = job-mch.mr-waste / v-up-hs
         mch-act.qty   = 0
         mch-act.code  = "MR".

        IF AVAIL mach AND AVAIL eb THEN
          RUN est/getcrusz.p (ROWID(mach), ROWID(eb), mch-act.dept, "M R",
                              INPUT-OUTPUT mch-act.crew).
         
        find first job-code
            where job-code.code eq "MR"
              and job-code.cat  eq "MR"
            no-lock no-error.
        if not avail job-code then do:
          find first job-code where job-code.cat eq "MR" no-lock no-error.
          if avail job-code then mch-act.code = job-code.code.
        end. 
      end.
      
      else do:
        assign
         mch-act.crew  = mach.run-crusiz
         mch-act.qty   = job-mch.run-qty - job-mch.mr-waste
         mch-act.waste = mch-act.qty * job-mch.wst-prct / 100
         mch-act.qty   = mch-act.qty - mch-act.waste
         mch-act.waste = mch-act.waste / v-up-hs
         mch-act.qty   = mch-act.qty   / v-up-hs
         mch-act.code  = "RUN".

        IF AVAIL mach AND AVAIL eb THEN
          RUN est/getcrusz.p (ROWID(mach), ROWID(eb), mch-act.dept, "RUN",
                              INPUT-OUTPUT mch-act.crew).
         
        find first job-code
            where job-code.code eq "RUN"
              and job-code.cat  eq "RUN"
            no-lock no-error.
        if not avail job-code then do:
          find first job-code where job-code.cat eq "RUN" no-lock no-error.
          if avail job-code then mch-act.code = job-code.code.
        end.
      end.
      
      assign
       mch-act.hours = mch-act.hours * v-factor 
       mch-act.qty   = mch-act.qty   * v-factor
       mch-act.waste = mch-act.waste * v-factor
       mch-act.start = v-time
       mch-act.stopp = v-time + (mch-act.hours * 3600).
    end.
  end.
end.

