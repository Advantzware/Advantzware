/* --------------------------------------------------- jc/jobstds.p 07/03 JLF */
/* Job Costing - Validate that Job Standards may be rebuilt                   */
/* -------------------------------------------------------------------------- */

def input param ip-rowid as rowid.
     
{sys/inc/var.i shared}
     

def new shared var nufile as log no-undo.
def new shared var fil_id as recid no-undo.

DEFINE SHARED VARIABLE vError AS CHARACTER NO-UNDO.

def var choice as log no-undo.
def var hld-stat as cha no-undo.

def buffer bf-job for job.


   


find job where rowid(job) eq ip-rowid no-lock.

if NOT job.opened then do:
  /*message "Cannot rebuild closed jobs..." view-as alert-box error. */
    vError = "Cannot rebuild closed jobs...".
    return.
end.

  
FOR EACH est-op NO-LOCK
    WHERE est-op.company EQ job.company
      AND est-op.est-no  EQ job.est-no
      AND est-op.line    LT 500,
    FIRST mach NO-LOCK
    where (mach.company eq cocode and  mach.loc eq locode)
      AND mach.m-code EQ est-op.m-code,
    FIRST reftable NO-LOCK
    WHERE reftable.reftable EQ "mach.obsolete"
      AND reftable.company  EQ mach.company
      AND reftable.loc      EQ mach.loc
      AND reftable.code     EQ mach.m-code
      AND reftable.val[1]   EQ 1:
  /*MESSAGE "Machine: " + TRIM(mach.m-code) +
          " is obsolete, please replace to create/update job standards..."
      VIEW-AS ALERT-BOX ERROR.
  */    

    vError = "Machine: " + TRIM(mach.m-code) +
          " is obsolete, please replace to create/update job standards...".
  RETURN.
END.


run jc/chkrebld.p (recid(job), output choice).

if choice then do:
  /*message "Cannot rebuild jobs after GL has been posted..."
      view-as alert-box error.
      */
     vError = "Cannot rebuild jobs after GL has been posted...".
  return.
end.

choice = no.

/*
message "Recalculate Job Standards for Job# " +
        TRIM(job.job-no) + "-" + STRING(job.job-no2,"99") +
        "?"
    view-as alert-box question button yes-no update choice.  
   
if choice then do:
  SESSION:SET-WAIT-STATE("general").
*/  

  if job.est-no eq "" then do:
    find first job-hdr
        where job-hdr.company eq cocode
          and job-hdr.job     eq job.job
        no-lock no-error.
    if avail job-hdr then do:
      find bf-job where recid(bf-job) eq recid(job) EXCLUSIVE-LOCK.
      if avail bf-job then bf-job.est-no = job-hdr.est-no.
    end.
  end.
    
  assign
   nufile   = yes
   hld-stat = job.stat.

  run jc/jc-calc.p (recid(job)).

  
  fil_id = RECID(job).
  
  /*RUN po/do-po.p.  */

  nufile = no.
       
  if hld-stat ne "P" then job.stat = hld-stat.

  
  /*
  SESSION:SET-WAIT-STATE("").
END.
*/
