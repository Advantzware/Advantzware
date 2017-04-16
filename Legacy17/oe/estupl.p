/* --------------------------------------------------- oe/estupl.p 10/97 fwk  */
/* Update Estimate from Order for Multi Est per Order - o/e module            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xoe-ord for oe-ord.
def shared var fil_id as recid no-undo.
def shared var nufile as log no-undo.
def var choice as log no-undo.

def shared var v-create-job as   log    no-undo.
def shared var v-qty-mod    as   log    no-undo.

def var v-exp-limit as   int init 10    no-undo.
def var v-est-no    like est.est-no     no-undo.
def var hld-nufile  like nufile         no-undo.
def var hld-id      like fil_id         no-undo.
def var hld-stat      like job.stat       no-undo.


DISABLE TRIGGERS FOR LOAD OF itemfg.

find oe-ordl where recid(oe-ordl) eq fil_id no-lock.
 
waitwarn:
do x = 1 to v-exp-limit.
  v-est-no = oe-ordl.est-no.
  run util/rjust.p (input-output v-est-no, 8).
  find first est
      where est.company eq xoe-ord.company
        and est.est-no  eq v-est-no
      exclusive-lock no-error no-wait.

  if not avail est then do:
    message " Estimate Record NOT Found ! " view-as alert-box error.
    undo, leave.
  end.
  else
  if locked est and x eq 1 then do:
     choice = no.
     message  "Estimate is in use, unable to update information."
        "Do you wish to wait? " view-as alert-box question
         button yes-no update choice .
  end.

  if not avail est and locked est and choice then do:
    pause 2 message "Waiting for estimate...".
    if x ne v-exp-limit then
    next waitwarn.
  end.

  if avail est then do:
    if est.ord-date le xoe-ord.ord-date or est.ord-date eq ? then
      est.ord-date = xoe-ord.ord-date.
    if est.ord-no lt xoe-ord.ord-no or est.ord-no eq 0 then
      est.ord-no = xoe-ord.ord-no.
  end.

  else do:
    message  "Estimate record will not be updated from this order!!! " view-as alert-box error.
    leave waitwarn.
  end.
end. /* 1 to v-exp-limit */

/* find first sys-ctrl  where sys-ctrl.company eq cocode                       */
/*                        and sys-ctrl.name    eq "OEFGUPDT"                   */
/*                        no-lock no-error.                                    */
/*                                                                             */
for each eb where eb.company eq oe-ordl.company
              and eb.est-no  eq oe-ordl.est-no
              and (eb.part-no eq oe-ordl.part-no OR
                   eb.est-type EQ 2              OR
                   eb.est-type EQ 6):
/*                                                                             */
/*   IF (eb.stock-no EQ "" OR (AVAIL sys-ctrl AND sys-ctrl.int-fld EQ 1)) AND  */
/*      (eb.form-no EQ 0 OR (eb.est-type NE 2 AND eb.est-type NE 6))      THEN */
/*     assign eb.part-no    = oe-ordl.part-no                                  */
/*            eb.part-dscr1 = oe-ordl.i-name                                   */
/*            eb.part-dscr2 = oe-ordl.part-dscr1                               */
/*            eb.stock-no   = oe-ordl.i-no.                                    */
/*                                                                             */
  IF eb.cust-no EQ xoe-ord.cust-no THEN eb.ord-no = oe-ordl.ord-no.
end.

if v-create-job and oe-ordl.job-no ne "" then do:
  find first job where job.company eq cocode
                   and job.job-no  eq oe-ordl.job-no
                   and job.job-no2 eq oe-ordl.job-no2
                   no-error.

  if avail job /*and index("HAWPRL",job.stat) ne 0*/ and job.opened then do:
    /*
    choice = job.stat eq "P".

    if not choice and v-qty-mod then do while true on endkey undo, leave:
      message "Recreate Job Standards?:" update choice.
      leave.
    end.
    */
    if job.stat eq "P" or v-qty-mod then do:
      run jc/chkrebld.p (recid(job), output choice).

      if not choice then do:
        assign
         hld-id     = fil_id
         hld-nufile = nufile
         hld-stat   = job.stat
         nufile     = yes.

        run jc/jc-calc.p(recid(job), NO).

        assign
         fil_id   = hld-id
         nufile   = hld-nufile.
         
        if hld-stat ne "P" then job.stat = hld-stat.
     
        find oe-ordl where recid(oe-ordl) eq fil_id no-lock no-error.
      end.  
    end.
  end.
end.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
