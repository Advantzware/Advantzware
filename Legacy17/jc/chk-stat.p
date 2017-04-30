/* ---------------------------------------------------- jc/chk-stat.p 8/94 gb */
/* Job Costing - Job Status Verification                                      */
/* -------------------------------------------------------------------------- */

def input parameter rec-id as recid no-undo.
def input parameter chk-type as int no-undo.
DEF INPUT PARAMETER show-message AS LOG NO-UNDO.
def output parameter v-good as logical no-undo.


DEF BUFFER b-job FOR job.

def var tmpstore as ch no-undo.
def var msgs as ch format 'x(21)' extent 2 init
    ["Cannot Issue Materials","Cannot Post Production"] no-undo.


find job where recid(job) eq rec-id no-lock no-error.
if not avail job then leave.
    
FOR EACH b-job
    WHERE b-job.company EQ job.company
      AND b-job.job-no  EQ job.job-no
      AND b-job.job-no2 EQ job.job-no2
    BY b-job.job:
  rec-id = RECID(b-job).
END.

find job where recid(job) eq rec-id no-lock no-error.

if job.stat eq "P" then                                     /*** PENDING ***/
  assign
   v-good   = no
   tmpstore = msgs[chk-type] + " to a Job which has not been Released.".
   
else
if job.stat eq "D" then                                     /*** DELETED ***/
  assign
   v-good   = no
   tmpstore = msgs[chk-type] + " to a Deleted Job.".
   
else
if index("CX",job.stat) gt 0 then               /*** CLOSED or COMPLETED ***/     assign
   v-good   = no
   tmpstore = msgs[chk-type] + " to a Closed or Completed Job.".
   
else
if job.stat eq "H" then                                     /*** ON HOLD ***/
  assign
   v-good   = no
   tmpstore = msgs[chk-type] + " to a Job that is on Hold.".
   
else v-good = yes.

if not v-good AND show-MESSAGE then /*run sys/msg/wemsg.p(tmpstore, " E R R O R ", 4). */
     MESSAGE tmpstore VIEW-AS ALERT-BOX ERROR.

