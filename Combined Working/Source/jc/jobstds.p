/* --------------------------------------------------- jc/jobstds.p 07/03 JLF */
/* Job Costing - Validate that Job Standards may be rebuilt                   */
/* -------------------------------------------------------------------------- */

def input param ip-rowid as rowid.

{sys/inc/var.i shared}

def new shared var nufile as log no-undo.
def new shared var fil_id as recid no-undo.

def var choice as log no-undo.
def var hld-stat as cha no-undo.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR gvlUseJobQty AS LOG NO-UNDO.
DEF VAR lvlQtySame AS LOG NO-UNDO.
def buffer bf-job for job.


find job where rowid(job) eq ip-rowid no-lock.

if NOT job.opened then do:
  message "Cannot rebuild closed jobs..." view-as alert-box error.
  return.
end.
  
FOR EACH est-op NO-LOCK
    WHERE est-op.company EQ job.company
      AND est-op.est-no  EQ job.est-no
      AND est-op.line    LT 500,
    FIRST mach NO-LOCK
    {sys/look/machW.i}
      AND mach.m-code EQ est-op.m-code:
 IF mach.obsolete THEN DO:
  MESSAGE "Machine: " + TRIM(mach.m-code) +
          " is obsolete, please replace to create/update job standards..."
      VIEW-AS ALERT-BOX ERROR.
  RETURN.
 END.
END.

run jc/chkrebld.p (recid(job), output choice).

if choice then do:
  message "Cannot rebuild jobs after GL has been posted..."
      view-as alert-box error.
  return.
end.

choice = no.
message "Recalculate Job Standards for Job# " +
        TRIM(job.job-no) + "-" + STRING(job.job-no2,"99") +
        "?"
    view-as alert-box question button yes-no update choice.  
FIND FIRST job-hdr where job-hdr.company EQ job.company
      and job-hdr.job     EQ job.job
      and job-hdr.job-no  EQ job.job-no
      and job-hdr.job-no2 EQ job.job-no2
    NO-LOCK NO-ERROR.

if choice THEN  do:
    IF AVAIL job-hdr AND job-hdr.ord-no EQ 0 THEN DO:
        RUN est-qty-eq-job (INPUT RECID(job), OUTPUT lvlQtySame).
        ll = NO.
        IF NOT lvlQtySame THEN DO:
            MESSAGE "Use quantity from job? (NO for quantity from estimate)"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL UPDATE ll.
            IF ll = ? THEN
               RETURN NO-APPLY.
        END.
         gvlUseJobQty = ll.
         DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
         hProc = SESSION:FIRST-PROCEDURE.
         DO WHILE VALID-HANDLE(hProc):
            IF index(hProc:FILE-NAME, "v-job") GT 0 THEN
                LEAVE. /* found it. */
            hProc = hProc:NEXT-SIBLING.
         END.
        
        
         IF VALID-HANDLE(hProc) THEN DO:
            RUN setUseJobQty IN hProc (INPUT gvlUseJobQty).
         END.
    END.

  SESSION:SET-WAIT-STATE("general").

  if job.est-no eq "" then do:
    find first job-hdr
        where job-hdr.company eq cocode
          and job-hdr.job     eq job.job
        no-lock no-error.
    if avail job-hdr then do:
      find bf-job where recid(bf-job) eq recid(job) no-error.
      if avail bf-job then bf-job.est-no = job-hdr.est-no.
    end.
  end.
    
  assign
   nufile   = yes
   hld-stat = job.stat.
        
  run jc/jc-calc.p (recid(job), YES).

  fil_id = RECID(job).
  RUN po/doPo.p (YES) /* Yes Indicates to prompt for RM */.

  nufile = no.
       
  if hld-stat ne "P" then job.stat = hld-stat.
    
  SESSION:SET-WAIT-STATE("").
END.

PROCEDURE est-qty-eq-job:
/* To determine if the estimate qty is same as the job qty - may not have */
/* to prompt user which one they want to use */
DEF INPUT PARAMETER ipJobRec AS RECID NO-UNDO.
DEFINE OUTPUT PARAMETER oplSameQty AS LOGICAL     NO-UNDO.
DEF BUFFER xest FOR est.
DEF BUFFER xeb FOR eb.
DEF VAR v-qty AS INT NO-UNDO.
DEF VAR ll-combo-req-qty AS LOG  NO-UNDO. /* testing */
DEF VAR v-job-qty AS INT NO-UNDO.
DEF VAR lvlSameQty AS LOG NO-UNDO.
lvlSameQty = NO.

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name EQ "JOB QTY"
    NO-ERROR.


IF AVAIL sys-ctrl THEN
ASSIGN
 ll-combo-req-qty  = sys-ctrl.int-fld EQ 0.

  FIND FIRST xest
      where xest.company EQ cocode
        and xest.est-no  EQ job.est-no
      NO-LOCK NO-ERROR.


  /* Get estimate quantity to compare */
 IF xest.est-type EQ 4 OR
    xest.est-type EQ 8 OR
    xest.est-type EQ 3 THEN DO:


    FOR EACH xeb WHERE xeb.company EQ xest.company
        AND xeb.est-no EQ xest.est-no 
        NO-LOCK
        BREAK BY xeb.stock-no:


      IF FIRST-OF(xeb.stock-no) THEN DO:
          v-qty = 0.
          FOR EACH eb fields(form-no blank-no yld-qty bl-qty) WHERE
            eb.company  EQ xest.company AND
            eb.est-no   EQ xest.est-no AND
            eb.stock-no EQ xeb.stock-no
            NO-LOCK :                                                                          
            v-qty = v-qty + (IF ll-combo-req-qty EQ YES THEN eb.bl-qty ELSE eb.yld-qty).


            FIND FIRST job-hdr where job-hdr.company EQ cocode
                  and job-hdr.job     EQ job.job
                  and job-hdr.job-no  EQ job.job-no
                  and job-hdr.job-no2 EQ job.job-no2
                  and job-hdr.frm     EQ xeb.form-no
                  and job-hdr.i-no    EQ xeb.stock-no
                  NO-LOCK NO-ERROR.  
            
            IF AVAIL job-hdr THEN
              v-job-qty = job-hdr.qty.
          END. /* each eb */
      END.  /* first of xeb.stock-no*/

    END. /* each xeb */

 END. /* if type 4, 8 or 3 */

 ELSE DO:

     FOR EACH xeb WHERE xeb.company EQ xest.company
        AND xeb.est-no EQ xest.est-no 
        NO-LOCK
        BREAK BY xeb.stock-no:


          FIND FIRST job-hdr where job-hdr.company EQ cocode
                and job-hdr.job     EQ job.job
                and job-hdr.job-no  EQ job.job-no
                and job-hdr.job-no2 EQ job.job-no2
                and job-hdr.frm     EQ xeb.form-no
                and job-hdr.i-no    EQ xeb.stock-no
             NO-LOCK NO-ERROR.  

          IF AVAIL job-hdr THEN
            v-job-qty = job-hdr.qty.


     END. /* each xeb */


     ASSIGN v-qty = xest.est-qty[1] .

 END. /* if not type 4, 8 or 3 */
 IF v-job-qty EQ v-qty THEN
     lvlSameQty = TRUE.

 oplSameQty = lvlSameQty.

END PROCEDURE.
