/* ---------------------------------------------------jc/jc-reopn.p 10/94 gb */
/* Job Costing - ReOpen Job                                                   */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var v-fin-qty as int no-undo.
def var v as int no-undo.
DEF VAR ll-set AS LOG NO-UNDO.


find job where rowid(job) eq ip-rowid exclusive-lock no-error.
if not avail job then return.

find first jc-ctrl where jc-ctrl.company eq cocode no-lock.

find first sys-ctrl
    where sys-ctrl.company eq job.company
      and sys-ctrl.name    eq "AUTOISSU"
    no-lock no-error.

for each job-hdr
    where job-hdr.company eq job.company
      and job-hdr.job     eq job.job
      and job-hdr.job-no  eq job.job-no
      and job-hdr.job-no2 eq job.job-no2
    NO-LOCK,

    FIRST itemfg EXCLUSIVE-LOCK
    WHERE itemfg.company eq cocode
      AND itemfg.i-no    eq job-hdr.i-no:
    RUN fg/chkfgloc.p (INPUT job-hdr.i-no, INPUT job-hdr.loc).
    FIND FIRST itemfg-loc 
        WHERE itemfg-loc.company EQ cocode
          AND itemfg-loc.i-no    EQ job-hdr.i-no
          AND itemfg-loc.loc     EQ job-hdr.loc
        EXCLUSIVE-LOCK NO-ERROR.

   assign v-fin-qty = 0.
   for each fg-act where fg-act.company = cocode and
                         fg-act.job = job.job and
                         fg-act.i-no = job-hdr.i-no AND
                         fg-act.loc  = job-hdr.loc
                         no-lock:
      v-fin-qty = v-fin-qty + fg-act.qty.
   end.
   if v-fin-qty lt job-hdr.qty then do:
      IF NOT itemfg.pur-man THEN
        assign
         itemfg-loc.q-ono   = itemfg-loc.q-ono + (job-hdr.qty - v-fin-qty)
         itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
        
      run fg/comp-upd.p (recid(itemfg), (job-hdr.qty - v-fin-qty),
                         "q-ono", job-hdr.e-num).
   end.   
   FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
   assign v-fin-qty = 0.
   for each fg-act where fg-act.company = cocode and
                         fg-act.job = job.job and
                         fg-act.i-no = job-hdr.i-no
                         no-lock:
      v-fin-qty = v-fin-qty + fg-act.qty.
   end.
   if v-fin-qty lt job-hdr.qty then do:
      IF NOT itemfg.pur-man THEN
        assign
         itemfg.q-ono   = itemfg.q-ono + (job-hdr.qty - v-fin-qty)
         itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.
        
      run fg/comp-upd.p (recid(itemfg), (job-hdr.qty - v-fin-qty),
                         "q-ono", job-hdr.e-num).
   end.

   IF NOT ll-set THEN
     IF itemfg.isaset                                                    AND
        CAN-FIND(FIRST reftable
                 WHERE reftable.reftable EQ "jc/jc-calc.p"
                   AND reftable.company  EQ job.company
                   AND reftable.loc      EQ ""
                   AND reftable.code     EQ STRING(job.job,"999999999")) AND
        NOT CAN-FIND(FIRST reftable
                     WHERE reftable.reftable EQ "jc/jc-calc.p"
                       AND reftable.company  EQ job.company
                       AND reftable.loc      EQ ""
                       AND reftable.code     EQ STRING(job.job,"999999999")
                       AND reftable.code2    EQ job-hdr.i-no)
     THEN ll-set = YES.

     ELSE
     IF NOT AVAIL sys-ctrl OR sys-ctrl.char-fld NE "FGPost" THEN
       RUN jc/autopc&p.p (BUFFER job, job-hdr.i-no,
                          job-hdr.frm, job-hdr.blank-no, -1).
end.

IF NOT AVAIL sys-ctrl OR sys-ctrl.char-fld NE "FGPost" THEN DO:
  {jc/jc-autop.i -1}

  IF ll-set THEN
  FOR EACH reftable NO-LOCK
      WHERE reftable.reftable EQ "jc/jc-calc.p"
        AND reftable.company  EQ job.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ STRING(job.job,"999999999"):

    RUN jc/autopc&p.p (BUFFER job, reftable.code2,
                       INT(reftable.val[12]), INT(reftable.val[13]), -1).
  END.
END.

assign
 job.stat       = "W"
 job.lock       = NO
 job.opened     = YES
 job.close-date = ?.

{jc/closeaud.i job}
reftable.val[5] = 1.
 
FIND CURRENT reftable NO-LOCK NO-ERROR.

run jc/job-cls1.p (recid(job), -1).

for each job-hdr
    where job-hdr.company eq cocode
      and job-hdr.job     eq job.job
      and job-hdr.job-no  eq job.job-no
      and job-hdr.job-no2 eq job.job-no2:

  job-hdr.opened = job.opened.

  {util/mkjobkey.i}
end.
