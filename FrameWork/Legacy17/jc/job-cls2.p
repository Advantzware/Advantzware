
def input parameter v-recid as recid.

{sys/inc/var.i shared}

DEF BUFFER b-jc-calc FOR reftable.

def var v-bwt       like item.basis-w                                   no-undo.
def var v-len       like item.s-len                                     no-undo.
def var v-wid       like item.s-wid                                     no-undo.
def var v-up        as   int                                            no-undo.
def var v-on        as   int                                            no-undo.
def var v-out       as   int                                            no-undo.
def var v-qty       as   dec                                            no-undo.
def var v-wst       as   dec                                            no-undo.

DEF TEMP-TABLE tt-jh NO-UNDO LIKE job-hdr.

def TEMP-TABLE w-fed NO-UNDO
  field s-num like job-hdr.frm
  field b-num like job-hdr.blank-no
  field fed   as   dec.

{jc/job-cls3.i NEW}


find job where recid(job) eq v-recid no-lock no-error.

if not avail job then leave.

find first sys-ctrl
    where sys-ctrl.company eq job.company
      and sys-ctrl.name    eq "CLOSEJOB"
    no-lock no-error.

if (not avail sys-ctrl) or (not sys-ctrl.log-fld) then leave.

FIND FIRST est NO-LOCK
    WHERE est.company EQ job.company
      AND est.est-no  EQ job.est-no
    NO-ERROR.

IF AVAIL est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN
FOR EACH b-jc-calc NO-LOCK
    WHERE b-jc-calc.reftable EQ "jc/jc-calc.p"
      AND b-jc-calc.company  EQ job.company
      AND b-jc-calc.loc      EQ ""
      AND b-jc-calc.code     EQ STRING(job.job,"999999999"),
    FIRST job-hdr NO-LOCK
    WHERE job-hdr.company  EQ job.company
      AND job-hdr.job      EQ job.job
      AND job-hdr.job-no   EQ job.job-no
      AND job-hdr.job-no2  EQ job.job-no2:

  ACCUM 1 (TOTAL).

  CREATE tt-jh.
  BUFFER-COPY job-hdr TO tt-jh
  ASSIGN
   tt-jh.j-no     = (ACCUM TOTAL 1)
   tt-jh.i-no     = b-jc-calc.code2
   tt-jh.n-on     = b-jc-calc.val[11]
   tt-jh.frm      = b-jc-calc.val[12]
   tt-jh.blank-no = b-jc-calc.val[13].
END.

ELSE
FOR EACH job-hdr NO-LOCK
    WHERE job-hdr.company  EQ job.company
      AND job-hdr.job      EQ job.job
      AND job-hdr.job-no   EQ job.job-no
      AND job-hdr.job-no2  EQ job.job-no2:

  CREATE tt-jh.
  BUFFER-COPY job-hdr TO tt-jh.
END.

FOR EACH tt-jh:
  CREATE w-fed.
  ASSIGN
   w-fed.s-num = tt-jh.frm
   w-fed.b-num = tt-jh.blank-no
   v-up        = 1
   v-out       = 1.
   
  IF AVAIL est THEN DO:
    RUN sys/inc/numup.p (est.company, est.est-no, tt-jh.frm, OUTPUT v-up).

    FIND FIRST ef NO-LOCK
        WHERE ef.company eq est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no eq tt-jh.frm
        NO-ERROR.
    IF AVAIL ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-out).
  END.
  
  IF tt-jh.n-on NE 0 THEN v-on = tt-jh.n-on.

  FOR EACH job-mat
      WHERE job-mat.company EQ tt-jh.company
        AND job-mat.job     EQ tt-jh.job
        AND job-mat.job-no  EQ tt-jh.job-no
        AND job-mat.job-no2 EQ tt-jh.job-no2
        AND job-mat.frm     EQ tt-jh.frm
      NO-LOCK,

      FIRST item
      WHERE item.company    EQ job-mat.company
        AND item.i-no       EQ job-mat.i-no
        AND item.mat-type   EQ "B"
      NO-LOCK

      BREAK BY job-mat.i-no
            BY job-mat.rec_key:

    IF FIRST-OF(job-mat.i-no) THEN
    FOR EACH mat-act
        WHERE mat-act.company EQ job-mat.company
          AND mat-act.job     EQ job-mat.job
          AND mat-act.job-no  EQ job-mat.job-no
          AND mat-act.job-no2 EQ job-mat.job-no2
          AND mat-act.i-no    EQ job-mat.i-no
          AND mat-act.s-num   EQ job-mat.frm
        USE-INDEX job NO-LOCK:

      ASSIGN
       v-bwt = job-mat.basis-w
       v-len = job-mat.len
       v-wid = job-mat.wid.

      IF v-len EQ 0 THEN v-len = item.s-len.

      IF v-wid EQ 0 THEN
        v-wid = IF item.r-wid NE 0 THEN item.r-wid ELSE item.s-wid.

      IF v-bwt EQ 0 THEN v-bwt = item.basis-w.

      IF job-mat.qty-uom EQ "EA" THEN
        v-qty = mat-act.qty.
      ELSE
        RUN sys/ref/convquom.p(job-mat.qty-uom, "EA",
                               v-bwt, v-len, v-wid, item.s-dep,
                               mat-act.qty, OUTPUT v-qty).

      {sys/inc/roundup.i v-qty}
    
      /*v-on = if job-mat.n-up ne 0 then job-mat.n-up else (v-up * v-out).*/

      w-fed.fed = w-fed.fed + (v-qty * v-on * v-out).
    END.
  END.
END.

RUN jc/job-cls3.p (BUFFER job).

v-qty = 0.

FOR EACH w-qty
    BREAK BY w-qty.s-num
          BY w-qty.b-num
          BY w-qty.seq
          BY w-qty.pass:

  IF FIRST-OF(w-qty.b-num) THEN DO:
    v-qty = 0.

    FIND FIRST w-fed
        WHERE w-fed.s-num EQ w-qty.s-num
          AND w-fed.b-num EQ w-qty.b-num
        NO-ERROR.
    IF AVAIL w-fed THEN v-qty = w-fed.fed.
  END.

  IF w-qty.fin + w-qty.wst GT v-qty THEN v-qty = w-qty.fin + w-qty.wst.

  ASSIGN
   w-qty.fed = v-qty
   w-qty.wst = w-qty.fed - w-qty.fin
   v-qty     = w-qty.fin.

  IF w-qty.p-type EQ 0 THEN w-qty.b-num = 0.
END.

ASSIGN
 v-on  = 0
 v-wst = 0.

FOR EACH w-qty WHERE w-qty.p-type EQ 0
    BREAK BY w-qty.s-num
          BY w-qty.b-num
          BY w-qty.seq
          BY w-qty.dept
          BY w-qty.pass
          BY w-qty.wst:

  /*ASSIGN
   v-on  = v-on + w-qty.n-on
   v-wst = v-wst + w-qty.wst.

  IF LAST-OF(w-qty.pass) THEN
    ASSIGN
     w-qty.n-on = v-on
     w-qty.wst  = v-wst 
     v-wst      = 0.

  ELSE DELETE w-qty.*/
  IF NOT LAST-OF(w-qty.pass) THEN DELETE w-qty.
END.

for each w-qty:
  for each mch-act
      where mch-act.company  eq job.company
        and mch-act.job      eq job.job
        and mch-act.job-no   eq job.job-no
        and mch-act.job-no2  eq job.job-no2
        and mch-act.frm      eq w-qty.s-num
        and mch-act.blank-no eq w-qty.b-num
        and mch-act.dept     eq w-qty.dept
        and mch-act.pass     eq w-qty.pass,

      first job-code
      where job-code.code eq mch-act.code
      no-lock

      break by mch-act.op-date
            by mch-act.op-time:

    if job-code.cat eq "MR" then
      w-qty.wst = w-qty.wst - (mch-act.qty * w-qty.n-on).

    w-qty.wst = w-qty.wst - (mch-act.waste * w-qty.n-on).
    
    if last(mch-act.op-date) then
      mch-act.waste = mch-act.waste + (w-qty.wst / w-qty.n-on).
  end.
end.
