&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME job-mch

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEF BUFFER b-job FOR job.

DEF VAR ld AS DEC NO-UNDO.
DEF VAR ld-tot AS DEC NO-UNDO.
DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR lv-format-f AS CHAR NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF job-mat.

cocode = {&TABLENAME}.company.

IF cocode NE "" THEN DO:
  {sys/inc/jobcard.i "F"}
  lv-format-f = sys-ctrl.char-fld.
END.

/*IF {&TABLENAME}.m-code NE "" THEN
FOR EACH mach
    WHERE mach.company EQ job-mch.company
      AND mach.m-code  EQ job-mch.m-code
    NO-LOCK,
    FIRST reftable
    WHERE reftable.reftable EQ "mach.sch-m-code"
      AND reftable.company  EQ mach.company
      AND reftable.loc      EQ mach.loc
      AND reftable.code     EQ mach.m-code
      AND reftable.code2    NE mach.m-code
      AND reftable.code2    NE ""
    NO-LOCK:
  {&TABLENAME}.m-code = reftable.code2.
  LEAVE.
END.*/

/* rtark - 10.28.2016 */
IF {&TABLENAME}.spare-char-1 EQ "" THEN
{&TABLENAME}.spare-char-1 = {&TABLENAME}.m-code.

IF {&TABLENAME}.start-date GT TODAY + 180 THEN
  {&TABLENAME}.start-date = TODAY + 180.

IF {&TABLENAME}.end-date GT TODAY + 180 THEN
  {&TABLENAME}.end-date = TODAY + 180.

IF {&TABLENAME}.start-date-su GT TODAY + 180 THEN
  {&TABLENAME}.start-date-su = TODAY + 180.

IF {&TABLENAME}.end-date-su GT TODAY + 180 THEN
  {&TABLENAME}.end-date-su = TODAY + 180.

IF {&TABLENAME}.start-date-su EQ ? AND
   {&TABLENAME}.start-date    NE ? THEN
  {&TABLENAME}.start-date-su = {&TABLENAME}.start-date.

IF {&TABLENAME}.start-date-su EQ {&TABLENAME}.start-date    AND
   ({&TABLENAME}.start-time LT {&TABLENAME}.start-time-su OR
    {&TABLENAME}.start-time-su EQ 0)                        THEN
  {&TABLENAME}.start-time-su = {&TABLENAME}.start-time.

IF {&TABLENAME}.start-date-su NE old-{&TABLENAME}.start-date-su OR
   {&TABLENAME}.start-date    NE old-{&TABLENAME}.start-date    THEN
FOR EACH job
    WHERE job.company EQ {&TABLENAME}.company
      AND job.job     EQ {&TABLENAME}.job
      AND job.job-no  EQ {&TABLENAME}.job-no
      AND job.job-no2 EQ {&TABLENAME}.job-no2
    NO-LOCK:
  RUN jc/startdat.p (ROWID(job)).
  LEAVE.
END.
IF lv-format-f NE "Frankstn"                                         AND
   old-{&TABLENAME}.company NE ""                                    AND
   {&TABLENAME}.run-qty NE old-{&TABLENAME}.run-qty                  AND
   PROGRAM-NAME(2) NE "jc/shtcalc.p"                                 AND
   NOT CAN-FIND(FIRST b-{&TABLENAME}
                WHERE b-{&TABLENAME}.company EQ {&TABLENAME}.company
                  AND b-{&TABLENAME}.job     EQ {&TABLENAME}.job
                  AND b-{&TABLENAME}.job-no  EQ {&TABLENAME}.job-no
                  AND b-{&TABLENAME}.job-no2 EQ {&TABLENAME}.job-no2
                  AND b-{&TABLENAME}.frm     EQ {&TABLENAME}.frm
                  AND b-{&TABLENAME}.line    LT {&TABLENAME}.line
                  AND ROWID(b-{&TABLENAME})  NE ROWID({&TABLENAME})) THEN DO:

  ld-tot = 0.
  FOR EACH job-mat FIELDS(company rm-i-no qty n-up)
      WHERE job-mat.company EQ {&TABLENAME}.company
        AND job-mat.job     EQ {&TABLENAME}.job
        AND job-mat.job-no  EQ {&TABLENAME}.job-no
        AND job-mat.job-no2 EQ {&TABLENAME}.job-no2
        AND job-mat.frm     EQ {&TABLENAME}.frm
        NO-LOCK,
      FIRST item
      WHERE item.company EQ job-mat.company
        AND item.i-no    EQ job-mat.rm-i-no
        AND INDEX("1234BPR",item.mat-type) GT 0
        AND item.i-code  EQ "R"
      NO-LOCK:
    ld-tot = ld-tot + (job-mat.qty * job-mat.n-up).
  END.

  RELEASE job-mat.

  FOR EACH job-mat
      WHERE job-mat.company EQ {&TABLENAME}.company
        AND job-mat.job     EQ {&TABLENAME}.job
        AND job-mat.job-no  EQ {&TABLENAME}.job-no
        AND job-mat.job-no2 EQ {&TABLENAME}.job-no2
        AND job-mat.frm     EQ {&TABLENAME}.frm,
      FIRST item
      WHERE item.company EQ job-mat.company
        AND item.i-no    EQ job-mat.rm-i-no
        AND INDEX("1234BPR",item.mat-type) GT 0
      NO-LOCK:

    ld = {&TABLENAME}.run-qty * (IF item.i-code EQ "R" THEN
                                   ((job-mat.qty * job-mat.n-up) / ld-tot)
                                 ELSE 1).

    IF job-mat.qty-uom EQ "EA" THEN
      {sys/inc/roundup.i ld}
    ELSE
      RUN sys/ref/convquom.p("EA", job-mat.qty-uom,
                             job-mat.basis-w, job-mat.len, job-mat.wid, item.s-dep,
                             {&TABLENAME}.run-qty, OUTPUT ld).
    
    ASSIGN 
     job-mat.std-cost = job-mat.std-cost * (job-mat.qty / ld).
     job-mat.qty      = ld.
  END.
END.

FIND FIRST b-job WHERE
     b-job.company EQ {&TABLENAME}.company AND
     b-job.job     EQ {&TABLENAME}.job
     EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL b-job THEN
   b-job.user-id = USERID("NOSWEAT").

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
