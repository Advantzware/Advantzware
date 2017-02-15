&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME job-mat

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEF BUFFER b-job FOR job.

DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR lv-format-f AS CHAR NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF job-mch.

cocode = {&TABLENAME}.company.

IF cocode NE "" THEN DO:
  {sys/inc/jobcard.i "F"}
  lv-format-f = sys-ctrl.char-fld.
END.

IF {&TABLENAME}.qty-all EQ 0 AND
   NOT {&TABLENAME}.all-flg  THEN
  {&TABLENAME}.qty-all = {&TABLENAME}.qty - {&TABLENAME}.qty-iss.

IF {&TABLENAME}.qty-all LT 0 THEN {&TABLENAME}.qty-all = 0.

IF {&TABLENAME}.qty-all NE old-{&TABLENAME}.qty-all THEN DO:
  FIND FIRST item EXCLUSIVE-LOCK
      WHERE item.company EQ {&TABLENAME}.company
        AND item.i-no    EQ {&TABLENAME}.rm-i-no
      NO-ERROR NO-WAIT.
  IF AVAIL item THEN RUN rm/calcqcom.p (ROWID(item), OUTPUT item.q-comm).
END.

IF old-{&TABLENAME}.company NE ""           AND
   {&TABLENAME}.qty NE old-{&TABLENAME}.qty AND
    PROGRAM-NAME(2) NE "jc/shtcalc.p" THEN DO:

  /*{&TABLENAME}.std-cost = {&TABLENAME}.std-cost *
                          (old-{&TABLENAME}.qty / {&TABLENAME}.qty).
Removed for task 12161306 - why modify the standard cost (per UOM) if qty changes?               */

  ll = lv-format-f EQ "Frankstn".

  IF NOT ll THEN
  FOR EACH b-{&TABLENAME} NO-LOCK
      WHERE b-{&TABLENAME}.company EQ {&TABLENAME}.company
        AND b-{&TABLENAME}.job     EQ {&TABLENAME}.job
        AND b-{&TABLENAME}.job-no  EQ {&TABLENAME}.job-no
        AND b-{&TABLENAME}.job-no2 EQ {&TABLENAME}.job-no2
        AND b-{&TABLENAME}.frm     EQ {&TABLENAME}.frm
        AND CAN-FIND(FIRST item
                     WHERE item.company EQ b-{&TABLENAME}.company
                       AND item.i-no    EQ b-{&TABLENAME}.rm-i-no
                       AND INDEX("1234BPR",item.mat-type) GT 0
                       AND item.i-code  EQ "R"):
    ll = YES.
    LEAVE.
  END.

  IF ll THEN DO:
    ld = 0.

    IF lv-format-f EQ "Frankstn"           OR
       CAN-FIND(FIRST item
                WHERE item.company EQ {&TABLENAME}.company
                  AND item.i-no    EQ {&TABLENAME}.rm-i-no
                  AND INDEX("1234BPR",item.mat-type) GT 0
                  AND item.i-code  EQ "R") THEN
      ld = {&TABLENAME}.qty * (IF {&TABLENAME}.n-up EQ 0 THEN 1
                               ELSE {&TABLENAME}.n-up).

    FOR EACH b-{&TABLENAME} NO-LOCK
        WHERE b-{&TABLENAME}.company EQ {&TABLENAME}.company
          AND b-{&TABLENAME}.job     EQ {&TABLENAME}.job
          AND b-{&TABLENAME}.job-no  EQ {&TABLENAME}.job-no
          AND b-{&TABLENAME}.job-no2 EQ {&TABLENAME}.job-no2
          AND b-{&TABLENAME}.frm     EQ {&TABLENAME}.frm
          AND ROWID(b-{&TABLENAME})  NE ROWID({&TABLENAME})
          AND CAN-FIND(FIRST item
                       WHERE item.company EQ b-{&TABLENAME}.company
                         AND item.i-no    EQ b-{&TABLENAME}.rm-i-no
                         AND CAN-DO("1,2,3,4,B,P,R",item.mat-type)
                         AND (item.i-code  EQ "R" OR lv-format-f EQ "Frankstn")):
      ld = ld + (b-{&TABLENAME}.qty * (IF b-{&TABLENAME}.n-up EQ 0 THEN 1
                                       ELSE b-{&TABLENAME}.n-up)).
    END.

    RUN jc/machshts.p (ROWID({&TABLENAME}), ld, 0).
  END.

  ELSE
  FOR EACH item
      WHERE item.company EQ {&TABLENAME}.company
        AND item.i-no    EQ {&TABLENAME}.rm-i-no
        AND INDEX("1234BPR",item.mat-type) GT 0
      NO-LOCK,
      EACH job-mch
      WHERE job-mch.company EQ {&TABLENAME}.company
        AND job-mch.job     EQ {&TABLENAME}.job
        AND job-mch.job-no  EQ {&TABLENAME}.job-no
        AND job-mch.job-no2 EQ {&TABLENAME}.job-no2
        AND job-mch.frm     EQ {&TABLENAME}.frm
      BREAK BY job-mch.line:

    IF FIRST(job-mch.line) THEN DO:
      IF job-mat.qty-uom EQ "EA" THEN
        job-mch.run-qty = {&TABLENAME}.qty.
      ELSE
        RUN sys/ref/convquom.p(job-mat.qty-uom, "EA",
                               job-mat.basis-w, job-mat.len, job-mat.wid, item.s-dep,
                               {&TABLENAME}.qty, OUTPUT job-mch.run-qty).

      LEAVE.
    END.
  END.
END.

FIND FIRST b-job WHERE
     b-job.company EQ {&TABLENAME}.company AND
     b-job.job     EQ {&TABLENAME}.job
     EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL b-job THEN
   b-job.user-id = USERID("NOSWEAT").

IF TRIM({&TABLENAME}.rec_key) NE "" THEN DO:
  {custom/fibreaud.i}
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
