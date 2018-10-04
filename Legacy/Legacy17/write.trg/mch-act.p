&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME mch-act

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF VAR li AS INT NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.


IF {&TABLENAME}.qty EQ ? THEN {&TABLENAME}.qty = 0.

IF {&TABLENAME}.company NE "" AND
   {&TABLENAME}.dept NE ""    AND
   {&TABLENAME}.m-code NE ""  THEN DO:
  FIND FIRST mach NO-LOCK
      WHERE mach.company EQ {&TABLENAME}.company
        AND mach.m-code  EQ {&TABLENAME}.m-code
      NO-ERROR.
  IF AVAIL mach THEN DO:
    DO li = 1 TO EXTENT(mach.dept):
      IF {&TABLENAME}.dept EQ mach.dept[li] THEN DO:
        ll = YES.
        LEAVE.
      END.
    END.
    IF NOT ll THEN {&TABLENAME}.dept EQ mach.dept[1].
  END.
END.

IF {&TABLENAME}.op-date NE old-{&TABLENAME}.op-date THEN
FOR EACH job-code WHERE job-code.code EQ {&TABLENAME}.code NO-LOCK,
    EACH job-mch
    WHERE job-mch.company   EQ {&TABLENAME}.company
      AND job-mch.job       EQ {&TABLENAME}.job
      AND job-mch.job-no    EQ {&TABLENAME}.job-no
      AND job-mch.job-no2   EQ {&TABLENAME}.job-no2
      AND job-mch.m-code    EQ {&TABLENAME}.m-code
      AND job-mch.frm       EQ {&TABLENAME}.frm
      AND job-mch.blank-no  EQ {&TABLENAME}.blank-no
      AND job-mch.pass      EQ {&TABLENAME}.pass:

  IF (job-code.cat EQ "RUN" OR job-code.cat EQ "DT") THEN DO:
    IF {&TABLENAME}.op-date LT job-mch.start-date OR
       job-mch.start-date EQ ?                    THEN
      ASSIGN
       job-mch.start-date = {&TABLENAME}.op-date
       job-mch.start-time = {&TABLENAME}.start.

    ELSE
    IF ({&TABLENAME}.op-date EQ job-mch.start-date AND
        {&TABLENAME}.start LT job-mch.start-time)      OR
       job-mch.start-time EQ 0                         THEN
      job-mch.start-time = {&TABLENAME}.start.

    IF {&TABLENAME}.complete THEN
      IF {&TABLENAME}.op-date GT job-mch.end-date OR
         job-mch.end-date EQ ?                    THEN
        ASSIGN
         job-mch.end-date = {&TABLENAME}.op-date
         job-mch.end-time = {&TABLENAME}.stopp.

      ELSE
      IF ({&TABLENAME}.op-date EQ job-mch.start-date AND
          {&TABLENAME}.stopp GT job-mch.end-time)       OR
         job-mch.end-time EQ 0                          THEN
        job-mch.end-time = {&TABLENAME}.stopp.
  END.

  IF job-code.cat EQ "MR" THEN DO:
    IF {&TABLENAME}.op-date LT job-mch.start-date-su OR
       job-mch.start-date-su EQ ?                    THEN
      ASSIGN
       job-mch.start-date-su = {&TABLENAME}.op-date
       job-mch.start-time-su = {&TABLENAME}.start.

    ELSE
    IF ({&TABLENAME}.op-date EQ job-mch.start-date-su AND
        {&TABLENAME}.start LT job-mch.start-time-su)      OR
       job-mch.start-time-su EQ 0                         THEN
      job-mch.start-time-su = {&TABLENAME}.start.

    IF {&TABLENAME}.complete THEN
      IF {&TABLENAME}.op-date GT job-mch.end-date-su OR
         job-mch.end-date-su EQ ?                    THEN
        ASSIGN
         job-mch.end-date-su = {&TABLENAME}.op-date
         job-mch.end-time-su = {&TABLENAME}.stopp.

      ELSE
      IF ({&TABLENAME}.op-date EQ job-mch.start-date-su AND
          {&TABLENAME}.stopp GT job-mch.end-time-su)        OR
         job-mch.end-time-su EQ 0                           THEN
        job-mch.end-time-su = {&TABLENAME}.stopp.
  END.

  job-mch.anchored = YES.
END.
