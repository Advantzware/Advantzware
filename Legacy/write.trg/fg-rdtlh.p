&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME fg-rdtlh

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.

DEF VAR ll AS LOG NO-UNDO.


FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ {&TABLENAME}.r-no NO-ERROR.

IF AVAIL fg-rcpth            AND
   fg-rcpth.job-no NE ""     AND
   fg-rcpth.rita-code EQ "R" THEN DO:
  FIND FIRST job-hdr NO-LOCK
      WHERE job-hdr.company EQ fg-rcpth.company
        AND job-hdr.i-no    EQ fg-rcpth.i-no
        AND job-hdr.job-no  EQ fg-rcpth.job-no
        AND job-hdr.job-no2 EQ fg-rcpth.job-no2
      NO-ERROR.

  IF NOT AVAIL job-hdr THEN DO:
    FIND FIRST job NO-LOCK
        WHERE job.company EQ fg-rcpth.company
          AND job.job-no  EQ fg-rcpth.job-no
          AND job.job-no2 EQ fg-rcpth.job-no2
        NO-ERROR.
    IF AVAIL job THEN
    FIND FIRST reftable NO-LOCK
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job.job,"999999999")
          AND reftable.code2    EQ fg-rcpth.i-no
        NO-ERROR.
  END.
       
  IF AVAIL job-hdr AND job-hdr.std-tot-cost GT 0 THEN
    ASSIGN
     fg-rcpth.pur-uom  = "M"
     {&TABLENAME}.cost = job-hdr.std-tot-cost.
  ELSE
  IF AVAIL reftable AND reftable.val[5] GT 0 THEN
    ASSIGN
     fg-rcpth.pur-uom  = "M"
     {&TABLENAME}.cost = reftable.val[5].
END.

IF {&TABLENAME}.r-no NE 0 THEN
FOR EACH b-{&TABLENAME}
    WHERE b-{&TABLENAME}.r-no   EQ {&TABLENAME}.r-no
      AND ROWID(b-{&TABLENAME}) NE ROWID({&TABLENAME}):
  BUFFER-COMPARE b-{&TABLENAME} EXCEPT rec_key TO {&TABLENAME} SAVE RESULT IN ll.
  IF ll THEN DELETE b-{&TABLENAME}.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.


