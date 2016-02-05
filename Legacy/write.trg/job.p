&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME job

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}


DISABLE TRIGGERS FOR LOAD OF job-hdr.

ASSIGN
 {&TABLENAME}.opened  = LOOKUP({&TABLENAME}.stat,"C,Z") EQ 0
 {&TABLENAME}.user-id = USERID("nosweat")
 {&TABLENAME}.est-no  = FILL(" ",8 - LENGTH(TRIM({&TABLENAME}.est-no))) +
                        TRIM({&TABLENAME}.est-no).

IF {&TABLENAME}.opened AND {&TABLENAME}.start-date GT TODAY + 180 THEN
  {&TABLENAME}.start-date = TODAY + 180.

find first sys-ctrl WHERE
     sys-ctrl.company eq {&TABLENAME}.company AND
     sys-ctrl.name    eq "JOBDATESMAX" AND
     sys-ctrl.log-fld EQ YES
     no-lock no-error.

IF AVAIL sys-ctrl THEN
DO:
   IF {&TABLENAME}.opened AND
      {&TABLENAME}.due-date GT TODAY + sys-ctrl.int-fld THEN
      {&TABLENAME}.due-date = TODAY + sys-ctrl.int-fld.
END.
ELSE
IF {&TABLENAME}.opened AND {&TABLENAME}.due-date GT TODAY + 180 THEN
  {&TABLENAME}.due-date = TODAY + 180.

FOR EACH job-hdr
    WHERE job-hdr.company EQ {&TABLENAME}.company
      AND job-hdr.job     EQ {&TABLENAME}.job
      AND job-hdr.job-no  EQ {&TABLENAME}.job-no
      AND job-hdr.job-no2 EQ {&TABLENAME}.job-no2
    EXCLUSIVE
    BREAK BY job-hdr.job:

  IF {&TABLENAME}.start-date NE old-{&TABLENAME}.start-date THEN
    job-hdr.start-date = {&TABLENAME}.start-date.

  IF {&TABLENAME}.due-date NE old-{&TABLENAME}.due-date THEN
    job-hdr.due-date = {&TABLENAME}.due-date.

  IF job-hdr.opened NE {&TABLENAME}.opened THEN DO:
    job-hdr.opened = {&TABLENAME}.opened.
    FOR EACH itemfg
        WHERE itemfg.company EQ job-hdr.company
          AND itemfg.i-no    EQ job-hdr.i-no:
      RUN fg/calcqono.p (ROWID(itemfg), OUTPUT itemfg.q-ono).

      FIND FIRST itemfg-loc 
        WHERE itemfg-loc.company EQ itemfg.company
          AND itemfg-loc.i-no    EQ itemfg.i-no
          AND itemfg-loc.loc     EQ job-hdr.loc
        EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL itemfg-loc THEN         
        RUN fg/calcqool.p (ROWID(itemfg), job-hdr.loc, OUTPUT itemfg-loc.q-ono).
      FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
    END.
  END.

  IF FIRST(job-hdr.job)                                 AND
     LAST(job-hdr.job)                                  AND
     {&TABLENAME}.due-date NE old-{&TABLENAME}.due-date AND
     job-hdr.ord-no EQ 0                                THEN
    job-hdr.due-date = {&TABLENAME}.due-date.
END.

IF {&TABLENAME}.start-date NE old-{&TABLENAME}.start-date THEN
  RUN est/updprep.p (ROWID({&TABLENAME})).

IF NOT {&TABLENAME}.opened THEN RUN jc/jobnotes.p (BUFFER {&TABLENAME}).

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
