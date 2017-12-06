&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME job-hdr

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

DEF BUFFER b-job-hdr FOR {&TABLENAME}.
DEF BUFFER exc-job-hdr FOR {&TABLENAME}.

{methods/triggers/write.i}

DEF VAR v-full-cost AS LOG NO-UNDO.

{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DISABLE TRIGGERS FOR LOAD OF job.
DISABLE TRIGGERS FOR LOAD OF oe-ordl.
DISABLE TRIGGERS FOR LOAD OF itemfg.

{&TABLENAME}.est-no = FILL(" ",8 - LENGTH(TRIM({&TABLENAME}.est-no))) +
                      TRIM({&TABLENAME}.est-no).

DO WHILE {&TABLENAME}.j-no GT 0 AND
         CAN-FIND(FIRST b-job-hdr
                  WHERE b-job-hdr.j-no   EQ {&TABLENAME}.j-no
                    AND ROWID(b-job-hdr) NE ROWID({&TABLENAME})):
  {&TABLENAME}.j-no = {&TABLENAME}.j-no + 1.
END.

FOR EACH job
    WHERE job.company EQ {&TABLENAME}.company
      AND job.job     EQ {&TABLENAME}.job
    NO-LOCK:
  IF {&TABLENAME}.opened NE job.opened THEN DO:
    {&TABLENAME}.opened = job.opened.
    FOR EACH itemfg
        WHERE itemfg.company EQ {&TABLENAME}.company
          AND itemfg.i-no    EQ {&TABLENAME}.i-no:
      RUN fg/calcqono.p (ROWID(itemfg), OUTPUT itemfg.q-ono).

      RUN fg/chkfgloc.p (INPUT {&TABLENAME}.i-no, INPUT {&TABLENAME}.loc).
      FIND FIRST itemfg-loc 
        WHERE itemfg-loc.company EQ {&TABLENAME}.company
          AND itemfg-loc.i-no    EQ {&TABLENAME}.i-no
          AND itemfg-loc.loc     EQ {&TABLENAME}.loc
        EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL itemfg-loc THEN      
        RUN fg/calcqool.p (ROWID(itemfg), {&TABLENAME}.loc, OUTPUT itemfg-loc.q-ono).
      FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
    END.
  END.

  /* Make sure job-farm was created */
  RUN jc/addJobFarm.p (INPUT job.job).

  LEAVE.
END.

IF {&TABLENAME}.job NE 0 AND TRIM({&TABLENAME}.job-no) NE "" THEN DO:
  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ {&TABLENAME}.company
        AND sys-ctrl.name    EQ "FGOECOST"
      NO-LOCK NO-ERROR.
  v-full-cost = AVAIL sys-ctrl AND sys-ctrl.log-fld.

  IF {&TABLENAME}.sq-in LE 0   OR
     {&TABLENAME}.sq-in GT 100 OR
     {&TABLENAME}.sq-in EQ ?   THEN
    {&TABLENAME}.sq-in = 100.

  RUN jc/duedates.p (ROWID(job-hdr)).

  IF TRIM({&TABLENAME}.i-no) NE "" AND {&TABLENAME}.frm NE 0 THEN
  FOR EACH b-job-hdr NO-LOCK
      WHERE b-job-hdr.company EQ {&TABLENAME}.company
        AND b-job-hdr.job     EQ {&TABLENAME}.job
        AND b-job-hdr.job-no  EQ {&TABLENAME}.job-no
        AND b-job-hdr.job-no2 EQ {&TABLENAME}.job-no2
        AND ROWID(b-job-hdr)  NE ROWID({&TABLENAME}):

    IF b-job-hdr.i-no EQ {&TABLENAME}.i-no AND
       b-job-hdr.frm  EQ {&TABLENAME}.frm  THEN DO:
      FIND exc-job-hdr WHERE ROWID(exc-job-hdr) EQ ROWID(b-job-hdr)
          EXCLUSIVE NO-WAIT NO-ERROR.
      IF AVAIL exc-job-hdr THEN DELETE exc-job-hdr.
    END.
  END.

  IF {&TABLENAME}.std-tot-cost NE ? AND
     {&TABLENAME}.std-tot-cost NE 0 THEN DO:
    FOR EACH fg-rcpth NO-LOCK
        WHERE fg-rcpth.company   EQ {&TABLENAME}.company
          AND fg-rcpth.i-no      EQ {&TABLENAME}.i-no
          AND fg-rcpth.job-no    EQ {&TABLENAME}.job-no
          AND fg-rcpth.job-no2   EQ {&TABLENAME}.job-no2
          AND fg-rcpth.rita-code EQ "R"
        USE-INDEX i-no,

        EACH fg-rdtlh
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        USE-INDEX rm-rdtl:
      fg-rdtlh.cost = {&TABLENAME}.std-tot-cost.
    END.

    FOR EACH fg-bin
        WHERE fg-bin.company EQ {&TABLENAME}.company
          AND fg-bin.i-no    EQ {&TABLENAME}.i-no
          AND fg-bin.job-no  EQ {&TABLENAME}.job-no
          AND fg-bin.job-no2 EQ {&TABLENAME}.job-no2:
      ASSIGN
       fg-bin.std-tot-cost = {&TABLENAME}.std-tot-cost
       fg-bin.std-mat-cost = {&TABLENAME}.std-mat-cost
       fg-bin.std-lab-cost = {&TABLENAME}.std-lab-cost
       fg-bin.std-fix-cost = {&TABLENAME}.std-fix-cost
       fg-bin.std-var-cost = {&TABLENAME}.std-var-cost.
    END.
  END.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
