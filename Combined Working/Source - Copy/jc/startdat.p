
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DEF VAR ldt AS DATE NO-UNDO.
DEF VAR ll AS LOG INIT NO NO-UNDO.
DEF VAR v-jobstd AS LOG NO-UNDO.
DEF VAR v-index AS INT NO-UNDO.

DEF temp-table w-date FIELD w-date1 LIKE job.start-date
                      FIELD w-date2 LIKE w-date1.

FIND job WHERE ROWID(job) EQ ip-rowid EXCLUSIVE-LOCK NO-ERROR.

DO v-index = 1 TO 5:

   IF PROGRAM-NAME(v-index) MATCHES "*jobstds*" THEN
   DO:
      v-jobstd = YES.
      LEAVE.
   END.
END.

IF AVAIL job THEN DO:
  FOR EACH job-mch
      WHERE job-mch.company        EQ job.company
        AND job-mch.job            EQ job.job
        AND job-mch.job-no         EQ job.job-no
        AND job-mch.job-no2        EQ job.job-no2
        AND job-mch.anchored       EQ YES
        AND (job-mch.start-date-su NE ? OR
             job-mch.start-date    NE ?)
      BREAK BY 1:

    ll = YES.

    IF job-mch.start-date-su NE ? THEN DO:
      CREATE w-date.
      w-date1 = job-mch.start-date-su.
    END.

    IF job-mch.start-date NE ? THEN DO:
      CREATE w-date.
      w-date1 = job-mch.start-date.
    END.
  END.

  IF NOT ll AND NOT v-jobstd THEN DO:
    FOR EACH job-hdr FIELDS(company ord-no job-no job-no2 i-no job)
        WHERE job-hdr.company EQ job.company
          AND job-hdr.job     EQ job.job
          AND job-hdr.job-no  EQ job.job-no
          AND job-hdr.job-no2 EQ job.job-no2
          AND job-hdr.ord-no  NE 0
        NO-LOCK,

        FIRST oe-ordl FIELDS(company ord-no i-no)
        WHERE oe-ordl.company EQ job-hdr.company
          AND oe-ordl.ord-no  EQ job-hdr.ord-no
          AND oe-ordl.job-no  EQ job-hdr.job-no
          AND oe-ordl.job-no2 EQ job-hdr.job-no2
          AND oe-ordl.i-no    EQ job-hdr.i-no
        NO-LOCK,

        FIRST oe-ord FIELDS(prod-date lead-days ord-date due-date)
        WHERE oe-ord.company EQ oe-ordl.company
          AND oe-ord.ord-no  EQ oe-ordl.ord-no
        NO-LOCK
    
        BREAK BY job-hdr.job:

      CREATE w-date.
      w-date2 = oe-ord.ord-date.

      IF oe-ord.prod-date NE ? THEN w-date1 = oe-ord.prod-date.

      ELSE DO:
        FIND FIRST itemfg
            WHERE itemfg.company EQ oe-ordl.company
              AND itemfg.i-no    EQ oe-ordl.i-no
            NO-LOCK NO-ERROR.

        w-date1 = oe-ord.due-date - (IF AVAIL itemfg THEN itemfg.lead-days
                                                 ELSE oe-ord.lead-days).
      END.
    END.

    FOR EACH w-date BREAK BY w-date2 DESC:
      IF FIRST(w-date2) THEN ldt = w-date2.
      IF w-date1 LT ldt THEN w-date1 = ldt.
    END.
  END.

  FIND sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ job.company
                          AND sys-ctrl.name EQ 'Schedule' NO-ERROR.
  IF NOT AVAIL sys-ctrl OR
     NOT (sys-ctrl.char-fld EQ 'NoDate' AND sys-ctrl.log-fld) THEN
  FOR EACH w-date BY w-date1:
    job.start-date = w-date1.
    LEAVE.
  END.
END.
