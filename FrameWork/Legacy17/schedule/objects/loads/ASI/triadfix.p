DEFINE VARIABLE dueDate AS DATE NO-UNDO.

FOR EACH job-hdr NO-LOCK WHERE job-hdr.company EQ '001'
                           AND job-hdr.opened EQ YES
                         BREAK BY job-hdr.job-no BY job-hdr.job-no2:
  IF FIRST-OF(job-hdr.job-no2) THEN
  FOR EACH job OF job-hdr NO-LOCK,
      EACH job-mch WHERE job-mch.company EQ job.company
                             AND job-mch.job EQ job.job
                             AND job-mch.job-no EQ job.job-no
                             AND job-mch.job-no2 EQ job.job-no2
                             AND job-mch.run-complete EQ NO
      BREAK BY job.job BY job-mch.frm BY job-mch.line:
    dueDate = IF job.due-date NE ? THEN job.due-date ELSE 12.31.2049.
    FIND FIRST oe-ordl NO-LOCK
         WHERE oe-ordl.company EQ job-mch.company
           AND oe-ordl.job-no EQ job-mch.job-no
           AND oe-ordl.job-no2 EQ job-mch.job-no2 
           AND oe-ordl.i-no EQ job-mch.i-no NO-ERROR.
    IF NOT AVAILABLE oe-ordl OR oe-ordl.req-date EQ ? THEN
    DO:
      FIND FIRST oe-ord NO-LOCK
           WHERE oe-ord.company EQ job-mch.company
             AND oe-ord.job-no EQ job-mch.job-no
             AND oe-ord.job-no2 EQ job-mch.job-no2 NO-ERROR.
      IF NOT AVAILABLE oe-ord THEN
      DO:
        FIND FIRST oe-ordl NO-LOCK
             WHERE oe-ordl.company EQ job-mch.company
               AND oe-ordl.job-no EQ job-mch.job-no
               AND oe-ordl.job-no2 EQ job-mch.job-no2 NO-ERROR.
        IF AVAILABLE oe-ordl THEN
        FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
      END. /* not avail oe-ord */
      IF AVAILABLE oe-ord THEN
          dueDate = oe-ord.due-date.
    END.
    ELSE
    dueDate = oe-ordl.req-date.
    IF job-mch.job-no BEGINS 'w' OR dueDate GE 8.1.2007 THEN NEXT.
    job-mch.run-complete = YES.
  END. /* each job, each job-mch */
END. /* each job-hdr */
