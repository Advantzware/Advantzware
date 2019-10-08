/* asiPrint.i */

{{&print}/includes/rptLayout.i}
{{&print}/includes/resourceFrame.i}
{{&print}/includes/outputTo.i}
RUN buildWrkRpt.
FOR EACH ttblJob NO-LOCK WHERE {{&print}/includes/filterWhere.i}
                           AND ttblJob.jobCompleted EQ NO,
    FIRST job-mch NO-LOCK WHERE ROWID(job-mch) EQ TO-ROWID(ENTRY(2,ttblJob.rowIDs)),
    FIRST mach NO-LOCK WHERE mach.company EQ job-mch.company
                         AND mach.m-code EQ job-mch.m-code
                         AND (CAN-DO(departmentValue,mach.dept[1])
                          OR departmentValue EQ '')
    BREAK BY ttblJob.resource BY ttblJob.jobSequence
    {{&print}/includes/framePhrase.i}:
  {{&print}/includes/resourceExcel.i}
  IF FIRST-OF(ttblJob.resource) THEN
  DO WITH FRAME resourceFrame:
    IF NOT ipExcel THEN
    DISPLAY ttblJob.resource ttblJob.resourceDescription FILL('=',40) @ dashLine. 
    RUN buildLines (YES).
  END. /* do with */
  ELSE
  RUN buildLines (NO).
  {{&print}/includes/jobNotes.i}
  IF LAST-OF(ttblJob.resource) AND NOT ipExcel THEN DO:
    IF linesPerPageValue NE 0 THEN
    VIEW FRAME resourceFrame.
    PAGE.
    HIDE FRAME resourceFrame.
  END.
END. /* each ttbljob */
HIDE FRAME resourceFrame.
{{&print}/includes/outputClose.i}
