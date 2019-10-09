/* jobByResource.p */

&SCOPED-DEFINE printProgramName jobByResource
&SCOPED-DEFINE printProgramTitle Jobs By Resource Report
&SCOPED-DEFINE useTable ttblJob

{schedule/scopDir.i}
{{&print}/includes/rptLayout.i}
{{&print}/includes/resourceFrame.i}

RUN ttblSortByBuild.

{{&print}/includes/outputTo.i}
RUN buildWrkRpt.
FOR EACH ttblJob NO-LOCK WHERE {{&print}/includes/filterWhere.i}
                           AND ttblJob.jobCompleted EQ NO
    BREAK BY ttblJob.department BY ttblJob.resource BY ttblJob.sortBy WITH STREAM-IO:
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
  END. /* if last-of */
END. /* each ttbljob */
HIDE FRAME resourceFrame.
{{&print}/includes/outputClose.i}

PROCEDURE ttblSortByBuild:
  FOR EACH ttblJob NO-LOCK:
    ttblJob.sortBy = sortBy().
  END.
END PROCEDURE.
