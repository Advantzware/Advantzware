/* numericalJob.p */

&SCOPED-DEFINE printProgramName numericalJob
&SCOPED-DEFINE printProgramTitle Numerical Job

{schedule/scopDir.i}
{{&print}/includes/printDefs.i}

{{&print}/includes/outputTo.i}
FOR EACH ttblJob NO-LOCK WHERE {{&print}/includes/filterWhere.i}
    BREAK BY ttblJob.jobSort BY ttblJob.resourceSequence
    WITH FRAME f1 STREAM-IO DOWN WIDTH 200:
  IF FIRST-OF(ttblJob.jobSort) THEN
  DISPLAY ttblJob.job.
  DISPLAY
    ttblJob.resourceSequence LABEL 'Seq'
    ttblJob.resource
    INTEGER(ttblJob.userField15) LABEL 'Run Qty'.
  IF LAST-OF(ttblJob.jobSort) THEN
  DOWN 1.
END. /* each ttbljob */
{{&print}/includes/outputClose.i}
