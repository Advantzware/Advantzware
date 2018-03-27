/* pendingJob.p */

&SCOPED-DEFINE printProgramName pendingJobs
&SCOPED-DEFINE printProgramTitle Pending Jobs Report
&SCOPED-DEFINE useTable pendingJob

{schedule/scopDir.i}
{{&print}/includes/rptLayout.i}
{{&viewers}/includes/setPendingJob.i}

/* override setting in printDefs.i */
&GLOBAL-DEFINE jobTable pendingJob

RUN ttblSortByBuild.

{{&print}/includes/outputTo.i}
RUN buildWrkRpt.
RUN setPendingJob.
FOR EACH pendingJob NO-LOCK WHERE {{&print}/includes/filterWhere.i}
    BREAK BY pendingJob.sortBy BY pendingJob.jobSort BY pendingJob.resourceSequence
    {{&print}/includes/framePhrase.i} DOWN:
  RUN buildLines (FIRST(pendingJob.jobSort)).
  IF ipExcel THEN PUT UNFORMATTED SKIP.
  IF LAST-OF(pendingJob.sortBy) AND NOT ipExcel THEN
  DOWN 1.
END. /* each pendingJob */
{{&print}/includes/outputClose.i}

PROCEDURE ttblSortByBuild:
  FOR EACH pendingJob NO-LOCK:
    pendingJob.sortBy = sortBy().
  END.
END PROCEDURE.
