/* status.p */

&SCOPED-DEFINE printProgramName status
&SCOPED-DEFINE printProgramTitle Status Report
&SCOPED-DEFINE useTable ttblSortBy

{schedule/scopDir.i}
{{&print}/includes/rptLayout.i}

/* override setting in printDefs.i */
&SCOPED-DEFINE jobTable {&useTable}
&SCOPED-DEFINE jobStartDate dueDate
&SCOPED-DEFINE jobEndDate dueDate

RUN ttblSortByBuild.

{{&print}/includes/outputTo.i}
RUN buildWrkRpt.
FOR EACH ttblSortBy NO-LOCK WHERE {{&print}/includes/filterWhere.i}
    BREAK BY ttblSortBy.sortBy BY ttblSortBy.jobSort
    {{&print}/includes/framePhrase.i}:
  RUN buildLines (FIRST(ttblSortBy.sortBy)).
  {{&print}/includes/jobNotes.i}
END. /* each ttblSortBy */
{{&print}/includes/outputClose.i}

PROCEDURE ttblSortByBuild:
  FOR EACH ttblJob NO-LOCK:
    CREATE ttblSortBy.
    BUFFER-COPY ttblJob EXCEPT jobType TO ttblSortBy
      ASSIGN ttblSortBy.jobType = 'S'.
    ttblSortBy.sortBy = sortBy().
  END.
  FOR EACH pendingJob NO-LOCK:
    CREATE ttblSortBy.
    BUFFER-COPY pendingJob EXCEPT jobType TO ttblSortBy
      ASSIGN ttblSortBy.jobType = 'P'.
    ttblSortBy.sortBy = sortBy().
  END.
END PROCEDURE.
