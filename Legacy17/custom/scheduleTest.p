DEFINE VARIABLE scheduleHndl AS HANDLE NO-UNDO.
DEFINE VARIABLE calcDueDate AS DATE NO-UNDO.
DEFINE VARIABLE calcStartDate AS DATE NO-UNDO.

FIND FIRST job NO-LOCK WHERE job.company EQ '001'
                         AND job.job-no EQ '   708'
                         AND job.job-no2 EQ 0 NO-ERROR.
RUN custom/schedule.p PERSISTENT SET scheduleHndl.
RUN scheduleJob IN scheduleHndl (ROWID(job),OUTPUT calcStartDate,OUTPUT calcDueDate).

