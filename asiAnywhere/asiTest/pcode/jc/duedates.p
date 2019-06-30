DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF job.

FIND job-hdr WHERE ROWID(job-hdr) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL job-hdr THEN
FOR EACH job
    WHERE job.company   EQ job-hdr.company
      AND job.job       EQ job-hdr.job
      AND job.job-no    EQ job-hdr.job-no
      AND job.job-no2   EQ job-hdr.job-no2
    EXCLUSIVE:

  IF job.due-date GT job-hdr.due-date OR
     job.due-date EQ ?                OR
     job.due-date LT 01/01/0001       OR
     job.due-date GT 12/31/9999       THEN
    job.due-date = job-hdr.due-date.

  RUN est/updprep.p (ROWID(job)).

  LEAVE.
END.
