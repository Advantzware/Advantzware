/* setPendingJob.i */

DEFINE TEMP-TABLE jobID NO-UNDO
  FIELD job LIKE pendingJob.job
  FIELD jobSort LIKE pendingJob.jobSort
  FIELD jobSelected AS LOGICAL
  FIELD dueDate LIKE pendingJob.dueDate
    INDEX jobSort IS PRIMARY UNIQUE jobSort
    INDEX jobID IS UNIQUE job
    INDEX dueDate dueDate.

PROCEDURE newEnd:
  {{&includes}/{&Board}/newEnd.i}
END PROCEDURE.

PROCEDURE newStart:
  {{&includes}/{&Board}/newStart.i}
END PROCEDURE.

PROCEDURE setPendingJob:
  DEFINE VARIABLE priorEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE priorEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE priorTimeSpan AS INTEGER NO-UNDO.

  RUN getConfiguration.
  EMPTY TEMP-TABLE jobID.
  FOR EACH pendingJob EXCLUSIVE-LOCK
      BREAK BY pendingJob.job DESCENDING
            BY pendingJob.resourceSequence DESCENDING:
    IF FIRST-OF(pendingJob.job) THEN
    DO:
      CREATE jobID.
      ASSIGN
        jobID.job = pendingJob.job
        jobID.jobSort = pendingJob.jobSort
        jobID.dueDate = pendingJob.dueDate
        priorEndDate = pendingJob.dueDate
        priorEndTime = pendingJob.dueTime
        priorTimeSpan = pendingDays * 86400.
      IF pendingDays EQ 1 THEN priorTimeSpan = priorTimeSpan + 1.
    END.
    RUN newStart (priorTimeSpan,priorEndDate,priorEndTime,
                  OUTPUT pendingJob.endDate,OUTPUT pendingJob.endTime).
    RUN newStart (pendingjob.origStartTime,pendingJob.endDate,pendingJob.endTime,
                  OUTPUT pendingJob.startDate,OUTPUT pendingJob.startTime).
    ASSIGN
      pendingJob.timeSpan = pendingJob.origStartTime
      priorEndDate = pendingJob.startDate
      priorEndTime = pendingJob.startTime
      priorTimeSpan = 0.
  END. /* each pendingjob */
END PROCEDURE.
