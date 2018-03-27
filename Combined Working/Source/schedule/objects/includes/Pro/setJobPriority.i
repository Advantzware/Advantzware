/* setJobPriority.i - used in procedure setJobPriority in board.i & resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:     set all jobs priority values prior to resetting date/time
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH buffJob EXCLUSIVE-LOCK:
    IF buffJob.jobLocked OR buffJob.jobCompleted THEN NEXT.
    buffJob.priority-1 = calcPriority(1,buffJob.resource,buffJob.jobSequence,buffJob.resourceSequence).
    buffJob.priority-2 = calcPriority(2,buffJob.resource,buffJob.jobSequence,buffJob.resourceSequence).
    buffJob.priority-3 = calcPriority(3,buffJob.resource,buffJob.jobSequence,buffJob.resourceSequence).
    ASSIGN
      buffJob.startDateTime = ?
      buffJob.endDateTime = ?.
  END. /* each buffjob */
