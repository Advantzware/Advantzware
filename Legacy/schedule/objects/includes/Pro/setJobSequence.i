/* setJobSequence.i - used in procedure setJobSequence in boardProc.i */

/*------------------------------------------------------------------------------
  Purpose:     set the job sequence value in ttblJob
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE jobSeq AS INTEGER NO-UNDO.
  DEFINE VARIABLE sortSeq AS INTEGER NO-UNDO.

  FOR EACH ttblJob EXCLUSIVE-LOCK
      BREAK BY ttblJob.resource BY ttblJob.startDateTime BY ttblJob.resourceSequence:
    IF FIRST-OF(ttblJob.resource) THEN jobSeq = 0.
    ASSIGN
      jobSeq = jobSeq + 1
      ttblJob.jobSequence = jobSeq
      ttblJob.sequenced = YES.
  END. /* each ttbljob */
  FOR EACH ttblJob EXCLUSIVE-LOCK BY ttblJob.startDateTime BY ttblJob.resourceSequence:
    ASSIGN
      sortSeq = sortSeq + 1
      ttblJob.sortSequence = sortSeq.
  END. /* each ttbljob */
