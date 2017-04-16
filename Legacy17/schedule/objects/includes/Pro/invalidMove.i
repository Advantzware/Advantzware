/* invalidMove.i - used in include jobEndMove.i */

/*------------------------------------------------------------------------------
  Purpose:  check to see if a moving job is invalid because prior
    Notes:  resource sequence exists
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lvDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvDateTime AS DECIMAL NO-UNDO.

  DEFINE BUFFER ttblJobBuff FOR ttblJob.

  FIND FIRST ttblJobBuff NO-LOCK
       WHERE ttblJobBuff.job EQ ipJob
         AND ttblJobBuff.resourceSequence LT ipResourceSequence
         AND ttblJobBuff.endDateTime GT ipStartDateTime
         AND ROWID(ttblJobBuff) NE ipRowID
         AND (completedHide EQ NO
          OR ttblJobBuff.jobCompleted EQ NO) NO-ERROR.
  IF AVAILABLE ttblJobBuff THEN
  DO:
    rtnValue = YES.
    IF ttblJobBuff.lagTime NE 0 THEN
    DO:
      RUN addTime (ttblJobBuff.startDate,ttblJobBuff.startTime,ttblJobBuff.lagTime,
                   OUTPUT lvDate,OUTPUT lvTime).
      ASSIGN
        lvDateTime = numericDateTime(lvDate,lvTime)
        rtnValue = lvDateTime GT ipStartDateTime.
    END. /* lagtime ne 0 */
  END. /* avail ttbljobbuff */
  RETURN rtnValue.
  /*
  RETURN CAN-FIND(FIRST ttblJob
                  WHERE ttblJob.job EQ ipJob
                    AND ttblJob.resourceSequence LT ipResourceSequence
                    AND ttblJob.endDateTime GT ipStartDateTime
                    AND ROWID(ttblJob) NE ipRowID
                    AND (completedHide EQ NO
                     OR ttblJob.jobCompleted EQ NO)).
  */
