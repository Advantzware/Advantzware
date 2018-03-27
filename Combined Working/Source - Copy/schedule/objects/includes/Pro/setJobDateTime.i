/* setJobDateTime.i - used in procedure setJobDateTime in board.i & resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:     check all jobs and reset start/end date/time
  Parameters:  ttbljob rowid, board data prompt used
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipBoardDatePrompt AS LOGICAL NO-UNDO.

  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvDowntimeSpan AS INTEGER NO-UNDO.
  DEFINE VARIABLE saveDate AS DATE NO-UNDO.
  DEFINE VARIABLE saveTime AS INTEGER NO-UNDO.
  
  FOR EACH buffJob EXCLUSIVE-LOCK USE-INDEX priority
      WHERE buffJob.startDateTime EQ ?:
    ASSIGN
      lvStartDate = buffJob.startDate
      lvStartTime = buffJob.startTime
      lvStartDateTime = numericDateTime(lvStartDate,lvStartTime).
    IF NOT ipBoardDatePrompt OR ROWID(buffJob) NE ipRowID OR ipRowID EQ ? THEN
    DO:
      IF cascadeJob THEN
      DO:
        RUN getPriorJobResource (buffJob.job,buffJob.resourceSequence,lvStartDateTime,
                                 INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime).
        lvStartDateTime = numericDateTime(lvStartDate,lvStartTime).
      END.
      RUN getPriorJobSequence (buffJob.resource,buffJob.jobSequence,lvStartDateTime,
                               INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime).
      lvStartDateTime = numericDateTime(lvStartDate,lvStartTime).
    END.
    RUN newEnd (buffjob.timeSpan,lvStartDate,lvStartTime,
                OUTPUT lvEndDate,OUTPUT lvEndTime).
    ASSIGN
      saveDate = lvEndDate
      saveTime = lvEndTime.
    RUN downtimeSpan (buffjob.resource,buffjob.timeSpan,lvStartDate,lvStartTime,
                      OUTPUT lvEndDate,OUTPUT lvEndTime,OUTPUT lvDowntimeSpan).
    IF saveDate EQ lvEndDate AND saveTime EQ lvEndTime AND
       CAN-FIND(FIRST boardDowntime
                WHERE boardDowntime.resource EQ buffjob.resource
                  AND boardDowntime.startDatetime LE lvStartDateTime
                  AND boardDowntime.endDateTime GE lvStartDateTime) THEN
    DO:
      lvEndDateTime = numericDateTime(lvEndDate,lvEndTime).
      RUN afterDowntime (buffjob.resource,buffjob.timeSpan,
                        INPUT-OUTPUT lvStartDateTime,INPUT-OUTPUT lvEndDateTime,
                        INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime,
                        INPUT-OUTPUT lvEndDate,INPUT-OUTPUT lvEndTime).
    END.
    ASSIGN
      buffJob.startDate = lvStartDate
      buffJob.startTime = lvStartTime
      buffJob.endDate = lvEndDate
      buffJob.endTime = lvEndTime
      buffJob.downtimeSpan = lvDowntimeSpan.
    buffJob.startDateTime = numericDateTime(buffJob.startDate,buffJob.startTime).
    buffJob.endDateTime = numericDateTime(buffJob.endDate,buffJob.endTime).
  END. /* each buffjob */
