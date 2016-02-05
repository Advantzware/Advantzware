/* newJobSequence.i - used in procedure moveExisting in boardProc.i & resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:     set job sequence of selected job
  Parameters:  job row id, resource, start date & time, output start/end/add sequence
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipStartDateTime AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipJobSequence AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipConflict AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipBoardDatePrompt AS LOGICAL NO-UNDO.

  DEFINE OUTPUT PARAMETER opStartSequence AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opEndSequence AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opAddSequence AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvLast AS LOGICAL NO-UNDO INITIAL YES.

  DEFINE BUFFER bJob FOR ttblJob.

  FIND bJob NO-LOCK WHERE ROWID(bJob) EQ ipRowID.
  
  FOR EACH buffJob NO-LOCK
      WHERE buffJob.resource EQ ipResource
        AND ROWID(buffJob) NE ipRowID BY buffJob.jobSequence:
    
    IF buffJob.jobLocked OR buffJob.jobCompleted OR
       buffJob.endDateTime LE ipStartDateTime THEN NEXT.
    
    IF ipConflict THEN
    DO:
      IF bJob.resourceSequence NE 1 THEN
      DO:
        RUN getPriorJobResource (bJob.job,bJob.resourceSequence,buffJob.startDateTime,
                                 INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime).
        lvStartDateTime = numericDateTime(lvStartDate,lvStartTime).
      END.
      IF lvStartDateTime LT buffJob.startDateTime THEN
      ASSIGN
        lvStartDate = buffJob.startDate
        lvStartTime = buffJob.startTime.
    END. /* if ipconflict */
    
    IF ipBoardDatePrompt THEN
    ASSIGN
      lvStartDate = pixelDate(ipStartDateTime)
      lvStartTime = pixelTime(ipStartDateTime).
    
    IF ipConflict OR ipBoardDatePrompt THEN
    DO:
      RUN newEnd (bJob.timeSpan,lvStartDate,lvStartTime,
                  OUTPUT lvEndDate,OUTPUT lvEndTime).
      ASSIGN
        bJob.startDate = lvStartDate
        bJob.startTime = lvStartTime
        bJob.endDate = lvEndDate
        bJob.endTime = lvEndTime.
    END. /* if ipconflict or ipboarddateprompt */
    
    /* assume moving back in time */
    ASSIGN
      bJob.jobSequence = buffJob.jobSequence
      opStartSequence = bJob.jobSequence
      opEndSequence = ipJobSequence
      opAddSequence = 1.
    
    /* check if moving forward in time */
    IF bJob.jobSequence GT ipJobSequence THEN
    ASSIGN
      bJob.jobSequence = bJob.jobSequence - 1
      opStartSequence = ipJobSequence
      opEndSequence = bJob.jobSequence
      opAddSequence = -1.
    
    lvLast = NO.
    LEAVE.
  END. /* each buffjob */

  IF lvLast THEN
  DO:
    ASSIGN
      lvStartDate = pixelDate(ipStartDateTime)
      lvStartTime = pixelTime(ipStartDateTime).
    FIND LAST buffJob NO-LOCK WHERE buffJob.resource EQ ipResource
                                AND ROWID(buffJob) NE ipRowID NO-ERROR.
    IF NOT ipBoardDatePrompt AND AVAILABLE buffJob THEN
    ASSIGN
      lvStartDate = buffJob.endDate
      lvStartTime = buffJob.endTime.
    RUN newEnd (bJob.timeSpan,lvStartDate,lvStartTime,
                OUTPUT lvEndDate,OUTPUT lvEndTime).
    ASSIGN
      bJob.startDate = lvStartDate
      bJob.startTime = lvStartTime
      bJob.endDate = lvEndDate
      bJob.endTime = lvEndTime
      bJob.jobSequence = IF AVAILABLE buffJob THEN buffJob.jobSequence + 1
                         ELSE bJob.jobSequence
      opStartSequence = bJob.jobSequence
      opEndSequence = ipJobSequence
      opAddSequence = 1.
  END. /* lvlast */
