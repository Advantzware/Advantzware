/* pPriorAvailable.i - used in procedure pPriorAvailable in board.w,
                       resourceDetail.w & pending.w */

/*------------------------------------------------------------------------------
  Purpose:     find prior available time slot for job being moved
  Parameters:  resource, row id, iop startDateTime & endDateTime,
               op startDate, startTime, endDate and endTime
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipTimeSpan AS INTEGER NO-UNDO.
  
  DEFINE INPUT-OUTPUT PARAMETER iopStartDateTime AS DECIMAL NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopEndDateTime AS DECIMAL NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopStartDate AS DATE NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopStartTime AS INTEGER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopEndDate AS DATE NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopEndTime AS INTEGER NO-UNDO.

  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvTimeSpan AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvDowntimeSpan AS INTEGER NO-UNDO.

  DO WHILE TRUE:
    FIND FIRST boardDowntime NO-LOCK
         WHERE boardDowntime.resource EQ ipResource
           AND boardDowntime.startDateTime LE iopEndDateTime
           AND boardDowntime.endDateTime GT iopEndDateTime NO-ERROR.
    IF AVAILABLE boardDowntime THEN DO:
      ASSIGN
        iopEndDate = boardDowntime.startDate
        iopEndTime = boardDowntime.startTime
        .
      IF boardDowntime.startTime EQ 0 THEN
      ASSIGN
        iopEndDate = iopEndDate - 1
        iopEndTime = 86400
        .
      RUN newStart (ipTimeSpan,iopEndDate,iopEndTime,
                    OUTPUT iopStartDate,OUTPUT iopStartTime).
      ASSIGN
        iopStartDateTime = numericDateTime(iopStartDate,iopStartTime)
        iopEndDateTime = numericDateTime(iopEndDate,iopEndTime)
        .
    END. /* if avail */
    ELSE LEAVE.
  END. /* while true */

  IF checkDowntimeConflict(ipResource,iopStartDateTime,iopEndDateTime) THEN
  RUN pDowntimeSpan (ipResource,ipTimeSpan,iopEndDate,iopEndTime,
                     OUTPUT iopStartDate,OUTPUT iopStartTime,OUTPUT lvDowntimeSpan).
  
  IF checkJobConflict(ipResource,iopStartDateTime,iopEndDateTime,ipRowID) THEN
  FOR EACH buffJob NO-LOCK USE-INDEX DateTimeIdx
      WHERE buffJob.resource EQ ipResource
        AND (buffJob.startDateTime LE iopEndDateTime
         OR buffJob.endDateTime LE iopEndDateTime)
        AND ROWID(buffJob) NE ipRowID
      :
    ASSIGN
      lvEndDate = buffJob.startDate
      lvEndTime = buffJob.startTime
      lvEndDateTime = buffJob.startDateTime
      .
    RUN newStart (ipTimeSpan,lvEndDate,lvEndTime,
                  OUTPUT lvStartDate,OUTPUT lvStartTime).
    RUN pDowntimeSpan (ipResource,ipTimeSpan,iopEndDate,iopEndTime,
                       OUTPUT iopStartDate,OUTPUT iopStartTime,OUTPUT lvDowntimeSpan).
    lvStartDateTime = numericDateTime(lvStartDate,lvStartTime).
    IF checkJobConflict(ipResource,lvStartDateTime,lvEndDateTime,ipRowID) THEN
    NEXT.
    ASSIGN
      iopStartDate = lvStartDate
      iopStartTime = lvStartTime
      iopStartDateTime = lvStartDateTime
      iopEndDate = lvEndDate
      iopEndTime = lvEndTime
      iopEndDateTime = lvEndDateTime
      .
    RETURN.
  END. /* if checkjobconflict */
