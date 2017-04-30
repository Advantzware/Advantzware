/* firstAvailable.i - used in procedure firstAvailable in boardProc.i,
                      resourceDetail.w & pending.w */

/*------------------------------------------------------------------------------
  Purpose:     find first available time slot for job being moved
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
           AND boardDowntime.startDateTime LE iopStartDateTime
           AND boardDowntime.endDateTime GT iopStartDateTime NO-ERROR.
    IF AVAILABLE boardDowntime THEN
    DO:
      ASSIGN
        iopStartDate = boardDowntime.endDate
        iopStartTime = boardDowntime.endTime.
      IF boardDowntime.endTime EQ 86400 THEN
      ASSIGN
        iopStartDate = iopStartDate + 1
        iopStartTime = 0.
      RUN newEnd (ipTimeSpan,iopStartDate,iopStartTime,
                  OUTPUT iopEndDate,OUTPUT iopEndTime).
      ASSIGN
        iopStartDateTime = numericDateTime(iopStartDate,iopStartTime)
        iopEndDateTime = numericDateTime(iopEndDate,iopEndTime).
    END. /* if avail */
    ELSE LEAVE.
  END. /* while true */

  IF checkDowntimeConflict(ipResource,iopStartDateTime,iopEndDateTime) THEN
  RUN downtimeSpan (ipResource,ipTimeSpan,iopStartDate,iopStartTime,
                    OUTPUT iopEndDate,OUTPUT iopEndTime,OUTPUT lvDowntimeSpan).
  IF checkJobConflict(ipResource,iopStartDateTime,iopEndDateTime,ipRowID) THEN
  FOR EACH buffJob NO-LOCK USE-INDEX DateTimeIdx
      WHERE buffJob.resource EQ ipResource
        AND (buffJob.startDateTime GE iopStartDateTime
         OR buffJob.endDateTime GE iopStartDateTime)
        AND ROWID(buffJob) NE ipRowID:
    ASSIGN
      lvStartDate = buffJob.endDate
      lvStartTime = buffJob.endTime
      lvStartDateTime = buffJob.endDateTime.
    RUN newEnd (ipTimeSpan,lvStartDate,lvStartTime,
                OUTPUT lvEndDate,OUTPUT lvEndTime).
    RUN downtimeSpan (ipResource,ipTimeSpan,lvStartDate,lvStartTime,
                      OUTPUT lvEndDate,OUTPUT lvEndTime,OUTPUT lvDowntimeSpan).
    lvEndDateTime = numericDateTime(lvEndDate,lvEndTime).
    IF checkJobConflict(ipResource,lvStartDateTime,lvEndDateTime,ipRowID) THEN
    NEXT.
    ASSIGN
      iopStartDate = lvStartDate
      iopStartTime = lvStartTime
      iopStartDateTime = lvStartDateTime
      iopEndDate = lvEndDate
      iopEndTime = lvEndTime
      iopEndDateTime = lvEndDateTime.
    RETURN.
  END. /* if checkjobconflict */
