/* beforeDowntime.i - used in procedure beforeDowntime in boardProc.i &
                      resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:     find prior available time slot for job being moved
  Parameters:  resource, row id, timespan, iop startDateTime & endDateTime,
               op startDate, startTime, endDate and endTime
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipTimeSpan AS INTEGER NO-UNDO.
  
  DEFINE INPUT-OUTPUT PARAMETER iopStartDateTime AS DECIMAL NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopEndDateTime AS DECIMAL NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER opStartDate AS DATE NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER opStartTime AS INTEGER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER opEndDate AS DATE NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER opEndTime AS INTEGER NO-UNDO.

  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.

  FOR EACH boardDowntime NO-LOCK USE-INDEX descendDowntime
      WHERE boardDowntime.resource EQ ipResource
        AND (boardDowntime.startDatetime LE iopEndDateTime
         OR boardDowntime.endDateTime LE iopEndDateTime):
    ASSIGN
      lvEndDate = boardDowntime.startDate
      lvEndTime = boardDowntime.startTime
      lvEndDateTime = boardDowntime.startDateTime.
    RUN newStart (ipTimeSpan,lvEndDate,lvEndTime,
                  OUTPUT lvStartDate,OUTPUT lvStartTime).
    lvStartDateTime = numericDateTime(lvStartDate,lvStartTime).
    IF checkDowntimeConflict(ipResource,lvStartDateTime,lvEndDateTime) THEN
    NEXT.
    ASSIGN
      opStartDate = lvStartDate
      opStartTime = lvStartTime
      iopStartDateTime = lvStartDateTime
      opEndDate = lvEndDate
      opEndTime = lvEndTime
      iopEndDateTime = lvEndDateTime.
    RETURN.
  END.
