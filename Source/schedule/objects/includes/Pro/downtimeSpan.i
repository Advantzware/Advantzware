/* downtimeSpan.i - used in board.w & resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:     calculate new ending date & time and downtime span value
  Parameters:  fixed job time span, new start date & time,
               output new end date & time, downtime span
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipTimeSpan AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER newStartDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER newStartTime AS INTEGER NO-UNDO.
  
  DEFINE OUTPUT PARAMETER newEndDate AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER newEndTime AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opDowntimeSpan AS INTEGER NO-UNDO.

  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.

  RUN newEnd (ipTimeSpan,newStartDate,newStartTime,
              OUTPUT newEndDate,OUTPUT newEndTime).
  ASSIGN
    lvStartDateTime = numericDateTime(newStartDate,newStartTime)
    lvEndDateTime = numericDateTime(newEndDate,newEndTime).
  FOR EACH boardDowntime NO-LOCK
      WHERE boardDowntime.resource EQ ipResource
        AND boardDowntime.startDateTime GE lvStartDateTime:
    IF boardDowntime.startDateTime GE lvEndDateTime THEN
    RETURN.
    opDowntimeSpan = opDowntimeSpan + boardDowntime.downtimeSpan.
    RUN newEnd (ipTimeSpan + opDowntimeSpan,newStartDate,newStartTime,
                OUTPUT newEndDate,OUTPUT newEndTime).
    lvEndDateTime = numericDateTime(newEndDate,newEndTime).
  END.
