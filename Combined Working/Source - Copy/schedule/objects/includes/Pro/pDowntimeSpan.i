/* pDowntimeSpan.i - used in board.w & resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:     calculate new starting date & time and downtime span value
  Parameters:  fixed job time span, new end date & time,
               output new start date & time, downtime span
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipTimeSpan AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER newEndDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER newEndTime AS INTEGER NO-UNDO.
  
  DEFINE OUTPUT PARAMETER newStartDate AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER newStartTime AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opDowntimeSpan AS INTEGER NO-UNDO.

  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.

  RUN newStart (ipTimeSpan,newEndDate,newEndTime,
                OUTPUT newStartDate,OUTPUT newStartTime).
  ASSIGN
    lvStartDateTime = numericDateTime(newStartDate,newStartTime)
    lvEndDateTime = numericDateTime(newEndDate,newEndTime).
  FOR EACH boardDowntime NO-LOCK
      WHERE boardDowntime.resource EQ ipResource
        AND boardDowntime.endDateTime LE lvEndDateTime
         BY boardDowntime.endDateTime DESCENDING:
    IF boardDowntime.startDateTime GE lvStartDateTime THEN
    RETURN.
    opDowntimeSpan = opDowntimeSpan + boardDowntime.downtimeSpan.
    RUN newStart (ipTimeSpan + opDowntimeSpan,newEndDate,newEndTime,
                  OUTPUT newStartDate,OUTPUT newStartTime).
    lvStartDateTime = numericDateTime(newStartDate,newStartTime).
  END.
