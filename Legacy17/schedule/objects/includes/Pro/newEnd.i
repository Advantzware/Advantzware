/* newEnd.i - used in procedure newEnd in pro/boardProc.i and detailResource.w */

/*------------------------------------------------------------------------------
  Purpose:     calculate new ending date & time
  Parameters:  inputs timespan, start date & time, output new end date & time
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipTimeSpan AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER newStartDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER newStartTime AS INTEGER NO-UNDO.
  
  DEFINE OUTPUT PARAMETER newEndDate AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER newEndTime AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE days AS INTEGER NO-UNDO.
  
  ASSIGN
    newEndTime = newStartTime + ipTimeSpan
    days = TRUNCATE(newEndTime / 86400,0)
    newEndDate = newStartDate + days
    newEndTime = newEndTime - days * 86400.
