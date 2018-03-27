/* newStart.i - used in procedure newStart in pro/boardProc.i,
                detailResource.w & pending.w */

/*------------------------------------------------------------------------------
  Purpose:     calculate new starting date & time
  Parameters:  inputs timespan and end date & time, output new start date & time
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipTimeSpan AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER newEndDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER newEndTime AS INTEGER NO-UNDO.
  
  DEFINE OUTPUT PARAMETER newStartDate AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER newStartTime AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE days AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  
  IF ipTimeSpan GT newEndTime THEN
  ASSIGN
    i = ipTimeSpan - newEndTime
    days = TRUNCATE(i / 86400,0)
    newStartTime = 86400 - (i - days * 86400)
    newStartDate = newEndDate - days - (IF i / 86400 GT 0 THEN 1 ELSE 0).
  ELSE
  ASSIGN
    newStartTime = newEndTime - ipTimeSpan
    newStartDate = newEndDate.
