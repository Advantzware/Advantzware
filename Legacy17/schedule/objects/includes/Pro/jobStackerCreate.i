/* jobStackerCreate.i - used in procedure jobStackerCreate in boardProc.i & resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:     create temp-table stack record used to auto schedule jobs
  Parameters:  lots of stuff
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipSortOrder AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobSequence AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipJob AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipDowntimeSpan AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipStartDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipStartTime AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipEndDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipEndTime AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipLagTime AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobChanged AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.

  CREATE jobStacker.
  ASSIGN
    jobStacker.startDateTime = numericDateTime(ipstartDate,ipstartTime)
    jobStacker.endDateTime = numericDateTime(ipEndDate,ipEndTime)
    jobStacker.sortOrder = ipSortOrder
    jobStacker.resource = ipResource
    jobStacker.jobSequence = ipJobSequence
    jobStacker.job = ipJob
    jobStacker.jobChanged = ipJobChanged
    jobStacker.downtimeSpan = ipDowntimeSpan
    jobStacker.startDate = ipStartDate
    jobStacker.startTime = ipStartTime
    jobStacker.endDate = ipEndDate
    jobStacker.endTime = ipEndTime
    jobStacker.lagTime = ipLagTime
    jobStacker.ttblRowID = ipRowID.
