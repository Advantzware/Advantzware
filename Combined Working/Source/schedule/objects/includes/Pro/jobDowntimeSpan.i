/* jobDowntimeSpan.i */

/*------------------------------------------------------------------------------
  Purpose:     set job's downtime span and reset ending date & time accordingly
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN msgFrame ('Set Job Downtime Span').
  FOR EACH ttblJob EXCLUSIVE-LOCK:
    RUN downtimeSpan (ttblJob.resource,ttblJob.timeSpan,
                      ttblJob.startDate,ttblJob.startTime,
                      OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime,
                      OUTPUT ttblJob.downtimeSpan).
    ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
    ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
  END.
