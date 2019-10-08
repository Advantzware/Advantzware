/* btnStatus.i - used in btnStatus-{} triggers in completeJob.w & completePending.w */

IF ipBoard NE '{&Board}' THEN
RETURN NO-APPLY.
ASSIGN
  status-{1} = NOT status-{1}
  status-{1}:SCREEN-VALUE = STRING(status-{1})
  {&jobTable}.jobStatus[{1}] = status-{1}.
RUN setCompletedStatus.
RUN setStatusValues.
RUN setWholeJob.
