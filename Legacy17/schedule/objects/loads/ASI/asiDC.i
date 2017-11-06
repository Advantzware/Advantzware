/* asiDC.i */

FOR EACH {1} EXCLUSIVE-LOCK:
  IF NOT {1}.liveUpdate THEN NEXT.
  IF NOT {1}.jobLocked THEN {1}.startDateTime = ?.
  RUN {&loads}/ASI/asiDC.p ({1}.rowIDs,OUTPUT lvStartDate,OUTPUT lvStartTime,
                            OUTPUT lvEndDate,OUTPUT lvEndTime,
                            OUTPUT lvJobLocked,OUTPUT lvJobCompleted,
                            OUTPUT lvMRCompleted,OUTPUT lvRunCompleted).
  IF NOT lvJobLocked THEN NEXT.
  lvTimeSpan = IF '{1}' EQ 'pendingJob' THEN {1}.origStartTime ELSE {1}.timeSpan.
  IF lvEndDate EQ ? AND lvEndTime EQ ? THEN
  DO:
    RUN newEnd (lvTimeSpan,lvStartDate,lvStartTime,OUTPUT lvEndDate,OUTPUT lvEndTime).
    RUN downtimeSpan ({1}.resource,lvTimeSpan,lvStartDate,lvStartTime,
                      OUTPUT lvEndDate,OUTPUT lvEndTime,OUTPUT lvDowntimeSpan).
  END. /* if lvenddate */
  ASSIGN
    {1}.startDate = lvStartDate
    {1}.startTime = lvStartTime
    {1}.endDate = lvEndDate
    {1}.endTime = lvEndTime
    {1}.downtimeSpan = lvDowntimeSpan
    {1}.jobLocked = lvJobLocked
    {1}.jobCompleted = lvJobCompleted
    .
  {1}.startDateTime = numericDateTime({1}.startDate,{1}.startTime).
  {1}.endDateTime = numericDateTime({1}.endDate,{1}.endTime).
  &IF '{1}' EQ 'pendingJob' &THEN
  CREATE ttblJob.
  BUFFER-COPY {1} TO ttblJob.
  ASSIGN
    ttblJob.timeSpan = lvTimeSpan
    ttblJob.origStartDate = {1}.startDate
    ttblJob.origStartTime = {1}.startTime
    ttblJob.origEndDate = {1}.endDate
    ttblJob.origEndTime = {1}.endTime
    .
  DELETE {1}.
  &ENDIF
  ASSIGN
    ttblJob.jobBGColor = jobBGColor()
    ttblJob.jobFGColor = jobFGColor()
    ttblJob.statusLabel = jobStatus()
    .
END. /* each {1} */
