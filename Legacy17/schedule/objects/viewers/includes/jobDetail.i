/* jobDetail.i */

IF AVAILABLE {1} THEN
ASSIGN
  resource = {1}.resource
  jobSequence = {1}.jobSequence
  job = {1}.job
  resourceSequence = {1}.resourceSequence
  startDate = {1}.startDate
  startTime = STRING({1}.startTime,'HH:MM:SS am')
  endDate = {1}.endDate
  endTime = STRING({1}.endTime,'HH:MM:SS am')
  origStartDate = {1}.origStartDate
  origStartTime = STRING({1}.origStartTime,'HH:MM:SS am')
  origEndDate = {1}.origEndDate
  origEndTime = STRING({1}.origEndTime,'HH:MM:SS am')
  dueDate = {1}.dueDate
  dueTime = STRING({1}.dueTime,'HH:MM:SS am')
  timeSpan = STRING(ROUND({1}.timeSpan / 3600,2))
  downtimeSpan = STRING(ROUND({1}.downtimeSpan / 3600,2))
  lockImageName = '{&images}/' + TRIM(STRING({1}.jobLocked,'/un')) + 'locked.gif'
  noteRowID = TO-ROWID(ENTRY(2,{1}.rowIDs))
  noteImage:HIDDEN = NOT CAN-FIND(FIRST jobNotes WHERE jobNotes.jobRowID EQ noteRowID
                                                   AND jobNotes.jobStatus EQ NO
                                                   AND jobNotes.deleteNote EQ NO).
