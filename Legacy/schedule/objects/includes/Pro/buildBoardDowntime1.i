/* buildBoardDowntime1.i - used in procedure buildBoardDowntime in buildProc.i */

IF NOT CAN-FIND(boardDowntime
       WHERE boardDowntime.resource EQ ttblResource.resource
         AND boardDowntime.startDateTime EQ numericDateTime({&startDate},{&startTime})
         AND boardDowntime.endDateTime EQ numericDateTime({&endDate},{&endTime})) AND
   {&startTime} NE {&endTime} THEN
DO:
  CREATE boardDowntime.
  ASSIGN
    downtimeFound = YES
    boardDowntime.downtimeSpan = timeSpan({&startDate},{&startTime},{&endDate},{&endTime})
    boardDowntime.resource = ttblResource.resource
    boardDowntime.startDate = {&startDate}
    boardDowntime.startTime = {&startTime}
    boardDowntime.endDate = {&endDate}
    boardDowntime.endTime = {&endTime}.
  boardDowntime.startDateTime = numericDateTime({&startDate},{&startTime}).
  boardDowntime.endDateTime = numericDateTime({&endDate},{&endTime}).
END.
ELSE
IF {&startTime} EQ {&endTime} THEN
downtimeFound = YES.
