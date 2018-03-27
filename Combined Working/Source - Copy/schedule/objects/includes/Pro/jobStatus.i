/* jobStatus.i  - used in procedure getConfiguration in completeJob.w */

status-{1} = IF btnStatus-{1}:LABEL NE '' THEN {&jobTable}.jobStatus[{1}] ELSE YES
status-{1}:SCREEN-VALUE = STRING(status-{1})
status-{1}:BGCOLOR = IF status-{1} THEN 10 ELSE 12
statusButton[{1}] = btnStatus-{1}:HANDLE
statusValue[{1}] = status-{1}:HANDLE
