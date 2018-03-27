/* rowDisplay.i - used in trigger rowdisplay in jobBrowse.w & resourceDetail.w */

  ASSIGN
    bgColorJob = ?
    fgColorJob = ?.
  &IF '{&useTtbl}' EQ 'ttblJob' &THEN
  ASSIGN
    bgColorJob = {&useTtbl}.jobBGColor
    fgColorJob = {&useTtbl}.jobFGColor.
  &ELSEIF '{&colorJobTable}' EQ 'pendingJob' &THEN
  ASSIGN
    bgColorJob = jobBGColor()
    fgColorJob = jobFGColor().
  &ENDIF
  DO i = 1 TO EXTENT(cellColumn):
    IF NOT VALID-HANDLE(cellColumn[i]) THEN LEAVE.
    IF i GE 2 AND i LE 3 THEN
    ASSIGN
      cellColumn[i]:BGCOLOR = bgColorJob
      cellColumn[i]:FGCOLOR = fgColorJob.
    FIND browseColumn NO-LOCK WHERE browseColumn.colOrder EQ i NO-ERROR.
    IF AVAILABLE browseColumn AND (browseColumn.colName EQ 'calcTimeField' OR
                                   browseColumn.colName EQ 'jobCompleted' OR
                                   browseColumn.colName BEGINS 'udf' OR
                                   browseColumn.colName BEGINS 'user') THEN
    CASE browseColumn.colLabel:
      WHEN 'C' THEN
      cellColumn[i]:SCREEN-VALUE = IF {&useTtbl}.jobCompleted THEN 'C' ELSE ''.
      WHEN 'Start Time' THEN
      cellColumn[i]:SCREEN-VALUE = STRING({&useTtbl}.startTime,'HH:MM:SS am').
      WHEN 'End Time' THEN
      cellColumn[i]:SCREEN-VALUE = STRING({&useTtbl}.endTime,'HH:MM:SS am').
      WHEN 'Due Time' THEN
      cellColumn[i]:SCREEN-VALUE = STRING({&useTtbl}.dueTime,'HH:MM:SS am').
      WHEN 'Orig Start' THEN
      cellColumn[i]:SCREEN-VALUE = STRING({&useTtbl}.origStartTime,'HH:MM:SS am').
      WHEN 'Orig End' THEN
      cellColumn[i]:SCREEN-VALUE = STRING({&useTtbl}.origEndTime,'HH:MM:SS am').
      WHEN 'Job Time' THEN
      cellColumn[i]:SCREEN-VALUE = specialTime({&useTtbl}.timeSpan).
      WHEN 'Downtime' THEN
      cellColumn[i]:SCREEN-VALUE = specialTime({&useTtbl}.downtimeSpan).
      WHEN 'Total Time' THEN
      cellColumn[i]:SCREEN-VALUE = specialTime({&useTtbl}.timeSpan + {&useTtbl}.downtimeSpan).
      OTHERWISE
      cellColumn[i]:FORMAT = 'X(256)'.
    END CASE.
  END.
