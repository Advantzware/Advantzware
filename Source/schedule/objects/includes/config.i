/* config.i - used in configGetPut.i */

PROCEDURE {1}{6}:
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  
  {2} {3} VALUE(SEARCH('{&data}/' + ID + '/config.dat')) NO-ECHO.
  {4} {5}.
  {4} {{&includes}/config{6}.i}
  {4} colorPriority.
  DO i = 1 TO EXTENT(jobBGColor):
    {4} jobLabel[i] jobBGColor[i] customLabel[i] customValue[i] customBGColor[i]
        jobFGColor[i] customFGColor[i].
    &IF "{1}" EQ "Get" &THEN
    IF jobBGColor[i] GE 16 AND jobBGColor[i] LE 29 THEN
    jobBGColor[i] = jobBGColor[i] + 14.
    IF customBGColor[i] GE 16 AND customBGColor[i] LE 29 THEN
    customBGColor[i] = customBGColor[i] + 14.
    IF jobFGColor[i] GE 16 AND jobFGColor[i] LE 29 THEN
    jobFGColor[i] = jobFGColor[i] + 14.
    IF customFGColor[i] GE 16 AND customFGColor[i] LE 29 THEN
    customFGColor[i] = customFGColor[i] + 14.
    &ENDIF
  END.
  {2} CLOSE.
  
  &IF "{1}" EQ "Get" &THEN
  IF downtimeConflictBGColor GE 16 AND downtimeConflictBGColor LE 29 THEN
  downtimeConflictBGColor = downtimeConflictBGColor + 14.
  IF downtimeConflictFGColor GE 16 AND downtimeConflictFGColor LE 29 THEN
  downtimeConflictFGColor = downtimeConflictFGColor + 14.
  IF flashLightColor GE 16 AND flashLightColor LE 29 THEN
  flashLightColor = flashLightColor + 14.
  IF gridBGColor GE 16 AND gridBGColor LE 29 THEN
  gridBGColor = gridBGColor + 14.
  IF gridLineColor GE 16 AND gridLineColor LE 29 THEN
  gridLineColor = gridLineColor + 14.
  IF jobConflictBGColor GE 16 AND jobConflictBGColor LE 29 THEN
  jobConflictBGColor = jobConflictBGColor + 14.
  IF jobConflictFGColor GE 16 AND jobConflictFGColor LE 29 THEN
  jobConflictFGColor = jobConflictFGColor + 14.
  IF lightBulbColor GE 16 AND lightBulbColor LE 29 THEN
  lightBulbColor = lightBulbColor + 14.
  IF resourceBGColor GE 16 AND resourceBGColor LE 29 THEN
  resourceBGColor = resourceBGColor + 14.
  IF timeLineColor GE 16 AND timeLineColor LE 29 THEN
  timeLineColor = timeLineColor + 14.
  &ENDIF

END PROCEDURE.
