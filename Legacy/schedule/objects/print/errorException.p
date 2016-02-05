/* errorException.p */

&SCOPED-DEFINE printProgramName errorException
&SCOPED-DEFINE printProgramTitle Error Exception

{schedule/scopDir.i}
{{&print}/includes/printDefs.i}

DEFINE VARIABLE dispStart AS CHARACTER NO-UNDO LABEL 'Time' FORMAT 'x(10)'.
DEFINE VARIABLE dispEnd AS CHARACTER NO-UNDO LABEL 'Time' FORMAT 'x(10)'.
DEFINE VARIABLE sortSeq AS INTEGER NO-UNDO.
DEFINE VARIABLE jobError AS LOGICAL NO-UNDO.
DEFINE VARIABLE fixError AS LOGICAL NO-UNDO.
DEFINE VARIABLE fixed AS LOGICAL NO-UNDO.

FUNCTION numericDateTime RETURNS DECIMAL (ipDate AS DATE,ipTime AS INTEGER):
  {{&includes}/numericDateTime.i}
END FUNCTION.

IF ipBoard EQ '{&Board}' THEN
MESSAGE 'Auto Fix Errors?' VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    TITLE 'Error Exception Question' UPDATE fixError.

{{&print}/includes/outputTo.i}
IF ipExcel THEN
EXPORT DELIMITER ',' 'Job' 'Resource' 'ResSeq' 'Start Date' 'Start Time' 'End Date' 'End Time' 'Locked' ''.
DO WHILE TRUE:
  fixed = NO.
  FOR EACH ttblJob NO-LOCK
      BREAK BY ttblJob.job BY ttblJob.resourceSequence BY ttblJob.jobSequence:
    IF FIRST-OF(ttblJob.job) THEN
    ASSIGN
      sortSeq = 0
      jobError = NO.
    IF ttblJob.sortSequence LT sortSeq AND NOT jobError THEN
    DO:
      RUN jobError (ttblJob.job).
      ASSIGN
        jobError = YES
        fixed = YES.
    END. /* if sortsequence */
    sortSeq = ttblJob.sortSequence.
  END. /* each ttblJob */
  IF fixError AND fixed THEN
  RUN setJobSequence.
  ELSE LEAVE.
END. /* do while */
{{&print}/includes/outputClose.i}

PROCEDURE getConfiguration:
END PROCEDURE.

PROCEDURE jobError:
  DEFINE INPUT PARAMETER ipJob AS CHARACTER NO-UNDO.

  DEFINE VARIABLE priorSpan AS INTEGER NO-UNDO.
  DEFINE VARIABLE priorDate AS DATE NO-UNDO.
  DEFINE VARIABLE priorTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE newDate AS DATE NO-UNDO.
  DEFINE VARIABLE newTime AS INTEGER NO-UNDO.

  FOR EACH buffJob NO-LOCK WHERE buffJob.job EQ ipJob
      BREAK BY buffJob.job BY buffJob.resourceSequence BY buffJob.jobSequence
      WITH STREAM-IO WIDTH 132:
    IF FIRST-OF(buffJob.job) THEN
    DO:
      IF NOT ipExcel THEN
      DISPLAY buffJob.job.
      sortSeq = 0.
    END. /* if first-of */
    IF ipExcel THEN
    EXPORT DELIMITER ','
      buffJob.job
      buffJob.resource
      buffJob.resourceSequence
      buffJob.startDate
      STRING(buffJob.startTime,'HH:MM:SSam')
      buffJob.endDate
      STRING(buffJob.endTime,'HH:MM:SSam')
      buffJob.jobLocked
      'Error' WHEN buffJob.sortSequence LT sortSeq.
    ELSE
    DISPLAY
      buffJob.resource
      buffJob.resourceSequence LABEL 'Seq'
      buffJob.startDate LABEL 'Start'
      dispStart
      STRING(buffJob.startTime,'HH:MM:SSam') @ dispStart
      buffJob.endDate LABEL 'End'
      dispEnd
      STRING(buffJob.endTime,'HH:MM:SSam') @ dispEnd
      buffJob.jobLocked
      'Error' WHEN buffJob.sortSequence LT sortSeq.
    IF fixError AND buffJob.sortSequence LT sortSeq THEN
    DO:
      RUN newEnd (priorSpan,priorDate,priorTime,OUTPUT newDate,OUTPUT newTime).
      IF ipExcel THEN
      EXPORT DELIMITER ',' '' '' ''
        priorDate
        STRING(priorTime,'HH:MM:SSam')
        newDate
        STRING(newTime,'HH:MM:SSam')
        '' 'Fix'.
      ELSE
      DO:
        DOWN 1.
        DISPLAY
          priorDate @ buffJob.startDate
          STRING(priorTime,'HH:MM:SSam') @ dispStart
          newDate @ buffJob.endDate
          STRING(newTime,'HH:MM:SSam') @ dispEnd
          'Fix'.
      END. /* else */
      ASSIGN
        buffJob.startDate = priorDate
        buffJob.startTime = priorTime
        buffJob.endDate = newDate
        buffJob.endTime = newTime.
      buffJob.startDateTime = numericDateTime(buffJob.startDate,buffJob.startTime).
      buffJob.endDateTime = numericDateTime(buffJob.endDate,buffJob.endTime).
    END. /* if fixerror */
    ASSIGN
      priorSpan = buffJob.timeSpan
      priorDate = buffJob.endDate
      priorTime = buffJob.endTime
      sortSeq = buffJob.sortSequence.
    IF LAST-OF(buffJob.job) AND NOT ipExcel THEN
    DOWN 1.
  END. /* each buffJob */
END PROCEDURE.

PROCEDURE newEnd:
  {{&includes}/{&Board}/newEnd.i}
END PROCEDURE.

PROCEDURE setJobSequence:
  {{&includes}/{&Board}/setJobSequence.i}
END PROCEDURE.
