/* buildBoardDowntime.i */

/*------------------------------------------------------------------------------
  Purpose:     build downtime widget rectangles
  Parameters:  <none>
  Notes:       3 scenarios: resource based on day of week, or date, or calendar
               wide and day of week
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dateIdx AS DATE NO-UNDO.
  DEFINE VARIABLE downtimeFound AS LOGICAL NO-UNDO.
  DEFINE VARIABLE firstDate AS DATE NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE lastDate AS DATE NO-UNDO.

  &SCOPED-DEFINE ttblFile ttblDowntime
  &SCOPED-DEFINE startDate ttblDowntime.startDate
  &SCOPED-DEFINE startTime ttblDowntime.startTime
  &SCOPED-DEFINE endDate ttblDowntime.startDate
  &SCOPED-DEFINE endTime ttblDowntime.endTime
  
  RUN msgFrame ('Building Board Downtime').
  FIND FIRST ttblJob NO-LOCK USE-INDEX startDateIdx NO-ERROR.
  firstDate = IF AVAILABLE ttblJob THEN ttblJob.startDate ELSE TODAY - 1.
  FIND LAST ttblJob NO-LOCK USE-INDEX endDateIdx NO-ERROR.
  IF AVAILABLE ttblJob THEN
  lastDate = (IF ttblJob.endDate LT TODAY THEN TODAY
              ELSE ttblJob.endDate) + endDateBuffer.
  ELSE lastDate = TODAY + endDateBuffer.
  EMPTY TEMP-TABLE boardDowntime.
  FOR EACH ttblResource NO-LOCK:
    /* resource based and date specific */
    FOR EACH ttblDowntime NO-LOCK
        WHERE ttblDowntime.resource EQ ttblResource.resource
          AND ttblDowntime.startDate GE firstDate
          AND ttblDowntime.startDate LE lastDate:
      {{&includes}/Pro/buildBoardDowntime1.i}
    END. /* each ttblDowntime */
    
    IF CAN-FIND(FIRST ttblDowntime WHERE ttblDowntime.dayID NE 0) THEN
    DO dateIdx = firstDate TO lastDate:
      downtimeFound = NO.
      /* calendar wide, not resource based, date specific */
      FOR EACH ttblDowntime NO-LOCK
          WHERE ttblDowntime.resource EQ '<Calendar>'
            AND ttblDowntime.startDate EQ dateIdx:
        IF CAN-FIND(FIRST buffDowntime
           WHERE buffDowntime.resource EQ ttblResource.resource
             AND buffDowntime.startDate EQ dateIdx) THEN NEXT.
        {{&includes}/Pro/buildBoardDowntime1.i}.
      END. /* each ttblDowntime */
      /* resource based and day of week, not date specific */
      IF NOT downtimeFound THEN
      FOR EACH ttblDowntime NO-LOCK
          WHERE ttblDowntime.resource EQ ttblResource.resource
            AND ttblDowntime.startDate EQ ?
            AND ttblDowntime.dayID EQ WEEKDAY(dateIdx) + 7:
        IF CAN-FIND(FIRST buffDowntime
           WHERE buffDowntime.resource EQ ttblResource.resource
             AND buffDowntime.startDate EQ dateIdx) THEN NEXT.
        &SCOPED-DEFINE startDate dateIdx
        &SCOPED-DEFINE endDate dateIdx
        {{&includes}/Pro/buildBoardDowntime1.i}.
      END. /* each ttblDowntime */
      /* calendar wide, not resource based, based on day of week */
      IF NOT downtimeFound THEN
      FOR EACH ttblDowntime NO-LOCK
          WHERE ttblDowntime.resource EQ '<Calendar>'
            AND ttblDowntime.startDate EQ ?
            AND ttblDowntime.dayID EQ WEEKDAY(dateIdx):
        IF CAN-FIND(FIRST buffDowntime
           WHERE buffDowntime.resource EQ ttblResource.resource
             AND buffDowntime.startDate EQ dateIdx) THEN NEXT.
        {{&includes}/Pro/buildBoardDowntime1.i}.
      END. /* each ttblDowntime */
    END. /* do dateIdx */
  END. /* each ttblresource */
