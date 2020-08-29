/* htmlDefs.i - rstark - 8.13.2020     */
/* used in capacityPage.w and sbHTML.p */

/*&Scoped-define DebugLog*/

DEFINE VARIABLE iSortOrder AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttTime NO-UNDO
    FIELD timeKey   AS CHARACTER
    FIELD timeDate  AS DATE
    FIELD timeSlice AS INTEGER 
    FIELD timeType1 AS CHARACTER 
    FIELD timeType2 AS CHARACTER 
    FIELD newJob    AS LOGICAL
    FIELD sortOrder AS INTEGER
    FIELD jobCount  AS INTEGER 
    FIELD spanText  AS CHARACTER
      INDEX ttTime IS PRIMARY
          timeKey
          timeDate  DESCENDING
          timeSlice DESCENDING
          timeType2 DESCENDING
          .

FUNCTION checkDowntimeConflict RETURNS LOGICAL 
  (ipcResource AS CHARACTER,ipdtDate AS DATE,ipiTime AS INTEGER) :

    RETURN CAN-FIND(FIRST ttblDowntime
                    WHERE ttblDowntime.dayID      EQ WEEKDAY(ipdtDate)
                      AND ttblDowntime.resource   EQ ipcResource
                      AND (ttblDowntime.startDate EQ ipdtDate
                       OR ttblDowntime.startDate  EQ ?)
                      AND ttblDowntime.startTime  LE ipiTime
                      AND ttblDowntime.endTime    GE ipiTime
                   ).

END FUNCTION.

FUNCTION checkJobConflict RETURNS LOGICAL
  (ipcResource AS CHARACTER,ipStartDateTime AS DECIMAL,ipEndDateTime AS DECIMAL) :

    RETURN CAN-FIND(FIRST ttblJob
                    WHERE ttblJob.{1}            EQ ipcResource
                      AND ((ttblJob.startDateTime GE ipStartDateTime
                      AND ttblJob.startDateTime  LT ipEndDateTime)
                       OR (ttblJob.endDateTime   LE ipEndDateTime
                      AND ttblJob.endDateTime    GT ipStartDateTime)
                       OR (ttblJob.startDateTime LE ipStartDateTime
                      AND ttblJob.endDateTime    GE ipEndDateTime))
                   ).

END FUNCTION.

FUNCTION fixTime RETURNS INTEGER
  (ipTime AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  correct invalid time value
    Notes:  
------------------------------------------------------------------------------*/
    IF ipTime EQ ? THEN ipTime = 0.
    RETURN INTEGER(ipTime - TRUNCATE(ipTime / 86400,0) * 86400).

END FUNCTION.

FUNCTION fTimeSlice RETURNS LOGICAL
  (ipcKey       AS CHARACTER,
   ipdtDate     AS DATE,
   ipiTimeSlice AS INTEGER,
   ipcTimeType1 AS CHARACTER,
   ipcTimeType2 AS CHARACTER,
   iplNewJob    AS LOGICAL,
   ipcSpanText  AS CHARACTER):

    IF ipcTimeType1 EQ "Avail" AND
       CAN-FIND(FIRST ttTime
                WHERE ttTime.timeKey   EQ ipcKey
                  AND ttTime.timeDate  EQ ipdtDate
                  AND ttTime.timeSlice EQ ipiTimeSlice
                  AND ttTime.timeType2 EQ ipcTimeType2) THEN
    RETURN TRUE.

    IF CAN-FIND(FIRST ttTime
                WHERE ttTime.timeKey   EQ ipcKey
                  AND ttTime.timeDate  EQ ipdtDate
                  AND ttTime.timeSlice EQ ipiTimeSlice
                  AND ttTime.timeType1 EQ ipcTimeType1
                  AND ttTime.timeType2 EQ ipcTimeType2) THEN
    RETURN TRUE.

    IF ipcTimeType1 NE "DT" THEN DO:
        FIND FIRST ttblDowntime
             WHERE ttblDowntime.dayID      EQ WEEKDAY(ipdtDate)
               AND ttblDowntime.resource   EQ ENTRY(1,ipcKey)
               AND (ttblDowntime.startDate EQ ipdtDate
                OR ttblDowntime.startDate  EQ ?)
               AND ttblDowntime.startTime  LE ipiTimeSlice
               AND ttblDowntime.endTime    GE ipiTimeSlice
             NO-ERROR.
        IF AVAILABLE ttblDowntime THEN
        CASE ipcTimeType2:
            WHEN "Start" THEN
            ipiTimeSlice = ttblDowntime.endTime.
            WHEN "End" THEN
            ipiTimeSlice = ttblDowntime.startTime.
        END CASE.
    END. /* if ne dt */

    IF CAN-FIND(FIRST ttTime
                WHERE ttTime.timeKey   EQ ipcKey
                  AND ttTime.timeDate  EQ ipdtDate
                  AND ttTime.timeSlice EQ ipiTimeSlice
                  AND ttTime.timeType1 EQ ipcTimeType1
                  AND ttTime.timeType2 EQ ipcTimeType2) THEN
    RETURN TRUE.

    CREATE ttTime.
    ASSIGN
        ttTime.timeKey   = ipcKey
        ttTime.timeDate  = ipdtDate
        ttTime.timeSlice = ipiTimeSlice
        ttTime.timeType1 = ipcTimeType1
        ttTime.timeType2 = ipcTimeType2
        ttTime.newJob    = iplNewJob
        iSortOrder       = iSortOrder + 1
        ttTime.sortOrder = iSortOrder
        ttTime.spanText  = ipcSpanText
        .
    RETURN TRUE.

END FUNCTION.

FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  put the date and time in decimal format YYYYMMDD.TTTTT
    Notes:  
------------------------------------------------------------------------------*/
      IF ipTime LT 0 OR ipTime EQ ? THEN ipTime = 0.
      IF ipTime GT 86400 THEN
      ipTime = ipTime - TRUNCATE(ipTime / 86400,0) * 86400.
      IF ipDate EQ ? AND ipTime EQ ? THEN
      RETURN 0.99999.
      ELSE
      IF ipDate EQ ? THEN
      RETURN DECIMAL('0.' + STRING(ipTime,'99999')).
      ELSE
      RETURN DECIMAL(STRING(YEAR(ipDate),'9999') +
                     STRING(MONTH(ipDate),'99') +
                     STRING(DAY(ipDate),'99') + '.' +
                     STRING(ipTime,'99999')).

END FUNCTION.

FUNCTION timeSpan RETURNS INTEGER
  (ipStartDate AS DATE,ipStartTime AS INTEGER,ipEndDate AS DATE,ipEndTime AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  calculate time span between 2 dates & times in seconds
    Notes:  
------------------------------------------------------------------------------*/
    RETURN IF ipStartDate EQ ipEndDate THEN ipEndTime - ipStartTime
           ELSE (86400 - ipStartTime) + (ipEndDate - ipStartDate - 1) * 86400 + ipEndTime.

END FUNCTION.

/* **********************  Internal Procedures  *********************** */

PROCEDURE downtimeLoop:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMachine    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiTimeSpan   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipdtStartDate AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipiStartTime  AS INTEGER   NO-UNDO.
    
    DEFINE INPUT-OUTPUT PARAMETER iopdtEndDate AS DATE    NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiEndTime  AS INTEGER NO-UNDO.

    DEFINE VARIABLE idx   AS INTEGER NO-UNDO.
    DEFINE VARIABLE lLoop AS LOGICAL NO-UNDO INITIAL TRUE.

    &IF DEFINED(DebugLog) NE 0 &THEN
    EXPORT "start loop" ipcMachine ipdtStartDate STRING(ipiStartTime,"hh:mm:ss am")
        iopdtEndDate STRING(iopiEndTime,"hh:mm:ss am").
    &ENDIF
    DO WHILE lLoop:
        lLoop = FALSE.
        IF checkDowntimeConflict(ipcMachine,ipdtStartDate,ipiStartTime) THEN DO:
            lLoop = TRUE.
            RUN downtimeSpan (ipcMachine,ipiTimeSpan,ipdtStartDate,ipiStartTime,
                              OUTPUT iopdtEndDate,OUTPUT iopiEndTime).
        END. /* if */
        IF checkDowntimeConflict(ipcMachine,iopdtEndDate,iopiEndTime) THEN DO:
            lLoop = TRUE.
            RUN downtimeSpan (ipcMachine,ipiTimeSpan,iopdtEndDate,iopiEndTime,
                              OUTPUT iopdtEndDate,OUTPUT iopiEndTime).
        END. /* if */
        idx = idx + 1.
        IF idx GE 10 THEN LEAVE.
    END. /* do while */
    &IF DEFINED(DebugLog) NE 0 &THEN
    EXPORT "  end loop" ipcMachine ipdtStartDate STRING(ipiStartTime,"hh:mm:ss am")
        iopdtEndDate STRING(iopiEndTime,"hh:mm:ss am").
    &ENDIF

END PROCEDURE.

PROCEDURE downtimeSpan :
/*------------------------------------------------------------------------------
  Purpose:     calculate new ending date & time and downtime span value
  Parameters:  fixed job time span, new start date & time,
               output new end date & time, downtime span
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipResource   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipTimeSpan   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER newStartDate AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER newStartTime AS INTEGER   NO-UNDO.
  
    DEFINE OUTPUT PARAMETER newEndDate AS DATE    NO-UNDO.
    DEFINE OUTPUT PARAMETER newEndTime AS INTEGER NO-UNDO.

    DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lvEndDateTime   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lvDowntimeSpan  AS INTEGER NO-UNDO.

    RUN newEnd (ipTimeSpan,newStartDate,newStartTime,
                OUTPUT newEndDate,OUTPUT newEndTime).
    ASSIGN
        lvStartDateTime = numericDateTime(newStartDate,newStartTime)
        lvEndDateTime   = numericDateTime(newEndDate,newEndTime)
        .
    FOR EACH ttblDowntime
        WHERE ttblDowntime.resource      EQ ipResource
          AND ttblDowntime.startDateTime GE lvStartDateTime
        :
        IF ttblDowntime.startDateTime GE lvEndDateTime THEN
        RETURN.
        lvDowntimeSpan = lvDowntimeSpan
                       + timeSpan(newStartDate,newStartTime,
                                  newStartDate,newEndTime).
        RUN newEnd (ipTimeSpan + lvDowntimeSpan,newStartDate,newStartTime,
                    OUTPUT newEndDate,OUTPUT newEndTime).
        lvEndDateTime = numericDateTime(newEndDate,newEndTime).
    END.

END PROCEDURE.

PROCEDURE firstAvailable :
/*------------------------------------------------------------------------------
  Purpose:     find first available time slot for job
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipMachine       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipTimeSpan      AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipStartDateTime AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipEndDateTime   AS DECIMAL   NO-UNDO.

    DEFINE OUTPUT PARAMETER opStartDate AS DATE    NO-UNDO.
    DEFINE OUTPUT PARAMETER opStartTime AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opEndDate   AS DATE    NO-UNDO.
    DEFINE OUTPUT PARAMETER opEndTime   AS INTEGER NO-UNDO.
  
    DEFINE VARIABLE lvStartDate     AS DATE    NO-UNDO.
    DEFINE VARIABLE lvStartTime     AS INTEGER NO-UNDO.
    DEFINE VARIABLE lvEndDate       AS DATE    NO-UNDO.
    DEFINE VARIABLE lvEndTime       AS INTEGER NO-UNDO.
    DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lvEndDateTime   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lvTimeSpan      AS INTEGER NO-UNDO.
    DEFINE VARIABLE iDay            AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMonth          AS INTEGER NO-UNDO.
    DEFINE VARIABLE iYear           AS INTEGER NO-UNDO.

    DEFINE BUFFER ttblJob FOR ttblJob.

    &IF DEFINED(DebugLog) NE 0 &THEN
    EXPORT
        " start firstAvailable"
        ipMachine
        ipStartDateTime
        ipEndDateTime
        "checkJobConflict(ipMachine,ipStartDateTime,ipEndDateTime):"
        checkJobConflict(ipMachine,ipStartDateTime,ipEndDateTime)
        .
    &ENDIF
    IF checkJobConflict(ipMachine,ipStartDateTime,ipEndDateTime) THEN
        FOR EACH ttblJob
            WHERE ttblJob.{1}            EQ ipMachine
              AND (ttblJob.startDateTime GE ipStartDateTime
               OR  ttblJob.endDateTime   GE ipStartDateTime)
            :
            ASSIGN
                lvStartDate = ttblJob.endDate
                lvStartTime = ttblJob.endTime
                .
            lvStartDateTime = numericDateTime(lvStartDate,lvStartTime).
            RUN newEnd (ipTimeSpan,lvStartDate,lvStartTime,OUTPUT lvEndDate,OUTPUT lvEndTime).
            RUN downtimeLoop (
                ipMachine,
                ipTimeSpan,
                lvStartDate,
                lvStartTime,
                INPUT-OUTPUT lvEndDate,
                INPUT-OUTPUT lvEndTime
                ).
            lvEndDateTime = numericDateTime(lvEndDate,lvEndTime).
            IF checkJobConflict(ipMachine,lvStartDateTime,lvEndDateTime) THEN NEXT.
            ASSIGN
                opStartDate = lvStartDate
                opStartTime = lvStartTime
                opEndDate   = lvEndDate
                opEndTime   = lvEndTime
                .
            &IF DEFINED(DebugLog) NE 0 &THEN
            EXPORT
                "return firstAvailable"
                ipMachine
                lvStartDateTime
                lvEndDateTime
                opStartDate
                STRING(opStartTime,"hh:mm:ss am")
                opEndDate
                STRING(opEndTime,"hh:mm:ss am")
                .
            PUT SKIP(1).
            &ENDIF
            RETURN.
        END. /* each ttblJob */
    ELSE DO:
        ASSIGN
            iYear       = INTEGER(SUBSTRING(ENTRY(1,STRING(ipStartDateTime),"."),1,4))
            iMonth      = INTEGER(SUBSTRING(ENTRY(1,STRING(ipStartDateTime),"."),5,2))
            iDay        = INTEGER(SUBSTRING(ENTRY(1,STRING(ipStartDateTime),"."),7,2))
            lvStartDate = DATE(iMonth,iDay,iYear)
            lvStartTime = INTEGER(ENTRY(2,STRING(ipStartDateTime,"99999999.99999"),"."))
            iYear       = INTEGER(SUBSTRING(ENTRY(1,STRING(ipEndDateTime),"."),1,4))
            iMonth      = INTEGER(SUBSTRING(ENTRY(1,STRING(ipEndDateTime),"."),5,2))
            iDay        = INTEGER(SUBSTRING(ENTRY(1,STRING(ipEndDateTime),"."),7,2))
            lvEndDate   = DATE(iMonth,iDay,iYear)
            lvEndTime   = INTEGER(ENTRY(2,STRING(ipEndDateTime,"99999999.99999"),"."))
            .
        RUN downtimeLoop (
            ipMachine,
            ipTimeSpan,
            lvStartDate,
            lvStartTime,
            INPUT-OUTPUT lvEndDate,
            INPUT-OUTPUT lvEndTime
            ).
        ASSIGN
            opStartDate = lvStartDate
            opStartTime = lvStartTime
            opEndDate   = lvEndDate
            opEndTime   = lvEndTime
            .
    END. /* else */
    ASSIGN
        lvStartDateTime = numericDateTime(opStartDate,opStartTime)
        lvEndDateTime = numericDateTime(opEndDate,opEndTime)
        .
    &IF DEFINED(DebugLog) NE 0 &THEN
    EXPORT
        "   end firstAvailable"
        ipMachine
        lvStartDateTime
        lvEndDateTime
        opStartDate
        STRING(opStartTime,"hh:mm:ss am")
        opEndDate
        STRING(opEndTime,"hh:mm:ss am")
        .
    PUT SKIP(1).
    &ENDIF

END PROCEDURE.

PROCEDURE newEnd :
/*------------------------------------------------------------------------------
  Purpose:     calculate new ending date & time
  Parameters:  inputs timespan, start date & time, output new end date & time
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipTimeSpan   AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER newStartDate AS DATE    NO-UNDO.
    DEFINE INPUT PARAMETER newStartTime AS INTEGER NO-UNDO.
  
    DEFINE OUTPUT PARAMETER newEndDate  AS DATE    NO-UNDO.
    DEFINE OUTPUT PARAMETER newEndTime  AS INTEGER NO-UNDO.
  
    DEFINE VARIABLE days AS INTEGER NO-UNDO.
  
    ASSIGN
        newEndTime = newStartTime + ipTimeSpan
        days       = TRUNCATE(newEndTime / 86400,0)
        newEndDate = newStartDate + days
        newEndTime = newEndTime - days * 86400
        .

END PROCEDURE.

PROCEDURE newStart :
/*------------------------------------------------------------------------------
  Purpose:     calculate new starting date & time
  Parameters:  inputs timespan and end date & time, output new start date & time
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipTimeSpan AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER newEndDate AS DATE    NO-UNDO.
    DEFINE INPUT PARAMETER newEndTime AS INTEGER NO-UNDO.
  
    DEFINE OUTPUT PARAMETER newStartDate AS DATE    NO-UNDO.
    DEFINE OUTPUT PARAMETER newStartTime AS INTEGER NO-UNDO.
  
    DEFINE VARIABLE days AS INTEGER NO-UNDO.
    DEFINE VARIABLE i    AS INTEGER NO-UNDO.
  
    IF ipTimeSpan GT newEndTime THEN
        ASSIGN
            i            = ipTimeSpan - newEndTime
            days         = TRUNCATE(i / 86400,0)
            newStartTime = 86400 - (i - days * 86400)
            newStartDate = newEndDate - days - (IF i / 86400 GT 0 THEN 1 ELSE 0)
            .
    ELSE
        ASSIGN
            newStartTime = newEndTime - ipTimeSpan
            newStartDate = newEndDate
            .

END PROCEDURE.
