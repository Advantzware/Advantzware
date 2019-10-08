&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : custom/schedule.p
    Purpose     : persistent procedures used to set job-mch date & times

    Syntax      : RUN custom/schedule.p PERSISTENT SET scheduleHndl.

    Description : 

    Author(s)   : Ron Stark
    Created     : 4.15.2005
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE debugCode NO
&SCOPED-DEFINE runLive YES

DEFINE BUFFER bJob FOR job.
DEFINE BUFFER bJobMch FOR job-mch.

DEFINE TEMP-TABLE ttblJob NO-UNDO
  FIELD job AS CHARACTER
  FIELD startDateTime AS DECIMAL
  FIELD endDateTime AS DECIMAL
  FIELD startDate AS DATE
  FIELD startTime AS INTEGER
  FIELD endDate AS DATE
  FIELD endTime AS INTEGER
    INDEX dataTimeIdx IS PRIMARY startDateTime endDateTime.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-checkJobConflict) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkJobConflict Procedure 
FUNCTION checkJobConflict RETURNS LOGICAL
  (ipStartDateTime AS DECIMAL,ipEndDateTime AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fixTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fixTime Procedure 
FUNCTION fixTime RETURNS INTEGER
  (ipTime AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-numericDateTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD numericDateTime Procedure 
FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-timeSpan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD timeSpan Procedure 
FUNCTION timeSpan RETURNS INTEGER
  (ipStartDate AS DATE,ipStartTime AS INTEGER,ipEndDate AS DATE,ipEndTime AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-calcEnd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcEnd Procedure 
PROCEDURE calcEnd :
/*------------------------------------------------------------------------------
  Purpose:     calculate ending date/time based on start date/time & mr/run hrs
  Parameters:  date, time, mr hr, run hr
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipTime AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipMR AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipRun AS DECIMAL NO-UNDO.

  DEFINE OUTPUT PARAMETER opDate AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER opTime AS INTEGER NO-UNDO.

  DEFINE VARIABLE totalTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE days AS INTEGER NO-UNDO.

  IF ipTime EQ ? THEN ipTime = 0.
  IF ipMR EQ ? THEN ipMR = 0.
  IF ipRun EQ ? THEN ipRun = 0.
  ASSIGN
    totalTime = ipTime + ipMR * 3600 + ipRun * 3600
    days = TRUNCATE(totalTime / 86400,0)
    opDate = ipDate + days
    opTime = totalTime - days * 86400.
  IF opDate EQ ? THEN opDate = ipDate.
  IF opTime EQ ? THEN opTime = ipTime.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-downtimeSpan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE downtimeSpan Procedure 
PROCEDURE downtimeSpan :
/*------------------------------------------------------------------------------
  Purpose:     calculate new ending date & time and downtime span value
  Parameters:  fixed job time span, new start date & time,
               output new end date & time, downtime span
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMachine AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipTimeSpan AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipStartDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipStartTime AS INTEGER NO-UNDO.
  
  DEFINE INPUT-OUTPUT PARAMETER iopEndDate AS DATE NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopEndTime AS INTEGER NO-UNDO.

  DEFINE VARIABLE lvCapacity AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST mach-calendar
                  WHERE mach-calendar.company EQ ipCompany
                    AND mach-calendar.m-code EQ ipMachine
                    AND mach-calendar.m-date GE ipStartDate) THEN
  RETURN.
  
  FOR EACH mach-calendar NO-LOCK
      WHERE mach-calendar.company EQ ipCompany
        AND mach-calendar.m-code EQ ipMachine
        AND mach-calendar.m-date GE ipStartDate:
    IF ipStartDate EQ mach-calendar.m-date AND
       ipStartTime GT mach-calendar.end-time THEN NEXT.
    lvCapacity = lvCapacity + timeSpan(mach-calendar.m-date,mach-calendar.start-time,
                                       mach-calendar.m-date,mach-calendar.end-time).
    IF lvCapacity LT ipTimeSpan THEN NEXT.
    ASSIGN
      iopEndDate = mach-calendar.m-date
      iopEndTime = mach-calendar.end-time - (lvCapacity - ipTimeSpan).
    RETURN.
  END. /* each mach-calendar */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-firstAvailable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE firstAvailable Procedure 
PROCEDURE firstAvailable :
/*------------------------------------------------------------------------------
  Purpose:     find first available time slot for job
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMachine AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipTimeSpan AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipStartDateTime AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipEndDateTime AS DECIMAL NO-UNDO.

  DEFINE INPUT-OUTPUT PARAMETER opStartDate AS DATE NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER opStartTime AS INTEGER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER opEndDate AS DATE NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER opEndTime AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvTimeSpan AS INTEGER NO-UNDO.

  IF checkJobConflict(ipStartDateTime,ipEndDateTime) THEN
  FOR EACH ttblJob NO-LOCK WHERE ttblJob.startDateTime GE ipStartDateTime
                              OR ttblJob.endDateTime GE ipStartDateTime:
    RUN getStartCapacity (ipCompany,ipMachine,ttblJob.endDate,ttblJob.endTime,
                          OUTPUT lvStartDate,OUTPUT lvStartTime).
    lvStartDateTime = numericDateTime(lvStartDate,lvStartTime).
    RUN newEnd (ipTimeSpan,lvStartDate,lvStartTime,OUTPUT lvEndDate,OUTPUT lvEndTime).
    RUN downtimeSpan (ipCompany,ipMachine,ipTimeSpan,lvStartDate,lvStartTime,
                      INPUT-OUTPUT lvEndDate,INPUT-OUTPUT lvEndTime).
    lvEndDateTime = numericDateTime(lvEndDate,lvEndTime).
    IF checkJobConflict(lvStartDateTime,lvEndDateTime) THEN NEXT.
    ASSIGN
      opStartDate = lvStartDate
      opStartTime = lvStartTime
      opEndDate = lvEndDate
      opEndTime = lvEndTime.
    RETURN.
  END. /* each ttbljob */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStartCapacity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getStartCapacity Procedure 
PROCEDURE getStartCapacity :
/*------------------------------------------------------------------------------
  Purpose:     find first avail capacity record to set start date & time
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMachine AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipStartDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipStartTime AS INTEGER NO-UNDO.
  
  DEFINE OUTPUT PARAMETER opStartDate AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER opStartTime AS INTEGER NO-UNDO.

  ASSIGN
    opStartDate = ipStartDate
    opStartTime = ipStartTime.

  IF CAN-FIND(FIRST mach-calendar
              WHERE mach-calendar.company EQ ipCompany
                AND mach-calendar.m-code EQ ipMachine
                AND mach-calendar.m-date EQ ipStartDate
                AND mach-calendar.start-time LE ipStartTime
                AND mach-calendar.end-time GE ipStartTime) THEN
  RETURN.
  
  FIND FIRST mach-calendar NO-LOCK
       WHERE mach-calendar.company EQ ipCompany
         AND mach-calendar.m-code EQ ipMachine
         AND mach-calendar.m-date EQ ipStartDate NO-ERROR.
  IF AVAILABLE mach-calendar THEN
  DO:
    IF ipStartTime LT mach-calendar.start-time THEN
    opStartTime = mach-calendar.start-time.
    ELSE
    DO:
      FIND FIRST mach-calendar NO-LOCK
           WHERE mach-calendar.company EQ ipCompany
             AND mach-calendar.m-code EQ ipMachine
             AND mach-calendar.m-date GT ipStartDate NO-ERROR.
      IF AVAILABLE mach-calendar THEN
      ASSIGN
        opStartDate = mach-calendar.m-date
        opStartTime = mach-calendar.start-time.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-newEnd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newEnd Procedure 
PROCEDURE newEnd :
/*------------------------------------------------------------------------------
  Purpose:     calculate new ending date & time
  Parameters:  inputs timespan, start date & time, output new end date & time
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipTimeSpan AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER newStartDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER newStartTime AS INTEGER NO-UNDO.
  
  DEFINE OUTPUT PARAMETER newEndDate AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER newEndTime AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE days AS INTEGER NO-UNDO.
  
  ASSIGN
    newEndTime = newStartTime + ipTimeSpan
    days = TRUNCATE(newEndTime / 86400,0)
    newEndDate = newStartDate + days
    newEndTime = newEndTime - days * 86400.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-newStart) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newStart Procedure 
PROCEDURE newStart :
/*------------------------------------------------------------------------------
  Purpose:     calculate new starting date & time
  Parameters:  inputs timespan and end date & time, output new start date & time
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipTimeSpan AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER newEndDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER newEndTime AS INTEGER NO-UNDO.
  
  DEFINE OUTPUT PARAMETER newStartDate AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER newStartTime AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE days AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  
  IF ipTimeSpan GT newEndTime THEN
  ASSIGN
    i = ipTimeSpan - newEndTime
    days = TRUNCATE(i / 86400,0)
    newStartTime = 86400 - (i - days * 86400)
    newStartDate = newEndDate - days - (IF i / 86400 GT 0 THEN 1 ELSE 0).
  ELSE
  ASSIGN
    newStartTime = newEndTime - ipTimeSpan
    newStartDate = newEndDate.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-scheduleJob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE scheduleJob Procedure 
PROCEDURE scheduleJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE OUTPUT PARAMETER opStartDate AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER opDueDate AS DATE NO-UNDO.

  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvTimeSpan AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDateMR AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTimeMR AS INTEGER NO-UNDO.

&IF '{&debugCode}' EQ 'YES' &THEN
  &SCOPED-DEFINE debugFile c:/tmp/schedule.txt
  OUTPUT TO {&debugFile}.
  PUT UNFORMATTED 'NOTE: Definitions Section of custom/schedule.p:' SKIP(1)
    '~&Scop debugCode {&debugCode}' SKIP
    '~&Scop runLive   {&runLive}' SKIP.
&ENDIF
  
  FIND job NO-LOCK WHERE ROWID(job) EQ ipRowID NO-ERROR.
  IF NOT AVAILABLE job THEN RETURN.

  ASSIGN
    lvStartDate = IF job.start-date GE TODAY AND job.start-date NE ? THEN job.start-date ELSE TODAY
    lvStartTime = IF lvStartDate EQ TODAY THEN TIME ELSE 0.
  FOR EACH job-mch EXCLUSIVE-LOCK
      WHERE job-mch.company EQ job.company
        AND job-mch.job EQ job.job
        AND job-mch.run-complete EQ NO,
      FIRST mach NO-LOCK WHERE mach.company EQ job-mch.company
                           AND mach.loc EQ job.loc
                           AND mach.m-code EQ job-mch.m-code
      BY job.job BY job-mch.frm BY job-mch.blank-no BY mach.d-seq
      BY mach.m-seq BY job-mch.pass BY job-mch.m-code:
    RUN ttblJobCreate (job-mch.company,job-mch.m-code,ROWID(job-mch)).
    RUN calcEnd (lvStartDate,lvStartTime,job-mch.mr-hr,job-mch.run-hr,
                 OUTPUT lvEndDate,OUTPUT lvEndTime).
    ASSIGN
      lvStartDateTime = numericDateTime(lvStartDate,lvStartTime)
      lvEndDateTime = numericDateTime(lvEndDate,lvEndTime)
      lvTimeSpan = timeSpan(lvStartDate,lvStartTime,lvEndDate,lvEndTime).
    RUN firstAvailable (job-mch.company,job-mch.m-code,
                        lvTimeSpan,lvStartDateTime,lvEndDateTime,
                        INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime,
                        INPUT-OUTPUT lvEndDate,INPUT-OUTPUT lvEndTime).
    RUN calcEnd (lvStartDate,lvStartTime,job-mch.mr-hr,0,
                 OUTPUT lvEndDateMR,OUTPUT lvEndTimeMR).
&IF '{&runLive}' EQ 'YES' &THEN
    ASSIGN
      job-mch.start-date-su = lvStartDate
      job-mch.start-time-su = lvStartTime
      job-mch.end-date-su = lvEndDateMR
      job-mch.end-time-su = lvEndTimeMR
      job-mch.start-date = lvEndDateMR
      job-mch.start-time = lvEndTimeMR
      job-mch.end-date = lvEndDate
      job-mch.end-time = lvEndTime.
&ENDIF
&IF '{&debugCode}' EQ 'YES' &THEN
    DISP job-mch.m-code LABEL 'Machine'
      job-mch.job-no LABEL 'Job' job-mch.job-no2 NO-LABEL
      job-mch.start-date-su LABEL 'SDate SU'
      job-mch.start-time-su LABEL 'STime SU'
      STRING(job-mch.start-time-su,'hh:mm:ss') @ job-mch.start-time-su
      job-mch.end-date-su LABEL 'EDate SU'
      job-mch.end-time-su LABEL 'ETime SU'
      STRING(job-mch.end-time-su,'hh:mm:ss') @ job-mch.end-time-su
      job-mch.start-date LABEL 'SDateRun'
      job-mch.start-time LABEL 'STimeRun'
      STRING(job-mch.start-time,'hh:mm:ss') @ job-mch.start-time
      job-mch.end-date COLUMN-LABEL 'EDateRun'
      job-mch.end-time LABEL 'ETimeRun'
      STRING(job-mch.end-time,'hh:mm:ss') @ job-mch.end-time
      job-mch.mr-hr LABEL 'MR/Run'
      WITH STREAM-IO WIDTH 132.
    DOWN.
    DISP lvStartDate @ job-mch.start-date-su
      STRING(lvStartTime,'hh:mm:ss') @ job-mch.start-time-su
      lvEndDateMR @ job-mch.end-date-su
      STRING(lvEndTimeMR,'hh:mm:ss') @ job-mch.end-time-su
      lvEndDateMR @ job-mch.start-date
      STRING(lvEndTimeMR,'hh:mm:ss') @ job-mch.start-time
      lvEndDate @ job-mch.end-date
      STRING(lvEndTime,'hh:mm:ss') @ job-mch.end-time
      job-mch.run-hr @ job-mch.mr-hr.
&ENDIF
    IF opStartDate EQ ? THEN opStartDate = lvStartDate.
    ASSIGN
      lvStartDate = lvEndDate
      lvStartTime = lvEndTime.
  END. /* each job-mch */
  opDueDate = lvEndDate + 1.
&IF '{&debugCode}' EQ 'YES' &THEN
  DISPLAY opStartDate opDueDate WITH STREAM-IO SIDE-LABELS 1 COLUMN.
  OUTPUT CLOSE.
&IF DEFINED(FWD-VERSION) > 0 &THEN
  open-mime-resource "text/plain" string("file:///" + value({&debugFile})) false.
&ELSE
  OS-COMMAND NO-WAIT notepad.exe {&debugFile}.
&ENDIF
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ttblJobCreate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttblJobCreate Procedure 
PROCEDURE ttblJobCreate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMachine AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.

  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvDowntimeSpan AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvTimeSpan AS INTEGER NO-UNDO.

  EMPTY TEMP-TABLE ttblJob.
  lvStartDateTime = numericDateTime(TODAY,TIME).
  FOR EACH bJobMch NO-LOCK WHERE bJobMch.company EQ ipCompany
                             AND bJobMch.m-code EQ ipMachine
                             AND bJobMch.run-complete EQ NO
                             AND ROWID(bJobMch) NE ipRowId
      BY bJobMch.start-date-su BY bJobMch.start-time-su
      BY bJobMch.end-date BY bJobMch.end-time:
    IF CAN-FIND(FIRST job-hdr WHERE job-hdr.company EQ bJobMch.company
                                AND job-hdr.job EQ bJobMch.job
                                AND job-hdr.opened EQ NO) THEN NEXT.
    ASSIGN
      lvEndDate = bJobMch.end-date
      lvEndTime = fixTime(bJobMch.end-time)
      lvEndDateTime = numericDateTime(lvEndDate,lvEndTime).
    IF bJobMch.start-date-su EQ ? OR lvStartDateTime GT lvEndDateTime THEN NEXT.
    ASSIGN
      lvStartDate = bJobMch.start-date-su
      lvStartTime = fixTime(bJobMch.start-time-su).
    CREATE ttblJob.
    ASSIGN
      ttblJob.job = bJobMch.job-no + '-' + STRING(bJobMch.job-no2) + '.' + STRING(bJobMch.frm)
      ttblJob.startDate = lvStartDate
      ttblJob.startTime = lvStartTime
      ttblJob.endDate = lvEndDate
      ttblJob.endTime = lvEndTime
      ttblJob.endDateTime = lvEndDateTime.
    ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
&IF '{&debugCode}' EQ 'YES' &THEN
    DISPLAY ttblJob EXCEPT startDateTime endDateTime WITH STREAM-IO WIDTH 132.
    DISPLAY STRING(ttblJob.startTime,'hh:mm:ss') @ ttblJob.startTime
      STRING(ttblJob.endTime,'hh:mm:ss') @ ttblJob.endTime.
&ENDIF
  END. /* each bjobmch */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-checkJobConflict) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkJobConflict Procedure 
FUNCTION checkJobConflict RETURNS LOGICAL
  (ipStartDateTime AS DECIMAL,ipEndDateTime AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN CAN-FIND(FIRST ttblJob
                  WHERE (ttblJob.startDateTime GE ipStartDateTime
                    AND ttblJob.startDateTime LT ipEndDateTime)
                     OR (ttblJob.endDateTime LE ipEndDateTime
                    AND ttblJob.endDateTime GT ipStartDateTime)
                     OR (ttblJob.startDateTime LE ipStartDateTime
                    AND ttblJob.endDateTime GE ipEndDateTime)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fixTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fixTime Procedure 
FUNCTION fixTime RETURNS INTEGER
  (ipTime AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  correct invalid time value
    Notes:  
------------------------------------------------------------------------------*/
  IF ipTime EQ ? THEN ipTime = 0.
  RETURN INTEGER(ipTime - TRUNCATE(ipTime / 86400,0) * 86400).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-numericDateTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION numericDateTime Procedure 
FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  put the date and time in decimal format YYYYMMDD.TTTTT
    Notes:  
------------------------------------------------------------------------------*/
  IF ipTime LT 0 THEN ipTime = 0.
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-timeSpan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION timeSpan Procedure 
FUNCTION timeSpan RETURNS INTEGER
  (ipStartDate AS DATE,ipStartTime AS INTEGER,ipEndDate AS DATE,ipEndTime AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  calculate time span between 2 dates & times in seconds
    Notes:  
------------------------------------------------------------------------------*/
  RETURN IF ipStartDate EQ ipEndDate THEN ipEndTime - ipStartTime
         ELSE (86400 - ipStartTime) + (ipEndDate - ipStartDate - 1) * 86400 + ipEndTime.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

