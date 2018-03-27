&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : sbHTML.p
    Purpose     : web page generation of job potential starting date/time

    Syntax      : RUN schedule/sbHTML.p (ROWID(job)).

    Description : 

    Author(s)   : Ron Stark
    Created     : 12.17.2017
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER ipcType    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iprRowID   AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttJob NO-UNDO LIKE job-mch 
  FIELD jobMchRowID   AS ROWID 
  FIELD d-seq         AS INTEGER
  FIELD m-seq         AS INTEGER
    INDEX ttJob IS PRIMARY job frm blank-no d-seq m-seq pass m-code
    .
DEFINE TEMP-TABLE ttblJob NO-UNDO
  FIELD m-code        AS CHARACTER 
  FIELD job           AS CHARACTER
  FIELD frm           AS INTEGER 
  FIELD blank-no      AS INTEGER 
  FIELD pass          AS INTEGER 
  FIELD startDateTime AS DECIMAL
  FIELD endDateTime   AS DECIMAL
  FIELD startDate     AS DATE
  FIELD startTime     AS INTEGER
  FIELD endDate       AS DATE
  FIELD endTime       AS INTEGER
  FIELD newJob        AS LOGICAL 
    INDEX dataTimeIdx IS PRIMARY startDateTime endDateTime
    INDEX startDate startDate
    INDEX endDate endDate
    .
DEFINE BUFFER bTtblJob FOR ttblJob.
DEFINE BUFFER bJobMch  FOR ttJob.

DEFINE TEMP-TABLE ttTime NO-UNDO 
  FIELD timeSlice AS INTEGER 
  FIELD timeType1 AS CHARACTER 
  FIELD timeType2 AS CHARACTER 
  FIELD newJob    AS LOGICAL 
    INDEX ttTime IS PRIMARY timeSlice
    .

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

&IF DEFINED(EXCLUDE-fTimeSlice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fTimeSlice Procedure
FUNCTION fTimeSlice RETURNS LOGICAL 
  (ipiTimeSlice AS INTEGER, ipcTimeType AS CHARACTER, ipcTimeType2 AS CHARACTER, iplNewJob AS LOGICAL) FORWARD.

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
   Other Settings: CODE-ONLY COMPILE
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

SESSION:SET-WAIT-STATE ("General").
RUN pScheduleJob (iprRowID).
RUN pHTMLPage.
SESSION:SET-WAIT-STATE ("").

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
  DEFINE INPUT PARAMETER ipDate AS DATE    NO-UNDO.
  DEFINE INPUT PARAMETER ipTime AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipMR   AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipRun  AS DECIMAL NO-UNDO.

  DEFINE OUTPUT PARAMETER opDate AS DATE    NO-UNDO.
  DEFINE OUTPUT PARAMETER opTime AS INTEGER NO-UNDO.

  DEFINE VARIABLE totalTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE days      AS INTEGER NO-UNDO.

  IF ipTime EQ ? THEN ipTime = 0.
  IF ipMR EQ ? THEN ipMR     = 0.
  IF ipRun EQ ? THEN ipRun   = 0.
  ASSIGN
    totalTime = ipTime + ipMR * 3600 + ipRun * 3600
    days      = TRUNCATE(totalTime / 86400,0)
    opDate    = ipDate + days
    opTime    = totalTime - days * 86400
    .
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
  DEFINE INPUT PARAMETER ipCompany   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMachine   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipTimeSpan  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipStartDate AS DATE      NO-UNDO.
  DEFINE INPUT PARAMETER ipStartTime AS INTEGER   NO-UNDO.
  
  DEFINE INPUT-OUTPUT PARAMETER iopEndDate AS DATE    NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopEndTime AS INTEGER NO-UNDO.

  DEFINE VARIABLE lvCapacity AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST mach-calendar
                  WHERE mach-calendar.company EQ ipCompany
                    AND mach-calendar.m-code  EQ ipMachine
                    AND mach-calendar.m-date  GE ipStartDate) THEN
  RETURN.
  
  FOR EACH mach-calendar NO-LOCK
      WHERE mach-calendar.company EQ ipCompany
        AND mach-calendar.m-code  EQ ipMachine
        AND mach-calendar.m-date  GE ipStartDate
      :
    IF ipStartDate EQ mach-calendar.m-date AND
       ipStartTime GT mach-calendar.end-time THEN NEXT.
    lvCapacity = lvCapacity + timeSpan(mach-calendar.m-date,mach-calendar.start-time,
                                       mach-calendar.m-date,mach-calendar.end-time).
    IF lvCapacity LT ipTimeSpan THEN NEXT.
    ASSIGN
      iopEndDate = mach-calendar.m-date
      iopEndTime = mach-calendar.end-time - (lvCapacity - ipTimeSpan)
      .
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
  DEFINE INPUT PARAMETER ipCompany       AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMachine       AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipTimeSpan      AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipStartDateTime AS DECIMAL   NO-UNDO.
  DEFINE INPUT PARAMETER ipEndDateTime   AS DECIMAL   NO-UNDO.

  DEFINE INPUT-OUTPUT PARAMETER opStartDate AS DATE    NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER opStartTime AS INTEGER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER opEndDate   AS DATE    NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER opEndTime   AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE lvStartDate     AS DATE    NO-UNDO.
  DEFINE VARIABLE lvStartTime     AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDate       AS DATE    NO-UNDO.
  DEFINE VARIABLE lvEndTime       AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndDateTime   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvTimeSpan      AS INTEGER NO-UNDO.

  IF checkJobConflict(ipStartDateTime,ipEndDateTime) THEN
  FOR EACH ttblJob NO-LOCK
      WHERE ttblJob.m-code         EQ ipMachine
        AND (ttblJob.startDateTime GE ipStartDateTime
         OR  ttblJob.endDateTime   GE ipStartDateTime)
      :
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
      opEndDate   = lvEndDate
      opEndTime   = lvEndTime
      .
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
  DEFINE INPUT PARAMETER ipCompany    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMachine    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipStartDate  AS DATE      NO-UNDO.
  DEFINE INPUT PARAMETER ipStartTime  AS INTEGER   NO-UNDO.
  
  DEFINE OUTPUT PARAMETER opStartDate AS DATE      NO-UNDO.
  DEFINE OUTPUT PARAMETER opStartTime AS INTEGER   NO-UNDO.

  ASSIGN
    opStartDate = ipStartDate
    opStartTime = ipStartTime.

  IF CAN-FIND(FIRST mach-calendar
              WHERE mach-calendar.company    EQ ipCompany
                AND mach-calendar.m-code     EQ ipMachine
                AND mach-calendar.m-date     EQ ipStartDate
                AND mach-calendar.start-time LE ipStartTime
                AND mach-calendar.end-time   GE ipStartTime) THEN
  RETURN.
  
  FIND FIRST mach-calendar NO-LOCK
       WHERE mach-calendar.company EQ ipCompany
         AND mach-calendar.m-code  EQ ipMachine
         AND mach-calendar.m-date  EQ ipStartDate
       NO-ERROR.
  IF AVAILABLE mach-calendar THEN DO:
    IF ipStartTime LT mach-calendar.start-time THEN
    opStartTime = mach-calendar.start-time.
    ELSE DO:
      FIND FIRST mach-calendar NO-LOCK
           WHERE mach-calendar.company EQ ipCompany
             AND mach-calendar.m-code  EQ ipMachine
             AND mach-calendar.m-date  GT ipStartDate
           NO-ERROR.
      IF AVAILABLE mach-calendar THEN
      ASSIGN
        opStartDate = mach-calendar.m-date
        opStartTime = mach-calendar.start-time
        .
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pHTMLPage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHTMLPage Procedure
PROCEDURE pHTMLPage:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    &Scoped-define fontFace Arial, Helvetica, sans-serif
    
    DEFINE VARIABLE cDays       AS CHARACTER NO-UNDO INITIAL "Sun,Mon,Tue,Wed,Thu,Fri,Sat".    
    DEFINE VARIABLE lAltLine    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cBGColor    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartDate AS DATE      NO-UNDO.
    DEFINE VARIABLE dtEndDate   AS DATE      NO-UNDO.
    DEFINE VARIABLE dtDate      AS DATE      NO-UNDO.
    DEFINE VARIABLE iJobs       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTime       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iStartTime  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iEndTime    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cType1      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cType2      AS CHARACTER NO-UNDO.
    
    FIND FIRST ttblJob
         WHERE ttblJob.newJob EQ YES 
         USE-INDEX startDate
         NO-ERROR.
    IF NOT AVAILABLE ttblJob THEN RETURN.
    dtStartDate = ttblJob.startDate.
    FIND LAST ttblJob
         WHERE ttblJob.newJob EQ YES 
         USE-INDEX startDate.
    dtEndDate = ttblJob.endDate.
    OUTPUT TO "c:\tmp\sbHTML.htm".
    PUT UNFORMATTED
        '<html>' SKIP
        '<head>' SKIP
        '<title>Schedule Job: ' job.job-no '-' job.job-no2 '</title>' SKIP
        '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' SKIP
        '<meta http-equiv="Refresh" content="120">' SKIP
        '</head>' SKIP
        '<a name="Top"></a>' SKIP
        '<form>' SKIP
        '<fieldset>' SKIP
        '  <legend><font face="{&fontFace}">Schedule Job: <b>' job.job-no '-' job.job-no2 '</b> (updated '
        STRING(TODAY,'99.99.9999') ' @ ' STRING(TIME,'hh:mm:ss am') ')</font>'
        '~&nbsp;</legend>' SKIP
        '  <img src="' SEARCH("Graphics/asiicon.ico")
        '" align="middle">~&nbsp;<b><a href="http://www.advantzware.com" target="_blank">'
        '<font face="{&fontFace}">Advantzware, Inc.</a>~&nbsp;~&copy;</b></font>' SKIP
        '~&nbsp;~&nbsp;~&nbsp;~&nbsp;~&nbsp;<font face="{&fontFace}"><font color="#FF0000"><b>Projected Completion: </font>'
        ttblJob.endDate ' - ' STRING(ttblJob.endTime,"hh:mm:ss am")
        '</font></b>' SKIP 
        '  <table align="right" cellspacing="2" cellpadding="8">' SKIP
        '    <tr>' SKIP 
        '      <td><font face="{&fontFace}">Legend:</font></td>' SKIP 
        '      <td bgcolor="#00CCFF"><font face="{&fontFace}"><b>Job: ' job.job-no '-' job.job-no2 '</b></font></td>' SKIP  
        '      <td bgcolor="#C0BEBE"><font face="{&fontFace}"><b>Downtime</b></font></td>' SKIP 
        '      <td bgcolor="#AAD5B9"><font face="{&fontFace}"><b>Booked Job</b></font></td>' SKIP
        '      <td bgcolor="#F1FE98"><font face="{&fontFace}"><b>Available</b></font></td>' SKIP 
        '    </tr>' SKIP  
        '  </table>' SKIP 
        '  <table border="1" cellspacing="0" cellpadding="5" width="100%">' SKIP
        '    <tr>' SKIP
        '      <td bgcolor="#C0BEBE" align="center" nowrap width="13%"><font face="{&fontFace}"><b>'
        'Operation</b></font></td>' SKIP
        .
        DO dtDate = dtStartDate TO dtEndDate:
            PUT UNFORMATTED
                '      <td bgcolor="#C0BEBE" align="center" nowrap><font face="{&fontFace}">'
                ENTRY(WEEKDAY(dtDate),cDays) ' ' MONTH(dtDate) '/' DAY(dtDate) '</font></td>' SKIP
                .
        END. /* do dtdate */
    PUT UNFORMATTED '    </tr>' SKIP.
    
    FOR EACH ttblJob
        WHERE ttblJob.newJob EQ YES
        BREAK BY ttblJob.startDateTime
              BY ttblJob.m-code
        :
        PUT UNFORMATTED
            '    <tr>' SKIP
            '      <td' cBGColor ' align="left" nowrap><font face="{&fontFace}">'
            '<img src="'
            (IF SEARCH("Graphics/48x48/" + ttblJob.m-code + ".png") NE ? THEN
                SEARCH("Graphics/48x48/" + ttblJob.m-code + ".png") ELSE
                SEARCH("Graphics/48x48/gearwheels.png"))
            '" width="48" height="48" align="left">~&nbsp~&nbsp~&nbsp~&nbsp<b>'
            ttblJob.m-code '</b> (f:<b>' ttblJob.frm
            '</b> b:<b>' ttblJob.blank-no
            '</b> p:<b>' ttblJob.pass ')</b><br>'
            ttblJob.startDate ' - ' STRING(ttblJob.startTime,"hh:mm:ss am") '<br>'
            ttblJob.endDate ' - ' STRING(ttblJob.endTime,"hh:mm:ss am") '</font></td>' SKIP
            .
        DO dtDate = dtStartDate TO dtEndDate:
            EMPTY TEMP-TABLE ttTime.
            PUT UNFORMATTED
                '      <td' cBGColor ' align="center" nowrap><font face="{&fontFace}">' SKIP
                .
            iJobs = 0.
            FOR EACH bTtblJob
                WHERE bTtblJob.m-code EQ ttblJob.m-code
                  AND (bTtblJob.startDate EQ dtDate
                   OR (bTtblJob.startDate LT dtDate
                  AND  bTtbljob.endDate GT dtDate)
                   OR  bTtblJob.endDate EQ dtDate)
                BY bTtblJob.startDateTime
                :
                iStartTime = IF bTtblJob.startDate EQ dtDate THEN bTtblJob.startTime ELSE 0.
                fTimeSlice (iStartTime,"Job","Start",ROWID(bTtblJob) EQ ROWID(ttblJob)).
                iEndTime = IF bTtblJob.endDate EQ dtDate THEN bTtblJob.endTime ELSE 86400.
                fTimeSlice (iEndTime,"Job","End",ROWID(bTtblJob) EQ ROWID(ttblJob)).
                iJobs = iJobs + 1.
            END. /* each bttbljob */
            PUT UNFORMATTED 
                '        <table border="1" cellspacing="0" cellpadding="8" width="100%">' SKIP
                '          <tr>' SKIP  
                .
            fTimeSlice (0,"Avail","Start",NO).
            fTimeSlice (86400,"Avail","End",NO).
            FOR EACH ttTime BREAK BY ttTime.timeSlice:
                IF ttTime.timeSlice EQ 0 THEN DO:
                    ASSIGN
                        iTime  = 0
                        cType1 = ttTime.timeType1
                        cType2 = ttTime.timeType2
                        
                        .
                    NEXT.
                END. /* timeslice eq 0 */
                IF ttTime.timeType2 NE "Start" OR cType2 NE "Start" THEN
                cType1 = ttTime.timeType1.
                PUT UNFORMATTED
                    '            <td bgcolor="#'
                    (IF ttTime.newJob AND ttTime.timeType2 EQ "End" THEN "00CCFF" ELSE
                     IF cType1 EQ "Avail" THEN "F1FE98" ELSE
                     IF cType1 EQ "Job"   THEN "AAD5B9" ELSE "C0BEBE")
                    '" align="center" width="' ROUND((ttTime.timeSlice - iTime) / 86400 * 100,0) '%' '" nowrap>~&nbsp;'
                    '</td>' SKIP
                    .
                ASSIGN
                    iTime  = ttTime.timeSlice
                    cType1 = ttTime.timeType1
                    cType2 = ttTime.timeType2
                    .
            END. /* each tttime */
            PUT UNFORMATTED
                '            </td>' SKIP
                '          </tr>' SKIP 
                '        </table>' SKIP 
                '      </font></td>' SKIP
                .
        END. /* do dtdate */
        PUT UNFORMATTED
            '    </tr>' SKIP
            .
        ASSIGN
            lAltLine = NOT lAltLine
            cBGColor = IF lAltLine THEN ' bgcolor="EEDFD2"' ELSE ''
            .
    END. /* each ttbljob */
    PUT UNFORMATTED
        '  </table>' SKIP
        '  <div align="left"><font face="{&fontFace}"><a href="#Top">Top</a></font>' SKIP
        '  <div align="right"><font face="{&fontFace}">~&copy; Advantzware, Inc., All Rights Reserved</font></div>' SKIP
        '</fieldset>' SKIP
        '</form>' SKIP
        '</html>' SKIP
        .
    OUTPUT CLOSE.
    OS-COMMAND NO-WAIT START "c:\tmp\sbHTML.htm".

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-scheduleJob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pScheduleJob Procedure 
PROCEDURE pScheduleJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iprRowID AS ROWID     NO-UNDO.

  DEFINE VARIABLE lvStartDate     AS DATE      NO-UNDO.
  DEFINE VARIABLE lvStartTime     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lvEndDate       AS DATE      NO-UNDO.
  DEFINE VARIABLE lvEndTime       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE lvEndDateTime   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE lvTimeSpan      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lvEndDateMR     AS DATE      NO-UNDO.
  DEFINE VARIABLE lvEndTimeMR     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lvMachine       AS CHARACTER NO-UNDO.

  ASSIGN
    lvStartDate = TODAY
    lvStartTime = TIME
    .
  IF iprRowID NE ? THEN DO: 
      FIND job NO-LOCK WHERE ROWID(job) EQ iprRowID NO-ERROR.
      IF NOT AVAILABLE job THEN RETURN.
    
      ASSIGN
        lvStartDate = IF job.start-date GE TODAY AND job.start-date NE ? THEN job.start-date ELSE TODAY
        lvStartTime = IF lvStartDate EQ TODAY THEN TIME ELSE 0
        .
      FOR EACH job-mch NO-LOCK
          WHERE job-mch.company      EQ job.company
            AND job-mch.job          EQ job.job
            AND job-mch.run-complete EQ NO
          :
          FIND FIRST mach NO-LOCK
               WHERE mach.company EQ job-mch.company
                 AND mach.loc     EQ job.loc
                 AND mach.m-code  EQ job-mch.m-code
               NO-ERROR.
          IF NOT AVAILABLE mach THEN NEXT.
          lvMachine = IF mach.sch-m-code NE "" THEN mach.sch-m-code ELSE mach.m-code.
          CREATE ttJob.
          BUFFER-COPY job-mch TO ttJob
              ASSIGN 
                ttJob.jobMchRowID = ROWID(job-mch)
                ttJob.d-seq       = mach.d-seq
                ttJob.m-seq       = mach.m-seq
                ttJob.m-code      = lvMachine
                . 
      END. /* each job-mch */
  END. /* iprrowid ne ? */

  FOR EACH ttJob USE-INDEX ttJob:
    RUN ttblJobCreate (ttJob.company,ttJob.m-code,ROWID(ttJob)).
    RUN calcEnd (lvStartDate,lvStartTime,ttJob.mr-hr,ttJob.run-hr,
                 OUTPUT lvEndDate,OUTPUT lvEndTime).
    ASSIGN
      lvStartDateTime = numericDateTime(lvStartDate,lvStartTime)
      lvEndDateTime   = numericDateTime(lvEndDate,lvEndTime)
      lvTimeSpan      = timeSpan(lvStartDate,lvStartTime,lvEndDate,lvEndTime)
      .
    RUN firstAvailable (ttJob.company,ttJob.m-code,
                        lvTimeSpan,lvStartDateTime,lvEndDateTime,
                        INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime,
                        INPUT-OUTPUT lvEndDate,INPUT-OUTPUT lvEndTime).
    RUN calcEnd (lvStartDate,lvStartTime,ttJob.mr-hr,0,
                 OUTPUT lvEndDateMR,OUTPUT lvEndTimeMR).
    CREATE ttblJob.
    ASSIGN
      ttblJob.m-code        = ttJob.m-code
      ttblJob.job           = ttJob.job-no + '-' + STRING(ttJob.job-no2) + '.' + STRING(ttJob.frm)
      ttblJob.frm           = ttJob.frm
      ttblJob.blank-no      = ttJob.blank-no
      ttblJob.pass          = ttJob.pass
      ttblJob.startDate     = lvStartDate
      ttblJob.startTime     = lvStartTime
      ttblJob.endDate       = lvEndDate
      ttblJob.endTime       = lvEndTime
      ttblJob.endDateTime   = lvEndDateTime
      ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime)
      ttblJob.newJob        = YES
      .
    ASSIGN
      lvStartDate = lvEndDate
      lvStartTime = lvEndTime
      .
  END. /* each ttjob */

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
  DEFINE INPUT PARAMETER ipRowID   AS ROWID     NO-UNDO.

  DEFINE VARIABLE lvStartDate     AS DATE    NO-UNDO.
  DEFINE VARIABLE lvStartTime     AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDate       AS DATE    NO-UNDO.
  DEFINE VARIABLE lvEndTime       AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndDateTime   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvDowntimeSpan  AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvTimeSpan      AS INTEGER NO-UNDO.

  lvStartDateTime = numericDateTime(TODAY,TIME).
  FOR EACH bJobMch NO-LOCK
      WHERE bJobMch.company      EQ ipCompany
        AND bJobMch.m-code       EQ ipMachine
        AND bJobMch.run-complete EQ NO
        AND ROWID(bJobMch)       NE ipRowId
      BY bJobMch.start-date-su BY bJobMch.start-time-su
      BY bJobMch.end-date BY bJobMch.end-time
      :
    IF CAN-FIND(FIRST job-hdr
                WHERE job-hdr.company EQ bJobMch.company
                  AND job-hdr.job     EQ bJobMch.job
                  AND job-hdr.opened  EQ NO) THEN NEXT.
    ASSIGN
      lvEndDate     = bJobMch.end-date
      lvEndTime     = fixTime(bJobMch.end-time)
      lvEndDateTime = numericDateTime(lvEndDate,lvEndTime)
      .
    IF bJobMch.start-date-su EQ ? OR lvStartDateTime GT lvEndDateTime THEN NEXT.
    ASSIGN
      lvStartDate = bJobMch.start-date-su
      lvStartTime = fixTime(bJobMch.start-time-su)
      .
    CREATE ttblJob.
    ASSIGN
      ttblJob.m-code        = ipMachine
      ttblJob.job           = bJobMch.job-no + '-' + STRING(bJobMch.job-no2) + '.' + STRING(bJobMch.frm)
      ttblJob.frm           = bJobMch.frm
      ttblJob.blank-no      = bJobMch.blank-no
      ttblJob.pass          = bJobMch.pass
      ttblJob.startDate     = lvStartDate
      ttblJob.startTime     = lvStartTime
      ttblJob.endDate       = lvEndDate
      ttblJob.endTime       = lvEndTime
      ttblJob.endDateTime   = lvEndDateTime
      ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime)
      ttblJob.newJob        = NO
      .
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
                    AND ttblJob.startDateTime  LT ipEndDateTime)
                     OR (ttblJob.endDateTime   LE ipEndDateTime
                    AND ttblJob.endDateTime    GT ipStartDateTime)
                     OR (ttblJob.startDateTime LE ipStartDateTime
                    AND ttblJob.endDateTime    GE ipEndDateTime)).

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

&IF DEFINED(EXCLUDE-fTimeSlice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fTimeSlice Procedure
FUNCTION fTimeSlice RETURNS LOGICAL 
  (ipiTimeSlice AS INTEGER, ipcTimeType1 AS CHARACTER, ipcTimeType2 AS CHARACTER, iplNewJob AS LOGICAL):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
		IF NOT CAN-FIND(FIRST ttTime
		                WHERE ttTime.timeSlice EQ ipiTimeSlice) THEN DO: 
            CREATE ttTime.
            ASSIGN
                ttTime.timeSlice = ipiTimeSlice
                ttTime.timeType1 = ipcTimeType1
                ttTime.timeType2 = ipcTimeType2
                ttTime.newJob    = iplNewJob
                .
		END. /* not avail */
		RETURN TRUE.

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
