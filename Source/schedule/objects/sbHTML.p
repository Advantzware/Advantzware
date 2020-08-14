/* sbHTML.p - rstark - 8.13.2020 */

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&includes}/filterVars.i}
{{&includes}/ttblJob.i}

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

{{&includes}/htmlDefs.i resource}
DEFINE TEMP-TABLE bttTime LIKE ttTime.

DEFINE TEMP-TABLE resourceList NO-UNDO
    FIELD resource AS CHARACTER
    FIELD d-seq    LIKE mach.d-seq
    FIELD m-seq    LIKE mach.m-seq
        INDEX resourceList IS PRIMARY d-seq m-seq resource
        .

SESSION:SET-WAIT-STATE("General").
RUN pGetResources.
RUN pLoadDowntime.
RUN pFromPendingByDueDate.
RUN pHTMLPageVertical.
SESSION:SET-WAIT-STATE("").

/* **********************  Internal Procedures  *********************** */

PROCEDURE pCreatebttTime:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcKey       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtDate     AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipiTimeSlice AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcTimeType1 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTimeType2 AS CHARACTER NO-UNDO.

    CREATE bttTime.
    ASSIGN
        bttTime.timeKey   = ipcKey
        bttTime.timeDate  = ipdtDate
        bttTime.timeSlice = ipiTimeSlice
        bttTime.timeType1 = ipcTimeType1
        bttTime.timeType2 = ipcTimeType2
        .

END PROCEDURE.

PROCEDURE pCreateTtblDowntime:
    IF CAN-FIND(FIRST ttblDowntime
                WHERE ttblDowntime.dayID     EQ tempDowntime.dayID
                  AND ttblDowntime.resource  EQ tempDowntime.resource
                  AND ttblDowntime.startDate EQ tempDowntime.startDate
                  AND ttblDowntime.startTime EQ tempDowntime.startTime
                  AND ttblDowntime.endTime   EQ tempDowntime.endTime) THEN
    RETURN.
    CREATE ttblDowntime.
    BUFFER-COPY tempDowntime TO ttblDowntime.
    ttblDowntime.startDateTime = numericDateTime(ttblDowntime.startDate,ttblDowntime.startTime).
    ttblDowntime.endDateTime   = numericDateTime(ttblDowntime.startDate,ttblDowntime.endTime).
END PROCEDURE.

PROCEDURE pFromPendingByDueDate:
    DEFINE BUFFER bPendingJob FOR pendingJob.
    
    CREATE bPendingJob.
    INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/pending.dat')) NO-ECHO.
    IMPORT ^ ^.
    REPEAT:
        IMPORT bPendingJob.
        CREATE pendingJob.
        BUFFER-COPY bPendingJob TO pendingJob.
        IF pendingJob.rowIDs EQ '' THEN
        pendingJob.rowIDs = STRING(ROWID(pendingJob)).
    END. /* repeat */
    INPUT CLOSE.

    pendingDays = 365.
    FOR EACH bPendingJob
        BREAK BY bPendingJob.dueDate
              BY bPendingJob.job
              BY bPendingJob.resourceSeq
        :
        IF LAST-OF(bPendingJob.job) THEN DO:
            ASSIGN
                bPendingJob.startDate = bPendingJob.dueDate - pendingDays
                bPendingJob.startTime = 0
                .
            RUN newEnd (bPendingJob.timeSpan,bPendingJob.startDate,bPendingJob.startTime,
                        OUTPUT bPendingJob.endDate,OUTPUT bPendingJob.endTime).
            CREATE ttblJob.
            BUFFER-COPY bPendingJob TO ttblJob.
            ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
            ttblJob.endDateTime   = numericDateTime(ttblJob.endDate,ttblJob.endTime).

            RUN firstAvailable (
                ttblJob.resource,
                ttblJob.timeSpan,
                ttblJob.startDateTime,
                ttblJob.endDateTime,
                OUTPUT ttblJob.startDate,
                OUTPUT ttblJob.startTime,
                OUTPUT ttblJob.endDate,
                OUTPUT ttblJob.endTime
                ).
            RUN downtimeSpan (ttblJob.resource,ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                              OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).
            ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
            ttblJob.endDateTime   = numericDateTime(ttblJob.endDate,ttblJob.endTime).

            IF ttblJob.endDate GT TODAY + pendingLastDay THEN DO:
                DELETE ttblJob.
                NEXT.
            END. /* don't schedule, too far into the future */

            RUN pSetResourceSequence (bPendingJob.resource).
            ttblJob.sequenced = YES.

            RUN pSetDueDateJob (ROWID(bPendingJob)).

            DELETE bPendingJob.
        END. /* last-of */
    END. /* each bpendingjob */

END PROCEDURE.

PROCEDURE pGetResources:
    DEFINE VARIABLE resourceName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE resourceUse  AS CHARACTER NO-UNDO.

    INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/resourceList.dat')) NO-ECHO.
    IMPORT resourceUse.
    REPEAT:
        IMPORT resourceName.
        FIND FIRST mach NO-LOCK
             WHERE mach.company EQ ipcCompany
               AND mach.m-code  EQ resourceName
             NO-ERROR.
        IF NOT AVAILABLE mach THEN NEXT.
        CREATE resourceList.
        ASSIGN
            resourceList.resource = resourceName
            resourceList.d-seq    = mach.d-seq
            resourceList.m-seq    = mach.m-seq
            .
    END. /* repeat */
    INPUT CLOSE.
END PROCEDURE.

PROCEDURE pHTMLPageVertical:
    &Scoped-define fontFace Arial, Helvetica, sans-serif
    &Scoped-define fontFace Comic Sans MS
    &Scoped-define fontFace Tahoma
    
    DEFINE VARIABLE cDays       AS CHARACTER NO-UNDO INITIAL "Sun,Mon,Tue,Wed,Thu,Fri,Sat".
    DEFINE VARIABLE cKey        AS CHARACTER NO-UNDO.
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
    DEFINE VARIABLE iDays       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPercentage AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cHTMLPage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPageTitle  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bTtblJob FOR ttblJob.

    FIND FIRST ttblJob
         USE-INDEX startDateTimeIdx
         NO-ERROR.
    IF NOT AVAILABLE ttblJob THEN RETURN.
/*    dtStartDate = ttblJob.startDate.*/
    dtStartDate = TODAY.
    FIND LAST ttblJob
         USE-INDEX startDateTimeIdx.
    ASSIGN
        dtEndDate = ttblJob.endDate + 1
        iDays     = dtEndDate - dtStartDate + 1
        .
    FOR EACH resourceList
        :
        DO dtDate = dtStartDate TO dtEndDate:
            FOR EACH ttblDowntime
                WHERE  ttblDowntime.dayID     EQ WEEKDAY(dtDate)
                  AND (ttblDowntime.resource  EQ "<Calendar>"
                   OR  ttblDowntime.resource  EQ resourceList.resource)
                  AND (ttblDowntime.startDate EQ dtDate
                   OR  ttblDowntime.startDate EQ ?)
                :
                fTimeSlice (resourceList.resource,dtDate,ttblDowntime.startTime,"DT","Start",NO).
                fTimeSlice (resourceList.resource,dtDate,ttblDowntime.endTime,  "DT","End",  NO).
            END. /* each ttbldowntime */
        END. /* do dtdate */
        FOR EACH ttblJob
            WHERE ttblJob.resource EQ resourceList.resource
               BY ttblJob.startDateTime
            :
            DO dtDate = dtStartDate TO dtEndDate:
                iJobs = 0.
                FOR EACH bTtblJob
                    WHERE bTtblJob.resource   EQ ttblJob.resource
                      AND (bTtblJob.startDate EQ dtDate
                       OR (bTtblJob.startDate LT dtDate
                      AND  bTtbljob.endDate   GT dtDate)
                       OR  bTtblJob.endDate   EQ dtDate)
                    BY bTtblJob.startDateTime
                    :
                    ASSIGN
                        iStartTime = IF bTtblJob.startDate EQ dtDate THEN bTtblJob.startTime ELSE 0
                        iEndTime   = IF bTtblJob.endDate   EQ dtDate THEN bTtblJob.endTime   ELSE 86400
                        iJobs      = iJobs + 1
                        .
                    fTimeSlice (resourceList.resource,dtDate,iStartTime,"Job","Start",NO).
                    fTimeSlice (resourceList.resource,dtDate,iEndTime,  "Job","End",  NO).                
                END. /* each bttbljob */
                fTimeSlice (resourceList.resource,dtDate,0,    "Avail","Start",NO).
                fTimeSlice (resourceList.resource,dtDate,86400,"Avail","End",  NO).
            END. /* do dtdate */
        END. /* each ttbljob */
    END. /* each resourcelist */    

    DO jdx = 1 TO 4:
        IF jdx EQ 3 THEN
        RUN pSummarizeTimeSlices (dtStartDate, dtEndDate).
        CASE jdx:
            WHEN 1 THEN
            cPageTitle = "by Time".
            WHEN 2 THEN
            cPageTitle = "by Percentage".
            WHEN 3 THEN
            cPageTitle = "Time Summary".
            WHEN 4 THEN
            cPageTitle = "Percentage Summary".
        END CASE.
        cHTMLPage = "c:\tmp\sbHTMLCapacity." + SUBSTRING(ID,R-INDEX(ID,"/") + 1) + STRING(jdx) + ".htm".
        OUTPUT TO VALUE(cHTMLPage).
        PUT UNFORMATTED
            '<html>' SKIP
            '<head>' SKIP
            '<title>Schedule Capacity</title>' SKIP
            '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' SKIP
            '<meta http-equiv="Refresh" content="120">' SKIP
            '</head>' SKIP
            '<a name="Top"></a>' SKIP
            '<form>' SKIP
            '<fieldset>' SKIP
            '  <legend><font face="{&fontFace}"><b>Schedule Capacity ' cPageTitle '</b> (generated '
            STRING(TODAY,'99.99.9999') ' @ ' STRING(TIME,'hh:mm:ss am') ')</font>'
            '~&nbsp;</legend>' SKIP
            '  <img src="' SEARCH("Graphics/32x32/asiicon.png") '" align="middle">~&nbsp;'
            '<b><a href="http://www.advantzware.com" target="_blank">'
            '<font face="{&fontFace}">Advantzware, Inc.</a>~&nbsp;~&copy;</b></font>' SKIP
            '</font></b>' SKIP 
            '  <table align="center" cellspacing="2" cellpadding="8">' SKIP
            '    <tr>' SKIP 
            '      <td><font face="{&fontFace}">Legend:</font></td>' SKIP 
            '      <td bgcolor="#FF8585"><font face="{&fontFace}"><b>Downtime</b></font></td>' SKIP 
            '      <td bgcolor="#A1A5E2"><font face="{&fontFace}"><b>Booked Job</b></font></td>' SKIP
            '      <td bgcolor="#97F3A0"><font face="{&fontFace}"><b>Available</b></font></td>' SKIP 
            '    </tr>' SKIP  
            '  </table>' SKIP 
            '  <table border="1" cellspacing="0" cellpadding="0" width="100%" height="80%" style="border-color: white">' SKIP
            .
        RUN pOutputResources.
        DO dtDate = dtStartDate TO dtEndDate:
            PUT UNFORMATTED
                '    <tr style="height: ' INTEGER(90 / iDays) '%;">' SKIP
                '      <td bgcolor="#576490" align="center" nowrap><font face="{&fontFace}" color="#FFFFFF"><b>'
                ENTRY(WEEKDAY(dtDate),cDays) ' ' MONTH(dtDate) '/' DAY(dtDate) '/' YEAR(dtDate) '</b></font></td>' SKIP
                .
            FOR EACH resourceList
                :
                PUT UNFORMATTED
                    '      <td style="padding: 5px">' SKIP
                    '        <table border="1" cellspacing="0" cellpadding="0" align="center" width="70%" height="140px">' SKIP
                    .
                IF NOT CAN-FIND(FIRST ttTime
                                WHERE ttTime.timeKey  EQ resourceList.resource
                                  AND ttTime.timedate EQ dtDate) THEN DO:
                    fTimeSlice (resourceList.resource,dtDate,0,     "Avail","Start",NO).
                    fTimeSlice (resourceList.resource,dtDate,86400, "Avail","End",  NO).                
                END. /* if not can-find */
                FOR EACH ttTime
                    WHERE ttTime.timeKey  EQ resourceList.resource
                      AND ttTime.timeDate EQ dtDate
                       BY ttTime.timeSlice DESCENDING
                       BY ttTime.timeType2 DESCENDING
                    :
                    IF ttTime.timeSlice EQ 86400 THEN DO:
                        ASSIGN
                            iTime  = 86400
                            cType1 = ttTime.timeType1
                            cType2 = ttTime.timeType2                        
                            .
                        NEXT.
                    END. /* timeslice eq 86400 */
    
                    IF ttTime.timeType1 EQ "DT"   AND 
                       ttTime.timeType1 NE cType1 AND
                       ttTime.timeType2 NE cType2 THEN
                    cType1 = "Avail".
                    ELSE IF ttTime.timeType1 EQ "DT"    AND
                            ttTime.timeType2 EQ "End"   AND
                            cType2           EQ "Start" THEN
                    cType1 = "Avail".
                    ELSE IF ttTime.timeType1 NE cType1 AND
                            ttTime.timeType2 NE cType2 THEN
                    cType1 = "Avail".
                    ELSE IF ttTime.timeType2 NE "End" OR cType2 NE "End" THEN
                    cType1 = ttTime.timeType1.
    
                    iPercentage = ROUND((iTime - ttTime.timeSlice) / 86400 * 100,0).
                    IF iPercentage GT 0 THEN DO:
                        PUT UNFORMATTED
                            '          <tr style="height: ' iPercentage '%;">' SKIP
                            '            <td bgcolor="#'
                            (IF ttTime.newJob AND ttTime.timeType2 EQ "Start" THEN "85FEFE" ELSE
                             IF cType1 EQ "Avail" THEN "97F3A0" ELSE
                             IF cType1 EQ "Job"   THEN "A1A5E2" ELSE "FF8585")
                            '" align="center" nowrap><font face="{&fontFace}"><b>'
                            .
                        IF jdx EQ 1 OR jdx EQ 3 THEN
                            IF iPercentage EQ 100 THEN
                            PUT UNFORMATTED "24:00:00".
                            ELSE
                            PUT UNFORMATTED STRING(iTime - ttTime.timeSlice,"hh:mm:ss").
                        ELSE
                        PUT UNFORMATTED iPercentage '%'.
                        PUT UNFORMATTED 
                            '</b></font>'
                            '</td>' SKIP
                            '          </tr>' SKIP
                            .
                    END. /* if gt 0 */
                    ASSIGN
                        iTime  = ttTime.timeSlice
                        cType1 = ttTime.timeType1
                        cType2 = ttTime.timeType2
                        .
                END. /* each tttime */
                PUT UNFORMATTED
                    '        </table>' SKIP 
                    '      </td>' SKIP
                    .
            END. /* each resourcelist */
            PUT UNFORMATTED
                '      <td bgcolor="#576490" align="center" nowrap><font face="{&fontFace}" color="#FFFFFF"><b>'
                ENTRY(WEEKDAY(dtDate),cDays) ' ' MONTH(dtDate) '/' DAY(dtDate) '/' YEAR(dtDate) '</b></font></td>' SKIP
                '    </tr>' SKIP
                .
        END. /* do dtdate */
        RUN pOutputResources.
        PUT UNFORMATTED
            '  </table>' SKIP
            '  <div align="left"><font face="{&fontFace}"><a href="#Top">Top</a></font>' SKIP
            '  <div align="right"><font face="{&fontFace}">~&copy; Advantzware, Inc., All Rights Reserved</font></div>' SKIP
            '  <table align="center" cellspacing="2" cellpadding="8">' SKIP
            '    <tr>' SKIP 
            '      <td><font face="{&fontFace}">Legend:</font></td>' SKIP 
            '      <td bgcolor="#FF8585"><font face="{&fontFace}"><b>Downtime</b></font></td>' SKIP 
            '      <td bgcolor="#A1A5E2"><font face="{&fontFace}"><b>Booked Job</b></font></td>' SKIP
            '      <td bgcolor="#97F3A0"><font face="{&fontFace}"><b>Available</b></font></td>' SKIP 
            '    </tr>' SKIP  
            '  </table>' SKIP 
            '</fieldset>' SKIP
            '</form>' SKIP
            '</html>' SKIP
            .
        OUTPUT CLOSE.
        OS-COMMAND NO-WAIT START VALUE(cHTMLPage).
    END. /* do jdx */
    
/*    OUTPUT TO "c:\tmp\sbHTML1.txt".                     */
/*    FOR EACH ttTime                                     */
/*    BREAK BY ttTime.timeKey                             */
/*          BY ttTime.timeDate  DESCENDING                */
/*          BY ttTime.timeSlice DESCENDING                */
/*          BY ttTime.timeType2 DESCENDING                */
/*        :                                               */
/*        EXPORT                                          */
/*            ttTime.timeKey                              */
/*            ttTime.timeDate                             */
/*            ttTime.timeSlice                            */
/*            STRING(ttTime.timeSlice,"hh:mm:ss am")      */
/*            ttTime.timeType1                            */
/*            ttTime.timeType2                            */
/*            ttTime.newJob                               */
/*            .                                           */
/*        IF LAST-OF(ttTime.timeDate) THEN                */
/*        PUT UNFORMATTED SKIP(1).                        */
/*    END.                                                */
/*    OUTPUT CLOSE.                                       */
/*    OS-COMMAND NO-WAIT notepad.exe "c:\tmp\sbHTML1.txt".*/

/*    OUTPUT TO c:\tmp\ttblDowntime.txt.                     */
/*    FOR EACH ttblDowntime:                                 */
/*        EXPORT ttblDowntime.                               */
/*    END.                                                   */
/*    OUTPUT CLOSE.                                          */
/*    OS-COMMAND NO-WAIT notepad.exe c:\tmp\ttblDowntime.txt.*/

END PROCEDURE.

PROCEDURE pLoadDowntime:
    EMPTY TEMP-TABLE ttblDowntime.

    INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/downtimes.Actual.dat')) NO-ECHO.
    REPEAT:
        IMPORT tempDowntime.
        tempDowntime.dayID = tempDowntime.dayID MODULO 7.
        IF tempDowntime.dayID EQ 0 THEN
        tempDowntime.dayID = 7.
        IF tempDowntime.resource EQ "<Calendar>" THEN DO:
            FOR EACH resourceList
                :
                tempDowntime.resource = resourceList.resource.
                RUN pCreateTtblDowntime.
            END. /* each ttbljob */
        END.
        ELSE
        IF CAN-FIND(FIRST resourceList
                    WHERE resourceList.resource EQ tempDowntime.resource) THEN
        RUN pCreateTtblDowntime.
    END. /* repeat */
    INPUT CLOSE.

END PROCEDURE.

PROCEDURE pOutputResources:
    PUT UNFORMATTED
        '    <tr>' SKIP
        '      <td bgcolor="#576490" align="center" nowrap><font face="{&fontFace}" color="#FFFFFF"><b>'
        'Operation</b></font></td>' SKIP
        .
    FOR EACH resourceList
        :
        PUT UNFORMATTED
            '      <td bgcolor="#576490" align="left" nowrap><font face="{&fontFace}" color="#FFFFFF">'
            '<img src="'
            (IF SEARCH("Graphics/48x48/" + resourceList.resource + ".png") NE ? THEN
                SEARCH("Graphics/48x48/" + resourceList.resource + ".png") ELSE
                SEARCH("Graphics/48x48/gearwheels.png"))
            '" width="48" height="48" valign="middle">~&nbsp<b>'
            resourceList.resource '</b></font></td>' SKIP
            .
    END. /* each ttbljob */
    PUT UNFORMATTED
        '      <td bgcolor="#576490" align="center" nowrap><font face="{&fontFace}" color="#FFFFFF"><b>'
        'Operation</b></font></td>' SKIP
        '    </tr>' SKIP
        .

END PROCEDURE.

PROCEDURE pSetDueDateJob :
    DEFINE INPUT PARAMETER iprRowID AS ROWID NO-UNDO.

    DEFINE VARIABLE priorStartDate AS DATE      NO-UNDO.
    DEFINE VARIABLE priorStartTime AS INTEGER   NO-UNDO.
    DEFINE VARIABLE priorDateTime  AS INTEGER   NO-UNDO INITIAL ?.
    DEFINE VARIABLE dDueDate       AS DATE      NO-UNDO.
    DEFINE VARIABLE cJob           AS CHARACTER NO-UNDO.

    DEFINE BUFFER bufPendingJob FOR pendingJob.

    ASSIGN
      dDueDate       = ttblJob.dueDate
      cJob           = ttblJob.job
      priorStartDate = ttblJob.startDate
      priorStartTime = ttblJob.startTime
      priorDateTime  = ttblJob.startDateTime
      .
    FOR EACH bufPendingJob
        WHERE bufPendingJob.dueDate EQ dDueDate
          AND bufPendingJob.job     EQ cJob
          AND ROWID(bufPendingJob)  NE iprRowID
        BREAK BY bufPendingJob.job
              BY bufPendingJob.resourceSeq DESCENDING
        :
        ASSIGN
          bufPendingJob.endDate = priorStartDate
          bufPendingJob.endTime = priorStartTime
          .
        RUN newStart (bufPendingJob.timeSpan,bufPendingJob.endDate,bufPendingJob.endTime,
                      OUTPUT bufPendingJob.startDate,OUTPUT bufPendingJob.startTime).
        bufPendingJob.startDateTime = numericDateTime(bufPendingJob.startDate,bufPendingJob.startTime).
        bufPendingJob.endDateTime   = numericDateTime(bufPendingJob.endDate,bufPendingJob.endTime).

        CREATE ttblJob.
        BUFFER-COPY bufPendingJob TO ttblJob.
        ASSIGN
          ttblJob.origStartDate = bufPendingJob.startDate
          ttblJob.origStartTime = bufPendingJob.startTime
          ttblJob.origEndDate   = bufPendingJob.endDate
          ttblJob.origEndTime   = bufPendingJob.endTime
          .
        RUN downtimeSpan (ttblJob.resource,ttblJob.timeSpan,ttblJob.endDate,ttblJob.endTime,
                          OUTPUT ttblJob.startDate,OUTPUT ttblJob.startTime).
        ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
        ttblJob.endDateTime   = numericDateTime(ttblJob.endDate,ttblJob.endTime).
        
        ASSIGN
          priorStartDate = ttblJob.startDate
          priorStartTime = ttblJob.startTime
          priorDateTime  = ttblJob.startDateTime
          .
        RUN pSetResourceSequence (bufPendingJob.resource).
        ttblJob.sequenced = YES.

        DELETE bufPendingJob.
    END. /* each bufpendingjob */
    
END PROCEDURE.

PROCEDURE pSetResourceSequence :
    DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.

    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    FOR EACH buffJob
        WHERE buffJob.resource EQ ipResource
           BY buffJob.startDate
           BY buffJob.startTime
        :
        ASSIGN
            idx                 = idx + 1
            buffJob.jobSequence = idx
            .
    END. /* each buffJob */

END PROCEDURE.

PROCEDURE pSummarizeTimeSlices:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdtStartDate AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipdtEndDate   AS DATE NO-UNDO.

    DEFINE VARIABLE dtDate     AS DATE    NO-UNDO.
    DEFINE VARIABLE iAvail     AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBooked    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iDowntime  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTimeSlice AS INTEGER NO-UNDO.

    DO dtDate = ipdtStartDate TO ipdtEndDate:
        FOR EACH ttTime
            WHERE ttTime.timeDate EQ dtDate
            BREAK BY ttTime.timeKey
                  BY ttTime.timeDate
                  BY ttTime.timeSlice DESCENDING
                  BY ttTime.timeType2 DESCENDING
            :
            IF ttTime.timeType2 EQ "End" THEN
            iTimeSlice = ttTime.timeSlice.
            ELSE
            CASE ttTime.timeType1:
                WHEN "DT" THEN
                iDowntime = iDowntime + iTimeSlice - ttTime.timeSlice.
                WHEN "Job" THEN
                iBooked = iBooked + iTimeSlice - ttTime.timeSlice.
            END CASE.
            IF LAST-OF(ttTime.timeKey) THEN DO:
                IF iDowntime NE 0 THEN DO:
                    RUN pCreatebttTime (ttTime.timeKey,dtDate,0,"DT","Start").
                    RUN pCreatebttTime (ttTime.timeKey,dtDate,iDowntime,"DT","End").
                END. /* if idowntime */
                IF iBooked NE 0 THEN DO:
                    IF iDowntime LT 86400 THEN DO:
                        RUN pCreatebttTime (ttTime.timeKey,dtDate,iDowntime,"Job","Start").
                        iTimeSlice = iDowntime + iBooked.
                        IF iTimeSlice GT 86400 THEN
                        iTimeSlice = 86400.
                        RUN pCreatebttTime (ttTime.timeKey,dtDate,iTimeSlice,"Job","End").
                    END. /* if idowntime */
                END. /* if ibooked */
                iTimeSlice = iDowntime + iBooked.
                IF iTimeSlice LT 86400 THEN DO:
                    RUN pCreatebttTime (ttTime.timeKey,dtDate,iTimeSlice,"Avail","Start").
                    RUN pCreatebttTime (ttTime.timeKey,dtDate,86400,"Avail","End").
                END. /* if */
                ASSIGN
                    iAvail    = 0
                    iBooked   = 0
                    iDowntime = 0
                    .
            END. /* if last-of */
        END. /* each tttime */
    END. /* do dtdate */

/*    OUTPUT TO "c:\tmp\sbHTML1.txt".                     */
/*    FOR EACH ttTime                                     */
/*    BREAK BY ttTime.timeKey                             */
/*          BY ttTime.timeDate                            */
/*          BY ttTime.timeSlice                           */
/*          BY ttTime.timeType2 DESCENDING                */
/*        :                                               */
/*        EXPORT                                          */
/*            ttTime.timeKey                              */
/*            ttTime.timeDate                             */
/*            ttTime.timeSlice                            */
/*            STRING(ttTime.timeSlice,"hh:mm:ss am")      */
/*            ttTime.timeType1                            */
/*            ttTime.timeType2                            */
/*            ttTime.newJob                               */
/*            .                                           */
/*        IF LAST-OF(ttTime.timeDate) THEN                */
/*        PUT UNFORMATTED SKIP(1).                        */
/*    END.                                                */
/*    OUTPUT CLOSE.                                       */
/*    OS-COMMAND NO-WAIT notepad.exe "c:\tmp\sbHTML1.txt".*/
    
    EMPTY TEMP-TABLE ttTime.
    FOR EACH bttTime:
        CREATE ttTime.
        BUFFER-COPY bttTime TO ttTime.
    END. /* each btttime */
    
/*    OUTPUT TO "c:\tmp\sbHTML2.txt".                     */
/*    FOR EACH bttTime                                    */
/*    BREAK BY bttTime.timeKey                            */
/*          BY bttTime.timeDate                           */
/*          BY bttTime.timeSlice                          */
/*        :                                               */
/*        EXPORT                                          */
/*            bttTime.timeKey                             */
/*            bttTime.timeDate                            */
/*            bttTime.timeSlice                           */
/*            STRING(bttTime.timeSlice,"hh:mm:ss am")     */
/*            bttTime.timeType1                           */
/*            bttTime.timeType2                           */
/*            bttTime.newJob                              */
/*            .                                           */
/*        IF LAST-OF(bttTime.timeDate) THEN               */
/*        PUT UNFORMATTED SKIP(1).                        */
/*    END.                                                */
/*    OUTPUT CLOSE.                                       */
/*    OS-COMMAND NO-WAIT notepad.exe "c:\tmp\sbHTML2.txt".*/

END PROCEDURE.
