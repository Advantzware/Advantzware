/*------------------------------------------------------------------------
    File        : taskerProcs.i
    Purpose     : Common include used by AOA/tasker.w and AOA/taskMonitor.p

    Syntax      : {AOA/includes/taskerProcs.i}

    Description : Tasker Monitor Functions and Procedures

    Author(s)   : Ron Stark
    Created     : Tue Feb 09 16:58:41 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pHTMLFooter :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    PUT UNFORMATTED
        '  </table>' SKIP
        '</fieldset>' SKIP
        '</form>' SKIP
        '<div align="left"><font face="{&fontFace}"><a href="#Top">Top</a></font>' SKIP
        '<div align="right"><font face="{&fontFace}">~&copy; Advantzware, Inc., All Rights Reserved</font></div>' SKIP
        '</html>' SKIP
        .

END PROCEDURE.

PROCEDURE pHTMLHeader :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFolder AS CHARACTER NO-UNDO.

    PUT UNFORMATTED
        '<html>' SKIP
        '<head>' SKIP
        '<title>Scheduled Tasks for ' ipcUserID '</title>' SKIP
        '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' SKIP
        '<meta http-equiv="Refresh" content="60">' SKIP
        '<link rel="shortcut icon" href="' SEARCH("Graphics/32x32/asiicon.png") '">' SKIP
        '</head>' SKIP
        '<a name="Top"></a>' SKIP
        '<form>' SKIP
        '<fieldset>' SKIP
        '  <legend><font face="{&fontFace}"><b>Scheduled Tasks for ' ipcUserID '</b> (generated '
        STRING(TODAY,'99.99.9999') ' @ ' STRING(TIME,'hh:mm:ss am') ')</font>'
        '~&nbsp;</legend>' SKIP
        '  <img src="' SEARCH("Graphics/32x32/asiicon.png") '" align="middle">~&nbsp;'
        '<b><a href="http://www.advantzware.com" target="_blank">'
        '<font face="{&fontFace}">Advantzware, Inc.</a>~&nbsp;~&copy;</b></font>' SKIP
        '</font></b>' SKIP 
        '~&nbsp;~&nbsp;~&nbsp;~&nbsp;~&nbsp;~&nbsp;<b>User:</b>' SKIP  
        '  <select onchange="window.location=this.options[this.selectedIndex].value">' SKIP
        '    <option value="' ipcFolder '\tasker-ALL.htm"'
        (IF ipcUserID EQ "ALL" THEN ' selected' ELSE '')
        '>ALL</option>' SKIP
        .

END PROCEDURE.

PROCEDURE pHTMLResults:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cBGColor AS CHARACTER NO-UNDO INIT 'bgcolor="#DBDEF2"'.
    DEFINE VARIABLE cFile    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx      AS INTEGER   NO-UNDO.

    PUT UNFORMATTED
        '  </table>' SKIP
        '</fieldset>' SKIP
        '</form>' SKIP
        '<div align="left"><font face="{&fontFace}"><a href="#Top">Top</a></font>' SKIP
        '<a name="Results"></a>' SKIP
        '<form>' SKIP
        '<fieldset>' SKIP
        '  <legend><font face="{&fontFace}"><b>Task Results for ' ipcUserID '</b></font>'
        '~&nbsp;</legend>' SKIP
        '  <table border="1" cellspacing="2" cellpadding="2" width="100%" style="border-color: white">' SKIP
        '    <tr>' SKIP
        '      <td ' cBGColor '><b><u>Date Time</u></b></td>' SKIP
        '      <td ' cBGColor '><b><u>Type</u></b></td>' SKIP
        '      <td ' cBGColor '><b><u>User ID</u></b></td>' SKIP
        '      <td ' cBGColor '><b><u>Viewed</u></b></td>' SKIP
        '      <td ' cBGColor '><b><u>Archived</u></b></td>' SKIP
        '      <td ' cBGColor '><b><u>File Name</u></b></td>' SKIP
        '    </tr>' SKIP
        .
    FOR EACH taskResult NO-LOCK
        WHERE taskResult.user-id EQ ipcUserID
           OR ipcUserID EQ "ALL"
        :
        ASSIGN
            idx      = idx + 1
            cBGColor = IF idx MOD 2 EQ 0 THEN 'bgcolor="#DBDEF2"' ELSE 'bgcolor="#F4F4F4"'
            FILE-INFO:FILE-NAME = SEARCH(taskResult.folderFile)
            cFile    = FILE-INFO:FULL-PATHNAME
            .
        PUT UNFORMATTED
            '    <tr>' SKIP
            '      <td ' cBGColor '> ' taskResult.fileDateTime '</td>' SKIP
            '      <td ' cBGColor '> ' taskResult.fileType '</td>' SKIP
            '      <td ' cBGColor '> ' taskResult.user-id '</td>' SKIP
            '      <td ' cBGColor '> ' taskResult.viewed '</td>' SKIP
            '      <td ' cBGColor '> ' taskResult.archived '</td>' SKIP
            '      <td ' cBGColor '> ' taskResult.folderFile '</td>' SKIP
/*            '      <td ' cBGColor '><a href="' cFile '">' taskResult.folderFile '</a></td>' SKIP*/
            '    </tr>' SKIP
            .
    END. /* each taskresult */

END PROCEDURE.
    
PROCEDURE pHTMLTask:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cBGColor AS CHARACTER NO-UNDO INIT 'bgcolor="#DBDEF2"'.
    DEFINE VARIABLE idx      AS INTEGER   NO-UNDO.

    DEFINE BUFFER bTask FOR Task.

    PUT UNFORMATTED
        '  </select>' SKIP
        '  <div align="right"><font face="{&fontFace}"><a href="#Results">Task Results</a></font>' SKIP
        '  <table border="1" cellspacing="2" cellpadding="2" width="100%" style="border-color: white">' SKIP
        '    <tr>' SKIP
        '      <td ' cBGColor '><b><u>Run Now</u></b></td>' SKIP
        '      <td ' cBGColor '><b><u>Task Name</u></b></td>' SKIP
        '      <td ' cBGColor '><b><u>Type</u></b></td>' SKIP
        '      <td ' cBGColor '><b><u>Next Run Date</u></b></td>' SKIP
        '      <td ' cBGColor '><b><u>Next Run Time</u></b></td>' SKIP
        '      <td ' cBGColor '><b><u>Last Run Date</u></b></td>' SKIP
        '      <td ' cBGColor '><b><u>Last Run Time</u></b></td>' SKIP
        '      <td ' cBGColor '><b><u>Is Running</u></b></td>' SKIP
        '      <td ' cBGColor '><b><u>Task ID</u></b></td>' SKIP
        '      <td ' cBGColor '><b><u>Prgm Name</u></b></td>' SKIP
        '      <td ' cBGColor '><b><u>User ID</u></b></td>' SKIP
        '      <td ' cBGColor '><b><u>Run Syncronous</u></b></td>' SKIP
        '    </tr>' SKIP
        .
    FOR EACH bTask NO-LOCK
        WHERE (bTask.scheduled EQ YES
           OR  bTask.runNow    EQ YES)
          AND (bTask.user-id   EQ ipcUserID
           OR  ipcUserID       EQ "ALL")        
        BY bTask.runNow DESCENDING
        BY bTask.nextDate 
        BY bTask.nextTime
        :
        ASSIGN
            idx      = idx + 1
            cBGColor = IF idx MOD 2 EQ 0 THEN 'bgcolor="#DBDEF2"' ELSE 'bgcolor="#F4F4F4"'
            .
        PUT UNFORMATTED
            '    <tr>' SKIP
            '      <td ' cBGColor '> ' bTask.runNow '</td>' SKIP
            '      <td ' cBGColor '> ' bTask.taskName '</td>' SKIP
            '      <td ' cBGColor '> ' bTask.taskFormat '</td>' SKIP
            '      <td ' cBGColor '> ' STRING(bTask.nextDate,"99/99/9999") '</td>' SKIP
            '      <td ' cBGColor '> ' STRING(bTask.nextTime,"hh:mm:ss am") '</td>' SKIP
            '      <td ' cBGColor '> ' STRING(bTask.lastDate,"99/99/9999") '</td>' SKIP
            '      <td ' cBGColor '> ' STRING(bTask.lastTime,"hh:mm:ss am") '</td>' SKIP
            '      <td ' cBGColor '> ' bTask.isRunning '</td>' SKIP
            '      <td ' cBGColor '> ' bTask.taskID '</td>' SKIP
            '      <td ' cBGColor '> ' bTask.prgmName '</td>' SKIP
            '      <td ' cBGColor '> ' bTask.user-id '</td>' SKIP
            '      <td ' cBGColor '> ' bTask.runSync '</td>' SKIP
            '    </tr>' SKIP
            .
    END. /* each bTask */

END PROCEDURE.
    
PROCEDURE pHTMLTasks :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bTask       FOR Task.
    DEFINE BUFFER bTaskResult FOR TaskResult.

    FIND FIRST config NO-LOCK.
    IF config.taskerHTMLFolder EQ "" THEN RETURN.
    OUTPUT TO VALUE(config.taskerHTMLFolder + "\tasker-ALL.htm").
    RUN pHTMLHeader  ("ALL", config.taskerHTMLFolder).
    RUN pHTMLUserID  ("ALL", config.taskerHTMLFolder).
    RUN pHTMLTask    ("ALL").
    RUN pHTMLResults ("ALL").
    RUN pHTMLFooter.
    OUTPUT CLOSE.
    FOR EACH bTask NO-LOCK
        WHERE bTask.scheduled EQ YES
           OR bTask.runNow    EQ YES
        BREAK BY bTask.user-id
        :
        IF FIRST-OF(bTask.user-id) THEN DO:
            OUTPUT TO VALUE(config.taskerHTMLFolder + "\tasker-" + bTask.user-id + ".htm").
            RUN pHTMLHeader  (bTask.user-id, config.taskerHTMLFolder).
            RUN pHTMLUserID  (bTask.user-id, config.taskerHTMLFolder).
            RUN pHTMLTask    (bTask.user-id).
            RUN pHTMLResults (bTask.user-id).
            RUN pHTMLFooter.
            OUTPUT CLOSE.
        END. /* if first-of */
    END. /* each bTask */
    FOR EACH bTaskResult NO-LOCK
        BREAK BY bTaskResult.user-id
        :
        IF FIRST-OF(bTaskResult.user-id) THEN DO:
            IF CAN-FIND(FIRST bTask
                        WHERE bTask.user-id EQ bTaskResult.user-id
                          AND (bTask.scheduled EQ YES
                           OR  bTask.runNow    EQ YES)) THEN
            NEXT.
            OUTPUT TO VALUE(config.taskerHTMLFolder + "\tasker-" + bTaskResult.user-id + ".htm").
            RUN pHTMLHeader  (bTaskResult.user-id, config.taskerHTMLFolder).
            RUN pHTMLUserID  (bTaskResult.user-id, config.taskerHTMLFolder).
            RUN pHTMLTask    (bTaskResult.user-id).
            RUN pHTMLResults (bTaskResult.user-id).
            RUN pHTMLFooter.
            OUTPUT CLOSE.
        END. /* if first-of */
    END. /* each taskresult */

END PROCEDURE.

PROCEDURE pHTMLUserID:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFolder AS CHARACTER NO-UNDO.

    DEFINE BUFFER bTask       FOR Task.
    DEFINE BUFFER bTaskResult FOR TaskResult.

    FOR EACH users NO-LOCK
        :
        IF CAN-FIND(FIRST bTask
                    WHERE bTask.user-id EQ users.user_id
                      AND (bTask.scheduled EQ YES
                       OR  bTask.runNow    EQ YES)) OR
           CAN-FIND(FIRST bTaskResult
                    WHERE bTaskResult.user-id EQ users.user_id) THEN
        PUT UNFORMATTED
            '    <option value="' ipcFolder '\tasker-' users.user_id '.htm"'
            (IF users.user_id EQ ipcUserID THEN ' selected' ELSE '')
            '>' users.user_id '</option>' SKIP
            .
    END. /* each users */

END PROCEDURE.

PROCEDURE pLastExecuted:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCompany          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTaskerNotRunning AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iConfigID         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lTaskerNotRunning AS LOGICAL   NO-UNDO.

    DEFINE BUFFER emailConfig FOR emailConfig.

    IF DATE(dttOpenDateTime) NE DATE(NOW) THEN
    dttOpenDateTime = NOW.
    RUN spGetSessionParam ("Company", OUTPUT cCompany).
    RUN sys/ref/nk1look.p (
        cCompany,"TaskerNotRunning","I",NO,NO,"","",
        OUTPUT cTaskerNotRunning,OUTPUT lTaskerNotRunning
        ).
    iConfigID = INTEGER(cTaskerNotRunning).
    DO TRANSACTION:
        FIND FIRST config EXCLUSIVE-LOCK.
        config.taskerLastExecuted = NOW.
        FIND FIRST config NO-LOCK.
        IF CAN-FIND(FIRST emailConfig
                    WHERE emailConfig.configID EQ iConfigID
                      AND emailConfig.isActive EQ YES
                      AND emailConfig.notified EQ YES) THEN DO:
            FIND FIRST emailConfig EXCLUSIVE-LOCK
                 WHERE emailConfig.configID EQ iConfigID
                 NO-ERROR.
            IF AVAILABLE emailConfig THEN
            emailConfig.notified = NO.
        END. /* if can-find */
        RELEASE emailConfig.
    END. /* do trans */
    RUN pHTMLTasks.

END PROCEDURE.

PROCEDURE pRunCommand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcRun AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cDLC      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEXE      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParam    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPassword AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lSkip     AS LOGICAL   NO-UNDO.
    
    GET-KEY-VALUE SECTION 'STARTUP'
        KEY 'DLC'
        VALUE cDLC.
    cEXE = cDLC + "\bin\prowin"
         + REPLACE(STRING(PROCESS-ARCHITECTURE),"64","")
         + ".exe "
         .
    DO idx = 1 TO NUM-ENTRIES(SESSION:STARTUP-PARAMETERS):
        cParam = ENTRY(idx,SESSION:STARTUP-PARAMETERS).
        IF cParam BEGINS "-p " THEN NEXT.
        IF cParam BEGINS "-debugalert" THEN NEXT.
        IF cParam BEGINS "-param " THEN LEAVE.
        IF lSkip EQ NO THEN DO:
            IF ENTRY(1,cParam," ") EQ "-ininame" OR
               ENTRY(2,cParam," ") EQ "advantzware.pf" THEN
            ASSIGN
                FILE-INFO:FILE-NAME = SEARCH(ENTRY(2,cParam," "))
                ENTRY(2,cParam," ") = FILE-INFO:FULL-PATHNAME
                .
            opcRun = opcRun + cParam + " ".
        END. /* if lskip */
        IF cParam BEGINS "-pf" THEN lSkip = YES.
        IF cParam BEGINS "(end .pf)" THEN lSkip = NO.
    END. /* do idx */
    IF USERID("ASI") NE "NoSweat" THEN
    DO idx = 1 TO NUM-DBS:
        opcRun = opcRun + REPLACE(DBPARAM(idx),","," ") + " ".
    END. /* do idx */
    RUN spGetSessionParam ("Password", OUTPUT cPassword).
    IF cPassword NE "" THEN
    cPassword = " -U " + USERID("ASI") + " -P " + cPassword.
    ASSIGN
        opcRun = REPLACE(opcRun,"-U " + USERID("ASI") + " -P","")
        opcRun = cEXE
               + REPLACE(opcRun,"-ld ASI","-ld ASI " + cPassword)
               + " -p &1 -param &2"
               + " -debugalert"
               .

END PROCEDURE.

PROCEDURE pTaskEmails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cRunProgram AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRefresh    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE rRowID      AS ROWID     NO-UNDO.
    
    DEFINE BUFFER bTaskEmail   FOR TaskEmail.
    DEFINE BUFFER bCueCardText FOR cueCardText.

    FIND FIRST config NO-LOCK.
    {&OPEN-QUERY-EmailBrowse}
    FOR EACH bTaskEmail:
        IF bTaskEmail.mustExist EQ NO OR
           SEARCH(bTaskEmail.attachment) NE ? THEN DO:
            IF bTaskEmail.recipients EQ "Cue Card Message" THEN DO:
                IF AVAILABLE config AND config.cueCard THEN DO:
                    FIND LAST cueCardText NO-LOCK
                         WHERE cueCardText.cueID     EQ 0
                           AND cueCardText.cueTextID EQ 0
                           AND cueCardText.cueType   EQ "Message"
                         NO-ERROR.
                    IF AVAILABLE cueCardText THEN DO:
                        CREATE bCueCardText.
                        BUFFER-COPY cueCardText EXCEPT rec_key TO bCueCardText
                            ASSIGN
                                bCueCardText.cueText     = "Submitted Run Now Request is Available"
                                                         + CHR(10) + CHR(10) + "File: "
                                                         + bTaskEmail.attachment
                                bCueCardText.isActive    = YES
                                bCueCardText.cueOrder    = cueCardText.cueOrder + 1
                                bCueCardText.createdDate = TODAY
                                bCueCardText.createdTime = TIME
                                bCueCardText.createdFor  = bTaskEmail.user-id
                                .
                    END. /* if avail cuecardtext */
                END. /* if avail config and cuecard */
            END. /* if cue card message */
            ELSE DO:
                ASSIGN
                    FILE-INFO:FILE-NAME = "AOA\TaskEmail.r" 
                    cRunProgram = FILE-INFO:FULL-PATHNAME
                    .
                IF cRunProgram EQ ? THEN
                ASSIGN
                    FILE-INFO:FILE-NAME = "AOA\TaskEmail.p" 
                    cRunProgram = FILE-INFO:FULL-PATHNAME
                    .
                RUN VALUE(cRunProgram) (
                    bTaskEmail.subject,
                    bTaskEmail.body,
                    bTaskEmail.attachment,
                    bTaskEmail.recipients,
                    bTaskEmail.rec_key
                    ).
/*                OS-COMMAND NO-WAIT VALUE(            */
/*                    SUBSTITUTE(                      */
/*                        cRun,                        */
/*                        cRunProgram,           "~"" +*/
/*                        PROPATH               + "+" +*/
/*                        bTaskEmail.subject    + "+" +*/
/*                        bTaskEmail.body       + "+" +*/
/*                        bTaskEmail.attachment + "+" +*/
/*                        bTaskEmail.recipients + "+" +*/
/*                        bTaskEmail.rec_key    + "~"" */
/*                        )                            */
/*                    ).                               */
            END. /* else */
            DELETE bTaskEmail.
            lRefresh = YES.
        END. /* if search */
    END. /* each bTaskEmail */
    IF lRefresh THEN
    {&OPEN-QUERY-EmailBrowse}

END PROCEDURE.

PROCEDURE pTasks :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cRunProgram AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rRowID      AS ROWID     NO-UNDO.

    DEFINE BUFFER bTask FOR Task.

    {&OPEN-QUERY-TaskBrowse}
    GET FIRST TaskBrowse.
    DO WHILE AVAILABLE Task:
        ASSIGN
            dttDateTime = DATETIME(Task.nextDate, Task.nextTime * 1000)
            rRowID      = ROWID(Task)
            .
        IF Task.isRunning EQ NO AND
          (Task.runNow    EQ YES OR
         ((Task.startDate EQ ?   OR Task.startDate LE TODAY) AND
          (Task.endDate   EQ ?   OR Task.endDate   GE TODAY) AND
           dttDateTime    LE NOW)) THEN DO:
            &IF "{&program-id}" NE "taskMonitor." &THEN
            REPOSITION TaskBrowse TO ROWID rRowID.
            &ENDIF
            CASE Task.taskType:
                WHEN "Jasper" THEN
                FILE-INFO:FILE-NAME = IF Task.runSync THEN "AOA\runSync.r"  ELSE "AOA\runASync.r".
                OTHERWISE /* Data PA */
                FILE-INFO:FILE-NAME = IF Task.runSync THEN "AOA\runSyncU.r" ELSE "AOA\runASyncU.r".
            END CASE.
            cRunProgram = FILE-INFO:FULL-PATHNAME.
            IF cRunProgram EQ ? THEN
            CASE Task.taskType:
                WHEN "Jasper" THEN
                FILE-INFO:FILE-NAME = IF Task.runSync THEN "AOA\runSync.p"  ELSE "AOA\runASync.p".
                OTHERWISE /* Data PA */
                FILE-INFO:FILE-NAME = IF Task.runSync THEN "AOA\runSyncU.p" ELSE "AOA\runASyncU.p".
            END CASE.
            cRunProgram = FILE-INFO:FULL-PATHNAME.
            IF cRunProgram NE ? THEN DO:
                DO TRANSACTION:
                    FIND FIRST bTask EXCLUSIVE-LOCK
                         WHERE ROWID(bTask) EQ ROWID(Task).
                    bTask.isRunning = YES.
                    RELEASE bTask.
                END. /* do trans */
                IF Task.runSync THEN
                RUN VALUE(cRunProgram) (ROWID(Task)).
                ELSE DO:
                    OS-COMMAND NO-WAIT VALUE(
                            SUBSTITUTE(
                                cRun,
                                cRunProgram,
                                "~"" + PROPATH + "+" + STRING(ROWID(Task)) + "~""
                                )
                            ).
                    PAUSE 2 NO-MESSAGE.
                END. /* else */
            END. /* if ne ? */
        END.
        GET NEXT TaskBrowse.
    END. /* do while */
    {&OPEN-QUERY-EmailBrowse}

END PROCEDURE.

PROCEDURE pTrackAudit :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iAuditID AS INTEGER NO-UNDO.

    RUN spCreateAuditHdr (
        "LOG",           /* type  */
        "ASI",           /* db    */
        "{&program-id}", /* table */
        "ND1",           /* key   */
        OUTPUT iAuditID
        ).
    RUN spCreateAuditDtl (
        iAuditID, /* audit id     */
        "",       /* field        */
        0,        /* extent       */
        ipcType,  /* before value */
        "",       /* after value  */
        NO        /* index field  */
        ).

END PROCEDURE.

/* ************************  Function Implementations ***************** */
