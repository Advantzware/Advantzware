/*------------------------------------------------------------------------
    File        : taskMonitor.p
    Purpose     : Ability to run Task Monitor as a Service

    Syntax      : RUN AOA/taskMonitor.p
    Note        : requires system/session.p running as SUPER-PROCEDURE

    Description : Task Monitor without User Interface

    Author(s)   : Ron Stark
    Created     : Tue Feb 09 16:55:10 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{AOA/includes/taskerDefs.i}

DEFINE VARIABLE hSession AS HANDLE NO-UNDO.

DEFINE QUERY TaskBrowse FOR Task SCROLLING.

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define program-id taskMonitor.
&Scoped-define OPEN-QUERY-TaskBrowse OPEN QUERY TaskBrowse ~
FOR EACH Task NO-LOCK ~
WHERE Task.scheduled EQ YES OR Task.runNow EQ YES ~
BY Task.runNow DESCENDING ~
BY Task.nextDate ~
BY Task.nextTime INDEXED-REPOSITION.

/* ***************************  Main Block  *************************** */

RUN system/session.p PERSISTENT SET hSession.
SESSION:ADD-SUPER-PROCEDURE (hSession).

RUN pRunCommand (OUTPUT cRun).
dttOpenDateTime = NOW.
DO WHILE TRUE:
    IF SEARCH("taskMonitor.stop") NE ? THEN
    LEAVE.
    RUN pTasks.
    RUN pTaskEmails.
    RUN pLastExecuted.
    PAUSE 10 NO-MESSAGE.
END. /* do while true */

{AOA/includes/taskerProcs.i}
