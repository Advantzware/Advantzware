/***************************************************K*******************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : start.p
    Purpose     : SmartFramework Scheduler runtime startup procedure

    Syntax      :

    Description :

    Author(s)   :
    Created     : Sat Jul 16 23:57:17 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Scheduler.Runtime.* FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH .

DEFINE VARIABLE lVerbose             AS LOGICAL   NO-UNDO INIT ? .
DEFINE VARIABLE cKeepRunning         AS CHARACTER NO-UNDO INIT ? .
DEFINE VARIABLE cPauseBetweenPolling AS CHARACTER NO-UNDO INIT ? .

DEFINE VARIABLE iKeepRunning         AS INTEGER   NO-UNDO INIT ? .
DEFINE VARIABLE iPauseBetweenPolling AS INTEGER   NO-UNDO INIT ? .

{Consultingwerk/products.i}

/* ***************************  Main Block  *************************** */

MESSAGE "########################################################################":U                                  SKIP
        "### SmartComponent Library Scheduler Agent Startup                      ":U                                  SKIP
        "### Framework Version:":U (NEW Consultingwerk.FrameworkVersion()):GetVersionString()                         SKIP
        "### OpenEdge Version:":U SUBSTITUTE ("&1&2":U,PROVERSION, Consultingwerk.Util.SessionHelper:GetPatchLevel()) SKIP
        "### Agent Type:":U SESSION:CLIENT-TYPE SESSION:WINDOW-SYSTEM                                                 SKIP
        "###":U (NEW Consultingwerk.FrameworkVersion()):GetCopyRights()                                               SKIP
        "########################################################################":U                                  SKIP (2).

ASSIGN SESSION:ERROR-STACK-TRACE = TRUE
       SESSION:DEBUG-ALERT       = TRUE .

ASSIGN lVerbose             = DataTypeHelper:ToLogical (DYNAMIC-FUNCTION("getParameter":U IN SOURCE-PROCEDURE, INPUT "Verbose":U))
       cKeepRunning         = DYNAMIC-FUNCTION("getParameter":U IN SOURCE-PROCEDURE, INPUT "KeepRunning":U)
       cPauseBetweenPolling = DYNAMIC-FUNCTION("getParameter":U IN SOURCE-PROCEDURE, INPUT "PauseBetweenPolling":U)
       .

IF cKeepRunning > "":U THEN DO:
    ASSIGN iKeepRunning = DataTypeHelper:ToInteger(cKeepRunning) .

    IF iKeepRunning <> ? THEN
        SchedulerRuntime:KeepRunning = iKeepRunning .
END.

IF cPauseBetweenPolling > "":U THEN DO:
    ASSIGN iPauseBetweenPolling = DataTypeHelper:ToInteger(cPauseBetweenPolling) .

    IF iPauseBetweenPolling <> ? THEN
        SchedulerRuntime:PauseBetweenPolling = iPauseBetweenPolling .
END.

IF lVerbose <> ? THEN
    SchedulerRuntime:Verbose = lVerbose .

SchedulerRuntime:Execute () .

MESSAGE SKIP(0) "*** Scheduler runtime exiting with success."{&TRAN} .

RETURN "0":U .

CATCH err AS Progress.Lang.Error:
    MESSAGE SKIP(0) Consultingwerk.Util.ErrorHelper:FormattedErrorMessagesExt (err) .

    MESSAGE SKIP(0) "*** Scheduler runtime exiting with error."{&TRAN} .

    RETURN "1":U .
END CATCH.


