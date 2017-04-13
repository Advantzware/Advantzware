/* The point of this procedure is to solve a problem that can occur when a PSTimer
 fires a tick event while the session is displaying a message view-as alert-box.
 See comment in ABHackWin.w/PSTimerSpy.OCX.Tick :

 Problem with a message that comes from find/replace dialog-box called from the editor window:
 an unstable situation can occur in 9.1E00 with errors 2910 or 4123 (in client logs)
 solution is to detect we are in such a dialog, then stop the timer until a printable
 key event occurs thanks to a little temporary persistent trigger in protools/abhack/ABHackSleepUntilPrintable.p */

DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE gshprocEditorTriggers AS HANDLE     NO-UNDO.
DEFINE VARIABLE hEditorWin AS HANDLE      NO-UNDO.

hEditorWin = phEditor:WINDOW NO-ERROR.

IF  NOT VALID-HANDLE(phEditor)
 OR NOT VALID-HANDLE(hEditorWin)
 THEN DO:
    DELETE PROCEDURE THIS-PROCEDURE.
    RETURN.
END.

SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "restartABHackSpyingTimer" ANYWHERE
 RUN-PROCEDURE "killEdtPrintableTrigPersitProc".
SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "killEdtPrintableTrigPersitProc" IN SOURCE-PROCEDURE.

/* 21-NOV-2006 sla: finally, do not use ANY-KEY to let Ctrl-F work for the code preview window */
ON 'any-printable', 'BACKSPACE', 'DELETE-CHARACTER'
 , 'CURSOR-UP', 'CURSOR-DOWN', 'CURSOR-LEFT', 'CURSOR-RIGHT'
 , 'PAGE-UP', 'PAGE-DOWN', 'HOME', 'END'
 OF hEditorWin ANYWHERE DO:
    PUBLISH "restartABHackSpyingTimer".
    RUN killEdtPrintableTrigPersitProc.
END.

ON 'ENTRY' OF hEditorWin DO:
    PUBLISH "restartABHackSpyingTimer".
    RUN killEdtPrintableTrigPersitProc.
END.

PROCEDURE killEdtPrintableTrigPersitProc:
    DELETE PROCEDURE THIS-PROCEDURE.
    /*IF OS-GETENV("userName") = "E250050" THEN
     MESSAGE "restartABHackSpyingTimer killing " STRING(THIS-PROCEDURE:HANDLE)
         VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
END PROCEDURE.


/*
IF VALID-HANDLE(phEditor) THEN ON 'ANY-KEY' OF phEditor DO:
    PUBLISH "restartABHackSpyingTimer".

    /* restartABHackSpyingTimer will do the job
    DELETE PROCEDURE THIS-PROCEDURE.*/
END.
*/

/* 21-NOV-2006 sla: Let's try the ANYKEY event instead
ON 'any-printable', 'BACKSPACE', 'DELETE-CHARACTER' ANYWHERE DO:
    PUBLISH "restartABHackSpyingTimer".
    DELETE PROCEDURE THIS-PROCEDURE.
END. */

/* 20-NOV-2006 sla: This is to restrictive, use ANYWHERE trigger isntead
ON 'any-printable', 'BACKSPACE', 'DELETE-CHARACTER' OF phEditor DO:
    PUBLISH "restartABHackSpyingTimer".

    /* reset the overiden trigger */
    IF VALID-HANDLE(gshprocEditorTriggers) THEN DELETE PROCEDURE gshprocEditorTriggers.
    RUN protools/abhack/procEditorTriggers.p  PERSISTENT SET gshprocEditorTriggers (phEditor).

    DELETE PROCEDURE THIS-PROCEDURE.
END.*/



