/* 24-MAR-2007 sla: Improvement to handle start/stop on CTRL-ALT-O as an anywhere
trigger.  The best way to give the ability to enable or disable an anywhere trigger
defined dynamically is to handle it in a decicated persistent PROCEDURE  */

DEFINE VARIABLE ghABHackProc AS HANDLE     NO-UNDO.

ghABHackProc = SOURCE-PROCEDURE.

PUBLISH "KillabhackCtrlOAnywhereTrig". /* one instance at a time please */

ON 'CTRL-ALT-O' ANYWHERE DO:
    IF VALID-HANDLE(ghABHackProc) THEN RUN startStopSpyingTimer IN ghABHackProc.
    ELSE DELETE PROCEDURE THIS-PROCEDURE.
END.

SUBSCRIBE TO "KillabhackCtrlOAnywhereTrig" ANYWHERE. /* with such an event name, a subscribe anywhere is not abusive */

PROCEDURE KillabhackCtrlOAnywhereTrig:
    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.
