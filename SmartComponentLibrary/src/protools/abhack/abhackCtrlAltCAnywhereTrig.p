/* 03-APR-2007 sla: Improvement to handle start/stop on ALT-CTRL-C as an anywhere
trigger.  The best way to give the ability to enable or disable an anywhere trigger
defined dynamically is to handle it in a decicated persistent PROCEDURE  */

DEFINE VARIABLE ghABHackProc AS HANDLE     NO-UNDO.

ghABHackProc = SOURCE-PROCEDURE.

PUBLISH "KillabhackCtrlAltCAnywhereTrig". /* one instance at a time please */

ON 'CTRL-ALT-C' ANYWHERE DO:
    IF VALID-HANDLE(ghABHackProc) THEN RUN copyUISVBelowMouse IN ghABHackProc.
    ELSE DELETE PROCEDURE THIS-PROCEDURE.
END.

SUBSCRIBE TO "KillabhackCtrlAltCAnywhereTrig" ANYWHERE. /* with such an event name, a subscribe anywhere is not abusive */

PROCEDURE KillabhackCtrlAltCAnywhereTrig:
    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.
