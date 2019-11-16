

DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.

DEFINE VARIABLE ghABHackProc     AS HANDLE   NO-UNDO.
DEFINE VARIABLE ghCustomTriggers AS HANDLE   NO-UNDO.
DEFINE VARIABLE ghWindow         AS HANDLE   NO-UNDO.


ghABHackProc = SOURCE-PROCEDURE.

/* 28-MAR-2008 sla: Request from Jan Keirse to allow adding company specific custom triggers */ 
/* 15-SEP-2008 sla: code refined to avoid a procedure memory leak as pointed out by Dries */
IF  SEARCH("protools/abhack/pcustomeditortriggers.p") <> ? 
 OR SEARCH("protools/abhack/pcustomeditortriggers.r") <> ? 
 THEN DO: 
    /* 02-NOV-2009 sla: better handling of custom proc */
    PUBLISH "abhackKillPcustomeditortriggers".
    RUN protools/abhack/pcustomeditortriggers.p PERSISTENT SET ghCustomTriggers (INPUT phEditor, INPUT ghABHackProc).
END. 



                                           
ghWindow = phEditor:WINDOW.

/* 10-JUN-2007 sla: catch the Maximize/Restore Actions to fire the ABHack improvements */                           
ON 'WINDOW-MAXIMIZED' OF ghWindow DO:
    ghWindow:WINDOW-STATE = WINDOW-MAXIMIZED.
    RUN ctrlM IN ghABHackProc (phEditor).
END.

ON 'WINDOW-MAXIMIZED' OF ghWindow  DO:
    ghWindow:WINDOW-STATE = WINDOW-NORMAL.
    RUN ctrlM IN ghABHackProc (phEditor).
END.

/* 28-OCT-2007 sla: new feature to adapt the size of a window to the number of lines in the editor */
ON 'CTRL-W' OF phEditor RUN adaptWindow IN ghABHackProc (phEditor) NO-ERROR.

ON 'ALT-PAGE-UP'   OF phEditor RUN JumpMruSection IN ghABHackProc (phEditor, "up") NO-ERROR.
ON 'ALT-PAGE-DOWN' OF phEditor RUN JumpMruSection IN ghABHackProc (phEditor, "down") NO-ERROR.


ON 'ALT-CURSOR-DOWN'  OF phEditor RUN altCursorUpDown  IN ghABHackProc (phEditor, "down") NO-ERROR.
ON 'ALT-CURSOR-UP'    OF phEditor RUN altCursorUpDown  IN ghABHackProc (phEditor, "up") NO-ERROR.
/* 01-OCT-2007 sla: adding 'ALT-CTRL-CURSOR-UP' to use this feature on Linux with a Compiz 3D Desktop */
ON 'ALT-CTRL-CURSOR-DOWN', 'ALT-CTRL-CURSOR-UP'  OF phEditor RUN altCtrlCursorDown  IN ghABHackProc (phEditor) NO-ERROR.

/* 24-MAR-2007 sla: now using a PERSISTENT anywhere trigger in abhackCtrlOAnywhereTrig.p
 ON 'CTRL-ALT-O' OF phEditor RUN startStopSpyingTimer IN ghABHackProc NO-ERROR. */

ON 'CTRL-ALT-P' OF phEditor RUN ctrlAltP IN ghABHackProc (phEditor) NO-ERROR.

ON 'CTRL-ALT-I' OF phEditor RUN ctrlAltI IN ghABHackProc (phEditor) NO-ERROR.

ON 'ALT-CURSOR-LEFT'  OF phEditor RUN findSelected IN ghABHackProc (phEditor, 'left') NO-ERROR.
ON 'ALT-CURSOR-RIGHT' OF phEditor RUN findSelected IN ghABHackProc (phEditor, 'right') NO-ERROR.

ON 'ALT-CTRL-CURSOR-LEFT'  OF phEditor RUN completeWithSimilar IN ghABHackProc (phEditor, 'left') NO-ERROR.
ON 'ALT-CTRL-CURSOR-RIGHT' OF phEditor RUN completeWithSimilar IN ghABHackProc (phEditor, 'right') NO-ERROR.

/* 15-JAN-2007 sla: This one was working in V9, but was broken in OE 10, let's reanable it' */
ON 'ALT-CTRL-V' OF phEditor phEditor:SOURCE-COMMAND('list-clipboards', '').

ON 'ALT-C' OF phEditor RUN altCPressed IN ghABHackProc (phEditor) NO-ERROR.

ON 'ALT-H' OF phEditor RUN altHPressed IN ghABHackProc (phEditor) NO-ERROR.

ON 'ALT-S' OF phEditor RUN altSPressed IN ghABHackProc (phEditor) NO-ERROR.

ON 'ALT-CTRL-X' OF phEditor RUN analyzeXref IN ghABHackProc (phEditor) NO-ERROR.

ON 'CTRL-F3' OF phEditor RUN CtrlF3Pressed IN ghABHackProc.
ON 'CTRL-F'  OF phEditor RUN CtrlFPressed  IN ghABHackProc (phEditor).

ON '(' OF phEditor RUN insertClosingGuy IN ghABHackProc (phEditor ,'(') NO-ERROR.
ON '[' OF phEditor RUN insertClosingGuy IN ghABHackProc (phEditor ,'[') NO-ERROR.
ON '"' OF phEditor RUN insertClosingGuy IN ghABHackProc (phEditor ,'"') NO-ERROR.
ON "'" OF phEditor RUN insertClosingGuy IN ghABHackProc (phEditor ,"'") NO-ERROR.
ON '~{' OF phEditor RUN insertClosingGuy IN ghABHackProc (phEditor ,'~{') NO-ERROR.

ON 'F6'     OF phEditor RUN saveFile IN ghABHackProc (phEditor, "F6") NO-ERROR.
ON 'CTRL-S' OF phEditor RUN saveFile IN ghABHackProc (phEditor, "Ctrl-s") NO-ERROR.

/* 14-JAN-2007 sla: added Alt-G to do the same as Ctrl-Alt-G */
ON 'ALT-CTRL-G', 'ALT-G' OF phEditor RUN loadGlobalResources IN ghABHackProc (phEditor) NO-ERROR.

ON 'ALT-CTRL-M' OF phEditor RUN compileFile IN ghABHackProc (phEditor) NO-ERROR.

/* 01-NOV-2008 sla: new feature generate Listing on alt-Ctrl-T */
ON 'ALT-CTRL-T' OF phEditor RUN generateListing IN ghABHackProc (phEditor, "list") NO-ERROR.

/* 06-NOV-2008 jankeir: new feature generate debug Listing on alt-Ctrl-D */
ON 'ALT-CTRL-D' OF phEditor RUN generateListing IN ghABHackProc (phEditor, "debug") NO-ERROR.

ON 'ALT-CTRL-E' OF phEditor RUN openInProcedureWindow IN ghABHackProc (phEditor) NO-ERROR.

ON 'CTRL-CURSOR-LEFT'  OF phEditor RUN ctrlCursorLeft IN ghABHackProc (phEditor) NO-ERROR.

ON 'CTRL-CURSOR-RIGHT' OF phEditor RUN ctrlCursorRight IN ghABHackProc (phEditor) NO-ERROR.

ON 'ALT-CTRL-L' OF phEditor RUN alignCode IN ghABHackProc (phEditor) NO-ERROR.

ON '.' OF phEditor RUN dotPressed IN ghABHackProc (phEditor) NO-ERROR.

/* 05-JAN-2007 sla: will now call this guy from the insertClosingGuy PROCEDURE 
 ON '(' OF phEditor RUN leftParenthesePressed IN ghABHackProc (phEditor) NO-ERROR. */

ON ':' OF phEditor RUN colonPressed IN ghABHackProc (phEditor) NO-ERROR.

ON '/' OF phEditor DO:
     RUN slashPressed IN ghABHackProc (phEditor) NO-ERROR.
    IF RETURN-VALUE = "noApply" THEN RETURN NO-APPLY.
END.

ON '+' OF phEditor DO:
    RUN plusPressed IN ghABHackProc (phEditor) NO-ERROR.
    IF RETURN-VALUE = "noApply" THEN RETURN NO-APPLY.
END.

ON 'ctrl-Backspace':U OF phEditor DO:
    RUN CtrlBackspacePressed IN ghABHackProc (phEditor) NO-ERROR.

    IF RETURN-VALUE = "YES" THEN DO:
        phEditor:SOURCE-COMMAND('delete-prev-word', '').
        RETURN NO-APPLY.
    END.
    /* IF RETURN-VALUE <> "noApply" THEN phEditor:SOURCE-COMMAND('undo', ''). */
END.

ON 'ctrl-del':U OF phEditor DO:
    RUN CtrlDeletePressed IN ghABHackProc (phEditor) NO-ERROR.

    IF RETURN-VALUE = "YES" THEN DO:
        phEditor:SOURCE-COMMAND('delete-word', '').
        RETURN NO-APPLY.
    END.
END.

/*
/* sadly, they have not implemented the value-changed event for the source-code editor, so I cannot track mouse-paste */
ON 'ANY-PRINTABLE', 'BACKSPACE', 'DELETE-CHARACTER' OF phEditor DO:
    RUN fireValueChanged IN ghABHackProc (phEditor) NO-ERROR.
    IF RETURN-VALUE = "noApply" THEN RETURN NO-APPLY.
END.
*/

/* for debugging purposes */
FUNCTION getTargetEditorHandle RETURNS HANDLE ():
    RETURN phEditor.
END FUNCTION.


/* 02-NOV-2009 sla: better handling of custom proc with a abhackKillPcustomeditortriggers event */
PROCEDURE killProcedure:    
    PUBLISH "abhackKillPcustomeditortriggers".
    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.
