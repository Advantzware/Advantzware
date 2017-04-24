/* 22-JAN-2007 sla: new external .p to make the open file in AppBuilder 
 really act on behalf of the AppBuilder */

DEFINE INPUT  PARAMETER pcFileName      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER phAppBuilderWin AS HANDLE      NO-UNDO.
DEFINE VARIABLE hWin AS HANDLE      NO-UNDO.

DEFINE TEMP-TABLE ttABChildWinInMyPool NO-UNDO LABEL "ttABChildWinInMyPool (to keep track AppBuilder children window(s) that has(have) been created in my widget-pool)"
 FIELD hwin AS HANDLE
 INDEX hwin IS PRIMARY UNIQUE hwin.

THIS-PROCEDURE:CURRENT-WINDOW = phAppBuilderWin.

/* 12-MAY-2007 sla: fix to avoid the deletion of the file window when
ABHack is closed.  The problem is we have to keep this PROCEDURE running persistently for ever...
   => 14-MAY-2007 sla solution implemented with ABHackOpenFileInABCleanUp event
But, not this problem could well open without abhack if a tool with a widget-pool was launched after the AppBuilder... */
CREATE WIDGET-POOL.

PUBLISH "ABHackOpenFileInABCleanUp".  /* ask running instances to cleanup */

SUBSCRIBE TO "ABHackOpenFileInABCleanUp" ANYWHERE. /* I usually avoid anywhere events, but this case is a perfect exception, plus the event name is pretty unique */

/* 14-MAY-2007 sla: keep track of windows that were existing before the open action */
hWin = phAppBuilderWin:FIRST-CHILD NO-ERROR.
DO WHILE hwin <> ?:
    CREATE ttABChildWinInMyPool.
    ttABChildWinInMyPool.hwin = hWin.
    hwin = hwin:NEXT-SIBLING.
END.

/* now open the file */    
RUN adeuib/_open-w.p (pcFileName, "", "OPEN").

IF SEARCH("adeuib/_sanitiz.r") <> ? 
 OR SEARCH("adeuib/_sanitiz.p") <> ? 
 THEN RUN adeuib/_sanitiz.p. /* 14-MAY-2007 sla: does not hurt, thanks to Jan Keirse to point this animal */


/* Now remove windows that were running before the open action, but keep any new one  */
hWin = phAppBuilderWin:FIRST-CHILD NO-ERROR.
DO WHILE hwin <> ?:
    FIND ttABChildWinInMyPool WHERE ttABChildWinInMyPool.hwin = hWin NO-ERROR.
    
    /* this window was already running before openFileInAB, so it is not in my widget-pool */
    IF AVAILABLE ttABChildWinInMyPool THEN DELETE ttABChildWinInMyPool.
    
    /* New window, keep this guy in ttABChildWinInMyPool */
    ELSE DO:
        CREATE ttABChildWinInMyPool.
        ttABChildWinInMyPool.hwin = hWin.
    END.
    hwin = hwin:NEXT-SIBLING.
END.


PROCEDURE ABHackOpenFileInABCleanUp:
    DEFINE VARIABLE lFoundOneStillAlive AS LOGICAL     NO-UNDO.
    
    FOR EACH ttABChildWinInMyPool:
        lFoundOneStillAlive =    VALID-HANDLE(ttABChildWinInMyPool.hwin)
                             AND ttABChildWinInMyPool.hwin:TYPE = "WINDOW"
                             AND ttABChildWinInMyPool.hwin:PARENT = phAppBuilderWin.
        IF lFoundOneStillAlive THEN RETURN.  /* found one still alive, keep my WIDGET-POOL */
    END.
    
    /* my windows are no longer there => kill myself and delete my widget-pool */
    IF NOT lFoundOneStillAlive THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.    
