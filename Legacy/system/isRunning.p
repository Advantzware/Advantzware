/* isRunning.p */

DEFINE TEMP-TABLE ttIsRunning NO-UNDO
    FIELD prgTitle  AS CHARACTER 
    FIELD prgmName  AS CHARACTER 
    FIELD hWindow   AS HANDLE 
    FIELD hFrame    AS HANDLE
    FIELD frameName AS CHARACTER
        INDEX prgTitle IS PRIMARY 
            prgTitle
            prgmName
            .
DEFINE VARIABLE hWidget   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hFrame    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hChild    AS HANDLE    NO-UNDO.

EMPTY TEMP-TABLE ttIsRunning.

ASSIGN 
    hWidget = SESSION:HANDLE
    hWidget = hWidget:FIRST-CHILD
    . 
DO WHILE VALID-HANDLE(hWidget):
    hFrame = hWidget:FIRST-CHILD.
    IF VALID-HANDLE(hFrame) THEN DO:
        IF hFrame:TYPE EQ "FRAME" THEN
        RUN pCreateTtIsRunning (hWidget, hFrame).
    END. /* if valid hframe */

    IF VALID-HANDLE(hFrame) THEN
    hChild = hFrame:FIRST-CHILD.
    
    IF VALID-HANDLE(hChild) THEN  
    hChild = hChild:FIRST-CHILD.
    
    DO WHILE VALID-HANDLE(hChild):
        IF hChild:TYPE EQ "FRAME" THEN
        RUN pCreateTtIsRunning (hWidget, hChild).
        hChild = hChild:NEXT-SIBLING.
    END. /* do while */
    hWidget = hWidget:NEXT-SIBLING.
END. /* do while */

PROCEDURE pCreateTtIsRunning:
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER iphFrame  AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE hThisProc AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cName     AS CHARACTER NO-UNDO.

    hThisProc = iphFrame:INSTANTIATING-PROCEDURE.
    IF NOT VALID-HANDLE(hThisProc) THEN RETURN.
    cName = ENTRY(1,hThisProc:NAME,".").
    CREATE ttIsRunning.
    ASSIGN 
        ttIsRunning.prgTitle  = iphWidget:TITLE
        ttIsRunning.prgmName  = cName
        ttIsRunning.hWindow   = iphWidget
        ttIsRunning.hFrame    = iphFrame
        ttIsRunning.frameName = iphFrame:NAME
        .
END PROCEDURE.

OUTPUT TO c:\tmp\widgets.dat.
FOR EACH ttIsRunning:
    DISPLAY 
        ttIsRunning.prgTitle FORMAT "x(30)" LABEL "TITLE"
        ttIsRunning.prgmName FORMAT "x(20)" LABEL "NAME"
        STRING(ttIsRunning.hWindow) LABEL "WINDOW"
        STRING(ttIsRunning.hFrame) LABEL "FRAME"
        ttIsRunning.frameName LABEL "F-NAME" FORMAT "x(12)"
            WITH STREAM-IO DOWN WIDTH 200. 
END.
OUTPUT CLOSE.
OS-COMMAND NO-WAIT notepad.exe c:\tmp\widgets.dat.
