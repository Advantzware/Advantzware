/* runCueCard.i - rstark - 9.25.2018 */

IF fSuperRunning("session.") THEN DO:
    IF NOT VALID-HANDLE(hCueWindow) THEN
    hCueWindow = {&WINDOW-NAME}:HANDLE.
    IF NOT VALID-HANDLE(hCueFrame) THEN 
    hCueFrame = FRAME {&FRAME-NAME}:HANDLE. 
    RUN spRunCueCard ("System", cCuePrgmName, hCueWindow, hCueFrame, lCueActive).
END. /* if super running */
