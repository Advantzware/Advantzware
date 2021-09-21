/* pStatusMessage.i - rstark - 9.15.2021 */

PROCEDURE pStatusMessage :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcStatusMessage AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiStatusMessage AS INTEGER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        statusMessage:SCREEN-VALUE = " " + CAPS(ipcStatusMessage).
        CASE ipiStatusMessage:
            WHEN 0 THEN
            ASSIGN
                statusMessage:BGCOLOR = ?
                statusMessage:FGCOLOR = ?
                .
            WHEN 1 THEN
            ASSIGN
                statusMessage:BGCOLOR = 10
                statusMessage:FGCOLOR = 0
                .
            WHEN 2 THEN
            ASSIGN
                statusMessage:BGCOLOR = 11
                statusMessage:FGCOLOR = 0
                .
            WHEN 3 THEN
            ASSIGN
                statusMessage:BGCOLOR = 12
                statusMessage:FGCOLOR = 15
                .
        END CASE.
    END. /* with frame */

END PROCEDURE.
