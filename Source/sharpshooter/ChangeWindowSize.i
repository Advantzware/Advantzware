/* ChangeWindowSize.i */

PROCEDURE ChangeWindowSize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiScreenHeight AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiScreenWidth  AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiScreenTop    AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiScreenLeft   AS INTEGER NO-UNDO.

    ASSIGN
        {&WINDOW-NAME}:Y                     = IF ipiScreenTop EQ ? THEN {&WINDOW-NAME}:Y ELSE ipiScreenTop
        {&WINDOW-NAME}:X                     = IF ipiScreenLeft EQ ? THEN {&WINDOW-NAME}:X ELSE ipiScreenLeft
        {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = ipiScreenHeight
        {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS  = ipiScreenWidth
        {&WINDOW-NAME}:HEIGHT                = {&WINDOW-NAME}:VIRTUAL-HEIGHT - 1.22 - INTEGER({&WINDOW-NAME}:STATUS-AREA) /* Subtracting status area and title bar height */
        {&WINDOW-NAME}:WIDTH                 = {&WINDOW-NAME}:VIRTUAL-WIDTH
        FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT   = {&WINDOW-NAME}:HEIGHT
        FRAME {&FRAME-NAME}:VIRTUAL-WIDTH    = {&WINDOW-NAME}:WIDTH
        FRAME {&FRAME-NAME}:HEIGHT           = {&WINDOW-NAME}:HEIGHT
        FRAME {&FRAME-NAME}:WIDTH            = {&WINDOW-NAME}:WIDTH
        .

    RUN pWinReSize NO-ERROR.
END PROCEDURE.
