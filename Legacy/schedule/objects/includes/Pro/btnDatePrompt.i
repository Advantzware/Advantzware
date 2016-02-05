/* btnDatePrompt.i - trigger for btnDatePrompt in board.w */

  ASSIGN
    boardDatePrompt = NOT boardDatePrompt
    SELF:TOOLTIP = SELF:PRIVATE-DATA + ' ' + STRING(NOT boardDatePrompt,'On/Off')
    ldummy = SELF:LOAD-IMAGE('{&images}/date' +
                             TRIM(STRING(boardDatePrompt,'On/Off')) + '.bmp').
