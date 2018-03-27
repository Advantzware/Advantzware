/* btnDetail.i - trigger for btnDetail in board.w */

  ASSIGN
    detailWindow = NOT detailWindow
    SELF:TOOLTIP = SELF:PRIVATE-DATA + ' ' + STRING(NOT detailWindow,'On/Off')
    ldummy = SELF:LOAD-IMAGE('{&images}/detailWin' +
                             TRIM(STRING(detailWindow,'On/Off')) + '.bmp').
