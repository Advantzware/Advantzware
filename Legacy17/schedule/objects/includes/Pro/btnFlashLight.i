/* btnFlashLight.i - trigger for btnFlashLight in board.w */

  ASSIGN
    flashLight = NOT flashLight
    SELF:TOOLTIP = SELF:PRIVATE-DATA + ' ' + STRING(NOT flashLight,'On/Off')
    ldummy = SELF:LOAD-IMAGE('{&images}/lightBulb' +
                             TRIM(STRING(flashLight,'On/Off')) + '.bmp').
  IF NOT flashLight THEN
  RUN showFlashLight ('').
