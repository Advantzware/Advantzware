/* setGrid.i - used in procedure setGrid */

&SCOPED-DEFINE formula ({1} - 1) * rectWidth + resourceGrid:WIDTH-PIXELS + 1

WHEN {1} THEN
DO:
  day-{1}:SCREEN-VALUE = ''.
  IF interval-{1}:SCREEN-VALUE NE STRING(intK[{1}],'99') THEN
  interval-{1}:SCREEN-VALUE = STRING(intK[{1}],'99'). /* change when different */
  IF intK[{1}] = 0 THEN /* '00' displayed for an interval */
  ASSIGN
    day-{1}:SCREEN-VALUE = IF hourMode THEN SUBSTRING(STRING(intDate[{1}]),1,5)
                           ELSE SUBSTR(STRING(intT[{1}] * 36,'HH:MM am'),1,2) +
                                SUBSTR(STRING(intT[{1}] * 36,'HH:MM am'),7,2)
    day-{1}:SCREEN-VALUE = REPLACE(day-{1}:SCREEN-VALUE,'/','.')
    day-{1}:SCREEN-VALUE = REPLACE(day-{1}:SCREEN-VALUE,' ','')
    ldummy = day-{1}:MOVE-TO-TOP().
  IF boardGrid-{1}:BGCOLOR NE intColor[{1}] THEN
  boardGrid-{1}:BGCOLOR = intColor[{1}]. /* change only when different */
  ASSIGN
    interval-{1}:X = {&formula}
    day-{1}:X = {&formula}
    interval-{1}:TOOLTIP = 'Date: ' + STRING(intDate[{1}],'99/99/9999') + '~n' +
                           'Start: ' + STRING(intStime[{1}],'HH:MM:SS am') + '~n' +
                           'End: ' + STRING(intEtime[{1}],'HH:MM:SS am')
    day-{1}:TOOLTIP = interval-{1}:TOOLTIP.
  IF ipSetGrid THEN /* happens when entire board is destroyed and rebuilt */
  ASSIGN
    boardGrid-{1}:X = {&formula}
    boardGrid-{1}:WIDTH-PIXELS =
      IF {&formula} + rectWidth LE FRAME {&FRAME-NAME}:WIDTH-PIXELS THEN rectWidth
      ELSE FRAME {&FRAME-NAME}:WIDTH-PIXELS - boardGrid-{1}:X
    boardGrid-{1}:HEIGHT-PIXELS = resourceGrid:HEIGHT-PIXELS.
END.
