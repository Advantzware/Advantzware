/* hideLightBulb.i - used in showBoard in board.w */

DO i = 1 TO resourceIdx:
  IF NOT lightBulbWidget[i]:HIDDEN THEN
  lightBulbWidget[i]:HIDDEN = YES.
END.
