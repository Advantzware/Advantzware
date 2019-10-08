/* fgcat.i */

  WHEN "cat-format" THEN
  DO:
    IF AVAILABLE fgcat THEN
    cat-format = IF fgcat.commrate = 1 THEN yes ELSE no.
    DISPLAY cat-format WITH FRAME {&FRAME-NAME}.
  END.
