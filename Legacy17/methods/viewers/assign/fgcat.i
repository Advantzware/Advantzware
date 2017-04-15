/* fgcat.i */

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
    cat-format
    fgcat.commrate = INT(cat-format).
  DISABLE cat-format.
END.
