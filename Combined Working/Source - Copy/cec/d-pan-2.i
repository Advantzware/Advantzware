  
  DO WITH FRAME {&FRAME-NAME}:
    dscr-{1}:SCREEN-VALUE = "".
      FIND FIRST scoreType 
             WHERE scoreType.company   EQ cocode
             AND scoreType.scoreType EQ type-{1}:SCREEN-VALUE
           NO-LOCK NO-ERROR.
      IF AVAIL scoreType THEN dscr-{1}:SCREEN-VALUE = scoreType.description.
  END.

  