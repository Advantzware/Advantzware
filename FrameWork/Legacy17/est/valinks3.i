
DO WITH FRAME {&FRAME-NAME}:
  IF DEC(eb.i-ps{2}[{1}]:SCREEN-VALUE) EQ 0 THEN
    fi_unit-{1}:SCREEN-VALUE = "".
  /* RUN getUnit# ({1}). - implement this code to fire on leave of */
END.
