
DO WITH FRAME {&FRAME-NAME}:
  IF DEC(eb.i-ps{2}[{1}]:SCREEN-VALUE) EQ 0 OR
     eb.i-code{2}[{1}]:SCREEN-VALUE EQ ""   THEN
    ASSIGN
     eb.i-ps{2}[{1}]:SCREEN-VALUE   = ""
     eb.i-code{2}[{1}]:SCREEN-VALUE = ""
     eb.i-dscr{2}[{1}]:SCREEN-VALUE = ""
     eb.i-%{2}[{1}]:SCREEN-VALUE    = "".
END.
