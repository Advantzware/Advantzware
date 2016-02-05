/* conflict.i - used in config.w */

  ASSIGN {&SELF-NAME}.
  IF {&SELF-NAME} THEN
  DO WITH FRAME defaultsFrame:
    ASSIGN
      {1} = NO
      {2} = NO.
    DISPLAY {1} {2}.
    DISABLE {1} {2}.
  END.
  ELSE
  ENABLE {1} {2} WITH FRAME defaultsFrame.
