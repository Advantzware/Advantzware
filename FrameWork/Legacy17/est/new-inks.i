
DO WITH FRAME {&FRAME-NAME}:
  eb.i-dscr{2}[{1}]:SCREEN-VALUE = "".

  FIND item
      {sys/look/itemivW.i}
        AND item.i-no EQ eb.i-code{2}[{1}]:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF AVAIL item THEN eb.i-dscr{2}[{1}]:SCREEN-VALUE = item.i-name.

  IF "{2}" EQ "2" AND {1} EQ 1 THEN RUN show-pr-type.
END.
