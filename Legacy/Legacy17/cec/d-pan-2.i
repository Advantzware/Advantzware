  
  DO WITH FRAME {&FRAME-NAME}:
    dscr-{1}:SCREEN-VALUE = "".

    FIND FIRST reftable
        WHERE reftable.reftable EQ "score-type"
          AND reftable.company  EQ cocode
          AND reftable.loc      EQ ""
          AND reftable.code     EQ type-{1}:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL reftable THEN dscr-{1}:SCREEN-VALUE = reftable.dscr.
  END.
