  
  DEF VAR ld-total AS DEC EXTENT 2 NO-UNDO.
  DEF VAR i AS INT NO-UNDO.


  FOR EACH tt-array:
    DELETE tt-array.
  END.

  DO i = 1 TO EXTENT(ld-{1}-array):
    CREATE tt-array.
    ASSIGN
     tt-dec  = ld-{1}-array[i]
     tt-type = lv-{1}-scr-type[i].
  END.

  RUN cec/d-panels.w (NO, {&self-name}:LABEL, INPUT-OUTPUT TABLE tt-array).

  i = 0.
  FOR EACH tt-array:
    i = i + 1.
    IF i GT EXTENT(ld-{1}-array) THEN LEAVE.
    ASSIGN
       ld-{1}-array[i]    = tt-dec
       ld-total[1]        = ld-total[1] + (IF v-cecscrn-char NE "Decimal" THEN TRUNC(tt-dec,0)
                                           ELSE tt-dec)
       lv-{1}-scr-type[i] = tt-type.

    IF v-cecscrn-char NE "Decimal" THEN
       ld-total[2] = ld-total[2] + ((tt-dec - TRUNC(tt-dec,0)) * 100).
  END.

  IF v-cecscrn-char NE "Decimal" THEN
     ASSIGN
        ld-total[1] = ld-total[1] + TRUNC(ld-total[2] / li-16-32,0)
        ld-total[2] = (ld-total[2] MODULO li-16-32) / 100.

  {&self-name}:SCREEN-VALUE = STRING(ld-total[1] + ld-total[2]).

  APPLY "leave" TO {&self-name}.

  RETURN NO-APPLY.
