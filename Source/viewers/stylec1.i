  
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
       lv-{1}-scr-type[i] = tt-type.
     ld-total[1] = ld-total[1] + tt-dec .    
  END.

  ld-total[1] = DYNAMIC-FUNCTION("sfCommon_ConvDecimalTo1632",cocode, ld-total[1]) .
  {&self-name}:SCREEN-VALUE = STRING(ld-total[1]).

  APPLY "leave" TO {&self-name}.

  RETURN NO-APPLY.
