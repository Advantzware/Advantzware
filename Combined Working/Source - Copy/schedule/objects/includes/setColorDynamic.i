/* setColorDynamic.i */

  DEFINE VARIABLE colorValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE redValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE greenValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE blueValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  COLOR-TABLE:NUM-ENTRIES = 99.
  DO i = 16 TO 29:
    COLOR-TABLE:SET-DYNAMIC(i,YES).
  END.

  IF SEARCH('{&data}/sbColors.dat') EQ ? THEN DO:
    OUTPUT TO '{&data}/sbColors.dat'.
    DO i = 16 TO 29:
      PUT UNFORMATTED '0,0,0' SKIP.
    END.
    OUTPUT CLOSE.
  END.
  
  INPUT FROM VALUE(SEARCH('{&data}/sbColors.dat')) NO-ECHO.
  DO i = 16 TO 29:
    IMPORT colorValue.
    ASSIGN
      redValue = INTEGER(ENTRY(1,colorValue))
      greenValue = INTEGER(ENTRY(2,colorValue))
      blueValue = INTEGER(ENTRY(3,colorValue)).
    COLOR-TABLE:SET-RGB-VALUE(i,RGB-VALUE(redValue,greenValue,blueValue)).
  END.
  INPUT CLOSE.
