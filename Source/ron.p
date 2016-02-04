DEFINE VARIABLE codeLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE tableName AS CHARACTER NO-UNDO.

OUTPUT TO 'z:/asi_gui9/pco300/util/fxRecKey.i'.
INPUT FROM 'z:/asi_gui9/pco300/util/fxRecKey.sav' NO-ECHO.
REPEAT:
  IMPORT UNFORMATTED codeLine.
  IF codeLine BEGINS 'PROCEDURE fix' AND
     INDEX(codeLine,'fixTables') EQ 0 THEN DO:
    PUT UNFORMATTED codeLine SKIP.
    ASSIGN
      tableName = REPLACE(codeLine,'PROCEDURE fix','')
      tableName = REPLACE(tableName,':','').
    IMPORT UNFORMATTED codeLine.
    PUT UNFORMATTED codeLine SKIP(1)
      '  DISABLE TRIGGERS FOR LOAD OF ' LC(tableName) '.' SKIP.
  END.
  ELSE
  PUT UNFORMATTED (IF codeLine NE '' THEN codeLine ELSE ' ') SKIP.
END.
INPUT CLOSE.
OUTPUT CLOSE.
