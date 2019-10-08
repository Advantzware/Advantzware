/* ttbldir.i */

PROCEDURE Build-Directory-Table:
  DEFINE VARIABLE search-dir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE file-name AS CHARACTER FORMAT "X(16)" NO-UNDO.
  DEFINE VARIABLE attr-list AS CHARACTER FORMAT "X(4)" NO-UNDO.
  DEFINE VARIABLE dir-found AS LOGICAL NO-UNDO.

  search-dir = "./".
  INPUT FROM OS-DIR(search-dir) NO-ECHO.
  REPEAT:
    SET file-name ^ attr-list.
    IF attr-list = "f" THEN
    NEXT.
    CREATE ttbldir.
    ttbldir.dirname = LC(file-name).
  END.
  INPUT CLOSE.
END PROCEDURE.
