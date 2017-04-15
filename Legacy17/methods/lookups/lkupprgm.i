/* lkupprgm.i */

PROCEDURE Build-LkupPrgm-Table:
  DEFINE VARIABLE search-dir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE file-name AS CHARACTER FORMAT "X(16)" NO-UNDO.
  DEFINE VARIABLE attr-list AS CHARACTER FORMAT "X(4)" NO-UNDO.
  DEFINE VARIABLE dir-found AS LOGICAL NO-UNDO.

  search-dir = "./searches".
  INPUT FROM OS-DIR(search-dir) NO-ECHO.
  REPEAT:
    SET file-name ^ attr-list.
    IF attr-list NE "f" OR INDEX(file-name,".p") = 0 THEN
    NEXT.
    CREATE lkupprgm.
    ASSIGN
      file-name = REPLACE(file-name,".p",".")
      lkupprgm.lkupprgm = LC(file-name).
  END.
  INPUT CLOSE.
END PROCEDURE.
