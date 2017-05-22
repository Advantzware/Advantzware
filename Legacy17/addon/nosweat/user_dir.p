/* user_dir.p */

DEFINE VARIABLE search-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE file-name AS CHARACTER FORMAT "X(16)" NO-UNDO.
DEFINE VARIABLE attr-list AS CHARACTER FORMAT "X(4)" NO-UNDO.
DEFINE VARIABLE dir-found AS LOGICAL NO-UNDO.

search-dir = './users'.
INPUT FROM OS-DIR(search-dir) NO-ECHO.
REPEAT:
  SET file-name ^ attr-list.
  IF attr-list = 'f' THEN
  NEXT.
  IF file-name = USERID("NOSWEAT") THEN
  DO:
    dir-found = yes.
    LEAVE.
  END.
END.
INPUT CLOSE.
IF NOT dir-found THEN
OS-CREATE-DIR VALUE(search-dir + "/" + USERID("NOSWEAT")).
/*
IF SEARCH(search-dir + "/" + USERID("NOSWEAT") + "/menu.lst") = ? THEN
OS-COPY "./menu.lst" VALUE(search-dir + "/" + USERID("NOSWEAT") + "/menu.lst").
*/
