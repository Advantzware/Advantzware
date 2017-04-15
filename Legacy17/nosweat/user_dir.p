/* user_dir.p 
05/20/03  YSK use usermenu instead of users directory to increase performance */

DEFINE VARIABLE search-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE file-name AS CHARACTER FORMAT "X(16)" NO-UNDO.
DEFINE VARIABLE attr-list AS CHARACTER FORMAT "X(4)" NO-UNDO.
DEFINE VARIABLE dir-found AS LOGICAL NO-UNDO.

search-dir = './usermenu'.
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
IF NOT dir-found THEN DO:
   OS-CREATE-DIR VALUE(search-dir + "/" + USERID("NOSWEAT")).
   OS-CREATE-DIR VALUE("users/" + USERID("NOSWEAT")).
END.

/*
IF SEARCH(search-dir + "/" + USERID("NOSWEAT") + "/menu.lst") = ? THEN
OS-COPY "./menu.lst" VALUE(search-dir + "/" + USERID("NOSWEAT") + "/menu.lst").
*/
