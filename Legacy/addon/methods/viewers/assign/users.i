/* users.i */

DEFINE VARIABLE cdummy AS CHARACTER FORMAT "X(30)" LABEL "User Password" NO-UNDO.

FIND NOSWEAT._user
    WHERE NOSWEAT._user._userid = users.user_id EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAILABLE NOSWEAT._user THEN
DO:
  UPDATE SKIP(1) SPACE(12) "Enter New User's Initial Password" SKIP(1)
    SPACE(5) cdummy SPACE(5) SKIP(1)
      WITH FRAME f-password
          CENTERED ROW 10 SIDE-LABELS TITLE "User Password" OVERLAY.
  HIDE FRAME f-password NO-PAUSE.
  CREATE NOSWEAT._user.
  ASSIGN
    NOSWEAT._user._userid = users.user_id
    NOSWEAT._user._password = ENCODE(cdummy).
END.
NOSWEAT._user._user-name = users.user_name.
