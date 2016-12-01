/* users.i */

DEFINE VARIABLE cdummy AS CHARACTER FORMAT "X(30)" LABEL "User Password" NO-UNDO.

DEFINE BUFFER bUsr FOR usr.

FIND ASI._user
    WHERE ASI._user._userid = users.user_id EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAILABLE ASI._user THEN DO:
  UPDATE SKIP(1) SPACE(12) "Enter New User's Initial Password" SKIP(1)
    SPACE(5) cdummy SPACE(5) SKIP(1)
      WITH FRAME f-password
          COLUMN 30 ROW 10 SIDE-LABELS TITLE "User Password" OVERLAY.
  HIDE FRAME f-password NO-PAUSE.
  CREATE ASI._user.
  ASSIGN
    ASI._user._userid = users.user_id
    ASI._user._password = ENCODE(cdummy).
END.
ASI._user._user-name = users.user_name.

FIND FIRST bUsr WHERE bUsr.uid EQ users.user_id NO-ERROR.
IF AVAILABLE bUsr AND ROWID(bUsr) NE ROWID(usr) THEN
DELETE bUsr.

IF usr.uid EQ '' THEN
usr.uid = users.user_id.
