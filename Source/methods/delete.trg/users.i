/* users.i */

FOR EACH usercomp WHERE usercomp.user_id = users.user_id.
    DELETE usercomp.
END.

FOR EACH usercust WHERE usercust.user_id = users.user_id:
    DELETE usercust.
END.

FOR EACH uservend WHERE uservend.user_id = users.user_id:
    DELETE uservend.
END.

FIND asi._user EXCLUSIVE-LOCK
    WHERE asi._user._userid = users.user_id NO-ERROR.
IF AVAILABLE asi._user THEN DELETE asi._user.

DEF VAR v-file AS CHAR FORMAT "x(30)" NO-UNDO.

IF SEARCH("./usermenu/" + trim(users.user_id) + "/menu.lst" ) <> ? THEN 
         OS-DELETE VALUE("./usermenu/" + users.user_id + "/menu.lst").
IF SEARCH("./usermenu/" + trim(users.user_id) + "/menu.fol" ) <> ? THEN 
         OS-DELETE VALUE("./usermenu/" + users.user_id + "/menu.fol").
IF SEARCH("./usermenu/" + trim(users.user_id) + "/menu.cor" ) <> ? THEN 
         OS-DELETE VALUE("./usermenu/" + users.user_id + "/menu.cor").
IF SEARCH("./usermenu/" + trim(users.user_id) + "/menu.tmp" ) <> ? THEN 
         OS-DELETE VALUE("./usermenu/" + users.user_id + "/menu.tmp").


