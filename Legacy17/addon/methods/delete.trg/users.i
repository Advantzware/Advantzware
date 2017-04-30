/* users.i */

FOR EACH usercomp WHERE usercomp.user_id = users.user_id.
    DELETE usercomp.
END.

FIND NOSWEAT._user EXCLUSIVE-LOCK
    WHERE NOSWEAT._user._userid = users.user_id NO-ERROR.
IF AVAILABLE NOSWEAT._user THEN DELETE NOSWEAT._user.

/*
FIND FIRST usr EXCLUSIVE-LOCK WHERE usr.uid EQ users.user_id NO-ERROR.
IF AVAILABLE usr THEN DELETE usr.
*/

DEF VAR v-file AS cha FORM "x(30)" NO-UNDO.

IF SEARCH("./usermenu/" + trim(users.user_id) + "/menu.lst" ) <> ? THEN 
         OS-DELETE VALUE("./usermenu/" + users.user_id + "/menu.lst").
IF SEARCH("./usermenu/" + trim(users.user_id) + "/menu.fol" ) <> ? THEN 
         OS-DELETE VALUE("./usermenu/" + users.user_id + "/menu.fol").
IF SEARCH("./usermenu/" + trim(users.user_id) + "/menu.cor" ) <> ? THEN 
         OS-DELETE VALUE("./usermenu/" + users.user_id + "/menu.cor").
IF SEARCH("./usermenu/" + trim(users.user_id) + "/menu.tmp" ) <> ? THEN 
         OS-DELETE VALUE("./usermenu/" + users.user_id + "/menu.tmp").


