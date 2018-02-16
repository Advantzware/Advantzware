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

FOR EACH reftable WHERE
    reftable.reftable EQ "users.phone-no" AND
    reftable.company EQ users.user_id:
    DELETE reftable.
END.

FOR EACH reftable WHERE
    reftable.reftable EQ "users.fax-no" AND
    reftable.company EQ users.user_id:
    DELETE reftable.
END.

FOR EACH reftable WHERE
    reftable.reftable EQ "users.phone-cnty" AND
    reftable.company EQ users.user_id:
    DELETE reftable.
END.

FOR EACH reftable WHERE
    reftable.reftable EQ "users.fax-cnty" AND
    reftable.company EQ users.user_id:
    DELETE reftable.
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


