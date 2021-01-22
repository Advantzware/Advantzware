/* users.i */

FOR EACH usercomp EXCLUSIVE-LOCK
    WHERE usercomp.user_id EQ users.user_id
    :
    DELETE usercomp.
END.

FOR EACH usercust EXCLUSIVE-LOCK
    WHERE usercust.user_id EQ users.user_id
    :
    DELETE usercust.
END.

FOR EACH uservend EXCLUSIVE-LOCK
    WHERE uservend.user_id EQ users.user_id
    :
    DELETE uservend.
END.

{methods/delete.trg/removeUser.i}

FIND FIRST ASI._user EXCLUSIVE-LOCK
     WHERE ASI._user._userid EQ users.user_id
     NO-ERROR.
IF AVAILABLE ASI._user THEN
DELETE ASI._user.
