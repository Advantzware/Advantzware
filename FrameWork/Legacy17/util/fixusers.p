/* fixusers.p */

FOR EACH usr EXCLUSIVE-LOCK:
  IF CAN-FIND(FIRST users WHERE users.user_id EQ usr.uid) THEN NEXT.
  DELETE usr.
END. /* each usr */

FOR EACH users NO-LOCK:
  IF CAN-FIND(FIRST usr WHERE usr.uid EQ users.user_id) THEN NEXT.
  CREATE usr.
  ASSIGN
    usr.uid = users.user_id
    usr.usr-lang = 'English'.
END. /* each users */

MESSAGE 'Users Fix Complete!' VIEW-AS ALERT-BOX.
