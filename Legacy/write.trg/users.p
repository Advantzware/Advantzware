&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME users

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

IF CONNECTED ("SmartDB") THEN
RUN custom/writeSmartUser.p (users.user_id, users.user_name).
