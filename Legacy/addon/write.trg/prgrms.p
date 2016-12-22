&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME("ASI")
&Scoped-define TABLENAME prgrms

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}
