&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('tmp')
&Scoped-define TABLENAME emailcod

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}
