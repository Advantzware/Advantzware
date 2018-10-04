&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME ap-chk

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}
