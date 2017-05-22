&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('asi')
&Scoped-define TABLENAME e-item-cust

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}
