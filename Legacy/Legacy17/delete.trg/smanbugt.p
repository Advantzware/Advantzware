&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('asi')
&Scoped-define TABLENAME smanbugt

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
