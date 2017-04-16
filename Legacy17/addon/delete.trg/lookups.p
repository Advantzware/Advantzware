&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('NOSWEAT')
&Scoped-define TABLENAME lookups

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
