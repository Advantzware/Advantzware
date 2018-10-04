&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('asi')
&Scoped-define TABLENAME smanbcat

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
