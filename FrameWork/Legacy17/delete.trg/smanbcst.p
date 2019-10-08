&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('asi')
&Scoped-define TABLENAME smanbcst

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
