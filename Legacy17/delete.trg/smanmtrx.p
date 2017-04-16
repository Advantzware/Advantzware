&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('asi')
&Scoped-define TABLENAME smanmtrx

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
