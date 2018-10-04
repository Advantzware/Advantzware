&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('EMPTRACK')
&Scoped-define TABLENAME emplogin

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
{methods/delete.trg/emplogin.i}
