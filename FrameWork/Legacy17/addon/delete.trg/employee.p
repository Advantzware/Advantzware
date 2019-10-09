&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('EMPTRACK')
&Scoped-define TABLENAME employee

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
{methods/delete.trg/{&TABLENAME}.i}
