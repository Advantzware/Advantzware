&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME machchrg

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
{methods/delete.trg/{&TABLENAME}.i}
