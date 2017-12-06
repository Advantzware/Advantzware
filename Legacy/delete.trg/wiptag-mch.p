&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME wiptag-mch

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.
{custom/globdefs.i}

{methods/triggers/delete.i}
