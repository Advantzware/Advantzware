&Scoped-define ACTION DELETE
&Scoped-define DBNAME LDBNAME('sb')
&Scoped-define TABLENAME sbNote

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
