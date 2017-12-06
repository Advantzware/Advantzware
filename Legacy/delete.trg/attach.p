&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME attach

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/audittrg.i}
