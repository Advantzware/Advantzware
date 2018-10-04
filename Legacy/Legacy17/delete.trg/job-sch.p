&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME job-sch
TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.
/*
{methods/triggers/delete.i}
*/
