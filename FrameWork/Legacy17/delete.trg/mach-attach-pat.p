&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME mach-attach-pat

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

   
 
