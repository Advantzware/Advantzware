&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME quote-vendor

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

ASSIGN
   {&TABLENAME}.upd-date = TODAY 
   {&TABLENAME}.upd-time = TIME
   {&TABLENAME}.upd-user = USERID("nosweat").


/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

RETURN.
