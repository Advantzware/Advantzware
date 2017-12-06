&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME loadtag

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

IF {&TABLENAME}.partial EQ ? THEN {&TABLENAME}.partial = 0.

IF {&TABLENAME}.qty-case LT 0 THEN
  {&TABLENAME}.qty-case = {&TABLENAME}.qty-case * -1.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

