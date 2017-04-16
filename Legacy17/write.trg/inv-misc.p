&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME inv-misc

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

FIND FIRST inv-head OF {&TABLENAME} NO-LOCK NO-ERROR.
IF AVAIL inv-head THEN RUN oe/oeinvup2.p (ROWID(inv-head), INPUT NO).

IF TRIM({&TABLENAME}.rec_key) NE "" THEN DO:
  {custom/coloraud.i}
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
