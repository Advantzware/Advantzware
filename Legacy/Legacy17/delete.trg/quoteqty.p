&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('NOSWEAT')
&Scoped-define TABLENAME quoteqty

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}


FIND FIRST quoteitm OF {&TABLENAME} NO-LOCK NO-ERROR.

IF AVAIL quoteitm THEN DO:
  FIND FIRST quotehd OF {&TABLENAME} NO-LOCK NO-ERROR.
  {delete.trg/quoteqty.i}
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
