&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME ar-cashl

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}


FOR EACH ar-cash WHERE ar-cash.c-no EQ {&TABLENAME}.c-no:
  {&TABLENAME}.cust-no = ar-cash.cust-no.
  LEAVE.
END.

IF {&TABLENAME}.inv-no NE 0 THEN
FOR EACH ar-inv
    WHERE ar-inv.company EQ {&TABLENAME}.company
      AND ar-inv.inv-no  EQ {&TABLENAME}.inv-no
    NO-LOCK:

  IF {&TABLENAME}.inv-date EQ ? THEN
    {&TABLENAME}.inv-date = ar-inv.inv-date.

  IF {&TABLENAME}.amt-due  EQ 0                       AND
     {&TABLENAME}.inv-no   NE old-{&TABLENAME}.inv-no THEN
    {&TABLENAME}.amt-due = ar-inv.due.

  LEAVE.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
