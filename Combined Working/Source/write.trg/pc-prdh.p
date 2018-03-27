&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME pc-prdh

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.


/*IF {&TABLENAME}.m-code      NE ""                               AND
   old-{&TABLENAME}.m-code  NE ""                               AND
   ({&TABLENAME}.m-code     NE old-{&TABLENAME}.m-code     OR
    {&TABLENAME}.trans-date NE old-{&TABLENAME}.trans-date OR
    {&TABLENAME}.shift      NE old-{&TABLENAME}.shift)          THEN
FOR EACH pc-prdd
    WHERE pc-prdd.company EQ old-{&TABLENAME}.company
      AND pc-prdd.m-code  EQ old-{&TABLENAME}.m-code
      AND pc-prdd.op-date EQ old-{&TABLENAME}.trans-date
      AND pc-prdd.shift   EQ old-{&TABLENAME}.shift:
  ASSIGN
   pc-prdd.m-code  = {&TABLENAME}.m-code
   pc-prdd.op-date = {&TABLENAME}.trans-date
   pc-prdd.shift   = {&TABLENAME}.shift.
END.*/


IF {&TABLENAME}.m-code NE "" THEN
   FOR EACH b-{&TABLENAME}
       WHERE b-{&TABLENAME}.company    EQ {&TABLENAME}.company
         AND b-{&TABLENAME}.m-code     EQ {&TABLENAME}.m-code
         AND b-{&TABLENAME}.trans-date EQ {&TABLENAME}.trans-date
         AND b-{&TABLENAME}.shift      EQ {&TABLENAME}.shift
         AND ROWID(b-{&TABLENAME})     NE ROWID({&TABLENAME}):
     DELETE b-{&TABLENAME}.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
