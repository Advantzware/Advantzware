&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME stax

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.

DEF VAR li AS INT NO-UNDO.

/* which is the master?  Tax Code or Tax Code1? */
find first reftable no-lock where reftable.reftable = "v10-TaxCode-Upgrade"
                              and reftable.code     = "10 Extents" no-error.
if available(reftable) then DO li = 1 to extent({&TABLENAME}.tax-code): /* synch original with new */
    Assign {&TABLENAME}.tax-code[li] = {&TABLENAME}.tax-code1[li]
           {&TABLENAME}.tax-dscr[li] = {&TABLENAME}.tax-dscr1[li]
           {&TABLENAME}.tax-rate[li] = {&TABLENAME}.tax-rate1[li]
           {&TABLENAME}.tax-acc[li]  = {&TABLENAME}.tax-acc1[li]
           {&TABLENAME}.tax-frt[li]  = {&TABLENAME}.tax-frt1[li].
end.
else DO li = 1 to extent({&TABLENAME}.tax-code): /* synch new with original */
    Assign {&TABLENAME}.tax-code1[li] = {&TABLENAME}.tax-code[li]
           {&TABLENAME}.tax-dscr1[li] = {&TABLENAME}.tax-dscr[li]
           {&TABLENAME}.tax-rate1[li] = {&TABLENAME}.tax-rate[li]
           {&TABLENAME}.tax-acc1[li]  = {&TABLENAME}.tax-acc[li]
           {&TABLENAME}.tax-frt1[li]  = {&TABLENAME}.tax-frt[li].
end.

IF {&TABLENAME}.tax-group   EQ {&TABLENAME}.tax-code[1] AND
   {&TABLENAME}.tax-code[1] NE ""                       THEN
FOR EACH b-{&TABLENAME}
    WHERE b-{&TABLENAME}.company   EQ {&TABLENAME}.company
      AND b-{&TABLENAME}.tax-group NE {&TABLENAME}.tax-code[1]
      AND ROWID(b-{&TABLENAME})    NE ROWID({&TABLENAME}):
  DO li = 1 TO EXTENT({&TABLENAME}.tax-code):
    IF b-{&TABLENAME}.tax-code[li] NE ""                       AND
       b-{&TABLENAME}.tax-code[li] EQ {&TABLENAME}.tax-code[1] THEN
      ASSIGN
       b-{&TABLENAME}.tax-dscr[li] = {&TABLENAME}.tax-dscr[1]
       b-{&TABLENAME}.tax-rate[li] = {&TABLENAME}.tax-rate[1]
       b-{&TABLENAME}.tax-acc[li]  = {&TABLENAME}.tax-acc[1]
       b-{&TABLENAME}.tax-frt[li]  = {&TABLENAME}.tax-frt[1].
  END.
  DO li = 1 TO EXTENT({&TABLENAME}.tax-code1):
    IF b-{&TABLENAME}.tax-code1[li] NE ""                       AND
       b-{&TABLENAME}.tax-code1[li] EQ {&TABLENAME}.tax-code1[1] THEN
      ASSIGN
       b-{&TABLENAME}.tax-dscr1[li] = {&TABLENAME}.tax-dscr1[1]
       b-{&TABLENAME}.tax-rate1[li] = {&TABLENAME}.tax-rate1[1]
       b-{&TABLENAME}.tax-acc1[li]  = {&TABLENAME}.tax-acc1[1]
       b-{&TABLENAME}.tax-frt1[li]  = {&TABLENAME}.tax-frt1[1].
  END.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
