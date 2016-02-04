&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME ap-payl

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}


FOR EACH ap-pay WHERE ap-pay.c-no EQ {&TABLENAME}.c-no NO-LOCK:
  IF ap-pay.vend-no NE ""                                      AND
     NOT CAN-FIND(FIRST ap-inv
                  WHERE ap-inv.company EQ ap-pay.company
                    AND ap-inv.vend-no EQ {&TABLENAME}.vend-no
                    AND ap-inv.inv-no  EQ {&TABLENAME}.inv-no) THEN
    {&TABLENAME}.vend-no = ap-pay.vend-no.

  IF {&TABLENAME}.inv-no NE "" THEN
  FOR EACH ap-inv
      WHERE ap-inv.company EQ ap-pay.company
        AND ap-inv.vend-no EQ ap-payl.vend-no
        AND ap-inv.inv-no  EQ ap-payl.inv-no
      NO-LOCK:
    {&TABLENAME}.due-date = ap-inv.due-date.
  END.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
