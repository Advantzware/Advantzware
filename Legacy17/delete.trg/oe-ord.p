&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME oe-ord

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

DEF BUFFER oe-ord-close-checked FOR reftable.
DEF BUFFER oe-ord-whs-order FOR reftable.
DEFINE BUFFER bf-oe-ord FOR oe-ord .


/* not delete if estimate exists */
FIND est WHERE est.rec_key EQ oe-ord.rec_key NO-LOCK NO-ERROR.
IF NOT AVAIL est THEN DO:
  {methods/triggers/delete.i}
END.

IF {&TABLENAME}.ord-no NE 0 THEN
FOR EACH eb
    WHERE eb.company EQ {&TABLENAME}.company
      AND eb.ord-no  EQ {&TABLENAME}.ord-no
    USE-INDEX ord-no:
  eb.ord-no = 0. 
END.

FOR EACH oe-ord-close-checked
    WHERE oe-ord-close-checked.reftable EQ "oe-ord.close-checked"
      AND oe-ord-close-checked.company  EQ STRING({&TABLENAME}.company,"x(10)") +
                                           STRING({&TABLENAME}.ord-no,"9999999999"):
  DELETE oe-ord-close-checked.
END.

FOR EACH oe-ord-whs-order
    WHERE oe-ord-whs-order.reftable EQ "oe-ord.whs-order"
      AND oe-ord-whs-order.company  EQ {&TABLENAME}.company
      AND oe-ord-whs-order.loc      EQ STRING({&TABLENAME}.ord-no,"9999999999"):
  DELETE oe-ord-whs-order.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
    
