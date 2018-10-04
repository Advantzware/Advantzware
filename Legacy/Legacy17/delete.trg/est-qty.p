&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME est-qty

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}


IF {&TABLENAME}.eqty NE 0 THEN
FOR EACH est-op
    WHERE est-op.company EQ {&TABLENAME}.company
      AND est-op.est-no  EQ {&TABLENAME}.est-no
      AND est-op.qty     EQ {&TABLENAME}.eqty:
  DELETE est-op.    
END.

FOR EACH est
    WHERE est.company EQ {&TABLENAME}.company
      AND est.est-no  EQ {&TABLENAME}.est-no:
  ASSIGN
   est.updated-date = TODAY
   est.updated-id   = USERID("nosweat").
  LEAVE.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
