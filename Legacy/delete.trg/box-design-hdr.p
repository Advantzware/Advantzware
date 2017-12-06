&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME box-design-hdr

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}


FOR EACH box-design-line
    WHERE box-design-line.design-no EQ {&TABLENAME}.design-no
      AND box-design-line.company   EQ {&TABLENAME}.company
      AND box-design-line.est-no    EQ {&TABLENAME}.est-no
      AND box-design-line.eqty      EQ {&TABLENAME}.eqty
      AND box-design-line.form-no   EQ {&TABLENAME}.form-no
      AND box-design-line.blank-no  EQ {&TABLENAME}.blank-no:
  DELETE box-design-line.
END.

IF {&TABLENAME}.est-no NE "" THEN
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
