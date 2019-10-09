&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME box-design-line

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}


FOR EACH box-design-hdr OF {&TABLENAME}
    WHERE box-design-hdr.est-no NE ""
    NO-LOCK,
    FIRST est
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
