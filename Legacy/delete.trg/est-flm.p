&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME est-flm

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}


FOR EACH ef
    WHERE ef.company EQ {&TABLENAME}.company
      AND ef.est-no  EQ {&TABLENAME}.est-no
      AND ef.eqty    EQ {&TABLENAME}.eqty
      AND ef.form-no EQ {&TABLENAME}.snum
    NO-LOCK:
  RUN est/upd-leaf.p (ROWID(ef)).
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
