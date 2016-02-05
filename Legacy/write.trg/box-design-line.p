&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME box-design-line

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}


FOR EACH box-design-hdr OF {&TABLENAME}
    WHERE box-design-hdr.est-no NE ""
    NO-LOCK,
    FIRST est
    WHERE est.company EQ {&TABLENAME}.company
      AND est.est-no  EQ {&TABLENAME}.est-no:

  {&TABLENAME}.est-no = FILL(" ",8 - LENGTH(TRIM({&TABLENAME}.est-no))) +
                        TRIM({&TABLENAME}.est-no).

  ASSIGN
   est.updated-date = TODAY
   est.updated-id   = USERID("nosweat").
  LEAVE.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
