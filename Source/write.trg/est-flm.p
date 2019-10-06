&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME est-flm

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.


DISABLE TRIGGERS FOR LOAD OF est.

{&TABLENAME}.est-no = FILL(" ",8 - LENGTH(TRIM({&TABLENAME}.est-no))) +
                      TRIM({&TABLENAME}.est-no).

FIND FIRST est
    WHERE est.company EQ {&TABLENAME}.company
      AND est.est-no  EQ {&TABLENAME}.est-no
    NO-ERROR.

IF AVAIL est THEN DO:
  ASSIGN
   est.updated-date = TODAY
   est.updated-id   = USERID("nosweat")
   est.mod-date     = est.updated-date.
END.

IF {&TABLENAME}.i-no NE "" AND {&TABLENAME}.snum NE 0 THEN DO:
  FOR EACH ef
      WHERE ef.company EQ {&TABLENAME}.company
        AND ef.est-no  EQ {&TABLENAME}.est-no
        AND ef.form-no EQ {&TABLENAME}.snum
      NO-LOCK:
    RUN est/upd-leaf.p (ROWID(ef)).
  END.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
