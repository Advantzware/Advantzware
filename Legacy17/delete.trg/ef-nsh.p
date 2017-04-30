&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME ef-nsh
TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.


DISABLE TRIGGERS FOR LOAD OF b-{&TABLENAME}.

FIND FIRST ef OF {&TABLENAME} NO-LOCK NO-ERROR.

IF AVAIL ef THEN
FOR EACH b-{&TABLENAME} OF ef
    WHERE b-{&TABLENAME}.orig-no EQ {&TABLENAME}.pass-no
      AND ROWID(b-{&TABLENAME})  NE ROWID({&TABLENAME}):
  DELETE b-{&TABLENAME}.
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
