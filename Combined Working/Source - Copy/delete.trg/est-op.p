&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME est-op

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.

DEF VAR li AS INT NO-UNDO.
DEF VAR ll-foam AS LOG NO-UNDO.



IF {&TABLENAME}.line LT 500 THEN DO:
  FIND FIRST ef NO-LOCK
      WHERE ef.company  EQ {&TABLENAME}.company
        AND ef.est-no   EQ {&TABLENAME}.est-no
        AND ef.form-no  EQ {&TABLENAME}.s-num
        AND ef.est-type GE 5
      NO-ERROR.
  IF AVAIL ef THEN RUN cec/isitfoam.p (ROWID(ef), OUTPUT ll-foam).

  FOR EACH b-{&TABLENAME}
      WHERE b-{&TABLENAME}.company EQ {&TABLENAME}.company
        AND b-{&TABLENAME}.est-no  EQ {&TABLENAME}.est-no
        AND b-{&TABLENAME}.line    LT 500
        AND (NOT ll-foam OR NOT CAN-DO("RC,DC",b-{&TABLENAME}.dept))
        AND ROWID(b-{&TABLENAME})  NE ROWID({&TABLENAME})
      BREAK BY b-{&TABLENAME}.qty
            BY b-{&TABLENAME}.s-num
            BY b-{&TABLENAME}.b-num
            BY b-{&TABLENAME}.dept
            BY b-{&TABLENAME}.line:
            
    IF FIRST-OF(b-{&TABLENAME}.dept) THEN li = 0.
    
    ASSIGN
     li                     = li + 1
     b-{&TABLENAME}.op-pass = li.
  END.
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
