&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME est-op

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-est FOR est.
    
DEF VAR li AS INT NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF est.
DISABLE TRIGGERS FOR LOAD OF reftable.

{&TABLENAME}.est-no = FILL(" ",8 - LENGTH(TRIM({&TABLENAME}.est-no))) +
                      TRIM({&TABLENAME}.est-no).
    
DO li = 1 TO EXTENT({&TABLENAME}.att-qty):
  IF {&TABLENAME}.att-type[li] EQ "" THEN {&TABLENAME}.att-qty[li] = 0.
END.

IF {&TABLENAME}.line LT 500 THEN DO:
  
  FIND FIRST est
      WHERE est.company EQ {&TABLENAME}.company
        AND est.est-no  EQ {&TABLENAME}.est-no
      NO-ERROR.

  IF AVAIL est THEN DO:
    FIND FIRST eb OF est NO-LOCK NO-ERROR.

    IF AVAIL eb THEN DO:

      IF est.est-type EQ 1 OR
         est.est-type EQ 5 OR
         est.est-type EQ 6 THEN DO:
        IF {&TABLENAME}.qty EQ 0 THEN {&TABLENAME}.qty = eb.eqty.
      END.

      ELSE
      IF {&TABLENAME}.qty EQ eb.eqty THEN {&TABLENAME}.qty = 0.
    END.

    ASSIGN
     est.updated-date = TODAY
     est.updated-id   = USERID("nosweat")
     est.mod-date     = est.updated-date.
  END.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
