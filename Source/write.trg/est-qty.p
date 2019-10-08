&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME est-qty

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.

/*DEF TEMP-TABLE tt-qty FIELD tt-qty AS INT INDEX tt-qty tt-qty.

DEF VAR li AS INT NO-UNDO.*/

{methods/triggers/write.i}


DISABLE TRIGGERS FOR LOAD OF est.
DISABLE TRIGGERS FOR LOAD OF est-op.

{&TABLENAME}.est-no = FILL(" ",8 - LENGTH(TRIM({&TABLENAME}.est-no))) +
                      TRIM({&TABLENAME}.est-no).

FIND FIRST est
    WHERE est.company EQ {&TABLENAME}.company
      AND est.est-no  EQ {&TABLENAME}.est-no
    NO-ERROR.

IF AVAIL est THEN DO:
  IF est.est-type EQ 1 THEN
  FOR EACH b-{&TABLENAME}
      WHERE b-{&TABLENAME}.company EQ est.company
        AND b-{&TABLENAME}.est-no  EQ est.est-no
        AND b-{&TABLENAME}.eqty    NE 0
        AND ROWID(b-{&TABLENAME})  NE ROWID({&TABLENAME})
      BREAK BY b-{&TABLENAME}.eqty
            BY b-{&TABLENAME}.rec_key:
    IF NOT FIRST-OF(b-{&TABLENAME}.eqty) THEN DELETE b-{&TABLENAME}.
  END.

  ASSIGN
   est.updated-date = TODAY
   est.updated-id   = USERID("nosweat")
   est.mod-date     = est.updated-date.

  IF (est.est-type EQ 1 OR est.est-type GE 5)                   AND
     {&TABLENAME}.eqty NE old-{&TABLENAME}.eqty                 AND
     old-{&TABLENAME}.eqty NE 0                                 AND
     NOT CAN-FIND(FIRST est-op
                  WHERE est-op.company EQ {&TABLENAME}.company
                    AND est-op.est-no  EQ {&TABLENAME}.est-no
                    AND est-op.qty     EQ {&TABLENAME}.eqty)    THEN
  FOR EACH est-op
      WHERE est-op.company EQ {&TABLENAME}.company
        AND est-op.est-no  EQ {&TABLENAME}.est-no
        AND est-op.qty     EQ old-{&TABLENAME}.eqty:
    est-op.qty = {&TABLENAME}.eqty.
  END.
END.

/*
IF AVAIL est AND est.est-type GE 5 AND est.est-type LE 6 THEN DO:
  DO li = 1 TO EXTENT({&TABLENAME}.qty):
    IF {&TABLENAME}.qty[li] GT 0 THEN DO:
      CREATE tt-qty.
      tt-qty = {&TABLENAME}.qty[li].
    END.
  END.

  ASSIGN
   {&TABLENAME}.qty = 0
   li               = 0.

  FOR EACH tt-qty WHERE tt-qty GT 0 BREAK BY tt-qty:
    IF FIRST-OF(tt-qty) THEN DO:
      li = li + 1.
      IF li LE EXTENT({&TABLENAME}.qty) THEN {&TABLENAME}.qty[li] = tt-qty.
    END.
  END.

  ASSIGN
   {&TABLENAME}.eqty = {&TABLENAME}.qty[1]
   est.est-qty[1]    = {&TABLENAME}.qty[1]
   est.est-qty[2]    = {&TABLENAME}.qty[2]
   est.est-qty[3]    = {&TABLENAME}.qty[3]
   est.est-qty[4]    = {&TABLENAME}.qty[4].

  FOR EACH eb
      WHERE eb.company EQ {&TABLENAME}.company
        AND eb.est-no  EQ {&TABLENAME}.est-no
        AND eb.eqty    NE {&TABLENAME}.eqty:
    eb.eqty = {&TABLENAME}.eqty.
  END.

  FOR EACH ef
      WHERE ef.company EQ {&TABLENAME}.company
        AND ef.est-no  EQ {&TABLENAME}.est-no
        AND ef.eqty    NE {&TABLENAME}.eqty:
    ef.eqty = {&TABLENAME}.eqty.
  END.
END.
*/

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
