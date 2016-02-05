&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME est

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF VAR lv-cust-no LIKE eb.cust-no NO-UNDO.

DEF BUFFER b-eb FOR eb.
DEF BUFFER b-est-op FOR est-op.


DISABLE TRIGGERS FOR LOAD OF eb.
DISABLE TRIGGERS FOR LOAD OF ef.
DISABLE TRIGGERS FOR LOAD OF est-op.
DISABLE TRIGGERS FOR LOAD OF b-eb.

{&TABLENAME}.est-no = FILL(" ",8 - LENGTH(TRIM({&TABLENAME}.est-no))) +
                      TRIM({&TABLENAME}.est-no).

IF {&TABLENAME}.est-type EQ ? THEN
  RUN est/resettyp.p (ROWID({&TABLENAME}), OUTPUT {&TABLENAME}.est-type).

IF {&TABLENAME}.est-type EQ 3 THEN {&TABLENAME}.est-type = 4.

FOR EACH est-op
    WHERE est-op.company EQ {&TABLENAME}.company
      AND est-op.est-no  EQ {&TABLENAME}.est-no
    EXCLUSIVE:

  IF est-op.line LT 500                                AND
     est-op.qty NE 0                                   AND
     CAN-FIND(FIRST b-est-op
              WHERE b-est-op.company EQ est-op.company
                AND b-est-op.est-no  EQ est-op.est-no
                AND b-est-op.line    LT 500
                AND b-est-op.qty     NE est-op.qty)    AND
     NOT CAN-FIND(FIRST est-qty
                  WHERE est-qty.company EQ est-op.company
                    AND est-qty.est-no  EQ est-op.est-no
                    AND est-qty.eqty    EQ est-op.qty) THEN DELETE est-op.

  ELSE DO:
    FIND FIRST mach
        WHERE mach.company EQ est-op.company
          AND mach.m-code  EQ est-op.m-code
        NO-LOCK NO-ERROR.

    IF AVAIL mach THEN est-op.op-sb = mach.p-type NE "B".
        
    IF est-op.op-sb THEN
      est-op.b-num = IF {&TABLENAME}.est-type EQ 5 THEN 1 ELSE 0.
     
    ELSE
    IF est-op.b-num EQ 0 THEN est-op.b-num = 1.
  END.
END.

RUN est/resetops.p (ROWID({&TABLENAME})).

FOR EACH ef OF {&TABLENAME} BREAK BY ef.eqty:
  IF FIRST-OF(ef.eqty) THEN DO:
    ASSIGN
     {&TABLENAME}.form-qty = 0
     ef.blank-qty          = 0.
    FOR EACH eb OF ef NO-LOCK:
      ef.blank-qty = ef.blank-qty + 1.
    END.
  END.
  {&TABLENAME}.form-qty = {&TABLENAME}.form-qty + 1.
END.

IF est.est-type NE 4 AND
   est.est-type NE 8 THEN
FOR EACH eb OF est WHERE eb.cust-no NE "" NO-LOCK:
  FOR EACH b-eb OF est WHERE ROWID(b-eb) NE ROWID(eb):
    b-eb.cust-no = eb.cust-no.
  END.
  LEAVE.
END.

IF {&TABLENAME}.entered-id EQ "" THEN
  ASSIGN
   {&TABLENAME}.entered-date = TODAY
   {&TABLENAME}.entered-id   = USERID("nosweat").
ELSE
  ASSIGN
   {&TABLENAME}.updated-date = TODAY
   {&TABLENAME}.updated-id   = USERID("nosweat")
   {&TABLENAME}.mod-date     = {&TABLENAME}.updated-date.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
