&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME ar-inv

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}


DISABLE TRIGGERS FOR LOAD OF ar-invl.

IF {&TABLENAME}.company NE "" AND {&TABLENAME}.inv-no NE 0 THEN
  RUN ar/checkinv#.p (BUFFER {&TABLENAME}).

FOR EACH ar-invl WHERE ar-invl.x-no EQ {&TABLENAME}.x-no:
  ar-invl.cust-no = {&TABLENAME}.cust-no.
END.

IF {&TABLENAME}.inv-date NE old-{&TABLENAME}.inv-date THEN
FOR EACH ar-ledger
    WHERE ar-ledger.company  EQ {&TABLENAME}.company
      AND ar-ledger.cust-no  EQ {&TABLENAME}.cust-no
      AND ar-ledger.ref-date EQ old-{&TABLENAME}.inv-date
      AND ar-ledger.ref-num  EQ "INV# " + STRING({&TABLENAME}.inv-no):
  ar-ledger.ref-date = {&TABLENAME}.inv-date.
END.

IF {&TABLENAME}.net     EQ ? THEN {&TABLENAME}.net     = 0.
IF {&TABLENAME}.gross   EQ ? THEN {&TABLENAME}.gross   = 0.
IF {&TABLENAME}.due     EQ ? THEN {&TABLENAME}.due     = 0.
IF {&TABLENAME}.tax-amt EQ ? THEN {&TABLENAME}.tax-amt = 0.
IF {&TABLENAME}.freight EQ ? THEN {&TABLENAME}.freight = 0.
IF {&TABLENAME}.t-comm  EQ ? THEN {&TABLENAME}.t-comm  = 0.
IF {&TABLENAME}.t-sales EQ ? THEN {&TABLENAME}.t-sales = 0.
IF {&TABLENAME}.t-cost  EQ ? THEN {&TABLENAME}.t-cost  = 0.
IF {&TABLENAME}.t-disc  EQ ? THEN {&TABLENAME}.t-disc  = 0.
    
RELEASE cust.
FIND FIRST cust NO-LOCK
    WHERE cust.company EQ {&TABLENAME}.company
      AND cust.cust-no EQ {&TABLENAME}.cust-no
    NO-ERROR.
IF AVAIL cust THEN {&TABLENAME}.curr-code[1] = cust.curr-code.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

