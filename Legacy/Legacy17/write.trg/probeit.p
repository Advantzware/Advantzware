&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME probeit

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEFINE BUFFER b-sys-ctrl FOR sys-ctrl.
DEFINE BUFFER b-probe FOR probe.
DEF VAR vmclean2 AS LOG NO-UNDO.

FIND FIRST eb
    WHERE eb.company  EQ {&TABLENAME}.company
      AND eb.est-no   EQ {&TABLENAME}.est-no
      AND eb.comm     NE 0
      AND eb.est-type LE 4
    NO-LOCK NO-ERROR.

FIND FIRST est
    WHERE est.company EQ {&TABLENAME}.company
      AND est.est-no  EQ {&TABLENAME}.est-no
    NO-LOCK.

find first b-sys-ctrl WHERE
     b-sys-ctrl.company eq {&TABLENAME}.company AND
     b-sys-ctrl.name    eq "SETPRINT"
     no-lock no-error.
  
IF AVAIL b-sys-ctrl THEN
   vmclean2 = b-sys-ctrl.char-fld eq "McLean".

FIND FIRST b-probe WHERE
     b-probe.company EQ {&TABLENAME}.company AND
     b-probe.est-no EQ {&TABLENAME}.est-no AND
     b-probe.LINE EQ {&TABLENAME}.LINE
     NO-LOCK NO-ERROR.

IF est.est-type EQ 6 AND AVAIL b-probe AND b-probe.set-chg NE 0 AND vmclean2 THEN
   ASSIGN
   {&TABLENAME}.net-profit   = ROUND(((1 - ({&TABLENAME}.full-cost / {&TABLENAME}.sell-price)) * 100) - b-probe.set-chg,2)
   {&TABLENAME}.gross-profit = ROUND(((1 - ({&TABLENAME}.fact-cost / {&TABLENAME}.sell-price)) * 100) - b-probe.set-chg,2).
ELSE
   ASSIGN
   {&TABLENAME}.net-profit   = ROUND((1 - ({&TABLENAME}.full-cost / {&TABLENAME}.sell-price)) * 100,2)
   {&TABLENAME}.gross-profit = ROUND((1 - ({&TABLENAME}.fact-cost / {&TABLENAME}.sell-price)) * 100,2).

IF AVAIL eb THEN
  ASSIGN
   {&TABLENAME}.net-profit   = {&TABLENAME}.net-profit   - eb.comm
   {&TABLENAME}.gross-profit = {&TABLENAME}.gross-profit - eb.comm.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
