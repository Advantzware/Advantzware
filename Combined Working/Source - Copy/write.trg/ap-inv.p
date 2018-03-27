&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME ap-inv

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.

{sys/inc/var.i NEW SHARED}

{custom/globdefs.i}

ASSIGN
 cocode = g_company
 locode = g_loc.

ASSIGN
 {&TABLENAME}.user-id  = USERID("nosweat")
 {&TABLENAME}.upd-date = TODAY
 {&TABLENAME}.upd-time = TIME.

IF {&TABLENAME}.inv-no NE ""                          AND
   {&TABLENAME}.due-date NE old-{&TABLENAME}.due-date THEN
FOR EACH ap-payl
    WHERE ap-payl.inv-no  EQ {&TABLENAME}.inv-no
      AND ap-payl.vend-no EQ {&TABLENAME}.vend-no
    USE-INDEX inv-no,
    FIRST ap-pay
    WHERE ap-pay.company EQ ap-inv.company
      AND ap-pay.c-no    EQ ap-payl.c-no
    USE-INDEX c-no NO-LOCK:
  ap-payl.due-date = {&TABLENAME}.due-date.
END.

IF {&TABLENAME}.inv-date NE old-{&TABLENAME}.inv-date THEN
FOR EACH ap-ledger
    WHERE ap-ledger.company  EQ {&TABLENAME}.company
      AND ap-ledger.vend-no  EQ {&TABLENAME}.vend-no
      AND ap-ledger.ref-date EQ old-{&TABLENAME}.inv-date
      AND ap-ledger.refnum   EQ "INV# " + ap-inv.inv-no:
  ap-ledger.ref-date = {&TABLENAME}.inv-date.
END.

RELEASE vend.
FIND FIRST vend NO-LOCK
    WHERE vend.company EQ {&TABLENAME}.company
      AND vend.vend-no EQ {&TABLENAME}.vend-no
    NO-ERROR.
IF AVAIL vend THEN DO:
  ap-inv.curr-code[1] = vend.curr-code.
  IF {&TABLENAME}.posted NE old-{&TABLENAME}.posted THEN
    RUN ap/vendobal.p (ROWID(vend)).
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
