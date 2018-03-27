&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME oe-rel

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

{sys/inc/var.i NEW SHARED}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEF BUFFER oe-rel-audit FOR reftable.

DEF VAR lv-s-code LIKE oe-rell.s-code EXTENT 2 NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF oe-ord.
DISABLE TRIGGERS FOR LOAD OF oe-rell.
DISABLE TRIGGERS FOR LOAD OF oe-boll.
DISABLE TRIGGERS FOR LOAD OF oe-relh.
DISABLE TRIGGERS FOR LOAD OF oe-bolh.
DISABLE TRIGGERS FOR LOAD OF b-{&TABLENAME}.
DISABLE TRIGGERS FOR LOAD OF oe-rel-audit.

cocode = {&TABLENAME}.company.

IF {&TABLENAME}.cases EQ ? THEN {&TABLENAME}.cases = 0.

IF {&TABLENAME}.r-no    NE 0 AND
   {&TABLENAME}.link-no EQ 0 THEN
DO:
   FOR EACH oe-rell
      WHERE oe-rell.company  EQ {&TABLENAME}.company
        AND oe-rell.ord-no   EQ {&TABLENAME}.ord-no
        AND oe-rell.i-no     EQ {&TABLENAME}.i-no
        AND oe-rell.line     EQ {&TABLENAME}.line
        AND oe-rell.posted   EQ YES
        AND oe-rell.link-no  EQ {&TABLENAME}.r-no
      USE-INDEX ord-no NO-LOCK:
      ASSIGN
      {&TABLENAME}.rel-no   = oe-rell.rel-no
      {&TABLENAME}.b-ord-no = oe-rell.b-ord-no
      {&TABLENAME}.po-no    = oe-rell.po-no
      {&TABLENAME}.link-no  = oe-rell.r-no.
      LEAVE.
   END.

   FOR EACH oe-rell
      WHERE oe-rell.company  EQ {&TABLENAME}.company
        AND oe-rell.ord-no   EQ {&TABLENAME}.ord-no
        AND oe-rell.i-no     EQ {&TABLENAME}.i-no
        AND oe-rell.line     EQ {&TABLENAME}.line
        AND oe-rell.posted   EQ NO
        AND oe-rell.link-no  EQ {&TABLENAME}.r-no
      USE-INDEX ord-no NO-LOCK:

      FIND FIRST truck-run-print WHERE
           truck-run-print.company EQ oe-rell.company AND
           truck-run-print.oe-rel-r-no EQ {&TABLENAME}.r-no   
           EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL truck-run-print THEN
      DO:
         ASSIGN
         truck-run-print.rel-no   = oe-rell.rel-no
         truck-run-print.b-ord-no = oe-rell.b-ord-no
         truck-run-print.po-no    = oe-rell.po-no
         truck-run-print.link-no  = oe-rell.r-no.
        
         RELEASE truck-run-print.
      END.

      LEAVE.
   END.
END.

IF {&TABLENAME}.link-no EQ 0                    AND
   {&TABLENAME}.po-no NE old-{&TABLENAME}.po-no THEN
FOR EACH oe-rell
    WHERE oe-rell.company  EQ old-{&TABLENAME}.company
      AND oe-rell.ord-no   EQ old-{&TABLENAME}.ord-no
      AND oe-rell.i-no     EQ old-{&TABLENAME}.i-no
      AND oe-rell.line     EQ old-{&TABLENAME}.line
      AND oe-rell.rel-no   EQ old-{&TABLENAME}.rel-no
      AND oe-rell.b-ord-no EQ old-{&TABLENAME}.b-ord-no
      AND oe-rell.po-no    EQ old-{&TABLENAME}.po-no
    USE-INDEX ord-no,
    FIRST oe-relh
    WHERE oe-relh.r-no    EQ oe-rell.r-no
      AND oe-relh.posted  EQ NO
      AND oe-relh.deleted EQ NO
    NO-LOCK:

  oe-rell.po-no = {&TABLENAME}.po-no.
END.

{sys/inc/oeuserid.i}



    /* Per Joe, don't calculate if it's on this list */
IF NOT INDEX("CZPAB",{&TABLENAME}.stat) GT 0 THEN
  RUN oe/rel-stat.p (ROWID({&TABLENAME}), OUTPUT {&TABLENAME}.stat).
/* Out Per Joe
IF old-{&TABLENAME}.ord-no NE 0         AND
   INDEX("SIL ",{&TABLENAME}.stat) GT 0 THEN DO:
    {&TABLENAME}.qty = {&TABLENAME}.tot-qty.
  /* RUN fg/fgitmloc.p (INPUT {&TABLENAME}.i-no, INPUT ROWID({&TABLENAME})). */
END.
*/
  
IF old-{&TABLENAME}.ord-no = 0         AND
   INDEX("SIL ",{&TABLENAME}.stat) = 0 AND
 {&TABLENAME}.tot-qty EQ 0 THEN DO:
    {&TABLENAME}.tot-qty = {&TABLENAME}.qty.
 /*   RUN fg/fgitmloc.p (INPUT {&TABLENAME}.i-no, INPUT ROWID({&TABLENAME})). */
END.  

FOR EACH oe-ord
    WHERE oe-ord.company EQ {&TABLENAME}.company
      AND oe-ord.ord-no  EQ {&TABLENAME}.ord-no:

  IF oeuserid-log AND oe-ord.user-id NE USERID("nosweat") THEN
    oe-ord.user-id = USERID("nosweat").

  {&TABLENAME}.cust-no = oe-ord.cust-no.

  FIND oe-ordl OF oe-ord
      WHERE oe-ordl.i-no EQ {&TABLENAME}.i-no
      NO-LOCK NO-ERROR.
  IF AVAIL oe-ordl THEN {&TABLENAME}.line = oe-ordl.line.

  LEAVE.
END.



/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
