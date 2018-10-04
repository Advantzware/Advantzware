&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
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

/*IF old-{&TABLENAME}.r-no NE 0 AND {&TABLENAME}.link-no EQ 0 THEN DO:
  FIND FIRST reftable
      WHERE reftable.reftable EQ "oe-rel.s-code"
        AND reftable.company  EQ STRING(oe-rel.r-no,"9999999999")
      NO-LOCK NO-ERROR.
  lv-s-code[1] = IF AVAIL reftable THEN reftable.code ELSE "B".

  FOR EACH b-{&TABLENAME}
      WHERE b-{&TABLENAME}.company  EQ {&TABLENAME}.company
        AND b-{&TABLENAME}.ord-no   EQ {&TABLENAME}.ord-no
        AND b-{&TABLENAME}.i-no     EQ {&TABLENAME}.i-no
        AND b-{&TABLENAME}.line     EQ {&TABLENAME}.line
        AND b-{&TABLENAME}.po-no    EQ {&TABLENAME}.po-no
        AND b-{&TABLENAME}.ship-id  EQ {&TABLENAME}.ship-id
        AND b-{&TABLENAME}.rel-date EQ {&TABLENAME}.rel-date
        AND b-{&TABLENAME}.carrier  EQ {&TABLENAME}.carrier
        AND b-{&TABLENAME}.qty      EQ {&TABLENAME}.qty
        AND b-{&TABLENAME}.link-no  EQ 0
        AND ROWID(b-{&TABLENAME})   NE ROWID({&TABLENAME})
      USE-INDEX ord-item:

    FIND FIRST reftable
        WHERE reftable.reftable EQ "oe-rel.s-code"
          AND reftable.company  EQ STRING(b-oe-rel.r-no,"9999999999")
        NO-LOCK NO-ERROR.
    lv-s-code[2] = IF AVAIL reftable THEN reftable.code ELSE "B".

    IF lv-s-code[1] EQ lv-s-code[2] THEN DELETE b-{&TABLENAME}.
  END.
END.*/

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
  

IF PROGRAM-NAME(2) NE ?                AND
   {&TABLENAME}.r-no NE 0              AND
   INDEX("SIL",{&TABLENAME}.stat) GT 0 AND
   NOT CAN-FIND(FIRST oe-rel-audit
                WHERE oe-rel-audit.reftable EQ "oe-rel-audit"
                  AND oe-rel-audit.company  EQ STRING({&TABLENAME}.r-no,"9999999999")
                USE-INDEX reftable)
THEN DO:
  CREATE oe-rel-audit.
  ASSIGN
   oe-rel-audit.reftable = "oe-rel-audit"
   oe-rel-audit.company  = STRING({&TABLENAME}.r-no,"9999999999")
   oe-rel-audit.loc      = USERID("nosweat")
   oe-rel-audit.code     = STRING(TODAY,"99/99/9999")
   oe-rel-audit.code2    = STRING(TIME,"99999")
   oe-rel-audit.val[1]   = {&TABLENAME}.qty
   oe-rel-audit.val[2]   = {&TABLENAME}.tot-qty
   oe-rel-audit.dscr     = PROGRAM-NAME(2).
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

IF NOT CAN-FIND(FIRST reftable WHERE
   reftable.reftable EQ "oe-rel.lot-no" AND
   reftable.company  EQ STRING(oe-rel.r-no,"9999999999")) THEN
   DO:
      CREATE reftable.
      ASSIGN
         reftable.reftable = "oe-rel.lot-no"
         reftable.company = STRING(oe-rel.r-no,"9999999999").
      RELEASE reftable.
   END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
