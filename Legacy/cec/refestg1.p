
DEF INPUT  PARAM ip-rowid       AS   ROWID  NO-UNDO.
DEF INPUT  PARAM ip-type        AS   CHAR   NO-UNDO.
DEF INPUT  PARAM ip-extent      AS   INT    NO-UNDO.
DEF OUTPUT PARAM op-ref-rec-qty AS   RECID  NO-UNDO.
DEF OUTPUT PARAM op-ref-rec-cst AS   RECID  NO-UNDO.

DEF TEMP-TABLE w-qty NO-UNDO FIELD w-qty AS INT.

DEF VAR v AS INT NO-UNDO.
DEF VAR ld-per-set AS DEC NO-UNDO.


FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL eb THEN
FIND FIRST ef OF eb NO-LOCK NO-ERROR.

ld-per-set = IF eb.est-type EQ 2 THEN eb.cust-%
             ELSE
             IF eb.est-type EQ 6 THEN eb.quantityPerSet
             ELSE 1.

IF ld-per-set LT 0 THEN ld-per-set = -1 / ld-per-set.

IF AVAIL ef THEN
FIND FIRST est
    WHERE est.company EQ ef.company
      AND est.est-no  EQ ef.est-no
    NO-LOCK NO-ERROR.
               
IF AVAIL est THEN DO:
  IF ip-type EQ "MAT" THEN DO:
    {cec/refestg1.i "MAT" ip-extent}
  END.
  ELSE DO:
    {cec/refestg1.i "LAB" ip-extent}
  END.
END.
