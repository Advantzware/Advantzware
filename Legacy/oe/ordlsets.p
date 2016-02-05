
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-term LIKE report.term-id NO-UNDO.
DEF OUTPUT PARAM op-sets AS DEC DECIMALS 10 NO-UNDO.

DEF BUFFER b-oe-ordl FOR oe-ordl.

DEF VAR ld-sets LIKE op-sets NO-UNDO.
DEF VAR ld-inv LIKE op-sets NO-UNDO.
DEF VAR ld-pset AS DEC NO-UNDO.


FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-ordl THEN
FOR EACH b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}
    NO-LOCK
    BREAK BY b-oe-ordl.line:

  RUN oe/ordlsqty.p (ROWID(b-oe-ordl),
                     OUTPUT ld-inv,
                     OUTPUT ld-sets).

  FOR EACH oe-boll
      WHERE oe-boll.company EQ b-oe-ordl.company
        AND oe-boll.ord-no  EQ b-oe-ordl.ord-no
        AND oe-boll.i-no    EQ b-oe-ordl.i-no
        AND oe-boll.line    EQ b-oe-ordl.line
        AND CAN-FIND(FIRST oe-bolh
                     WHERE oe-bolh.b-no   EQ oe-boll.b-no
                       AND oe-bolh.posted EQ NO
                     USE-INDEX b-no)
        AND CAN-FIND(FIRST report
                     WHERE report.term-id EQ ip-term
                       AND report.rec-id  EQ RECID(oe-boll))
      USE-INDEX ord-no NO-LOCK:
    ld-sets = ld-sets + oe-boll.qty.
  END.

  ld-pset = b-oe-ordl.qty / oe-ordl.qty.
   
  IF ld-pset EQ ? THEN ld-pset = 0.

  ld-sets = ld-sets / ld-pset.

  IF ld-sets EQ ? THEN ld-sets = 0.

  IF FIRST(b-oe-ordl.line) OR ld-sets LT op-sets THEN op-sets = ld-sets.
END.
