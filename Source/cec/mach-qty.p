
DEF INPUT PARAM ip-rowid1 AS ROWID NO-UNDO.
DEF INPUT PARAM ip-rowid2 AS ROWID NO-UNDO.
DEF INPUT PARAM ip-on-f   AS INT   NO-UNDO.
DEF INPUT PARAM ip-len    AS DEC   NO-UNDO.
DEF INPUT PARAM ip-qty    AS DEC   NO-UNDO.
DEF OUTPUT PARAM op-qty   AS DEC   NO-UNDO.

DEF VAR lv-rc-seq LIKE dept.fc NO-UNDO.
DEF VAR ld-yld AS DEC NO-UNDO.
DEF VAR ll-tandem AS LOG NO-UNDO.

DEF BUFFER b-eb FOR eb.


FIND mach WHERE ROWID(mach) EQ ip-rowid1 NO-LOCK NO-ERROR.

IF AVAIL mach THEN
FIND eb WHERE ROWID(eb) EQ ip-rowid2 NO-LOCK NO-ERROR.

IF AVAIL eb THEN
FIND FIRST ef OF eb NO-LOCK NO-ERROR.

IF AVAIL ef THEN
FIND FIRST est OF ef NO-LOCK NO-ERROR.

IF AVAIL est THEN DO:
  IF eb.est-type EQ 8 THEN DO:
    ASSIGN
     ld-yld = 1
     ip-qty = 0.

    RUN ce/com/istandem.p (ROWID(est), OUTPUT ll-tandem).

    IF ll-tandem THEN
    FOR EACH b-eb NO-LOCK
        WHERE b-eb.company EQ est.company
          AND b-eb.est-no  EQ est.est-no:
      ip-qty = ip-qty + eb.bl-qty.
    END.

    ELSE ip-qty = eb.bl-qty.
  END.

  ELSE
  IF mach.p-type EQ "A" THEN ld-yld = 1.

  ELSE
  IF mach.p-type EQ "P" THEN
  FOR EACH b-eb
      WHERE b-eb.company EQ est.company
        AND b-eb.est-no  EQ est.est-no
        AND b-eb.form-no NE 0
      NO-LOCK:
    ld-yld = ld-yld +
             (IF b-eb.quantityPerSet LT 0 THEN (-1 / b-eb.quantityPerSet) ELSE b-eb.quantityPerSet).
  END.

  ELSE ld-yld = IF eb.quantityPerSet LT 0 THEN (-1 / eb.quantityPerSet) ELSE eb.quantityPerSet.

  RUN est/rc-seq.p (OUTPUT lv-rc-seq).

  IF ip-qty EQ 0 THEN ip-qty = est.est-qty[1].

  op-qty = ip-qty * ld-yld.

  IF INDEX("AB",mach.p-type) LE 0 THEN DO:
    op-qty = op-qty / eb.num-up.

    IF lv-rc-seq LT mach.d-seq THEN op-qty = op-qty / (ef.n-out * ef.n-out-l).
    ELSE
    IF mach.dept[1] EQ "RC" THEN op-qty = op-qty / ip-on-f.

    IF (mach.p-type EQ "R" OR mach.dept[1] EQ "LM") AND mach.therm THEN
      op-qty = op-qty * (ip-len / 12).

    {sys/inc/roundup.i op-qty}
  END.
END.
