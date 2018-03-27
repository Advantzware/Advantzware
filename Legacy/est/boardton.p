
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-tons AS DEC DECIMALS 10 NO-UNDO.

DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR ld-qty AS DEC NO-UNDO.
DEF VAR ld-set AS DEC NO-UNDO.

  
FIND FIRST ef WHERE ROWID(ef) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL ef THEN DO:
  cocode = ef.company.

  {ce/msfcalc.i}

  FIND FIRST est NO-LOCK
      WHERE est.company EQ ef.company
        AND est.est-no  EQ ef.est-no
      NO-ERROR.

  IF AVAIL est THEN
  FOR EACH eb NO-LOCK
      WHERE eb.company EQ ef.company
        AND eb.est-no  EQ ef.est-no
        AND eb.form-no EQ ef.form-no:

    IF eb.est-type EQ 1 OR eb.est-type EQ 5 THEN
      ld-qty = est.est-qty[1].

    ELSE
    IF eb.est-type EQ 2 OR eb.est-type EQ 6 THEN DO:
      ASSIGN
       ld-set = IF eb.est-type LE 4 THEN eb.cust-% ELSE eb.quantityPerSet
       ld-qty = est.est-qty[1] *
                (IF ld-set LT 0 THEN -1 / ld-set ELSE ld-set).
    END.

    ELSE ld-qty = eb.yld-qty.

    op-tons = op-tons + (ld-qty * eb.t-wid * eb.t-len).
  END.

  op-tons = (IF v-corr THEN (op-tons * .007) ELSE (op-tons / 144)) / 1000 *
            ef.weight / 2000.
END.
