
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEF VAR lv-die-in LIKE ef.die-in NO-UNDO.


FIND ef WHERE ROWID(ef) EQ ip-rowid NO-ERROR.

IF AVAIL ef THEN DO:
  cocode = ef.company.

  FOR EACH eb
      WHERE eb.company EQ ef.company
        AND eb.est-no  EQ ef.est-no
        AND eb.form-no EQ ef.form-no
      NO-LOCK:
    lv-die-in = lv-die-in + IF eb.die-in NE ? THEN eb.die-in ELSE 0.
  END.

  FOR EACH est-flm
      WHERE est-flm.company EQ ef.company
        AND est-flm.est-no  EQ ef.est-no
        AND est-flm.snum    EQ ef.form-no
      NO-LOCK:

    IF est-flm.bnum NE 0 AND est-flm.wid NE 0 AND est-flm.len NE 0 AND
       CAN-FIND(FIRST ITEM
                {sys/look/itemW.i}
                  AND item.i-no     EQ est-flm.i-no
                  AND item.mat-type EQ "W") THEN
    FOR EACH eb
        WHERE eb.company  EQ ef.company
          AND eb.est-no   EQ ef.est-no
          AND eb.form-no  EQ ef.form-no
          AND eb.blank-no EQ est-flm.bnum
        NO-LOCK:

      lv-die-in = lv-die-in + ((est-flm.wid + est-flm.len) * 2 * eb.num-up).
      LEAVE.
    END.
  END.

  IF lv-die-in NE 0 THEN ef.die-in = lv-die-in.

  IF ef.die-in EQ ? THEN ef.die-in = 0.
END.
