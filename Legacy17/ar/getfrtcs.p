
DEF INPUT  PARAM ip-rowid AS   ROWID NO-UNDO.
DEF INPUT  PARAM ip-qty   LIKE ar-invl.inv-qty NO-UNDO.
DEF OUTPUT PARAM op-cost  AS   DEC DECIMALS 10 NO-UNDO.

DEF VAR ld-qty AS DEC NO-UNDO.
DEF VAR ld-wgt AS DEC EXTENT 2 NO-UNDO.
DEF VAR ld-cst AS DEC EXTENT 2 NO-UNDO.


FIND ar-invl WHERE ROWID(ar-invl) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL ar-invl THEN DO:
  FIND ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK NO-ERROR.

  RELEASE ar-invl.

  IF AVAIL ar-inv AND ar-inv.f-bill AND ar-inv.freight NE 0 THEN DO:
    FOR EACH ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no NO-LOCK,
        FIRST itemfg
        WHERE itemfg.company EQ ar-invl.company
          AND itemfg.i-no    EQ ar-invl.i-no
        NO-LOCK:
      
      ld-wgt[2] = ld-wgt[2] + (ar-invl.inv-qty / 100 * itemfg.weight-100).

      IF ROWID(ar-invl) EQ ip-rowid THEN
        ld-wgt[1] = ip-qty / 100 * itemfg.weight-100.
    END.

    IF ld-wgt[1] NE 0 AND ld-wgt[1] NE ? AND
       ld-wgt[2] NE 0 AND ld-wgt[2] NE ? THEN
      op-cost = ar-inv.freight * (ld-wgt[1] / ld-wgt[2]).
  END.
END.
