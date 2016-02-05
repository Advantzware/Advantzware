
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT  PARAM ip-qty   AS DEC NO-UNDO.
DEF OUTPUT PARAM op-cost  LIKE fg-rctd.frt-cost NO-UNDO.

DEF VAR ld-qty AS DEC NO-UNDO.
DEF VAR ld-wgt AS DEC EXTENT 2 NO-UNDO.
DEF VAR ld-cst AS DEC EXTENT 2 NO-UNDO.
DEF VAR fg-uom-list AS CHAR NO-UNDO.
DEF VAR uom-list AS CHAR NO-UNDO.

FIND po-ordl WHERE ROWID(po-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL po-ordl THEN DO:
  FIND FIRST po-ord WHERE
       po-ord.company EQ po-ordl.company AND
       po-ord.po-no EQ po-ordl.po-no
       NO-LOCK NO-ERROR.

  RELEASE po-ordl.

  IF AVAIL po-ord AND po-ord.t-freight GT 0 THEN DO:
    FOR EACH po-ordl WHERE
        po-ordl.company EQ po-ord.company AND
        po-ordl.po-no EQ po-ord.po-no NO-LOCK:
      IF NOT po-ordl.item-type THEN DO:
        RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

        FIND FIRST itemfg
            WHERE itemfg.company EQ po-ordl.company
              AND itemfg.i-no    EQ po-ordl.i-no
            NO-LOCK NO-ERROR.

        ld-qty = po-ordl.ord-qty.

        IF LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0 THEN
          RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",
                                 0, po-ordl.s-len, po-ordl.s-wid, 0,
                                 ld-qty, OUTPUT ld-qty).
      
        ld-wgt[2] = ld-wgt[2] + (ld-qty / 100 * itemfg.weight-100).

        IF ROWID(po-ordl) EQ ip-rowid THEN
          ld-wgt[1] = ip-qty / 100 * itemfg.weight-100.
      END.
      ELSE DO:

        FIND FIRST item
            WHERE item.company EQ po-ordl.company
              AND item.i-no    EQ po-ordl.i-no
            NO-LOCK NO-ERROR.
        run sys/ref/uom-rm.p  (item.mat-type, OUTPUT uom-list).

        ld-qty = po-ordl.ord-qty.

        IF LOOKUP(po-ordl.pr-qty-uom,uom-list) EQ 0 THEN
          RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",
                                 0, po-ordl.s-len, po-ordl.s-wid, 0,
                                 ld-qty, OUTPUT ld-qty).
      
        ld-wgt[2] = ld-wgt[2] + (ld-qty / 100 * item.weight-100).

        IF ROWID(po-ordl) EQ ip-rowid THEN
          ld-wgt[1] = ip-qty / 100 * item.weight-100.
      END.
    END.

    IF ld-wgt[1] NE 0 AND ld-wgt[1] NE ? AND
       ld-wgt[2] NE 0 AND ld-wgt[2] NE ? THEN
      op-cost = po-ord.t-freight * (ld-wgt[1] / ld-wgt[2]).
  END.
END.
