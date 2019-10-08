
DEF INPUT  PARAM ip-type     AS   INT NO-UNDO.
DEF INPUT  PARAM ip-rowid    AS   ROWID NO-UNDO.
DEF INPUT  PARAM ip-job-no   LIKE job.job-no NO-UNDO.
DEF INPUT  PARAM ip-job-no2  LIKE job.job-no2 NO-UNDO.
DEF INPUT  PARAM ip-qty      AS   INT NO-UNDO.
DEF OUTPUT PARAM op-cost     AS   DEC NO-UNDO.

DEF VAR lv-uom LIKE itemfg.prod-uom NO-UNDO.
DEF VAR v-bol-no LIKE oe-boll.bol-no NO-UNDO.

v-bol-no = 0.
FIND ar-invl WHERE ROWID(ar-invl) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL ar-invl THEN DO:
  v-bol-no = ar-invl.bol-no.
  FIND fg-ctrl WHERE fg-ctrl.company EQ ar-invl.company NO-LOCK NO-ERROR.

  FIND FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ ar-invl.company
        AND itemfg.i-no    EQ ar-invl.i-no
      NO-ERROR.

  IF ip-type EQ 3 AND ar-invl.t-cost NE ? THEN
  DO:
    IF ar-invl.dscr[1] EQ "M" OR ar-invl.dscr[1] EQ "" THEN
       op-cost = ip-qty / 1000 * ar-invl.cost.
    ELSE
       op-cost = ip-qty * ar-invl.cost.
  END.

  IF ip-type EQ 2 THEN DO:
    FIND FIRST oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ ar-invl.company
          AND oe-ordl.ord-no  EQ ar-invl.ord-no
          AND oe-ordl.i-no    EQ ar-invl.i-no
        NO-ERROR.
    IF AVAIL oe-ordl THEN op-cost = oe-ordl.cost * ip-qty / 1000.
  END.

  IF ip-type EQ 1 THEN
    RUN sys/inc/bordcost.p (ip-job-no,
                            ip-job-no2,                            
                            ar-invl.i-no,
                            v-bol-no,
                            ip-qty,
                            YES,
                            OUTPUT op-cost).
  IF ip-type EQ 4 THEN
    RUN sys/inc/bordcostM.p (ip-job-no,
                            ip-job-no2,                            
                            ar-invl.i-no,
                            v-bol-no,
                            ip-qty,
                            YES,
                            OUTPUT op-cost).

  IF op-cost EQ ? THEN op-cost = 0.

  IF op-cost EQ 0 THEN DO:
    FIND FIRST po-ordl NO-LOCK
        WHERE po-ordl.company   EQ ar-invl.company
          AND po-ordl.po-no     EQ ar-invl.po-no-po
          AND po-ordl.i-no      EQ ar-invl.i-no
          AND po-ordl.deleted   EQ NO
          AND po-ordl.item-type EQ NO
          AND po-ordl.job-no    eq ip-job-no
          AND po-ordl.job-no2   eq ip-job-no2
        USE-INDEX po-no NO-ERROR.
    IF NOT AVAIL po-ordl THEN
    FOR EACH po-ordl NO-LOCK
        WHERE po-ordl.company   EQ ar-invl.company
          AND po-ordl.i-no      EQ ar-invl.i-no
          AND po-ordl.deleted   EQ no
          AND po-ordl.item-type EQ no
          AND po-ordl.job-no    EQ ip-job-no
          AND po-ordl.job-no2   EQ ip-job-no2
        USE-INDEX item
        BY po-ordl.po-no DESC:
      LEAVE.
    END.
    IF NOT AVAIL po-ordl THEN
    FOR EACH po-ordl NO-LOCK
        WHERE po-ordl.company   EQ ar-invl.company
          AND po-ordl.i-no      EQ ar-invl.i-no
          AND po-ordl.deleted   EQ NO
          AND po-ordl.item-type EQ NO
        USE-INDEX item
        BY po-ordl.po-no DESC:
      LEAVE.
    END.
    IF AVAIL po-ordl THEN
      ASSIGN
       op-cost = po-ordl.cons-cost
       lv-uom  = po-ordl.cons-uom.

    IF op-cost EQ 0 AND AVAIL itemfg THEN
      ASSIGN
       op-cost = IF itemfg.i-code EQ "C" AND ip-type EQ 2 THEN
                   itemfg.std-mat-cost ELSE
                 IF fg-ctrl.inv-meth EQ "A" THEN itemfg.avg-cost
                                            ELSE itemfg.last-cost
       lv-uom  = itemfg.prod-uom.

    op-cost = op-cost * ip-qty /
              (IF lv-uom EQ "C"  THEN 100               ELSE
               IF lv-uom EQ "M"  THEN 1000              ELSE
               IF lv-uom EQ "CS" AND AVAIL itemfg AND
                 itemfg.case-count NE 0 THEN itemfg.case-count ELSE 1).
  END.

  IF op-cost EQ ? THEN op-cost = 0.
END.
