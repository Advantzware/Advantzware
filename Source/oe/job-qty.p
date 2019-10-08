
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-qty-mod AS LOG NO-UNDO.


DEF VAR ll-add-overrun AS LOG NO-UNDO.
DEF VAR lv-job-no LIKE oe-ordl.job-no NO-UNDO.
DEF VAR lv-job-no2 LIKE oe-ordl.job-no2 NO-UNDO.
DEF VAR li-job-qty LIKE job-hdr.qty NO-UNDO.


FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-ordl THEN
  FIND FIRST oe-ord
      WHERE oe-ord.company EQ oe-ordl.company
        AND oe-ord.ord-no  EQ oe-ordl.ord-no
      NO-LOCK NO-ERROR.

ELSE DO:
  FIND oe-ord WHERE ROWID(oe-ord) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL oe-ord THEN
  FIND FIRST oe-ordl
      WHERE oe-ordl.company EQ oe-ord.company
        AND oe-ordl.ord-no  EQ oe-ord.ord-no
        AND oe-ordl.job-no  NE ""
      NO-LOCK NO-ERROR.
END.

IF AVAIL oe-ord AND AVAIL oe-ordl THEN DO:
  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ oe-ord.company
        AND sys-ctrl.name    EQ "JOB QTY"
      NO-LOCK NO-ERROR.
  ASSIGN
   ll-add-overrun = AVAIL sys-ctrl AND sys-ctrl.log-fld
   lv-job-no      = oe-ordl.job-no
   lv-job-no2     = oe-ordl.job-no2.

  RELEASE oe-ordl.

  FOR EACH job-hdr NO-LOCK 
      WHERE job-hdr.company EQ oe-ord.company
        AND job-hdr.job-no  EQ lv-job-no
        AND job-hdr.job-no2 EQ lv-job-no2
        AND job-hdr.ord-no  EQ oe-ord.ord-no
        AND NOT CAN-FIND(FIRST job WHERE job.company EQ job-hdr.company
                            AND job.job-no EQ job-hdr.job-no
                            AND job.job-no2 EQ job-hdr.job-no2
                            AND job.qty-changed )
      BREAK BY job-hdr.i-no:

    li-job-qty = li-job-qty + job-hdr.qty.

    IF LAST-OF(job-hdr.i-no) THEN DO:
      FIND FIRST oe-ordl NO-LOCK
          WHERE oe-ordl.company  EQ job-hdr.company
            AND oe-ordl.ord-no   EQ job-hdr.ord-no
            AND oe-ordl.job-no   EQ job-hdr.job-no
            AND oe-ordl.job-no2  EQ job-hdr.job-no2
            AND oe-ordl.i-no     EQ job-hdr.i-no
            AND oe-ordl.est-no   EQ job-hdr.est-no
          NO-ERROR.

      IF NOT AVAIL oe-ordl OR
         oe-ordl.qty * (1 + IF ll-add-overrun THEN (oe-ordl.over-pct / 100) ELSE 0)
                               NE li-job-qty THEN DO:
        op-qty-mod = YES.
        LEAVE.
      END.

      li-job-qty = 0.
    END.
  END.
END.
