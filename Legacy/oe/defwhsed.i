
  FIND FIRST oe-ord-whs-order NO-LOCK
      WHERE oe-ord-whs-order.reftable EQ "oe-ord.whs-order"
        AND oe-ord-whs-order.company  EQ {1}.company
        AND oe-ord-whs-order.loc      EQ STRING({1}.ord-no,"9999999999")
      NO-ERROR.
  IF AVAIL oe-ord-whs-order THEN
    oe-ordl.managed = (oe-ord-whs-order.val[1] EQ 1).
