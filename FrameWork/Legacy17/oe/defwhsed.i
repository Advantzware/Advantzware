
IF NOT CAN-FIND(FIRST oe-ordl-whs-item NO-LOCK
                WHERE oe-ordl-whs-item.reftable EQ "oe-ordl.whs-item"
                  AND oe-ordl-whs-item.company  EQ {1}.company
                  AND oe-ordl-whs-item.loc      EQ STRING({1}.ord-no,"9999999999")
                  AND oe-ordl-whs-item.code     EQ {1}.i-no
                  AND oe-ordl-whs-item.code2    EQ STRING({1}.line,"9999999999"))
THEN DO:
  CREATE oe-ordl-whs-item.
  ASSIGN
   oe-ordl-whs-item.reftable = "oe-ordl.whs-item"
   oe-ordl-whs-item.company  = {1}.company
   oe-ordl-whs-item.loc      = STRING({1}.ord-no,"9999999999")
   oe-ordl-whs-item.code     = {1}.i-no
   oe-ordl-whs-item.code2    = STRING({1}.line,"9999999999").

  FIND FIRST oe-ord-whs-order NO-LOCK
      WHERE oe-ord-whs-order.reftable EQ "oe-ord.whs-order"
        AND oe-ord-whs-order.company  EQ {1}.company
        AND oe-ord-whs-order.loc      EQ STRING({1}.ord-no,"9999999999")
      NO-ERROR.
  IF AVAIL oe-ord-whs-order THEN
    oe-ordl-whs-item.val[1] = oe-ord-whs-order.val[1].
END.
