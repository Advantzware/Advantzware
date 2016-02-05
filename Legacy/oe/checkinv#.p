
DEF PARAM BUFFER io-inv-head FOR inv-head.

DEF BUFFER inv-mult FOR inv-head.
DEF VAR lv-inv-no LIKE inv-head.inv-no NO-UNDO.

IF AVAIL io-inv-head THEN
DO WHILE TRUE:

  FIND FIRST inv-head NO-LOCK
      WHERE inv-head.company  EQ io-inv-head.company
        AND inv-head.inv-no   EQ io-inv-head.inv-no
        AND ROWID(inv-head)   NE ROWID(io-inv-head)
        AND (inv-head.cust-no NE io-inv-head.cust-no OR
             NOT CAN-FIND(FIRST inv-mult
                          WHERE inv-mult.company       EQ io-inv-head.company
                            AND inv-mult.cust-no       EQ io-inv-head.cust-no
                            AND inv-mult.multi-invoice EQ YES))
      NO-ERROR.
  FIND FIRST ar-inv
      WHERE ar-inv.company EQ io-inv-head.company
        AND ar-inv.inv-no  EQ io-inv-head.inv-no
      NO-LOCK NO-ERROR.

  IF NOT AVAIL inv-head AND NOT AVAIL ar-inv THEN LEAVE.

  REPEAT:
   
     FIND FIRST ar-ctrl WHERE
          ar-ctrl.company EQ io-inv-head.company
          EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    
     IF AVAIL ar-ctrl THEN
     DO:
        ASSIGN
           lv-inv-no = ar-ctrl.last-inv
           io-inv-head.inv-no = lv-inv-no + 1
           ar-ctrl.last-inv = io-inv-head.inv-no.
    
        FIND CURRENT ar-ctrl NO-LOCK NO-ERROR.
        LEAVE.
     END.
  END.
END.

