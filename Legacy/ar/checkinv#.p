
DEF PARAM BUFFER io-ar-inv FOR ar-inv.


IF AVAIL io-ar-inv THEN
DO WHILE CAN-FIND(FIRST ar-inv
                  WHERE ar-inv.company EQ io-ar-inv.company
                    AND ar-inv.inv-no  EQ io-ar-inv.inv-no
                    AND ROWID(ar-inv)  NE ROWID(io-ar-inv)):

  io-ar-inv.inv-no = io-ar-inv.inv-no + 1.

  FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ io-ar-inv.company
      EXCLUSIVE NO-ERROR NO-WAIT.
  IF AVAIL ar-ctrl THEN ar-ctrl.last-inv = io-ar-inv.inv-no.
END.

