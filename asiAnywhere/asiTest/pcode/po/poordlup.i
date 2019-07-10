
DEF VAR v-hld-qty AS DEC NO-UNDO.
DEF VAR li-loop AS INT NO-UNDO.


li-loop = 0.
DO WHILE li-loop LT 1000:
  li-loop = li-loop + 1.

  IF {1}po-ordl.item-type THEN DO:
    FIND FIRST item EXCLUSIVE
        WHERE item.company eq {1}po-ordl.company
          AND item.i-no    eq {1}po-ordl.i-no
        USE-INDEX i-no NO-ERROR NO-WAIT.
    IF AVAIL item THEN DO:
      IF item.i-code EQ "R" THEN
        IF item.r-wid NE 0 THEN {1}po-ordl.s-wid = item.r-wid.
        ELSE DO:
          IF item.s-wid NE 0 THEN {1}po-ordl.s-wid = item.s-wid.
          IF item.s-len NE 0 THEN {1}po-ordl.s-len = item.s-len.
        END.

      RUN po/rm-q-ono.p (BUFFER {1}po-ordl, OUTPUT v-hld-qty).

      item.q-ono = item.q-ono + (v-hld-qty * v-factor).
          
      IF item.q-ono LT 0 AND v-reopen THEN item.q-ono = 0.
    
      item.q-avail = item.q-onh + item.q-ono - item.q-comm.

      li-loop = 1000.
    END.

    ELSE li-loop = 1000.
  END.

  ELSE DO:
    FIND FIRST itemfg EXCLUSIVE
        WHERE itemfg.company EQ {1}po-ordl.company
          AND itemfg.i-no    EQ {1}po-ordl.i-no
        USE-INDEX i-no NO-ERROR.
    IF AVAIL itemfg THEN DO:
      RUN fg/calcqono.p (ROWID(itemfg), OUTPUT itemfg.q-ono).

      li-loop = 1000.
    END.

    ELSE li-loop = 1000.
  END.
END.
