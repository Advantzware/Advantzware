
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{rm/bestvend.i}

DEF TEMP-TABLE w-run NO-UNDO
    FIELD w-qty AS DEC
    FIELD w-cost AS DEC
    FIELD w-setups AS DEC.

DEF VAR ll-select AS LOG NO-UNDO.
DEF VAR li AS INT NO-UNDO.

EMPTY TEMP-TABLE tt-ei.
EMPTY TEMP-TABLE tt-eiv.

FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL eb THEN
FIND FIRST ef
    WHERE ef.company EQ eb.company
      AND ef.est-no  EQ eb.est-no
      AND ef.form-no EQ eb.form-no
    NO-LOCK NO-ERROR.

IF eb.pur-man THEN
FOR EACH e-itemfg-vend NO-LOCK
    WHERE e-itemfg-vend.company  EQ eb.company
      AND e-itemfg-vend.est-no   EQ eb.est-no
      AND e-itemfg-vend.eqty     EQ eb.eqty
      AND e-itemfg-vend.form-no  EQ eb.form-no
      AND e-itemfg-vend.blank-no EQ eb.blank-no
    BREAK BY e-itemfg-vend.vend-no:

  IF FIRST(e-itemfg-vend.vend-no) THEN DO:
    FIND FIRST reftable
        WHERE reftable.reftable EQ "e-itemfg-vend.std-uom"
          AND reftable.company  EQ e-itemfg-vend.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ e-itemfg-vend.est-no
          AND reftable.val[1]   EQ e-itemfg-vend.form-no
          AND reftable.val[2]   EQ e-itemfg-vend.blank-no
        NO-LOCK NO-ERROR.

    CREATE tt-ei.
    ASSIGN
     tt-ei.std-uom = IF AVAIL reftable THEN reftable.code2 ELSE "EA"
     tt-ei.i-no    = eb.stock-no
     tt-ei.company = e-itemfg-vend.company.

    DO li = 1 TO 10:
       ASSIGN
          tt-ei.run-qty[li] = e-itemfg-vend.run-qty[li]
          tt-ei.run-cost[li] = e-itemfg-vend.run-cost[li].
    END.
  END.

  IF NOT CAN-FIND(FIRST tt-eiv
                  WHERE tt-eiv.company   EQ e-itemfg-vend.company
                    AND tt-eiv.i-no      EQ eb.stock-no
                    AND tt-eiv.vend-no   EQ e-itemfg-vend.vend-no) THEN DO:
    CREATE tt-eiv.
    ASSIGN
     tt-eiv.row-id = ROWID(e-itemfg-vend)
     tt-eiv.i-no   = eb.stock-no
     tt-eiv.company = e-itemfg-vend.company
     tt-eiv.vend-no = e-itemfg-vend.vend-no
     tt-eiv.item-type = e-itemfg-vend.item-type
     tt-eiv.rec_key   = e-itemfg-vend.rec_key.

    DO li = 1 TO 10:
       ASSIGN
          tt-eiv.run-qty[li] = e-itemfg-vend.run-qty[li]
          tt-eiv.run-cost[li] = e-itemfg-vend.run-cost[li]
          tt-eiv.setups[li] = e-itemfg-vend.setups[li]
          tt-eiv.roll-w[li] = e-itemfg-vend.roll-w[li]
          tt-eiv.SELECTED[li] = e-itemfg-vend.SELECTED[li].
    END.

    DO li = 11 TO 30:
       tt-eiv.roll-w[li] = e-itemfg-vend.roll-w[li].
    END.

  END.
END.

ELSE
FOR EACH e-item
    WHERE e-item.company EQ ef.company
      AND e-item.i-no    EQ ef.board
    NO-LOCK:

  CREATE tt-ei.
  ASSIGN
     tt-ei.std-uom = e-item.std-uom
     tt-ei.i-no    = e-item.i-no
     tt-ei.company = e-item.company.

  DO li = 1 TO 10:
     ASSIGN
        tt-ei.run-qty[li] = e-item.run-qty[li]
        tt-ei.run-cost[li] = e-item.run-cost[li].
  END.


     DO li = 1 TO 10:
        ASSIGN
           tt-ei.run-qty[li + 10] = e-item.run-qty[li]
           tt-ei.run-cost[li + 10] = e-item.run-cost[li].
     END.


    

  FOR EACH e-item-vend OF e-item NO-LOCK:
    IF NOT CAN-FIND(FIRST tt-eiv
                    WHERE tt-eiv.company   EQ e-item-vend.company
                      AND tt-eiv.i-no      EQ e-item-vend.i-no
                      AND tt-eiv.vend-no   EQ e-item-vend.vend-no) THEN DO:
      CREATE tt-eiv.
      
      DO li = 1 TO 10:
         ASSIGN
            tt-eiv.run-qty[li] = e-item-vend.run-qty[li]
            tt-eiv.run-cost[li] = e-item-vend.run-cost[li]
            tt-eiv.setups[li] = e-item-vend.setups[li]
            tt-eiv.roll-w[li] = e-item-vend.roll-w[li].
      END.

      DO li = 11 TO 30:
         tt-eiv.roll-w[li] = e-item-vend.roll-w[li].
      END.

      

      IF AVAIL e-item-vend THEN
      DO:         

         DO li = 1 TO 10:
            ASSIGN
               tt-eiv.run-qty[li + 10] = e-item-vend.runQtyXtra[li]
               tt-eiv.run-cost[li + 10] = e-item-vend.runCostXtra[li]
               tt-eiv.setups[li + 10] = e-item-vend.setupsXtra[li].
         END.
      END.

      ASSIGN
         tt-eiv.row-id = ROWID(e-item-vend)
         tt-eiv.company = e-item-vend.company
         tt-eiv.i-no    = e-item-vend.i-no
         tt-eiv.vend-no = e-item-vend.vend-no
         tt-eiv.item-type = e-item-vend.item-type
         tt-eiv.rec_key = e-item-vend.rec_key.
    END.
  END.
END.

ll-select = NO.

check-for-selected:
FOR EACH tt-eiv.
  DO li = 1 TO EXTENT(tt-eiv.selected):
    IF tt-eiv.selected[li] THEN DO:
      ll-select = YES.
      LEAVE check-for-selected.
    END.
  END.
END.

IF ll-select THEN
FOR EACH tt-eiv:
  FOR EACH w-run:
    DELETE w-run.
  END.

  DO li = 1 TO EXTENT(tt-eiv.selected):
    IF tt-eiv.selected[li] THEN DO:
      CREATE w-run.
      ASSIGN
       w-qty    = tt-eiv.run-qty[li]
       w-cost   = tt-eiv.run-cost[li]
       w-setups = tt-eiv.setups[li].
    END.
  END.

  ASSIGN
   tt-eiv.run-qty  = 0
   tt-eiv.run-cost = 0
   tt-eiv.setups   = 0
   li              = 0.

  FOR EACH w-run WHERE w-qty GT 0 BREAK BY w-qty:
    li = li + 1.

    IF li GE 1 AND li LE EXTENT(tt-eiv.run-qty) THEN
      ASSIGN
       tt-eiv.run-qty[li]  = IF LAST(w-qty) THEN 999999999.999999 ELSE w-qty
       tt-eiv.run-cost[li] = w-cost
       tt-eiv.setups[li]   = w-setups.

    DELETE w-run.
  END.
END.
