    
    DEF VAR lv-foreachr AS CHAR NO-UNDO.


    lv-foreachr = STRING(TODAY,"99/99/9999") + STRING(TIME,"999999").

    IF NOT AVAIL oe-ctrl THEN
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK.
   
    foreachr:
    FOR EACH oe-relh NO-LOCK
        WHERE oe-relh.company  EQ cocode
          AND oe-relh.posted   EQ NO
          AND oe-relh.rel-date GE v-fdate
          AND oe-relh.rel-date LE v-tdate
          AND oe-relh.release# GE v-s-rel
          AND oe-relh.release# LE v-e-rel
          AND oe-relh.cust-no  GE v-s-cust-no
          AND oe-relh.cust-no  LE v-e-cust-no
          AND oe-relh.printed  EQ v-printed 
          AND oe-relh.stat     NE "W"
          AND oe-relh.w-ord = NO
          AND CAN-FIND(FIRST oe-rell
                       WHERE oe-rell.company EQ cocode
                         AND oe-rell.r-no    EQ oe-relh.r-no
                         AND oe-rell.ord-no  GE v-s-ord
                         AND oe-rell.ord-no  LE v-e-ord
                       USE-INDEX r-no)
          /* gdm - 02020902 - ANY HOLD OE-RELH WILL NOT BE PRINTED */
          
        USE-INDEX post:

        /* gdm - 03110907 - ANY HOLD OE-RELH WILL NOT BE PRINTED */
        FIND FIRST sys-ctrl NO-LOCK
            WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.name EQ "RELCREDT" NO-ERROR.
        IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN NEXT foreachr.

      FOR EACH reftable
          WHERE reftable.reftable EQ "oe-relh.can-print"
            AND reftable.rec_key  EQ oe-relh.rec_key
          USE-INDEX rec_key:
        DELETE reftable.
      END.

      IF NOT oe-ctrl.p-pick THEN
      FOR EACH oe-rell
          WHERE oe-rell.company EQ oe-relh.company
            AND oe-rell.r-no    EQ oe-relh.r-no
            AND CAN-FIND(FIRST oe-ord
                         WHERE oe-ord.company EQ oe-rell.company
                           AND oe-ord.ord-no  EQ oe-rell.ord-no
                           AND oe-ord.stat    EQ "H")
          NO-LOCK:
        NEXT foreachr.
      END.

      CREATE reftable.
      ASSIGN
       reftable.reftable = "oe-relh.can-print"
       reftable.rec_key  = oe-relh.rec_key
       reftable.company  = lv-foreachr.
    END.

    RELEASE oe-relh.
