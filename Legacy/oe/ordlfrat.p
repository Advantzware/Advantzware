
DEF INPUT  PARAM ip-rowid       AS   ROWID                  NO-UNDO.
DEF OUTPUT PARAM op-freight     AS   DEC DECIMALS 10        NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR lv-tot-qty LIKE oe-ordl.qty NO-UNDO.
DEF VAR lv-rel-qty LIKE oe-rel.qty NO-UNDO.
DEF VAR lv-carrier LIKE oe-rel.carrier NO-UNDO.
DEF VAR ld AS DEC DECIMALS 10 NO-UNDO.
DEF VAR ld-freight AS DEC DECIMALS 10 NO-UNDO.
DEF VAR li-palls AS INT NO-UNDO.
DEF VAR li-cases AS INT NO-UNDO.
DEF VAR v-del-zone LIKE itemfg.frt-class NO-UNDO.

FOR EACH oe-ordl WHERE ROWID(oe-ordl) EQ ip-rowid NO-LOCK,
    FIRST itemfg OF oe-ordl NO-LOCK,
    FIRST oe-ord OF oe-ordl NO-LOCK:

  lv-tot-qty = oe-ordl.qty.

  FOR EACH oe-rel
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
      NO-LOCK,
      FIRST shipto
      WHERE shipto.company EQ oe-ord.company
        AND shipto.cust-no EQ oe-ord.cust-no
        AND shipto.ship-id EQ oe-rel.ship-id
      NO-LOCK:

    ASSIGN
     lv-rel-qty = oe-rel.qty
     lv-tot-qty = lv-tot-qty - lv-rel-qty.

    IF lv-tot-qty LT 0 THEN
      ASSIGN
       lv-rel-qty = lv-rel-qty + lv-tot-qty
       lv-tot-qty = 0.

    IF lv-rel-qty GT 0 THEN DO:
      lv-carrier = oe-rel.carrier.
      RUN get-freight.    
    END.
  END.

  IF lv-tot-qty NE 0 THEN DO:
    lv-rel-qty = lv-tot-qty.

    FOR EACH shipto
        WHERE shipto.company EQ oe-ord.company
          AND shipto.cust-no EQ oe-ord.cust-no
        NO-LOCK
        BREAK BY shipto.ship-no DESC:
      
      IF LAST(shipto.ship-no)             OR
         shipto.ship-id EQ oe-ord.cust-no THEN do:
        lv-carrier = shipto.carrier.
        RUN get-freight.
        LEAVE.
      END.
    END.
  END.
END.

RETURN.

PROCEDURE get-freight.

  FOR EACH carrier OF shipto NO-LOCK:

    FIND FIRST fg-bin
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
          AND fg-bin.loc     EQ itemfg.def-loc
          AND fg-bin.loc-bin EQ itemfg.def-loc-bin
        NO-LOCK NO-ERROR.

    IF oe-ordl.est-no NE "" THEN
    FIND FIRST eb
        WHERE eb.company  EQ oe-ordl.company
          AND eb.est-no   EQ oe-ordl.est-no
          AND eb.stock-no EQ oe-ordl.i-no
        NO-LOCK NO-ERROR.

    ASSIGN
     li-cases = lv-rel-qty / oe-ordl.cas-cnt
     li-palls = li-cases /
                (IF AVAIL eb AND eb.cas-pal NE 0 THEN eb.cas-pal
                 ELSE
                 IF oe-ordl.cases-unit NE 0 THEN
                    oe-ordl.cases-unit
                 ELSE
                 IF AVAIL fg-bin           AND
                    fg-bin.cases-unit NE 0 THEN fg-bin.cases-unit
                 ELSE 1).

    CASE carrier.chg-method:
      WHEN "W" THEN DO:                                     /* Weight in Lbs */
        ld = (IF oe-ordl.t-weight NE 0 THEN (oe-ordl.t-weight / oe-ordl.qty * lv-rel-qty)
              ELSE (itemfg.weight-100 * lv-rel-qty / 100)).
      END.

      WHEN "P" THEN DO:                                     /* # of Pallets */
        ld = li-palls.                        
      END.

      OTHERWISE DO:                                         /* MSF */
        ld = itemfg.t-sqft * lv-rel-qty / 1000.
      END.
    END CASE.

    v-del-zone = shipto.dest-code.
    /* Indicates to always copy BOL freight class from item */
    FIND first sys-ctrl where sys-ctrl.company eq cocode
                          and sys-ctrl.name    eq "BOLFreight" 
                        no-lock no-error.
    IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "FGFreightClass" THEN DO:
      IF AVAIL(itemfg) AND itemfg.frt-class GT "" THEN DO:
        FIND FIRST carr-mtx WHERE carr-mtx.company  EQ oe-ordl.company
                              AND carr-mtx.loc      EQ shipto.loc
                              AND carr-mtx.carrier  EQ lv-carrier
                              AND carr-mtx.del-zone EQ itemfg.frt-class
                            NO-LOCK NO-ERROR.
        IF AVAIL carr-mtx THEN
          ASSIGN v-del-zone = itemfg.frt-class.    
      END.
    END.
    RUN sys/inc/getfrate.p (shipto.loc, lv-carrier, v-del-zone,
                            shipto.ship-zip, ld, ld, 1, OUTPUT ld-freight).

    op-freight = op-freight + ld-freight.
  END.

END PROCEDURE.
