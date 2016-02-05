DISABLE TRIGGERS FOR LOAD OF oe-boll.
DISABLE TRIGGERS FOR LOAD OF oe-bolh.

DEF INPUT PARAMETER iprBolhRow AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER opdFreight AS DECIMAL DECIMALS 6 NO-UNDO.


DEF VAR dFreight AS DEC DECIMALS 6 NO-UNDO.
DEF VAR v-pallets AS INT NO-UNDO.
DEF VAR v-qty-pal AS DEC NO-UNDO.
DEF VAR v-frt-chg AS DEC NO-UNDO.
DEF VAR v-del-zone LIKE oe-ordl.del-zone NO-UNDO.
DEF VAR v-other-freight AS DEC NO-UNDO DECIMALS 10.
DEF VAR dTotBasis AS DECIMAL NO-UNDO DECIMALS 10.
DEF VAR tot-other-freight AS DEC NO-UNDO DECIMALS 10.
DEF VAR ldMinRate AS DEC NO-UNDO.
DEF VAR dTotFreight AS DEC NO-UNDO DECIMALS 10.
DEF BUFFER bf-oe-boll FOR oe-boll.


 FIND  oe-bolh WHERE ROWID(oe-bolh) EQ iprBolhRow EXCLUSIVE-LOCK NO-ERROR.
 IF NOT AVAIL oe-bolh THEN
   RETURN.

 ASSIGN oe-bolh.tot-pallets = 0
        dTotFreight         = 0
        tot-other-freight   = 0.
 
 /* Obtain total basis weight for all lines on the BOL */
 FOR EACH bf-oe-boll
     WHERE bf-oe-boll.company EQ oe-bolh.company
       AND bf-oe-boll.b-no    EQ oe-bolh.b-no:
   RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,
               INPUT oe-bolh.cust-no,
               INPUT oe-bolh.ship-id,
               INPUT oe-bolh.carrier,
               INPUT rowid(bf-oe-boll), 
               OUTPUT v-other-freight). 
   tot-other-freight = tot-other-freight + v-other-freight.
 END.


 /* Obtain the total freight for all lines on BOL */
 FOR EACH bf-oe-boll
     WHERE bf-oe-boll.company EQ oe-bolh.company
       AND bf-oe-boll.b-no    EQ oe-bolh.b-no:

   FIND FIRST shipto
         WHERE shipto.company EQ oe-bolh.company
           AND shipto.cust-no EQ oe-bolh.cust-no
           AND shipto.ship-id EQ oe-bolh.ship-id
         NO-LOCK NO-ERROR.
   IF NOT AVAIL shipto THEN
     NEXT.

   RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,
               INPUT oe-bolh.cust-no,
               INPUT oe-bolh.ship-id,
               INPUT oe-bolh.carrier,
               INPUT rowid(bf-oe-boll), 
               OUTPUT v-other-freight). 
  
  FIND FIRST ITEMfg WHERE itemfg.company = oe-bolh.company
      AND itemfg.i-no EQ bf-oe-boll.i-no
    NO-LOCK NO-ERROR.
  v-del-zone = shipto.dest-code.
  /* Indicates to always copy BOL freight class from item */
  find first sys-ctrl where sys-ctrl.company eq oe-bolh.company
                        and sys-ctrl.name    eq "BOLFreight" 
                      no-lock no-error.
  IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "FGFreightClass" THEN DO:
    IF AVAIL(itemfg) AND itemfg.frt-class GT "" THEN DO:
      FIND FIRST carr-mtx WHERE carr-mtx.company  EQ oe-bolh.company
                            AND carr-mtx.loc      EQ shipto.loc
                            AND carr-mtx.carrier  EQ oe-bolh.carrier
                            AND carr-mtx.del-zone EQ itemfg.frt-class
                          NO-LOCK NO-ERROR.
      IF AVAIL carr-mtx THEN
        ASSIGN v-del-zone = itemfg.frt-class.    
    END.
  END.


   RUN oe/getLineFrt.p (oe-bolh.company, 
                        shipto.loc, 
                        oe-bolh.carrier, 
                        v-del-zone,
                        shipto.ship-zip, 
                        v-other-freight, 
                        tot-other-freight, 
                        1, 
                        OUTPUT dFreight, 
                        OUTPUT ldMinRate).

   ASSIGN bf-oe-boll.freight = dFreight. 
          dTotFreight = dTotFreight + dFreight.
          dTotBasis = dTotBasis + v-other-freight.
          oe-bolh.tot-pallets = oe-bolh.tot-pallets + bf-oe-boll.tot-pallets.
 END. /* each oe-boll */

 /* If the total freight calculated is below minimum, distribute minimum to lines */
 IF dTotFreight LT ldMinRate THEN DO:

   dTotFreight = ldMinRate.

   /* distribute total to lines */
   FOR EACH bf-oe-boll
       WHERE bf-oe-boll.company EQ oe-bolh.company
         AND bf-oe-boll.b-no    EQ oe-bolh.b-no
       EXCLUSIVE-LOCK:

     RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,
               INPUT oe-bolh.cust-no,
               INPUT oe-bolh.ship-id,
               INPUT oe-bolh.carrier,
               INPUT rowid(bf-oe-boll), 
               OUTPUT v-other-freight). 
 
     /* line freight is total / (basis  / total basis */
     bf-oe-boll.freight = dTotFreight * v-other-freight / dTotBasis.

   END. /* each bol */

 END. /* if below minimum */

 /*oe-bolh.freight = dTotFreight.*/ /* task NO 5051503 */
 FIND CURRENT oe-bolh NO-LOCK.
 opdFreight = dTotFreight.
