DEF INPUT PARAMETER ip-company AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-ship-id AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-carrier AS CHAR NO-UNDO.
DEF INPUT PARAMETER v-oe-boll AS ROWID.
DEF OUTPUT PARAMETER v-out-frt AS DEC NO-UNDO.


DEF VAR v-pallets AS INT NO-UNDO.

DEF BUFFER bf-oe-boll FOR oe-boll.
FIND bf-oe-boll WHERE ROWID(bf-oe-boll) = v-oe-boll NO-LOCK.
FIND FIRST oe-bolh WHERE oe-bolh.b-no EQ bf-oe-boll.b-no NO-LOCK NO-ERROR.
                  
IF AVAIL oe-bolh THEN
FIND FIRST shipto
      WHERE shipto.company EQ oe-bolh.company
        AND shipto.cust-no EQ ip-cust-no
        AND shipto.ship-id EQ ip-ship-id
      NO-LOCK NO-ERROR.
      
IF AVAIL shipto THEN
FIND FIRST carrier
    WHERE carrier.company EQ shipto.company
      AND carrier.loc     EQ shipto.loc
      AND carrier.carrier EQ ip-carrier
    NO-LOCK NO-ERROR.

IF AVAIL carrier THEN
FOR EACH itemfg
   WHERE itemfg.company EQ bf-oe-boll.company
     AND itemfg.i-no    EQ bf-oe-boll.i-no
   NO-LOCK:

  FIND FIRST fg-bin
      WHERE fg-bin.company EQ ip-company
        AND fg-bin.i-no    EQ bf-oe-boll.i-no
        AND fg-bin.tag     EQ bf-oe-boll.tag
        AND fg-bin.loc     EQ bf-oe-boll.loc
        AND fg-bin.loc-bin EQ bf-oe-boll.loc-bin
        AND fg-bin.job-no  EQ bf-oe-boll.job-no
        AND fg-bin.job-no2 EQ bf-oe-boll.job-no2
      NO-LOCK NO-ERROR.

  CASE carrier.chg-method:
    WHEN "W" THEN DO:                                     /* Weight in Lbs */
      v-out-frt = IF bf-oe-boll.weight NE 0 THEN bf-oe-boll.weight
                  ELSE (itemfg.weight-100 * bf-oe-boll.qty / 100).
    END.

    WHEN "P" THEN DO:                                     /* # of Pallets */
      RUN oe/pallcalc.p (ROWID(bf-oe-boll), OUTPUT v-pallets).
      FIND CURRENT bf-oe-boll EXCLUSIVE-LOCK.
      bf-oe-boll.tot-pallets = v-pallets.
      FIND CURRENT bf-oe-boll NO-LOCK.
      v-out-frt = v-pallets.                        
    END.

    OTHERWISE DO:                                         /* MSF */
      v-out-frt = itemfg.t-sqft * bf-oe-boll.qty / 1000.
    END.
  END CASE.
END.
