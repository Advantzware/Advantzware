
DEF INPUT  PARAM ip-rowid       AS   ROWID                  NO-UNDO.
DEF INPUT  PARAM ip-cust-no     LIKE oe-bolh.cust-no        NO-UNDO.
DEF INPUT  PARAM ip-ship-id     LIKE oe-bolh.ship-id        NO-UNDO.
DEF INPUT  PARAM ip-carrier     LIKE oe-bolh.carrier        NO-UNDO.
DEF OUTPUT PARAM op-freight     AS   DEC DECIMALS 10        NO-UNDO.

/*{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}*/

DEFINE SHARED VARIABLE g_company AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE g_loc AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE cocode AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE locode AS CHAR NO-UNDO.

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR v-pallets AS INT NO-UNDO.
DEF VAR v-qty-pal AS DEC NO-UNDO.
DEF VAR v-frt-chg AS DEC NO-UNDO.


FIND FIRST oe-boll WHERE ROWID(oe-boll) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-boll THEN
FIND FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no NO-LOCK NO-ERROR.
                  
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
   WHERE itemfg.company EQ oe-boll.company
     AND itemfg.i-no    EQ oe-boll.i-no
   NO-LOCK:

  FIND FIRST fg-bin
      WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ oe-boll.i-no
        AND fg-bin.tag     EQ oe-boll.tag
        AND fg-bin.loc     EQ oe-boll.loc
        AND fg-bin.loc-bin EQ oe-boll.loc-bin
        AND fg-bin.job-no  EQ oe-boll.job-no
        AND fg-bin.job-no2 EQ oe-boll.job-no2
      NO-LOCK NO-ERROR.

  CASE carrier.chg-method:
    WHEN "W" THEN DO:                                     /* Weight in Lbs */
      v-frt-chg = IF oe-boll.weight NE 0 THEN oe-boll.weight
                  ELSE (itemfg.weight-100 * oe-boll.qty / 100).
    END.

    WHEN "P" THEN DO:                                     /* # of Pallets */
      RUN oe/pallcalc.p (ROWID(oe-boll), OUTPUT v-pallets).
      v-frt-chg = v-pallets.                        
    END.

    OTHERWISE DO:                                         /* MSF */
      v-frt-chg = itemfg.t-sqft * oe-boll.qty / 1000.
    END.
  END CASE.

  RUN sys/inc/getfrate.p (shipto.loc, ip-carrier, shipto.dest-code,
                          v-frt-chg, 1, OUTPUT v-frt-chg).

  op-freight = v-frt-chg.

  LEAVE.
END.
