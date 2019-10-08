
DEF INPUT  PARAM ip-rowid       AS   ROWID                  NO-UNDO.
DEF INPUT  PARAM ip-cust-no     LIKE oe-bolh.cust-no        NO-UNDO.
DEF INPUT  PARAM ip-ship-id     LIKE oe-bolh.ship-id        NO-UNDO.
DEF INPUT  PARAM ip-carrier     LIKE oe-bolh.carrier        NO-UNDO.
DEF OUTPUT PARAM op-freight     AS   DEC DECIMALS 10        NO-UNDO.
MESSAGE "Internal Error - Program Out of Date"
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
IF TRUE THEN RETURN.
DEF BUFFER b-oe-boll FOR oe-boll.
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR v-pallets AS INT NO-UNDO.
DEF VAR v-qty-pal AS DEC NO-UNDO.
DEF VAR v-frt-chg AS DEC NO-UNDO.
DEF VAR v-del-zone LIKE oe-ordl.del-zone NO-UNDO.
DEF VAR v-other-freight AS DEC NO-UNDO.
DEF VAR tot-other-freight AS DEC NO-UNDO.

tot-other-freight = 0.
FIND FIRST oe-boll WHERE ROWID(oe-boll) EQ ip-rowid NO-LOCK NO-ERROR.
IF AVAIL oe-boll THEN DO:
  FOR EACH b-oe-boll WHERE b-oe-boll.company = oe-boll.company
                       AND b-oe-boll.bol-no  = oe-boll.bol-no
                       AND b-oe-boll.i-no    = oe-boll.i-no
                     NO-LOCK.
     RUN other-lines (INPUT rowid(b-oe-boll), OUTPUT v-other-freight).
     /* total weight of all lines on BOL for this item */
     tot-other-freight = tot-other-freight + v-other-freight.
  END.
END.
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

  v-del-zone = shipto.dest-code.
  /* Indicates to always copy BOL freight class from item */
  find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "BOLFreight" 
                      no-lock no-error.
  IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "FGFreightClass" THEN DO:
    IF AVAIL(itemfg) AND itemfg.frt-class GT "" THEN DO:
      FIND FIRST carr-mtx WHERE carr-mtx.company  EQ oe-bolh.company
                            AND carr-mtx.loc      EQ shipto.loc
                            AND carr-mtx.carrier  EQ ip-carrier
                            AND carr-mtx.del-zone EQ itemfg.frt-class
                          NO-LOCK NO-ERROR.
      IF AVAIL carr-mtx THEN
        ASSIGN v-del-zone = itemfg.frt-class.    
    END.
  END.

  RUN sys/inc/getfrate.p (shipto.loc, ip-carrier, v-del-zone,
                          shipto.ship-zip, v-frt-chg, tot-other-freight, 1, OUTPUT v-frt-chg).

  op-freight = v-frt-chg.

  LEAVE.
END.

PROCEDURE other-lines:
DEF INPUT PARAMETER v-oe-boll AS ROWID.
DEF OUTPUT PARAMETER v-out-frt AS DEC NO-UNDO.
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
      WHERE fg-bin.company EQ cocode
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
      v-out-frt = v-pallets.                        
    END.

    OTHERWISE DO:                                         /* MSF */
      v-out-frt = itemfg.t-sqft * bf-oe-boll.qty / 1000.
    END.
  END CASE.
END.
END PROCEDURE.
