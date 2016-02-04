
DEF INPUT  PARAM ip-rowid       AS   ROWID                  NO-UNDO.
DEF INPUT  PARAM ip-cust-no     LIKE oe-bolh.cust-no        NO-UNDO.
DEF INPUT  PARAM ip-ship-id     LIKE oe-bolh.ship-id        NO-UNDO.
DEF INPUT  PARAM ip-carrier     LIKE oe-bolh.carrier        NO-UNDO.
DEF OUTPUT PARAM op-freight     AS   DEC DECIMALS 10        NO-UNDO.
MESSAGE "Internal Error - Program out of Date"
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
RETURN.
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR v-frt-chg AS DEC NO-UNDO.


FIND FIRST oe-bolh WHERE ROWID(oe-bolh) EQ ip-rowid NO-LOCK NO-ERROR.
    
IF AVAIL oe-bolh THEN DO:
  FOR EACH oe-boll EXCLUSIVE-LOCK
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no:

    RUN oe/bollfrat.p (ROWID(oe-boll),
                       ip-cust-no,
                       ip-ship-id,
                       ip-carrier,
                       OUTPUT v-frt-chg). 
    IF v-frt-chg GT 0 THEN
      oe-boll.freight = v-frt-chg.
    op-freight = op-freight + v-frt-chg.
  END.

  IF op-freight EQ ? THEN op-freight = 0.

  FIND FIRST shipto NO-LOCK
      WHERE shipto.company EQ oe-bolh.company
        AND shipto.cust-no EQ ip-cust-no
        AND shipto.ship-id EQ ip-ship-id
      NO-ERROR.

  IF AVAIL shipto AND shipto.del-chg NE 0 THEN DO:
    RUN oe/bolfrteq.p (BUFFER oe-bolh, (op-freight + shipto.del-chg), op-freight).
    op-freight = op-freight + shipto.del-chg.
  END.
END.
