
DEF INPUT  PARAM ip-loc         LIKE loc.loc                NO-UNDO.
DEF INPUT  PARAM ip-carrier     LIKE carrier.carrier        NO-UNDO.
DEF INPUT  PARAM ip-del-zone    LIKE carr-mtx.del-zone      NO-UNDO.
DEF INPUT  PARAM ip-qty         AS   DEC DECIMALS 10        NO-UNDO.
DEF INPUT  PARAM ip-rels        AS   INT                    NO-UNDO.
DEF OUTPUT PARAM op-freight     AS   DEC DECIMALS 10        NO-UNDO.

/*{custom/globdefs.i}*/

/*{sys/inc/var.i NEW SHARED}*/
DEFINE SHARED VARIABLE g_company AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE g_loc AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE cocode AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE locode AS CHAR NO-UNDO.

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR li AS INT NO-UNDO.

FOR FIRST carr-mtx
    WHERE carr-mtx.company  EQ cocode
      AND carr-mtx.loc      EQ ip-loc
      AND carr-mtx.carrier  EQ ip-carrier
      AND carr-mtx.del-zone EQ ip-del-zone
    NO-LOCK,

    FIRST carrier OF carr-mtx NO-LOCK:

  DO li = 1 TO 10:
    op-freight = carr-mtx.rate[li] * ip-qty.
    IF carr-mtx.weight[li] GE ip-qty THEN LEAVE.
  END.

  IF carrier.chg-method EQ "W" THEN op-freight = op-freight / 100.

  IF op-freight LT carr-mtx.min-rate THEN op-freight = carr-mtx.min-rate.
      
  op-freight = op-freight + (carr-mtx.min-rate * (ip-rels - 1)).  
END.
