/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       If there is a minimum, returns it in op-min so the caller
               can deal with it 
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ip-company     AS   CHAR                   NO-UNDO.
DEF INPUT  PARAM ip-loc         LIKE loc.loc                NO-UNDO.
DEF INPUT  PARAM ip-carrier     LIKE carrier.carrier        NO-UNDO.
DEF INPUT  PARAM ip-del-zone    LIKE carr-mtx.del-zone      NO-UNDO.
DEF INPUT  PARAM ip-zip-code    LIKE shipto.ship-zip        NO-UNDO.
DEF INPUT  PARAM ip-qty         AS   DEC DECIMALS 10        NO-UNDO.
DEF INPUT  PARAM ip-tot-qty         AS   DEC DECIMALS 10        NO-UNDO.
DEF INPUT  PARAM ip-rels        AS   INT                    NO-UNDO.
DEF OUTPUT PARAM op-freight     AS   DEC DECIMALS 10        NO-UNDO.
DEF OUTPUT PARAM op-min         AS   DEC DECIMALS 10        NO-UNDO.
DEF OUTPUT PARAM op-mtxrate     AS   DEC DECIMALS 10        NO-UNDO.
DEF VAR v-zip-code AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.

/* {custom/globdefs.i}        */
/*                            */
/* {sys/inc/var.i NEW SHARED} */



FIND FIRST carr-mtx
    WHERE carr-mtx.company  EQ ip-company
      AND carr-mtx.loc      EQ ip-loc
      AND carr-mtx.carrier  EQ ip-carrier
      AND carr-mtx.del-zone EQ ip-del-zone
      AND carr-mtx.del-zip  EQ ip-zip-code
    NO-LOCK NO-ERROR.
IF AVAIL carr-mtx THEN
  v-zip-code = ip-zip-code.
ELSE
  v-zip-code = "".

FOR FIRST carr-mtx
    WHERE carr-mtx.company  EQ ip-company
      AND carr-mtx.loc      EQ ip-loc
      AND carr-mtx.carrier  EQ ip-carrier
      AND carr-mtx.del-zone EQ ip-del-zone
      AND (IF v-zip-code GT "" THEN carr-mtx.del-zip = v-zip-code
                               ELSE TRUE)
    NO-LOCK,

    FIRST carrier OF carr-mtx NO-LOCK:

  DO li = 1 TO 10:
    op-freight = carr-mtx.rate[li] * ip-qty.
    op-mtxrate = carr-mtx.rate[li].
    IF carr-mtx.weight[li] GE ip-tot-qty THEN LEAVE.
  END.

  IF carrier.chg-method EQ "W" THEN op-freight = op-freight / 100.

  IF op-freight LT carr-mtx.min-rate THEN op-min = carr-mtx.min-rate.
      
  /* op-freight = op-freight + (carr-mtx.min-rate * (ip-rels - 1)). */
END.

