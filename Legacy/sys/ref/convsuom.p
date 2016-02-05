/* --------------------------------------------------- sys/ref/convquom.p 9/94*/
/* UOM Conversion from one Unit to Another for Board Primarily.               */
/* Converts qty based on square feet                                              */
/* INPUT:  from-uom, to-uom, square-feet/sheet, from-qty (in from-uom)              */
/* OUTPUT: to-qty (in to-uom)                                                      */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER v-ip-fr-uom LIKE job-mat.sc-uom NO-UNDO.
DEFINE INPUT PARAMETER v-ip-to-uom LIKE job-mat.sc-uom NO-UNDO.
DEFINE INPUT PARAMETER v-ip-sq-feet AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER v-ip-in-qty AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER v-op-out-qty AS DECIMAL NO-UNDO.

DO:
  /* Convert from UOM to SHEETS */
  IF v-ip-fr-uom BEGINS "MSH" THEN
  v-ip-in-qty = v-ip-in-qty * 1000.
  ELSE
  IF v-ip-fr-uom BEGINS "MSF" THEN
  v-ip-in-qty = (1000 * v-ip-in-qty) / (v-ip-sq-feet).
  /************ Need basis weigth for this to work !
  ELSE
  IF v-ip-fr-uom BEGINS "TON" AND (v-ip-basis-w NE 0 AND
    v-ip-len NE 0 AND v-ip-wid NE 0) THEN
  v-ip-in-qty = (2000 * 1000 * v-ip-in-qty) /
  (v-ip-basis-w * (v-ip-sq-feet)).
  *************************************************/
  ELSE
  IF v-ip-fr-uom BEGINS "SF" THEN
  v-ip-in-qty = v-ip-in-qty / ((v-ip-sq-feet)).
  /******************* Need length for these to work !
  ELSE
  IF v-ip-fr-uom BEGINS "MLF" THEN
  v-ip-in-qty = (1000 * v-ip-in-qty) / (v-ip-len / 12.00).
  ELSE
  IF v-ip-fr-uom BEGINS "MLI" THEN
  v-ip-in-qty = (1000 * v-ip-in-qty) / v-ip-len.
  ELSE
  IF v-ip-fr-uom BEGINS "LF" THEN
  v-ip-in-qty = v-ip-in-qty / (v-ip-len / 12.00).
  *****************************************************/
  ELSE
  FROMUOM:
  REPEAT:
    /* put qty into an EA uom */
    FIND FIRST uom WHERE uom.uom = v-ip-fr-uom AND
      uom.mult NE 0 NO-LOCK NO-ERROR.
    IF AVAILABLE uom THEN
    DO:
      v-ip-in-qty = (IF v-ip-in-qty = 0 THEN
      1 ELSE
      v-ip-in-qty * uom.mult).
      IF uom.other NE "" AND uom.other NE uom.uom THEN
      DO:
        v-ip-fr-uom = uom.other.
        NEXT FROMUOM.
      END.
    END.
    ELSE
    v-ip-in-qty = (IF v-ip-in-qty = 0 THEN
    1 ELSE
    v-ip-in-qty).
    LEAVE FROMUOM.
  END.
END.
DO:
  /* Convert SHEETS to UOM */
  IF v-ip-to-uom BEGINS "MSH" THEN
  v-op-out-qty = v-ip-in-qty / 1000.
  ELSE
  IF v-ip-to-uom BEGINS "MSF" THEN
  v-op-out-qty = (((v-ip-sq-feet)) * v-ip-in-qty) / 1000.
  /********************************************
  ELSE
  IF v-ip-to-uom BEGINS "TON" AND (v-ip-basis-w NE 0 AND
    v-ip-len NE 0 AND v-ip-wid NE 0) THEN
  v-op-out-qty = (((((v-ip-sq-feet)) * v-ip-in-qty) / 1000)
  * v-ip-basis-w) / 2000.
  **********************************************/
  ELSE
  IF v-ip-to-uom BEGINS "SF" THEN
  v-op-out-qty = ((v-ip-sq-feet)) * v-ip-in-qty.
  /**********************************************
  ELSE
  IF v-ip-to-uom BEGINS "MLF" THEN
  v-op-out-qty = ((v-ip-len / 12.00) * v-ip-in-qty) / 1000.
  ELSE
  IF v-ip-to-uom BEGINS "MLI" THEN
  v-op-out-qty = (v-ip-len * v-ip-in-qty) / 1000.
  ELSE
  IF v-ip-to-uom BEGINS "LF" THEN
  v-op-out-qty = (v-ip-len / 12.00) * v-ip-in-qty.
  ************************************************/
  ELSE
  TOUOM:
  REPEAT:
    FIND FIRST uom WHERE uom.uom = v-ip-to-uom AND
      uom.mult NE 0 NO-LOCK NO-ERROR.
    IF AVAILABLE uom THEN
    DO:
      v-ip-in-qty = (IF v-ip-in-qty NE 0 THEN
      (v-ip-in-qty / uom.mult) ELSE
      0).
      IF uom.other NE "" AND uom.other NE uom.uom THEN
      DO:
        v-ip-to-uom = uom.other.
        NEXT TOUOM.
      END.
    END.
    v-op-out-qty = (IF v-ip-in-qty NE 0 THEN
    v-ip-in-qty
    ELSE
    1).
    LEAVE TOUOM.
  END.
END.
