/* po/podisdet2.i */

  DEF VAR v-out-qty AS DEC NO-UNDO.
  DEF VAR lv-cons-qty AS DEC NO-UNDO.
  DEF VAR lv-cons-cost AS DEC NO-UNDO.
  DEF VAR lv-t-cost AS DEC NO-UNDO.
  DEF VAR ll-ea AS LOG INIT NO NO-UNDO.
  DEF VAR lv-uom LIKE po-ordl.pr-qty-uom INIT NO NO-UNDO.
  DEF VAR lv-orig-uom AS CHAR NO-UNDO.
  DEF VAR v-basis-w AS DEC NO-UNDO.
  DEF VAR v-dep AS DEC NO-UNDO.
  DEF VAR v-len AS DEC NO-UNDO.
  DEF VAR v-wid AS DEC NO-UNDO.
  DEF VAR v-ord-qty AS DEC NO-UNDO.
  def NEW shared var factor# as decimal no-undo.
  DEFINE VARIABLE v-tot-msf AS DECIMAL NO-UNDO.

  {ce/msfcalc.i}

  RELEASE item.
  RELEASE itemfg.

  FIND FIRST item
      WHERE item.company EQ cocode
        AND item.i-no    EQ prmino
      NO-LOCK NO-ERROR.

  ASSIGN
    v-basis-w = IF AVAIL item THEN item.basis-w ELSE 0
    v-dep = IF AVAIL item THEN item.s-dep ELSE 0
    v-len = DEC(prmslen)
    v-wid = DEC(prmswid)
    v-ord-qty = DEC(prmqty)
    lv-orig-uom = prmprqtyuom
    /*{po/calc10.i v-len}
    {po/calc10.i v-wid}*/ .

  IF NOT AVAIL item THEN
  FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ prmino
      NO-LOCK NO-ERROR.

  IF AVAIL itemfg THEN
    RUN sys/ref/ea-um-fg.p (prmprqtyuom, OUTPUT ll-ea).

  IF ll-ea THEN
    ASSIGN
     lv-uom         = prmprqtyuom
     prmprqtyuom = "EA".

  IF v-len EQ 0 AND AVAIL ITEM AND
     ITEM.i-code EQ "R" AND item.r-wid GT 0 THEN
     DO:
        v-len = 12.

        IF lv-orig-uom EQ "ROLL" THEN
        DO:
           FIND FIRST uom WHERE
                uom.uom EQ "ROLL"
                NO-LOCK NO-ERROR.

           IF AVAIL uom THEN
              ASSIGN
                 prmprqtyuom = "LF".
                 v-ord-qty = v-ord-qty * uom.mult.
        END.
     END.

  IF prmprqtyuom EQ "EA"       OR
     (NOT po-ordl.item-type AND
      LOOKUP(prmprqtyuom,fg-uom-list) GT 0) THEN
     v-tot-msf = IF v-corr THEN ((v-len * v-wid * .007 * dec(prmqty)) / 1000)
                           ELSE ((((v-len * v-wid) / 144) * dec(prmqty)) / 1000).
  else do:
     /*convert whatever the UOM is into "EACH" first*/
     
     v-tot-msf = 0.
     if prmprqtyuom NE "EA" then do:
        v-tot-msf = 0.
        run sys/ref/convquom.p(prmprqtyuom,
                               "EA",
                               v-basis-w,
                               v-len,
                               v-wid,
                               v-dep,
                               v-ord-qty,
                               output v-out-qty).

        /*now convert from "EACH" into MSF*/   
        v-tot-msf = if v-corr THEN
                       ((v-len * v-wid * .007 * v-out-qty) / 1000)
                    else
                       ((((v-len * v-wid) / 144) * v-out-qty) / 1000).

          IF prmprqtyuom EQ "ROLL" THEN
             v-tot-msf = v-tot-msf * (12 / v-len).
     end. 
  end.
  

  lv-cons-qty = v-ord-qty.
  IF prmconsuom NE prmprqtyuom AND
     (po-ordl.item-type                           OR
      LOOKUP(prmconsuom,fg-uom-list)   EQ 0 OR
      LOOKUP(prmprqtyuom,fg-uom-list) EQ 0)       THEN
    RUN sys/ref/convquom.p(INPUT (prmprqtyuom),
                           INPUT (prmconsuom),
                           v-basis-w, v-len, v-wid, v-dep,
                           lv-cons-qty,
                           OUTPUT lv-cons-qty).     
  ttb_cost_qty_po.poconsqty = round(DEC(lv-cons-qty),4).

  

  /**  Calculate Extended cost, order quantity is based on UOM **/
  IF LOOKUP(prmpruom,"L,LOT") GT 0 THEN
    lv-t-cost = (prmcost + prmsetup) *
                IF prmqty LT 0 THEN -1 ELSE 1.

  ELSE DO:
    v-ord-qty = prmqty.

    IF prmprqtyuom NE prmpruom     AND
       (po-ordl.item-type                                 OR
        LOOKUP(prmprqtyuom,fg-uom-list) EQ 0 OR
        LOOKUP(prmpruom,fg-uom-list)     EQ 0)  THEN
   
      RUN sys/ref/convquom.p(prmprqtyuom,
                             prmpruom,
                             v-basis-w, v-len, v-wid, v-dep,
                             v-ord-qty, OUTPUT v-ord-qty).
     
    lv-t-cost = (v-ord-qty * prmcost) + prmsetup.
    
  END.

  ttb_cost_qty_po.poconscost = ROUND((lv-t-cost / lv-cons-qty),5).
 
  IF prmdiscount NE 0 THEN lv-t-cost = lv-t-cost * (1 - (prmdiscount / 100)).

  ASSIGN
     ttb_cost_qty_po.pototcost = round(lv-t-cost,4)
     ttb_cost_qty_po.poprqtyuom = lv-orig-uom
     ttb_cost_qty_po.poqty = v-ord-qty
     ttb_cost_qty_po.pototmsf = round(v-tot-msf,4)
    .
  IF ll-ea THEN prmprqtyuom = lv-uom.
