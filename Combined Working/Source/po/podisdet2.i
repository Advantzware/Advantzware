/* po/podisdet2.i */

  DEF VAR v-out-qty AS DEC NO-UNDO.
  DEF VAR lv-cons-qty AS DEC NO-UNDO.
  DEF VAR lv-cons-cost AS DEC NO-UNDO.
  DEF VAR lv-t-cost AS DEC NO-UNDO.
  DEF VAR ll-ea AS LOG INIT NO NO-UNDO.
  DEF VAR lv-uom LIKE po-ordl.pr-qty-uom INIT NO NO-UNDO.
  DEF VAR lv-orig-uom AS CHAR NO-UNDO.

  {ce/msfcalc.i}

  RELEASE item.
  RELEASE itemfg.
  IF po-ordl.item-type THEN DO:
  
    FIND FIRST item
        WHERE item.company EQ cocode
          AND item.i-no    EQ po-ordl.i-no{2}
        NO-LOCK NO-ERROR.
  END.
    ASSIGN
      v-basis-w = IF AVAIL item THEN item.basis-w ELSE v-basis-w
      v-dep = IF AVAIL item THEN item.s-dep ELSE v-dep
      v-len = DEC(po-ordl.s-len{2})
      v-wid = DEC(po-ordl.s-wid{2})
      v-ord-qty = DEC(po-ordl.ord-qty{2})
      lv-orig-uom = po-ordl.pr-qty-uom{2}
      {po/calc10.i v-len}
      {po/calc10.i v-wid}.
  
  IF NOT AVAIL item THEN
  FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ po-ordl.i-no{2}
      NO-LOCK NO-ERROR.

  IF AVAIL itemfg THEN
    RUN sys/ref/ea-um-fg.p (po-ordl.pr-qty-uom{2}, OUTPUT ll-ea).

  IF ll-ea AND ip-type NE "view" THEN
    ASSIGN
     lv-uom                = po-ordl.pr-qty-uom{2}
     po-ordl.pr-qty-uom{2} = "EA".

  IF v-len EQ 0 AND AVAIL ITEM AND
     ITEM.i-code EQ "R" AND item.r-wid GT 0 THEN
     DO:
        v-len = 12.

        IF lv-orig-uom EQ "ROLL" THEN
        DO:
           FIND FIRST uom WHERE
                uom.uom EQ "ROLL"
                NO-LOCK NO-ERROR.

           IF AVAIL uom AND ip-type NE "view" THEN
              ASSIGN
                 po-ordl.pr-qty-uom{2} = "LF".
                 v-ord-qty = v-ord-qty * uom.mult.
        END.
     END.
  
  IF po-ordl.pr-qty-uom{2} EQ "EA"       OR
     (NOT po-ordl.item-type AND
      LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) GT 0
      AND po-ordl.pr-qty-uom{2} NE "CS") THEN DO:
 

     v-tot-msf = IF v-corr THEN ((v-len * v-wid * .007 * dec(po-ordl.ord-qty{2})) / 1000)
                           ELSE ((((v-len * v-wid) / 144) * dec(po-ordl.ord-qty{2})) / 1000).
  END.
  ELSE DO:
     /*convert whatever the UOM is into "EACH" first*/     
     v-tot-msf = 0.
     IF po-ordl.pr-qty-uom{2} NE "EA" THEN DO:
        v-tot-msf = 0.
        IF po-ordl.pr-qty-uom{2} EQ "CS" THEN DO:
           IF AVAIL(itemfg) THEN
              v-out-qty = v-ord-qty * itemfg.case-count.           
        END.
        ELSE DO:
        
          RUN sys/ref/convquom.p(po-ordl.pr-qty-uom{2},
                                 "EA",
                                 v-basis-w,
                                 v-len,
                                 v-wid,
                                 v-dep,
                                 v-ord-qty,
                                 OUTPUT v-out-qty).
        END.
        /*now convert from "EACH" into MSF*/   
        v-tot-msf = IF v-corr THEN
                       ((v-len * v-wid * .007 * v-out-qty) / 1000)
                    ELSE
                       ((((v-len * v-wid) / 144) * v-out-qty) / 1000).
        
          IF po-ordl.pr-qty-uom{2} EQ "ROLL" THEN
             v-tot-msf = v-tot-msf * (12 / v-len).
     END. 
  END.

  lv-cons-qty = v-ord-qty.
  
  IF po-ordl.cons-uom{2} NE po-ordl.pr-qty-uom{2} AND
     (po-ordl.item-type                           OR
      LOOKUP(po-ordl.cons-uom{2},fg-uom-list)   EQ 0 OR
      LOOKUP(po-ordl.pr-qty-uom{2},fg-uom-list) EQ 0)       THEN DO:
    
    IF (po-ordl.pr-qty-uom{2} EQ "CS" 
        /* OR po-ordl.spare-int-1 EQ 1 */) AND AVAIL(itemfg) THEN DO:

        /* for CS, convert to EA first */      
        lv-cons-qty = lv-cons-qty * itemfg.case-count.

        RUN sys/ref/convquom.p (INPUT "EA", 
                           INPUT ({3} po-ordl.cons-uom),
                           v-basis-w, v-len, v-wid, v-dep,
                           lv-cons-qty,
                           OUTPUT lv-cons-qty).    
        
    END.
    ELSE DO:
     
        RUN sys/ref/convquom.p(INPUT ({3} po-ordl.pr-qty-uom),
                   INPUT ({3} po-ordl.cons-uom),
                   v-basis-w, v-len, v-wid, v-dep,
                   lv-cons-qty,
                   OUTPUT lv-cons-qty).
    END.

  END.
  IF ip-type NE "view" THEN 
    po-ordl.cons-qty{2} = {1}(lv-cons-qty).

  /**  Calculate Extended cost, order quantity is based on UOM **/
  IF LOOKUP({3} po-ordl.pr-uom,"L,LOT") GT 0 THEN
    lv-t-cost = ({3} po-ordl.cost + {3} po-ordl.setup) *
                IF po-ordl.ord-qty LT 0 THEN -1 ELSE 1.

  ELSE DO: 
    v-ord-qty = {3} po-ordl.ord-qty.
    
    /* Get quantity in the same UOM as the cost */
    IF {3} po-ordl.pr-qty-uom NE {3} po-ordl.pr-uom     AND
       (po-ordl.item-type                                 OR
        LOOKUP({3} po-ordl.pr-qty-uom,fg-uom-list) EQ 0 OR
        LOOKUP({3} po-ordl.pr-uom,fg-uom-list)     EQ 0)  THEN DO:
       
      IF po-ordl.pr-qty-uom{2} EQ "CS" AND AVAIL(itemfg) THEN DO:
        /* Convert quantity to EA */
        v-ord-qty = v-ord-qty * itemfg.case-count.
       
        RUN sys/ref/convquom.p("EA",
                               po-ordl.pr-uom{2},
                               v-basis-w, v-len, v-wid, v-dep,
                               v-ord-qty, OUTPUT v-ord-qty).
       

      END.
      ELSE DO:
        IF po-ordl.pr-uom{2} EQ "CS" THEN DO:
          /* Convert qty to EA */
          RUN sys/ref/convquom.p( po-ordl.pr-qty-uom{2},
                       "EA",
                       v-basis-w, v-len, v-wid, v-dep,
                       v-ord-qty, OUTPUT v-ord-qty).
          /* Convert to Cases */
          v-ord-qty = v-ord-qty / itemfg.case-count.
        END.
        ELSE DO:
           
          RUN sys/ref/convquom.p(po-ordl.pr-qty-uom{2},
                                 po-ordl.pr-uom{2},
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-ord-qty, OUTPUT v-ord-qty).
        
        END.
      END.
    END.
    
    lv-t-cost = (v-ord-qty * {3} po-ordl.cost) + {3} po-ordl.setup.
  END.
IF ip-type NE "view" THEN
  po-ordl.cons-cost{2} = {1}(lv-t-cost / lv-cons-qty).

  IF {3} po-ordl.disc NE 0 THEN lv-t-cost = lv-t-cost * (1 - ({3} po-ordl.disc / 100)).
IF ip-type NE "view" THEN
  ASSIGN
     po-ordl.t-cost{2} = {1}(lv-t-cost)
     po-ordl.pr-qty-uom{2} = lv-orig-uom.

  IF ll-ea AND ip-type NE "view" THEN po-ordl.pr-qty-uom{2} = lv-uom.

