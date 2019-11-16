/* --------------------------------------------- sys/inc/po-recqa2.p  */
/* Calculate Qty & Amt Received for a PO Line Item                            */
/* -------------------------------------------------------------------------- */

DEF INPUT  PARAM ip-recid AS RECID NO-UNDO.
DEF INPUT  PARAM ip-from-date  AS DATE NO-UNDO.
DEF INPUT  PARAM ip-to-date AS DATE NO-UNDO.
DEF OUTPUT PARAM op-qty   AS DEC NO-UNDO.
DEF OUTPUT PARAM op-amt   AS DEC NO-UNDO.

DEF VAR v-wid AS DEC NO-UNDO.
DEF VAR v-len AS DEC NO-UNDO.
DEF VAR v-dep AS DEC NO-UNDO.
DEF VAR v-bwt AS DEC NO-UNDO.
DEF VAR v-qty AS DEC NO-UNDO.
DEF VAR ld-sheeter AS DEC NO-UNDO.
DEFINE VARIABLE dv-t-cost AS DECIMAL NO-UNDO.
DEFINE VARIABLE dv-ord-qty               AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cfg-uom-list        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCkeckRec          AS LOGICAL INIT NO NO-UNDO .
DEFINE NEW SHARED VARIABLE v-basis-w AS DECIMAL NO-UNDO.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}
RUN sys/ref/uom-fg.p (?, OUTPUT cfg-uom-list).

FIND po-ordl WHERE RECID(po-ordl) EQ ip-recid NO-LOCK NO-ERROR.
                     
IF AVAIL po-ordl THEN
FIND FIRST po-ord NO-LOCK
    WHERE po-ord.company EQ cocode
      AND po-ord.po-no   EQ po-ordl.po-no
    NO-ERROR.

IF AVAIL po-ord THEN
  IF po-ordl.item-type THEN DO:
    FOR EACH rm-rcpth NO-LOCK
        WHERE rm-rcpth.company     EQ cocode
          AND rm-rcpth.i-no        EQ po-ordl.i-no
          AND rm-rcpth.po-no       EQ STRING(po-ordl.po-no)
          AND rm-rcpth.job-no      EQ po-ordl.job-no
          AND rm-rcpth.job-no2     EQ po-ordl.job-no2
          AND rm-rcpth.rita-code   EQ "R"
          AND rm-rcpth.trans-date  GE ip-from-date
          AND rm-rcpth.trans-date  LE ip-to-date
        USE-INDEX item-po,
      
      EACH rm-rdtlh NO-LOCK
      WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
        AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
        AND (rm-rdtlh.s-num    EQ po-ordl.s-num OR po-ordl.s-num EQ 0):

      ASSIGN
       v-bwt = 0
       v-len = po-ordl.s-len
       v-wid = po-ordl.s-wid
       v-dep = 0.

      FIND FIRST item NO-LOCK
          WHERE item.company EQ cocode
            AND item.i-no    EQ po-ordl.i-no
          NO-ERROR. 

      IF AVAIL item THEN DO:
        v-dep = item.s-dep.          
        {po/pol-dims.i}
      END.

      v-qty = rm-rdtlh.qty.

      IF rm-rcpth.pur-uom NE po-ordl.pr-qty-uom THEN
        RUN sys/ref/convquom.p (rm-rcpth.pur-uom, po-ordl.pr-qty-uom,
                                v-bwt, v-len, v-wid, v-dep,
                                v-qty, OUTPUT v-qty).

      ASSIGN
       op-qty = op-qty + v-qty
       op-amt = op-amt + (rm-rdtlh.qty * rm-rdtlh.cost) + rm-rdtlh.frt-cost.
    END.

    IF po-ord.type EQ "S" THEN
    FOR EACH rm-rcpth FIELDS(r-no rita-code)
        WHERE rm-rcpth.company   EQ po-ord.company
          AND rm-rcpth.vend-no   EQ po-ord.vend-no
          AND rm-rcpth.po-no     EQ TRIM(STRING(po-ord.po-no,">>>>>>>>>>"))
          AND rm-rcpth.rita-code EQ "I"
          AND rm-rcpth.trans-date  GE ip-from-date
          AND rm-rcpth.trans-date  LE ip-to-date
        USE-INDEX vend NO-LOCK,
        EACH rm-rdtlh FIELDS(qty cost)
        WHERE rm-rdtlh.r-no             EQ rm-rcpth.r-no
          AND rm-rdtlh.rita-code        EQ rm-rcpth.rita-code
          /*AND SUBSTR(rm-rdtlh.BOL,1,30) EQ po-ordl.i-no
          AND SUBSTR(rm-rdtlh.BOL,31,3) EQ STRING(po-ordl.line,"999")*/
        NO-LOCK:
    
      op-amt = op-amt - (rm-rdtlh.qty * rm-rdtlh.cost).
    END.
  END.

  ELSE DO:
    ASSIGN  op-qty = po-ordl.t-rec-qty  .
    ASSIGN
      v-basis-w = IF AVAIL item THEN item.basis-w ELSE v-basis-w
      v-dep = IF AVAIL item THEN item.s-dep ELSE v-dep
      v-len = DEC(po-ordl.s-len)
      v-wid = DEC(po-ordl.s-wid)
      dv-ord-qty = DEC(po-ordl.ord-qty)
       .
       
        FIND FIRST itemfg NO-LOCK
           WHERE itemfg.company EQ cocode
           AND itemfg.i-no    EQ po-ordl.i-no
           NO-ERROR.
          
      FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
          WHERE fg-rcpth.company   EQ cocode
            AND fg-rcpth.i-no      EQ po-ordl.i-no
            AND fg-rcpth.po-no     EQ STRING(po-ordl.po-no)
            AND fg-rcpth.rita-code EQ "R"
            AND fg-rcpth.trans-date  GE ip-from-date
            AND fg-rcpth.trans-date  LE ip-to-date
          USE-INDEX item-po,
              
          EACH fg-rdtlh FIELDS(qty cost) NO-LOCK
          WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
            AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
    
        ASSIGN  
          lCkeckRec = YES .
        /* op-qty = op-qty + fg-rdtlh.qty
         op-amt = op-amt + (fg-rdtlh.qty / 1000 * fg-rdtlh.cost).*/
      END.

 IF lCkeckRec THEN do:
  IF LOOKUP( po-ordl.pr-uom,"L,LOT") GT 0 THEN
    dv-t-cost = ( po-ordl.cost +  po-ordl.setup) *
                IF op-qty LT 0 THEN -1 ELSE 1.

  ELSE DO: 
    dv-ord-qty = op-qty .
    /* Get quantity in the same UOM as the cost */
    IF  po-ordl.pr-qty-uom NE  po-ordl.pr-uom     AND
       (po-ordl.item-type                                 OR
        LOOKUP( po-ordl.pr-qty-uom,cfg-uom-list) EQ 0 OR
        LOOKUP( po-ordl.pr-uom,cfg-uom-list)     EQ 0)  THEN DO:
       
      IF po-ordl.pr-qty-uom EQ "CS" AND AVAIL(itemfg) THEN DO:
        /* Convert quantity to EA */
        dv-ord-qty = dv-ord-qty * itemfg.case-count.
       
        RUN sys/ref/convquom.p("EA",
                               po-ordl.pr-uom,
                               v-basis-w, v-len, v-wid, v-dep,
                               dv-ord-qty, OUTPUT dv-ord-qty).
       

      END.
      ELSE DO:
        IF po-ordl.pr-uom EQ "CS" THEN DO:
          /* Convert qty to EA */
          RUN sys/ref/convquom.p( po-ordl.pr-qty-uom,
                       "EA",
                       v-basis-w, v-len, v-wid, v-dep,
                       dv-ord-qty, OUTPUT dv-ord-qty).
          /* Convert to Cases */
          dv-ord-qty = dv-ord-qty / itemfg.case-count.
        END.
        ELSE DO:
           
          RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,
                                 po-ordl.pr-uom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 dv-ord-qty, OUTPUT dv-ord-qty).
        
        END.
      END.
    END.
    
    dv-t-cost = (dv-ord-qty *  po-ordl.cost) +  po-ordl.setup.
  END.

  IF  po-ordl.disc NE 0 THEN dv-t-cost = dv-t-cost * (1 - ( po-ordl.disc / 100)).

  ASSIGN
     op-amt = (dv-t-cost) .

 END.  /* lCheckRes = yes */
  
END. /* else do*/
