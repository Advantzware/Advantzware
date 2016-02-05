/* --------------------------------------------- sys/inc/po-recqa.p 04/01 JLF */
/* Calculate Qty & Amt Received for a PO Line Item                            */
/* -------------------------------------------------------------------------- */

DEF INPUT  PARAM ip-recid AS   RECID NO-UNDO.
DEF OUTPUT PARAM op-qty   AS   DEC NO-UNDO.
DEF OUTPUT PARAM op-amt   AS   DEC NO-UNDO.

DEF VAR v-wid AS DEC NO-UNDO.
DEF VAR v-len AS DEC NO-UNDO.
DEF VAR v-dep AS DEC NO-UNDO.
DEF VAR v-bwt AS DEC NO-UNDO.
DEF VAR v-qty AS DEC NO-UNDO.
DEF VAR ld-sheeter AS DEC NO-UNDO.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}


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
    FOR EACH rm-rcpth
        WHERE rm-rcpth.company   EQ po-ord.company
          AND rm-rcpth.vend-no   EQ po-ord.vend-no
          AND rm-rcpth.po-no     EQ TRIM(STRING(po-ord.po-no,">>>>>>>>>>"))
          AND rm-rcpth.rita-code EQ "I"
        USE-INDEX vend NO-LOCK,
        EACH rm-rdtlh
        WHERE rm-rdtlh.r-no             EQ rm-rcpth.r-no
          AND rm-rdtlh.rita-code        EQ rm-rcpth.rita-code
          /*AND SUBSTR(rm-rdtlh.BOL,1,30) EQ po-ordl.i-no
          AND SUBSTR(rm-rdtlh.BOL,31,3) EQ STRING(po-ordl.line,"999")*/
        NO-LOCK:
    
      op-amt = op-amt - (rm-rdtlh.qty * rm-rdtlh.cost).
    END.
  END.

  ELSE
  FOR EACH fg-rcpth NO-LOCK
      WHERE fg-rcpth.company   EQ cocode
        AND fg-rcpth.i-no      EQ po-ordl.i-no
        AND fg-rcpth.po-no     EQ STRING(po-ordl.po-no)
        AND fg-rcpth.rita-code EQ "R"
      USE-INDEX item-po,
          
      EACH fg-rdtlh NO-LOCK
      WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:

    ASSIGN  
     op-qty = op-qty + fg-rdtlh.qty
     op-amt = op-amt + (fg-rdtlh.qty / 1000 * fg-rdtlh.cost).
  END.
  
