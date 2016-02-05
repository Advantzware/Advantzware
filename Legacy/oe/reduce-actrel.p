/* Reduce oe-rel.qty value when deleting an oe-boll line */
DEF INPUT PARAM iprRelRow AS ROWID NO-UNDO.
DEF INPUT PARAM iprRellRow AS ROWID NO-UNDO.
DEF INPUT PARAM ipiOrigQty AS INT NO-UNDO.
DEF VAR v-new-qty AS INT NO-UNDO.
     
FIND oe-rel WHERE ROWID(oe-rel) EQ iprRelRow EXCLUSIVE-LOCK.

/* Looking for oe-rell records returned to their original */
/* unposted state after delete of oe-boll                 */
v-new-qty = 0.
FOR EACH oe-rell
  WHERE oe-rell.company  EQ oe-rel.company
    AND oe-rell.r-no     EQ oe-rel.link-no
    AND oe-rell.ord-no   EQ oe-rel.ord-no
    AND oe-rell.i-no     EQ oe-rel.i-no
    AND oe-rell.line     EQ oe-rel.line
    AND oe-rell.rel-no   EQ oe-rel.rel-no
    AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
    AND oe-rell.po-no    EQ oe-rel.po-no
    AND (IF iprRellRow NE ? THEN ROWID(oe-rell) EQ iprRellRow ELSE TRUE)
    AND oe-rell.posted   EQ NO
  USE-INDEX r-no
  BREAK BY oe-rell.r-no:
 
    IF LAST(oe-rell.r-no) THEN LEAVE.
END.

IF AVAIL oe-rell THEN
  v-new-qty = v-new-qty + oe-rell.qty.  

oe-rel.qty = oe-rel.qty - (ipiOrigQty - v-new-qty).

      

