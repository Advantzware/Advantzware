
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-qty AS DEC DECIMALS 10 NO-UNDO.

{sys/inc/var.i SHARED}

DEF VAR v-len AS DEC NO-UNDO.
DEF VAR v-wid AS DEC NO-UNDO.
DEF VAR v-dep AS DEC NO-UNDO.
DEF VAR v-bwt AS DEC NO-UNDO.


FIND po-ordl WHERE ROWID(po-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL po-ordl THEN DO:
  op-qty = po-ordl.t-rec-qty.
        
  IF po-ordl.item-type THEN DO:
     FIND FIRST item 
         WHERE item.company EQ po-ordl.company
           AND item.i-no    EQ po-ordl.i-no
         NO-LOCK NO-ERROR. 
     IF AVAIL item THEN DO:
       v-dep = item.s-dep.          
       {po/pol-dims.i}
    
/*##BL -Do not understand this use of order qty rather than paying for what was received*/
/*##BL -This particular function appears to just check that what was received, paid*/
/*        IF item.i-code EQ "R" AND     */
/*           item.stocked EQ NO THEN    */
/*           op-qty = po-ordl.cons-qty. */
     END.
    
     IF po-ordl.cons-uom NE po-ordl.pr-qty-uom THEN
        RUN sys/ref/convquom.p(po-ordl.cons-uom, po-ordl.pr-qty-uom,
                               v-bwt, v-len, v-wid, v-dep,
                               op-qty, OUTPUT op-qty).
  END.
         
  ELSE
    IF po-ordl.pr-qty-uom NE "EA" THEN
      RUN sys/ref/convquom.p("EA", po-ordl.pr-qty-uom,
                             0, 0, 0, 0,
                             op-qty, OUTPUT op-qty).
                                   
  op-qty = op-qty - po-ordl.t-inv-qty.
END.
