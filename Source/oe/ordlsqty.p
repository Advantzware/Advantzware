
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-inv-qty LIKE oe-ordl.inv-qty NO-UNDO.
DEF OUTPUT PARAM op-ship-qty LIKE oe-ordl.ship-qty NO-UNDO.

DEF VAR lInvQty AS LOG NO-UNDO.
DEF BUFFER b-oe-ordl FOR oe-ordl.


FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-rowid NO-LOCK NO-ERROR.


IF AVAIL oe-ordl THEN DO:
    /*to get more accurate invoice qty - 05281303*/
    lInvQty = NO.
    FOR EACH ar-invl
        WHERE ar-invl.company  EQ oe-ordl.company
          AND ar-invl.ord-no   EQ oe-ordl.ord-no
          AND ar-invl.i-no     EQ oe-ordl.i-no
/*           AND ar-invl.ord-line EQ oe-ordl.LINE */
        NO-LOCK:
        ASSIGN 
            lInvQty = YES
            op-inv-qty = op-inv-qty + ar-invl.inv-qty.
    END.
   /*#to get more accurate invoice qty - 05281303*/
   FOR EACH inv-head 
        WHERE inv-head.company EQ oe-ordl.company
          AND inv-head.cust-no EQ oe-ordl.cust-no
        NO-LOCK:
        FOR EACH inv-line 
            WHERE inv-line.r-no   EQ inv-head.r-no
              AND inv-line.ord-no EQ oe-ordl.ord-no
              AND inv-line.i-no   EQ oe-ordl.i-no
              AND inv-line.line   EQ oe-ordl.LINE
            NO-LOCK:
            ASSIGN 
                lInvQty = YES
                op-inv-qty = op-inv-qty + inv-line.inv-qty.
        END.            
    END.
    FOR EACH oe-boll
        WHERE oe-boll.company EQ oe-ordl.company
          AND oe-boll.ord-no  EQ oe-ordl.ord-no
          AND oe-boll.i-no    EQ oe-ordl.i-no
          AND oe-boll.line    EQ oe-ordl.line
          AND oe-boll.s-code  NE "T"
          AND CAN-FIND(FIRST oe-bolh
                       WHERE oe-bolh.b-no   EQ oe-boll.b-no
                         AND oe-bolh.posted EQ YES
                       USE-INDEX b-no)
        USE-INDEX ord-no NO-LOCK:


     /* From oe-bolp3.i */
        IF oe-boll.s-code NE "S" AND NOT oe-ordl.is-a-component AND NOT lInvQty THEN
            op-inv-qty = op-inv-qty + oe-boll.qty.
    
        IF (oe-boll.s-code NE "I"                                            OR
        CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})) THEN
            op-ship-qty = op-ship-qty + oe-boll.qty.
    END.
END.
