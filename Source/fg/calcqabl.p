/* Copy of calcqa&b but with location */
DEFINE INPUT  PARAMETER ip-rowid AS ROWID NO-UNDO.
DEFINE INPUT  PARAMETER ip-loc LIKE itemfg-loc.loc NO-UNDO.
DEFINE OUTPUT PARAMETER op-q-alloc LIKE itemfg.q-alloc NO-UNDO.
DEFINE OUTPUT PARAMETER op-q-back LIKE itemfg.q-back NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEFINE VARIABLE ld-qty LIKE oe-ordl.t-ship-qty NO-UNDO.

FIND itemfg NO-LOCK WHERE 
    ROWID(itemfg) EQ ip-rowid 
    NO-ERROR.

IF AVAILABLE itemfg THEN DO:
    FIND FIRST oe-ctrl NO-LOCK WHERE 
        oe-ctrl.company EQ itemfg.company 
        NO-ERROR.

    /*** itemfg.q-alloc & itemfg.q-back from customer orders ***/
    IF AVAILABLE oe-ctrl THEN FOR EACH oe-ordl NO-LOCK WHERE 
        oe-ordl.company EQ itemfg.company AND 
        oe-ordl.opened  EQ YES AND 
        oe-ordl.i-no    EQ itemfg.i-no AND 
        oe-ordl.stat    NE "C" AND 
        CAN-FIND(FIRST oe-ord OF oe-ordl WHERE 
            oe-ord.type NE "T" AND 
            oe-ord.deleted = NO)      
        USE-INDEX item:
        IF oe-ordl.is-a-component THEN DO:
            FIND FIRST fg-set NO-LOCK WHERE 
                fg-set.company EQ oe-ordl.company AND 
                fg-set.part-no EQ oe-ordl.i-no
                NO-ERROR.
            IF AVAILABLE fg-set THEN DO:
                FIND FIRST itemfg NO-LOCK WHERE 
                    itemfg.company EQ oe-ordl.company AND 
                    itemfg.i-no    EQ fg-set.set-no
                    NO-ERROR.
                IF AVAILABLE itemfg 
                AND itemfg.alloc NE YES THEN 
                    NEXT.
            END.
        END. /* oe-ordl.is-a-component */
        ASSIGN 
            ld-qty = 0.

        IF oe-ctrl.u-inv THEN FOR EACH oe-boll NO-LOCK WHERE 
            oe-boll.company EQ oe-ordl.company AND 
            oe-boll.ord-no  EQ oe-ordl.ord-no AND 
            oe-boll.i-no    EQ oe-ordl.i-no AND 
            oe-boll.loc     EQ ip-loc AND 
            oe-boll.line    EQ oe-ordl.line AND 
            oe-boll.s-code  NE "T" AND 
            CAN-FIND(FIRST oe-bolh WHERE 
                oe-bolh.b-no   EQ oe-boll.b-no AND 
                oe-bolh.posted EQ YES
                USE-INDEX b-no)
            USE-INDEX ord-no:  
            ASSIGN 
                ld-qty = ld-qty + oe-boll.qty.
        END. /* EACH oe-boll */       
        ELSE ASSIGN 
            ld-qty = oe-ordl.ship-qty.

        FOR EACH oe-rel NO-LOCK WHERE 
            oe-rel.company EQ oe-ordl.company AND 
            oe-rel.ord-no EQ oe-ordl.ord-no AND 
            oe-rel.LINE   EQ oe-ordl.LINE AND 
            oe-rel.stat NE "Z" AND 
            oe-rel.stat NE "C" AND 
            oe-rel.spare-char-1 EQ ip-loc:            
            ASSIGN 
                op-q-alloc = op-q-alloc + oe-rel.tot-qty /* oe-rel.spare-dec-1 */.
        END. /* EACH oe-rel */

        FOR EACH oe-rell NO-LOCK WHERE 
            oe-rell.company  EQ itemfg.company AND 
            oe-rell.ord-no   EQ oe-ordl.ord-no AND 
            oe-rell.i-no     EQ oe-ordl.i-no AND 
            oe-rell.loc      EQ ip-loc AND 
            oe-rell.line     EQ oe-ordl.line AND 
            oe-rell.b-ord-no NE 0 AND 
            CAN-FIND(FIRST oe-relh WHERE 
                oe-relh.r-no EQ oe-rell.r-no)
            USE-INDEX ord-no:

            FIND FIRST oe-boll NO-LOCK WHERE 
                oe-boll.company  EQ oe-rell.company AND 
                oe-boll.ord-no   EQ oe-rell.ord-no AND 
                oe-boll.line     EQ oe-rell.line AND 
                oe-boll.i-no     EQ oe-rell.i-no AND 
                oe-boll.loc      EQ oe-rell.loc AND 
                oe-boll.r-no     EQ oe-rell.r-no AND 
                oe-boll.rel-no   EQ oe-rell.rel-no AND 
                oe-boll.b-ord-no EQ oe-rell.b-ord-no AND 
                oe-boll.po-no    EQ oe-rell.po-no AND 
                CAN-FIND(FIRST oe-bolh WHERE 
                    oe-bolh.b-no EQ oe-boll.b-no)
                USE-INDEX ord-no 
                NO-ERROR.

            IF NOT AVAILABLE oe-boll THEN ASSIGN 
                op-q-back = op-q-back + oe-rell.qty.
        END. /* EACH oe-rell */
    END. /* EACH oe-ordl */
END. /* AVAIL itemfg */
