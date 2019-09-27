&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME oe-rell

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-itemfg-loc FOR itemfg-loc.

DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-q-alloc LIKE b-itemfg.q-alloc NO-UNDO.
DEF VAR lv-q-back LIKE b-itemfg.q-back NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.
DISABLE TRIGGERS FOR LOAD OF itemfg.
DISABLE TRIGGERS FOR LOAD OF oe-rel.


cocode = {&TABLENAME}.company.
{sys/inc/oereordr.i}

FIND FIRST oe-ordl NO-LOCK
    WHERE oe-ordl.company EQ {&TABLENAME}.company
      AND oe-ordl.ord-no  EQ {&TABLENAME}.ord-no
      AND oe-ordl.i-no    EQ {&TABLENAME}.i-no
      AND oe-ordl.line    EQ {&TABLENAME}.line
    USE-INDEX item-ord 
    NO-ERROR.
IF AVAILABLE oe-ordl 
    AND oe-rell.s-code  NE "I"
    AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
    AND NOT CAN-FIND(FIRST oe-boll
                        WHERE oe-boll.company  EQ oe-rell.company
                        AND oe-boll.r-no     EQ oe-rell.r-no
                        AND oe-boll.ord-no   EQ oe-rell.ord-no
                        AND oe-boll.i-no     EQ oe-rell.i-no
                        AND oe-boll.line     EQ oe-rell.line
                        AND oe-boll.rel-no   EQ oe-rell.rel-no
                        AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
                        AND oe-boll.po-no    EQ oe-rell.po-no
                        USE-INDEX ord-no) THEN 
    DO:
        
    FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl)
        EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF AVAIL b-oe-ordl THEN b-oe-ordl.t-rel-qty = b-oe-ordl.t-rel-qty - {&TABLENAME}.qty.
    
END.

IF {&TABLENAME}.b-ord-no GE 0 THEN DO:

    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ {&TABLENAME}.company
        AND itemfg.i-no    EQ {&TABLENAME}.i-no
        NO-ERROR.
    IF AVAILABLE itemfg THEN DO:
        
        FIND b-itemfg WHERE ROWID(b-itemfg) EQ ROWID(itemfg)
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF AVAIL b-itemfg THEN
            ASSIGN
                b-itemfg.q-back  = b-itemfg.q-back - {&TABLENAME}.qty
                .
    END.

    RUN fg/chkfgloc.p (INPUT {&TABLENAME}.i-no, INPUT {&TABLENAME}.loc).
    FOR EACH itemfg-loc NO-LOCK
        WHERE itemfg-loc.company EQ {&TABLENAME}.company
        AND itemfg-loc.i-no    EQ {&TABLENAME}.i-no
        AND itemfg-loc.loc     EQ {&TABLENAME}.loc
        :
        
        IF itemfg-loc.q-back  NE lv-q-back  THEN 
        DO:
            FIND b-itemfg-loc WHERE ROWID(b-itemfg-loc) EQ ROWID(itemfg-loc)
                EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF AVAIL b-itemfg-loc THEN
                ASSIGN           
                    b-itemfg-loc.q-back  = b-itemfg-loc.q-back - {&TABLENAME}.qty
                    .
        END.
    
        LEAVE.
    END. /* For each itemfg-loc */
END. /* If b-ord-no gt 0 */

IF NOT CAN-FIND(FIRST b-{&TABLENAME}
    WHERE b-{&TABLENAME}.company  EQ {&TABLENAME}.company
    AND b-{&TABLENAME}.ord-no   EQ {&TABLENAME}.ord-no
    AND b-{&TABLENAME}.i-no     EQ {&TABLENAME}.i-no
    AND b-{&TABLENAME}.line     EQ {&TABLENAME}.line
    AND b-{&TABLENAME}.r-no     EQ {&TABLENAME}.r-no
    AND b-{&TABLENAME}.rel-no   EQ {&TABLENAME}.rel-no
    AND b-{&TABLENAME}.b-ord-no EQ {&TABLENAME}.b-ord-no
    AND ROWID(b-{&TABLENAME})   NE ROWID({&TABLENAME})
    USE-INDEX ord-no) THEN DO:

    FOR EACH oe-ordl
        WHERE oe-ordl.company EQ {&TABLENAME}.company
        AND oe-ordl.ord-no  EQ {&TABLENAME}.ord-no
        AND oe-ordl.i-no    EQ {&TABLENAME}.i-no
        AND oe-ordl.line    EQ {&TABLENAME}.line
        USE-INDEX item-ord NO-LOCK:
        FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl)
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF AVAIL b-oe-ordl THEN b-oe-ordl.rel-stat = NO.
    END.

    FOR EACH oe-rel WHERE oe-rel.r-no = {&TABLENAME}.link-no 
    /*oe-rel.link-no EQ {&TABLENAME}.r-no USE-INDEX link*/ :
        ASSIGN
            oe-rel.rel-no   = 0
            oe-rel.b-ord-no = 0
            oe-rel.link-no  = 0.

        FIND FIRST truck-run-print WHERE
            truck-run-print.company EQ {&TABLENAME}.company AND
            truck-run-print.oe-rel-r-no EQ {&TABLENAME}.r-no
            EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL truck-run-print THEN
        DO:
            ASSIGN
                truck-run-print.rel-no = 0
                truck-run-print.b-ord-no = 0
                truck-run-print.link-no = 0.

            RELEASE truck-run-print.
        END.
         
    END. /* Each oe-rel */
END. /* If can't find any remaining releases */

FOR EACH oe-rel WHERE oe-rel.r-no EQ {&TABLENAME}.link-no USE-INDEX seq-no:
    ASSIGN
        oe-rel.qty = oe-rel.qty - {&TABLENAME}.Qty
        .  
    IF oe-rel.qty < 0 THEN oe-rel.qty = 0.

END.

FOR EACH oe-rel
    WHERE oe-rel.company  EQ {&TABLENAME}.company
    AND oe-rel.ord-no   EQ {&TABLENAME}.ord-no
    AND oe-rel.rel-no   EQ {&TABLENAME}.rel-no
    AND oe-rel.b-ord-no EQ {&TABLENAME}.b-ord-no
    AND oe-rel.i-no     EQ {&TABLENAME}.i-no
    AND oe-rel.line     EQ {&TABLENAME}.line
    USE-INDEX ord-item:
   
    /* In case this was not done in the prior loop */
    IF oe-rel.r-no NE {&TABLENAME}.link-no THEN 
        ASSIGN
            oe-rel.qty = oe-rel.qty - {&TABLENAME}.Qty
            .
    IF oe-rel.qty < 0 THEN oe-rel.qty = 0.
END.

IF TRIM({&TABLENAME}.rec_key) NE "" THEN
    FOR EACH reftable WHERE reftable.rec_key EQ {&TABLENAME}.rec_key:
        DELETE reftable.
    END.

IF {&TABLENAME}.ord-no NE 0 THEN
    FOR EACH oe-rel
        WHERE oe-rel.company EQ {&TABLENAME}.company
        AND oe-rel.ord-no  EQ {&TABLENAME}.ord-no:
        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).
    END.

