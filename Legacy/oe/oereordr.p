DEF PARAM BUFFER io-itemfg FOR itemfg.
DEFINE INPUT PARAMETER ip-oereordr AS LOG NO-UNDO.
DEF OUTPUT PARAM op-alloc AS INT NO-UNDO.
     
DEF VAR v-type AS CHAR NO-UNDO.

DEF BUFFER s-code FOR reftable.

IF AVAIL io-itemfg THEN
FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ io-itemfg.company NO-LOCK NO-ERROR.

IF AVAIL oe-ctrl THEN
FOR EACH oe-ordl NO-LOCK
    WHERE oe-ordl.company EQ io-itemfg.company
      AND oe-ordl.i-no    EQ io-itemfg.i-no
      AND oe-ordl.opened  EQ YES
    USE-INDEX ITEM,

    FIRST oe-ord NO-LOCK
    WHERE oe-ord.company EQ oe-ordl.company
      AND oe-ord.ord-no  EQ oe-ordl.ord-no:
     
  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
      USE-INDEX ord-item:

      FIND FIRST s-code WHERE
                 s-code.reftable EQ "oe-rel.s-code" AND
                 s-code.company  EQ STRING(oe-rel.r-no,"9999999999")
                 NO-LOCK NO-ERROR.
        
    {oe/rel-stat.i v-type}.
    
    IF v-type EQ "P" AND ip-oereordr THEN do:
        /* 04-16-10 AH add alloc qty only when code is not "I"nvoice */
        IF AVAIL s-code AND s-code.CODE <> "I" THEN /*assuming ip-oereordr only yes and ?*/
           op-alloc = op-alloc + (IF ip-oereordr THEN oe-rel.qty ELSE oe-rel.tot-qty).
    END.
    ELSE IF ip-oereordr EQ ? AND lookup(v-type,"S,L,I,A,B") > 0 THEN
    DO:
       IF (AVAIL s-code AND s-code.CODE <> "I") OR NOT AVAIL s-code THEN
          op-alloc = op-alloc + oe-rel.tot-qty.
    END.
    
    ELSE
    IF v-type EQ "Z" AND NOT oe-ctrl.u-inv AND ip-oereordr THEN
    FOR EACH inv-line NO-LOCK
        WHERE inv-line.company EQ oe-boll.company
          AND inv-line.ord-no  EQ oe-boll.ord-no
          AND inv-line.b-no    EQ oe-boll.b-no
          AND inv-line.i-no    EQ oe-boll.i-no
          AND inv-line.line    EQ oe-boll.line:

      op-alloc = op-alloc + inv-line.ship-qty.
    END.
  END.
    
  IF ip-oereordr THEN
  FOR EACH oe-rell FIELDS(qty) NO-LOCK
      WHERE oe-rell.company EQ oe-ordl.company
        AND oe-rell.ord-no  EQ oe-ordl.ord-no
        AND oe-rell.i-no    EQ oe-ordl.i-no
        AND oe-rell.line    EQ oe-ordl.line
        AND CAN-FIND(FIRST oe-relh
                     WHERE oe-relh.company EQ oe-rell.company
                       AND oe-relh.r-no    EQ oe-rell.r-no
                       AND oe-relh.posted  EQ NO
                       AND oe-relh.deleted EQ NO)
      USE-INDEX ord-no:

    /* 04-16-10 AH add alloc qty only when code is not "I"nvoice */
    IF oe-rell.s-code <> "I" THEN
       op-alloc = op-alloc + oe-rell.qty.  
  END.
END.
