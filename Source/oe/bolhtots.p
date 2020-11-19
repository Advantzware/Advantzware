
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEFINE VARIABLE dExpectedRevenue AS DECIMAL NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF oe-bolh.
DISABLE TRIGGERS FOR LOAD OF oe-boll.

FIND oe-bolh WHERE ROWID(oe-bolh) EQ ip-rowid NO-ERROR.

IF AVAIL oe-bolh THEN DO:
  ASSIGN
   oe-bolh.tot-wt  = 0
   oe-bolh.expectedRevenue = 0
        /* oe-bolh.freight = 0 */.

  FOR EACH oe-boll
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no:

    IF oe-boll.weight LT 0 THEN oe-boll.weight = oe-boll.weight * -1.
    ASSIGN
     oe-bolh.tot-wt  = oe-bolh.tot-wt  + oe-boll.weight
            /* oe-bolh.freight = oe-bolh.freight + oe-boll.freight */.
      RUN pGetSellTotPrice(BUFFER oe-boll,OUTPUT dExpectedRevenue).
     oe-bolh.expectedRevenue = oe-bolh.expectedRevenue + dExpectedRevenue . 
  END.
END.

PROCEDURE pGetSellTotPrice PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Get Sell Price 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-boll FOR oe-boll.      
    DEFINE OUTPUT PARAMETER opdSellTotPrice AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dSellPrice AS DECIMAL NO-UNDO.
    
    FIND FIRST oe-rell NO-LOCK
        WHERE oe-rell.company EQ ipbf-oe-boll.company
        AND oe-rell.ord-no  EQ ipbf-oe-boll.ord-no
        AND oe-rell.r-no    EQ ipbf-oe-boll.r-no
        AND oe-rell.i-no    EQ ipbf-oe-boll.i-no
        AND oe-rell.line    EQ ipbf-oe-boll.line NO-ERROR.
    IF AVAILABLE oe-rell THEN     
        FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.    
    
    IF AVAILABLE oe-relh AND AVAILABLE oe-rell AND oe-rell.link-no EQ 0 THEN 
    DO:
        FIND FIRST oe-rel
            WHERE oe-rel.company  EQ oe-rell.company
            AND oe-rel.ord-no   EQ oe-rell.ord-no
            AND oe-rel.line     EQ oe-rell.line
            AND oe-rel.i-no     EQ oe-rell.i-no
            AND oe-rel.ship-id  EQ oe-relh.ship-id
            AND oe-rel.link-no  EQ 0
            NO-ERROR.

        IF NOT AVAILABLE oe-rel THEN
            FIND FIRST oe-rel
                WHERE oe-rel.company  EQ oe-rell.company
                AND oe-rel.ord-no   EQ oe-rell.ord-no
                AND oe-rel.line     EQ oe-rell.line
                AND oe-rel.i-no     EQ oe-rell.i-no
                AND oe-rel.link-no  EQ 0
                NO-ERROR.
    END.

    ELSE IF AVAILABLE oe-rell THEN        
            FIND FIRST oe-rel
                WHERE oe-rel.r-no EQ oe-rell.link-no
                USE-INDEX seq-no NO-ERROR.

    IF AVAILABLE oe-rel THEN 
    DO:
        dSellPrice = oe-rel.price .
    END.
        
    IF dSellPrice EQ 0 THEN
    DO:
        FIND FIRST oe-ordl  NO-LOCK
            WHERE oe-ordl.company EQ ipbf-oe-boll.company
            AND oe-ordl.ord-no  EQ ipbf-oe-boll.ord-no
            AND oe-ordl.line    EQ ipbf-oe-boll.line
            AND oe-ordl.i-no    EQ ipbf-oe-boll.i-no  NO-ERROR.
        
        IF AVAILABLE oe-ordl THEN
        DO:
            dSellPrice = oe-ordl.price . 
        END.          
    END.
    
    IF ipbf-oe-boll.zeroPrice EQ 1 THEN
        dSellPrice = 0.
    ELSE IF ipbf-oe-boll.sell-price NE 0 THEN
            dSellPrice = ipbf-oe-boll.sell-price.
            
   RUN Conv_CalcTotalPrice(ipbf-oe-boll.company, 
                        ipbf-oe-boll.i-no,
                        ipbf-oe-boll.qty,
                        dSellPrice,
                        (IF AVAIL oe-ordl THEN oe-ordl.pr-uom ELSE "EA"),
                        (IF AVAIL oe-ordl THEN oe-ordl.disc ELSE 0),
                        (IF AVAIL oe-ordl AND oe-ordl.pr-uom EQ "CS" THEN oe-ordl.cas-cnt
                                                   ELSE ipbf-oe-boll.qty-case),    
                        OUTPUT opdSellTotPrice).    
        
END PROCEDURE.        
