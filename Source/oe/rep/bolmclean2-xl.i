/* ---------------------------------------------- oe/rep/bolxprt2.i 02/04 YSK */
/* PRINT detail                                                               */
/* -------------------------------------------------------------------------- */
iLineCount = 22.
IF lv-bolfmt-int NE 1 THEN
DO:
    MAIN-LOOP:
    FOR EACH oe-boll WHERE
        oe-boll.company EQ oe-bolh.company AND
        oe-boll.b-no EQ oe-bolh.b-no,      
        FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ oe-boll.i-no
        NO-LOCK BREAK BY oe-boll.i-no
        BY oe-boll.ord-no
        BY oe-boll.line
        BY oe-boll.po-no
        BY oe-boll.job-no
        BY oe-boll.job-no2 :

        FIND FIRST oe-ordl
            WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no  EQ oe-boll.ord-no
            AND oe-ordl.i-no    EQ oe-boll.i-no
            AND oe-ordl.line    EQ oe-boll.line
            NO-LOCK NO-ERROR.

        FIND FIRST oe-ord
            WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-boll.ord-no
            NO-LOCK NO-ERROR. 
        
            ASSIGN
                v-cases       = 0
                v-partial     = 0
                v-total-count = 0
                v-weight      = 0.  

        ASSIGN
            v-cases       = v-cases + oe-boll.cases
            v-partial     = v-partial + oe-boll.partial
            v-total-count = v-total-count + (oe-boll.cases * oe-boll.qty-case)
                       + oe-boll.partial
            v-weight      = v-weight + oe-boll.weight
            dTotWeight    = dTotWeight + oe-boll.weight
            iTotalQty     = iTotalQty + oe-boll.qty
            iTotalCase    = iTotalCase + oe-boll.cases  + (IF oe-boll.partial GT 0 THEN 1 ELSE 0).
         
        IF AVAILABLE itemfg AND itemfg.frt-class NE "" THEN
            chWorkSheet:Range("T34"):VALUE =  itemfg.frt-class  .      
       
            IF iLineCount GT 28 THEN LEAVE MAIN-LOOP.
            iLineCount  = iLineCount  + 1.
            chWorkSheet:Range("A" + string(iLineCount)):VALUE = "Part#:" + oe-ordl.part-no + " Name:" +
                itemfg.i-name + " PO#:" + oe-boll.po-no + " Order#:" +
                string(oe-ordl.ord-no) .  
            chWorkSheet:Range("J" + string(iLineCount)):VALUE = STRING(v-total-count).
            chWorkSheet:Range("L" + string(iLineCount)):VALUE = STRING(v-weight) .
            chWorkSheet:Range("P" + string(iLineCount)):VALUE = STRING(oe-boll.cases) + " x " + string(oe-boll.qty-case) + " + 1 x " + string(oe-boll.partial) + 
                " = " + string(oe-boll.cases * oe-boll.qty-case + oe-boll.partial) + " total"  .   
        
    END .  
    
END.
ELSE IF lv-bolfmt-int EQ 1 THEN
DO:   
    MAIN-LOOP:
    FOR EACH oe-boll WHERE
        oe-boll.company EQ oe-bolh.company AND
        oe-boll.b-no EQ oe-bolh.b-no,      
        FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ oe-boll.i-no
        NO-LOCK BREAK BY oe-boll.i-no
        BY oe-boll.ord-no
        BY oe-boll.qty-case
        BY oe-boll.line
        BY oe-boll.po-no
        BY oe-boll.job-no
        BY oe-boll.job-no2 :

        FIND FIRST oe-ordl
            WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no  EQ oe-boll.ord-no
            AND oe-ordl.i-no    EQ oe-boll.i-no
            AND oe-ordl.line    EQ oe-boll.line
            NO-LOCK NO-ERROR.

        FIND FIRST oe-ord
            WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-boll.ord-no
            NO-LOCK NO-ERROR.


        IF FIRST-OF(oe-boll.qty-case) THEN
            ASSIGN
                v-cases       = 0
                v-partial     = 0
                v-total-count = 0
                v-weight      = 0
                iTotalSumCases = 0
                iTotalSumPartial = 0
                iTotalSumQtyCase = 0.
        

        ASSIGN
            iTotalSumCases       = iTotalSumCases + oe-boll.cases
            iTotalSumPartial     = iTotalSumPartial + oe-boll.partial
            iTotalSumQtyCase     = oe-boll.qty-case
            
            v-total-count = v-total-count + (oe-boll.cases * oe-boll.qty-case)
                       + oe-boll.partial
            v-weight      = v-weight + oe-boll.weight
            dTotWeight    = dTotWeight + oe-boll.weight
            iTotalQty     = iTotalQty + oe-boll.qty
            iTotalCase    = iTotalCase + oe-boll.cases  + (IF oe-boll.partial GT 0 THEN 1 ELSE 0).
         
        IF AVAILABLE itemfg AND itemfg.frt-class NE "" THEN
            chWorkSheet:Range("T34"):VALUE =  itemfg.frt-class  .
      
        IF LAST-OF(oe-boll.qty-case) THEN
        DO:
            IF iLineCount GT 28 THEN LEAVE MAIN-LOOP.
            iLineCount  = iLineCount  + 1.
            chWorkSheet:Range("A" + string(iLineCount)):VALUE = "Part#:" + oe-ordl.part-no + " Name:" +
                itemfg.i-name + " PO#:" + oe-boll.po-no + " Order#:" +
                string(oe-ordl.ord-no) .  
            chWorkSheet:Range("J" + string(iLineCount)):VALUE = STRING(v-total-count).
            chWorkSheet:Range("L" + string(iLineCount)):VALUE = STRING(v-weight) .
            chWorkSheet:Range("P" + string(iLineCount)):VALUE = STRING(iTotalSumCases) + " x " + string(iTotalSumQtyCase) + " + 1 x " + string(iTotalSumPartial) + 
                " = " + string(iTotalSumCases * iTotalSumQtyCase + iTotalSumPartial) + " total"  .   
        END.
        
    END .
END.

chWorkSheet:Range("J29"):VALUE = STRING(iTotalQty).
chWorkSheet:Range("L29"):VALUE = STRING(dTotWeight) .
chWorkSheet:Range("A34"):VALUE = STRING(oe-bolh.tot-pallets) .
chWorkSheet:Range("B34"):VALUE = "Pallet" .
chWorkSheet:Range("C34"):VALUE =  iTotalCase.
chWorkSheet:Range("D34"):VALUE = "Cartons". 
chWorkSheet:Range("F34"):VALUE = dTotWeight.
chWorkSheet:Range("A39"):VALUE =  oe-bolh.tot-pallets.
chWorkSheet:Range("C39"):VALUE = iTotalCase. 
chWorkSheet:Range("F39"):VALUE = dTotWeight.   
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
