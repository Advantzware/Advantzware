/* ---------------------------------------------- oe/rep/bolxprt2.i 02/04 YSK */
/* PRINT detail                                                               */
/* -------------------------------------------------------------------------- */

for each oe-boll where
    oe-boll.company eq oe-bolh.company and
    oe-boll.b-no eq oe-bolh.b-no,      
    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq oe-boll.i-no
    NO-LOCK BREAK BY oe-boll.i-no
                  BY oe-boll.ord-no
                  BY oe-boll.line
                  BY oe-boll.po-no
                  BY oe-boll.job-no
                  BY oe-boll.job-no2 :

  find first oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.ord-no  eq oe-boll.ord-no
        and oe-ordl.i-no    eq oe-boll.i-no
        and oe-ordl.line    eq oe-boll.line
      no-lock no-error.

  find first oe-ord
      where oe-ord.company eq cocode
        and oe-ord.ord-no  eq oe-boll.ord-no
      no-lock no-error.


  IF lv-bolfmt-int = 1 AND
     FIRST-OF(oe-boll.i-no) THEN
     ASSIGN
        v-cases = 0
        v-partial = 0
        v-total-count = 0
        v-weight = 0.
  ELSE
     ASSIGN
        v-cases = 0
        v-partial = 0
        v-total-count = 0
        v-weight = 0.

  ASSIGN
     v-cases = v-cases + oe-boll.cases
     v-partial = v-partial + oe-boll.partial
     v-total-count = v-total-count + (oe-boll.cases * oe-boll.qty-case)
                   + oe-boll.partial
     v-weight = v-weight + oe-boll.weight.
  
  IF (lv-bolfmt-int = 1 AND
     LAST-OF(oe-boll.ord-no)) OR
     lv-bolfmt-int NE 1 THEN
  DO:
    /* Line# */  
    ASSIGN LvLineCnt = LvLineCnt + 1
           inrowcount = inrowcount + 1
           v-cell = "R" + string(inrowcount) + "C1".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = LvLineCnt .  
          
    
    /* Customer Item */  
    ASSIGN v-cell = "R" + string(inrowcount) + "C2".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = oe-ordl.part-no .  
   
    
    /* Our Item */  
    ASSIGN v-cell = "R" + string(inrowcount) + "C6".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = oe-boll.i-no .  
          
    
    /* Order Quantity */  
    ASSIGN v-cell = "R" + string(inrowcount) + "C10".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = oe-ordl.qty .  
    
    
    /* This DELY */  
    ASSIGN v-cell = "R" + string(inrowcount) + "C13".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = oe-ordl.ship-qty .  
          
    
    /* BAL DUE */  
    ASSIGN v-cell = "R" + string(inrowcount) + "C16".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = ( oe-ordl.qty - oe-ordl.ship-qty ) .  
   
    
    /* qty um */  
    ASSIGN v-cell = "R" + string(inrowcount) + "C19".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = "EA" .  
   
    
    /* Product Desc */  
    ASSIGN v-cell = "R" + string(inrowcount) + "C21".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = itemfg.i-name .  
          
    /* Nbr Units */  
    ASSIGN v-cell = "R" + string(inrowcount) + "C38".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = v-cases.  
          
    
    /* Unit Count */  
    ASSIGN v-cell = "R" + string(inrowcount) + "C40".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = oe-boll.qty-case .  
          
    /* ODD Unit */  
    ASSIGN v-cell = "R" + string(inrowcount) + "C42".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = v-partial.  
          
    /* Total Count */  
    ASSIGN v-cell = "R" + string(inrowcount) + "C45".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = v-total-count.  
          
    /* Item Weight */  
    ASSIGN v-cell = "R" + string(inrowcount) + "C50".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = v-weight.
  END.
end .
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
