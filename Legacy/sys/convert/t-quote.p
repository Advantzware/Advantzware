/* t- quote.p */
DISABLE TRIGGERS FOR LOAD OF quotehd.
DISABLE TRIGGERS FOR LOAD OF quoteitm.
DISABLE TRIGGERS FOR LOAD OF quoteqty.
DISABLE TRIGGERS FOR LOAD OF quotechg.

DEF VAR i AS INT NO-UNDO.
DEF VAR j AS INT NO-UNDO.
DEF VAR v-line AS INT NO-UNDO.
DEF VAR lv-qitm-recid AS RECID NO-UNDO.


FOR EACH quote NO-LOCK BY quote.company BY quote.q-no :
    FIND FIRST quotehd WHERE quotehd.company = quote.company
                         AND quotehd.loc = quote.loc
                         AND quotehd.q-no = quote.q-no NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN NEXT.

    CREATE quotehd.
    assign quotehd.carrier = quote.carrier            
           quotehd.comment[1] = quote.comment[1]            
           quotehd.comment[2] = quote.comment[2]            
           quotehd.comment[3] = quote.comment[3]            
           quotehd.comment[4] = quote.comment[4]            
           quotehd.comment[5] = quote.comment[5]            
           quotehd.company = quote.company            
           quotehd.contact =  quote.contact           
           quotehd.cust-no =  quote.cust-no           
           /*quotehd.del-cnt =  quote.delivery-cnt     */      
           quotehd.del-date = quote.del-date           
           quotehd.del-zone = quote.del-zone           
           quotehd.e-num =    quote.e-num           
           quotehd.est-no =   quote.est-no           
           quotehd.loc =       quote.loc                     
           quotehd.part-dscr1 =   quote.part-dscr1        
           quotehd.q-no =  quote.q-no              
           quotehd.quo-date = quote.quo-date           
           quotehd.rfq =      quote.rfq           
          quotehd.ship-id =  quote.ship-id           
          quotehd.ship-no =   quote.ship-no                    
          quotehd.sman =      quote.sman          
          quotehd.sold-id =    quote.sold-id
          quotehd.sold-no = quote.sold-no                               
          quotehd.terms =   quote.terms            
           quotehd.part-dscr =  quote.part-dscr[1]
          /*quotehd.upd-date =            
          quotehd.upd-user =             
          quotehd.curr-code = ""          
          quotehd.sts =                  
          /*quotehd.rec_key =              */
           quotehd.ex-rate =   0          
          */
        .
      
      
      
      DO i = 1 TO 5:
         ASSIGN quotehd.shipto[i] = quote.shipto[i]           
                quotehd.soldto[i] = quote.soldto[i]                    
                quotehd.billto[i] = quote.billto[i]
             .
      END.
        /* quoteitm */
      j = 0.
      v-line = 0.
      FOR EACH quoteit WHERE quoteit.q-no = quote.q-no NO-LOCK 
          BREAK BY quoteit.part-no:
           FIND FIRST eb WHERE eb.company = quoteit.company
                            AND eb.est-no = quoteit.est-no
                            AND eb.part-no = quoteit.part-no
                            NO-LOCK NO-ERROR.
          IF FIRST-OF(quoteit.part-no) THEN DO:
             v-line = v-line + 1.    
             CREATE quoteitm.
             assign quoteitm.company = quote.company           
             quoteitm.e-num =   quoteit.e-num           
             quoteitm.eqty =   IF AVAIL eb THEN eb.eqty ELSE 0            
             quoteitm.est-no =   quote.est-no          
             quoteitm.i-coldscr = quoteit.i-coldscr         
             quoteitm.i-dscr =   quoteit.i-dscr         
             quoteitm.line =    v-line /*quoteit.line           */
             quoteitm.loc =    quote.loc            
             /*quoteitm.ord-no =             */
             quoteitm.part-dscr1 =  quoteit.part-dscr1       
             quoteitm.part-dscr2 =  quoteit.part-dscr2       
             quoteitm.part-dscr3 = ""        
             quoteitm.part-no =    quoteit.part-no        
             quoteitm.price =      quoteit.price        
             quoteitm.q-no =       quoteit.q-no        
             quoteitm.qty =        quoteit.qty        
             /*quoteitm.rec_key =      */      
             quoteitm.size =       quoteit.size                 
             quoteitm.style =      quoteit.style        
             quoteitm.uom =        quoteit.uom        .
             /*quoteitm.sts =                
             quoteitm.upd-date =           
             quoteitm.upd-user =           */
             lv-qitm-recid = RECID(quoteitm).
          END.
          
          /*================ */
          j = j + 1.
          IF j > 10  THEN LEAVE.
          IF NOT AVAIL quoteitm THEN FIND FIRST quoteitm WHERE RECID(quoteitm) = lv-qitm-recid NO-LOCK.

          FIND FIRST quoteqty WHERE quoteqty.company = quote.company
                                AND quoteqty.loc = quote.loc
                                AND quoteqty.q-no = quoteit.q-no
                                AND quoteqty.LINE = quoteitm.LINE
                                AND quoteqty.qty = quoteit.qty
                                AND quoteqty.quote-date = quote.quo-date
                                NO-LOCK NO-ERROR.
          IF NOT AVAIL quoteqty THEN DO:
             create quoteqty.
             assign quoteqty.company =  quote.company          
                 quoteqty.loc =    quote.loc                                       
                 quoteqty.q-no =  quoteit.q-no             
                 quoteqty.line =    quoteitm.line           
                 quoteqty.qty = quoteit.qty               
                 quoteqty.quote-date = quote.quo-date        
                 quoteqty.price =  quoteit.price            
                 quoteqty.prof-on = quote.prof-on[j]           
                 quoteqty.profit =  quote.profit[j]                            
                 quoteqty.uom = quoteit.uom      
                 quoteqty.mat-cost = quote.mat-cost-array[j]           
                 quoteqty.vo-cost =        quote.vo-cost-array[j]    
                 quoteqty.fo-cost =       quote.fo-cost-array[j]    
                 quoteqty.lab-cost = quote.lab-cost-array[j]          
                 quoteqty.misc-price =    quote.misc-price[j]          
                  /*quoteqty.quote-user =         
                    quoteqty.rec_key =            
                    quoteqty.rels =               
                    quoteqty.tot-lbs =            
                    quoteqty.under-pct =                  
                    quoteqty.del-cnt =            
                   quoteqty.over-pct =           
                 */
                .
           /*==================*/
          END.
          IF quote.misc-code[j] <> "" THEN DO:     
              FIND FIRST est-prep WHERE est-prep.company = quotehd.company
                                    AND est-prep.est-no = quoteit.est-no
                                    AND est-prep.CODE = quote.misc-code[j]
                     NO-LOCK NO-ERROR.
                create quotechg.
                assign quotechg.company = quotehd.company  
                       quotechg.loc =  quotehd.loc                                     
                       quotechg.q-no = quote.q-no              
                       quotechg.line = quoteit.LINE              
                       quotechg.qty =  quoteit.qty
                       quotechg.quote-date =  quotehd.quo-date       
                       quotechg.amt =  quote.misc-price[j]                                     
                       quotechg.bill = quote.misc-code[j]                          
                       quotechg.charge = quote.misc-dscr[j]                       
                       quotechg.prep-qty =  quoteqty.qty                                
                       quotechg.b-num =   if avail eb then eb.form-no  else 1
                       quotechg.s-num =   if avail eb then eb.blank-no else 1
                       quotechg.simon =   IF AVAIL est-prep THEN est-prep.simon ELSE ""
                /*       quotechg.amtz =                                   
                       quotechg.cost =               
                       quotechg.labf =               
                       quotechg.labm =                                                             
                       quotechg.matf =               
                       quotechg.matm =               
                       quotechg.mkup =                                                         
                       quotechg.rec_key =                                   
                                     */
                       .
          END.
       END.  /* each quoteit */
       DISP quotehd.q-no WITH FRAME qqq DOWN.
       PAUSE 0.
END.
