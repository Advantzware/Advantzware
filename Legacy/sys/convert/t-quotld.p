/* load quote records to gui db */
DEF VAR ii AS INT NO-UNDO.
input from c:\hop\dumpdata\quote.d NO-ECHO.

repeat:
   create quote.
   SET
       quote.company                 
          quote.loc                     
          quote.q-no                    
          quote.est-no                  
          quote.e-num                   
          quote.part-dscr1             
          quote.billto[1 FOR 5]                  
          quote.soldto[1 FOR 5]                  
          quote.shipto[1 FOR 5]
          quote.comment[1]
quote.comment[2]
quote.comment[3]
          quote.comment[4]
          quote.comment[5]                           
          quote.quo-date                
          quote.del-date                
          quote.rfq                     
          quote.qty[1] FORM ">>>>>>>9"                    
          quote.qty[2] FORM ">>>>>>>9"                                        
          quote.qty[3] FORM ">>>>>>>9"                                        
          quote.qty[4] FORM ">>>>>>>9"                                        
          quote.qty[5] FORM ">>>>>>>9"                                        
          quote.qty[6] FORM ">>>>>>>9"                                        
          quote.qty[7] FORM ">>>>>>>9"                                        
          quote.qty[8] FORM ">>>>>>>9"                                        
          quote.qty[9] FORM ">>>>>>>9"                    
          quote.qty[10] FORM ">>>>>>>9"                                                             
          quote.price[1]                   
          quote.price[2]                   
          quote.price[3]                   
          quote.price[4]                   
          quote.price[5]                   
          quote.price[6]                   
          quote.price[7]                   
          quote.price[8]                   
          quote.price[9]                   
          quote.price[10]                   
          quote.profit[1] form "->>,>>>,>>9.99"                  
          quote.profit[2]  form "->,>>>,>>9.99"                
          quote.profit[3] form "->,>>>,>>9.99"                 
          quote.profit[4]  form "->,>>>,>>9.99"                
          quote.profit[5]  form "->,>>>,>>9.99"                
          quote.profit[6]  form "->,>>>,>>9.99"                
          quote.profit[7]   form "->,>>>,>>9.99"               
          quote.profit[8]   form "->,>>>,>>9.99"               
          quote.profit[9]   form "->,>>>,>>9.99"               
          quote.profit[10] form "->,>>>,>>9.99"
          quote.prof-on[1 FOR 10] FORM "x(10)"
          quote.misc-dscr[1 FOR 10]        
          quote.misc-price[1 FOR 10]  
          quote.misc-code[1 FOR 10]        
       quote.sman                    
       quote.cust-no                 
       quote.ship-no                 
       quote.sold-no                 
       quote.carrier                 
       quote.del-zone                
       quote.terms                   
       quote.part-dscr[1 FOR 10]        
       quote.ship-id                 
       quote.sold-id                 
       quote.misc-price-array[1 FOR 100]  FORM "->>>,>>>,>>9.<<<<<<<"
       quote.mat-cost-array[1 FOR 10]     FORM "->>>,>>>,>>9.<<<<<<<"     
       quote.lab-cost-array[1 FOR 10]     FORM "->>>,>>>,>>9.<<<<<<<"     
       quote.fo-cost-array[1 FOR 10]      FORM "->>>,>>>,>>9.<<<<<<<"     
       quote.vo-cost-array[1 FOR 10]      FORM "->>>,>>>,>>9.<<<<<<<"               
       .
   
              
              ii = ii + 1.
     DISP quote.q-no WITH FRAME qqq DOWN.
     PAUSE 0.
   
 END.
           MESSAGE ii "END load"  VIEW-AS ALERT-BOX.
