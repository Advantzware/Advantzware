/* sys/convert/t-quote2.p  run this if t-quote.p does not have this logic */
DEF VAR i AS INT NO-UNDO.

FOR EACH quote NO-LOCK:
    FIND FIRST quotehd WHERE quotehd.company = quote.company
                         AND quotehd.loc = quote.loc
                         AND quotehd.q-no = quote.q-no
        NO-LOCK NO-ERROR.
    IF NOT AVAIL quotehd THEN NEXT.
    i = 0.
    FOR EACH quoteit WHERE quoteit.q-no = quote.q-no NO-LOCK:
       i = i + 1.            
       IF i > 10 THEN LEAVE.
       FIND FIRST quoteqty WHERE quoteqty.company = quote.company
                             AND quoteqty.loc = quote.loc
                             AND quoteqty.q-no = quote.q-no
                             AND quoteqty.LINE = quoteit.LINE
                             AND quoteqty.qty = quoteit.qty
                             NO-ERROR.
      IF AVAIL quoteqty THEN
          ASSIGN quoteqty.prof-on = quote.prof-on[i]           
                 quoteqty.profit =  quote.profit[i]                                                  
                 quoteqty.mat-cost = quote.mat-cost-array[i]           
                 quoteqty.vo-cost =        quote.vo-cost-array[i]    
                 quoteqty.fo-cost =       quote.fo-cost-array[i]    
                 quoteqty.lab-cost = quote.lab-cost-array[i]          
                 quoteqty.misc-price =    quote.misc-price[i]          
          
          .
   END.
   DISP quote.q-no.
   PAUSE 0.
END.
