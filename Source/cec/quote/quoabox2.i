/* Mod: Ticket - 103137 (Format Change for Order No. and Job No.    */
/*cec/quote/quoabox2.i*/
     
     PUT "<R2><C5><#1><R+5><C+45><IMAGE#1=" ls-full-img1 SKIP. /* Abox logo */ 
     PUT "<=1>" SKIP. 
     /*PUT "<C1><#2><R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
     PUT "<C1><#2>"
         "<P10><=2><R+6>"
      /*    v-comp-add1 AT 8 SKIP
          v-comp-add2 AT 8 SKIP
          v-comp-add3 AT 8 SKIP
          v-comp-add4 AT 8 */ SKIP 
         "<FCourier New>"
         "Bill To:"  space(40) "Ship To:"  shipto[5] SKIP
         SPACE(5) bill[1]  shipto[1] AT 55 skip
         SPACE(5) bill[2]  shipto[2] AT 55 SKIP
         SPACE(5) bill[3]  shipto[3] AT 55 SKIP
         SPACE(5) bill[4]  shipto[4] AT 55 SKIP.
   
     v-printline = v-printline + 10.
     PUT "<|10><R4><C50><#3><FROM><R8><C80><RECT>" SKIP.
     PUT "<R6><C50><FROM><R6><C80><LINE>" SKIP      
         "<R4><C62><FROM><R6><C62><LINE>" SKIP
         "<R6><C65><FROM><R8><C65><LINE>" SKIP
         .
    
   PUT "<FArial><P12><=#3><R-2> <B>Quotation#: " v-first-q-no "</B><P10>" SKIP
   "<=#3> Customer ID             Contact"
   "<=#3><R+2> Telephone                       Fax <FCourier New>" 
   "<=3><R+1> " xquo.cust-no  space(6) xquo.contact /*cust.contact*/
   "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999"  space(5) cust.fax
   .

   PUT "<|10><R16><C1><#4><FROM><R20><C80><RECT>" SKIP
   "<R18><C1><FROM><R18><C80><LINE>" SKIP    
   "<R16><C11><FROM><R20><C11><LINE>" SKIP
   "<R16><C22><FROM><R20><C22><LINE>" SKIP
   "<R16><C38><FROM><R20><C38><LINE>" SKIP
   "<R16><C52><FROM><R20><C52><LINE>" SKIP
   "<R16><C70><FROM><R20><C70><LINE>" SKIP
   .
   PUT "<FArial><=4><R+1> Quote Date         FOB                     Ship Via                                  Terms                      Sales Person                       Over-Under %" SKIP
   "<FCourier New><=4><R+3> " xquo.quo-date FORM "99/99/9999" space(2)
   cust.fob-code FORM "x(11)" SPACE(2)
   carrier.dscr FORM "x(20)" SPACE(1)
   terms.dscr FORM "x(15)" space(1) sman.sname space(2) v-over-under SKIP.


   PUT "<|10><R21><C1><#5><FROM><R23><C80><RECT>" SKIP    
             "<R21><C8><FROM><R23><C8><LINE>" SKIP
             "<R21><C25><FROM><R23><C25><LINE>" SKIP
             "<R21><C48><FROM><R23><C48><LINE>" SKIP
             "<R21><C58><FROM><R23><C58><LINE>" SKIP
             "<R21><C66><FROM><R23><C66><LINE>" SKIP
             "<R21><C71><FROM><R23><C71><LINE>" SKIP
             .
   PUT "<FArial><=5><R+1> Est#                Description                          Item/Style/Color/Board                       Quantity              Price        UOM       Ext. Price" SKIP(1).
   PUT "<FCourier New>".
