/*cec/quote/quoottp1.i */

        PUT "<C3><R2><#1><R+8><C+45><IMAGE#1=" ls-full-img1 SKIP /* pacific package */ 
   /*  PUT "<C+25><#1>".
     PUT "<=1>" SKIP. 
     /*PUT "<C1><#2><R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
     PUT "<C1><#2>"
         "<P10><=2><R+9>"
          v-comp-add1 AT 8 SKIP
          v-comp-add2 AT 8 SKIP
          v-comp-add3 AT 8 SKIP
          v-comp-add4 AT 8 SKIP(1) */
         /*"<FCourier New>" */
        "<FCourier New><P12>"
         space(4) "Bill To:"            "Ship To:" AT 45 /*shipto[5]*/ SKIP(1)
         SPACE(4) bill[1]  shipto[1] AT 50 skip
         SPACE(4) bill[2]  shipto[2] AT 50 SKIP
         SPACE(4) bill[3]  shipto[3] AT 50 SKIP
         SPACE(4) bill[4]  shipto[4] AT 50 SKIP.
   
     v-printline = v-printline + 15.
     PUT "<||3><R4><C50><#3><FROM><R8><C80><RECT>" SKIP.
     PUT "<R6><C50><FROM><R6><C80><LINE>" SKIP      
         "<R4><C62><FROM><R6><C62><LINE>" SKIP
         "<R6><C65><FROM><R8><C65><LINE>" SKIP
         .
       
   PUT "<FCourier New><P12><=#3><R-2> <B>Quotation#: " v-first-q-no "</B><P10>" SKIP
   "<=#3> Customer ID           Contact"
   "<=#3><R+2> Telephone                Fax " /*<FCourier New>*/ 
   "<=3><R+1> " xquo.cust-no  space(7) xquo.contact /*cust.contact*/
   "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999"  space(6) cust.fax
   .

   PUT "<||3><R18><C1><#4><FROM><R22><C81><RECT>" SKIP
   "<R20><C1><FROM><R20><C81><LINE>" SKIP    
   "<R18><C11><FROM><R22><C11><LINE>" SKIP
   "<R18><C22><FROM><R22><C22><LINE>" SKIP
   "<R18><C38><FROM><R22><C38><LINE>" SKIP
   "<R18><C52><FROM><R22><C52><LINE>" SKIP
   "<R18><C70><FROM><R22><C70><LINE>" SKIP
   .
   PUT "<FCourier New><=4><R+1> Quote Date      FOB          Ship Via           Terms           Sales Person       Over-Under%" SKIP
   "<=4><R+3> " xquo.quo-date FORM "99/99/9999" space(3)
   cust.fob-code FORM "x(11)" SPACE(2)
   carrier.dscr FORM "x(19)" 
   terms.dscr FORM "x(15)" space(2) sman.sname space(1) v-over-under SKIP.

PUT "<||3><R23><C1><#5><FROM><R25><C81><RECT>" SKIP    
             "<R23><C6><FROM><R25><C6><LINE>" SKIP
             "<R23><C33><FROM><R25><C33><LINE>" SKIP
             "<R23><C50><FROM><R25><C50><LINE>" SKIP
             "<R23><C57><FROM><R25><C57><LINE>" SKIP
             "<R23><C64><FROM><R25><C64><LINE>" SKIP
             "<R23><C74><FROM><R25><C74><LINE>" SKIP
             .
  PUT "<=5><R+1> Est#   Description/Style/Color/Board      Item            Quantity Release     Price     UOM "
       /*"<FCourier New>"*/
       SKIP(1).
  

