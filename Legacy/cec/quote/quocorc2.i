 /* cec/quote/quocorc2.i */
 
  PUT "<R2><C2><#1><R+10><C+80><IMAGE#1=" ls-full-img1 SKIP. 
     
     /*PUT "<C1><#2><R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
                 
     /*     PUT "<C1><#2>"
         "<P10><=2><R+9>"
          v-comp-add1 AT 8 SKIP
          v-comp-add2 AT 8 SKIP
          v-comp-add3 AT 8 SKIP
          v-comp-add4 AT 8 SKIP(1)
         "<FCourier New>"
         "Bill To:"  space(40) "Ship To:"  shipto[5] SKIP
         SPACE(5) bill[1]  shipto[1] AT 55 skip
         SPACE(5) bill[2]  shipto[2] AT 55 SKIP
         SPACE(5) bill[3]  shipto[3] AT 55 SKIP
         SPACE(5) bill[4]  shipto[4] AT 55 SKIP.
    */

     PUT "<FArial><P10><C1><R-5><#2><=2>"
       /*  "<=2><R+4>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
         "<P10><=2><R+5>" */
          v-comp-add1 AT 66 SKIP
          v-comp-add2 AT 37 SKIP
          v-comp-add3 AT 37 SKIP
          v-comp-add4 AT 37 SKIP
          v-comp-add5 AT 37  SKIP
          lv-email AT 37 SKIP(4)
         "<FCourier New>"
         "Bill To:"  space(40) "Ship To:"  shipto[5] SKIP
         SPACE(5) bill[1]  shipto[1] AT 55 skip
         SPACE(5) bill[2]  shipto[2] AT 55 SKIP
         SPACE(5) bill[3]  shipto[3] AT 55 SKIP
         SPACE(5) bill[4]  shipto[4] AT 55 SKIP.
    /* IF lv-display-comp THEN
        PUT "<=2><C3><R+2><FGCOLOR=" trim(lv-comp-color) + ">"
            "<=2><C3><R+3><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
            . 
    */        
     v-printline = v-printline + 15.
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

   /* same as request - no space between hd & line
   PUT "<R11><#4><R17><C80><RECT>" SKIP
   "<R13><C1><FROM><R13><C80><LINE>" SKIP
   "<R15><C1><FROM><R15><C80><LINE>" SKIP

   "<R11><C20><FROM><R15><C20><LINE>" SKIP
   "<R11><C30><FROM><R15><C30><LINE>" SKIP
   "<R11><C40><FROM><R15><C40><LINE>" SKIP
   "<R11><C50><FROM><R15><C55><LINE>" SKIP
   "<R11><C60><FROM><R15><C60><LINE>" SKIP
   "<R11><C70><FROM><R15><C70><LINE>" SKIP
   .
   */  

   PUT "<|10><R23><C1><#4><FROM><R27><C80><RECT>" SKIP
   "<R25><C1><FROM><R25><C80><LINE>" SKIP    
   "<R23><C11><FROM><R27><C11><LINE>" SKIP
   "<R23><C22><FROM><R27><C22><LINE>" SKIP
   "<R23><C38><FROM><R27><C38><LINE>" SKIP
   "<R23><C52><FROM><R27><C52><LINE>" SKIP
   "<R23><C70><FROM><R27><C70><LINE>" SKIP
   .
   PUT "<FArial><=4><R+1> Quote Date         FOB                     Ship Via                                  Terms                      Sales Person                       Over-Under %" SKIP
   "<FCourier New><=4><R+3> " xquo.quo-date FORM "99/99/9999" space(2)
   cust.fob-code FORM "x(11)" SPACE(2)
   carrier.dscr FORM "x(20)" SPACE(1)
   terms.dscr FORM "x(15)" space(1) sman.sname space(2) v-over-under SKIP.


   PUT "<|10><R28><C1><#5><FROM><R30><C80><RECT>" SKIP    
             "<R28><C6><FROM><R30><C6><LINE>" SKIP
             "<R28><C20><FROM><R30><C20><LINE>" SKIP
             "<R28><C45><FROM><R30><C45><LINE>" SKIP
             "<R28><C55><FROM><R30><C55><LINE>" SKIP
             "<R28><C62><FROM><R30><C62><LINE>" SKIP
             "<R28><C72><FROM><R30><C72><LINE>" SKIP
             .
   PUT "<FArial><=5><R+1> Est#      Description/Size                   Item/Style/Color/Board                               Quantity    Release             Price           UOM " SKIP(1).
   PUT "<FCourier New>".
