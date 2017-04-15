 /* cec/quote/quoxfib2.i */
 
     PUT "<C1><R2><#1><R+4><C+65><IMAGE#1=" ls-full-img1 SKIP 
     /*
     PUT "<=1>" SKIP. 
                 
     PUT "<C1><#2><Farial>" 
         "<=2><R+4>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)" */
         "<P10><=1><R+5.5>"
          v-comp-add1 AT 8 SKIP.

     IF v-comp-add2 NE "" THEN
       PUT v-comp-add2 AT 8 SKIP.

     PUT  v-comp-add3 AT 8 SKIP
          v-comp-add4 AT 8 SKIP
          v-comp-add5 AT 8 /*"<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" */ SKIP
          /*lv-email AT 8*/ SKIP(1)
         "<FCourier New>"
         "Bill To:"  space(40) "Ship To:"  xquo.shipto[5] SKIP
         SPACE(5) bill[1]  xquo.shipto[1] AT 55 skip
         SPACE(5) bill[2]  xquo.shipto[2] AT 55 SKIP
         SPACE(5) bill[3]  xquo.shipto[3] AT 55 SKIP
         SPACE(5) bill[4]  xquo.shipto[4] AT 55 SKIP.
    /* IF lv-display-comp THEN
        PUT "<=2><C3><R+2><FGCOLOR=" trim(lv-comp-color) + ">"
            "<=2><C3><R+3><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
            . 
    */        
     v-printline = v-printline + 15.
     /*PUT "<|10><R4><C50><#3><FROM><R8><C80><RECT>" SKIP.*/
    /* PUT "<R6><C50><FROM><R6><C80><LINE>"       
         "<R4><C62><FROM><R6><C62><LINE>" 
         "<R6><C65><FROM><R8><C65><LINE>" 
         .*/
    
   PUT "<FArial><P12><=#3>"
       "<=#3><R+3><c47> <B>          Quote#: " v-first-q-no FORM ">>>>>9" "</B>" /*"      Page#: " + string(PAGE-NUM - lv-pg-num,">>9") FORM "x(30)"*/
       "<=#3><R+5><c47> <B>   Quote Date: " v-quo-date FORM "99/99/9999" "</B>"
       "<P10>" SKIP
       /*"<=#3>     Customer ID                   Contact"
       "<=#3><R+2>       Telephone                            Fax <FCourier New>" 
       "<=3><R+1> " xquo.cust-no  space(6) xquo.contact /*cust.contact*/
       "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999"  space(5) cust.fax*/
       .
   PUT "<|10><R20><C1><#4><FROM><R24><C80><RECT>" SKIP
       "<R22><C1><FROM><R22><C80><LINE>" SKIP    
       "<R20><C11><FROM><R24><C11><LINE>" SKIP
       "<R20><C22><FROM><R24><C22><LINE>" SKIP
       "<R20><C38><FROM><R24><C38><LINE>" SKIP
       "<R20><C52><FROM><R24><C52><LINE>" SKIP
       "<R20><C70><FROM><R24><C70><LINE>" SKIP
       .                   

   lv-fob-code = cust.fob-code.

   IF carrier.carrier EQ "CPU" THEN
     lv-fob-code = "ORIG".

   PUT "<FArial><=4><R+1>   Quote Date               FOB                        Ship Via                            Terms                          Sales Person               Over-Under %" SKIP
       "<FCourier New><=4><R+3> " v-quo-date FORM "99/99/9999" space(2)
       lv-fob-code FORM "x(11)" SPACE(2)
       carrier.dscr FORM "x(20)" SPACE(1)
       terms.dscr FORM "x(15)" space(1) sman.sname space(2) v-over-under SKIP.

   PUT "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
             "<R25><C7><FROM><R27><C7><LINE>" SKIP
             "<R25><C23><FROM><R27><C23><LINE>" SKIP
             "<R25><C49><FROM><R27><C49><LINE>" SKIP
             "<R25><C58><FROM><R27><C58><LINE>" SKIP
             "<R25><C62><FROM><R27><C62><LINE>" SKIP
             "<R25><C72><FROM><R27><C72><LINE>" SKIP
             /*"<R28><C70><FROM><R30><C70><LINE>" SKIP*/
             .
   PUT "<FArial><=5><c26>" "Item Description/Style/Size"
       "<=5><R+1> Est#            Part#                                              Board/Color                                      Quantity     Rel                 Price           UOM    <FCourier New>" SKIP(1). /* Extended $*/
   
