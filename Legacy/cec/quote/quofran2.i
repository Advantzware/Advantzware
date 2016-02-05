 /* cec/quote/quofran2.i */
 
     PUT "<C3><R2><#1><R+8><C+65><IMAGE#1=" ls-full-img1 SKIP.
     /*PUT "<=1>" SKIP. */
     PUT "<C1><#2><Farial>"
     /*    "<=2><R+4>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)" */
         "<P10><=2>"
          /*v-comp-add1 AT 8 SKIP
          v-comp-add2 AT 8 SKIP
          v-comp-add3 AT 8 SKIP
          v-comp-add4 AT 8 SKIP
          v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
          lv-email AT 8 SKIP(3) */
         "<FCourier New>"
         "Bill To:"  space(40) "Ship To:"  xquo.shipto[5] SKIP
         SPACE(5) bill[1]  xquo.shipto[1] AT 55 skip
         SPACE(5) bill[2]  xquo.shipto[2] AT 55 SKIP
         SPACE(5) bill[3]  xquo.shipto[3] AT 55 SKIP
         SPACE(5) bill[4]  xquo.shipto[4] AT 55 SKIP.
/*
     IF lv-display-comp THEN
        PUT "<=2><C3><R+2><FGCOLOR=" trim(lv-comp-color) + ">"
            "<=2><C3><R+3><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
            . */

     v-printline = v-printline + 10.
     PUT "<|10><R4><C50><#3><FROM><R8><C80><RECT>" SKIP.
     PUT "<R6><C50><FROM><R6><C80><LINE>"       
         "<R4><C62><FROM><R6><C62><LINE>" 
         "<R6><C65><FROM><R8><C65><LINE>" 
         .
    
   PUT "<FArial><P12><=#3>" /*<R-3>" "    Page#: " + string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" */
       "<=#3><R-2><P13> <B>Quotation#: " v-first-q-no "</B><P12>" "           Page#: " + string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(30)"
       "<P10>" SKIP
   "<=#3> Customer ID             Contact"
   "<=#3><R+2> Telephone                       Fax <FCourier New>" 
   "<=3><R+1> " xquo.cust-no  space(6) xquo.contact /*cust.contact*/
   "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999"  space(5) cust.fax
   .

   PUT "<|10><R17><C1><#4><FROM><R21><C80><RECT>" SKIP
   "<=4><R+2><C1><FROM><C80><LINE>" SKIP    
   "<=4><C11><FROM><R+4><C11><LINE>" SKIP
   "<=4><C27><FROM><R+4><C27><LINE>" SKIP
   /*"<R23><C38><FROM><R27><C38><LINE>" SKIP*/
   "<=4><C52><FROM><R+4><C52><LINE>" SKIP
   /*"<=4><C70><FROM><R+4><C70><LINE>" SKIP*/
   .
   PUT "<FArial><=4><R+1>  Quote Date                  FOB                                        Terms                                                      Sales Person    " /*   Over-Under %"*/ SKIP
   "<FCourier New><=4><R+3> " v-quo-date FORM "99/99/9999" space(2)
   cust.fob-code FORM "x(11)" SPACE(8)
  /* carrier.dscr FORM "x(20)" SPACE(1)*/
   terms.dscr FORM "x(30)" space(1) sman.sname space(2) /*v-over-under*/ SKIP.


   PUT "<|10><R22><C1><#5><FROM><R24><C80><RECT>" SKIP    
             "<R22><C7><FROM><R24><C7><LINE>" SKIP
             "<R22><C24.5><FROM><R24><C24.5><LINE>" SKIP
             "<R22><C50><FROM><R24><C50><LINE>" SKIP
            /* "<R28><C56><FROM><R30><C56><LINE>" SKIP*/
             "<R22><C61.5><FROM><R24><C61.5><LINE>" SKIP
             "<R22><C72><FROM><R24><C72><LINE>" SKIP
             .
   PUT "<FArial><=5><R+1>  Est#        Part#/Description                   Item/Style/Board/Color                                     Quantity                  Price           UOM " SKIP(1).
   PUT "<FCourier New>".
