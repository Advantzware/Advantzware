 /* cec/quote/qusclair.i */
 
  /*PUT "<C+25><#1>"
      "<=1>" SKIP 
      "<C1><#2><Farial>"
      "<=2><R+4>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
      "<P10><=2><R+5>"
       v-comp-add1 AT 8 SKIP
       v-comp-add2 AT 8 SKIP
       v-comp-add3 AT 8 SKIP
       v-comp-add4 AT 8 SKIP
       v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
       lv-email AT 8 */
       PUT "<C2><R3><#1><R+8><C+30><IMAGE#1=" ls-full-img1  SKIP
      "<FCourier New>"
      "Bill To:"  space(40) "Ship To:" v-ship-phone  /*xquo.shipto[5]*/ SKIP
      SPACE(5) bill[1]  xquo.shipto[1] AT 55 skip
      SPACE(5) bill[2]  xquo.shipto[2] AT 55 SKIP
      SPACE(5) bill[3]  xquo.shipto[3] AT 55 SKIP
      SPACE(5) bill[4]  xquo.shipto[4] AT 55 SKIP.
                       
  /*IF lv-display-comp THEN
     PUT "<=2><C3><R+2><FGCOLOR=" trim(lv-comp-color) + ">"
         "<=2><C3><R+3><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)". */ 

  v-printline = v-printline + 15.
  PUT "<|10><R4><C40><#3><FROM><R9><C80><RECT>" SKIP
      "<R6><C40><FROM><R6><C80><LINE>"       
      "<R4><C52><FROM><R6><C52><LINE>" 
      "<R6><C60><FROM><R8><C60><LINE>"
      "<R8><C40><FROM><R8><C80><LINE>"
      "<FArial><P12><=#3>"
       "<=#3><R-2> <B>Quotation#: " v-first-q-no "</B>" "           Page#: " + string(PAGE-NUM /*- lv-pg-num*/ ,">>9") /*+ " of " + string(lv-tot-pg)*/ FORM "x(30)"
       "<P10>" SKIP
   "<=#3> Customer ID             Contact"
   "<=#3><R+2> Telephone                                      Fax"
   "<=#3><R+4> Email:  "     v-email format "x(42)" "<FCourier New>".
  PUT
   "<=3><R+1> " xquo.cust-no  space(6) xquo.contact
   "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999"  space(10) cust.fax.
   
   

   PUT "<|10><R18><C1><#4><FROM><R22><C81><RECT>" SKIP
   "<R20><C1><FROM><R20><C81><LINE>" SKIP    
   "<R18><C11><FROM><R22><C11><LINE>" SKIP
   "<R18><C22><FROM><R22><C22><LINE>" SKIP
   "<R18><C38><FROM><R22><C38><LINE>" SKIP
   "<R18><C52><FROM><R22><C52><LINE>" SKIP
   "<R18><C70><FROM><R22><C70><LINE>" SKIP
   .
   PUT "<FArial><=4><R+1> Quote Date         FOB                     Ship Via                                  Terms                      Sales Person                           Phone#" SKIP
   "<FCourier New><=4><R+3> " v-quo-date FORM "99/99/9999" space(2)
   cust.fob-code FORM "x(11)" SPACE(2)
   carrier.dscr FORM "x(20)" SPACE(1)
   terms.dscr FORM "x(15)" space(1) sman.sname SPACE(-1) v-phone-no /*v-over-under*/ SKIP.

   PUT "<|10><R23><C1><#5><FROM><R25><C81><RECT>" SKIP    
             "<R23><C7><FROM><R25><C7><LINE>" SKIP
             "<R23><C20><FROM><R25><C20><LINE>" SKIP
             "<R23><C45><FROM><R25><C45><LINE>" SKIP
             "<R23><C56><FROM><R25><C56><LINE>" SKIP
             "<R23><C63><FROM><R25><C63><LINE>" SKIP
             "<R23><C75><FROM><R25><C75><LINE>" SKIP.
  
   PUT "<FArial><=5><R+1> Est#/Qt#    Part#/FG Item#                  Item/Style/Color/Board                          Quantity        Release           Price                  UOM " SKIP.
   PUT "<FCourier New>".

lv-pg-num = PAGE-NUM.

