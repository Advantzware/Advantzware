 /* cec/quote/quoxpnt2.i */
 
 
 PUT "<R1><C2><#89><R+16><C+43><IMAGE#89=" ls-full-img1 FORMAT "x(200)". /* Abox logo */ 
  PUT "<=1>" SKIP. 
  PUT "<C1><#2><Farial>"
      "<=2><R+3>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
      "<P10><=2><R+4>" SKIP(3)
      /* v-comp-add1 AT 8 SKIP
       v-comp-add2 AT 8 SKIP
       v-comp-add3 AT 8 SKIP
       v-comp-add4 AT 8 SKIP*/
       v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
       lv-email AT 8 SKIP(3)
      "<FCourier New>"
      "Bill To:"  space(40) "Ship To:"  xquo.shipto[5] SKIP
      SPACE(5) bill[1]  xquo.shipto[1] AT 55 skip
      SPACE(5) bill[2]  xquo.shipto[2] AT 55 SKIP
      SPACE(5) bill[3]  xquo.shipto[3] AT 55 SKIP
      SPACE(5) bill[4]  xquo.shipto[4] AT 55 SKIP.
 
  

  IF lv-display-comp THEN
     PUT "<=2><C3><R+2><FGCOLOR=" trim(lv-comp-color) + ">"
         "<=2><C3><R+3><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)". 

  v-printline = v-printline + 15.
  PUT "<|10><R4><C50><#3><FROM><R8><C80><RECT>" SKIP
      "<R6><C50><FROM><R6><C80><LINE>"       
      "<R4><C62><FROM><R6><C62><LINE>" 
      "<R6><C65><FROM><R8><C65><LINE>"
      "<FArial><P12><=#3>"
       "<=#3><R-2> <B>Quotation#: " v-first-q-no "</B>" "           Page#: " + string(PAGE-NUM /*- lv-pg-num*/ ,">>9") /*+ " of " + string(lv-tot-pg)*/ FORM "x(30)"
       "<P10>" SKIP
   "<=#3> Customer ID             Contact"
   "<=#3><R+2> Telephone                       Fax <FCourier New>" 
   "<=3><R+1> " xquo.cust-no  space(6) xquo.contact
   "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999"  space(5) cust.fax
   .

   PUT "<|10><R23><C1><#4><FROM><R27><C80><RECT>" SKIP
   "<R25><C1><FROM><R25><C80><LINE>" SKIP    
   "<R23><C11><FROM><R27><C11><LINE>" SKIP
   "<R23><C22><FROM><R27><C22><LINE>" SKIP
   "<R23><C38><FROM><R27><C38><LINE>" SKIP
   "<R23><C52><FROM><R27><C52><LINE>" SKIP
   "<R23><C70><FROM><R27><C70><LINE>" SKIP
   .
   PUT "<FArial><=4><R+1> Quote Date         FOB                     Ship Via                                  Terms                      Sales Person                       Over-Under %" SKIP
   "<FCourier New><=4><R+3> " v-quo-date FORM "99/99/9999" space(2)
   cust.fob-code FORM "x(11)" SPACE(2)
   carrier.dscr FORM "x(20)" SPACE(1)
   terms.dscr FORM "x(15)" space(1) sman.sname space(2) v-over-under SKIP.

   PUT "<|10><R28><C1><#5><FROM><R30><C80><RECT>" SKIP    
             "<R28><C7><FROM><R30><C7><LINE>" SKIP
             "<R28><C24><FROM><R30><C24><LINE>" SKIP
             "<R28><C52><FROM><R30><C52><LINE>" SKIP
             "<R28><C60><FROM><R30><C60><LINE>" SKIP
             "<R28><C67><FROM><R30><C67><LINE>" SKIP
             "<R28><C75><FROM><R30><C75><LINE>" SKIP.
  
   PUT "<FArial><=5><R+1><C1.5>Est#/Qt#<C8>Part#/Description<C25>Item/Style/Color/Board<C53>Quantity<C61>Release<C68>Price<C76>UOM" "<FCourier New>" SKIP(1).
   /* PUT "<FCourier New>".*/

lv-pg-num = PAGE-NUM.

