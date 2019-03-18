/* cec/quote/quochatt10.i */
  
PUT "<C2><R2><#1><R+11><C+50><IMAGE#1=" ls-full-img1  SKIP
    "<FCourier New>"
    "Bill To:"  space(40) "Ship To:"  xquo.shipto[5] SKIP
    SPACE(5) bill[1]  xquo.shipto[1] AT 55 skip
    SPACE(5) bill[2]  xquo.shipto[2] AT 55 SKIP
    SPACE(5) bill[3]  xquo.shipto[3] AT 55 SKIP
    SPACE(5) bill[4]  xquo.shipto[4] AT 55 SKIP.

v-printline = v-printline + 15.
PUT "<|10><R4><C50><#3><FROM><R9.5><C80><RECT>" SKIP
    "<R6><C50><FROM><R6><C80><LINE>"       
    "<R4><C62><FROM><R6><C62><LINE>" 
    "<R6><C62><FROM><R8><C62><LINE>"
    "<R8><C50><FROM><R8><C80><LINE>"
    "<FArial><P12><=#3>"
    "<=#3><R-2> <B>Quotation#: " v-first-q-no "</B>" "           Page#: " + string(PAGE-NUMBER /*- lv-pg-num*/ ,">>9") /*+ " of " + string(lv-tot-pg)*/ FORM "x(30)"
    "<P10>" SKIP
    "<=#3> Customer ID             Contact"
    "<=#3><R+2> Telephone                 Fax # " 
    "<=#3><R+4.3> Email: <FCourier New>" 
    "<=3><R+1> " xquo.cust-no  space(6) xquo.contact
    "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999"  space(1) string(cust.fax,"(999)999-9999") FORMAT "x(14)"
    "<=3><R+4.3> " SPACE(5) cEmail format "x(40)"
    .

PUT "<|10><R20><C1><#4><FROM><R24><C80><RECT>" SKIP
    "<R22><C1><FROM><R22><C80><LINE>" SKIP    
    "<R20><C11><FROM><R24><C11><LINE>" SKIP
    "<R20><C22><FROM><R24><C22><LINE>" SKIP
    "<R20><C38><FROM><R24><C38><LINE>" SKIP
    "<R20><C52><FROM><R24><C52><LINE>" SKIP
    "<R20><C70><FROM><R24><C70><LINE>" SKIP
    .
PUT "<FArial><=4><R+1> Quote Date         FOB                     Ship Via                                  Terms                      Sales Person                       Over-Under %" SKIP
    "<FCourier New><=4><R+3> " v-quo-date FORM "99/99/9999" space(2)
    cust.fob-code FORM "x(11)" SPACE(2)
    carrier.dscr FORM "x(20)" SPACE(1)
    terms.dscr FORM "x(15)" space(1) sman.sname space(2) v-over-under SKIP.

PUT "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
    "<R25><C7><FROM><R27><C7><LINE>" SKIP
    "<R25><C20><FROM><R27><C20><LINE>" SKIP
    "<R25><C45><FROM><R27><C45><LINE>" SKIP
    "<R25><C56><FROM><R27><C56><LINE>" SKIP
    "<R25><C63><FROM><R27><C63><LINE>" SKIP
    "<R25><C73><FROM><R27><C73><LINE>" SKIP.
  
PUT "<FArial><=5><R+1> Est#/Qt#    Part#/Description               Item/Style/Color/Board                             Quantity      Release             Price           UOM " SKIP.
PUT "<FCourier New>".

lv-pg-num = PAGE-NUMBER.

