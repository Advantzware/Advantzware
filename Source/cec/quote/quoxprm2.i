/* Mod: Ticket - 103137 (Format Change for Order No. and Job No.    */ 
 /* cec/quote/quoxprm2.i */
 
RUN XMLOutput (lXMLOutput,'',STRING(PAGE-NUM),'Page'). /* rstark 05181205 */

PUT "<R1><C2><#1><R+10><C+45><IMAGE#1=" ls-full-img1 SKIP.
 
PUT "<=1><R+10>"
  "<FCourier New>"
  "Bill To:"  space(40) "Ship To:"  xquo.shipto[5] SKIP
  SPACE(5) bill[1]  xquo.shipto[1] AT 55 skip
  SPACE(5) bill[2]  xquo.shipto[2] AT 55 SKIP
  SPACE(5) bill[3]  xquo.shipto[3] AT 55 SKIP
  SPACE(5) bill[4]  xquo.shipto[4] AT 55 SKIP.

v-printline = v-printline + 15.
PUT "<|10><R4><C50><#3><FROM><R8><C80><RECT>" SKIP.
PUT "<R6><C50><FROM><R6><C80><LINE>"       
  "<R4><C62><FROM><R6><C62><LINE>" 
  "<R6><C65><FROM><R8><C65><LINE>" 
  .

PUT "<FArial><P12><=#3>"
  "<=#3><R-2> <B>Quotation#: " v-first-q-no FORMAT ">>>>>9" "</B>" "           Page#: " + string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(30)"
  "<P10>" SKIP
  "<=#3> Customer ID             Contact"
  "<=#3><R+2> Telephone                       Fax <FCourier New>" 
  "<=3><R+1> " xquo.cust-no  space(6) xquo.contact /*cust.contact*/
  "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999"  space(5) cust.fax
  .

PUT "<|10><R18><C1><#4><FROM><R22><C80><RECT>" SKIP
  "<R20><C1><FROM><R20><C80><LINE>" SKIP    
  "<R18><C11><FROM><R22><C11><LINE>" SKIP
  "<R18><C22><FROM><R22><C22><LINE>" SKIP
  "<R18><C38><FROM><R22><C38><LINE>" SKIP
  "<R18><C52><FROM><R22><C52><LINE>" SKIP
  "<R18><C70><FROM><R22><C70><LINE>" SKIP
  .

PUT "<FArial><=4><R+1> Quote Date         FOB                     Ship Via                                  Terms                      Sales Person                       Over-Under %" SKIP
  "<FCourier New><=4><R+3> " v-quo-date FORM "99/99/9999" space(2)
  cust.fob-code FORM "x(11)" SPACE(2)
  carrier.dscr FORM "x(20)" SPACE(1)
  terms.dscr FORM "x(15)" space(1) sman.sname space(2) v-over-under SKIP.


PUT "<|10><R23><C1><#5><FROM><R25><C80><RECT>" SKIP    
  "<R23><C8><FROM><R25><C8><LINE>" SKIP
  "<R23><C25.5><FROM><R25><C25.5><LINE>" SKIP
  "<R23><C51.5><FROM><R25><C51.5><LINE>" SKIP
  "<R23><C59><FROM><R25><C59><LINE>" SKIP
  "<R23><C66><FROM><R25><C66><LINE>" SKIP
  "<R23><C75><FROM><R25><C75><LINE>" SKIP
.

PUT "<FArial><=5><R+1> Est#/Qt#            Part#/Description                        Item/Style/Color/Board                    Quantity      Release         Price           UOM " SKIP(1).
PUT "<FCourier New>".
