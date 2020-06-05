/* oe/rep/acksoule.i */

PUT "<FArial>"   /*<R+8>*/
    "<C3><R1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP. 
               
PUT "<=1><R+10><P10>"  /*bigger address */
   "<FCourier New>" 
   "Bill To:" AT 6      SKIP
   SPACE(5) oe-ord.cust-name 
   skip
   SPACE(5) oe-ord.addr[1] SKIP
   SPACE(5) oe-ord.addr[2] SKIP
    
   SPACE(5) v-addr3  /*v-sold-addr3 AT 71*/  SKIP.

  PUT "<=1><R+10><P10>" SPACE(55)  "Ship To:" SKIP
      SPACE(58)  v-ship-name   SKIP
      SPACE(58)  v-contact    SKIP
      SPACE(58)  v-ship-add   SKIP
      SPACE(58)  v-ship-add2  SKIP
      SPACE(58)  v-addr4   SKIP.


v-printline = v-printline + 14.
PUT "<||3><R2><C50><#3><FROM><R10><C80><RECT>" .
PUT "<R4><C50><FROM><R4><C80><LINE>" 
    "<R6><C50><FROM><R6><C80><LINE>" 
    "<R8><C50><FROM><R8><C80><LINE>"
    "<R2><C61><FROM><R4><C61><LINE>" 
    "<R6><C65><FROM><R10><C65><LINE>"
    .

PUT "<FArial><=#3><P10>" 
    "<=#3> Customer ID          <B>ACKNOWLEDGEMENT</B>"
    "<=#3><R+2> Customer Service Representative" 
    "<=#3><R+4> CSR Telephone                   Order Date "
    "<=3><R+6> Status                              Date and Time"
    "<=3><R+1> " oe-ord.cust-no  space(20) oe-ord.ord-no SPACE(5)
    "<=3><R+3> " ( IF AVAIL users THEN users.user_name ELSE "") FORMAT "x(30)"  /*v-cust-phone  space(10) cust.fax*/
    "<=3><R+5> " v-cust-phone  space(21) oe-ord.ord-date .
 IF LENGTH(lv-prt-sts) < 38 THEN /*revised*/
      PUT  "<=3><R+7> " lv-prt-sts FORM "x(38)" SPACE(21)
     lv-prt-date space(1) lv-prt-time.
 else /*Original*/
      PUT   "<=3><R+7> " lv-prt-sts FORM "x(38)" SPACE(22)
           lv-prt-date space(1) lv-prt-time.



      
PUT /*"<R16><C77><FROM><R+2><C+3><RECT><||3>"*/
    "<|10><R18><C1><#4><FROM><R22><C80><RECT>" 
    "<R20><C1><FROM><R20><C80><LINE>" 
    "<R18><C11><FROM><R22><C11><LINE>"
    "<R18><C18><FROM><R22><C18><LINE>"
    "<R18><C34><FROM><R22><C34><LINE>"
    "<R18><C48><FROM><R22><C48><LINE>"
    "<R18><C65><FROM><R22><C65><LINE>"
    "<R18><C72><FROM><R22><C72><LINE>"
    .
PUT "<FArial><=4><R+1>  Ship Date                 FOB             Ship Via                         Terms                    Sales Person                        Order#          Quote#" SKIP
"<FCourier New><=4><R+3> " oe-ord.due-date FORM "99/99/9999" space(4)
cust.fob-code FORM "x(6)" SPACE(1)
v-shipvia  FORM "x(18)" SPACE(1)
oe-ord.terms-d  FORM "x(15)" space(1) v-salesman FORM "x(20)" space(1) oe-ord.ord-no space(4) v-q-no SKIP.


PUT "<|10><R23><C1><#5><FROM><R25><C80><RECT>"      
       "<R23><C4><FROM><R25><C4><LINE>" 
       "<R23><C16><FROM><R25><C16><LINE>"     
       "<R23><C48><FROM><R25><C48><LINE>" 
       "<R23><C56><FROM><R25><C56><LINE>" 
       "<R23><C65><FROM><R25><C65><LINE>" 
       "<R23><C69><FROM><R25><C69><LINE>" 
       .

PUT "<FArial><R23><C7>Customer   " SKIP
    " Ln#      Part#/P.O.#            Item Description                                                      Ordered         Unit Price      UOM        Ext. Price" SKIP(1).
PUT "<FCourier New>"          .    
v-printline = v-printline + 6.
