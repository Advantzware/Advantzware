/* == oe/rep/acksthpk.i */
PUT "<FArial>".
     
PUT "<C3><R2><#1><R+13><C+25><IMAGE#1=" ls-full-img1 SKIP  /* company image */ 
   /*"<P10><=2><R+4>"
    v-comp-add1 AT 8 SKIP
    v-comp-add2 AT 8  SKIP
    v-comp-add3 AT 8 SKIP
    v-comp-add4 AT 8 SKIP(1)*/
   "<FCourier New>"
   "Bill To:" SPACE(30) "Sold To:"  SKIP
   SPACE(5) oe-ord.cust-name  oe-ord.sold-name AT 45 skip
   SPACE(5) oe-ord.addr[1]  oe-ord.sold-addr[1] AT 45 SKIP
   SPACE(5) oe-ord.addr[2]  oe-ord.sold-addr[2] AT 45 SKIP
   SPACE(5) v-addr3  v-sold-addr3 AT 45 SKIP.
v-printline = v-printline + 14.
PUT "<|10><R4><C50><#3><FROM><R10><C80><RECT>" SKIP.
PUT "<R6><C50><FROM><R6><C80><LINE>" SKIP      
    "<R8><C50><FROM><R8><C80><LINE>" SKIP      
   "<R4><C61><FROM><R6><C61><LINE>" SKIP
   "<R6><C65><FROM><R8><C65><LINE>" SKIP
   "<R8><C65><FROM><R10><C65><LINE>" SKIP
   .

PUT "<FArial><P12><=#3><R-2> <B>Acknowledgement</B> " "<P10>" SKIP
    "<=#3> Customer ID             Contact"
    "<=#3><R+2> Telephone                      Fax" 
    "<=#3><R+4> Customer PO                        Order Date <FCourier New>"
    "<=3><R+1> " oe-ord.cust-no  space(5) cust.contact
    "<=3><R+3> " v-cust-phone  space(5) cust.fax
    "<=3><R+5> " oe-ord.po-no space(5) oe-ord.ord-date .


PUT "<|10><R23><C1><#4><FROM><R27><C82><RECT>" SKIP
"<R25><C1><FROM><R25><C82><LINE>" SKIP    
"<R23><C11><FROM><R27><C11><LINE>" SKIP
"<R23><C22><FROM><R27><C22><LINE>" SKIP
"<R23><C38><FROM><R27><C38><LINE>" SKIP
"<R23><C52><FROM><R27><C52><LINE>" SKIP
"<R23><C65><FROM><R27><C65><LINE>" SKIP
"<R23><C72><FROM><R27><C72><LINE>" SKIP
.
PUT "<FArial><=4><R+1> Date Req.             FOB                     Ship Via                              Terms                        Sales Person              Order#    From Quote#" SKIP
"<FCourier New><=4><R+3> " lv-due-date FORM "99/99/9999" space(2)
cust.fob-code FORM "x(11)" SPACE(2)
v-shipvia /*carrier.carrier*/ FORM "x(20)" SPACE(1)
oe-ord.terms-d /*terms.dscr*/ FORM "x(15)" space(5) v-salesman space(8) oe-ord.ord-no space(2) v-q-no SKIP.


PUT "<|10><R28><C1><#5><FROM><R30><C82><RECT>" SKIP    
       "<R28><C6><FROM><R30><C6><LINE>" SKIP
       "<R28><C20><FROM><R30><C20><LINE>" SKIP
     /*"<R26><C30><FROM><R28><C30><LINE>" SKIP */
       "<R28><C49><FROM><R30><C49><LINE>" SKIP
       "<R28><C61><FROM><R30><C61><LINE>" SKIP
       "<R28><C72><FROM><R30><C72><LINE>" SKIP
       .

PUT "<FArial><=5><R+1> Line        Customer Part#           Description                                                             Ordered                             Price               UOM" SKIP(1).
PUT "<FCourier New>"  .
v-printline = v-printline + 6.
