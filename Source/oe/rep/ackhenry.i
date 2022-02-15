/* oe/rep/ackhenry.i */

PUT "<FArial>".
     PUT "" SKIP. /* pacific package */ 
           
PUT "<=1>" SKIP.
IF AVAIL bf-shipto AND bf-shipto.broker THEN do:
  PUT "<C1><#2>" 
    "<=2><R+1>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"     
   "<P10><=2><R+2>"
     v-comp-add1 AT 8 SKIP
     v-comp-add2  AT 8  SKIP
     v-comp-add3  AT 8 SKIP
     v-comp-add4  AT 8 skip
     v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
     lv-email AT 8 SKIP(1) .
END.
ELSE do:
    PUT "<C2><R2><#1><R+10><C+40><IMAGE#1=" ls-full-img1  SKIP .
END.
  PUT
   "<FCourier New>"
   "Bill To:" SPACE(30) "Ship To:"  SKIP
   SPACE(5) oe-ord.cust-name 
    v-ship-name AT 45 FORM "x(30)" skip
   SPACE(5) oe-ord.addr[1] 
    v-ship-add[1] AT 45 FORM "x(30)" SKIP
   SPACE(5) oe-ord.addr[2] 
    v-ship-add[2] AT 45 FORM "x(30)" SKIP
   SPACE(5) v-addr3  v-ship-add[3] AT 45 SKIP.

 IF AVAIL bf-shipto AND bf-shipto.broker THEN
        PUT "<=2><C3><FGCOLOR=" trim(lv-comp-color) + ">"
            "<=2><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
            "<P10>".

v-printline = v-printline + 14.
PUT "<|10><R4><C50><#3><FROM><R10><C80><RECT>" SKIP.
PUT "<R6><C50><FROM><R6><C80><LINE>" SKIP      
    "<R8><C50><FROM><R8><C80><LINE>" SKIP      
   "<R4><C64><FROM><R10><C64><LINE>" SKIP
   .

PUT "<FArial><P12><=#3><R-2> <B>Acknowledgement</B> " "<P10>" SKIP
    "<=#3> Customer ID                 <B>Order Number</B>"
    "<=#3><R+2> Telephone                     Contact" 
    "<=#3><R+4> Customer PO                Order Date <FCourier New>"
    "<=3><R+1> " oe-ord.cust-no  space(9) "<B>" oe-ord.ord-no "</B>"
    "<=3><R+3> " v-cust-phone  space(4) oe-ord.contact
    "<=3><R+5> " oe-ord.po-no space(2) oe-ord.ord-date .
  

PUT "<|10><R19><C1><#4><FROM><R23><C80><RECT>" SKIP
"<R21><C1><FROM><R21><C80><LINE>" SKIP    
"<R19><C11><FROM><R23><C11><LINE>" SKIP
"<R19><C22><FROM><R23><C22><LINE>" SKIP
"<R19><C42><FROM><R23><C42><LINE>" SKIP
"<R19><C52><FROM><R23><C52><LINE>" SKIP
"<R19><C65><FROM><R23><C65><LINE>" SKIP
"<R19><C72><FROM><R23><C72><LINE>" SKIP
.
PUT "<FArial><=4><R+1> Date Req.             FOB                             Ship Via                               Terms                Sales Person            Order#      Quote#" SKIP
"<FCourier New><=4><R+3> " lv-due-date FORM "99/99/9999" space(2)
oe-ord.fob-code FORM "x(11)" SPACE(2) /* gdm 01060906 */
v-shipvia /*carrier.carrier*/ FORM "x(20)" SPACE(5)
oe-ord.terms-d /*terms.dscr*/ FORM "x(15)" space(1) v-salesman space(8) oe-ord.ord-no space(2) v-q-no FORMAT ">>>>>9" SKIP.

IF v-print-fmt eq "Henry" THEN
DO:

        PUT "<|10><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
               "<R24><C6><FROM><R26><C6><LINE>" SKIP
               "<R24><C20><FROM><R26><C20><LINE>" SKIP
             /*"<R26><C30><FROM><R28><C30><LINE>" SKIP */
               "<R24><C48><FROM><R26><C48><LINE>" SKIP
               "<R24><C57><FROM><R26><C57><LINE>" SKIP
               "<R24><C65><FROM><R26><C65><LINE>" SKIP
               "<R24><C70><FROM><R26><C70><LINE>" SKIP
               .

        PUT "<FArial><=5><R+1> Line        Customer Part#           Description                                                         Ordered            Price         UOM   Extended Total" SKIP(1).

END.
ELSE DO:

        PUT "<|10><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
               "<R24><C6><FROM><R26><C6><LINE>" SKIP
               "<R24><C20><FROM><R26><C20><LINE>" SKIP
             /*"<R26><C30><FROM><R28><C30><LINE>" SKIP */
               "<R24><C49><FROM><R26><C49><LINE>" SKIP
               "<R24><C61><FROM><R26><C61><LINE>" SKIP
               "<R24><C72><FROM><R26><C72><LINE>" SKIP
               .

        PUT "<FArial><=5><R+1> Line        Customer Part#           Description                                                             Ordered                             Price          UOM" SKIP(1).

END.
PUT "<FCourier New>"          .
v-printline = v-printline + 6.

