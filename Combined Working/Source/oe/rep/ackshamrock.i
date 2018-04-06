/* oe/rep/ackshamrock.i */

PUT "<FArial>".
     PUT "" SKIP. /* pacific package */ 
           
PUT "<=1>" SKIP.
/*PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
    "<=2><R+1>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"     
   "<P10><=2><R+2>"
     v-comp-add1 AT 8 SKIP
     v-comp-add2  AT 8  SKIP
     v-comp-add3  AT 8 SKIP
     v-comp-add4  AT 8 skip
     v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
     lv-email AT 8 SKIP(1)*/
    PUT "<C2><R2><#1><R+10><C+40><IMAGE#1=" ls-full-img1  SKIP
   "<FCourier New>"
   "Bill To:" SPACE(30) "Ship To:"  SKIP
   SPACE(5) oe-ord.cust-name 
    v-ship-name AT 45 FORM "x(30)" skip
   SPACE(5) oe-ord.addr[1] 
    v-ship-add AT 45 FORM "x(30)" SKIP
   SPACE(5) oe-ord.addr[2] 
    v-ship-add2 AT 45 FORM "x(30)" SKIP
   SPACE(5) v-addr3  v-ship-add3 AT 45 SKIP.

 /*IF lv-display-comp THEN
        PUT "<=2><C3><FGCOLOR=" trim(lv-comp-color) + ">"
            "<=2><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
            "<P10>".*/

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
    "<=3><R+1> " oe-ord.cust-no  space(5) oe-ord.contact
    "<=3><R+3> " v-cust-phone  space(5) cust.fax
    "<=3><R+5> " oe-ord.po-no space(5) oe-ord.ord-date .

PUT "<|10><R19><C1><#4><FROM><R23><C80><RECT>" SKIP
"<R21><C1><FROM><R21><C80><LINE>" SKIP    
"<R19><C11><FROM><R23><C11><LINE>" SKIP
"<R19><C22><FROM><R23><C22><LINE>" SKIP
"<R19><C38><FROM><R23><C38><LINE>" SKIP
"<R19><C52><FROM><R23><C52><LINE>" SKIP
/*"<R19><C65><FROM><R23><C65><LINE>" SKIP*/
"<R19><C72><FROM><R23><C72><LINE>" SKIP
.
PUT "<FArial><=4><R+1> Date Req.             FOB                     Ship Via                              Terms                           Sales Person                           Order#" SKIP
"<FCourier New><=4><R+3> " lv-due-date FORM "99/99/9999" space(2)
oe-ord.fob-code FORM "x(11)" SPACE(2) /* gdm 01060906 */
v-shipvia /*carrier.carrier*/ FORM "x(20)" SPACE(1)
oe-ord.terms-d /*terms.dscr*/ FORM "x(15)" space(3) v-salesman FORMAT "X(20)" space(2) oe-ord.ord-no SKIP.


PUT "<|10><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
       "<R24><C6><FROM><R26><C6><LINE>" SKIP
       "<R24><C20><FROM><R26><C20><LINE>" SKIP     
       "<R24><C46><FROM><R26><C46><LINE>" SKIP
       "<R24><C56><FROM><R26><C56><LINE>" SKIP
       "<R24><C65><FROM><R26><C65><LINE>" SKIP
       "<R24><C69><FROM><R26><C69><LINE>" SKIP
       .


PUT "<FArial><=5><R+1> Line        Customer Part#           Description                                                       Ordered                Price      UOM       Amount" SKIP(1).
PUT "<FCourier New>"         .
v-printline = v-printline + 6.

