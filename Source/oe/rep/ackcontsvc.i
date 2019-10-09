/* oe/rep/ackcontsvc.i */

PUT "<FCourier New>"   
    "<C3><R4><#1><R+8><C+30><IMAGE#1=" ls-full-img1 SKIP. 
    
           
PUT "<FCourier New><=1><R+6><P10>"  /*bigger address */
    SKIP(1)
   "Bill To:" AT 6 SPACE(57) "Ship To:" AT 65 SKIP
   SPACE(5) oe-ord.cust-name v-ship-adr[1] AT 65 skip
   SPACE(5) oe-ord.addr[1]   v-ship-adr[2] AT 65 SKIP
   SPACE(5) oe-ord.addr[2]   v-ship-adr[3] AT 65 SKIP
   SPACE(5) v-addr3          v-ship-adr[4] AT 65  SKIP.

v-printline = v-printline + 14.
PUT "<||3><R4><C50><#3><FROM><R10><C80><RECT>" SKIP.
PUT "<R6><C50><FROM><R6><C80><LINE>" SKIP     /* middle line 1 */ 
    "<R8><C50><FROM><R8><C80><LINE>" SKIP     /* middle line 2 */   
    "<R4><C61><FROM><R6><C61><LINE>" SKIP     /* row 1 col 1/2 */
    "<R4><C69><FROM><R6><C69><LINE>" SKIP     /* row 1 col 2/3 */
    "<R8><C63><FROM><R10><C63><LINE>" SKIP    /* row 3 col 1/2 */
    .

PUT "<R2><C50><FArial><P14><B>Order Acknowledgement" SKIP
    "<=#3><P10>" SKIP
    "<=#3> Order Date            Order#        Delivery Date"
    "<=#3><R+2> Contact"
    "<=#3><R+4> Telephone                 Fax</B><FCourier New>" 
    "<=3><R+1> " oe-ord.ord-date  SPACE(4) oe-ord.ord-no SPACE(3) oe-ord.due-date FORMAT "99/99/9999"
    "<=3><R+3> " cust.contact FORMAT "x(25)" space(7) 
    "<=3><R+5> " v-cust-phone  space(6) cust.fax .

PUT "<|3><R19><C1><#4><FROM><R23><C80><RECT>" SKIP
    "<R21><C1><FROM><R21><C80><LINE>" SKIP    /* via */
    "<R19><C15><FROM><R23><C15><LINE>" SKIP   /* fob */
    "<R19><C24><FROM><R23><C24><LINE>" SKIP   /* frt */
    "<R19><C33><FROM><R23><C33><LINE>" SKIP   /* terms */
    "<R19><C54><FROM><R23><C54><LINE>" SKIP   /*  salesman */
    "<R19><C70><FROM><R23><C70><LINE>" SKIP   /* over/under */
.

PUT "<=4><R+1><FArial><B>            Ship Via                  FOB              Freight"
    "                          Terms                                  Sales Person            Over/Under</B><FCourier New>" SKIP
    "<=4><R+3> "  v-shipvia FORMAT "x(17)" space(1)
    oe-ord.fob FORMAT "x(10)" 
    (     IF oe-ord.frt-pay = "P" THEN "Prepaid"
     ELSE IF oe-ord.frt-pay = "B" THEN "Bill"
     ELSE IF oe-ord.frt-pay = "C" THEN "Collect"
     ELSE "3rd Party") SPACE(2)
     oe-ord.terms-d FORMAT "x(24)" SPACE(1) 
     v-salesman FORMAT "x(18)"  SPACE(2) 
     oe-ord.over-pct FORMAT "99.9/" oe-ord.under-pct FORMAT "99.9"
     SKIP.

PUT "<|3><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
       "<R24><C14.2><FROM><R26><C14.2><LINE>" SKIP
       "<R24><C27><FROM><R26><C27><LINE>" SKIP
       "<R24><C59><FROM><R26><C59><LINE>" SKIP
       "<R24><C67><FROM><R26><C67><LINE>" SKIP
       "<R24><C76.5><FROM><R26><C76.5><LINE>" SKIP.

PUT "<=5><FArial><B> Customer Part#" SKIP
    " Cont Serv FG#          Customer PO#        "
    " Item Description                                                        Quantity        Unit Price   UOM"
    "</B><FCourier New>" SKIP(1).
v-printline = v-printline + 6.                                               
