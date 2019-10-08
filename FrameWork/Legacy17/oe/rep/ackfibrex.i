/* oe/rep/ackfibrex.i */

PUT "<FMS Mincho>"
    "<C1><R+7><C+35><IMAGE#1=" ls-full-img1  SKIP
    "<FArial><P18><R1.5><C51.5><B>Acknowledgement</B> <P10>" SKIP
    "<FMS Mincho>"
    "<=1><R+5><P10>"
    SKIP(1)
    "15250 Don Julian Road" AT 6 SKIP
    "Industry, CA 91745" AT 6 SKIP
    v-comp-add4 AT 6 SKIP
    v-comp-add5 AT 6 SKIP
    "www.fleetwood-fibre.com" AT 6 SKIP
   SKIP(1)
   "Bill To:" AT 4 SPACE(30) "Sold To: <P9>"  SKIP
   SPACE(9) oe-ord.cust-name 
    oe-ord.sold-name AT 52 FORM "x(30)" skip
   SPACE(9) oe-ord.addr[1] 
    oe-ord.sold-addr[1] AT 52 FORM "x(30)" SKIP
   SPACE(9) oe-ord.addr[2] 
    oe-ord.sold-addr[2] AT 52 FORM "x(30)" SKIP
   SPACE(9) v-addr3  v-sold-addr3 AT 52 "<P10>" SKIP

   "<||3><R4><C50><#3><FROM><R6><C75><RECT>" SKIP
   "<R6><C50><FROM><R6><C75><LINE>" SKIP    
   "<R4><C61><FROM><R6><C61><LINE>" SKIP
   "<R20><C8><FROM><R24><C8><LINE>" SKIP
   "<R20><C32><FROM><R24><C32><LINE>" SKIP
   "<R20><C50><FROM><R24><C50><LINE>" SKIP
   "<R20><C69.5><FROM><R24><C69.5><LINE>" SKIP
   "<=#3><P10>" SKIP
   "<=#3> Customer ID         Order Date"
   "<=3><R+1> " oe-ord.cust-no  space(12) oe-ord.ord-date
   "<|10><R20><C1><#4><FROM><R24><C80><RECT>" SKIP
   "<R22><C1><FROM><R22><C80><LINE>" SKIP   
/*    "<R20><C11><FROM><R24><C11><LINE>" SKIP */
   "<R25><C6><FROM><R27><C6><LINE>" SKIP
   "<R25><C21><FROM><R27><C21><LINE>" SKIP
   "<R25><C45><FROM><R27><C45><LINE>" SKIP
   "<R25><C62><FROM><R27><C62><LINE>" SKIP
   "<R25><C75><FROM><R27><C75><LINE>" SKIP
   "<=4><R+1>   FOB               SHIP VIA                         TERMS                  SALES PERSON            ORDER#" SKIP
   "<=4><R+3> " cust.fob-code FORM "x(10)" v-shipvia FORM "x(30)" SPACE(5) oe-ord.terms-d FORM "x(24)" space(1) v-salesman FORM "x(20)" space(8) oe-ord.ord-no SKIP
   "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP
   "<=5><R+1><C1>  LINE     CUSTOMER PART#               ITEM NAME                    QUANTITY                  PRICE        UOM" SKIP(1).

v-printline = v-printline + 20.
