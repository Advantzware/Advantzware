/* oe/rep/ackdee.i rtc 11/18/2008 Task# 10220804 */
/* copied from  oe/rep/fibrex.i                  */

PUT 
   "<FMS Mincho>"
   "<C3><R2><#1><R+8><C+45><IMAGE#1=" ls-full-img1 SKIP 
   "<FArial><P18><R1.5><C51.5><B>Acknowledgement</B> <P10>" SKIP
   "<FMS Mincho>"
   "<=1><R+5><P8>"
   SKIP(1)

   "Dee Paper Co."        AT 9 SKIP
   "100 Broomall Street"  AT 9 SKIP
   "Chester, PA 19013"    AT 9 SKIP
   "Tel. 610-876-9285"    AT 9 SKIP
   "Fax 610-876-7040"     AT 9 SKIP
   SKIP(1)

   "<P10>Bill To:" AT 4 SPACE(30) "Ship To: <P9>"  SKIP
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
   "<R20><C22><FROM><R24><C22><LINE>" SKIP
   "<R20><C38><FROM><R24><C38><LINE>" SKIP
   "<R20><C53><FROM><R24><C53><LINE>" SKIP
   "<R20><C70><FROM><R24><C70><LINE>" SKIP
   "<=#3><P10>" SKIP
   "<=#3> Customer ID         Order Date"
   "<=3><R+1> " oe-ord.cust-no  space(12) oe-ord.ord-date
   "<|10><R20><C1><#4><FROM><R24><C80><RECT>" SKIP
   "<R22><C1><FROM><R22><C80><LINE>" SKIP   
   "<R20><C11><FROM><R24><C11><LINE>" SKIP
   "<R25><C6><FROM><R27><C6><LINE>" SKIP
   "<R25><C21><FROM><R27><C21><LINE>" SKIP
   "<R25><C45><FROM><R27><C45><LINE>" SKIP
   "<R25><C62><FROM><R27><C62><LINE>" SKIP
   "<R25><C75><FROM><R27><C75><LINE>" SKIP
   "<=4><R+1> Delivery Date      FOB         Over Run/Under Run          Terms               Sales Person          Order #" SKIP
   "<=4><R+3> " oe-ord.due-date FORM "99/99/9999" space(4)
   cust.fob-code FORM "x(14)" SPACE(2)
   oe-ord.over-pct "  / " oe-ord.under-pct SPACE(5) 
   oe-ord.terms-d FORM "x(15)" space(6) v-salesman FORM "x(20)" space(8) oe-ord.ord-no SKIP
   "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP
   "<=5><R+1><C1>  Line     Customer Part#                 Item Name                    Quantity                Price        UOM" SKIP(1).

v-printline = v-printline + 20.
