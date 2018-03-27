/* oe/rep/invknight1.i  */

PUT "<FTimes New Roman>"
    "<C3><R4><#2>"
    "<R+8><C+45><IMAGE#1=" ls-full-img1 SKIP(3)
    "<=1>"
    "<C1><#2>"
    "<P10><=2><R+6>"
    "<FCourier New>"
    SPACE(12) " " SKIP
    SPACE(12) " " SKIP
    SPACE(12) " "
    SKIP(2)
    space(12) "BILL TO:" SPACE(43) "SHIP TO:" SKIP
    SPACE(12) inv-head.cust-name v-shipto-name AT 64 skip
    SPACE(12) inv-head.addr[1]   v-shipto-addr[1] AT 64 SKIP
    SPACE(12) inv-head.addr[2]  v-shipto-addr[2] AT 64 SKIP
    SPACE(12) v-addr3   v-sold-addr3 AT 64 "</B>" SKIP.

v-printline = v-printline + 15.
PUT "<|10><R7><C53><#3><FROM><R9><C78><RECT>" SKIP
    "<R8><C53><FROM><R8><C78><LINE>" SKIP
    "<R7><C65><FROM><R9><C65><LINE>" SKIP
    "<FArial><P12><=#3><R-2> <P10>" ip-copy-title FORM "x(20)" SKIP
    "<=#3>          INVOICE#                    " inv-head.inv-no
    "<=#3><R+1>              DATE               " v-inv-date "<FCourier New>"
    SKIP(1)
    "<|10><R21><C1><#4><FROM><R24><C80><RECT>" SKIP
    "<R22><C1><FROM><R22><C80><LINE>" SKIP    
    "<R21><C11><FROM><R24><C11><LINE>" SKIP
    "<R21><C22><FROM><R24><C22><LINE>" SKIP
    "<R21><C40><FROM><R24><C40><LINE>" SKIP
    "<R21><C53><FROM><R24><C53><LINE>" SKIP
    SKIP 
    "<R21><C72><FROM><R24><C72><LINE>" SKIP.

FIND FIRST sman WHERE sman.company = inv-head.company 
                  AND sman.sman = v-salesman
                  NO-LOCK NO-ERROR.
ASSIGN
   v-salesname = IF AVAIL sman THEN sman.sname ELSE ""
   v-printline = v-printline + 3
   v-salesname = "".   /* temp taken out to not show salesman BJ */

PUT "<FArial><=4>   SHIP DATE              FOB                           SHIP VIA                          TERMS                     SALESMAN NAME               BOL#" SKIP
     "<FCourier New><=4><R+2><B> " v-date-ship FORM "99/99/9999" space(2)
     v-fob FORM "x(12)" SPACE(1)
     v-shipvia FORM "x(20)" SPACE(1)
     xinv-head.terms-d FORM "x(15)" space(1) 
     v-salesname FORM "x(23)" 
     xinv-head.bol-no "</B>"
    SKIP 
   "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
    "<R25><C13><FROM><R27><C13><LINE>" SKIP
    "<R25><C25><FROM><R27><C25><LINE>"     
    "<R25><C52><FROM><R27><C52><LINE>" SKIP
    "<R25><C61><FROM><R27><C61><LINE>" SKIP
   /* "<R25><C65><FROM><R27><C65><LINE>" */
    "<R25><C71><FROM><R27><C71><LINE>" SKIP
    "<FArial><=5>  CUST PO#              CUST PART#                                                                           <C62>  P      PRICE                  " SKIP
    "  OUR ORDER#        ITEM#                                     DESCRIPTION                                 SHIPPED     C      (UOM)        AMOUNT" SKIP(1).

v-printline = v-printline + 3.
           
PUT "<FCourier New><B>".
