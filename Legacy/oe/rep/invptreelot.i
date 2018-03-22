/* ------------------------------------------------------- oe/rep/invptreelot.i */
/* PRINT INVOICE   Xprint form for PEACHTREE FG Lot                    */
/* Modifications: br 01/12/11 Task 12081001 - remove bill to and ship to 
                                customer codes 
*/
/* -------------------------------------------------------------------------- */

PUT "<B><FTimes New Roman>".
PUT "<C7><R1><#1>"
     "<R+12><C+90><IMAGE#1=" ls-full-img1  SKIP(2).
PUT "<=1>". 
PUT "<C1><#2>"
    "<P10><=2><R+10>"
    "<FCourier New>"
    SKIP(2)
    space(8) "BILL TO:" SPACE(47) "SHIP TO:" SKIP
    SPACE(8) v-soldto-name v-shipto-name AT 68 skip
    SPACE(8) v-soldto-addr[1]  v-shipto-addr[1] AT 68 SKIP
    SPACE(8) v-soldto-addr[2]  v-shipto-addr[2] AT 68 SKIP
    SPACE(8) v-addr3   v-sold-addr3 AT 68 "</B>" SKIP.
v-printline = v-printline + 15.
PUT "<|10><R7><C53><#3><FROM><R9><C78><RECT>" SKIP.
PUT "<R8><C53><FROM><R8><C78><LINE>" SKIP
    "<R7><C65><FROM><R9><C65><LINE>" SKIP .

PUT "<FArial><P12><=#3><R-2> <P10>" ip-copy-title FORM "x(20)" SKIP
    "<=#3>          INVOICE#                    " inv-head.inv-no
    "<=#3><R+1>              DATE               " v-inv-date "<FCourier New>"    
    SKIP(1)
    .

PUT "<|10><R21><C1><#4><FROM><R24><C80><RECT>" SKIP
    "<R22><C1><FROM><R22><C80><LINE>" SKIP    
    "<R21><C11><FROM><R24><C11><LINE>" SKIP
    "<R21><C22><FROM><R24><C22><LINE>" SKIP
    "<R21><C38><FROM><R24><C38><LINE>" SKIP
    "<R21><C52><FROM><R24><C52><LINE>" SKIP SKIP 
    "<R21><C72><FROM><R24><C72><LINE>" SKIP
    .
v-printline = v-printline + 3.

FIND FIRST sman WHERE sman.company = inv-head.company 
                  AND sman.sman = v-salesman
                  NO-LOCK NO-ERROR.
v-salesname = IF AVAIL sman THEN sman.sname ELSE "".

PUT "<FArial><=4>  SHIP DATE             FOB                      SHIP VIA                            TERMS                      SALESMAN NAME                   BOL#" SKIP
     "<FCourier New><=4><R+2><B> " v-date-ship FORM "99/99/9999" space(2)
     v-fob FORM "x(12)" SPACE(1)
     v-shipvia FORM "x(20)" SPACE(1)
     xinv-head.terms-d FORM "x(15)" space(1) 
     v-salesname FORM "x(23)"
     xinv-head.bol-no "</B>"
    SKIP.


PUT 
   "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
    "<R25><C13><FROM><R27><C13><LINE>" SKIP
    "<R25><C25><FROM><R27><C25><LINE>"     
    "<R25><C52><FROM><R27><C52><LINE>" SKIP
    "<R25><C60><FROM><R27><C60><LINE>" SKIP
    "<R25><C65><FROM><R27><C65><LINE>" 
    "<R25><C71><FROM><R27><C71><LINE>" SKIP 
    .   

PUT "<FArial><=5>  CUST PO#              CUST PART#                                                                               SHIPPED/ <C62> P       PRICE                  " SKIP
    "  OUR ORDER#        ITEM#                                     DESCRIPTION/LOT#                   INVOICE        C       (UOM)        AMOUNT" SKIP(1).

v-printline = v-printline + 3.
           
PUT "<FCourier New><B>".
