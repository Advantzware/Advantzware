/* ar/rep/invptreelot.i  */

PUT "<B><FTimes New Roman>"
    "<C3><R1><#1>"
    "<R+12><C+90><IMAGE#1=" ls-full-img1 SKIP(2)
    "<=1>" SKIP
    "<C1><#2>"
    "<P10><=2><R+10>"
    "<FCourier New>"
    SKIP(1)
    space(12) "BILL TO:" SPACE(43) "SHIP TO:" SKIP
    SPACE(14) v-soldto-name v-shipto-name AT 64 skip
    SPACE(14) v-soldto-addr[1]   v-shipto-addr[1] AT 64 SKIP
    SPACE(14) v-soldto-addr[2]  v-shipto-addr[2] AT 64 SKIP
    SPACE(14) v-addr3   v-sold-addr3 AT 64 "</B>" SKIP.

v-printline = v-printline + 15.

PUT "<|10><R5><C53><#3><FROM><R7><C78><RECT>" SKIP
    "<R6><C53><FROM><R6><C78><LINE>" SKIP
    "<R5><C65><FROM><R7><C65><LINE>" SKIP .
        
PUT "<FArial><P12><=#3><R-2> <P10>" ip-copy-title FORM "x(20)" SKIP
    "<=#3>          INVOICE#                    " ar-inv.inv-no
    "<=#3><R+1>              DATE               " v-inv-date "<FCourier New>"    
    SKIP(1).

PUT "<|10><R21><C1><#4><FROM><R24><C80><RECT>" SKIP
    "<R22><C1><FROM><R22><C80><LINE>" SKIP    
    "<R21><C11><FROM><R24><C11><LINE>" SKIP
    "<R21><C22><FROM><R24><C22><LINE>" SKIP
    "<R21><C38><FROM><R24><C38><LINE>" SKIP
    "<R21><C52><FROM><R24><C52><LINE>" SKIP
                                       SKIP 
    "<R21><C72><FROM><R24><C72><LINE>" SKIP.

v-printline = v-printline + 3.
FIND FIRST sman WHERE sman.company = ar-inv.company 
                  AND sman.sman = v-salesman
                  NO-LOCK NO-ERROR.
v-salesname = IF AVAIL sman THEN sman.sname ELSE "".

PUT "<FArial><=4>  SHIP DATE             FOB                      SHIP VIA                            TERMS                      SALESMAN NAME                   BOL#" SKIP
     "<FCourier New><=4><R+2><B> " v-date-ship FORM "99/99/9999" space(2)
     v-fob FORM "x(12)" SPACE(1)
     v-shipvia FORM "x(20)" SPACE(1)
     ar-inv.terms-d FORM "x(15)" space(1)
     v-salesname FORM "x(23)"
     lv-bol-no FORM ">>>>>>>" "</B>"
    SKIP.

PUT 
    "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
    "<R25><C13><FROM><R27><C13><LINE>" SKIP
    "<R25><C25><FROM><R27><C25><LINE>"     
    "<R25><C52><FROM><R27><C52><LINE>" SKIP
    "<R25><C60><FROM><R27><C60><LINE>" SKIP
    "<R25><C65><FROM><R27><C65><LINE>" 
    "<R25><C71><FROM><R27><C71><LINE>" SKIP.   

PUT "<FArial><=5>  CUST PO#              CUST PART#                                                                                SHIPPED<C62> P       PRICE                  " SKIP
    "  OUR ORDER#        ITEM#                                     DESCRIPTION/LOT#                    INVOICE        C       (UOM)        AMOUNT" SKIP(1).

v-printline = v-printline + 3.
           
PUT "<FCourier New><B>".
