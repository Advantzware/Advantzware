/* ar/rep/invcscgsm.i */

PUT "<FArial>".
PUT "<C+25><#1>".
PUT "<=1>" SKIP.
PUT "<C1><#2><R+5><C+40><IMAGE#2=" ls-full-img1 SKIP
    "<P9><=2><R+5>" SKIP
    v-comp-add1 AT 23 SKIP
    v-comp-add2 AT 23 SKIP
    v-comp-add3 AT 23 SKIP
    v-comp-add4 AT 23 SKIP
    v-comp-add5 AT 23 SKIP
    lv-email    AT 23 SKIP
    "<FCourier New><P10>"
    "Bill To:"  "<C44>Ship To:" SKIP
    SPACE(7) ar-inv.cust-name "<C53>" v-shipto-name skip
    SPACE(7) ar-inv.addr[1]   "<C53>" v-shipto-addr[1] SKIP
    SPACE(7) ar-inv.addr[2]  "<C53>" v-shipto-addr[2] SKIP
    SPACE(7) v-addr3   "<C53>" v-sold-addr3 SKIP.
v-printline = v-printline + 18.

PUT "<|10><R4><C50><#3><FROM><R10><C80><RECT>" SKIP.
PUT "<R6><C50><FROM><R6><C80><LINE>" SKIP
    "<R8><C50><FROM><R8><C80><LINE>" SKIP
    "<R4><C62><FROM><R6><C62><LINE>" SKIP
    "<R6><C65><FROM><R8><C65><LINE>" SKIP
    "<R8><C65><FROM><R10><C65><LINE>" SKIP.
        
PUT "<FArial><P12><=#3><R-2> <B>Invoice#: " ar-inv.inv-no "</B><P10>" SKIP
    "<=#3> Customer ID             Contact"
    "<=#3><R+2> Telephone                        Fax" 
    "<=#3><R+4>                                          Invoice Date <FCourier New>"    
    "<=3><R+1> " ar-inv.cust-no  space(7) cust.contact
    "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999" space(5) cust.fax
    "<=3><R+5> " v-trailer SPACE(3) v-inv-date.
    
PUT "<|10><R20><C1><#4><FROM><R24><C80><RECT>" SKIP
    "<R22><C1><FROM><R22><C80><LINE>" SKIP    
    "<R20><C11><FROM><R24><C11><LINE>" SKIP
    "<R20><C19><FROM><R24><C19><LINE>" SKIP
    "<R20><C31><FROM><R24><C31><LINE>" SKIP
    "<R20><C56><FROM><R24><C56><LINE>" SKIP
    "<R20><C74><FROM><R24><C74><LINE>" SKIP.

v-printline = v-printline + 5.

PUT "<FArial><=4><R+1> Ship Date              BOL#                 Ship Via                          Terms                                              Sales Person                  FOB" SKIP
    "<FCourier New><=4><R+3> " v-date-ship FORM "99/99/9999" space(2)
    ar-invl.bol-no SPACE(1)
    v-shipvia FORM "x(14)" SPACE(1)               
    ar-inv.terms-d FORM "x(28)" space(2) v-salesman FORM "x(20)" SPACE(1)
    v-fob FORM "x(7)" 
    SKIP.

PUT "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
    "<R25><C8><FROM><R27><C8><LINE>" SKIP
    "<R25><C15><FROM><R27><C15><LINE>" SKIP
    "<R25><C25.5><FROM><R27><C25.5><LINE>" SKIP
    "<R25><C38.5><FROM><R27><C38.5><LINE>" SKIP
    "<R25><C58><FROM><R27><C58><LINE>" SKIP
    "<R25><C66.5><FROM><R27><C66.5><LINE>" SKIP
    "<R25><C70.5><FROM><R27><C70.5><LINE>" SKIP
    "<FArial><=5><R+0>                                            Cust PO        Customer Part#" SKIP
    "<FArial><=5><R+1>   Ordered     Shipped          Order#          Finished Goods #          Description                                    Price      UOM          Amount" SKIP(1).

v-printline = v-printline + 4.

PUT "<FCourier New>".
