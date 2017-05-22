/* oe/rep/invxapc.i */

PUT "<FArial>".
PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP. /* APC package */ 
PUT "<=1><R+11>"
    "<FCourier New>"
    space(5) "Bill To:" SPACE(37) "Ship To:" SKIP
    SPACE(5) inv-head.cust-name v-shipto-name AT 50 skip
    SPACE(5) inv-head.addr[1]   v-shipto-addr[1] AT 50 SKIP
    SPACE(5) inv-head.addr[2]  v-shipto-addr[2] AT 50 SKIP
    SPACE(5) v-addr3   v-sold-addr3 AT 50 SKIP.
v-printline = v-printline + 18.

PUT "</B><P10><R4><C50><#3><FROM><R10><C80><RECT><||3>" SKIP.
PUT "<R6><C50><FROM><R6><C80><LINE><||3>" SKIP
    "<R8><C50><FROM><R8><C80><LINE><||3>" SKIP
    "<R4><C62><FROM><R6><C62><LINE><||3>" SKIP
    "<R6><C65><FROM><R8><C65><LINE><||3>" SKIP
    "<R8><C65><FROM><R10><C65><LINE><||3>" SKIP.
        
PUT "<FArial><P12><=#3><R-2> <B>Invoice#: " inv-head.inv-no "</B><P10>                            Page: " string(PAGE-NUM - v-page-num,">>9") SKIP
    "<=#3> Customer ID             Contact"
    "<=#3><R+2> Telephone                        Fax" 
    "<=#3><R+4> Customer PO                  Invoice Date <FCourier New>"    
    "<=3><R+1> " inv-head.cust-no  space(7) cust.contact
    "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999" space(5) cust.fax
    "<=3><R+5> " v-po-no space(3) v-inv-date .

    
PUT "<R21><C1><#4><FROM><R25><C80><RECT><||3>" SKIP
    "<R23><C1><FROM><R23><C80><LINE><||3>" SKIP    
    "<R21><C11><FROM><R25><C11><LINE><||3>" SKIP
    "<R21><C22><FROM><R25><C22><LINE><||3>" SKIP
    "<R21><C38><FROM><R25><C38><LINE><||3>" SKIP
    "<R21><C55><FROM><R25><C55><LINE><||3>" SKIP
    "<R21><C65><FROM><R25><C65><LINE><||3>" SKIP
    "<R21><C72><FROM><R25><C72><LINE><||3>" SKIP
    .
v-printline = v-printline + 5.


PUT "<FArial><=4><R+1>     Ship Date               FOB                        Ship Via                           Terms                             S.Person        Pallets         BOL#" SKIP
     "<FCourier New><=4><R+3> " v-date-ship FORM "99/99/9999" space(2)
     v-fob FORM "x(12)" SPACE(1)
     v-shipvia FORM "x(20)" SPACE(1)
     xinv-head.terms-d FORM "x(15)" space(4) v-salesman FORM "x(8)"
     v-tot-pallets FORM "->>>>,>>9" SPACE(3)
     v-bol-no
    SKIP.


PUT "<R26><C1><#5><FROM><R28><C80><RECT><||3>" SKIP    
                "<R26><C8><FROM><R28><C8><LINE><||3>" SKIP
                "<R26><C15><FROM><R28><C15><LINE><||3>" SKIP
                "<R26><C21><FROM><R28><C21><LINE><||3>" SKIP
                "<R26><C34><FROM><R28><C34><LINE><||3>" SKIP
                "<R26><C56><FROM><R28><C56><LINE><||3>" SKIP
                "<R26><C65><FROM><R28><C65><LINE><||3>" SKIP
                "<R26><C69><FROM><R28><C69><LINE><||3>" SKIP
                .   
PUT "<FArial><=5><R+1>   Ordered     Shipped      Order      Item#/CustPart#                       Description                             Price       UOM             Amount" SKIP(1).
v-printline = v-printline + 4.
           

PUT "<FCourier New>".
