/* oe/rep/invxprnt.i */

PUT "<FArial>".
PUT "<C+25><#1>".
PUT "<=1>" SKIP.
  PUT "<C2><R2><#1><R+10><C+37><IMAGE#1=" ls-full-img1  SKIP
    "<FCourier New>"
    space(4) "Bill To:" "<C50>" "Ship To:" SKIP
    SPACE(4) inv-head.cust-name "<C50>" v-shipto-name  skip
    SPACE(4) inv-head.addr[1] "<C50>"  v-shipto-addr[1]  SKIP
    SPACE(4) inv-head.addr[2] "<C50>" v-shipto-addr[2]  SKIP
    SPACE(4) v-addr3 "<C50>"  v-sold-addr3  SKIP.
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
    "<=3><R+5> " v-ord-po-no space(3) v-inv-date .

    
PUT "<R21><C1><#4><FROM><R25><C80><RECT><||3>" SKIP
    "<R23><C1><FROM><R23><C80><LINE><||3>" SKIP    
    "<R21><C10.5><FROM><R25><C10.5><LINE><||3>" SKIP
    "<R21><C21.5><FROM><R25><C21.5><LINE><||3>" SKIP
    "<R21><C47.2><FROM><R25><C47.2><LINE><||3>" SKIP
    "<R21><C60.5><FROM><R25><C60.5><LINE><||3>" SKIP
    "<R21><C68><FROM><R25><C68><LINE><||3>" SKIP
    "<R21><C74.5><FROM><R25><C74.5><LINE><||3>" SKIP
    .
v-printline = v-printline + 5.


PUT "<FArial><=4><R+1>     Ship Date               FOB                           Ship Via                                                 Terms               S.Person       Pallets     BOL#" SKIP
     "<FCourier New><=4><R+3> " v-date-ship FORM "99/99/9999" space(1)
     v-fob FORM "x(12)" SPACE(1)
     v-shipvia FORM "x(30)" SPACE(1)
     xinv-head.terms-d FORM "x(15)" space(1) v-salesman FORM "x(8)"
     v-tot-pallets FORM "->>>,>>9" 
     xinv-head.bol-no FORMAT ">>>>>9"
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
IF NOT lPrintQtyAll THEN DO:  
PUT "<FArial><=5>"  space(37) " Order /" SKIP
                   " Ordered     Shipped     Cust PO      Item#/CustPart#                       Description                             Price       UOM             Amount" SKIP(1).
END.
ELSE DO:
PUT "<FArial><=5>" " Ordered" space(25) " Order /" SKIP
                   " Shipped     Invoiced     Cust PO      Item#/CustPart#                       Description                             Price       UOM             Amount" SKIP(1).
END.
v-printline = v-printline + 4.
           

PUT "<FCourier New>".
