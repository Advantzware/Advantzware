/* oe/rep/invandrop.i */

PUT "<FArial>"
    "<C2><#2><R+9><C+75><IMAGE#1=" ls-full-img1 SKIP
    "<=1>" 
    "<R+9>"
    "<FCourier New>"
    space(3) "Sold To:" SPACE(29) "Ship To:" SKIP
    SPACE(7) inv-head.cust-name v-shipto-name AT 45 skip
    SPACE(7) inv-head.addr[1] v-shipto-addr[1] AT 45 SKIP
    SPACE(7) inv-head.addr[2] v-shipto-addr[2] AT 45 SKIP
    SPACE(7) v-addr3   v-sold-addr3 AT 45 SKIP.

v-printline = v-printline + 18.

PUT "<R3><C50><#3>"
    "<=#3><R-0.8><FArial><C+22><P16><B>Invoice</B><P10>"
    "<=#3><R+1><C+21>Invoice Number:"
    "<=#3><R+2><C+27.5>" inv-head.inv-no
    "<=#3><R+3><C+23>Invoice Date:"
    "<=#3><R+4><C+24>" v-inv-date
    "<=#3><R+5><C+27>Page:" 
    "<=#3><R+6><C+29>" string(PAGE-NUM - v-page-num,">>9")
    "<R17><C1><#4><R17><C1><FROM><R17><C80><LINE><||3>" SKIP
    "<R25><C1><FROM><R25><C80><LINE><||3>" 
    "<R17><C1><FROM><R25><C1><LINE><||3>" 
    "<R17><C80><FROM><R25><C80><LINE><||3>" 
    "<R19><C1><FROM><R19><C80><LINE><||3>" SKIP    
    "<R21><C1><FROM><R21><C80><LINE><||3>" SKIP    
    "<R23><C1><FROM><R23><C80><LINE><||3>" SKIP    
    "<R17><C10><FROM><R25><C10><LINE><||3>" SKIP
    "<R17><C31><FROM><R25><C31><LINE><||3>" SKIP
    "<R17><C45><FROM><R25><C45><LINE><||3>" SKIP
    "<R17><C69><FROM><R25><C69><LINE><||3>" SKIP.

v-printline = v-printline + 5.

PUT "<FArial><=4><R+1>  Customer ID            Customer Phone/FAX                Customer PO                        Payment Terms                           Due Date" SKIP     
    "<FCourier New><=4><R+3>" space(1) inv-head.cust-no  space(7) string(cust.phone,"999-9999") + "," + string(SUBSTRING(cust.fax,4),"999-9999") FORM "x(20)" SPACE(1)
     v-po-no FORM "x(15)" space(3) xinv-head.terms-d FORM "x(25)" space(4)
    v-due-date SKIP
    "<FArial><=4><R+5>   Salesman                          Ship Via                                Order#                                Weight/Pallets                        Bill Lading #" SKIP
    "<FCourier New><=4><R+7> " SPACE(3) v-salesman FORM "x(8)" SPACE(3)
    v-shipvia FORM "x(20)" SPACE(6)
    v-ord-no SPACE(7)
    v-t-weight FORM "->>>>,>>9" SPACE(1)  "/" 
    v-tot-pallets FORM "->>>>,>>9" SPACE(11)
    xinv-head.bol-no
    SKIP
    "<R26><C1><#5><FROM><R48><C80><RECT><||3>" SKIP    
    "<R28><C1><FROM><R28><C80><LINE><||3>" SKIP
    "<R26><C8><FROM><R48><C8><LINE><||3>" SKIP
    "<R26><C23><FROM><R48><C23><LINE><||3>" SKIP
    "<R26><C53><FROM><R48><C53><LINE><||3>" SKIP
    "<R26><C66><FROM><R48><C66><LINE><||3>" SKIP
    "<FArial><=5><R+1>   Quantity                  Item                                               Description                                       Unit Price                    Extension"
    SKIP(1).

v-printline = v-printline + 4.
           
PUT "<FCourier New>".
