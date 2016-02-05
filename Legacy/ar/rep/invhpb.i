/* ar/rep/invhpb.i */

PUT "<FCourier New>".
PUT "<C3><R3><#1><R+10><C+65><IMAGE#1=" ls-full-img1 SKIP. 
PUT "<P10><R15><C1><#2>" 
            SPACE(6) "Bill To:" SPACE(45) "Ship To:" SKIP(1)
            SPACE(6) ar-inv.cust-name v-shipto-name AT 60 skip
            SPACE(6) ar-inv.addr[1]   v-shipto-addr[1] AT 60 SKIP
            SPACE(6) ar-inv.addr[2]  v-shipto-addr[2] AT 60 SKIP
            SPACE(6) v-addr3   v-sold-addr3 AT 60 SKIP.
v-printline = v-printline + 18.

PUT "<||3></B><P10><R8><C50><#3><FROM><R14><C80><RECT><||3>" SKIP.
PUT "<=3><R+2><C50><FROM><C80><LINE><||3>" SKIP
    "<=3><R+4><C50><FROM><C80><LINE><||3>" SKIP
    "<=3><C62><FROM><R+2><C62><LINE><||3>" SKIP
    "<=3><C65.5><R+2><FROM><R+2><C65.5><LINE><||3>" SKIP
    "<=3><C69.7><R12><FROM><C69.7><R14><LINE><||3>" SKIP.
        
PUT "<P10><=#3>" SKIP
    "<=#3> Customer ID   Invoice#"
    "<=#3><R+2> Telephone         Fax" 
    "<=#3><R+4> Contact                Invoice Date"    
    "<=3><R+1> " ar-inv.cust-no  space(8) ar-inv.inv-no FORM ">>>>>9"
    "<=3><R+3> " cust.area-code + cust.phone format "(999) 999-9999" space(4) cust.fax
    "<=3><R+5> " cust.contact FORM "x(22)" SPACE (3) v-inv-date .

    
PUT "<R22><C1><#4><FROM><R26><C81><RECT><||3>" SKIP
    "<=4><R+2><C1><FROM><C81><LINE><||3>" SKIP    
    "<=4><C11><FROM><R+4><C11><LINE><||3>" SKIP
    "<=4><C22><FROM><R+4><C22><LINE><||3>" SKIP
    "<=4><C38><FROM><R+4><C38><LINE><||3>" SKIP
    "<=4><C55><FROM><R+4><C55><LINE><||3>" SKIP
    "<=4><C72><FROM><R+4><C72><LINE><||3>" SKIP.
v-printline = v-printline + 5.


PUT "<=4><R+1> Ship Date       FOB           Ship Via             Terms         Salesperson's Name     BOL#" SKIP
     "<=4><R+3> " v-date-ship FORM "99/99/9999" space(2)
     v-fob FORM "x(14)" 
     v-shipvia FORM "x(18)" 
     ar-inv.terms-d FORM "x(20)"
     v-slsname FORM "x(20)"
     lv-bol-no
    SKIP.


PUT "<R27><C1><#5><FROM><R29><C81><RECT><||3>" SKIP    
    "<=5><C6><FROM><R+2><C6><LINE><||3>" SKIP
    "<=5><C19><FROM><R+2><C19><LINE><||3>" SKIP
    "<=5><C25><FROM><R+2><C25><LINE><||3>" SKIP
    "<=5><C38><FROM><R+2><C38><LINE><||3>" SKIP
    "<=5><C59><FROM><R+2><C59><LINE><||3>" SKIP
    "<=5><C67><FROM><R+2><C67><LINE><||3>" SKIP
    "<=5><C72><FROM><R+2><C72><LINE><||3>" SKIP.

PUT "<=5><R+1> Order    Cust PO#     Ship  Item#/CustPart#        Description         Price    UOM    Amount" SKIP(1).
v-printline = v-printline + 4.
          


