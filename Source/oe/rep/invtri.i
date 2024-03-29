/* oe/rep/invtri.i */
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No). */
PUT "<FCourier New>".
PUT "<C3><R3><#1><R+8><C+65><IMAGE#1=" ls-full-img1 SKIP. 
PUT "<P10><R15><C1><#2>"
            SPACE(6) "Bill To:" SPACE(56) "Ship To:" SKIP(1)
            SPACE(6) inv-head.cust-name v-shipto-name AT 71 skip
            SPACE(6) inv-head.addr[1]   v-shipto-addr[1] AT 71 SKIP
            SPACE(6) inv-head.addr[2]  v-shipto-addr[2] AT 71 SKIP
            SPACE(6) v-addr3   v-sold-addr3 AT 71 SKIP.
v-printline = v-printline + 18.

PUT "<||3></B><P10><R8><C50><#3><FROM><R14><C80><RECT><||3>" SKIP.
PUT "<=3><R+2><C50><FROM><C80><LINE><||3>" SKIP
    "<=3><R+4><C50><FROM><C80><LINE><||3>" SKIP
    "<=3><C62><FROM><R+2><C62><LINE><||3>" SKIP
    "<=3><R+2><C65><FROM><R+4><C65><LINE><||3>" SKIP.
        
PUT "<P10><=#3>" SKIP
    "<=#3> Customer ID         Invoice#"
    "<=#3><R+2> Telephone                  Fax" 
    "<=#3><R+4> Contact              Invoice Date "    
    "<=3><R+1> " inv-head.cust-no  space(12) inv-head.inv-no
    "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999" space(5) cust.fax
    "<=3><R+5> " cust.contact FORM "x(22)" v-inv-date .

    
PUT "<R22><C1><#4><FROM><R26><C81><RECT><||3>" SKIP
    "<=4><R+2><C1><FROM><C80><LINE><||3>" SKIP    
    "<=4><C11><FROM><R+4><C11><LINE><||3>" SKIP
    "<=4><C22><FROM><R+4><C22><LINE><||3>" SKIP
    "<=4><C38><FROM><R+4><C38><LINE><||3>" SKIP
    "<=4><C55><FROM><R+4><C55><LINE><||3>" SKIP
    "<=4><C72><FROM><R+4><C72><LINE><||3>" SKIP.
v-printline = v-printline + 5.


PUT "<=4><R+1> Ship Date       FOB           Ship Via           Terms           Salesperson's Name     BOL#" SKIP
     "<=4><R+3> " v-date-ship FORM "99/99/9999" space(2)
     v-fob FORM "x(14)" 
     v-shipvia FORM "x(18)" 
     xinv-head.terms-d FORM "x(20)"
     v-slsname FORM "x(20)"
     xinv-head.bol-no
    SKIP.


PUT "<R27><C1><#5><FROM><R29><C81><RECT><||3>" SKIP    
                "<=5><C13><FROM><R+2><C13><LINE><||3>" SKIP
                "<=5><C19><FROM><R+2><C19><LINE><||3>" SKIP
                "<=5><C26><FROM><R+2><C26><LINE><||3>" SKIP
                "<=5><C40><FROM><R+2><C40><LINE><||3>" SKIP
                "<=5><C59><FROM><R+2><C59><LINE><||3>" SKIP
                "<=5><C67><FROM><R+2><C67><LINE><||3>" SKIP
                "<=5><C72><FROM><R+2><C72><LINE><||3>" SKIP
                .   
PUT "<=5><R+1><C2> Cust PO#
             <C13> Ship
             <C20> Order
             <C26> Item#/CustPart#
             <C42> Description
             <C60> Price
             <C67> UOM
             <C73> Amount" SKIP(1).
v-printline = v-printline + 4.
          


