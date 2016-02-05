/* oe/rep/invdee.i */

PUT "<FCourier New>".
PUT "<C3><R2><#1><R+8><C+65><IMAGE#1=" ls-full-img1 SKIP. 
PUT "<P10><R12><C1><#2>" /*"<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
            "<P10><=2><R+5>"
            space(3) v-comp-add1  SKIP
            space(3) v-comp-add2 SKIP
            space(3) v-comp-add3 SKIP
            space(3) v-comp-add4 SKIP
            space(3) v-comp-add5 SKIP
            space(3) "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" lv-email  SKIP(2) */
            SPACE(6) "Bill To:" SPACE(45) "Ship To:" SKIP(1)
            SPACE(6) ar-inv.cust-name v-shipto-name AT 60 skip
            SPACE(6) ar-inv.addr[1]   v-shipto-addr[1] AT 60 SKIP
            SPACE(6) ar-inv.addr[2]  v-shipto-addr[2] AT 60 SKIP
            SPACE(6) v-addr3   v-sold-addr3 AT 60 SKIP.
v-printline = v-printline + 18.
/*
IF lv-display-comp THEN
   PUT "<=2><C3><R+2><FGCOLOR=" trim(lv-comp-color) + ">"
       "<=2><C3><R+3><P20><B>" lv-comp-name "<FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" . 
*/
PUT "<||3></B><P10><R5><C50><#3><FROM><R11><C80><RECT><||3>" SKIP.
PUT "<=3><R+2><C50><FROM><C80><LINE><||3>" SKIP
    "<=3><R+4><C50><FROM><C80><LINE><||3>" SKIP
    "<=3><C62><FROM><R+2><C62><LINE><||3>" SKIP
    "<=3><R+2><C66><FROM><R+4><C66><LINE><||3>" SKIP.
        
PUT "<P10><=#3>" SKIP
    "<=#3> Customer ID         Invoice#"
    "<=#3><R+2> Telephone                  Fax" 
    "<=#3><R+4> Contact              Invoice Date "    
    "<=3><R+1> " ar-inv.cust-no  space(12) ar-inv.inv-no
    "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999" space(5) cust.fax
    "<=3><R+5> " cust.contact FORM "x(22)"  /*v-po-no*/ v-inv-date .

    
PUT "<R19><C1><#4><FROM><R23><C81><RECT><||3>" SKIP
    "<=4><R+2><C1><FROM><C80><LINE><||3>" SKIP    
    "<=4><C11><FROM><R+4><C11><LINE><||3>" SKIP
    "<=4><C22><FROM><R+4><C22><LINE><||3>" SKIP
    "<=4><C38><FROM><R+4><C38><LINE><||3>" SKIP
    "<=4><C55><FROM><R+4><C55><LINE><||3>" SKIP
/*  "<R23><C60><FROM><R27><C60><LINE><||3>" SKIP*/
    "<=4><C72><FROM><R+4><C72><LINE><||3>" SKIP
    .
v-printline = v-printline + 5.


PUT "<=4><R+1> Ship Date       FOB           Ship Via           Terms           Salesperson's Name     BOL#" SKIP
     "<=4><R+3> " v-date-ship FORM "99/99/9999" space(2)
     v-fob FORM "x(14)" 
     v-shipvia FORM "x(18)" 
     ar-inv.terms-d FORM "x(20)" /*space(1)  v-salesman FORM "x(8)" */
     /*v-tot-pallets FORM "->>>,>>>,>>9" */
     v-slsname FORM "x(20)"
     lv-bol-no
    SKIP.


PUT "<R24><C1><#5><FROM><R26><C81><RECT><||3>" SKIP    
                "<=5><C13><FROM><R+2><C13><LINE><||3>" SKIP
                "<=5><C19><FROM><R+2><C19><LINE><||3>" SKIP
                "<=5><C24><FROM><R+2><C24><LINE><||3>" SKIP
                "<=5><C37><FROM><R+2><C37><LINE><||3>" SKIP
                "<=5><C60><FROM><R+2><C60><LINE><||3>" SKIP
                "<=5><C67><FROM><R+2><C67><LINE><||3>" SKIP
                "<=5><C72><FROM><R+2><C72><LINE><||3>" SKIP
                .   
PUT "<=5><R+1>  Cust PO#      Ship  Order Item#/CustPart#        Description           Price   UOM    Amount" SKIP(1).
v-printline = v-printline + 4.
          


