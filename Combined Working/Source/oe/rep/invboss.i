/* oe/rep/invboss.i */

/*PUT "<FArial>".
PUT "<C+25><#1>". */
PUT "<FTimes New Roman>".
        PUT "<C3><R3><#1>"
             "<R+3><C+45><IMAGE#1=" ls-full-img1 SKIP(2). /* image */ .

PUT "<=1>" SKIP.
PUT "<C1><#2>"
  /*  "<P10><=2><R+4>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)" */
    "<P10><=2><R+4>" 
    space(4) v-comp-add1  SKIP .
    IF v-comp-add2 NE "" THEN PUT space(4) v-comp-add2 SKIP .
    IF v-comp-add3 NE "" THEN PUT space(4) v-comp-add3 SKIP .
    IF v-comp-add4 NE "" THEN PUT space(4) v-comp-add4 SKIP .
    IF v-comp-add5 NE "" THEN PUT space(4) v-comp-add5 SKIP .
    PUT SKIP(2) .
   /* space(3) "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" lv-email FORMAT "X(48)" SKIP(1) */
PUT "<FCourier New>"
    space(7) "Bill To:" SPACE(47) "Ship To:" SKIP
    SPACE(7) inv-head.cust-name v-shipto-name AT 63 skip
    SPACE(7) inv-head.addr[1]   v-shipto-addr[1] AT 63 SKIP.
   IF inv-head.addr[2] NE "" OR v-shipto-addr[2] NE "" THEN
  PUT /*"<FCourier New>"*/
    SPACE(7) inv-head.addr[2]   v-shipto-addr[2] AT 63 SKIP.
 PUT /*"<FCourier New>"*/
    SPACE(7) v-addr3   v-sold-addr3 AT 63 SKIP.
v-printline = v-printline + 18.

PUT "</B><P10><R4><C50><#3><FROM><R7><C81><RECT><||3>" SKIP.
PUT "<R5.5><C50><FROM><R5.5><C81><LINE><||3>" SKIP
/*    "<R8><C50><FROM><R8><C80><LINE><||3>" SKIP */
    "<R4><C65><FROM><R7><C65><LINE><||3>" SKIP
 /*   "<R6><C65><FROM><R8><C65><LINE><||3>" SKIP
    "<R8><C65><FROM><R10><C65><LINE><||3>" SKIP */ .

PUT "<FArial><P12><=#3>     Invoice#:                 Invoice Date <FCourier New>"    SKIP
    "<=3><R+1.6> " SPACE(1) inv-head.inv-no  "<C67>" v-inv-date .

    
PUT "<R21><C35><#4><FROM><R25><C81><RECT><||3>" SKIP
    "<R23><C35><FROM><R23><C81><LINE><||3>" SKIP    
   /* "<R21><C11><FROM><R25><C11><LINE><||3>" SKIP */
  /*  "<R21><C20><FROM><R25><C20><LINE><||3>" SKIP */
    "<R21><C47><FROM><R25><C47><LINE><||3>" SKIP 
    "<R21><C69><FROM><R25><C69><LINE><||3>" SKIP
 /*   "<R21><C65><FROM><R25><C65><LINE><||3>" SKIP
    "<R21><C72><FROM><R25><C72><LINE><||3>" SKIP */
    .
v-printline = v-printline + 5.


PUT "<FArial><=4><R+1><C37>Ship Date<C55>Terms<C71>Sales Rep       " SKIP(.6)
     "<FCourier New><=4><R+2.8><C36>" v-date-ship FORM "99/99/9999" space(2)
   /*  v-fob FORM "x(12)" SPACE(1)  
     "<C22>" v-shipvia FORM "x(20)" SPACE(1) */
     "<C48>"xinv-head.terms-d FORM "x(15)" "<C71>" v-salesman FORM "x(8)"
   /*  v-tot-pallets FORM "->>>>,>>9" 
     xinv-head.bol-no */
    SKIP.

PUT "<R25.1><C1><#5><FROM><R27.1><C81><RECT><||3>" SKIP    
                "<R25.1><C8><FROM><R27.1><C8><LINE><||3>" SKIP 
               /* "<R26><C15><FROM><R28><C15><LINE><||3>" SKIP */
                "<R25.1><C23><FROM><R27.1><C23><LINE><||3>" SKIP
                "<R25.1><C37><FROM><R27.1><C37><LINE><||3>" SKIP
                "<R25.1><C56><FROM><R27.1><C56><LINE><||3>" SKIP
                "<R25.1><C65><FROM><R27.1><C65><LINE><||3>" SKIP
                "<R25.1><C69><FROM><R27.1><C69><LINE><||3>" SKIP .   

PUT "<FArial><=5> Shipped<C12.5>Order#/" SKIP .
PUT "<FArial><=5><R+1> Invoiced<C14>Po#<C24>Item#/CustPart#<C42.5>Description<C58.7>Price<C65.7>UM<C72>Amount" SKIP(1).
v-printline = v-printline + 3.
           

PUT "<FCourier New>".
