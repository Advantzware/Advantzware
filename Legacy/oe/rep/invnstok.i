/*oe/rep/invnstok.i  */

     PUT "<FArial>".
        PUT "<C+25><#1>". /*<R+5><C+25><IMAGE#1=" ls-full-img1 SKIP. /* pacific package */ */
        PUT "<=1>" SKIP.
        PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
            "<P10><=2><R+12>" SKIP
            /*space(3) v-comp-add1 SKIP
            space(3) v-comp-add2   SKIP
            space(3) v-comp-add3   SKIP
            space(3) v-comp-add4 SKIP(1) */
            "<FCourier New>"
            space(7) "Bill To:"                           "         Ship To:" AT 58 SKIP
            SPACE(7) inv-head.cust-name           v-shipto-name AT 53 skip
            SPACE(7) inv-head.addr[1]             v-shipto-addr[1] AT 53 SKIP
            SPACE(7) inv-head.addr[2]             v-shipto-addr[2] AT 53 SKIP
            SPACE(7) v-addr3                      v-sold-addr3 AT 53 . 
            /*"Ship To:" AT 55 SKIP
            v-shipto-name AT 63 skip
            v-shipto-addr[1] AT 63 SKIP
            v-shipto-addr[2] AT 63 SKIP
            v-sold-addr3 AT 63 SKIP(1)            
            .*/
        v-printline = v-printline + 10.
        PUT "<|10><R4><C50><#3><FROM><R10><C80><RECT>" SKIP.
        PUT "<R6><C50><FROM><R6><C80><LINE>" SKIP
            "<R8><C50><FROM><R8><C80><LINE>" SKIP
            "<R4><C62><FROM><R6><C62><LINE>" SKIP
            "<R6><C65><FROM><R8><C65><LINE>" SKIP
            "<R8><C65><FROM><R10><C65><LINE>" SKIP.
        
PUT "<FArial><P12><=#3><R2.5> <B>Invoice#: " inv-head.inv-no "</B><P10>" SKIP
    "<=#3> Customer ID             Contact"
    "<=#3><R+2> Telephone                        Fax" 
    "<=#3><R+4> Customer PO                  Invoice Date <FCourier New>"    
    "<=3><R+1> " inv-head.cust-no  space(7) cust.contact
    "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999" space(5) cust.fax
    "<=3><R+5> " v-rel-po-no space(3) v-inv-date .
    
PUT "<|10><R23><C1><#4><FROM><R27><C80><RECT>" SKIP
    "<R25><C1><FROM><R25><C80><LINE>" SKIP    
    "<R23><C11><FROM><R27><C11><LINE>" SKIP
    "<R23><C22><FROM><R27><C22><LINE>" SKIP
    "<R23><C42><FROM><R27><C42><LINE>" SKIP
    "<R23><C62><FROM><R27><C62><LINE>" SKIP
    "<R23><C72><FROM><R27><C72><LINE>" SKIP
    .
v-printline = v-printline + 5.


PUT "<FArial><=4><R+1> Ship Date             FOB                    Ship Via                                          Terms                                          S.Person            BOL#" SKIP
     "<FCourier New><=4><R+3> " v-date-shipa FORM "x(10)" space(2)
     v-fob FORM "x(12)" SPACE(1)
     v-shipvia FORM "x(20)" SPACE(5)
     xinv-head.terms-d FORM "x(15)" space(11) v-salesman FORM "x(8)" 
     xinv-head.bol-no
    SKIP.


PUT "<|10><R28><C1><#5><FROM><R30><C80><RECT>" SKIP    
                "<R28><C8><FROM><R30><C8><LINE>" SKIP
                "<R28><C17><FROM><R30><C17><LINE>" SKIP
                "<R28><C25><FROM><R30><C25><LINE>" SKIP
                "<R28><C51><FROM><R30><C51><LINE>" SKIP
                "<R28><C57><FROM><R30><C57><LINE>" SKIP
                "<R28><C65><FROM><R30><C65><LINE>" SKIP
                "<R28><C69><FROM><R30><C69><LINE>" SKIP
                .   
PUT "<FArial><=5><R+1> Order Qty        Shipped      Invoiced                             Description                                 Ln#         Price        UOM           Amount" SKIP(1).
v-printline = v-printline + 4.
           
/*  page.   ???*/
PUT "<FCourier New>".

