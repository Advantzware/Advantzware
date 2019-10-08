/* oe/rep/invpacif.i */

PUT "<FArial>".
        PUT "<C+25><#1>". /*<R+5><C+25><IMAGE#1=" ls-full-img1 SKIP. /* pacific package */ */
        PUT "<=1>" SKIP.
        PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
            "<P10><=2><R+9>"
            space(3) v-comp-add1 SKIP
            space(3) v-comp-add2 space(74) "Attn: Account Payable Dept."  SKIP
            space(3) v-comp-add3 space(74) "Remit To: 26895 Gloucester Way Langley, BC V4W 3Y3"  SKIP
            space(3) v-comp-add4 SKIP
            "<FCourier New>"
            "Bill To:" SPACE(30) "Ship To:" SKIP
            SPACE(7) inv-head.cust-name v-shipto-name AT 50 skip
            SPACE(7) inv-head.addr[1]   v-shipto-addr[1] AT 50 SKIP
            SPACE(7) inv-head.addr[2]  v-shipto-addr[2] AT 50 SKIP
            SPACE(7) v-addr3   v-sold-addr3 AT 50 SKIP.
        v-printline = v-printline + 15.
        PUT "<|10><R4><C50><#3><FROM><R10><C80><RECT>" SKIP.
        PUT "<R6><C50><FROM><R6><C80><LINE>" SKIP
            "<R8><C50><FROM><R8><C80><LINE>" SKIP
            "<R4><C62><FROM><R6><C62><LINE>" SKIP
            "<R6><C65><FROM><R8><C65><LINE>" SKIP
            "<R8><C65><FROM><R10><C65><LINE>" SKIP.

 
PUT "<FArial><P12><=#3><R-2> <B>Invoice#: " inv-head.inv-no "</B><P10>"
    string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(15)" AT 65  
 
    SKIP
    "<=#3> Customer ID             Contact"
    "<=#3><R+2> Telephone                        Fax" 
    "<=#3><R+4> Customer PO                  Invoice Date <FCourier New>"    
    "<=3><R+1> " inv-head.cust-no  space(7) cust.contact
    "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999" space(5) cust.fax
    "<=3><R+5> " v-po-no space(3) v-inv-date .

/* same as request - no space between hd & line
PUT "<R11><#4><R17><C80><RECT>" SKIP
    "<R13><C1><FROM><R13><C80><LINE>" SKIP
    "<R15><C1><FROM><R15><C80><LINE>" SKIP

    "<R11><C20><FROM><R15><C20><LINE>" SKIP
    "<R11><C30><FROM><R15><C30><LINE>" SKIP
    "<R11><C40><FROM><R15><C40><LINE>" SKIP
    "<R11><C50><FROM><R15><C55><LINE>" SKIP
    "<R11><C60><FROM><R15><C60><LINE>" SKIP
    "<R11><C70><FROM><R15><C70><LINE>" SKIP
    .
*/  
    
PUT "<|10><R23><C1><#4><FROM><R27><C80><RECT>" SKIP
    "<R25><C1><FROM><R25><C80><LINE>" SKIP    
    "<R23><C11><FROM><R27><C11><LINE>" SKIP
    "<R23><C22><FROM><R27><C22><LINE>" SKIP
    "<R23><C38><FROM><R27><C38><LINE>" SKIP
    "<R23><C52><FROM><R27><C52><LINE>" SKIP
    "<R23><C60><FROM><R27><C60><LINE>" SKIP
    "<R23><C72><FROM><R27><C72><LINE>" SKIP
    .
v-printline = v-printline + 5.


PUT "<FArial><=4><R+1> Ship Date             FOB                    Ship Via                                 Terms                       S.Person         Pallets/Bags         BOL#" SKIP
     "<FCourier New><=4><R+3> " v-date-ship FORM "99/99/9999" space(2)
     v-fob FORM "x(12)" SPACE(1)
     v-shipvia FORM "x(20)" SPACE(1)
     xinv-head.terms-d FORM "x(15)" space(1) v-salesman FORM "x(8)"
     v-tot-pallets FORM "->>>,>>>,>>9" 
     xinv-head.bol-no
    SKIP.


PUT "<|10><R28><C1><#5><FROM><R30><C80><RECT>" SKIP    
                "<R28><C8><FROM><R30><C8><LINE>" SKIP
                "<R28><C15><FROM><R30><C15><LINE>" SKIP
                "<R28><C21><FROM><R30><C21><LINE>" SKIP
                "<R28><C34><FROM><R30><C34><LINE>" SKIP
                "<R28><C56><FROM><R30><C56><LINE>" SKIP
                "<R28><C64><FROM><R30><C64><LINE>" SKIP
                "<R28><C68><FROM><R30><C68><LINE>" SKIP
                .   
PUT "<FArial><=5><R+1>   Ordered     Shipped       B.O.      Item#/CustPart#        Description                                             Price     UOM                 Amount" SKIP(1).
v-printline = v-printline + 4.

PUT "<FCourier New>".
