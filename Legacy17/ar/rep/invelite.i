/* ar/rep/invelite.i */

PUT "<FArial>".
PUT "<C+25><#1>". /*<R+5><C+25><IMAGE#1=" ls-full-img1 SKIP. /* company logo */ */
PUT "<=1>" SKIP.
PUT "<C1><#2><R+7><C+65><IMAGE#2=" ls-full-img1 SKIP  /* company image */ 
            "<P10><=2><R+5>" SKIP
            v-comp-add1 AT 37 SKIP
            v-comp-add2 AT 37 /*space(74) "Attn: Account Payable Dept."  */ SKIP
            v-comp-add3 AT 37 /*space(74) "Remit To: 5821 Production Way Langley, BC V3A 4N5"  */ SKIP
            v-comp-add4 AT 37 SKIP
            v-comp-add5 AT 37 SKIP
            lv-email AT 37 SKIP(1)
            "<FCourier New>"
            "Bill To:"  "<C44>Ship To:" SKIP
             SPACE(7) ar-inv.cust-name "<C53>" v-shipto-name skip
            SPACE(7) ar-inv.addr[1]   "<C53>" v-shipto-addr[1] SKIP
            SPACE(7) ar-inv.addr[2]  "<C53>" v-shipto-addr[2] SKIP
            SPACE(7) v-addr3   "<C53>" v-sold-addr3 SKIP.
v-printline = v-printline + 18.
/*IF lv-display-comp THEN PUT "<=2><C3><R+3><P20><B>" lv-comp-name "</B><P10>" . */

PUT "<|10><R4><C50><#3><FROM><R10><C80><RECT>" SKIP.
PUT "<R6><C50><FROM><R6><C80><LINE>" SKIP
    "<R8><C50><FROM><R8><C80><LINE>" SKIP
    "<R4><C62><FROM><R6><C62><LINE>" SKIP
    "<R6><C65><FROM><R8><C65><LINE>" SKIP
    "<R8><C65><FROM><R10><C65><LINE>" SKIP.
        
PUT "<FArial><P12><=#3><R-2> <B>Invoice#: " ar-inv.inv-no "</B><P10>" SKIP
    "<=#3> Customer ID             Contact"
    "<=#3><R+2> Telephone                        Fax" 
    "<=#3><R+4> Customer PO                  Invoice Date <FCourier New>"    
    "<=3><R+1> " ar-inv.cust-no  space(7) cust.contact
    "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999" space(5) cust.fax
    "<=3><R+5> " v-po-no space(3) v-inv-date .

    
PUT "<|10><R23><C1><#4><FROM><R27><C80><RECT>" SKIP
    "<R25><C1><FROM><R25><C80><LINE>" SKIP    
    "<R23><C11><FROM><R27><C11><LINE>" SKIP
    "<R23><C19><FROM><R27><C19><LINE>" SKIP
    "<R23><C35><FROM><R27><C35><LINE>" SKIP
    "<R23><C49><FROM><R27><C49><LINE>" SKIP
    "<R23><C59><FROM><R27><C59><LINE>" SKIP
    "<R23><C68><FROM><R27><C68><LINE>" SKIP
    .
v-printline = v-printline + 5.


PUT "<FArial><=4><R+1> Ship Date              BOL#                  Ship Via                             Terms                     S.Person              Pallets               FOB" SKIP
     "<FCourier New><=4><R+3> " v-date-ship FORM "99/99/9999" space(2)
     v-bol-no SPACE(1)
     v-shipvia FORM "x(20)" SPACE(1)
     ar-inv.terms-d FORM "x(15)" space(1) v-salesman FORM "x(8)"
     v-tot-pallets FORM "->>>,>>>,>>9" SPACE(3)
     v-fob FORM "x(12)" 
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
PUT "<FArial><=5><R+1>   Ordered     Shipped     Order#      Item#/CustPart#       Description                                             Price    UOM                  Amount" SKIP(1).
v-printline = v-printline + 4.
           

PUT "<FCourier New>".
