/* ar/rep/invsthpklg.i */

PUT "<FArial>".
      PUT "<C1><#2>"
       "<P10><=2><R+4>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"  /* company image */ 
       "<P10><=2><R+5>"
            space(3) v-comp-add1 SKIP
            space(3) v-comp-add2 SKIP 
            space(3) v-comp-add3 SKIP
            space(3) v-comp-add4 SKIP
            SPACE(3) v-comp-add5 SKIP 
            space(3) lv-email FORMAT "X(48)" SKIP(1)
            "<FCourier New><P10>"
            "Bill To:" SPACE(30) "Ship To:" SKIP
            SPACE(7) ar-inv.cust-name v-shipto-name AT 50 skip
            SPACE(7) ar-inv.addr[1]   v-shipto-addr[1] AT 50 SKIP
            SPACE(7) ar-inv.addr[2]  v-shipto-addr[2] AT 50 SKIP
            SPACE(7) v-addr3   v-sold-addr3 AT 50 SKIP.
        v-printline = v-printline + 15.
        PUT "<|10><R4><C50><#3><FROM><R10><C80><RECT>" SKIP.
        PUT "<R6><C50><FROM><R6><C80><LINE>" SKIP
            "<R8><C50><FROM><R8><C80><LINE>" SKIP
            "<R4><C62><FROM><R6><C62><LINE>" SKIP
            "<R6><C65><FROM><R8><C65><LINE>" SKIP
            "<R8><C65><FROM><R10><C65><LINE>" SKIP.
IF lv-display-comp THEN
      PUT "<=2><C3><R+2><FGCOLOR=" trim(lv-comp-color) + ">"
          "<=2><C2><R+3><P20><B>" lv-comp-name "<FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" .
        
PUT "<FArial><P12><=#3><R-2> <B>Invoice#: " ar-inv.inv-no "</B><P10>" SKIP
    "<=#3> Customer ID             Contact"
    "<=#3><R+2> Telephone                        Fax" 
    "<=#3><R+4> Customer PO                  Invoice Date <FCourier New>"    
    "<=3><R+1> " ar-inv.cust-no  space(7) cust.contact
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
     ar-inv.terms-d FORM "x(15)" space(1) v-salesman FORM "x(8)"
     v-tot-pallets FORM "->>>,>>>,>>9" 
     lv-bol-no
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
PUT "<FArial><=5><R+1>   Ordered     Shipped       B.O.      Item#                        Description                                              Price     UOM                 Amount" SKIP(1).
v-printline = v-printline + 4.
           

PUT "<FCourier New>".

