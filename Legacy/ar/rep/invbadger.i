/* oe/rep/invBadger.i */
PUT "<FArial>".
       PUT "<C3><R2><#1><R+10><C+40><IMAGE#1=" ls-full-img1 SKIP  /* company image */ 
          /*  "<P10><=2><R+9>"
            space(3) v-comp-add1 SKIP
            space(3) v-comp-add2 space(74) "Attn: Account Payable Dept."  SKIP
            space(3) v-comp-add3 space(74) "Remit To: 5821 Production Way Langley, BC V3A 4N5"  SKIP
            space(3) v-comp-add4 SKIP
            */
            "<FCourier New><P10><=1><R+10>" SKIP
            space(10) "Bill To:"  "Ship To:" AT 60 SKIP
            SPACE(10) ar-inv.cust-name v-shipto-name AT 60 skip
            SPACE(10) ar-inv.addr[1]   v-shipto-addr[1] AT 60 SKIP
            SPACE(10) ar-inv.addr[2]  v-shipto-addr[2] AT 60 SKIP
            SPACE(10) v-addr3   v-sold-addr3 AT 60 SKIP.
        v-printline = v-printline + 15.
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

    
PUT "<|10><R20><C1><#4><FROM><R24><C80><RECT>" SKIP
    "<R22><C1><FROM><R22><C80><LINE>" SKIP    
    "<R20><C11><FROM><R24><C11><LINE>" SKIP
    "<R20><C22><FROM><R24><C22><LINE>" SKIP
    "<R20><C38><FROM><R24><C38><LINE>" SKIP
    "<R20><C52><FROM><R24><C52><LINE>" SKIP
    "<R20><C60><FROM><R24><C60><LINE>" SKIP
    "<R20><C72><FROM><R24><C72><LINE>" SKIP
    .
v-printline = v-printline + 5.

PUT "<FArial><=4><R+1>    Ship Date                 FOB                       Ship Via                           Terms                   S.Person         Pallets/Bags         BOL#" SKIP
     "<FCourier New><=4><R+3> " v-date-ship FORM "99/99/9999" space(2)
     v-fob FORM "x(12)" SPACE(1)
     v-shipvia FORM "x(20)" SPACE(1)
     ar-inv.terms-d FORM "x(15)" space(1) v-salesman FORM "x(8)"
     v-tot-pallets FORM "->>>,>>>,>>9" 
     lv-bol-no
    SKIP.


PUT "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
                "<R25><C10><FROM><R27><C10><LINE>" SKIP
	        "<R25><C43><FROM><R27><C43><LINE>" SKIP
                "<R25><C50><FROM><R27><C50><LINE>" SKIP
                "<R25><C63><FROM><R27><C63><LINE>" SKIP
                "<R25><C70><FROM><R27><C70><LINE>" SKIP
                /*"<R28><C68><FROM><R30><C68><LINE>" SKIP  */
                .   
PUT "<FArial><=5><R+1>     Shipped                                          Description"
	"<C44>Weight"
	"<C55>Price                 UOM              Amount" SKIP(1).
v-printline = v-printline + 4.
           

PUT "<FCourier New>".

