/* ar/rep/invaxis.i  */
lv-page = lv-page + 1.
PUT "<FTimes New Roman></B>".
        PUT "<C1><R1><#1>"
         /*    space(24) v-comp-add1 FORM "x(50)"  "</B>" SKIP(1)
             "<P10>" space(65) v-comp-add2 FORM "x(60)" SKIP
             space(75) v-comp-add3 FORM "x(60)" SKIP
             space(62) v-comp-add4 FORM "x(60)" "<P10>" SKIP  */  
             "<R+10><C+45><IMAGE#1=" ls-full-img1 SKIP(2). /* image */ .
/*PUT "<P10><R2><C66>PAGE " lv-page FORMAT ">9" " of <#PAGES>".*/
        PUT "<=1>".
        PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
            "<P10><=2><R+7><C53>"
            "<FCourier New>"
            "REMIT TO: " 
            "<=2><R+8><C53>" cRemitTo[1] SKIP
            "<=2><R+9><C53>" cRemitTo[2] SKIP
            "<=2><R+10><C53>" cRemitTo[3] 
            SKIP(1) 
            space(10) "BILL TO:" "<C53>SHIP TO:" SKIP
            SPACE(10) ar-inv.cust-name "<C53>" v-shipto-name skip
            SPACE(10) ar-inv.addr[1]   "<C53>" v-shipto-addr[1] SKIP
            SPACE(10) ar-inv.addr[2]  "<C53>" v-shipto-addr[2] SKIP
            SPACE(10) v-addr3   "<C53>" v-sold-addr3  SKIP
            /*SPACE(14) v-email    "</B>" SKIP */   .
        v-printline = v-printline + 17.
        PUT "<|10><R4><C53><#3><FROM><R7><C78><RECT>" SKIP.
        PUT "<R5><C53><FROM><R5><C78><LINE>" SKIP
            "<R6><C53><FROM><R6><C78><LINE>" SKIP
            "<R4><C65><FROM><R7><C65><LINE>" SKIP .
            /*"<R8><C65><FROM><R10><C65><LINE>" SKIP.*/
        
PUT "<FArial><P12><=#3><R-2> <P10>" ip-copy-title FORM "x(20)" SKIP
    "<=#3>          INVOICE#                    " ar-inv.inv-no
    "<=#3><R+1>              DATE               " v-inv-date 
    "<=#3><R+2>              PAGE                   " lv-page FORMAT ">9" " of [=@endPage" + TRIM(string(ar-inv.inv-no,">>>>>9")) + "-@startPage" + TRIM(string(ar-inv.inv-no,">>>>>9")) + "+1]" FORMAT "x(60)" "<FCourier New>"    
    SKIP(1)
    .

PUT "<|10><R21><C1><#4><FROM><R24><C80><RECT>" SKIP
    "<R22><C1><FROM><R22><C80><LINE>" SKIP    
    "<R21><C11><FROM><R24><C11><LINE>" SKIP
    "<R21><C22><FROM><R24><C22><LINE>" SKIP
    "<R21><C38><FROM><R24><C38><LINE>" SKIP
    "<R21><C52><FROM><R24><C52><LINE>" SKIP
 /* "<R21><C59><FROM><R24><C59><LINE>" */ SKIP 
    "<R21><C68><FROM><R24><C68><LINE>" SKIP
    .

v-printline = v-printline + 3.

PUT "<FArial><=4>  SHIP DATE             FOB                      SHIP VIA                            TERMS                      SALES REP                        BOL#" SKIP
     "<FCourier New><=4><R+2><B> " v-date-ship FORM "99/99/9999" space(2)
     v-fob FORM "x(12)" SPACE(1)
     v-shipvia FORM "x(20)" SPACE(1)
     ar-inv.terms-d FORM "x(15)" space(7)
    /* v-salesman FORM "x(8)" */
     v-salesman FORM "x(13)"
     lv-bol-no FORM ">>>>>>>" "</B>"
    SKIP.


PUT 
   /* "<|10><R28><C1><#5><FROM><R28><C80><LINE>" SKIP     
    "<|10><R31><C1><#6><FROM><R31><C80><LINE>" SKIP 
   */
   "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
    "<R25><C13><FROM><R27><C13><LINE>" SKIP
    "<R25><C25><FROM><R27><C25><LINE>"     
    "<R25><C42><FROM><R27><C42><LINE>" SKIP
    "<R26><C42><FROM><R26><C58><LINE>" SKIP
    "<R26><C50.5><FROM><R27><C50.5><LINE>" SKIP
    "<R25><C58><FROM><R27><C58><LINE>" SKIP
    "<R25><C60><FROM><R27><C60><LINE>" 
    "<R25><C67.4><FROM><R27><C67.4><LINE>"
    "<R25><C71><FROM><R27><C71><LINE>" SKIP 
    .   
/*puT "<FArial><=5><R+1>   Ordered     Shipped       B.O.      Item#                        Description                                              Price     UOM                 Amount" SKIP(1). */
PUT "<FArial><=5>  CUST PO#              CUST PART#    <C46>QUANTITY<C58.5>P                  " SKIP.
IF ip-print-s THEN
    PUT "<FArial> AXIS ORDER#        AXIS ITEM#                  DESCRIPTION                <C43>ORDERED<C51>SHIPPED<C58.5>C<C62>PRICE<C67.5>UOM<C73>AMOUNT" SKIP(1).
ELSE PUT "<FArial> AXIS ORDER#        AXIS ITEM#                  DESCRIPTION                <C43>ORDERED<C51>SHIPPED<C58.5>C<C62>PRICE<C67.5>UOM<C73>AMOUNT" SKIP(1).

v-printline = v-printline + 3.
           
PUT "<FCourier New><B>".
